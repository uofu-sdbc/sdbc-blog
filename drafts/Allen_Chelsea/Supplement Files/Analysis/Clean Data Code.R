###################################################################################################################
# This file is created by Chelsea Allen and references the "Functions.R" code.                                    #
# This is a template for cleaning and organizing data from REDCap (or similar) for analysis.                      #
# It is not generally needed to run this code on its own, but running "Progress Report Code.R" sources this file. #
###################################################################################################################

# open R project "Analysis" to set working directory
library(diagram)
library(likert)
library(gridExtra)
source("Functions.R")
detach(package:Hmisc); library(Hmisc)
detach(package:rmdHelpers); detach(package:dplyr); library(dplyr); library(rmdHelpers)

##### IMPORT DATA #####

R.file <- tail(list.files("../Data/",pattern="NotARealProject_R_.*\\.(r|R)",full.names=T),1) # selects most recent file with this format
env.data <- new.env()  # create new environment to load data into so that it doesn't overwrite any existing objects in the global environment; also allows for loading multiple files if needed without overwriting
sys.source(R.file,env.data,chdir=T)
dataDT <- as.Date(sub_all(c(".*_R_","\\.r$"),rep("",2),R.file),format="%Y-%m-%d_%H%M")

# write.csv(data.frame(Variable=names(env.data$data),Label=label(env.data$data)),file="REDCap data labels.csv",row.names=F)

##### FUNCTIONS #####

# insert project/data cleaning specific functions

##### RECODE & REVIEW #####

# Data is in env.data$data.  First, select all the variables we need and bring their factors as well.
# In this case, I took all the variables because I only created what I needed.  Usually you will use this step to select only the variables you need, and then use the names of those variables in the next steps to recode and clean the data.
# REDCap datasets usually have a lot of timestamp and completion variables that you may or may not need (also, just a lot of variables you will never use)

raw.vars <- raw.vars <- intersect(
  paste0(rep(intersect(names(env.data$data),
                       names(env.data$data %>% 
                               select(record_id, redcap_event_name, 
                                      date, dob, age.sc, sex, height, weight, bmi, race, ethnicity, 
                                      q1, q2, incl1, incl2, incl3, incl4, met.incl, screening, 
                                      sbp, dbp, hypo, htn, eligible, consent, randomize, group, 
                                      quest1, quest2, self.report1, self.report2, drop_info))),each=2),c("",".factor")),
  names(env.data$data))

rawdata <- env.data$data %>% select(all_of(raw.vars)) %>% 
  mutate(record_id=as.character(record_id), date=as.Date(date,format="%Y-%m-%d")) %>%
  mutate(across(.cols=everything(),.fns=replace_all,prev="",new=NA)) %>% # calls blank entries NA, REDCap does not always do this
  mutate_if(.predicate=function(xx) all(xx %in% c("Checked","Unchecked",NA))|all(xx %in% c("Yes","No")), 
            .funs=function(xx) factor(replace_all(xx,c("Checked","Unchecked"),c("Yes","No")),levels=c("No","Yes")))
  # all variables that are just "Checked"/"Unchecked" or "Yes"/"No" are converted to factors with levels "No" and "Yes"
# Here it might also be helpful to coalesce some variables that are actually the same
# i.e. if you have date_drop, date_prescreen, date_screen, etc. you might be able to coalesce those into a single date variable 


Clean <- rawdata %>% 
  transmute(ID=record_id, Date=date, 
            Event=factor(replace_all(redcap_event_name,c("prescreening","screening","baseline","week_4","week_8","month_3","month_6","lost_to_follow_up"),
                                     c("PreScreening","Screening","Baseline","Week 4","Week 8","Month 3","Month 6","Drop")), 
                         levels=c("PreScreening","Screening","Baseline","Week 4","Week 8","Month 3","Month 6","Drop")),
            # Demographics
            Sex=sex.factor, DOB=dob, Age=age.sc,
            Ethnicity=sort.size(ethnicity.factor), Race=sort.size(race.factor),
            Race.Eth=sort.size(case_when(Ethnicity=="Hispanic or Latino" ~ Ethnicity,
                                         Race=="Other" ~ "Other",
                                         Race=="Prefer not to answer" | Ethnicity=="Prefer not to answer" ~ "Prefer Not to Answer",
                                         is.na(Race) | is.na(Ethnicity) ~ NA_character_,
                                         T ~ paste0(Race,", Non-Hispanic/Latino"))),
            # Pre-Screening Questions & Inclusion criteria
            Screen.Question.1=q1, Screen.Question.2=q2, Inclusion.1=incl1, Inclusion.2=incl2, Inclusion.3=incl3, Inclusion.4=incl4, 
            Met.Inclusion=met.incl, Screening.Scheduled={screening==1},
            # Body Measurements
            Height=height, Weight=weight, BMI=bmi, 
            BMI.Cat=factor(case_when(BMI<18.5 ~ "Underweight",BMI<25 ~ "Normal", BMI<30 ~ "Overweight", BMI>=30 ~ "Obese"),
                           levels=c("Normal","Underweight","Overweight","Obese")),
            SBP=sbp, DBP=dbp, Hypo=hypo, HTN=htn,
            # Questionnaires & Self-Report
            Score.1=quest1, Score.2=quest2, SelfReport.1=self.report1, SelfReport.2=self.report2,
            # Randomization
            Eligible=eligible, Consented={consent==1}, Randomized={randomize==1}, Tmt=group.factor,
            # Study Withdrawl Information
            Dropped=case_when(drop_info %in% 1:2 ~ T, drop_info==3 ~ F, T ~ NA), Death={drop_info==3})

Clean.list <- lapply(split(Clean,Clean$Event),function(xx) 
  xx  %>% arrange(ID) %>% select_if(.predicate=function(yy) sum(!is.na(yy))>0) %>% select(-Event)
) # split into a list by events.  Any variables that do not have values associated with that event will be removed. "Event" is not needed.

dup.vars <- bind_rows(lapply(names(Clean.list),function(xx) data.frame(Event=xx,vars=names(Clean.list[[xx]])))) %>% 
  group_by(vars) %>% summarize(count=n()) %>% ungroup() %>% filter(count>1,vars!="ID")  
# all variables that appear in more than one event (except ID) so they can be labeled properly during merge

Clean1 <- Clean.list$PreScreening %>% rename_with(.fn=function(xx) replace_all(xx,prev=dup.vars$vars,new=paste0(dup.vars$vars,".PreScreening")))
for(i in names(Clean.list[names(Clean.list)!="PreScreening"])) {
  Clean1 <- merge(Clean1,Clean.list[[i]] %>% rename_with(.fn=function(xx) replace_all(xx,prev=dup.vars$vars,new=paste0(dup.vars$vars,".",i))),by="ID",all=T,sort=F)
}
Clean1 <- Clean1 %>% arrange(ID)

subj.ref <- Clean1 %>% filter(Randomized) %>% arrange(Date.Screening,ID) %>% 
  transmute(ID=ID,Subject=1:length(ID),Start=as.Date(Date.Screening),
            End=as.Date(coalesce(as.character(`Date.Month 6`),as.character(Date.Drop))), # if they have neither of these, they are still participating in the study
            DaysIn=coalesce(End,dataDT)-Start, Completed=!is.na(`Date.Month 6`),
            Status=case_when(Completed ~ "Completed", Dropped|Death ~ "Did Not Complete", !Completed ~ "Active"))

##### FUNCTIONS (for later use) #####

random <- function(xx) xx %>% filter(ID %in% subj.ref$ID)

display <- function(var.names) {
  replace_all(capwords(sub_all(c("_","\\.","SelfReport"),
                               c(" "," ","Self-Report"),var.names)),
              c("Race Eth","Hypo","HTN"),
              c("Race/Ethnicity","Hypotension","Hypertension")
  )}  # fill in vector of variable names and display names that should be changed
