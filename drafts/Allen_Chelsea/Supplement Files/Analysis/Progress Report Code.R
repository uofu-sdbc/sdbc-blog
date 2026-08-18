###########################################################################################################################################
# This file is created by Chelsea Allen and is intended to be a template for progress report code.                                        #
# This code relies on "Clean Data Code.R" and "Functions.R".  Only this file needs to be run and it will source these files.              #
# This is intended to be run in the "Analysis" R project with the data in a "Data" folder adjacent to the "Analysis" folder.              #
# Changing the structure or names of the files/folders will likely require changes to the code, specifically the "Clean Data Code.R" file #
###########################################################################################################################################

# open R project "Analysis" to set working directory
rm(list=ls())
source("Clean Data Code.R")
dataDT # make sure the data pull date is as expected

##### Project Variables #####

# Set variables
date0 <- as.Date("2024-01-01") # date enrollment period began
end.date <- as.Date("2026-12-31") # date enrollment period is scheduled to end
goal <- 180 # goal enrollment
goal.comp <- 150 # goal to complete study

# Calculated Variables
elp.mo <- as.numeric(dataDT-date0)/(365.25/12) # elapsed months
rem.mo <- as.numeric(end.date-dataDT)/(365.25/12) #remaining months (in enrollment period)

##### ANALYSIS #####

#Bring in dataset for analysis if not created with above lines
# Clean <- read.csv("Clean.csv",na.strings="NA",quote="\"",header=T)

tf.attr <- list() # list of lists
### Tables called t1, t2, etc. each with variables 'title', 'just', and 'comments' 
### Figures called f1, f2, etc. each with variables 'title', 'h', 'w', and 'comments'
### Headers called h_, etc. each with variables 'title', 'level', and 'comments'
### Calls called c_, each with variables 'title', and 'text' (title not printed in output, text an expression to evaluate)

counts <- Clean1 %>% transmute(PreScreening_Survey=!is.na(Date.PreScreening), `Met Inclusion Criteria`=as.logical(Met.Inclusion),
                               `Screening Scheduled`=Screening.Scheduled, `Screening Visit`=!is.na(Date.Screening), 
                               Eligible=as.logical(Eligible), Consented=as.logical(Consented), Randomized=as.logical(Randomized)) %>%
  group_by_all() %>% summarize(count=n())

incl <- sapply(names(counts)[-ncol(counts)],function(xx) {
  i <- which(names(counts)==xx)
  sum(counts$count[which(rowSums(counts[,1:i])==i)])
})
arrow.lab <- data.frame(before=c("Met Inclusion Criteria","Eligible"),
                        lab=c("'1a*'","'1b*'"),stringsAsFactors=F) %>% mutate(row=2*match(before,names(incl))-2,col=row-1)

f1 <- function() {
  nbox <- 2*length(incl)-1
  mat <- 0
  for(i in 2:nbox) {
    mat <- rbind(mat,replace(numeric(nbox+1),floor(i/2)*2-1,""))
  }
  mat <- as.data.frame(rbind(mat,0))
  for (i in 1:nrow(arrow.lab))
    mat[arrow.lab$row[i],arrow.lab$col[i]] <- arrow.lab$lab[i]
  # print(mat)
  N <- incl[1]
  for(i in 2:length(incl)) {
    N <- c(N,incl[i-1]-incl[i],incl[i])
  }
  lab <- paste0(c("","Not "),rep(sub("_"," ",names(incl)),each=2)[-1],"\nN=",N)
  # print(lab)
  par(mar=rep(0.1,4))
  plotmat(mat, pos = rep(2,nbox/2+0.5), name = lab,
          lwd = 1, box.lwd=2, box.lcol = c(rep(1,nbox),0), shadow.size=c(rep(0.01,nbox),0),
          curve=0, arr.type="triangle", arr.length=0.2, cex.txt = 0.8,
          box.size = 0.2, box.type = "square", box.prop = 0.25, box.cex=0.8)
}

x11(height=10,width=7); f1()
tf.attr$f1 <- list(title="Figure 1. Enrollment Status",h=length(incl)-1,w=4.5,
                   comments=paste0("Final enrolled number is ",tail(incl,1),
                                   ".\n*These exclusions are covered in the indicated table."))

### Table 1a: PreScreening Inclusion Criteria

inclusions <- Clean.list$PreScreening %>% select(starts_with("Inclusion."))

# inclusions %>% group_by_all() %>% summarize(count=n())
# colSums(inclusions==1,na.rm=T)
# colSums(inclusions==0,na.rm=T)

incl.var <- data.frame(names=c(paste0("Inclusion.",1:4)),
                       `Inclusion Criteria`=c("Survey 1 > 10","Survey 2 > 8", "Hypertension/Hypotension", 
                                              "Ability to do study"),
                       check.names=F,stringsAsFactors=F)

t1a <-  incl.var %>% mutate(`Number Failed`=sapply(inclusions %>% select(all_of(names)),function(xx) sum(xx==0,na.rm=T))) %>% select(-names)

tf.attr$t1a <- list(title="Table 1a. PreScreening Inclusion Criteria",just=c("L","C"),
                   comments=paste0("Some subjects failed to meet multiple inclusion criteria.\n",
                                   "Of the ",nrow(Clean.list$PreScreening)," subjects who completed the prescreening survey, ",
                                   sum(Clean.list$PreScreening$Met.Inclusion==0,na.rm=T)," did not meet at least 1 inclusion criteria and ",
                                   sum(Clean.list$PreScreening$Met.Inclusion==1,na.rm=T)," met all inclusion criteria and were able to be scheduled for a screening visit."))


### Table 1b: Eligibility at Screening

exclusions <- Clean.list$Screening %>% select(Hypo, HTN)

excl.var <- data.frame(names=c("Hypo","HTN"),
                       `Exclusion`=c("Hypotension in Screening Visit","Hypertension in Screening Visit"),
                       check.names=F,stringsAsFactors=F)

t1b <-  excl.var %>% mutate(`Number Excluded`=sapply(exclusions %>% select(all_of(names)),function(xx) sum(xx==1,na.rm=T))) %>% select(-names)

tf.attr$t1b <- list(title="Table 1b. Exclusions at Screening Visit",just=c("L","C"),
                    comments=paste0("Of the ",nrow(Clean.list$Screening)," subjects who completed the screening visit, ",
                                    sum(Clean.list$Screening$Eligible==0,na.rm=T)," were excluded based on one of these criteria and ",
                                    sum(Clean.list$Screening$Eligible==1,na.rm=T)," were eligible for randomization."))



# Table 2: Demographics

(t2var <- data.frame(names = c("Sex","Age","Race","Ethnicity","Race.Eth"),stringsAsFactors=F) %>% 
    mutate(cols=match(names,names(Clean1)),outnames=display(names)))

t2 <- myNOgrtab(random(Clean1) %>% select(t2var$names),t2var$outnames,onelinebinary=F,allSums=T,footMissing=F)

tf.attr$t2 <- list(title="Table 2. Demographics (Enrolled Only)", just=c("L","L","R","C"), comments=NULL)


# Table 3: Physical Measures

(t3var <- data.frame(names=c("SBP","DBP","BMI")) %>% 
    mutate(cols=match(names,names(Clean)),outnames=display(names)))


t3dat <- random(Clean) %>% filter(Event %in% c("Screening","Week 8","Month 6")) %>% 
  select(all_of(t3var$names),Event) %>% filter(rowSums(!is.na(.))>1) %>% mutate(Event=droplevels(Event))
t3var$missing <- sapply(t3dat[,t3var$names],
                        function(xx) paste0(table(t3dat$Event[is.na(xx)]),collapse=" / "))

t3 <- mytab.long(myMultiGrtab(t3dat %>% select(all_of(t3var$names)), gvar=t3dat$Event,
                              dnames=t3var$outnames,onelinebinary=F,printpvalue=F,footMissing=F,allSums=T)[[1]] %>% 
                   rename_all(function(xx) sub_all("Screening","Baseline",xx)) %>% 
                   mutate_all(.funs=list(function(xx) 
                     sub_all(c("NA \\(NA\\)","NA \\(NA, NA\\)","\\(Inf, -Inf\\)","0 \\(NaN%\\)","\\(NA\\)"),
                             c(rep("--",4),"(--)"),xx))) %>% 
                   mutate('#Missing'=t3var$missing[match(Variable,t3var$outnames)]))

tf.attr$t3 <- list(title="Table 3. Physical Measures (Enrolled Only)", just=c("L",rep("R",ncol(t3)-2),"C"))


# Table 4: Survey Scores

(t4var <- data.frame(names=c("Score.1","Score.2","SelfReport.1","SelfReport.2")) %>% 
    mutate(cols=match(names,names(Clean)),outnames=display(names)))

t4dat <- random(Clean) %>% filter(Event %in% c("Baseline","Week 4","Week 8","Month 3","Month 6")) %>% 
  select(all_of(t4var$names),Event) %>% filter(rowSums(!is.na(.))>1) %>% mutate(Event=droplevels(Event))
t4var$missing <- sapply(t4dat[,t4var$names],
                        function(xx) paste0(table(t4dat$Event[is.na(xx)]),collapse=" / "))

t4 <- mytab.long(myMultiGrtab(t4dat %>% select(t4var$names), gvar=droplevels(t4dat$Event),
                              dnames=t4var$outnames,onelinebinary=F,printpvalue=F,footMissing=F,allSums=T)[[1]] %>% 
                   mutate_all(.funs=list(function(xx) 
                     sub_all(c("NA \\(NA\\)","NA \\(NA, NA\\)","\\(Inf, -Inf\\)","0 \\(NaN%\\)","\\(NA\\)"),
                             c(rep("--",4),"(--)"),xx))) %>% 
                   mutate('#Missing'=t4var$missing[match(Variable,t4var$outnames)]))

tf.attr$t4 <- list(title="Table 4. Survey Scores (Enrolled Only)", just=c("L",rep("R",ncol(t4)-2),"C"))



# Figure 2:

f2 <- function() {
  date.vars <- c("PreScreening","Randomized","Week 4","Week 8","Month 3","Month 6","Dropped","Death","Now")
  cols <- data.frame(tr=c(hcl.colors(length(date.vars)-3,palette="zissou1",alpha=0.5),
                          rgb(0.45,0.45,0.45,0.5),rgb(0,0,0,0.5),rgb(0.6,0.6,0.6,0.5))) %>% 
    mutate(op=substr(tr,1,7),pch=c(rep(19,length(date.vars)-1),20))
  dat <- merge(random(Clean1),subj.ref) %>% arrange(ID) %>% 
    mutate(Now=ifelse(Status=="Active",as.character(dataDT),NA),Randomized=Date.Screening,
           Dropped=case_when(Dropped ~ Date.Drop), Death=case_when(Death ~ Date.Drop)) %>%
    rename_at(vars(starts_with("Date")),function(xx) sub_all("^Date\\.","",xx)) %>%
    mutate(across(.cols=all_of(date.vars),.fns=function(xx) as.numeric(as.Date(xx)-as.Date(Randomized))))
  # View(dat %>% select(ID,all_of(date.vars)))
  dat <- dat %>% mutate(min.date=apply(dat[,date.vars],1,min,na.rm=T),max.date=apply(dat[,date.vars],1,max,na.rm=T))
  par(mar=c(4.1,4.1,2.1,2.1))
  plot(NA,xlim=expand_range(dat[,date.vars],mult=c(0,0.25)),ylim=rev(range(dat$Subject)),type="n",xlab="Days from Randomization",ylab="Subject",yaxt="n")
  axis(2,las=1,tick=F)
  nDays <- data.frame(vars=c("Randomized","Week 4","Week 8","Month 3","Month 6"),
                      days=c(0,4*7,8*7,365.25/4,365.25/2)) %>% mutate(num=sapply(vars,function(xx) {
                        if(xx %in% names(dat)) return(sum(!is.na(dat[,xx])))
                        return(0)})) # data frame of when events are expected and how many completed
  for(i in which(nDays$vars %in% date.vars)) {
    abline(v=nDays$days[i], col=cols$tr[date.vars==nDays$vars[i]])
    mtext(nDays$num[i],at=nDays$days[i],col=cols$op[date.vars==nDays$vars[i]])
  }
  segments(x0=dat$min.date,x1=dat$max.date,y0=dat$Subject,col=rgb(0.5,0.5,0.5))
  for(i in seq_along(date.vars)) points(dat[,c(date.vars[i],"Subject")],pch=cols$pch[i],col=cols$op[i])
  legend("bottomright",bty="n",
         legend=sub_all(c("Now"),paste0("Now (",as.character(dataDT),")"),date.vars),
         pch=cols$pch,col=cols$op)
  
  progress <<- dat %>% select(ID,Subject,Start,End,all_of(date.vars)) %>%
    mutate(Last=case_when(!is.na(`Month 6`) ~ 3, !is.na(`Week 8`) ~ 2, !is.na(Randomized) ~ 1))
}
x11(); f2()
tf.attr$f2 <- list(title="Figure 2. Study Progress", h=8, w=6.5,
                   comments="Days are from randomization time (negative are before).")

prog.sum <- progress %>% mutate(Dropped=coalesce(Dropped,Death)) %>% 
  select(Randomized,`Week 8`,`Month 6`,Dropped) %>% 
  mutate_all(.funs=list(function(xx) !is.na(xx))) %>%  group_by_all() %>% summarize(count=n()) %>% ungroup() %>%
  mutate(Last=case_when(`Month 6` ~ 3, `Week 8` ~ 2, Randomized ~ 1)) %>% 
  arrange(Last)
prog.rate <- reshape(as.data.frame(xtabs(count ~ Last + Dropped, prog.sum)) %>% transmute(Last,Dropped=as.logical(Dropped),count=Freq) %>% 
                       arrange(Last, Dropped) %>% mutate(Total=ifelse(Dropped,sum(count)-cumsum(count)+count,NA),Dropped=ifelse(Dropped,"Drop","Unk")),
                     direction="wide",idvar="Last",timevar="Dropped") %>% select_if(.predicate=function(xx) sum(!is.na(xx))>0) %>% rename(Total=Total.Drop) %>%
  mutate(drop.rate=ifelse(Last==3,0,count.Drop/Total),cont.rate=1-ifelse(drop.rate==0 & Last!=3,0.5/Total,drop.rate),overall=rev(cumprod(rev(cont.rate))),exp=overall*count.Unk,
         LastF=factor(Last,labels=c("Randomization","Week 8","Month 6")))
need <- ceiling((goal.comp-sum(prog.rate$exp))/prog.rate$overall[1]); if(need<0) need <- 0

# Figure 3

f3 <- function() {
  prog.rate <- prog.rate %>% mutate(Visit=paste0(LastF,"\nN = ",coalesce(Total,0)+count.Unk),
                                    Drop=paste0("Dropped\nN = ",count.Drop),
                                    NFP=paste0("NFP*\nN = ",count.Unk))
  marks <- nrow(prog.rate)
  nbox <- 3*marks-2
  mat <- 0
  for(i in 2:marks-1) {
    # mat <- rbind(mat,replace(numeric(nbox+1),floor(i/2)*2-1,""))
    mat <- rbind(mat,matrix(replace(numeric(nbox+1),3*i-2,""),nrow=3,ncol=nbox+1,byrow=T))
  }
  mat <- as.data.frame(mat)
  # for (i in 1:nrow(arrow.lab))
  #   mat[arrow.lab$row[i],arrow.lab$col[i]] <- arrow.lab$lab[i]
  # print(lab)
  par(mar=rep(0.1,4))
  plotmat(mat, pos = cbind(rep(c(0.21,0.79,0.4),times=marks),
                           1 - c(0,0,1/2)/marks - rep(seq(1/(2*marks),by=1/marks,length=marks),each=3))[1:nbox,], 
          name = c(t(prog.rate %>% select(Visit,Drop,NFP)))[1:nbox],
          lwd = 1, box.lwd=2, box.lcol = 1, shadow.size=0.01,
          curve=0, arr.type="triangle", arr.length=0.2, cex.txt = 0.8,
          box.size = 0.15, box.type = "square", box.prop = 0.35, box.cex=0.8)
}
x11(height=10,width=6); f3()

tf.attr$f3 <- list(title="Figure 3. Summary Study Progress (In-person Visits only)", h=nrow(prog.rate)+2, w=3,
                    comments=paste0("*NFP = No Further Progress: participants that haven't done the next visit, ",
                                    "but have not dropped.\nDrop out rate percentages below are calculated without considering the “NFP” group. ",
                                    "It is the percentage of those who dropped out of those who either dropped out or continued.\n\nDrop Rate by Study Status:\n",
                                    paste0(unlist(head(prog.rate,-1) %>% 
                                                    transmute(A=paste0("\t- After ",LastF," and before the next visit there was a ",
                                                                       round(drop.rate*100),"% drop out rate.\n\t\t - ",count.Drop," dropped out of ",
                                                                       Total," participants who either dropped or continued."))),collapse="\n"),
                                    "\n\nBased on the observed attrition rates, we would project that ",floor(sum(prog.rate$exp)),
                                    " of the subjects enrolled will complete the study (this includes the ",tail(prog.rate$count.Unk,1),
                                    " who have already completed).\n\nTo reach the goal of ",goal.comp," completed subjects, we estimate we need to enroll ",
                                    need, " more subjects, making ", nrow(subj.ref) + need, " enrolled in total."))

new.goal <- max(nrow(subj.ref) + need, goal)


# Figure 4:

f4 <- function() {
  date.df <- function(dates) {
    dates2 <- sort(c(rep(na.omit(dates),2),dataDT))
    data.frame(Date=dates2,'Number of Subjects'=floor(seq_along(dates2)/2),check.names = F)
  }
  data.list <- lapply(list('Inclusion Criteria'=(Clean1 %>% filter(Met.Inclusion==1))$Date.PreScreening,
                           Screened=(Clean1 %>% filter(!is.na(Date.Screening)))$Date.Screening,
                           Eligible=(Clean1 %>% filter(Eligible==1))$Date.Screening,
                           Randomized=random(Clean1)$Date.Screening),date.df)
  cols <- hcl.colors(length(data.list),palette="zissou1",alpha=0.7)
  plot(data.list[[1]],typ="l",col=cols[1],las=1,yaxs="i",ylim=expand_range(data.list[[1]]$'Number of Subjects',c(0,0.01)))
  for(i in seq_along(data.list)[-1]) {
    lines(data.list[[i]],col=cols[i])
  }
  ends <- unique(unlist(lapply(data.list,function(xx) max(xx$'Number of Subjects'))))
  axis(4,at=ends,las=1,tck=F,mgp=c(3,0.2,0))
  legend("topleft",legend=names(data.list),bty="n",lty=1,col=cols)
}
x11(); f4()
tf.attr$f4 <- list(title="Figure 4. Subject Screening Numbers", h=5, w=5)


# Figure 5:


f5 <- function() {
  date.df <- function(dates) {
    dates2 <- sort(c(rep(na.omit(dates),2),dataDT))
    data.frame(Date=dates2,'Number of Subjects'=floor(seq_along(dates2)/2),check.names = F)
  }
  rand <- date.df(unlist(random(Clean1) %>% transmute(X=Date.Baseline)))
  x0 <- as.numeric(date0)
  m <- 5*12/365.25; b <- -m*x0
  goal <- m*as.numeric(dataDT)+b
  # cols <- hcl.colors(length(data.list),palette="zissou1")
  plot(rand,typ="l",las=1,ylim=expand_range(c(rand$'Number of Subjects',goal),c(0,0.01)),xlim=expand_range(c(x0,rand$Date),0.01),yaxs="i",xaxs="i",xaxt="n")
  x.dates <- seq(floor_date(min(c(date0,rand$Date)),unit="month"),max(c(date0,rand$Date)),"month")
  while(length(x.dates) > 6) {x.dates <- rev(rev(x.dates)[as.logical(1:length(x.dates)%%2)])}
  axis(1,at=x.dates,labels=format(x.dates,"%b\n%Y"),tck=F)
  abline(a=b,b=m,col=rgb(0,0,0,alpha=0.25))
  # m <- 3.75*12/365.25; b <- -m*x0
  # abline(a=b,b=m,col=rgb(0,0,0,alpha=0.25),lty=2)
  ends <- unique(c(max(rand$'Number of Subjects'),floor(goal)))
  axis(4,at=ends,las=1,tck=F,mgp=c(3,0.2,0))
  legend("topleft",legend=c("Target","Randomized"),bty="n",col=rgb(0,0,0,alpha=c(0.25,1)),lty=1)
}
x11(); f5()
tf.attr$f5 <- list(title="Figure 5. Enrollment Numbers", h=5, w=5, 
                   comments=paste0("First day is taken as ", date0,". Our target is to enroll 5 per month.\n\n",
                                   "Our overall enrollment rate is ", myForm(nrow(subj.ref)/elp.mo,digitsx=1),
                                   " subjects per month and the rate for the last 3 months is ",
                                   myForm(sum(unlist(random(Clean1) %>% transmute(X=case_when(Randomized==1 ~ Date.Baseline) >= dataDT-365.25/4)))/3,digitsx=1)," subjects per month."))


# Figure 6:

f6.goal <- goal  # when getting to the end of the study, if you want to use the calculated goal as the goal, make f6.goal <- new.goal (which is calculated above)

f6 <- function() {
  date.df <- function(dates) {
    dates2 <- sort(c(rep(na.omit(dates),2),dataDT))
    data.frame(Date=dates2,'Number of Subjects'=floor(seq_along(dates2)/2),check.names = F)
  }
  rand <- date.df(random(Clean1)$Date.Baseline)
  x0 <- as.numeric(date0)
  m <- 4*12/365.25; b <- -m*x0
  proj <- merge(as.data.frame(table(Mo=floor_date(na.omit(random(Clean1)$Date.Baseline),unit="month"))) %>% mutate(Mo=as.Date(Mo)),
                data.frame(Mo=head(seq.Date(from=floor_date(date0,unit="month"),to=dataDT,by="month"),-1)),all=T) %>% 
    mutate(Freq=ifelse(is.na(Freq),0,Freq),endMo=ceiling_date(Mo,unit="month")-1) %>% 
    filter(Mo < floor_date(dataDT,unit="month")) %>% mutate(cumFreq=cumsum(Freq),NoMo=12*year(Mo)+month(Mo)-12*year(date0)-month(date0)+1) %>% 
    filter(Mo >= date0) %>% mutate(m.proj=cumsum(Freq*NoMo)/cumsum(NoMo)) %>% mutate(days=(f6.goal-cumFreq)/m.proj/12*365.25) %>% 
    mutate(end=endMo+days)
  # View(proj)
  proj.end <- dataDT+(f6.goal-max(rand$'Number of Subjects'))/tail(proj$m.proj,1)/12*365.25
  # b.proj <- max(rand$'Number of Subjects')-m.proj*as.numeric(dataDT)
  # proj <- m.proj*as.numeric(end.date+365)+b.proj
  cols <- hcl.colors(2,palette="zissou1")
  layout(cbind(1,2),widths=c(4,1))
  par(mar=c(5.1,4.1,3.1,2.1))
  plot(rand,typ="l",las=1,ylim=expand_range(c(0,f6.goal),c(0,0.01)),xlim=expand_range(c(x0,rand$Date,end.date,proj.end),0.01),yaxs="i",xaxs="i",lwd=2)
  segments(x0=proj$endMo,y0=proj$cumFreq,x1=proj$end,y1=f6.goal,col=rgb(0,0,0,alpha=0.2*proj$NoMo/max(proj$NoMo)))
  segments(x0=dataDT,y0=max(rand$'Number of Subjects'),x1=proj.end,y1=f6.goal,lwd=1)
  abline(h=f6.goal,v=end.date,col=cols[2])
  abline(h=nrow(subj.ref),v=dataDT,col=cols[1])
  axis(3,at=c(dataDT,proj.end),labels=format.Date(c(dataDT,proj.end),"%b\n%Y"),tck=F,mgp=c(3,0.2,0))
  axis(4,at=nrow(subj.ref),tck=F,mgp=c(3,0.2,0),las=1)
  # abline(a=b,b=m,col=rgb(0,0,0,alpha=0.25))
  # m <- 3.75*12/365.25; b <- -m*x0
  # abline(a=b,b=m,col=rgb(0,0,0,alpha=0.25),lty=2)
  # segments(x0=dataDT,x1=end.date+365,y0=max(rand$'Number of Subjects'),y1=max(rand$'Number of Subjects')+proj)
  # ends <- unique(c(max(rand$'Number of Subjects'),f6.goal))
  # axis(4,at=ends,las=1,tck=F,mgp=c(3,0.2,0))
  # View(proj)
  par(mar=c(5.1,0,3.1,0))
  plot(0:1,type="n",axes=F,xlab="",ylab="")
  legend("left",legend=c("\nEnrolled\n","Projected\nEnrollement","Current\nStatus*","\nTargets**\n","Previous\nProjections"),
         bty="n",col=c(1,1,cols,rgb(0,0,0,0.2)),lwd=c(2,1,1,1,1))
  tf.attr$f6 <<- list(title="Figure 6. Projected Enrollment", h=5.25, w=6.5,
                      comments=paste0("*Current Status is the current number enrolled, ", nrow(subj.ref),", and the date data was pulled, ", dataDT,
                                      ".\n**Targets is the targeted enrollment, ", f6.goal, ", and the targeted end date, ", end.date,".\n\n",
                                      "At the current estimated pace of ",myForm(tail(proj$m.proj,1),digitsx=2),
                                      " enrollments per month we are projected to reach ", f6.goal," by ", proj.end,
                                      ", or projected to reach ",floor(tail(proj$m.proj*rem.mo + nrow(subj.ref),1))," by the end date, ", end.date,
                                      ". This estimated pace was produced with a weighted average of previous monthly enrollments and is recalculated on the 1st of each month.\n",
                                      "In order to reach the targeted ", f6.goal, " subjects (",f6.goal-nrow(subj.ref)," remaining) by ", end.date,
                                      " we need to enroll ",myForm((f6.goal-nrow(subj.ref))/rem.mo,digitsx=2)," subjects per month.\n\n",
                                      "Enrollment for the past 3 months",
                                      paste0((tail(proj,3) %>% transmute(l=paste0("\n\t",format.Date(Mo,format="%b %Y"),": ",Freq)))$l,collapse="")))
}
x11(height=10.5,width=13);f6()


### Figure 7: Completion Status

f7 <- function() {
  date.df <- function(datesE,datesD=NULL) {
    if(is.null(datesD)) {
      dates2 <- sort(c(rep(na.omit(as.Date(datesE)),2),dataDT))
      return(data.frame(Date=dates2,Number=floor(seq_along(dates2)/2)))
    } else {
      dates2 <- rbind(data.frame(dates=na.omit(datesE),diff=1),
                      data.frame(dates=na.omit(datesD),diff=-1)) %>% 
        arrange(dates,diff) %>% mutate(Number=cumsum(diff),dates=as.Date(as.character(dates)))
      return(bind_rows(rbind(dates2,dates2 %>% mutate(Number=c(0,head(Number,-1)),diff=0)),
                       data.frame(dates=dataDT,Number=tail(dates2$Number,1))) %>% arrange(dates,abs(diff),diff) %>% select(-diff))
    }}
  rand <- date.df(subj.ref$Start,subj.ref$End[!subj.ref$Completed])
  # View(rand)
  x0 <- as.numeric(date0)
  m <- 4*12/365.25; b <- -m*x0
  goal <- goal.comp
  cols <- hcl.colors(2,palette="zissou1")
  layout(cbind(1,2),widths=c(4,1))
  par(mar=c(5.1,4.1,3.1,2.1))
  plot(rand,typ="l",las=1,ylim=expand_range(c(0,goal,rand$Number),c(0,0.01)),xlim=expand_range(c(x0,dataDT),c(0.001,0)),yaxs="i",xaxs="i",col=cols[1])
  comp <- date.df(subj.ref$End[subj.ref$Completed])
  lines(comp,col=cols[2])
  now <- data.frame(rand=tail(rand$Number,1),comp=tail(comp$Number,1)) %>% mutate(study=rand-comp,loc=(rand+comp)/2)
  axis(4,at=c(now$rand,now$comp),tck=F,mgp=c(3,0.2,0),las=1,col.ticks=cols)
  
  mtext(paste(now$study,"\nOn Study"),side=4,line=1,at=now$loc,las=1)
  par(mar=c(5.1,0,3.1,0))
  plot(0:1,type="n",axes=F,xlab="",ylab="")
  legend("bottomleft",legend=c("Randomized &\nNot Dropped","\nCompleted\n"),
         bty="n",col=cols,lwd=1)
  tf.attr$f7 <<- list(title="Figure 7. State of the Study", h=5.25, w=6.5,
                      comments=paste0("There are ",now$rand," subjects randomized that have not dropped. This figure includes the ",
                                      now$comp, " subjects that have completed the study and the ",now$study,
                                      " subjects that are currently still in the study (i.e. have neither dropped nor completed).",
                                      "\nBased on the observed attrition rates, we would project that ",
                                      floor(sum(prog.rate$exp)) - now$comp, " of the ", now$study,
                                      " subjects that are on study will complete it, making a total of ",
                                      floor(sum(prog.rate$exp))," completed subjects."))
}
x11(height=10.5,width=13); f7()

# Header 1:

tf.attr$h1 <- list(title="Study Status",level=1,comments=paste0("Data was pulled on ", dataDT,"."))

# Calls:

tf.attr$cLP <- list(title="Switch to Landscape",text="addPageBreak(rtf,width=11,height=8.5)")
tf.attr$cPP <- list(title="Switch to Portrait",text="addPageBreak(rtf,width=8.5,height=11)")
# tf.attr$c1 <- list(title="___",text="___")


### Output document ###

dataDT
out.order <- c("h1","f1","t1a","t1b","t2","t3","cLP","t4","cPP","f2","f3","f4","f5","f6","f7")
data.frame(out.order,obj_or_attr=out.order %in% ls() | startsWith(out.order,"h") | startsWith(out.order,"c"), attr=out.order %in% names(tf.attr))
sapply(tf.attr[out.order],function(xx) xx$title)

rtf <- RTF(paste0("Progress Reports/Study Progress Report ",dataDT,".doc"),width=8.5,height=11)
for (i in out.order) {
  if(substr(i,1,1)=="h") {
    addHeader(rtf,tf.attr[[i]]$title,TOC.level=tf.attr[[i]]$level)
    addParagraph(rtf,paste0(tf.attr[[i]]$comments,"\n"))
  } else if(substr(i,1,1)=="c") {
    eval(parse(text=tf.attr[[i]]$text))
  } else {
    addHeader(rtf,tf.attr[[i]]$title,TOC.level=3)
    if(substr(i,1,1)=="t")
      addTable(rtf,get(i),row.names=F,NA.string="",col.justify=tf.attr[[i]]$just, header.col.justify=tf.attr[[i]]$just)
    else if(substr(i,1,1)=="f")
      addPlot(rtf,get(i),width=tf.attr[[i]]$w,height=tf.attr[[i]]$h)
    else print(paste0(i,": type unknown"))
    addParagraph(rtf,paste0(tf.attr[[i]]$comments,"\n\n"))
  }
}
done.RTF(rtf)


