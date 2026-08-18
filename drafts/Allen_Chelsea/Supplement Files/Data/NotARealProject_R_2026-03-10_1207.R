#Clear existing data and graphics
rm(list=ls())
graphics.off()
#Load Hmisc library
library(Hmisc)
#Read Data
data=read.csv('NotARealProject_DATA_2026-03-10_1207.csv')
#Setting Labels

label(data$record_id) = "Record ID"
label(data$redcap_event_name) = "Event Name"
label(data$date) = "Date:"
label(data$dob) = "Date of Birth"
label(data$age.sc) = "Age"
label(data$sex) = "Gender"
label(data$height) = "Height (in)"
label(data$weight) = "Weight (lbs)"
label(data$bmi) = "BMI"
label(data$race) = "Race:"
label(data$ethnicity) = "Ethnicity:"
label(data$q1) = "Survey 1 Score"
label(data$q2) = "Survey 2 Score"
label(data$incl1) = "Is survey 1 score greater than 10?"
label(data$incl2) = "Is survey 2 score greater than 8?"
label(data$incl3) = "Have you ever been diagnosed with hypertension or hypotension?"
label(data$incl4) = "Will you be able to complete the in-person visits and online questionnaires for the duration of the study? (Outlined above)"
label(data$met.incl) = "Were all inclusion criteria met?"
label(data$screening) = "Was a screening visit scheduled?"
label(data$sbp) = "Systolic Blood Pressure (mmHg)"
label(data$dbp) = "Diastolic Blood Pressure (mmHg)"
label(data$hypo) = "Did the participant exhibit persistent hypotension during the screening visit?"
label(data$htn) = "Did the participant exhibit persistent hypertension during the screening visit?"
label(data$eligible) = "Is the participant eligible for the study?"
label(data$consent) = "Does the participant consent to participate in the study?"
label(data$randomize) = "Randomize the patient (choice=Yes)"
label(data$group) = "Randomization Group"
label(data$quest1) = "Questionnaire 1 Score"
label(data$quest2) = "Questionnaire 2 Score"
label(data$self.report1) = "Slider 1"
label(data$self.report2) = "Slider 2"
label(data$drop_info) = "Why did the participant drop out of the study?"
#Setting Units


#Setting Factors(will create new variable for factors)
mapping_redcap_event_name = c(
  "prescreening" = "Pre-screening REDCap Form",
  "screening" = "Screening Visit",
  "baseline" = "Baseline Visit",
  "week_4" = "Week 4 Online Questionnaires",
  "week_8" = "Week 8 Visit",
  "month_3" = "Month 3 Online Questionnaires",
  "month_6" = "Month 6 Visit",
  "lost_to_follow_up" = "Lost to Follow-up"
)
data$redcap_event_name.factor = factor(data$redcap_event_name, levels = names(mapping_redcap_event_name), labels = mapping_redcap_event_name)

mapping_sex = c(
  "1" = "Female",
  "2" = "Male",
  "3" = "Other",
  "4" = "Prefer not to answer"
)
data$sex.factor = factor(data$sex, levels = names(mapping_sex), labels = mapping_sex)

mapping_race = c(
  "1" = "White/Caucasian",
  "2" = "Black/African American",
  "3" = "Asian",
  "4" = "Native American/Alaska Native",
  "5" = "Native Hawaiian/Pacific Islander",
  "6" = "Other",
  "7" = "Two or More Races",
  "8" = "Prefer not to answer"
)
data$race.factor = factor(data$race, levels = names(mapping_race), labels = mapping_race)

mapping_ethnicity = c(
  "1" = "Not Hispanic or Latino",
  "2" = "Hispanic or Latino",
  "3" = "Prefer not to answer"
)
data$ethnicity.factor = factor(data$ethnicity, levels = names(mapping_ethnicity), labels = mapping_ethnicity)

mapping_incl1 = c(
  "0" = "No",
  "1" = "Yes"
)
data$incl1.factor = factor(data$incl1, levels = names(mapping_incl1), labels = mapping_incl1)

mapping_incl2 = c(
  "0" = "No",
  "1" = "Yes"
)
data$incl2.factor = factor(data$incl2, levels = names(mapping_incl2), labels = mapping_incl2)

mapping_incl3 = c(
  "0" = "Yes",
  "1" = "No"
)
data$incl3.factor = factor(data$incl3, levels = names(mapping_incl3), labels = mapping_incl3)

mapping_incl4 = c(
  "0" = "No",
  "1" = "Yes"
)
data$incl4.factor = factor(data$incl4, levels = names(mapping_incl4), labels = mapping_incl4)

mapping_met.incl = c(
  "0" = "No",
  "1" = "Yes"
)
data$met.incl.factor = factor(data$met.incl, levels = names(mapping_met.incl), labels = mapping_met.incl)

mapping_screening = c(
  "1" = "Screening visit is scheduled",
  "2" = "Patient declined to schedule visit and continue with study",
  "3" = "Patient could not be contacted to schedule visit"
)
data$screening.factor = factor(data$screening, levels = names(mapping_screening), labels = mapping_screening)

mapping_hypo = c(
  "0" = "No",
  "1" = "Yes"
)
data$hypo.factor = factor(data$hypo, levels = names(mapping_hypo), labels = mapping_hypo)

mapping_htn = c(
  "0" = "No",
  "1" = "Yes"
)
data$htn.factor = factor(data$htn, levels = names(mapping_htn), labels = mapping_htn)

mapping_eligible = c(
  "0" = "No",
  "1" = "Yes"
)
data$eligible.factor = factor(data$eligible, levels = names(mapping_eligible), labels = mapping_eligible)

mapping_consent = c(
  "0" = "No",
  "1" = "Yes"
)
data$consent.factor = factor(data$consent, levels = names(mapping_consent), labels = mapping_consent)

mapping_randomize = c(
  "0" = "Unchecked",
  "1" = "Checked"
)
data$randomize.factor = factor(data$randomize, levels = names(mapping_randomize), labels = mapping_randomize)

mapping_group = c(
  "1" = "A",
  "2" = "B"
)
data$group.factor = factor(data$group, levels = names(mapping_group), labels = mapping_group)

mapping_drop_info = c(
  "1" = "Patient Withdrew",
  "2" = "Lost to Follow-up",
  "3" = "Death"
)
data$drop_info.factor = factor(data$drop_info, levels = names(mapping_drop_info), labels = mapping_drop_info)

