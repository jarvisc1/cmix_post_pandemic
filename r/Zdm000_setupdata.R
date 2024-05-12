## This script is unlikely to go online. 

## Packages
library(data.table)
library(lubridate)

# Load participant data ---------------------------------------------------
#part <- qs::qread("../comix/data/part.qs")
input_folder<-"./data/zenodo/"
fname=paste0(input_folder,"CoMix_last_round_participant_common.csv")
part_data_common<-read.csv(file=fname)
fname=paste0(input_folder,"CoMix_last_round_participant_extra.csv")
part_data_extra<-read.csv(file=fname)
part<-merge(part_data_common,part_data_extra)


fname=paste0(input_folder,"CoMix_last_round_hh_common.csv")
hh_common<-read.csv(file=fname)
#generate part_id removing from hh_id the string "HH_"
hh_common$part_id<-gsub("HH_","",hh_common$hh_id)

part<-merge(part,hh_common,by="part_id")



fname=paste0(input_folder,"CoMix_last_round_sday.csv")
sday<-read.csv(file=fname)
part<-merge(part,sday,by="part_id")
part<-data.table(part)
### Rename part variables
part$part_wave_uid<-part$part_id
part$part_age_group<-part$part_age
# Transform weekday into a factor with explicit name day
part$weekday<-part$dayofweek
wday_levs <- c(0,1,2,3,4,5,6)
wday_labs <- c("Sunday","Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

part[, weekday := factor(weekday, levels = wday_levs, labels = wday_labs)]




fname=paste0(input_folder,"CoMix_last_round_contact_common.csv")
cont_data_common<-read.csv(file=fname)
fname=paste0(input_folder,"CoMix_last_round_contact_extra.csv")
cont_data_extra<-read.csv(file=fname)
contacts<-merge(cont_data_common,cont_data_extra)
contacts<-merge(contacts,part[,c("part_id","country")],by="part_id")

contacts<-data.table(contacts)
# rename contact frequency multi all in original name
contacts$cnt_frequency <-contacts$frequency_multi_all
contacts$cnt_frequency <- factor(contacts$cnt_frequency, levels = c("1-2 days","3-7 days","2-3 weeks","1 month","occasional","never met"),
                                 ordered=TRUE)

contacts$cnt_total_time <-factor(contacts$cnt_total_time,
                                 levels=c("<5m","5m-14m","15m-59m","60m-4h","4h+")
                                 ,ordered=TRUE)

contacts[, table(cnt_prec_yn)]
contacts[, table(cnt_prec)]
contacts[, table(cnt_prec_yn, cnt_prec_none)]
# Edit part chracteristics ------------------------------------------------

part[, dayweight := fifelse(weekday %in% c("Sunday", "Saturday"), 2/7, 5/7)]


## Turn age group in factor
part[part_age_group == "Under 1", part_age_group := "0-4"]
age_levs <- c("0-4", "5-11", "12-17", "18-29", "30-39", "40-49", "50-59", "60-69", "70-120", "Unknown")
age_labs <- c("0-4", "5-11", "12-17", "18-29", "30-39", "40-49", "50-59", "60-69", "70+", "Unknown")

part[, part_age_group := factor(part_age_group, levels = age_levs, labels = age_labs)]


## Clean up age so adults aren't children and otherwise
part[sample_type == "child" & part_age_group %in% c("18-29", "30-39", "40-49", "50-59", "60-69", "70+"), part_age_group := "Unknown"]
part[is.na(part_age_group), part_age_group := "Unknown"]

part[sample_type == "adult" & part_age_group %in% c("0-4","5-11","12-17"), part_age_group := "Unknown"]

part[, table(part_age_group, country)]

## Gender
gen_levs <- c("F", "M", "other")
gen_labs <- c("Female", "Male", "Other")

part[, part_gender := factor(part_gender, levels = gen_levs, labels = gen_labs)]
part[is.na(part_gender), part_gender := "Other"]

## Weekday
day_levs <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
day_labs <- c("Mon", "Tue", "Wed", "Thu", "Fr", "Sat", "Sun")

part[, weekday := factor(weekday, levels = day_levs, labels = day_labs)]

## Household size
part[, hh_size_group := factor(hh_size_group)]

## Country
country_levs <- c("all", "uk", "be", "nl", "ch")
country_labs <- c("All", "UK", "BE", "NL", "CH")

part[, country := factor(country, levels = country_levs, labels = country_labs)]

part[, table(country)]

part <- part[!is.na(country)]

## High risk

part[, part_high_risk := ifelse(part_high_risk_v2 == "yes" , "yes", "no")]
part[is.na(part_high_risk), part_high_risk := "unknown"]

hr_levs <- c("yes", "no", "unknown")
hr_labs <- c("Yes", "No", "Unknown")

part[, part_high_risk := factor(part_high_risk, levels = hr_levs, labels = hr_labs)]

part[is.na(part_face_mask), part_face_mask := "unknown"]

fm_levs <- c("yes", "no", "unknown")
fm_labs <- c("Yes", "No", "Unknown")

part[, part_face_mask := factor(part_face_mask, levels = fm_levs, labels = fm_labs)]

part[part_employstatus=="employed full-time (34 hours or more)", part_employed := "Full time"]
part[part_employstatus=="employed part-time (less than 34 hours)", part_employed := "Part time"]
part[part_employstatus=="self employed", part_employed := "Self employed"]

## Created a workplace variables
part[part_work_closed == "no", part_work_place := "open"]
part[part_work_closed == "yes", part_work_place := "closed"]

# Set order perception variables
perc_levs <- c("Strongly agree","Tend to agree","Neither agree nor disagree","Tend to disagree","Strongly disagree","Don’t know")
perc_labs <- c("Strongly agree","Tend to agree","Neither agree nor disagree","Tend to disagree","Strongly disagree","Don’t know")

part[, part_att_likely := factor(part_att_likely, levels = perc_levs, labels = perc_labs,ordered=TRUE)]
part[, part_att_serious := factor(part_att_serious, levels = perc_levs, labels = perc_labs,ordered=TRUE)]
part[, part_att_spread := factor(part_att_spread, levels = perc_levs, labels = perc_labs,ordered=TRUE)]


# Merge on total contacts ------------------------------------------------------
p_cnts <- qs::qread('./data/part_cnts.qs')

# Add on contacts ---------------------------------------------------------
dt <- merge(part, p_cnts, by = "part_wave_uid", all.x = TRUE)

dt <- dt[!is.na(country)]
#dt <- dt[survey_round == 1000 | survey_round %in% 97:101]

## Cut contacts at 50
cnt_names <- grep("n_cnt", names(dt), value = TRUE)

pminv <- function(x) pmin(x,100)
dt_nocap <- copy(dt)
dt[, (cnt_names) := lapply(.SD, pminv), .SDcols= cnt_names] 
dt[, n_cnt_workschool := pmin(n_cnt_work + n_cnt_school, 100)]


## Save locally
qs::qsave(part, file = "data/wrapup_part.qs")
qs::qsave(contacts, file = "data/wrapup_contacts.qs")
qs::qsave(dt, file = "data/wrapup_part_cnts.qs")
qs::qsave(dt_nocap, file = "data/wrapup_part_cnts_nocap.qs")


