

## Packages
library(data.table)
library(lubridate)
library(flextable)
library(gtsummary)
library(magrittr)



# Load data ---------------------------------------------------------------
dt <- qs::qread('data/wrapup_part_cnts.qs')

dt <- dt[survey_round == 1000]
# Filter to needed vars ---------------------------------------------------
## At the end put all these at the begining

## Age, Gender, Household size, day of week, country, High risk, Face mask wearing
## Employed, attending work


dt <- dt[, .(
  part_uid,
  ## Characteristics
  part_age_group, 
  sample_type,
  part_gender, 
  hh_size_group, 
  hh_size,
  weekday,
  country,
  ## Risk perception
  part_att_likely,
  part_att_serious,
  part_att_spread,
  ## Risk mitigation
  part_face_mask,
  part_vacc,
  part_high_risk, 
  part_high_risk_v2,
  ## Symptoms
  part_symp_fever,
  part_symp_cough,
  part_symp_sob,
  part_symp_headache,
  part_symp_bodyaches,
  part_symp_congestion,
  part_symp_sore_throat,
  part_symp_fatigue,
  ## Extra
  part_employed,
  part_work_place,
  part_attend_work_yesterday,
  part_employstatus ## Added full emply-status
  )]

## Fixing part_employed to include unemployment and retired
dt[grepl("unemp",dt$part_employstatus), part_employed := "Unemployed"]
dt[dt$part_employstatus=="full-time parent homemaker", part_employed := "Unemployed"]
dt[dt$part_employstatus=="long-term sick or disabled", part_employed := "Unemployed"]
dt[dt$part_employstatus=="student/pupil", part_employed := "Unemployed"]
dt[dt$part_employstatus=="retired", part_employed := "Retired"]

dt[is.na(part_employed), part_employed := "remove"]
#dt[is.na(part_attend_work_yesterday), part_employed := "remove"]
#dt[is.na(part_work_place), part_employed := "remove"]
dt[, part_symp_ache := max(part_symp_bodyaches,
                           part_symp_headache,
                           na.rm = TRUE),
   by = part_uid]

dt[, part_symp_fatigue := as.integer(part_symp_fatigue)]

dt[, part_symp_any := max(part_symp_fever,
                          part_symp_cough,
                          part_symp_sob,
                          part_symp_ache,
                          part_symp_congestion,
                          part_symp_sore_throat,
                          part_symp_fatigue),
   by = part_uid]

dt_copy <- dt
dt_copy$new <- 1
dt$new <- 0
dta <- rbind(dt, dt_copy)

dta[new == 1, country := "All"]
dta[, table(country)]




# Get percentages ---------------------------------------------------------

## Will remove unknown and not include in perc calculation
## Using the get function turns the variable name to get
get_perc <- function(dt_, group_var_, adult = FALSE){
  top <- dt_[, .(num = .N), by = .(country, get(group_var_))]
  bottom <- dt_[!is.na(get(group_var_)) & !(get(group_var_) %in% c("Unknown", "Other", "remove")),
                .(denom = .N), by = country]
  x1 <- merge(top, bottom)
  x1[!(get %in% c("Unknown", "Other", "remove")), 
     text:= paste0(formatC(num, big.mark = ","), 
                   " (", 
                   formatC(num/denom*100, digits = 1, format = "f"),
                   "%)")]
  x1[get %in% c("Unknown", "Other"), 
     text:= formatC(num, big.mark = ",")]
  x1 <- x1[get != "remove"]
  x1
}


## Get percentages

## Characteristics
per_adult <- get_perc(dta, "sample_type")
per_age_child   <- get_perc(dta[sample_type == "child"], "part_age_group")
per_age_adult   <- get_perc(dta[sample_type == "adult"], "part_age_group")
per_gen   <- get_perc(dta, "part_gender")
per_hh    <- get_perc(dta, "hh_size_group")
per_day   <- get_perc(dta, "weekday")

## Risk perception
per_att_likely <- get_perc(dta[sample_type == "adult"] ,"part_att_likely")
per_att_serious <- get_perc(dta[sample_type == "adult"] ,"part_att_serious")
per_att_spread <- get_perc(dta[sample_type == "adult"] ,"part_att_spread")

## Risk mitigation
per_fm <- get_perc(dta[sample_type == "adult"], "part_face_mask")
per_vc <- get_perc(dta[sample_type == "adult"], "part_vacc")
per_hr <- get_perc(dta[sample_type == "adult"], "part_high_risk")

## Symptoms
per_symp_fev <- get_perc(dta[sample_type == "adult"], "part_symp_fever")
per_symp_cough <- get_perc(dta[sample_type == "adult"], "part_symp_cough")
per_symp_sob <- get_perc(dta[sample_type == "adult"], "part_symp_sob")
per_symp_ache <- get_perc(dta[sample_type == "adult"], "part_symp_ache")
per_symp_cong <- get_perc(dta[sample_type == "adult"], "part_symp_congestion")
per_symp_st <- get_perc(dta[sample_type == "adult"], "part_symp_sore_throat")
per_symp_fatigue <- get_perc(dta[sample_type == "adult"], "part_symp_fatigue")
per_symp_any <- get_perc(dta[sample_type == "adult"], "part_symp_any")


## Employment
per_employed <- get_perc(dta[sample_type == "adult"], "part_employed")
per_workopen <- get_perc(dta[sample_type == "adult"], "part_work_place")
per_attend <- get_perc(dta[sample_type == "adult"], "part_attend_work_yesterday")


## Now reshape the above function made all vars to get

## Characteristics
row_count <- dcast(dta, . ~ country, )
row_adult <- dcast(per_adult, get ~ country, value.var = "text")
row_age_child   <- dcast(per_age_child, get ~ country, value.var = "text")
row_age_adult   <- dcast(per_age_adult, get ~ country, value.var = "text")
row_gen   <- dcast(per_gen, get ~ country, value.var = "text")
row_hh    <- dcast(per_hh, get ~ country, value.var = "text")
row_day   <- dcast(per_day, get ~ country, value.var = "text")

## Risk perception
# row_likely <- dcast(per_att_likely[get == "Strongly agree"], get ~ country, value.var = "text")
# row_serious <- dcast(per_att_serious[get == "Strongly agree"], get ~ country, value.var = "text")
# row_spread <- dcast(per_att_spread[get == "Strongly agree"], get ~ country, value.var = "text")
row_likely <- dcast(per_att_likely, get ~ country, value.var = "text")
row_serious <- dcast(per_att_serious, get ~ country, value.var = "text")
row_spread <- dcast(per_att_spread, get ~ country, value.var = "text")


## Risk mitigation
row_fm   <- dcast(per_fm[get == "Yes"], get ~ country, value.var = "text")
row_vc   <- dcast(per_vc[get == "Yes"], get ~ country, value.var = "text")
row_hr   <- dcast(per_hr[get == "Yes"], get ~ country, value.var = "text")

## Symptoms
row_symp_fev <- dcast(per_symp_fev[get == 1], get ~ country, value.var = "text")
row_symp_cough <- dcast(per_symp_cough[get == 1], get ~ country, value.var = "text")
row_symp_sob <- dcast(per_symp_sob[get == 1], get ~ country, value.var = "text")
row_symp_ache <- dcast(per_symp_ache[get == 1], get ~ country, value.var = "text")
row_symp_cong <- dcast(per_symp_cong[get == 1], get ~ country, value.var = "text")
row_symp_st <- dcast(per_symp_st[get == 1], get ~ country, value.var = "text")
row_symp_fatigue <- dcast(per_symp_fatigue[get == 1], get ~ country, value.var = "text")
row_symp_any <- dcast(per_symp_any[get == 1], get ~ country, value.var = "text")


## Employment
row_employed   <- dcast(data = per_employed, get ~ country, value.var = "text")
#reorder the lines of row_employed to have the value of column "get" ordered not in alphabetical order but according to order c("Full time","Part time","Self employed,"Unemployed,"Retired")
row_employed <- row_employed[order(match(row_employed$get, c("Full time","Part time","Self employed","Unemployed","Retired")))]


row_workopen   <- dcast(per_workopen, get ~ country, value.var = "text")
row_attend   <- dcast(per_attend[get == "yes"], get ~ country, value.var = "text")

row_count[, All := formatC(All, big.mark = ",")] 
row_count[, UK  := formatC(UK, big.mark = ",")] 
row_count[, BE  := formatC(BE, big.mark = ",")] 
row_count[, NL  := formatC(NL, big.mark = ",")] 
row_count[, CH  := formatC(CH, big.mark = ",")] 

## Formatting for a row
make_row <- function(row_, cat , val, top_row = FALSE){
  row_[, Value := val]
  if(top_row){
    row_[1, Category := cat]
  }else{
  row_[, Category := cat]
  }  
  row_[, .(Category, Value, All, UK, BE, NL, CH)]
}

## Format each row, remember each var is now called get

## Characteristics
row_01   <- make_row(row_count, "All", "")
row_02   <- make_row(row_adult, c("Adult", "Child"), "")
row_03   <- make_row(row_age_child, cat = "Age group (Children)", val = row_age_child$get, top_row = TRUE)
row_04   <- make_row(row_age_adult, cat = "Age group (Adult)", val = row_age_adult$get, top_row = TRUE)
row_05   <- make_row(row_gen, cat = "Gender", val = row_gen$get, top_row = TRUE)
row_06   <- make_row(row_hh, cat = "Household size", val = row_hh$get, top_row = TRUE)
row_07   <- make_row(row_day, cat = "Day of week", val = row_day$get, top_row = TRUE)

## Risk perception
#row_08 <- make_row(row_likely, cat = "Risk perception (Adults)", val = "Catching coronavirus")
row_08 <- make_row(row_likely, top_row=TRUE, cat ="Perceived susceptibility (Adults)", val = row_likely$get)
#row_09 <- make_row(row_serious, cat = "", val = "Serious illness from coronavirus")
row_09 <- make_row(row_serious, cat = "Perceived severity (Adults)", val =row_serious$get , top_row=TRUE)
#row_10 <- make_row(row_spread, cat = "", val = "Spreading coronavirus to vulnerable people")
row_10 <- make_row(row_spread, cat = "Perceived risk to the vulnerable (Adults)", val = row_spread$get, top_row=TRUE)

## Risk mitigation
row_11   <- make_row(row_fm, cat = "Risk mitigation (Adults)", val = "Face mask")
row_12   <- make_row(row_vc, cat = "", val = "Vaccinated")
row_13   <- make_row(row_hr, cat = "", val = "High risk")

## Symptoms

row_14 <- make_row(row_symp_fev, cat = "Symptoms (Adults)", val = "Fever")
row_15 <- make_row(row_symp_cough, cat = "", val = "Cough")
row_16 <- make_row(row_symp_sob, cat = "", val = "Shortness of breath")
row_17 <- make_row(row_symp_ache, cat = "", val = "Head or body ache")
row_18 <- make_row(row_symp_cong, cat = "", val = "Congestion")
row_19 <- make_row(row_symp_st, cat = "", val = "Sore throat")
row_20 <- make_row(row_symp_fatigue, cat = "", val = "Fatigue or tiredness")
row_21 <- make_row(row_symp_any, cat = "", val = "Any symptoms")

## Work
row_22   <- make_row(row_employed, cat = "Employed (Adults)", val = row_employed$get, top_row = TRUE)
row_23   <- make_row(row_workopen, cat = "Work open (Adults)", val = row_workopen$get, top_row = TRUE)
row_24   <- make_row(row_attend, cat = "Attended work (Adults)", val = "Yes", top_row = TRUE)

tab1 <- rbind(
      row_01, 
      row_02,
      row_03,
      row_04,
      row_05,
      row_06,
      row_07,
      row_08,
      row_09,
      row_10,
      row_11,
      row_12,
      row_13,
      row_14,
      row_15,
      row_16,
      row_18,
      row_19,
      row_20,
      row_21,
      row_22,
      row_23,
      row_24
      ) %>% 
  flextable()

tab1


print(tab1, preview = "docx")

save_as_docx(tab1, path = "outputs/tab1_characteristics.docx")



