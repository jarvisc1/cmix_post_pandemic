

## Packages
library(data.table)
library(lubridate)
library(flextable)
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
  part_employstatus,
  ## Contacts
  n_cnt,
  n_cnt_work,
  n_cnt_home,
  n_cnt_other,
  n_cnt_school
)]


## Fixing part_employed to include unemployment and retired
dt[grepl("unemp",dt$part_employstatus), part_employed := "Unemployed"]
dt[dt$part_employstatus=="full-time parent, homemaker", part_employed := "Unemployed"]
dt[dt$part_employstatus=="long-term sick or disabled", part_employed := "Unemployed"]
dt[dt$part_employstatus=="student/pupil", part_employed := "Unemployed"]
dt[dt$part_employstatus=="retired", part_employed := "Retired"]

dt[is.na(part_employed), part_employed := "remove"]
#dt[is.na(part_attend_work_yesterday), part_employed := "remove"]
#dt[is.na(part_work_place), part_employed := "remove"]


# Fixing column with symptoms to be numbers
part_symp_cols <- grep("part_symp", names(dt), value = TRUE)
for (col in part_symp_cols) {
  set(dt, j = col, value = as.numeric(dt[[col]]))
}


dt[, part_symp_ache := max(part_symp_bodyaches,
                           part_symp_headache,
                           na.rm = TRUE),
   by = part_uid]



dt[, part_symp_any := max(part_symp_fever,
                          part_symp_cough,
                          part_symp_sob,
                          part_symp_ache,
                          part_symp_congestion,
                          part_symp_sore_throat,
                          part_symp_fatigue, na.rm = TRUE),
   by = part_uid]



dta <- dt


# Get means and sds ---------------------------------------------------------

## Will remove unkown and not include in perc calculation
## Using the get function turns the varibale name to get
get_mean <- function(dt_, group_var_, cnt_var = "n_cnt"){
  top <- dt_[, .(num = .N), by = .(country, get(group_var_))]
  contact_ <- dt_[, .(n_cnt = mean(get(cnt_var), na.rm = TRUE),
                      sd_n_cnt = sd(get(cnt_var), na.rm = TRUE)),
                  by = .(country, get(group_var_))]
  x1 <- merge(top, contact_)
  x1[!(get %in% c("Unknown", "Other", "remove")),
     text:= paste0(formatC(n_cnt, digits = 1, format = "f"),
                   " (",
                   formatC(sd_n_cnt, digits = 1, format = "f"),
                   ")")]
  x1[get %in% c("Unknown", "Other"),
     num:= formatC(num, big.mark = ",")]
  #x1 <- x1[get != "remove"]
  x1 <- x1[!(get %in% c("Unknown", "Other", "remove"))]
  
  x1
}


## Contact setting
contact_sett = "n_cnt"
# contact_sett = "n_cnt_work"
# contact_sett = "n_cnt_home"
# contact_sett = "n_cnt_school"
# contact_sett = "n_cnt_other"


## Characteristics
per_adult <- get_mean(dta, "sample_type", cnt_var = contact_sett)
per_age_child   <- get_mean(dta[sample_type == "child"], "part_age_group", cnt_var = contact_sett)
per_age_adult   <- get_mean(dta[sample_type == "adult"], "part_age_group", cnt_var = contact_sett)
per_gen   <- get_mean(dta, "part_gender", cnt_var = contact_sett)
per_hh    <- get_mean(dta, "hh_size_group", cnt_var = contact_sett)
per_day   <- get_mean(dta, "weekday", cnt_var = contact_sett)

## Risk mitigation
per_fm <- get_mean(dta[sample_type == "adult"], "part_face_mask", cnt_var = contact_sett)
per_vc <- get_mean(dta[sample_type == "adult"], "part_vacc", cnt_var = contact_sett)
per_hr <- get_mean(dta[sample_type == "adult"], "part_high_risk", cnt_var = contact_sett)

# Employment
per_employed <- get_mean(dta[sample_type == "adult"], "part_employed", cnt_var = contact_sett)
per_workopen <- get_mean(dta[sample_type == "adult"], "part_work_place", cnt_var = contact_sett)
per_attend <- get_mean(dta[sample_type == "adult"], "part_attend_work_yesterday", cnt_var = contact_sett)


mean_cnt <- dta[, .(text = paste0(
  formatC(mean(get(contact_sett)), digits = 1, format = "f"),
  " (",
  formatC(sd(get(contact_sett)), digits = 1, format = "f"),
  ")"
)
),
by = country]




## Now reshape the above function made all vars to get

## Characteristics
row_count <- dcast(mean_cnt, . ~ country)
row_adult <- dcast(per_adult, get ~ country, value.var = "text")
row_age_child   <- dcast(per_age_child, get ~ country, value.var = "text")
row_age_adult   <- dcast(per_age_adult, get ~ country, value.var = "text")
row_gen   <- dcast(per_gen, get ~ country, value.var = "text")
row_hh    <- dcast(per_hh, get ~ country, value.var = "text")
row_day   <- dcast(per_day, get ~ country, value.var = "text")
## Risk mitigation
row_fm   <- dcast(per_fm, get ~ country, value.var = "text")
row_vc   <- dcast(per_vc, get ~ country, value.var = "text")
row_hr   <- dcast(per_hr, get ~ country, value.var = "text")
# Employment
row_employed   <- dcast(per_employed, get ~ country, value.var = "text")
row_workopen   <- dcast(per_workopen, get ~ country, value.var = "text")
row_attend   <- dcast(per_attend, get ~ country, value.var = "text")
#row_cnt <- dcast(mean_cnt, . ~ country)

#row_count[, All := formatC(All, big.mark = ",")] 
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
  row_[, .(Category, Value, UK, BE, NL, CH)]
}

## Format each row, remember each var is now called get

## Characteristics
row_1   <- make_row(row_count, "All", "Mean (SD)")
row_2   <- make_row(row_adult, c("Adult", "Child"), "")
row_3   <- make_row(row_age_child, cat = "Age group (Children)", val = row_age_child$get, top_row = TRUE)
row_4   <- make_row(row_age_adult, cat = "Age group (Adult)", val = row_age_adult$get, top_row = TRUE)
row_5   <- make_row(row_gen, cat = "Gender", val = row_gen$get, top_row = TRUE)
row_6   <- make_row(row_hh, cat = "Household size", val = row_hh$get, top_row = TRUE)
row_7   <- make_row(row_day, cat = "Day of week", val = row_day$get, top_row = TRUE)

## Risk mitigation
row_8   <- make_row(row_fm, cat = "Face mask", val = row_fm$get, top_row = TRUE)
row_9   <- make_row(row_fm, cat = "Vaccinated", val = row_fm$get, top_row = TRUE)
row_10   <- make_row(row_hr, cat = "High risk", val = row_hr$get, top_row = TRUE)

# Employment
row_11   <- make_row(row_employed, cat = "Employed (Adults)", val = row_employed$get, top_row = TRUE)
row_12   <- make_row(row_workopen, cat = "Work open (Adults)", val = row_workopen$get, top_row = TRUE)
row_13   <- make_row(row_attend, cat = "Attended work (Adults)", val = row_attend$get, top_row = TRUE)

tab3 <- rbind(row_1, 
              row_2,
              row_3,
              row_4,
              row_5,
              row_6,
              row_7,
              row_8,
              row_9,
              row_10,
              row_11,
              row_12,
              row_13
              
) %>% 
  flextable()

tab3


#print(tab3, preview = "docx")

save_as_docx(tab3, path = "outputs/tab3_contacts.docx")
