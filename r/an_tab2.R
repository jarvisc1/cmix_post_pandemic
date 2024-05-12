## an_table_two_new

## Packages
library(data.table)
library(lubridate)
library(flextable)
library(gtsummary)
library(magrittr)
library(boot)



# Load data ---------------------------------------------------------------
dt <- qs::qread('data/wrapup_part_cnts.qs')


# Functions ---------------------------------------------------------------

##For calculating bootstrapped mean
part_cnt_means_subset <- function(vals, indices) {
  mean(vals[indices])
} 


# Filter to needed vars ---------------------------------------------------
## At the end put all these at the begining

## Just contacts, country, and whether adults or children
dt <- dt[, .(
  survey_round,
  ## Characteristics
  part_age_group, 
  sample_type,
  country,
  ## Contacts
  n_cnt,
  n_cnt_home,
  n_cnt_work,
  n_cnt_school,
  n_cnt_other,
  dayweight
)]

dt <- dt[survey_round == 1000]

dt$survey_round <- NULL

## Functions for creating text
boot_95 <- function(x, n = 1000, weekday_weights){
  bsr <- boot(x, part_cnt_means_subset, R=n, weights = weekday_weights)
  bsr_ci <- boot.ci(bsr, index=1, type = "perc")
  bs_mean = mean(bsr$t)
  mean_low_ci = bsr_ci$percent[4]
  mean_high_ci = bsr_ci$percent[5]
  paste0(formatC(bs_mean, digits = 1, format = "f"), " (",
         formatC(mean_low_ci, digits = 1, format = "f"), " to ",
         formatC(mean_high_ci, digits = 1, format = "f"), ")")
}

mean(dt$n_cnt_work)
boot_95(dt$n_cnt_work, n = 1000, dt$dayweight)

bs_samp <- 1000

dt_means_all <- dt[, .(
  All    = boot_95(n_cnt, n = bs_samp, weekday_weights = dayweight),
  Home   = boot_95(n_cnt_home, n = bs_samp, weekday_weights = dayweight),
  Work   = boot_95(n_cnt_work, n = bs_samp, weekday_weights = dayweight),
  School = boot_95(n_cnt_school, n = bs_samp, weekday_weights = dayweight),
  Other  = boot_95(n_cnt_other, n = bs_samp, weekday_weights = dayweight)
  ), 
   by = .(country)]

dt_means_adult <- dt[sample_type == "adult", .(
  All    = boot_95(n_cnt, n = bs_samp, weekday_weights = dayweight),
  Home   = boot_95(n_cnt_home, n = bs_samp, weekday_weights = dayweight),
  Work   = boot_95(n_cnt_work, n = bs_samp, weekday_weights = dayweight),
  School = boot_95(n_cnt_school, n = bs_samp, weekday_weights = dayweight),
  Other  = boot_95(n_cnt_other, n = bs_samp, weekday_weights = dayweight)
  ), 
   by = .(country)]

dt_means_child <- dt[sample_type == "child", .(
  All    = boot_95(n_cnt, n = bs_samp, weekday_weights = dayweight),
  Home   = boot_95(n_cnt_home, n = bs_samp, weekday_weights = dayweight),
  Work   = boot_95(n_cnt_work, n = bs_samp, weekday_weights = dayweight),
  School = boot_95(n_cnt_school, n = bs_samp, weekday_weights = dayweight),
  Other  = boot_95(n_cnt_other, n = bs_samp, weekday_weights = dayweight)
  ), 
   by = .(country)]


all_mean <- dt_means_all %>% 
  melt(id.vars = "country") %>% 
  dcast(variable ~ country)
adult_mean <- dt_means_adult %>% 
  melt(id.vars = "country") %>% 
  dcast(variable ~ country)
child_mean <- dt_means_child %>% 
  melt(id.vars = "country") %>% 
  dcast(variable ~ country)

## Title rows for table two
empty_row <- data.table(variable = "",
                  UK = "",
                  BE = "",
                  NL = "",
                  CH = ""
                  )
# Create empty sample column
all_mean[, sample := ""]
adult_mean[, sample := ""]
child_mean[, sample := ""]

## Reorder columns
all_mean <- all_mean[, .(sample, variable, UK, BE, NL, CH)]
adult_mean <- adult_mean[, .(sample, variable, UK, BE, NL, CH)]
child_mean <- child_mean[, .(sample, variable, UK, BE, NL, CH)]

all_row <- data.table(
                  sample = "All",
                  variable = "",
                  UK = "",
                  BE = "",
                  NL = "",
                  CH = ""
                  )
adult_row <- data.table(
                  sample = "Adults",
                  variable = "",
                  UK = "",
                  BE = "",
                  NL = "",
                  CH = ""
                  )
child_row <- data.table(
                  sample = "Children",
                  variable = "",
                  UK = "",
                  BE = "",
                  NL = "",
                  CH = ""
                  )


combined_dta <- rbind(
  all_row,
  all_mean,
  adult_row,
  adult_mean,
  child_row,
  child_mean)
  



####### add POLYMOD

library(socialmixr)
library(data.table)

data("polymod")

source('r/functions/functions.R')

parts_poly = data.table(polymod$participants)
conts_poly = data.table(polymod$contacts)

parts_poly[!is.na(part_age), part_age_est_min := part_age]
parts_poly[!is.na(part_age), part_age_est_max := part_age]
conts_poly[!is.na(cnt_age_exact), cnt_age_est_min := cnt_age_exact]
conts_poly[!is.na(cnt_age_exact), cnt_age_est_max := cnt_age_exact]
conts_poly = conts_poly[!is.na(cnt_age_est_max)]

## Add in weekday var
weekday_ <- function(x){
  x[dayofweek == 0, weekday := "Sunday"]
  x[dayofweek == 1, weekday := "Monday"]
  x[dayofweek == 2, weekday := "Tuesday"]
  x[dayofweek == 3, weekday := "Wednesday"]
  x[dayofweek == 4, weekday := "Thursday"]
  x[dayofweek == 5, weekday := "Friday"]
  x[dayofweek == 6, weekday := "Saturday"]
  x
}

parts_poly <- weekday_(parts_poly)

conts_poly <- merge(conts_poly, parts_poly, by = "part_id")
conts_poly <- weekday_(conts_poly)

## Add in survey round
parts_poly[, survey_round := 1]
conts_poly[, survey_round := 1]



## Subset

## Get for each country
parts_poly_uk = parts_poly[country == 'United Kingdom']
parts_poly_be = parts_poly[country == 'Belgium']
conts_poly_uk = conts_poly[part_id %in% parts_poly_uk$part_id]
conts_poly_be = conts_poly[part_id %in% parts_poly_be$part_id]


# NL from a different data source -----------------------------------------
parts_poly_nl <- as.data.table(read.delim(file = "data/Participants_20071108_NL.txt"))
conts_poly_nl <- as.data.table(read.delim(file = "data/Contacts_20071108_NL.txt"))



# Select relevant columns
parts_poly_nl  <- subset(parts_poly_nl, select = c("local_id", "participant_age", "dayofweek"))

conts_poly_nl  <- subset(conts_poly_nl, select = c("local_id","cnt_home"
                                                   ,"cnt_work"
                                                   ,"cnt_school"
                                                   ,"cnt_transport"
                                                   ,"cnt_leisure"
                                                   ,"cnt_otherplace"
                                                    ))
#substitute the "x" with a TRUE and make the column a boolean for columns: cnt_home,cnt_work,cnt_school,cnt_transport,cnt_leisure,cnt_otherplace
conts_poly_nl[, cnt_home := ifelse(cnt_home == "x", TRUE, FALSE)][, cnt_work := ifelse(cnt_work == "x", TRUE, FALSE)
                ][, cnt_school := ifelse(cnt_school == "x", TRUE, FALSE)][, cnt_transport := ifelse(cnt_transport == "x", TRUE, FALSE)
                    ][, cnt_leisure := ifelse(cnt_leisure == "x", TRUE, FALSE)][, cnt_otherplace := ifelse(cnt_otherplace == "x", TRUE, FALSE)]


colnames(parts_poly_nl) <- c("part_id", "part_age", "dayofweek")
colnames(conts_poly_nl) <- c("part_id","cnt_home","cnt_work","cnt_school","cnt_transport","cnt_leisure","cnt_otherplace")

conts_poly_nl <- merge(conts_poly_nl, parts_poly_nl[,c("part_id", "dayofweek")], by = "part_id")
# 
# conts_poly_nl[, cnt_age_est_min := as.integer(cnt_age_est_min)]
# conts_poly_nl[is.na(cnt_age_est_max), cnt_age_est_max := as.integer(cnt_age_est_min)]
# # 
# parts_poly_nl[!is.na(part_age), part_age_est_min := part_age]
# parts_poly_nl[!is.na(part_age), part_age_est_max := part_age]
# 
## Add in named days
parts_poly_nl <- weekday_(parts_poly_nl)
conts_poly_nl <- weekday_(conts_poly_nl)

parts_poly_nl[, survey_round := 1]
conts_poly_nl[, survey_round := 1]

# set country
parts_poly_uk$country<-"UK (POLYMOD)"
parts_poly_be$country<-"BE (POLYMOD)"
parts_poly_nl$country<-"NL (POLYMOD)"
common_columns<-c("part_id","part_age","dayofweek","weekday","survey_round","country")
parts_poly_subset<-rbind(parts_poly_uk[,..common_columns],parts_poly_nl,parts_poly_be[,..common_columns])

common_columns<-c("part_id","cnt_home","cnt_work","cnt_school","cnt_transport","cnt_leisure","cnt_otherplace",
                 "dayofweek","weekday","survey_round"  )
conts_poly_subset<-rbind(conts_poly_uk[,..common_columns],conts_poly_nl,conts_poly_be[,..common_columns])


result <- conts_poly_subset[, .(
  n_cnt= sum((cnt_home == TRUE)|(cnt_work == TRUE)|(cnt_transport == TRUE)|(cnt_school == TRUE)|(cnt_leisure == TRUE)|(cnt_otherplace == TRUE)) 
  ,n_cnt_home = sum(cnt_home == TRUE)
  ,n_cnt_work = sum(cnt_work == TRUE)
  ,n_cnt_school = sum(cnt_school == TRUE)
  ,n_cnt_other = sum((cnt_transport == TRUE)|(cnt_leisure == TRUE)|(cnt_otherplace == TRUE) ) 
  
), by = part_id]

dt_polymod<-merge(result, parts_poly_subset[,c("part_id","part_age","country","weekday")], by="part_id")
# set dayweight=5/7 if column weekday is a weekday, else set dayweight=2/7
dt_polymod[, dayweight := ifelse(weekday %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), 5/7, 2/7)]


dt_polymod$n_cnt[is.na(dt_polymod$n_cnt)]<-0
dt_polymod$n_cnt_home[is.na(dt_polymod$n_cnt_home)]<-0
dt_polymod$n_cnt_work[is.na(dt_polymod$n_cnt_work)]<-0
dt_polymod$n_cnt_school[is.na(dt_polymod$n_cnt_school)]<-0
dt_polymod$n_cnt_other[is.na(dt_polymod$n_cnt_other)]<-0

dt_polymod$n_cnt_work[is.na(dt_polymod$n_cnt_work)]<-0
boot_95(dt$n_cnt_work, n = 1000, dt$dayweight)


dt_means_all_polymod <- dt_polymod[, .(
  All    = boot_95(n_cnt, n = bs_samp, weekday_weights = dayweight),
  Home   = boot_95(n_cnt_home, n = bs_samp, weekday_weights = dayweight),
  Work   = boot_95(n_cnt_work, n = bs_samp, weekday_weights = dayweight),
  School = boot_95(n_cnt_school, n = bs_samp, weekday_weights = dayweight),
  Other  = boot_95(n_cnt_other, n = bs_samp, weekday_weights = dayweight)
), 
by = .(country)]



dt_means_adult_polymod <- dt_polymod[part_age >= 18, .(
  All    = boot_95(n_cnt, n = bs_samp, weekday_weights = dayweight),
  Home   = boot_95(n_cnt_home, n = bs_samp, weekday_weights = dayweight),
  Work   = boot_95(n_cnt_work, n = bs_samp, weekday_weights = dayweight),
  School = boot_95(n_cnt_school, n = bs_samp, weekday_weights = dayweight),
  Other  = boot_95(n_cnt_other, n = bs_samp, weekday_weights = dayweight)
), 
by = .(country)]

dt_means_child_polymod <- dt_polymod[part_age < 18, .(
  All    = boot_95(n_cnt, n = bs_samp, weekday_weights = dayweight),
  Home   = boot_95(n_cnt_home, n = bs_samp, weekday_weights = dayweight),
  Work   = boot_95(n_cnt_work, n = bs_samp, weekday_weights = dayweight),
  School = boot_95(n_cnt_school, n = bs_samp, weekday_weights = dayweight),
  Other  = boot_95(n_cnt_other, n = bs_samp, weekday_weights = dayweight)
), 
by = .(country)]


all_mean_polymod <- dt_means_all_polymod %>% 
  melt(id.vars = "country") %>% 
  dcast(variable ~ country)
adult_mean_polymod <- dt_means_adult_polymod %>% 
  melt(id.vars = "country") %>% 
  dcast(variable ~ country)
child_mean_polymod <- dt_means_child_polymod %>% 
  melt(id.vars = "country") %>% 
  dcast(variable ~ country)

## Title rows for table two
empty_row_polymod <- data.table(variable = "",
                        `UK (POLYMOD)`= "",
                        `BE (POLYMOD)`= "",
                        `NL (POLYMOD)`= ""
                        
)
# Create empty sample column
all_mean_polymod[, sample := ""]
adult_mean_polymod[, sample := ""]
child_mean_polymod[, sample := ""]

## merge CoMix and Polymod
all_mean <- cbind(all_mean, all_mean_polymod)
adult_mean <- cbind(adult_mean, adult_mean_polymod)
child_mean <- cbind(child_mean, child_mean_polymod)

## Reorder columns
all_mean <- all_mean[, .(sample, variable, UK,`UK (POLYMOD)`, BE,`BE (POLYMOD)`, NL,`NL (POLYMOD)`, CH)]
adult_mean <- adult_mean[, .(sample, variable, UK,`UK (POLYMOD)`, BE,`BE (POLYMOD)`, NL,`NL (POLYMOD)`, CH)]
child_mean <- child_mean[, .(sample, variable, UK,`UK (POLYMOD)`, BE,`BE (POLYMOD)`, NL,`NL (POLYMOD)`, CH)]

all_row <- data.table(
  sample = "All",
  variable = "",
  UK = "",
  `UK (POLYMOD)`= "",
  BE = "",
  `BE (POLYMOD)`= "",
  NL = "",
  `NL (POLYMOD)`= "",
  CH = ""
)
adult_row <- data.table(
  sample = "Adults",
  variable = "",
  UK = "",
  `UK (POLYMOD)`= "",
  BE = "",
  `BE (POLYMOD)`= "",
  NL = "",
  `NL (POLYMOD)`= "",
  CH = ""
)
child_row <- data.table(
  sample = "Children",
  variable = "",
  UK = "",
  `UK (POLYMOD)`= "",
  BE = "",
  `BE (POLYMOD)`= "",
  NL = "",
  `NL (POLYMOD)`= "",
  CH = ""
)


combined_dta <- rbind(
  all_row,
  all_mean,
  adult_row,
  adult_mean,
  child_row,
  child_mean)



tab2 <- combined_dta %>% 
  flextable()
save_as_docx(tab2, path = "outputs/tab2_contacts.docx")