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
  


tab2 <- combined_dta %>% 
  flextable()


save_as_docx(tab2, path = "outputs/tab2_contacts.docx")


