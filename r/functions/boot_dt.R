
part_cnt_means_subset <- function(vals, indices) {
  mean(vals[indices])
}


varlab <- function(var) {
  varlookup <- c(
    n_cnt_all = "All",
    n_cnt_home = "Home",
    n_cnt_work = "Work",
    n_cnt_school = "School",
    n_cnt_other = "Other"
                 )
  varlookup[[var]]
}

k <- varlab("n_cnt_other")
k <- varlab("n_cnt_all")


boot_dt <- function(dt, cnts, age_group = "All", gender = "All", region = "All", sociogroup = "All", ethnicity = "All", week_, weekday_weights = TRUE){
  
  dt_subset <-  dt[week %in% week_]
  
  ## Restrict data based on options.
  
  if(!grepl("All", age_group)) dt_subset <- dt_subset[part_age_group == age_group]
  if(!grepl("All", gender)) dt_subset <- dt_subset[part_gender == gender]
  if(!grepl("All", region)) dt_subset <- dt_subset[regions == region]
  if(!grepl("All", sociogroup)) dt_subset <- dt_subset[part_social_group == sociogroup]
  if(!grepl("All", ethnicity)) dt_subset <- dt_subset[part_ethnicity == ethnicity]
  if(age_group == "All-Adults") dt_subset <- dt_subset[!part_age_group %in% c("[0,5)", "[5,12)",  "[12,18)")]
  if(region == "All-Adults") dt_subset <- dt_subset[!part_age_group %in% c("[0,5)", "[5,12)",  "[12,18)")]
  if(sociogroup == "All-Adults") dt_subset <- dt_subset[!part_age_group %in% c("[0,5)", "[5,12)",  "[12,18)")]
  if(ethnicity == "All-Adults") dt_subset <- dt_subset[!part_age_group %in% c("[0,5)", "[5,12)",  "[12,18)")]
  n <- nrow(dt_subset)
  
  if(weekday_weights){
    weekday_weights <- dt_subset$weekday_weights
  } else{
    weekday_weights <- rep(1, nrow(dt_subset))
  }

  bsr <- boot(dt_subset[[cnts]], part_cnt_means_subset, R=1000, weights = weekday_weights)
  bsr_ci <- boot.ci(bsr, index=1, type = "perc")
  setting_ <- varlab(cnts)
  setting_
  
  week_min <- min(week_)
  week_max <- max(week_)
  week_ <- week_min
  data.table(
    n = n,
    part_age_group = age_group,
    part_gender = gender,
    part_region = region,
    part_social_group = sociogroup,
    part_ethnicity = ethnicity,
    date_category = week_,
    week_min = week_min,
    week_max = week_max,
    setting = setting_,
    mean_raw = bsr$t0,
    bs_mean = mean(bsr$t),
    bs_sd = sd(bsr$t),
    mean_low_ci = bsr_ci$percent[4],
    mean_high_ci = bsr_ci$percent[5],
    ci_level = bsr_ci$percent[1],
    R = bsr$R
  )
}

