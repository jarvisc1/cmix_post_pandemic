# Contains functions and commonly used objects


spss_col_to_date <- function(col) {
  col <- unlist(lapply(col, spss_to_date))
  return(col)
}

spss_to_date <- function(x) {
  if(!is.na(x)) {
    x <- as.Date(as.numeric(x)/86400, origin = "1582-10-14")
  }
  return(x)
}

## AGE

age_bins <- c(0,1,5*(1:14))
age_bin_levels <- c(
  "<=1",
  "1-5",
  "6-10",
  "11-15",
  "16-20",
  
  "21-25",
  "26-30",
  "31-35",
  "36-40",
  "41-45",
  "46-50",
  "51-55",
  "56-60",
  "61-65",
  "66+"
)

# WIll remove when data received
rnorm_age_bins <- function(RESP_YEARS) {
  x <- rtruncnorm(1, 0,70, 
                  mean = RESP_YEARS, sd = RESP_YEARS * 0.8)
  # bins <- c(0,1,5*(1:14))
  x <- cut(x, age_bins, labels = age_bin_levels)
  return(x)
}

numeric_age_col <- function(age_bin, level) {
  #level must be "low" or "high"
  age_bin <- as.character(age_bin)
  regx = "-| |\\+|<|>|="
  age_split <- as.numeric(strsplit(age_bin, split = regx)[[1]])
  i <- case_when(level == "low" ~ 1, 
                 level == "high" ~ as.numeric(length(age_split)))
  age <- as.numeric(age_split[i])
  return(age)
}

## Belief Scale

likert_scale_levels = 1:6
likert_scale_labels <- c("Strongly agree",
                  "Tend to agree",
                  "Neither agree nor disagree",
                  "Tend to disagree",
                  "Strongly disagree",
                  "Don’t know")

matrix_age_bins <- c(0,1,5,15,60,75)




## For plots
age_labs <- c("0-4",
              "5-12", 
              "13-17",
              "18-29",
              "30-39",
              "40-49",
              "50-59",
              "60-69",
              "70+")


add_contact_count_col <- function(part_dt, cont_dt, new_var_name, filter_exp, 
                                  by = c("panel", "wave", "country_code", "part_id")) {
  count <- cont_dt[eval(filter_exp), .(var = .N), by = by]
  part_dt <- merge(part_dt, count, by = by, all.x = TRUE )
  part_dt[is.na(var), var := 0]
  setnames(part_dt, old = "var", new = new_var_name)
  part_dt
}
  

add_n_cnts_location_cols <- function(part_dt, cont_dt, replace_existing_cols = FALSE) {
  # change to replace_existing_cols = TRUE if columns already exist
  # default FALSE prevents unintended behavior
  original_part_dt <- part_dt
  n_cnt_names <- grep("n_cnt_", names(part_dt), value = T)
  if (replace_existing_cols) {
    message("replacing n_cnt_* columns")
    part_dt <- part_dt[, -n_cnt_names, with = FALSE]
  }
  
  
  count_list <- list(
    # ALL CONTACTs
    list(varname = "n_cnt_all", exp =  expression(TRUE)),
    list(varname = "n_cnt_work", exp = expression(cnt_work == "Yes")),
    list(varname = "n_cnt_school", exp = expression(cnt_school == "Yes")),
    list(varname = "n_cnt_home", exp = expression(cnt_home == "Yes")),
    list(varname = "n_cnt_inside", exp = expression(cnt_inside == "Yes")),
    list(varname = "n_cnt_outside", exp = expression(cnt_outside == "Yes")),
    list(varname = "n_cnt_not_home", exp = expression(cnt_home != "Yes")),
    list(varname = "n_cnt_not_home_not_household", exp = expression(cnt_home != "Yes" & hhm_contact_yn != "Yes")),
    list(varname = "n_cnt_household", exp = expression(hhm_contact_yn == "Yes")),
    list(varname = "n_cnt_not_household", exp = expression(hhm_contact_yn != "Yes")),
    list(varname = "n_cnt_home_household", 
         exp = expression(hhm_contact_yn == "Yes" & cnt_home == "Yes")),
    list(varname = "n_cnt_home_not_household", 
         exp = expression(hhm_contact_yn != "Yes" & cnt_home == "Yes")),
    list(varname = "n_cnt_home_other_home_not_household", 
         exp = expression(hhm_contact_yn != "Yes" & 
                            (cnt_home == "Yes" | cnt_other_house == "Yes"))),
    list(varname = "n_cnt_other", 
         exp = expression(cnt_home == "No" & cnt_work == "No" & cnt_school == "No")),
    list(varname = "n_cnt_outside_other", 
         exp = expression(cnt_home == "No" & cnt_work == "No" & cnt_school == "No" & cnt_outside == "Yes")),
    list(varname = "n_cnt_inside_other", 
         exp = expression(cnt_home == "No" & cnt_work == "No" & cnt_school == "No" & cnt_inside == "Yes")),
    list(varname = "n_cnt_outside_not_household", 
         exp = expression(hhm_contact_yn != "Yes" & cnt_outside == "Yes")),
    list(varname = "n_cnt_inside_not_household", 
         exp = expression(hhm_contact_yn != "Yes" & cnt_inside == "Yes")),
    # PHYSICAL CONTACT
    list(varname = "n_cnt_all_phys", exp =  expression(phys_contact == 1)),
    list(varname = "n_cnt_work_phys", 
         exp = expression(phys_contact == 1 & cnt_work == "Yes")),
    list(varname = "n_cnt_school_phys", 
         exp = expression(phys_contact == 1 & cnt_school == "Yes")),
    list(varname = "n_cnt_home_phys", 
         exp = expression(phys_contact == 1 & cnt_home == "Yes")),
    list(varname = "n_cnt_inside_phys", 
         exp = expression(phys_contact == 1 & cnt_inside == "Yes")),
    list(varname = "n_cnt_outside_phys",
         exp = expression(phys_contact == 1 & cnt_outside == "Yes")),
    list(varname = "n_cnt_not_home_phys", 
         exp = expression(phys_contact == 1 & cnt_home == "No")),
    list(varname = "n_cnt_household_phys", 
         exp = expression(phys_contact == 1 & hhm_contact_yn == "Yes")),
    list(varname = "n_cnt_not_household_phys", 
         exp = expression(phys_contact == 1 & hhm_contact_yn != "Yes")),
    list(varname = "n_cnt_home_household_phys", 
         exp = expression(phys_contact == 1 & hhm_contact_yn == "Yes" & cnt_home == "Yes")),
    list(varname = "n_cnt_home_not_household_phys", 
         exp = expression(phys_contact == 1 & hhm_contact_yn == "No" & cnt_home == "Yes")),
    list(varname = "n_cnt_other_phys", 
         exp = expression(phys_contact == 1 & cnt_home == "No" & cnt_work == "No" 
                          & cnt_school == "No")),
    
    # PLACES
    # 
    # list(varname = "n_cnt_health_facility",
    #      exp =  expression(cnt_health_facility == "cnt_health_facility")),
    # list(varname = "n_cnt_leisure", exp =  expression(cnt_leisure == "Yes")),
    # list(varname = "n_cnt_otheryn", exp =  expression(cnt_otheryn == "Yes")),
    # list(varname = "n_cnt_other_house",
    #      exp =  expression(cnt_other_house == "Yes")),
    # 
    # list(varname = "n_cnt_public_transport",
    #      exp =  expression(cnt_public_transport == "Yes")),
    # list(varname = "n_cnt_salon", exp =  expression(cnt_salon == "Yes")),
    # list(varname = "n_cnt_shop", exp =  expression(cnt_shop == "Yes")),
    # list(varname = "n_cnt_sport", exp =  expression(cnt_sport == "Yes")),
    # 
    # list(varname = "n_cnt_supermarket",exp =  expression(cnt_sport == "Yes")),
    # list(varname = "n_cnt_worship", exp =  expression(cnt_sport == "Yes")),
    
    # CONTACT FLAGS
    list(varname = "n_cnt_individual_identified", 
         exp = expression(individual_identified == 1)),
    list(varname = "n_cnt_suspected_non_contact", 
         exp = expression( suspected_non_contact == 1)),
    list(varname = "n_cnt_potential_hhm", exp = expression(potential_hhm == 1)),
    list(varname = "n_cnt_suspected_multiple_contact", 
         exp = expression(suspected_multiple_contact == 1)),
    
    # MASS CONTACTS
    list(varname = "n_cnt_individually_reported", 
         exp = expression(individually_reported == 1)),
    list(varname = "n_cnt_mass_reported",
         exp = expression(individually_reported == 0))
  )
  
  # Add each column
  for (count_item in count_list) {
    by <- c("panel", "wave", "country_code", "part_id")
    part_dt <- add_contact_count_col(part_dt, cont_dt, count_item$varname, 
                                     count_item$exp, by)
  }
  
  return(part_dt)
}




poly_add_n_cnts_location_cols <- function(part_dt, cont_dt, replace_existing_cols = FALSE) {
  # change to replace_existing_cols = TRUE if columns already exist
  # default FALSE prevents unintended behavior
  original_part_dt <- part_dt
  n_cnt_names <- grep("n_cnt_", names(part_dt), value = T)
  if (replace_existing_cols) {
    message("replacing n_cnt_* columns")
    part_dt <- part_dt[, -n_cnt_names, with = FALSE]
  }
  
  count_list <- list(
    # ALL CONTACTs
    list(varname = "n_cnt_all", exp =  expression(TRUE)),
    list(varname = "n_cnt_work", exp = expression(cnt_work == 1)),
    list(varname = "n_cnt_school", exp = expression(cnt_school == 1)),
    list(varname = "n_cnt_home", exp = expression(cnt_home == 1))#,
    
    # PHYSICAL CONTACT
    # list(varname = "n_cnt_all_phys", exp =  expression(phys_contact == 1)),
    # list(varname = "n_cnt_work_phys", 
    #      exp = expression(phys_contact == 1 & cnt_work == "Yes")),
    # list(varname = "n_cnt_school_phys", 
    #      exp = expression(phys_contact == 1 & cnt_school == "Yes")),
    # list(varname = "n_cnt_home_phys", 
    #      exp = expression(phys_contact == 1 & cnt_home == "Yes")),
    # list(varname = "n_cnt_inside_phys", 
  )
  
  # Add each column
  for (count_item in count_list) {
    # count_item <- count_list[[1]]
    by <- c("part_id")
    part_dt <- add_contact_count_col(part_dt, cont_dt, count_item$varname, 
                                     count_item$exp, by = by)
  }
  
  return(part_dt)
}
# 
# 
# add_n_cnts_location_cols_extra <- function(part_dt, cont_dt, replace_existing_cols = FALSE) {
#   # change to replace_existing_cols = TRUE if columns already exist
#   # default FALSE prevents unintended behavior
#   original_part_dt <- part_dt
#   n_cnt_names <- grep("n_cnt_", names(part_dt), value = T)
#   if (replace_existing_cols) {
#     message("replacing n_cnt_* columns")
#     part_dt <- part_dt[, -n_cnt_names, with = FALSE]
#   }
#   
#   count_list <- list(
#     # PLACES
#     list(varname = "n_cnt_health_facility", 
#          exp =  expression(cnt_sport == "cnt_health_facility")),
#     list(varname = "n_cnt_leisure", exp =  expression(cnt_leisure == "Yes")),
#     list(varname = "n_cnt_otheryn", exp =  expression(cnt_otheryn == "Yes")),
#     list(varname = "n_cnt_other_house", 
#          exp =  expression(cnt_other_house == "Yes")),
#     
#     list(varname = "n_cnt_public_transport", 
#          exp =  expression(cnt_public_transport == "Yes")),
#     list(varname = "n_cnt_salon", exp =  expression(cnt_salon == "Yes")),
#     list(varname = "n_cnt_shop", exp =  expression(cnt_shop == "Yes")),
#     list(varname = "n_cnt_sport", exp =  expression(cnt_sport == "Yes")),
#     
#     list(varname = "n_cnt_supermarket",exp =  expression(cnt_sport == "Yes")),
#     list(varname = "n_cnt_worship", exp =  expression(cnt_sport == "Yes"))
#   )
#   
#   # Add each column
#   for (count_item in count_list) {
#     by <- c("panel", "wave", "country_code", "part_id")
#     part_dt <- add_contact_count_col(part_dt, cont_dt, count_item$varname, 
#                                      count_item$exp, by)
#   }
#   
#   return(part_dt)
# }
# 
# 
# add_mask_cols <- function(part_dt, cont_dt, replace_existing_cols = FALSE) {
#   # change to replace_existing_cols = TRUE if columns already exist
#   # default FALSE prevents unintended behavior
#   # original_part_dt <- part_dt
#   # n_cnt_names <- grep("n_cnt_", names(part_dt), value = T)
#   # if (replace_existing_cols) {
#   #   message("replacing n_cnt_* columns")
#   #   part_dt <- part_dt[, -n_cnt_names, with = FALSE]
#   # }
#   
#   count_list <- list(
#     
#   )
#   
#   # Add each column
#   for (count_item in count_list) {
#     by <- c("panel", "wave", "country_code", "part_id")
#     part_dt <- add_contact_count_col(part_dt, cont_dt, count_item$varname, 
#                                      count_item$exp, by)
#   }
#   
#   return(part_dt)
# }



add_week_number <- function(dt) {
  dt[ , wave_id := toupper(paste(gsub("panel ", "", tolower(as.character(panel))), 
                              gsub("wave ", "", tolower(as.character(wave)))))]
  dt[ , week := fcase(wave_id == "A 1", 1,
                      wave_id == "A 2", 3,
                      wave_id == "A 3", 5,
                      wave_id == "A 4", 7,
                      wave_id == "A 5", 9,
                      wave_id == "A 6", 11,
                      wave_id == "A 7", 13,
                      wave_id == "A 8", 15,
                      wave_id == "A 9", 17,
                      wave_id == "A 10", 19,
                      
                      wave_id == "B 1", 2,
                      wave_id == "B 2", 4,
                      wave_id == "B 3", 6,
                      wave_id == "B 4", 8,
                      wave_id == "B 5", 10,
                      wave_id == "B 6", 12,
                      wave_id == "B 7", 14,
                      wave_id == "B 8", 16,
                      wave_id == "B 9", 18,
                      
                      wave_id == "C 1", 7,
                      wave_id == "C 2", 9,
                      wave_id == "C 3", 11,
                      wave_id == "C 4", 13,
                      wave_id == "C 5", 15,
                      wave_id == "C 6", 19,
                      
                      wave_id == "D 1", 8,
                      wave_id == "D 2", 10,
                      wave_id == "D 3", 12,
                      wave_id == "D 4", 14,
                      wave_id == "D 5", 16,
                      
                      wave_id == "E 1", 20,
                      wave_id == "EC 1", 20,
                      wave_id == "F 1", 21,
                      wave_id == "FC 1", 21,
                      wave_id == "E 2", 22,
                      wave_id == "EC 2", 22,
                      wave_id == "F 2", 23,
                      wave_id == "FC 2", 23,
                      wave_id == "E 3", 24,
                      wave_id == "EC 3", 24,
                      wave_id == "F 3", 25,
                      wave_id == "FC 3", 25,
                      wave_id == "E 4", 26,
                      wave_id == "EC 4", 26,
                      wave_id == "F 4", 27,
                      wave_id == "FC 4", 27

                      )]
  return(dt)
}


table_to_df <- function(table_expression) {
  df <- data.frame(unclass(eval(table_expression)))
  df$value <- rownames(df)
  nl <- length(names(df))
  order <- dt[,c(nl, 1:(nl - 1))]
  df <- df[, order]
  dt <- as.data.table(df)

  cols <- names(dt)[-1]
  dt[, cols] <- dt[, lapply(.SD, as.numeric), .SDcols = cols]
  totals <- colSums(dt[, c(2:nl), with = F])
  totals <- as.data.table(t(totals))
  totals[, value := "TOTAL"]
  dt <- rbind(dt, totals, use.names = TRUE)
  return(dt)
}



create_comix_survey_obj <- function(part, contacts) {
  
  contacts_m <- contacts
  contacts_m$cnt_age <- NULL # Need to remove for social mixr
  
  part_m <- part[, .(
    date,
    country,
    part_id,
    part_gender,
    part_age,
    part_pregnant,
    part_isolate,
    part_quarantine,
    part_limit_work,
    part_limit_school,
    part_work_closed,
    part_covid_test_result,
    part_covid_contact,
    area_2_name,
    area_3_name,
    hh_size
  )
  ]
  
  
  comix_survey <- socialmixr::survey(part_m, contacts_m)
  
  comix_survey
}


check_table_names <- function(env) {
  env_tables <- grep("table_", names(env), value = TRUE)
  new_tables <- setdiff(env_tables, expected_tables)
  missing_tables <- setdiff(expected_tables, env_tables)
  
  if (length(new_tables) > 0) {
    message(paste(
      c("Warning Messange : \nAdd the following new table(s) to the `expected_tables` variable:",
        new_tables), collapse = "\n"))
  }
  if( length(missing_tables) > 0) {
    message(paste(
      c("Warning Messange : \nThe following table(s) are missing:",
        missing_tables), collapse = "\n"))
  }
}

expected_tables <- c(
  "table_q76_3_scale3", "table_q76_2_scale3", "table_q76_1_scale3", 
  "table_q76_3_scale2", "table_q76_2_scale2", "table_q76_1_scale2", 
  "table_q76_3_scale1", "table_q76_2_scale1", "table_q76_1_scale1", 
  "table_q75_3_scale3", "table_q75_2_scale3", "table_q75_1_scale3", 
  "table_q75_3_scale2", "table_q75_2_scale2", "table_q75_1_scale2", 
  "table_q75_3_scale1", "table_q75_2_scale1", "table_q75_1_scale1", 
  "table_q63", "table_q62", "table_q34", "table_q33", "table_q30", 
  "table_q29", "table_q28", "table_q21", "participant_table_questions", 
  "table_q66", "table_q51", "table_q50", "table_q49", "table_q48a", 
  "table_q48", "table_q47", "table_q46", "table_q45", "table_q44", 
  "table_q43", "table_q42", "table_q41", "table_q40", "table_q39", 
  "table_q31", "table_q23", "table_contact_flag")


fwrite_details <- function(part, contacts, settings, panel_name_, panel_details,
                           filter_region, filter_type, nboots, comix_matrices_path) {

  details <- data.table(
    panel = paste(unique(part$panel), collapse = ","),
    wave_id = paste(unique(part$wave_id), collapse = ","),
    week = paste(unique(part$week), collapse = ","),
    date_start = min(part$date),
    date_end = max(part$date),
    panel_details = paste(panel_details, collapse = ","),
    part_n = nrow(part),
    contacts_n = nrow(contacts),
    mean_cnt_all = mean(part$n_cnt_all),
    iqr_25_cnt_all = quantile(part$n_cnt_all, p = 0.25),
    iqr75_cnt_all = quantile(part$n_cnt_all, p = 0.75),
    max_cnt_all = max(part$n_cnt_all),
    hh_size = mean(as.numeric(part$hh_size)),
    settings = paste(unique(part$week), collapse = ","),
    data_filter = tolower(panel_name_),
    filter_type = filter_type,
    filter_region = filter_region,
    nboots = nboots,    
    regions = paste(unique(part$regions), collapse = ", ")
  )
  fpath <- file.path(comix_matrices_path,  "data_filter_details.csv")
  fwrite(details, fpath)
  message(paste("Data details saved to:", fpath))
}


## ALSO SEE mean_summary_dt below (see r/average_contacts/an_wales_contacts.R)
mean_summary_dt <- function(part, contacts, description_name) {
  # use one of the following descriptions: "all", "< 100 contacts", "England", "England  & < 100"
  data.table(
    description = description_name,
    panel = paste(unique(part$panel), collapse = ","),
    wave_id = paste(unique(part$wave_id), collapse = ","),
    wave = paste(unique(part$wave), collapse = ","),
    week = paste(unique(part$week), collapse = ","),
    date_start = min(part$date),
    date_end = max(part$date),
    part_n = nrow(part),
    contacts_n = nrow(contacts),
    mean_cnt_not_home = mean(part$n_cnt_not_home),
    iqr_25_cnt_not_home = quantile(part$n_cnt_not_home, p = 0.25),
    iqr75_cnt_not_home = quantile(part$n_cnt_not_home, p = 0.75),
    iqr_not_home = paste(quantile(part$n_cnt_not_home, p = 0.25), "to", quantile(part$n_cnt_not_home, p = 0.75)),
    max_cnt_not_home = max(part$n_cnt_not_home),
    
    mean_cnt_all = mean(part$n_cnt_all),
    iqr_25_cnt_all = quantile(part$n_cnt_all, p = 0.25),
    iqr75_cnt_all = quantile(part$n_cnt_all, p = 0.75),
    iqr_all = paste(quantile(part$n_cnt_all, p = 0.25), "to", quantile(part$n_cnt_all, p = 0.75)),
    max_cnt_all = max(part$n_cnt_all),
    mean_cnt_school = mean(part$n_cnt_school),
    iqr_25_cnt_school = quantile(part$n_cnt_school, p = 0.25),
    iqr75_cnt_school = quantile(part$n_cnt_school, p = 0.75),
    iqr_school = paste(quantile(part$n_cnt_school, p = 0.25), "to", quantile(part$n_cnt_school, p = 0.75)),
    max_cnt_school = max(part$n_cnt_school),
    
    mean_class_size = mean(part$part_school_class_size, na.rm = TRUE),
    min_max_class_size = paste(min(part$part_school_class_size, na.rm = TRUE), "to", max(part$part_school_class_size, na.rm = TRUE)),
    hh_size = mean(as.numeric(part$hh_size)),
    regions = paste(unique(part$regions), collapse = ", ")
  )
}


mean_summary_dt_by <- function(part_dt, contacts, description_name, byvars, add_proportion_cols = FALSE) {
  describe <- data.table(
    description = description_name
  )

  summary <- part_dt[ , .(
    part_n = .N,
    cnt_n = sum(n_cnt_all),
    ## ALL
    mean_cnt_all = round(mean(n_cnt_all), 2),
    iqr_25_cnt_all = quantile(n_cnt_all, p = 0.25),
    iqr75_cnt_all = quantile(n_cnt_all, p = 0.75),
    iqr_all = paste(quantile(n_cnt_all, p = 0.25), "to", quantile(n_cnt_all, p = 0.75)),
    max_cnt_all = quantile(n_cnt_all, p = 1),
    
    ## HOME
    mean_cnt_home = round(mean(n_cnt_home), 2),
    iqr_25_cnt_home = quantile(n_cnt_home, p = 0.25),
    iqr75_cnt_home = quantile(n_cnt_home, p = 0.75),
    iqr_home = paste(quantile(n_cnt_home, p = 0.25), "to", quantile(n_cnt_home, p = 0.75)),
    max_cnt_home = quantile(n_cnt_home, p = 1),
    
    ## NOT HOUSEHOLD
    mean_cnt_not_household = round(mean(n_cnt_not_household), 2),
    iqr_25_cnt_not_household = quantile(n_cnt_not_household, p = 0.25),
    iqr75_cnt_not_household = quantile(n_cnt_not_household, p = 0.75),
    iqr_not_household = paste(quantile(n_cnt_not_household, p = 0.25), "to", quantile(n_cnt_not_household, p = 0.75)),
    max_cnt_not_household = quantile(n_cnt_not_household, p = 1),
    ## WORK
    mean_cnt_work = round(mean(n_cnt_work), 2),
    iqr_25_cnt_work = quantile(n_cnt_work, p = 0.25),
    iqr75_cnt_work = quantile(n_cnt_work, p = 0.75),
    max_cnt_work = quantile(n_cnt_work, p = 1),
    # iqr_work = paste(
    #   quantile(n_cnt_work, p = 0.25), "to", quantile(n_cnt_work, p = 0.75)),
    
    ## SCHOOL
    mean_cnt_school = round(mean(n_cnt_school), 2),
    iqr_25_cnt_school = quantile(n_cnt_school, p = 0.25),
    iqr75_cnt_school = quantile(n_cnt_school, p = 0.75),
    max_cnt_school = quantile(n_cnt_school, p = 1),
    # iqr_school = paste(
    #   quantile(n_cnt_school, p = 0.25), "to", quantile(n_cnt_school, p = 0.75)),
    
    ## OTHER 
    mean_cnt_other = mean(n_cnt_other),
    iqr_25_cnt_other = quantile(n_cnt_other, p = 0.25),
    iqr75_cnt_other = quantile(n_cnt_other, p = 0.75),
    max_cnt_other = quantile(n_cnt_other, p = 1),
    # iqr_other = paste(
    #   quantile(n_cnt_other, p = 0.25), "to", quantile(n_cnt_other, p = 0.75)),
   
     hh_size = mean(as.numeric(hh_size))
  ), by = byvars]
  
  summary <- cbind(describe, summary)
  
  if(add_proportion_cols) {
    # only works in specific cases
    weeks <- part_dt[, .(total_participants = .N), by = byvars]
    gt0 <- part_dt[n_cnt_all > 0, .(n_gt0_all = .N), by = byvars]
    gt1 <- part_dt[n_cnt_all > 1, .(n_gt1_all = .N), by = byvars]
    gt2 <- part_dt[n_cnt_all > 2, .(n_gt2_all = .N), by = byvars]
    
    gt0nhh <- part_dt[n_cnt_not_household > 0, 
                      .(n_gt0_not_household = .N), by = byvars]
    gt1nhh <- part_dt[n_cnt_not_household > 1, 
                      .(n_gt1_not_household = .N), by = byvars]
    gt2nhh <- part_dt[n_cnt_not_household > 2, 
                      .(n_gt2_not_household = .N), by = byvars]
    
    counts <- list(gt0, gt1, gt2, gt0nhh, gt1nhh, gt2nhh, weeks)
    counts <- Reduce(function(...) merge(..., by = byvars), counts)[order(week)]
    
    counts[, prop_gt0_all := n_gt0_all / total_participants]
    counts[, prop_gt1_all := n_gt1_all / total_participants]
    counts[, prop_gt2_all := n_gt2_all / total_participants]
    counts[, prop_gt0_not_household := n_gt0_not_household / total_participants]
    counts[, prop_gt1_not_household := n_gt1_not_household / total_participants]
    counts[, prop_gt2_not_household := n_gt2_not_household / total_participants]
    
    summary <- merge(summary, counts, by = byvars)
  }
    

  return(summary)
}

add_england_col <- function(part) {
  r_england <- c("South East", "North West", "West Midlands", "East Midlands", 
                "South West", "Greater London", "North East", "Yorkshire and The Humber", 
                "East of England")
  r_not_england <- c("Scotland", "Northern Ireland", "Wales")
  part[, england := fcase(
    regions %in% r_england, 1,
    regions %in% r_not_england, 0
    )]
  
  part
}

# add contact flags to participant 
part_add_nickname_flag_count <- function(part_dt, cont_dt) {
  id_cols = c("part_id", "wave_id", "country_code")
  count_dt <- cont_dt[!is.na(cnt_nickname_flag), .(part_flag_count = .N), 
                      by = c("cnt_nickname_flag", id_cols)]
  count_dt[, cnt_nickname_flag := paste0("n_", gsub(" ", "_", cnt_nickname_flag))]
  count_dt <- dcast(count_dt,
                    part_id + wave_id + country_code ~ cnt_nickname_flag,
                    value.var = "part_flag_count")
  part_dt <- merge(part_dt, count_dt, by = id_cols, all.x = TRUE)
  
  part_dt
}


trim_contacts <- function(cont_dt, n) {
  # order contacts by in order giving individually reported priority, then
  # in order to home, work, and school and assign an index number
  cont_dt[, part_nth_cont := 0]
  cont_dt[
    order(individually_reported, cnt_home, cnt_work, cnt_school, decreasing  = TRUE), 
    part_nth_cont := seq_along(.I), by = c("week", "part_id")]
  
  cont_dt <- cont_dt[part_nth_cont <= n]
  
  cont_dt[, part_nth_cont := NULL]
  return(cont_dt)
}

trim_school_contacts <- function(cont_dt, n) {
  cont_dt[, part_nth_cont := 0]
  cont_dt[(cnt_school == "Yes" & cnt_home == "No" & cnt_work == "No"), 
    part_nth_cont := seq_along(.I), by = c("week", "part_id")]
  
  cont_dt <- cont_dt[part_nth_cont <= n]
  
  cont_dt[, part_nth_cont := NULL]
  return(cont_dt)
}

# MATRIX SUMS

matrix_colSums_dt <- function(cm, over70 = F) {
  # matrix colsums to dt
  means <- colSums(cm, na.rm = TRUE)
  nms <- names(means)
  if(over70){

    cm <- cm[, 9]
    means <- as.data.table(cm)
    nms <- names(cm)
  }
  data.table(age_group = nms, mean_contacts = means)
}

matrix_rowSums_dt <- function(cm) {
  # matrix colsums to dt
  means <- rowSums(cm, na.rm = TRUE)
  
  data.table(age_group = names(means), mean_contacts = means)
}

matrix_colSums_dt_age_cats <- function(cm) {
  # matrix colsums to dt
  dt <- as.data.table(cm)
  ns <- names(dt)
  dt[, part_age := rep(ns)]
  # means <- colSums(cm)
  dt <- melt(dt,  id.vars = "part_age", 
                 measure.vars = ns[-10],
                 variable.name = "contact_age", 
                 value.name = "mean_contacts")
  dt[, c("age_min", "age_max") := tstrsplit(part_age, "-", fixed=TRUE)]
  dt[, contact_age_category := fcase(
    as.numeric(age_max) < 18, "0-18",
    as.numeric(age_min) >= 70, "70-120",
    default = "18-69"
  )]
  
  summ <- dt[, .(sum_means = sum(mean_contacts, na.rm = T)), by = c("part_age", "contact_age_category")]
}

dt_colSums_dt_age_cats <- function(dt) {
  # dt > matrix > colSums > dt
  browser()
  # cm <- as.matrix(dt[,-c(1), with = F][, names(dt)[-1] := lapply(.SD, as.numeric)], 
  #                 rownames.value =  names(dt)[-1])
  # means <- colSums(cm, na.rm = TRUE)
  ns <- dimnames(dt)[[1]]
  names(dt) <- ns
  dt[, part_age := rep(ns)]
  data.table(age_group = names(means), mean_contacts = means)
}

dt_colSums_dt <- function(dt, over70 = F) {
  # dt > matrix > colSums > dt
  cm <- as.matrix(dt[,-c(1), with = F][, names(dt)[-1] := lapply(.SD, as.numeric)], 
                  rownames.value =  names(dt)[-1])
  means <- colSums(cm, na.rm = TRUE)
  nms <- names(means)
  
  if(over70){
    cm <- cm[, 9]
    means <- as.data.table(cm)
    nms <- names(cm)
  }
  data.table(age_group = nms, mean_contacts = means)
}

dt_rowSums_dt <- function(dt) {
  # dt > matrix > colSums > dt
  # browser()
  cm <- as.matrix(dt[,-c(1), with = F][, names(dt)[-1] := lapply(.SD, as.numeric)], 
                  rownames.value =  names(dt)[-1])
  means <- rowSums(cm, na.rm = TRUE)
  data.table(age_group = names(means), mean_contacts = means)
}


cnt_summary_by <- function(cnt_dt, description_name, byvars) {
  describe <- data.table(
    description = description_name
  )
  
  summary <- cnt_dt[ , .(
    cnt_n = .N),
    by = byvars]
  
  summary <- cbind(describe, summary)
  
  summary
}



get_timeline <- function(all = FALSE, file_path = "data/uk/policy_tracker/20200806_Measures_to_limit_spread.xlsx") {
  # library(readxl)
  # policies <- readxl::read_xlsx(file_path, sheet = 2, skip = 1)
  # policies2 <- readxl::read_xlsx(file_path, sheet = 3, skip = 1)
  # pall <- rbind(policies, policies2)
  timeline_list <- list(
    list(
    date = as.Date("2020-03-24"),
    label = "Lockdown start"
    ),
    list(
    date = as.Date("2020-04-29"),
    label = "Survey change 1"
    ),
    list(
    date = as.Date("2020-05-11"),
    label = "Non-essential workers return"
    ),
    list(
    date = as.Date("2020-05-13"),
    label = "Survey change 2"
    ),
    list(
    date = as.Date("2020-06-08"),
    label = "Schools partially repoen"
    ),
    list(
    date = as.Date("2020-07-04"),
    label = "Lockdown easement"
    )
  )
  
  timeline <- rbindlist(timeline_list)
  timeline[, date := as.Date(date)]
  timeline <- timeline[order(date)]
  return(timeline)
}

# timeline_list <- list(
#   list(
#     lockdown_start = as.Date("2020-03-24"),
#     ls_text = "Lockdown start"
#   ),
#   list(
#     survey_change = as.Date("2020-04-29"),
#     survey_change_text = "Survey changed"
#   ),
#   list(
#     work_partial_reopen = as.Date("2020-05-11"),
#     survey_revert_text = "Non-essential workers return"
#   ),
#   list(
#     survey_revert = as.Date("2020-05-13"),
#     survey_revert_text = "Survey reverted"
#   ),
#   list(
#     schools_partial_reopen = as.Date("2020-06-08"),
#     spr_text = "Schools partially repoen"
#   ),
#   list(
#     lockdown_easement = as.Date("2020-07-04"),
#     le_text = "Lockdown easement"
#   )
# )
