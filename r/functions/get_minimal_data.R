## Name: get_minimal_data
## Description: filters cleaned data to required columns
## Input file: clean_participants.rds, clean_contacts.rds
## Functions: get_minimal_data
## Output file: None


# Packages ----------------------------------------------------------------
library(data.table)


# Source user written scripts ---------------------------------------------

# NONE 

# Input data ----------------------------------------------------------------


# Functions -------------------------------------------------------------------


get_minimal_data <- function(countries = c('uk', "be", "ch", "nl")){
  
  prt = qs::qread('../comix/data/part_min.qs')
  cnt = qs::qread('../comix/data/contacts.qs')
  
  # filter to be correct countries ------------------------------------------
  prt <- prt[country %in% countries]
  unique_pid <- unique(prt$part_wave_uid)
  cnt <- cnt[part_wave_uid %in% unique_pid]
  
  ##Add in children data for weeks where they were missed -------------------
  parts_exchild = prt[survey_round %in% 7:8 & panel %in% c("C", "D")]
  unique_pid <- unique(parts_exchild$part_wave_uid)
  contacts_exchild = cnt[part_wave_uid %in% unique_pid]

  parts_exchild[, survey_round := 700]
  contacts_exchild[, survey_round := 700]
  prt = rbind(prt, parts_exchild)
  cnt = rbind(cnt, contacts_exchild)
  
  
  list(cnt, prt)
  
}
