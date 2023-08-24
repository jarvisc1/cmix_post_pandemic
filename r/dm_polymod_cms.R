## dm_polymod_cms


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
conts_poly_nl  <- subset(conts_poly_nl, select = c("local_id", "cnt_age_l", "cnt_age_r", "cnt_touch"))


colnames(parts_poly_nl) <- c("part_id", "part_age", "dayofweek")
colnames(conts_poly_nl) <- c("part_id", "cnt_age_est_min", "cnt_age_est_max", "phys_contact")

conts_poly_nl <- merge(conts_poly_nl, parts_poly_nl, by = "part_id")

conts_poly_nl[, cnt_age_est_min := as.integer(cnt_age_est_min)]
conts_poly_nl[is.na(cnt_age_est_max), cnt_age_est_max := as.integer(cnt_age_est_min)]

parts_poly_nl[!is.na(part_age), part_age_est_min := part_age]
parts_poly_nl[!is.na(part_age), part_age_est_max := part_age]

## Add in named days
parts_poly_nl <- weekday_(parts_poly_nl)
conts_poly_nl <- weekday_(conts_poly_nl)

parts_poly_nl[, survey_round := 1]
conts_poly_nl[, survey_round := 1]



## Make at same age breaks
breaks = c(0,5,12,18,30,40,50,60,70,Inf)
popdata_totals_uk = get_popvec(breaks, year_ = 2006, country_ = "uk")
popdata_totals_nl = get_popvec(breaks, year_ = 2006, country_ = "nl")
popdata_totals_be = get_popvec(breaks, year_ = 2006, country_ = "be")
popdata_totals_ch = get_popvec(breaks, year_ = 2006, country_ = "ch")
# Prem matrix
breaks_prem=c(seq(0,75,by=5),Inf)
popdata_totals_ch_prem = get_popvec(breaks_prem, year_ = 2006, country_ = "ch")

get_domeig <- function(parts_poly_, conts_poly_, popdata_totals_) {
  # parts_poly_<-parts_poly_uk
  # conts_poly_<-conts_poly_uk
  # popdata_totals_<-popdata_totals_uk
  ct_ac = get_age_table(parts = parts_poly_, conts = conts_poly_ , breaks = breaks)
  cont_per_age_per_part = ct_ac[[1]]
  all_conts = ct_ac[[2]]
  cont_per_age_per_part = correct_truncation(cont_per_age_per_part, all_conts = all_conts, breaks = breaks, trunc=28)
  eg = get_matrix_2(cont_per_age_per_part, breaks, trunc=1000)
  eg_props = symetricise_matrix(eg = eg,popdata_totals=popdata_totals_,breaks = breaks)
  
  max(eigen(matrix(eg_props$aug_mean_sym, nrow = (length(breaks) - 1)))$values)
}
# Prem version of the function (change the age-break)
library(reshape2)

get_domeig_prem <- function(prem_matrices_data_,countryname_,popdata_totals_) {
  # prem_matrices_data_<-prem_matrices 
  # countryname_<-"CHE"
  # popdata_totals_<-popdata_totals_ch_prem
  eg=prem_matrices_data_[[countryname_]]
  dum<-cut(0,breaks_prem,right=FALSE)
  dum_names<-levels(dum)
  colnames(eg)<-dum_names
  rownames(eg)<-dum_names
  eg<-reshape2::melt(eg)
  colnames(eg)<-c("age_group","age_group_cont","means")
  eg<-data.table(eg)
  eg_props = symetricise_matrix(eg = eg,popdata_totals=popdata_totals_,breaks = breaks_prem)
  max(eigen(matrix(eg_props$aug_mean_sym, nrow = (length(breaks_prem) - 1)))$values)
}

pmod_eig_uk <- get_domeig(parts_poly_uk, conts_poly_uk, popdata_totals_uk)
pmod_eig_nl <- get_domeig(parts_poly_nl, conts_poly_nl, popdata_totals_nl)
pmod_eig_be <- get_domeig(parts_poly_be, conts_poly_be, popdata_totals_be)
pmod_eig_ch <- get_domeig(parts_poly, conts_poly, popdata_totals_ch)

load("./data/prem_contact_all.rdata")
prem_matrices<-contact_all
rm("contact_all")
prem_eig_ch <- get_domeig_prem(prem_matrices, "CHE", popdata_totals_ch_prem)


qs::qsave(pmod_eig_uk, file = "outputs/cm_data/pmod_uk_eig.qs")
qs::qsave(pmod_eig_be, file = "outputs/cm_data/pmod_be_eig.qs")
qs::qsave(pmod_eig_nl, file = "outputs/cm_data/pmod_nl_eig.qs")
qs::qsave(pmod_eig_ch, file = "outputs/cm_data/pmod_ch_eig.qs")

qs::qsave(prem_eig_ch, file = "outputs/cm_data/prem_ch_eig.qs")




# Uncertainty for PMOD ----------------------------------------------------

# Source user written scripts ---------------------------------------------

source('r/functions/get_minimal_data.R')
source('r/functions/functions.R')
source('r/functions/calc_cm.R')

# Input data ----------------------------------------------------------------

get_domeig_bs <- function(x1, x2, x3){
  xx <- calc_cm_bs(x1, x2, max_ = 50, weeks_range = 1, 
                   pop_data_ = x3, bs = 1000)
  xx[[2]][1][[1]][1]
}

pmod_eigs_uk <- get_domeig_bs(parts_poly_uk, conts_poly_uk, popdata_totals_uk)
pmod_eigs_nl <- get_domeig_bs(parts_poly_nl, conts_poly_nl, popdata_totals_nl)
pmod_eigs_be <- get_domeig_bs(parts_poly_be, conts_poly_be, popdata_totals_be)
pmod_eigs_ch <- get_domeig_bs(parts_poly, conts_poly, popdata_totals_ch)


qs::qsave(pmod_eigs_uk, file = "outputs/cm_data/pmod_uk_eigs.qs")
qs::qsave(pmod_eigs_be, file = "outputs/cm_data/pmod_be_eigs.qs")
qs::qsave(pmod_eigs_nl, file = "outputs/cm_data/pmod_nl_eigs.qs")
qs::qsave(pmod_eigs_ch, file = "outputs/cm_data/pmod_ch_eigs.qs")


