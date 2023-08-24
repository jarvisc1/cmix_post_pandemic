## Dm cms

## Contact matrices


library(data.table)

# Source user written scripts ---------------------------------------------

source('r/functions/get_minimal_data.R')
source('r/functions/functions.R')
source('r/functions/calc_cm.R')

# Input data ----------------------------------------------------------------

# extract data with useful columns
data =  get_minimal_data()

# decant data into relevant containers
contacts =  data[[1]]
parts =  data[[2]]

#
## set breaks and get population proportions 
breaks = c(0,5,12,18,30,40,50,60,70,Inf)
popdata_uk = get_popvec(breaks, year_ = 2020,country_ = "uk")
popdata_be = get_popvec(breaks, year_ = 2020, country_ = "be")
popdata_nl = get_popvec(breaks, year_ = 2020, country_ = "nl")
popdata_ch = get_popvec(breaks, year_ = 2020, country_ = "ch")

# Prem version
breaks_prem=c(seq(0,75,by=5),Inf)
popdata_ch_prem = get_popvec(breaks_prem, year_ = 2020, country_ = "ch")


# Get UK ------------------------------------------------------------------
puk <- parts[country == "uk"]
unique_puk_pid <- unique(puk$part_id)
cuk <- contacts[part_id %in% unique_puk_pid]


# Get BE ------------------------------------------------------------------
pbe <- parts[country == "be"]
unique_pbe_pid <- unique(pbe$part_id)
cbe <- contacts[part_id %in% unique_pbe_pid]


# NL ----------------------------------------------------------------------
pnl <- parts[country == "nl"]
unique_pnl_pid <- unique(pnl$part_id)
cnl <- contacts[part_id %in% unique_pnl_pid]


# Switzerland -------------------------------------------------------------
pch <- parts[country == "ch"]
unique_pch_pid <- unique(pch$part_id)
cch <- contacts[part_id %in% unique_pch_pid]

cms_uk = calc_cm(puk, cuk, max_ = 50, weeks_range = 1000, pop_data_ = popdata_uk)
cms_be = calc_cm(pbe, cbe, max_ = 50, weeks_range = 1000, pop_data_ = popdata_be)
cms_nl = calc_cm(pnl, cnl, max_ = 50, weeks_range = 1000, pop_data_ = popdata_nl)
cms_ch = calc_cm(pch, cch, max_ = 50, weeks_range = 1000, pop_data_ = popdata_ch)

cms_ch_prem = calc_cm(pch, cch, max_ = 50, weeks_range = 1000, pop_data_ = popdata_ch_prem)



qs::qsave(cms_uk, file = "outputs/cm_data/uk_cm.qs")
qs::qsave(cms_be, file = "outputs/cm_data/be_cm.qs")
qs::qsave(cms_nl, file = "outputs/cm_data/nl_cm.qs")
qs::qsave(cms_ch, file = "outputs/cm_data/ch_cm.qs")

qs::qsave(cms_ch_prem, file = "outputs/cm_data/ch_cm_prem.qs")


# Get the dominant eigenvalues --------------------------------------------
cms_uk_bs = calc_cm_bs(puk, cuk, max_ = 50, weeks_range = 1000, pop_data_ = popdata_uk, bs = 1000)
cms_be_bs = calc_cm_bs(pbe, cbe, max_ = 50, weeks_range = 1000, pop_data_ = popdata_be, bs = 1000)
cms_nl_bs = calc_cm_bs(pnl, cnl, max_ = 50, weeks_range = 1000, pop_data_ = popdata_nl, bs = 1000)
cms_ch_bs = calc_cm_bs(pch, cch, max_ = 50, weeks_range = 1000, pop_data_ = popdata_ch, bs = 1000)


cms_ch_prem_bs = calc_cm_prem_bs(pch, cch, max_ = 50, weeks_range = 1000, pop_data_ = popdata_ch_prem, bs = 1000)

eig_uk <- cms_uk_bs[[2]][1000][[1]][1]
eig_be <- cms_be_bs[[2]][1000][[1]][1]
eig_nl <- cms_nl_bs[[2]][1000][[1]][1]
eig_ch <- cms_ch_bs[[2]][1000][[1]][1]

eig_ch_prem <- cms_ch_prem_bs[[2]][1000][[1]][1]

qs::qsave(eig_uk, file = "outputs/cm_data/uk_eigs.qs")
qs::qsave(eig_be, file = "outputs/cm_data/be_eigs.qs")
qs::qsave(eig_nl, file = "outputs/cm_data/nl_eigs.qs")
qs::qsave(eig_ch, file = "outputs/cm_data/ch_eigs.qs")

qs::qsave(eig_ch_prem, file = "outputs/cm_data/ch_prem_eigs.qs")

## First LD UK
cms_uk_ld1_bs = calc_cm_bs(puk, cuk, max_ = 50, weeks_range = 1, pop_data_ = popdata_uk, bs = 1000)
eig_ld1_uk <- cms_uk_ld1_bs[[2]][1][[1]][1]

qs::qsave(eig_ld1_uk, file = "outputs/cm_data/uk_ld1_eigs.qs")
