

## Packages
library(data.table)
library(lubridate)
library(flextable)
library(magrittr)
library(dplyr)


# Load data ---------------------------------------------------------------
dt <- qs::qread('data/wrapup_part_cnts.qs')

dt <- dt[survey_round == 1000]
# Filter to needed vars ---------------------------------------------------
## Age, Gender, Household size, day of week, country, High risk, Face mask wearing
## Employed, attending work


dt <- dt[, .(
  part_uid,
  ## Characteristics
  part_age_group, 
  sample_type,
  part_gender, 
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
  ## Extra
  part_employed,
  part_work_place,
  part_attend_work_yesterday,
  part_employstatus,
  ## Contacts
  n_cnt
)]


dta <- dt


## Test significance for NL vs others (full population)
for(this_country in c("UK","BE","CH")){
  dum<-dta %>% filter(country == "NL")
  NL_contacts<-dum$n_cnt
  dum<-dta %>% filter(country == this_country)
  noNL_contacts<-dum$n_cnt
  #perform a two sample kolmogorov smirnov test on the two distributions  
  a<-ks.test(NL_contacts,noNL_contacts)
  print(paste0("P-value of the KS test for Netherlands vs ",this_country," = ", a$p.value))
}

## Test significance for NL vs others (adults)
for(this_country in c("UK","BE","CH")){
  dum<-dta %>% filter(country == "NL") %>% filter(sample_type=="adult")
  NL_contacts<-dum$n_cnt
  dum<-dta %>% filter(country == this_country) %>% filter(sample_type=="adult")
  noNL_contacts<-dum$n_cnt
  #perform a two sample kolmogorov smirnov test on the two distributions  
  a<-ks.test(NL_contacts,noNL_contacts)
  print(paste0("P-value of the KS test for Netherlands (adults) vs ",this_country, " (adults) "," = ", a$p.value))
}

## Test significance for NL vs others (children)
for(this_country in c("UK","BE","CH")){
  dum<-dta %>% filter(country == "NL") %>% filter(sample_type=="child")
  NL_contacts<-dum$n_cnt
  dum<-dta %>% filter(country == this_country) %>% filter(sample_type=="child")
  noNL_contacts<-dum$n_cnt
  #perform a two sample kolmogorov smirnov test on the two distributions  
  a<-ks.test(NL_contacts,noNL_contacts)
  print(paste0("P-value of the KS test for Netherlands (children) vs ",this_country, " (children) "," = ", a$p.value))
}



## Test significance for gender
for(this_country in c("UK","BE","NL","CH")){
  dum<-dta %>% filter(country == this_country) %>% filter(part_gender== "Male")
  M_contacts<-dum$n_cnt
  dum<-dta %>% filter(country == this_country) %>% filter(part_gender== "Female")    
  F_contacts<-dum$n_cnt
  #perform a two sample kolmogorov smirnov test on the two distributions  
  a<-ks.test(M_contacts,F_contacts)
  print(paste0("P-value of the KS test M vs F for ",this_country," = ", a$p.value))
}

## Facemask 
for(this_country in c("UK","BE","NL","CH")){
  dum<-dta %>% filter(country == this_country) %>% filter(part_face_mask== "Yes")
  YES_contacts<-dum$n_cnt
  dum<-dta %>% filter(country == this_country) %>% filter(part_face_mask== "No")    
  NO_contacts<-dum$n_cnt
  #perform a two sample kolmogorov smirnov test on the two distributions  
  a<-ks.test(YES_contacts,NO_contacts)
  print(paste0("P-value of the KS test (facemask)  for ",this_country," = ", a$p.value))
}


## High risk 
for(this_country in c("UK","BE","NL","CH")){
  dum<-dta %>% filter(country == this_country) %>% filter(part_high_risk== "Yes")
  YES_contacts<-dum$n_cnt
  dum<-dta %>% filter(country == this_country) %>% filter(part_high_risk== "No")    
  NO_contacts<-dum$n_cnt
  #perform a two sample kolmogorov smirnov test on the two distributions  
  a<-ks.test(YES_contacts,NO_contacts)
  print(paste0("P-value of the KS test (high risk)  for ",this_country," = ", a$p.value))
}

## Vaccinated
for(this_country in c("UK","BE","NL","CH")){
  dum<-dta %>% filter(country == this_country) %>% filter(part_vacc== "Yes")
  YES_contacts<-dum$n_cnt
  dum<-dta %>% filter(country == this_country) %>% filter(part_vacc== "No")    
  NO_contacts<-dum$n_cnt
  #perform a two sample kolmogorov smirnov test on the two distributions  
  a<-ks.test(YES_contacts,NO_contacts)
  print(paste0("P-value of the KS test (vaccinated)  for ",this_country," = ", a$p.value))
}

## Contacts on Friday for UK vs non-Friday
dum<-dta %>% filter(country == "UK") %>% filter(weekday== "Fr")
YES_contacts<-dum$n_cnt
dum<-dta %>% filter(country == "UK") %>% filter(weekday!="Fr")    
NO_contacts<-dum$n_cnt
#perform a two sample kolmogorov smirnov test on the two distributions  
a<-ks.test(YES_contacts,NO_contacts)
print(paste0("P-value of the KS test for contacts on Fridays in UK", a$p.value))

## Contacts on Monday for BE vs non-Monday
dum<-dta %>% filter(country == "BE") %>% filter(weekday== "Mon")
YES_contacts<-dum$n_cnt
dum<-dta %>% filter(country == "BE") %>% filter(weekday!="Mon")    
NO_contacts<-dum$n_cnt
#perform a two sample kolmogorov smirnov test on the two distributions  
a<-ks.test(YES_contacts,NO_contacts)
print(paste0("P-value of the KS test for contacts on Monday in BE", a$p.value))

## Contacts on Monday for NL vs non-Monday
dum<-dta %>% filter(country == "NL") %>% filter(weekday== "Mon")
YES_contacts<-dum$n_cnt
dum<-dta %>% filter(country == "NL") %>% filter(weekday!="Mon")    
NO_contacts<-dum$n_cnt
#perform a two sample kolmogorov smirnov test on the two distributions  
a<-ks.test(YES_contacts,NO_contacts)
print(paste0("P-value of the KS test for contacts on Monday in NL", a$p.value))


## Test for weekend vs non-weekend
for(this_country in c("UK","BE","NL","CH")){
  dum<-dta %>% filter(country == this_country) %>% filter(weekday %in% c("Sat","Sun "))
  YES_contacts<-dum$n_cnt
  dum<-dta %>% filter(country == this_country) %>% filter(weekday %in% c("Mon","Tue","Wed","Thu","Fr"))    
  NO_contacts<-dum$n_cnt
  #perform a two sample kolmogorov smirnov test on the two distributions  
  a<-ks.test(YES_contacts,NO_contacts)
  print(paste0("P-value of the KS test (Weekend)  for ",this_country," = ", a$p.value))
}


