## Name: functions
## Description: functions for manipulating and sampling data
## Input file: clean_participants.rds, clean_contacts.rds
## Functions: assign_min_max_ages, sample_age_table
## Output file: None


# Packages ----------------------------------------------------------------
library(data.table)
library(stringr)

# Source user written scripts ---------------------------------------------
source('r/functions/fitting_functions.R')


# Input data ----------------------------------------------------------------


# Give labels for age groups
get_labels <- function(breaks){
  labs = paste0(breaks[-length(breaks)]+1, "-", breaks[-1])
  labs[1] = paste0(breaks[1], "-", breaks[2])
  labs[length(labs)] = paste0(breaks[length(breaks)-1]+1,"+")
  labs
}





# Find the min and max values for age entered in the survey - if not recorded check string format in cont_age
assign_min_max_ages <- function(prts, cnts){
  
  # set default band to be participant's age if recorded 
  prts[!is.na(part_age), part_age_est_min := part_age]
  prts[!is.na(part_age), part_age_est_max := part_age]
  
  
  # get participant age as string 
  cnts[, cnt_age := as.character(cnt_age)]
  # set 65+ and 85+ entries to bands
  cnts[cnt_age == '65+', cnt_age := '65-100']
  cnts[cnt_age == '85+', cnt_age := '85-100']
  
  # get participant age as string again with modification
  strings = sapply(cnts$cnt_age, toString)
  # find all contacts with ages reported with '-' in 
  cnts = cnts[sapply(strings, function(X){grepl('-', X)})]
  
  # find numerical values to set bounds of contact age group
  swapsies = lapply(strings, FUN=function(STRING){as.numeric(str_extract_all(STRING,"\\(?[0-9,.]+\\)?")[[1]])})
  age_maxs = lapply(swapsies, max)
  age_mins = lapply(swapsies, min)
  
  # set bounts of contact age group
  cnts[is.na(cnt_age_est_min), cnt_age_est_max := age_maxs[is.na(cnts$cnt_age_est_min)]]
  cnts[is.na(cnt_age_est_min), cnt_age_est_min := age_mins[is.na(cnts$cnt_age_est_min)]]
  
  list(prts, cnts)
}


# sample ages of participants and contacts 
sample_age_table <- function(prts, cnts, agegroupbreaks){
  
  #create column of ages of participants
  sampled_ages_prts = mapply(FUN = function(X, Y){sample(rep(seq(X,Y),2), 1)}, prts[!is.na(part_age_est_min)]$part_age_est_min, prts[!is.na(part_age_est_min)]$part_age_est_max)
  prts_ages = prts[!is.na(part_age_est_min)][,c('part_id')]
  prts_ages[,'prt_assigned_age_groups' :=sampled_ages_prts]
  
  #create column of ages of contacts 
  sampled_ages_cnts = mapply(FUN = function(X, Y){sample(rep(seq(X,Y),2), 1)}, cnts[!is.na(cnt_age_est_min)]$cnt_age_est_min, cnts[!is.na(cnt_age_est_min)]$cnt_age_est_max)
  cnts_ages = cnts[!is.na(cnt_age_est_min)][,c('part_id')]
  cnts_ages[,'cnt_assigned_age_groups' :=sampled_ages_cnts]
  
  
  #combine in data.table
  setkey(prts_ages,part_id)
  setkey(cnts_ages,part_id)
  
  # perform the join using the merge function
  age_frame = merge(cnts_ages,prts_ages, all=TRUE, by=.EACHI, allow.cartesian=TRUE)
  age_frame[,cnt_age_group:=cut(cnt_assigned_age_groups,breaks=agegroupbreaks, right=FALSE)]
  age_frame[,prt_age_group:=cut(prt_assigned_age_groups,breaks=agegroupbreaks, right=FALSE)]
  
  age_frame
  
}


# extract the population data (Punked this off previous codebase by Kevin?)
getPopdata <- function(country_code="uk", iso3=NULL, year=as.numeric(format(Sys.Date(), "%Y"))){
  popdata <- qs::qread(file.path("./data/unwpp_data.qs"))
  
  if(is.null(iso3)){
    iso3_ <- switch(
      country_code,
      "uk" = "GBR",
      "be" = "BEL",
      "nl" = "NLD",
      "no" = "NOR",
      "ch" = "CHE",
      NULL
    )
    if(is.null(iso3_)){
      stop(sprintf("Don't know how to convert country code %s in ISO3 code. Update function or specify iso3", country_code))
    }
  } else {
    iso3_ <- iso3
  }
  
  popdata <- popdata[
    iso3 == iso3_
    & year == year
  ]
  
  if(nrow(popdata) > 0){
    return(popdata)
  } else {
    stop("Can't find population data")
  }
}

# get the proportion of the UK population in each age group 


# Get population by year
get_popvec <- function(breaks, year_, country_){
  ## Create labels
  #labs = get_labels(breaks)
  #labels = labs
  
  ## Get the population data
  popdata = getPopdata(country_code = country_)
  popdata <- popdata[year == year_]
  popdata[, age_group := cut(age,breaks=breaks, right=FALSE)]
  popdata_totals = unique(popdata[, pop_total := sum(total), by = age_group][, c('age_group', 'pop_total')])
  popdata_totals[, props := pop_total / sum(pop_total)]
  popdata_totals
}

get_matrix = function(conts, parts, week_choice, breaks, truncate_ = 100, param = "mu"){
  ct_ac = get_age_table(parts = parts, conts = contacts, breaks = breaks, week_choice = week_choice)
  cont_per_age_per_part = ct_ac[[1]]
  all_conts = ct_ac[[2]]
  eg = get_matrix_2(cont_per_age_per_part=cont_per_age_per_part, breaks=breaks, trunc=truncate_, param = param)
  
  return(list(eg, 0))
}



# this function takes a contact matrix output by get_matrix and makes it symmetrical using population proportions output from get_popvec()
symetricise_matrix = function(eg, popdata_totals, breaks) {
  eg_props = merge(eg, popdata_totals, by="age_group")
  
  # multiply mean contact rate by the proportion of the population in the participant age_group 
  eg_props[,aug_mean:=means * props * (length(breaks) - 1)]
  
  # merge data.table with same data table but swapping part and cont age_groups - effectively transposing the matrix
  eg_props = merge(eg_props, 
                   eg_props[,c('age_group', 'age_group_cont', 'aug_mean')], 
                   by.x=c('age_group', 'age_group_cont'), 
                   by.y = c('age_group_cont','age_group'), 
                   suffixes = c('_original', '_transpose'))
  # take the mean of the original and transposed values 
  eg_props[, aug_mean_sym := 0.5 * (aug_mean_original + aug_mean_transpose)]
  
  return(eg_props)
}


get_age_table = function(conts, parts, week_choice=NULL, breaks){
  
  
  if (!is.null(week_choice)){
    # filter data as required
    conts = conts[survey_round %in% week_choice]
    parts = parts[survey_round %in% week_choice]
  }
  
  # Get numeric age ranges for every participant and contact possible
  #parts_conts <-  assign_min_max_ages(parts, conts)
  # sample ages from ranges and construct a table of part_id, participant age and contact age
  age_table   <-  sample_age_table(parts, conts, breaks)
  #unique_parts = unique(age_table[,c("part_id", "prt_age_group")])
  #sample_props = unique(unique_parts[,count:=.N, by=prt_age_group][,c("prt_age_group", "count")])[order(prt_age_group)]
  #sample_props[,props:=count/sum(count)]
  #sample_props = sample_props[,c("prt_age_group", "props")]
  
  age_table = age_table[complete.cases(age_table[,'prt_age_group']),]
  
  age_table[,contacts := .N, by = c("part_id")]
  
  all_conts = unique(age_table[,c('part_id', 'prt_age_group', 'contacts' )])
  age_group_table = age_table[,c('prt_age_group', 'cnt_age_group')]
  
  # make matrix of totals 
  levs <- unique(unlist(cut(seq(0,120),breaks, right=FALSE), use.names = FALSE))
  count_mat_total = table(lapply(age_group_table, factor, sort(levs)))
  
  #Get counts per age group per participant
  cont_per_age_per_part = age_table[, (contacts = .N), by = .(part_id, prt_age_group, cnt_age_group)]
  
  # this bit is required to add 0s into the table i.e. where no contacts were reported with some age groups by some contacts.
  # make 'complete' data.table with all combinations of part_id and cnt_age_group
  all_part_cont_age_groups = data.table(expand.grid(unique(age_table$part_id), sort(levs)))
  names(all_part_cont_age_groups) = c('part_id','cnt_age_group' )
  
  # merge participant agegroups into the complete data.table
  all_part_cont_age_groups = merge(all_part_cont_age_groups, 
                                   unique(age_table[,c('part_id', 'prt_age_group')]), 
                                   all.x = TRUE, 
                                   by= c('part_id'), 
                                   allow.cartesian=TRUE)
  
  # merge counts of contacts into the complete data.table  - leaving entries with no values as NA then convert these to 0 
  cont_per_age_per_part = merge(all_part_cont_age_groups, cont_per_age_per_part, by=c('part_id', 'cnt_age_group', 'prt_age_group'), all.x = TRUE)
  cont_per_age_per_part[is.na(V1), V1:=0]
  
  
  
  
  return(list(cont_per_age_per_part, all_conts))
} 


correct_truncation = function(cont_per_age_per_part, all_conts, breaks, trunc)   {
  
  all_conts[, new_count:=contacts]
  
  counts = all_conts$contacts
  
  outs = optim(c(mu = mean(counts), k = 1), lower = c(mean = 1e-5, k = 1e-5), nb_loglik, x = counts, method = "L-BFGS-B", n=trunc)
  
  full_dist = rnbinom(n=1.e6, mu=outs$par['mu'], size=1./outs$par['k'])
  
  left_cen_dist = full_dist[full_dist >= trunc + 1]
  
  all_conts[contacts>=trunc + 1, new_count:= sample(left_cen_dist, length(contacts))]
  all_conts[, scale_factor := new_count/contacts]
  
  cont_per_age_per_part = merge(cont_per_age_per_part, all_conts[,c('part_id', 'scale_factor')], by=c('part_id'))
  cont_per_age_per_part[, V1 := round(V1 * scale_factor)]
  
  return(cont_per_age_per_part)
}




get_matrix_2 = function(cont_per_age_per_part, breaks, trunc, param = 'mu') {
  
  
  levs <- unique(unlist(cut(seq(0,120),breaks, right=FALSE), use.names = FALSE))
  # Get columns of age-groups to put into mapply
  eg = expand.grid(sort(levs),sort(levs))
  names(eg) = c('age_group', 'age_group_cont')
  # Get means from neg_binom opitmsisation
  eg['means'] = mapply(nbinom_optim_, eg$age_group, eg$age_group_cont, param=param, n=trunc, count_frame=list(cont_per_age_per_part ))
  # eg['size'] = mapply(nbinom_optim, eg$age_group, eg$age_group_cont, param='size')
  eg = data.table(eg)
  return(eg)
  
}

get_matrix_stan = function(cont_per_age_per_part, breaks, trunc, model, samp, prior_pars_mu, prior_pars_k) {
  
  
  levs <- unique(unlist(cut(seq(0,120),breaks, right=FALSE), use.names = FALSE))
  # Get columns of age-groups to put into mapply
  eg = expand.grid(sort(levs),sort(levs))
  names(eg) = c('age_group', 'age_group_cont')
  # Get means from neg_binom opitmsisation
  fit_nb = nbinom_optim_stan_multi_BunchTrunc(model, trunc, cont_per_age_per_part, eg, samp=samp, priors_mu = prior_pars_mu, priors_k=prior_pars_k)
  fit_mat = rstan::extract(fit_nb)
  
  return(list(eg, fit_mat$mu, fit_mat$k))
}



get_symmetric_means = function(eg, mus, popdata_totals, breaks) {
  
  eigs = c()
  eg_samps_aug = c()
  samps = length(mus) / ((length(breaks) - 1) **2)
  
  for(i in 1:samps){
    
    
    eg[, means := mus[i,]]
    
    eg_props = symetricise_matrix(eg, popdata_totals, breaks)
    
    eg_samps_aug = cbind(eg_samps_aug, eg_props$aug_mean_sym)
    
    
  }
  
  return(eg_samps_aug)
  
}

### This function is old and should not be used in new code 
get_eigs_from_means = function(eg, mus, popdata_totals, breaks) {
  
  eigs = c()
  eg_samps_aug = c()
  samps = length(mus) / ((length(breaks) - 1) **2)
  
  for(i in 1:samps){
    
    
    eg[, means := mus[i,]]
    
    eg_props = symetricise_matrix(eg, popdata_totals, breaks)
    
    eg_samps_aug = cbind(eg_samps_aug, eg_props$aug_mean_sym)
    
    
    eigs = append(eigs,max(eigen(matrix(eg_props$aug_mean_sym, nrow=length(breaks) - 1))$values))
    
    
    
  }
  
  return(list(eigs, eg_samps_aug))
  
}



# This function takes the raw data and outputs the contact matrix in the form of a 'nested' data.table 
get_matrix_OLD = function(conts, parts, week_choice, breaks, truncate_ = 100){
  
  
  
  
  # filter data as required
  conts = conts[survey_round %in% week_choice]
  parts = parts[survey_round %in% week_choice]
  
  # Get numeric age ranges for every participant and contact possible
  #parts_conts <-  assign_min_max_ages(parts, conts)
  # sample ages from ranges and construct a table of part_id, participant age and contact age
  age_table   <-  sample_age_table(parts, conts, breaks)
  unique_parts = unique(age_table[,c("part_id", "prt_age_group")])
  sample_props = unique(unique_parts[,count:=.N, by=prt_age_group][,c("prt_age_group", "count")])[order(prt_age_group)]
  sample_props[,props:=count/sum(count)]
  sample_props = sample_props[,c("prt_age_group", "props")]
  
  age_table = age_table[complete.cases(age_table[,'prt_age_group']),]
  age_group_table = age_table[,c('prt_age_group', 'cnt_age_group')]
  
  # make matrix of totals 
  levs <- unique(unlist(cut(seq(0,120),breaks, right=FALSE), use.names = FALSE))
  count_mat_total = table(lapply(age_group_table, factor, sort(levs)))
  
  #Get counts per age group per participant
  cont_per_age_per_part = age_table[, (contacts = .N), by = .(part_id, prt_age_group, cnt_age_group)]
  
  # this bit is required to add 0s into the table i.e. where no contacts were reported with some age groups by some contacts.
  # make 'complete' data.table with all combinations of part_id and cnt_age_group
  all_part_cont_age_groups = data.table(expand.grid(unique(age_table$part_id), sort(levs)))
  names(all_part_cont_age_groups) = c('part_id','cnt_age_group' )
  
  # merge participant agegroups into the complete data.table
  all_part_cont_age_groups = merge(all_part_cont_age_groups, 
                                   unique(age_table[,c('part_id', 'prt_age_group')]), 
                                   all.x = TRUE, 
                                   by= c('part_id'), 
                                   allow.cartesian=TRUE)
  
  # merge counts of contacts into the complete data.table  - leaving entries with no values as NA then convert these to 0 
  cont_per_age_per_part = merge(all_part_cont_age_groups, cont_per_age_per_part, by=c('part_id', 'cnt_age_group', 'prt_age_group'), all.x = TRUE)
  cont_per_age_per_part[is.na(V1), V1:=0]
  
  
  #DEFINE THE OPTIMISATION ROUTINE FOR NEG_BINOM (to be run on each pair of age groups)
  nbinom_optim <- function(i, j, param, n = truncate_) {
    
    complementary_logprob <- function(x) {
      tryCatch(log1p(-sum(exp(x))), error=function(e) -Inf)
    }
    nb_loglik <- function(x, par) {
      k <- par[["k"]]
      mean <- par[["mu"]]
      ll <- rep(NA_real_, length(x))
      ll[x < n] <- dnbinom(x[x < n], mu = mean, size = 1/k, log = TRUE)
      ll[x >= n] <- complementary_logprob(dnbinom(1:n, mu = mean, size = 1/k, log = TRUE))
      return(-sum(ll))
    }
    
    
    counts = cont_per_age_per_part[prt_age_group == i & cnt_age_group == j]$V1
    
    if(sum(counts != 0)){
      outs = optim(c(mu = 0.2, k = 0.2), lower = c(mean = 1e-5, k = 1e-5), nb_loglik, x = counts, method = "L-BFGS-B")
      return(as.numeric(outs$par[param]) )
    } else{
      return(0)
    }
  }
  
  
  # Get columns of age-groups to put into mapply
  eg = expand.grid(sort(levs),sort(levs))
  names(eg) = c('age_group', 'age_group_cont')
  # Get means from neg_binom opitmsisation
  eg['means'] = mapply(nbinom_optim, eg$age_group, eg$age_group_cont, param='mu')
  # eg['size'] = mapply(nbinom_optim, eg$age_group, eg$age_group_cont, param='size')
  
  return(list(eg, sample_props))
  
}



# Sample with replacement -------------------------------------------------


nbinom_optim_bs <- function(i, j, param, n, count_frame, bs = 1) {
  
  counts = count_frame[count_frame$prt_age_group == i & count_frame$cnt_age_group == j]$V1
  
  bs_optim <- function(counts_ ) {
    if(sum(counts_) == 0){
      return(0)
    }else{
      outs = optim(c(mu = 0.5, k = 1), lower = c(mean = 1e-5, k = 1e-5), nb_loglik, x = counts_, n = n, method = "L-BFGS-B")
      return(as.numeric(outs$par[param]))
    }
    
  }
  
  if(sum(counts != 0)){
    counts_mat = c(counts)
    for(k in 1:(bs-1)){
      counts_mat =  rbind(counts_mat, sample(counts, replace = TRUE))
    }
    
    outs_mat = apply(counts_mat, FUN = bs_optim, MARGIN = 1)
    outs_mat
  } else{
    return(rep(0,bs))
  }
}

get_matrix_bs = function(cont_per_age_per_part, breaks, trunc, param = 'mu', bs = 1) {
  
  
  levs <- unique(unlist(cut(seq(0,120),breaks, right=FALSE), use.names = FALSE))
  # Get columns of age-groups to put into mapply
  eg = expand.grid(sort(levs),sort(levs))
  names(eg) = c('age_group', 'age_group_cont')
  # Get means from neg_binom opitmsisation
  means_mat <- mapply(nbinom_optim_bs, eg$age_group, eg$age_group_cont, param=param, n=trunc, bs = bs, count_frame=list(cont_per_age_per_part ))
  # eg['size'] = mapply(nbinom_optim, eg$age_group, eg$age_group_cont, param='size')
  means_mat <- matrix(unlist(means_mat), ncol = (length(breaks)-1)^2)
  eg = data.table(eg)
  return(list(eg, means_mat))
}



