## calculate the contact matrices


calc_cm_general <- function(parts_ , conts_, breaks, max_ = 1000, popdata_totals, weeks_range=23:33, nwks=2, samples=10, fitwith='bs', outfolder='outputs/regular/', model_path='stan/trunc_negbinom_matrix_bunchtrunc.stan', prior_pars_mu=NULL, prior_pars_k=NULL){
  
  if(!dir.exists(paste0(outfolder, 'contact_matrices/'))){
    dir.create(paste0(outfolder, 'contact_matrices/'), recursive = TRUE)
  }
    
  
  conts_weekday = conts_[!weekday %in% c('Saturday', 'Sunday')]
  conts_weekend = conts_[weekday %in% c('Saturday', 'Sunday')]
  parts_weekday = parts_[!weekday %in% c('Saturday', 'Sunday')]
  parts_weekend = parts_[weekday %in% c('Saturday', 'Sunday')]
  
  
  if(fitwith == 'stan'){
    model = rstan::stan_model(model_path)
    if(is.null(prior_pars_mu)){
      prior_pars_mu = list(rbind(rep(1, (length(breaks) - 1)^2), rep(2, (length(breaks) - 1)^2)), rbind(rep(1, (length(breaks) - 1)^2), rep(2, (length(breaks) - 1)^2)))
    }
    if(is.null(prior_pars_k)){
      prior_pars_k = list(rbind(rep(1, (length(breaks) - 1)^2), rep(0.01, (length(breaks) - 1)^2)), rbind(rep(1, (length(breaks) - 1)^2), rep(0.01, (length(breaks) - 1)^2)))
    }
    
  }
  
  
  egs <- list()
  prior_table = list()
  # with weighting for weekend / weekday
  for(week in weeks_range){
    i = week 
    #print(i)
    weeks <- week:(week + nwks - 1)
    filename_primer = paste0(outfolder, 'contact_matrices/', fitwith, samples, '_ngrps', length(breaks) - 1, '_cap', max_, '_nwks', length(weeks),'_sr', week, '_')
    
    if(i %in% c(1:6, 17,18))  weeks <- c(weeks, 700)
    
    ct_ac_weekend = get_age_table(conts_weekend, parts_weekend, weeks, breaks)
    cont_per_age_per_part_weekend  = ct_ac_weekend[[1]]
    all_conts_weekend  = ct_ac_weekend[[2]]
    
    ct_ac_weekday = get_age_table(conts_weekday, parts_weekday, weeks, breaks)
    cont_per_age_per_part_weekday  = ct_ac_weekday[[1]]
    all_conts_weekday  = ct_ac_weekday[[2]]
    
    
    if (fitwith == 'stan'){
      outs_weekend = get_matrix_stan(cont_per_age_per_part_weekend, breaks, max_, model = model, samp=samples, prior_pars_mu=prior_pars_mu[[1]],prior_pars_k=prior_pars_k[[1]])
      outs_weekday = get_matrix_stan(cont_per_age_per_part_weekday, breaks, max_, model = model, samp=samples, prior_pars_mu=prior_pars_mu[[2]],prior_pars_k=prior_pars_k[[2]]) 
      prior_pars_mu = list(sapply(1:length(outs_weekend[[1]]$age_group), function(X){gamma_optim(outs_weekend[[2]][,X])}), sapply(1:length(outs_weekday[[1]]$age_group), function(X){gamma_optim(outs_weekday[[2]][,X])}))
      #prior_pars_k  = list(sapply(1:length(outs_weekend[[1]]$age_group), function(X){gamma_optim(outs_weekend[[3]][,X])}), sapply(1:length(outs_weekday[[1]]$age_group), function(X){gamma_optim(outs_weekday[[3]][,X])}))
      qs::qsave(outs_weekend[[2]], paste0(filename_primer, 'wecms.qs') )
      qs::qsave(outs_weekday[[2]], paste0(filename_primer, 'wdcms.qs') )
      qs::qsave(prior_pars_mu, paste0(filename_primer, 'prrs.qs') )
      
      

      
      
    }
    
    if (fitwith == 'bs'){
      outs_weekend = get_matrix_bs(cont_per_age_per_part_weekend, breaks, max_, bs=samples)
      outs_weekday = get_matrix_bs(cont_per_age_per_part_weekday, breaks, max_, bs=samples) 
    }
    
    mus = (outs_weekend[[2]] * 2./7) + (outs_weekday[[2]] * 5./7)
    
    
    symats = get_symmetric_means(data.table(outs_weekday[[1]]), mus, popdata_totals, breaks)
    #filename_primer = paste0(outfolder, 'contact_matrices/', fitwith, samples, '_cap', max_, '_nwks', length(weeks),'_sr', week, '_')
    print(paste0(filename_primer, 'scms.qs') )
    qs::qsave(symats, paste0(filename_primer, 'scms.qs') )
    

    egs[[i]] <- symats
  }
  return(list(outs_weekday[[1]], egs))
}






# OLD VERSIONS FOR REFERENCE ONLY--------------------------------------------------------------------------------------




calc_cm <- function(parts_ , conts_, max_ = 1000, weeks_range = 1:30, param = "mu", pop_data_ = popdata_totals){
  conts_weekday = conts_[!weekday %in% c('Saturday', 'Sunday')]
  conts_weekend = conts_[weekday %in% c('Saturday', 'Sunday')]
  parts_weekday = parts_[!weekday %in% c('Saturday', 'Sunday')]
  parts_weekend = parts_[weekday %in% c('Saturday', 'Sunday')]
  
  egs <- list()
  
  # with weighting for weekend / weekday
  for(week in weeks_range){
    i = week - min(weeks_range)+1
    #print(i)
    weeks <- c(week, week + 1)
    #if(i %in% c(1:6, 17,18))  weeks <- c(weeks, 700)
    outs_weekday = get_matrix(conts_weekday, parts_weekday, weeks, breaks, truncate_ = max_, param = param)
    outs_weekend = get_matrix(conts_weekend, parts_weekend, weeks, breaks, truncate_ = max_, param = param)
    eg_weekday = data.table(outs_weekday[[1]])
    eg_weekend = data.table(outs_weekend[[1]])
    
    eg = merge(eg_weekday, eg_weekend, by=c('age_group', 'age_group_cont'), suffixes = c('_weekday', '_weekend'))
    eg[,means := (means_weekday*(5./7.) + means_weekend*(2./7.))]
    
    eg_props = symetricise_matrix(eg, pop_data_, breaks)
    egs[[i]] <- eg_props
  }
  return(egs)
}

calc_cm_stan <- function(parts_ , conts_, max_ = 1000, popdata_totals, weeks_range=23:33, outfolder='outputs/regular/'){
  
  
  conts_weekday = conts_[!weekday %in% c('Saturday', 'Sunday')]
  conts_weekend = conts_[weekday %in% c('Saturday', 'Sunday')]
  parts_weekday = parts_[!weekday %in% c('Saturday', 'Sunday')]
  parts_weekend = parts_[weekday %in% c('Saturday', 'Sunday')]
  
  model = rstan::stan_model('stan/trunc_negbinom_matrix_bunchtrunc.stan')
  
  
  egs <- list()
  
  # with weighting for weekend / weekday
  for(week in weeks_range){
    i = week 
    #print(i)
    weeks <- c(week, week + 1)
    if(i %in% c(1:6, 17,18))  weeks <- c(weeks, 700)
    
    ct_ac_weekend = get_age_table(conts_weekend, parts_weekend, weeks, breaks)
    cont_per_age_per_part_weekend  = ct_ac_weekend[[1]]
    all_conts_weekend  = ct_ac_weekend[[2]]
    
    ct_ac_weekday = get_age_table(conts_weekday, parts_weekday, weeks, breaks)
    cont_per_age_per_part_weekday  = ct_ac_weekday[[1]]
    all_conts_weekday  = ct_ac_weekday[[2]]
    
    
    
    outs_weekend = get_matrix_stan(cont_per_age_per_part_weekend, breaks, max_, model)
    outs_weekday = get_matrix_stan(cont_per_age_per_part_weekday, breaks, max_, model)
    
    mus = (outs_weekend[[2]] * 2./7) + (outs_weekday[[2]] * 5./7)
    
    
    symats = get_eigs_from_means(data.table(outs_weekday[[1]]), mus, popdata_totals, breaks)
    filename_primer = paste0(outfolder, format(lubridate::today(),"%Y%m%d" ),'_cap', max_, '_nwks', length(weeks),'_sr', week, '_')
    qs::qsave(symats, paste0(filename_primer, 'scms.qs') )
    
    
  
    egs[[i]] <- symats
  }
  return(list(outs_weekday[[1]], egs))
}



calc_cm_bs <- function(parts_ , conts_, max_ = 1000, pop_data_ = popdata_totals, weeks_range=23:33, outfolder='outputs/regular/', bs = 1){
  
  
  conts_weekday = conts_[!weekday %in% c('Saturday', 'Sunday')]
  conts_weekend = conts_[weekday %in% c('Saturday', 'Sunday')]
  parts_weekday = parts_[!weekday %in% c('Saturday', 'Sunday')]
  parts_weekend = parts_[weekday %in% c('Saturday', 'Sunday')]
  
  egs <- list()
  
  # with weighting for weekend / weekday
  for(week in weeks_range){
    i = week 
    #print(i)
    weeks <- c(week, week + 1)
    if(i %in% c(1:6, 17,18))  weeks <- c(weeks, 700)
    
    ct_ac_weekend = get_age_table(conts_weekend, parts_weekend, weeks, breaks)
    cont_per_age_per_part_weekend  = ct_ac_weekend[[1]]
    all_conts_weekend  = ct_ac_weekend[[2]]
    
    ct_ac_weekday = get_age_table(conts_weekday, parts_weekday, weeks, breaks)
    cont_per_age_per_part_weekday  = ct_ac_weekday[[1]]
    all_conts_weekday  = ct_ac_weekday[[2]]
    
    outs_weekend = get_matrix_bs(cont_per_age_per_part_weekend, breaks, max_, bs = bs)
    outs_weekday = get_matrix_bs(cont_per_age_per_part_weekday, breaks, max_, bs = bs)
    
    mus = (outs_weekend[[2]] * 2./7) + (outs_weekday[[2]] * 5./7)
    
    
    eigs_symats = get_eigs_from_means(data.table(outs_weekday[[1]]), mus, pop_data_, breaks)
    filename_primer = paste0(outfolder, format(lubridate::today(),"%Y%m%d" ),'_cap', max_, '_nwks', length(weeks),'_sr', week, '_bs_',bs)
    qs::qsave(eigs_symats[[1]], paste0(filename_primer, 'eigs.qs') )
    qs::qsave(eigs_symats[[2]], paste0(filename_primer, 'scms.qs') )

    
  
    egs[[i]] <- eigs_symats
  }
  return(list(outs_weekday[[1]], egs))
}

calc_cm_prem_bs <- function(parts_ , conts_, max_ = 1000, pop_data_ = popdata_totals, weeks_range=23:33, outfolder='outputs/regular/', bs = 1){
  
  
  conts_weekday = conts_[!weekday %in% c('Saturday', 'Sunday')]
  conts_weekend = conts_[weekday %in% c('Saturday', 'Sunday')]
  parts_weekday = parts_[!weekday %in% c('Saturday', 'Sunday')]
  parts_weekend = parts_[weekday %in% c('Saturday', 'Sunday')]
  
  egs <- list()
  
  # with weighting for weekend / weekday
  for(week in weeks_range){
    i = week 
    #print(i)
    weeks <- c(week, week + 1)
    if(i %in% c(1:6, 17,18))  weeks <- c(weeks, 700)
    
    ct_ac_weekend = get_age_table(conts_weekend, parts_weekend, weeks, breaks_prem)
    cont_per_age_per_part_weekend  = ct_ac_weekend[[1]]
    all_conts_weekend  = ct_ac_weekend[[2]]
    
    ct_ac_weekday = get_age_table(conts_weekday, parts_weekday, weeks, breaks_prem)
    cont_per_age_per_part_weekday  = ct_ac_weekday[[1]]
    all_conts_weekday  = ct_ac_weekday[[2]]
    
    outs_weekend = get_matrix_bs(cont_per_age_per_part_weekend, breaks_prem, max_, bs = bs)
    outs_weekday = get_matrix_bs(cont_per_age_per_part_weekday, breaks_prem, max_, bs = bs)
    
    mus = (outs_weekend[[2]] * 2./7) + (outs_weekday[[2]] * 5./7)
    
    
    eigs_symats = get_eigs_from_means(data.table(outs_weekday[[1]]), mus, pop_data_, breaks_prem)
    filename_primer = paste0(outfolder, format(lubridate::today(),"%Y%m%d" ),'_cap', max_, '_nwks', length(weeks),'_sr', week, '_bs_',bs)
    qs::qsave(eigs_symats[[1]], paste0(filename_primer, 'eigs.qs') )
    qs::qsave(eigs_symats[[2]], paste0(filename_primer, 'scms.qs') )
    
    
    
    egs[[i]] <- eigs_symats
  }
  return(list(outs_weekday[[1]], egs))
}



# calc_cm_bs <- function(parts_ , conts_, max_ = 1000, weeks_range = 1:30, param = "mu"){
#   conts_weekday = conts_[!weekday %in% c('Saturday', 'Sunday')]
#   conts_weekend = conts_[weekday %in% c('Saturday', 'Sunday')]
#   parts_weekday = parts_[!weekday %in% c('Saturday', 'Sunday')]
#   parts_weekend = parts_[weekday %in% c('Saturday', 'Sunday')]
#   
#   egs <- list()
#   
#   # with weighting for weekend / weekday
#   for(week in weeks_range){
#     i = week - min(weeks_range)+1
#     #print(i)
#     weeks <- c(week, week + 1)
#     #if(i %in% c(1:6, 17,18))  weeks <- c(weeks, 700)
#     outs_weekday = get_matrix_bs(conts_weekday, parts_weekday, weeks, breaks, truncate_ = max_, param = param)
#     outs_weekend = get_matrix_bs(conts_weekend, parts_weekend, weeks, breaks, truncate_ = max_, param = param)
#     eg_weekday = data.table(outs_weekday[[1]])
#     eg_weekend = data.table(outs_weekend[[1]])
#     
#     eg = merge(eg_weekday, eg_weekend, by=c('age_group', 'age_group_cont'), suffixes = c('_weekday', '_weekend'))
#     eg[,means := (means_weekday*(5./7.) + means_weekend*(2./7.))]
#     
#     eg_props = symetricise_matrix(eg, popdata_totals, breaks)
#     egs[[i]] <- eg_props
#   }
#   return(egs)
# }
# 





