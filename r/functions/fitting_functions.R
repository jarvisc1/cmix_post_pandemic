## Name: fitting_functions
## Description: Contains functions used for fitting distributions
## Input file: NONE
## Functions: complementary_logprob, nb_loglik, nbinom_optim
## Output file: 


# Packages ----------------------------------------------------------------
library(data.table)


# Source user written scripts ---------------------------------------------



# Input data ----------------------------------------------------------------


# this function calculates log of the complement of the sum of a list of liklihoods. 
# It is used to find the log liklihood of a tail of a right censored distribution
complementary_logprob <- function(x) {
  tryCatch(log1p(-sum(exp(x))), error=function(e) -Inf)
}


# This funtion calculates the likelihood of a negarive binomial disrtibution given set of partameters 'par' and data 'x'.
nb_loglik <- function(x, par, n) {
  k <- par[["k"]]
  mean <- par[["mu"]]
  ll <- rep(NA_real_, length(x))
  ll[x < n] <- dnbinom(x[x < n], mu = mean, size = 1/k, log = TRUE)
  ll[x >= n] <- dnbinom(n, mu = mean, size = 1/k, log = TRUE)
  return(-sum(ll))
}

# This function optimises negative binomial parameters mu and k for contacts reported by age group i in age group j  
nbinom_optim_ <- function(i, j, param, n, count_frame) {
  
  counts = count_frame[count_frame$prt_age_group == i & count_frame$cnt_age_group == j]$V1
  
  if(sum(counts != 0)){
    outs = optim(c(mu = 0.5, k = 1), lower = c(mean = 1e-5, k = 1e-5), nb_loglik, x = counts, n = n, method = "L-BFGS-B")
    return(as.numeric(outs$par[param]) )
  } else{
    return(0)
  }
}


nbinom_optim_stan_multi = function(model, n, count_frame, eg) {
  counts = c()
  
  for (i in 1:length(eg$age_group)){
    new_counts = count_frame[count_frame$prt_age_group == eg$age_group[i] & count_frame$cnt_age_group == eg$age_group_cont[i]]$V1
    new_counts = append(new_counts, rep(1e6, 1000 - length(new_counts)))
    counts = append(counts, new_counts)
  }
  
  counts = matrix(counts, nrow=1000)
  
  data = list(N=1000, P=81, y=counts, t=n)
  
  init_fn <- function(){
    return(list(mu = truncnorm::rtruncnorm(n =81, a = 0, b = Inf, mean = 3, sd = 3),
                k = truncnorm::rtruncnorm(n = 81, a = 0, b = Inf, mean = 0.000001, sd = 0)))}
  
  fit = rstan::sampling(model, data, iter=1000, warmup=500, chains=1,init = init_fn)
  
  return(fit)
  
}

nbinom_optim_stan_multi_BunchTrunc = function(model, n, count_frame, eg, hard_cap=TRUE, samp, priors_mu, priors_k) {
  cores = parallel::detectCores() - 1
  counts = c()
  N_t = c()
  N_o = c()
  
  groups = sort(unique(eg$age_group))
  
  P = length(groups)^2
  
  for (i in 1:length(eg$age_group)){
    new_counts = count_frame[count_frame$prt_age_group == eg$age_group[i] & count_frame$cnt_age_group == eg$age_group_cont[i]]$V1
    if(hard_cap == TRUE){
      new_counts[new_counts>n] = n
      }
    new_obs = new_counts[new_counts<=n]
    N_o = append(N_o, length(new_obs))
    new_obs = append(new_obs, rep(1e6, 1000 - length(new_obs)))
    N_t = append(N_t, length(new_counts[new_counts>n]))
    
    
    counts = append(counts, new_obs)
  }
  
  counts = matrix(counts, nrow=1000)
  
  data = list(N=1000, P=P, y=counts, t=n, N_cens=N_t, N_obs=N_o, rate_mu=priors_mu[1,], shape_mu=priors_mu[2,], rate_k=priors_k[1,], shape_k=priors_k[2,])
  
  init_fn <- function(){
    return(list(mu = truncnorm::rtruncnorm(n =P, a = 0, b = Inf, mean = 3, sd = 3),
                k = truncnorm::rtruncnorm(n = P, a = 0, b = Inf, mean = 0.000001, sd = 0)))}
  
  fit = rstan::sampling(model, data, iter=samp * 2, warmup=samp, chains=1, init = init_fn, control=list(adapt_delta=0.99))
  
  return(fit)
  
} 
  


#fit gamma ---------------------

gm_loglik = function(x,par){ -sum(sapply(x, FUN=dgamma, rate=par[['rate']], shape=par[['shape']], log=TRUE))}

gamma_optim = function(x_){
  print('count')
  fit = optim(c(rate=1, shape=1), 
              gm_loglik, 
              lower = c(mean = 1e-5, k = 1e-5),
              x = x_, 
              method = "L-BFGS-B")  
  
  return(fit$par)
  
}

