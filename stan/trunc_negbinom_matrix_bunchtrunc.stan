//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  int<lower=0> P;
  int<lower=0> y[N,P];
  int<lower=0> t;
  int<lower=0> N_cens[P];
  int<lower=0> N_obs[P];
  real<lower=0> rate_mu[P];
  real<lower=0> shape_mu[P];
  real<lower=0> rate_k[P];
  real<lower=0> shape_k[P];
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real<lower=0> mu[P];
  real<lower=0> k[P];
  
}

transformed parameters{
  
  real sqrtk[P];
  
  sqrtk = sqrt(k);
  
}

// The model to be estimated. We model the output
// 'y' to be distributed as a negative binomialwith mean 'mu'
// and overdispersion 'k'. Values over the trubction value t are 
// assumed to sit in the tail of the distribution fitted to the lowe values
model {
  
  
 // for (p in 1:P){
 //   mu[p] ~ gamma(shape_mu[p], rate_mu[p]);
 //}
    mu ~ exponential(2);
    k ~ normal(0,1);

  for (p in 1:P){
    for(n in 1:N_obs[p]){

       target += neg_binomial_2_lpmf(y[n,p] | mu[p], 1/sqrtk[p]);
        
    }
    if(N_cens[p] > 0)   
       target += N_cens[p] * neg_binomial_2_lccdf(t | mu[p], 1/sqrtk[p]);
       
  }
    
}




