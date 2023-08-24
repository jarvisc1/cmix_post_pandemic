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
  int y[N,P];
  int<lower=0> t;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real<lower=0> mu[P];
  real<lower=0> k[P];
  real<lower=0> theta[P];
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  for (p in 1:P){
      for(n in 1:N){
//        // y[n] ~ neg_binomial_2(mu, 1./k); // no truncation here
//        // target += - log1m(neg_binomial_2_lpmf(t | mu, 1./k)); // manually adjusting computation of likelihood
//         //y[n,p] ~ neg_binomial_2(mu[p], k[p]);
//         
          if (y[n,p] == 0)
            target += log_sum_exp(bernoulli_lpmf(1 | theta[p]), bernoulli_lpmf(0 | theta[p]))  + neg_binomial_2_lpmf(y[n,p] | mu[p], k[p]);
            
          else if (y[n,p] > t && y[n,p] < 1e5)
            target += bernoulli_lpmf(0 | theta[p])  -  neg_binomial_2_lcdf(t | mu[p], k[p]);
            
          else if (y[n,p] < t  &&  y[n,p] > 0)
            target += bernoulli_lpmf(0 | theta[p])  +  neg_binomial_2_lpmf(y[n,p] | mu[p], k[p]);
       }
    }
  }
