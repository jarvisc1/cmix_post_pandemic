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
  
}

// The model to be estimated. We model the output
// 'y' to be distributed as a negative binomialwith mean 'mu'
// and overdispersion 'k'. Values over the trubction value t are 
// assumed to sit in the tail of the distribution fitted to the lowe values
model {
  
  mu ~ exponential(2);
  k ~  exponential(0.001);

  for (p in 1:P){
       for(n in 1:N){

         if (y[n,p] >= t && y[n,p] < 1e5)
           target += neg_binomial_2_lccdf(t | mu[p], k[p]);
         else if (y[n,p] < t)
           target += neg_binomial_2_lpmf(y[n,p] | mu[p], k[p]);
           
       }
  }
}

