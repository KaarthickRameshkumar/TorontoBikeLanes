data{
  int<lower = 1> n; // number of neighborhoods
  int y[n];         // car counts divided by time
  vector[n] distance; // Distance to city center
  vector[n] majorBikeLanes; // Number of major bike lanes
  vector[n] minorBikeLanes; // Number of minor bike lanes
  vector[n] logPT; // Poisson offset
  real<lower = 0> wmat[n, n];  // weight matrix
} 


parameters{
  real theta[n];
  real u[n];
  real beta5;
  real beta6;
  real beta7;
  real<lower=0> sigma2_u;
  real<lower=0> sigma2_v;
}

model{
  sigma2_u ~ inv_gamma(0.0005, 0.5);
  sigma2_v ~ inv_gamma(0.0005, 0.5);
  beta5 ~ uniform(-5,5);
  beta6 ~ uniform(-5,5);
  beta7 ~ uniform(-5,5);

  target +=  -0.5*n*log(sigma2_u);  
  for (i in 1:n) {
   for (j in 1:n) {
     target +=  -(u[i] - u[j])^2 * wmat[i,j] / (2 * sigma2_u);
   } 
  }

  for (i in 1:n) {
    theta[i] ~ normal(beta5*distance +
                  beta6*majorBikeLanes + beta7*minorBikeLanes + 
                  logPT[i] + u[i], sqrt(sigma2_v));
    y[i] ~ poisson(exp(theta[i]));
  }
  
}
