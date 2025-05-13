data{
  int<lower = 1> n; // number of neighborhoods
  real y[n];         // car counts divided by time
  vector[n] popchange; // Population change from 2011 to 2016
  vector[n] seniors; // Population of seniors
  vector[n] collegeDeg; // College Degree Holders
  vector[n] labourParticp; // Female Participation rate in labour force
  vector[n] distance; // Distance to city center
  vector[n] majorBikeLanes; // Number of major bike lanes
  vector[n] minorBikeLanes; // Number of minor bike lanes
  real<lower = 0> wmat[n, n];  // weight matrix
} 

parameters{
  real u[n]; // Spatial correlation terms
  real beta1; // Model parameters
  real beta2;
  real beta3;
  real beta4;
  real beta5;
  real beta6;
  real beta7;
  real<lower=0> sigma2_u; // Spatial Variance
  real<lower=0> sigma2_v; // LM variance
}

model{
  sigma2_u ~ inv_gamma(1000,21);
  sigma2_v ~ inv_gamma(10,21);
  beta1 ~ uniform(-30,30);
  beta2 ~ uniform(-20,20);
  beta3 ~ uniform(-20,20);
  beta4 ~ uniform(-20,20);
  beta5 ~ uniform(-20,20);
  beta6 ~ uniform(-20,20);
  beta7 ~ uniform(-20,20);
  target +=  -0.5*n*log(sigma2_u);  
  
  // weights being zero on the diagonal takes care of equal i and j case
  for (i in 1:n) {
   for (j in 1:n) {
     target +=  -(u[i] - u[j])^2 * wmat[i,j] / (2 * sigma2_u);
   } 
  }

  for (i in 1:n) {
    y[i] ~ normal(beta1*popchange[i] + beta2*seniors[i] +beta3*collegeDeg[i] + 
                  beta4*labourParticp[i] + beta5*distance[i] +
                  beta6*majorBikeLanes[i] + beta7*minorBikeLanes[i] + u[i], sqrt(sigma2_v));
  }
  
}

