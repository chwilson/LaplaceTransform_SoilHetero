
data {
  int<lower=0> N;
  vector[N] y;
  real<lower=0> time[N]; 
}


parameters {
  real<lower=0> mu; 
  real<lower=0> phi; 
 //  real<lower=0> mu2; 
//  real<lower=0> phi2; 
//  real<lower=0,upper=1> alpha; 
  real<lower=0> sigma;
}

model {
  
    mu ~ normal(0,100);
    phi ~ exponential(1); 
    
  for(i in 1:N){
  
  //  mu2 ~ normal(0,100);
  //  phi2 ~ exponential(0.01); 
  //  alpha ~ beta(2,2); 
    
  y[i] ~ normal(((mu*((1/phi)+1))/(mu*((1/phi)+1) + time[i]))^((1/phi)+2),sigma);
      // (1-alpha)*((mu2*((1/phi2)+1))/(mu2*((1/phi2)+1) + time[i]))^((1/phi2)+2), sigma);
  }
}

