data {
int<lower=1> n; // número de observaciones
int y[n];       // cantidad de acidentes fatales
int x[n];       // cantidad de los muertos
real a;         // 
real b;         // 
real c;         // 
real d;         // 
}
parameters{
real<lower=0> lambdas[n];
real<lower=0> mu;
real<lower=0> tau;
}
transformed parameters {
real alpha;
real beta;
alpha = mu^2/tau;
beta = mu/tau;
}
model {
mu ~ gamma(a, b);
tau ~ gamma(c, d);
// target += gamma_lpdf(mu | a, b);
// target += gamma_lpdf(tau | c, d);
for (i in 1:n) lambdas[i] ~ gamma(alpha, beta);
for (i in 1:n) y[i] ~ poisson(x[i]*lambdas[i]);
}
