data {
  int<lower=1> N; // observaciones
  int<lower=1> S; // escuelas
  real y[N];      // score de matematica 
  int scl[N];     // indice de las escuelas
  real m0;        // media para previa de mu
  real t0;        // desvio para la previa de mu
  real a;         // info previa de tau
  real t;         // media en previa de tau
  real b;         // info en previa de sigma
  real s;         // media en previa de sigma
}
parameters{
  real mus[S];
  real mu;
  real<lower=0> tau2;
  real<lower=0> sigma2;
}
transformed parameters {
  real tau;
  real sigma;
  tau = sqrt(tau2);
  sigma = sqrt(sigma2);
}
model {
  mu ~ normal(m0, t0);
  tau2 ~ inv_gamma(a/2, a*t/2);
  sigma2 ~ inv_gamma(b/2, b*s/2);
  for (j in 1:S) mus[j] ~ normal(mu, tau );
  for (i in 1:N) y[i] ~ normal( mus[ scl[i] ], sigma );
}
