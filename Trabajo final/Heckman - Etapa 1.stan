data {
      int<lower=1> n;   // la cantidad de datos totales
      int k;            // la cantidad de variables
      int<lower=0,upper=1> t[n]; // el vector de datos: t_i = 1 si trabaja o t_i = 0 si no trabaja
      row_vector[k] z[n];   // es una matrix de tamaño n x k de variables de control
}
parameters{
      vector[k] gamma;     // coeficientes gamma del PROBIT
}
transformed parameters {
      vector<lower=0,upper=1>[n] mu2;
      for(i in 1:n) mu2[i] = Phi(z[i] * gamma);
}
model {
      for(i in 1:k) gamma[i] ~ normal(0, 10);
      for(i in 1:n) t[i] ~ bernoulli(mu2[i]);
}
