// Runs a PROBIT and returns a gamma coefs and ystar (a measure of latent utility)

data {
      int<lower=1> n;                     // la cantidad de datos totales
      int k;                              // la cantidad de variables
      int<lower=0> t[n];                  // el vector de datos, en nuestro caso: t_i = 1 si trabaja o t_i = 0 si no trabaja
      vector<lower=-1,upper=1> sign;      // y esto qué mierda hace?
      matrix[n, k] z;                     // es una matrix de tamaño n x k de variables de control
}
parameters{
      vector[k] gamma[k];     // coeficientes gamma del PROBIT
}
model {
      vector[n] mu2;
      gamma ~ normal(0, 10);
      mu2 <- z*gamma;
      for(i in 1:n) mu2[i] <- Phi(mu2[i]);
      t ~ bernoulli(mu);
}
generated quantities{
      // esta sección no entiendo nada!!
      vector[n] ystar;
      {
            vector[n] mu2;
            mu2 <- z*gamma;
            for(i in 1:n){
                  real draw;
                  draw <- not_a_number();
                  if(sign[n] == 1) while(!(draw > 0)) draw <- normal_rng(mu2[n], 1);
                  else while(!(draw < 0)) draw <- normal_rng(mu2[n], 1);
            }
      }
}
