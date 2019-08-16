data {
      // define los datos y otros insumos del modelo
      int<lower=1> n;
      int<lower=0> y[n];
      real x[n];
}
parameters{
      // describe los parametros (los que tiene previa - tiene que tener distribución en el bloque de modelo)
      real alpha;
      real beta;
}
transformed parameters {
      // describe transformaciones de los parametros (si son necesarias)
      real<lower=0, upper=1> tita[n];
      for(i in 1:n) tita[i] = inv_logit(alpha + beta*x[i]);
}
model {
      // describe el modelo, como se distribuyen todas las variables
      alpha ~ normal(0, 10);
      beta ~ normal(0, 10);
      for(i in 1:n) y[i] ~ binomial(5, tita[i]);
}