data {
      // define los datos y otros insumos del modelo
      int<lower=1> n; // la cantidad de datos totales
      int k; // la cantidad de variables
      int<lower=0> y[n]; // el vector de datos, en nuestro caso, y_i: horas trabajadas
      matrix[n, k] x; // es una matrix de tamaño n x k de variables de control
}
parameters{
      // describe los parametros (los que tienen previa - tienen que tener distribución en el bloque de modelo)
      vector[k] beta[k]; // coeficientes beta de la regresión
}
model {
      // describe el modelo, como se distribuyen todas las variables
      beta ~ normal(0, 10);
      for(i in 1:n) y[i] ~ poisson(tita[i]);
}
