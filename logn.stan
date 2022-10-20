data {
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
}
parameters {
  vector[K] b;  // population-level effects
  real<lower=0> sigma;  // dispersion parameter
}
model {
  // likelihood including constants
    // initialize linear predictor term
    vector[N] mu = X * b - sigma*sigma / 2.0;
    target += lognormal_lpdf(Y | mu, sigma);
}