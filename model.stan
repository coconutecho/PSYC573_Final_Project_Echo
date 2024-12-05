data {
  int<lower=0> J;                  // number of clusters (e.g., activities)
  int<lower=0> N;                  // number of observations
  array[N] real y;                 // observations
  array[N,J] int<lower=0, upper=1> g; // observed binary activity indicators
}
parameters {
  vector[J] a;                     // slopes for each activity
  real b;                          // intercept
  real<lower=0.00001> sigma;             // standard deviation
}
transformed parameters {
  array[N] real mu;                // mean for each observation
  for (i in 1:N) {
    mu[i] = b;
    for (j in 1:J) {
      mu[i] += a[j] * g[i,j];
    }
  }
}
model {
  // Likelihood
  y ~ normal(mu, sigma);

  // Priors
  a ~ normal(0, 1);                // Prior for slopes
  b ~ normal(0, 1);                // Prior for intercept
  sigma ~ cauchy(0, 2);            // Prior for standard deviation
}
generated quantities {
  array[N] real ytilde;
  for (i in 1:N) {
    ytilde[i] = normal_rng(mu[i], sigma);
  }
}