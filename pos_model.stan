data {
  int<lower=0> J;                  // number of clusters (e.g., activities)
  int<lower=0> N;                  // number of observations
  array[N] real y;                 // observations
  array[J, N] int<lower=0, upper=1> g; // observed binary activity indicators
}
parameters {
  vector[J] a;                     // slopes for each activity
  real b;                          // intercept
  real<lower=0> sigma;             // standard deviation
}
transformed parameters {
  real mu[N];                      // mean for each observation
  for (n in 1:N) {
    mu[n] = b;
    for (j in 1:J) {
      mu[n] += a[j] * g[j, n];
    }
  }
}
model {
  // Likelihood
  y ~ normal(mu, sigma);

  // Priors
  a ~ normal(0, 1);                // Prior for slopes
  b ~ normal(0, 1);                // Prior for intercept
  sigma ~ normal(0, 1);            // Prior for standard deviation
}
