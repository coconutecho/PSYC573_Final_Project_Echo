data {
  int<lower=0> J;                  // number of clusters (e.g., activities)
  int<lower=0> N;                  // number of observations
  int<lower=0> P;                  // number of participants
  array[N] real y;                 // observations
  array[N] int pid;               // participant id
  array[N,J] int<lower=0, upper=1> g; // observed binary activity indicators
}
parameters {
  matrix[P,J] a;                   // slopes for each participant & activity
  array[P] real b;                 // intercept for each participant
  real<lower=0.00001> sigma;       // standard deviation
}
transformed parameters {
  // calculations
  matrix[N,J] g_mat;
  g_mat = to_matrix(g);
  vector[N] mu;                // mean for each observation
  mu = to_vector(b[pid]) + to_vector(rows_dot_product(g_mat, a[pid, ]));
}
model {
  // Likelihood
  y ~ normal(mu, sigma);

  // Priors
  to_vector(a) ~ normal(0, 1);     // Prior for slopes
  b ~ normal(0, 1);                // Prior for intercept
  sigma ~ cauchy(0, 2);            // Prior for standard deviation
}
generated quantities {
  array[N] real ytilde;
  ytilde = normal_rng(mu, sigma);
}
