library(tidyverse)
library(cmdstanr)

set.seed(79)


dfm.ag <- read.csv("Echo_Beyesian_data.csv", header = TRUE, sep =",")

dfm.ag$Activity.Category <- relevel(as.factor(dfm.ag$Activity.Category), ref="Rest and Sleep")

#####
# Centering within and between person for each emotion
#####
dfm.ag <- dfm.ag %>% group_by(record_id, Composite) %>% 
  dplyr::mutate(w_mean = mean(value, na.rm = T)) %>% ungroup() %>%
  dplyr::mutate(value_bc = scale(value, center = T, scale = T)[,1],
                value_wc = value - w_mean) 

dfm.ag <- dfm.ag %>%
  mutate(
    Composite = as.factor(Composite),
    Activity.Category = as.factor(Activity.Category),
    value = as.numeric(value)
  )

dfm.ag <- dfm.ag %>% drop_na(value_bc)
dfm.pos <- dfm.ag %>% filter(Composite == "PosAff")
dfm.neg <- dfm.ag %>% filter(Composite == "NegAff")
dfm.oth <- dfm.ag %>% filter(Composite == "Other")

df = dfm.pos

train_size <- floor(0.9 * nrow(df))
train_ind <- sample(seq_len(nrow(df)), size = train_size)
train_df <- df[train_ind, ]
test_df <- df[-train_ind, ]

# Create the matrix `g` from binary indicator columns
# Replace the column names below with the actual names of your binary columns
binary_columns <- c("Productive", "Social_Face", "Social_Virtual", 
                    "Screen_Time_Leisure", "Exercise", "Rest_Sleep", "Baby_Care")

# observations
g <- as.matrix(train_df[binary_columns])
y <- train_df$value_bc
test_g <- as.matrix(test_df[binary_columns])
test_y <- train_df$value_bc
# Number of activities (columns in g)
J <- ncol(g)
# Number of observations (rows in g)
N <- nrow(g)

pid <- factor(train_df$record_id)
P <- length(unique(pid))

# Prepare the data list for Stan
stan_data <- list(
  J = J,      # Number of clusters (activities)
  N = N,      # Number of observations
  y = y,      # Response variable
  g = g,      # Transpose g to match Stan's `J x N` format
  P = P,      # number of participants
  pid = pid   # index of participant
)

# Load and compile the Stan model
model <- cmdstan_model("model.stan")
n_chains <- 4

# Fit the model using sampling
fit <- model$sample(
  data = stan_data,        
  chains = n_chains,                 # Number of chains
  parallel_chains = n_chains,        # Run chains in parallel
  iter_sampling = 4000,       # Number of iterations
  iter_warmup = 1000,         # Number of warmup iterations
  thin = 5                    # thinning
)

# Print a summary of the model results
summary <- fit$summary(variables = c("a", "b", "sigma"))

# fit$cmdstan_diagnose()       # Diagnose any issues
fit$draws(variables = c("sigma")) %>% bayesplot::mcmc_trace()  # Trace plot

posterior_predictive <- fit$draws(variables = "ytilde")

# Convert posterior predictive samples to a matrix
y_tilde_matrix <- posterior::as_draws_matrix(posterior_predictive)

# Perform posterior predictive density overlay
bayesplot::ppc_dens_overlay(y[1:125], y_tilde_matrix[,1:125])

# pp_check(fit)

