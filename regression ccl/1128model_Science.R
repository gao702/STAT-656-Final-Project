rm(list = ls())
library(tidyverse)
library(ggplot2)
library(rstan)
library(bayesplot)
bayesplot_theme_set(theme_default(base_size = 24, base_family = "sans"))

df_raw <- readRDS(file = "./data/cleaned/cleaned_data.Rds")
df_clean <- df_raw

str(df_clean)

# Create new variables based on existing ones
df_clean <- df_clean %>%
  mutate(
    # Assign a numeric ID for each country
    country_id = as.numeric(factor(country)),
    # Converts country to a numeric ID
    school_id = as.numeric(factor(school_id)),
    # Re-index school_id
    
    # Gender mapping (male = 0, female = 1)
    gender = ifelse(gender == "male", 0, 1),
    
    # Computer usage mapping (no = 0, yes = 1)
    computer = ifelse(computer == "no", 0, 1),
    
    # Internet usage mapping (no = 0, yes = 1)
    internet = ifelse(internet == "no", 0, 1),
    
    # Public school mapping (public = 1, private = 0)
    public = ifelse(public_private == "public", 1, 0),
    
    family_class = ifelse(family_class == "middle class", 1, 0)
  )

# Check the new structure
str(df_clean)


# Check the re-indexed ranges
range(df_clean$school_id)  # Should be from 1 to the number of unique schools
range(df_clean$country_id) # Should be from 1 to the number of unique countries


# Classical linear regression using lm()
lm_model <- lm(science ~ gender + computer + internet + family_class,
               data = df_clean,
               weights = stu_wgt)

# Summary of the model
summary(lm_model)

# Bayesian Model
# Prepare data for Stan model
X <- as.matrix(df_clean[, c("gender", "computer", "internet", "family_class")]) # Ensure numeric matrix
X <- cbind(X, Offset = 1)  # Add offset/intercept column
y <- df_clean$science  # Response variable
stu_wgt <- df_clean$stu_wgt  # Student weights

# Reuse training data
x_test <- X
stu_wgt_test <- stu_wgt

stan_data <- list(
  N = nrow(X),
  N_test = nrow(X),
  K = ncol(X),
  pr_sd = 100,
  x = X,
  y = y,
  x_test = x_test,
  stu_wgt = stu_wgt,
  stu_wgt_test = stu_wgt_test
)

# Verify structure of stan_data
str(stan_data)

# stan code
stan_code <- "
data {
  int<lower=0> N;              // number of data items
  int<lower=0> N_test;         // number of test data items
  int<lower=0> K;              // number of predictors
  real<lower=0> pr_sd;         // std dev of the prior
  matrix[N, K] x;              // predictor matrix
  real y[N];                   // output vector
  matrix[N_test, K] x_test;    // predictor matrix
  vector[N] stu_wgt;        // student weights
  vector[N_test] stu_wgt_test; // student weigts
}

parameters {
  vector[K] beta;              // coefficients for predictors
  real<lower=0> sigma2;        //error scale
}

transformed parameters {
  vector[N] mu = x * beta;
  real<lower=0> sigma = sqrt(sigma2);
}

model {
  beta ~ normal(0, pr_sd); // Note: beta is k-dim
  sigma2 ~ inv_gamma(2, 1); //Close to a flat prior
  y ~ normal(mu, sigma * sqrt(stu_wgt));
}

generated quantities {
  real y_rep[N_test];  // Generated predicted values for the test data
  y_rep = normal_rng(x_test * beta, sqrt(sigma2) * sqrt(stu_wgt_test));  // Predictions for test data
}
"

# Compile the Stan model
#stan_model <- stan_model(model_code = stan_code)

# Fit the model using the sampling function
#fit <- sampling(stan_model,data = stan_data,iter = 1000, chains = 2)

# Save the fitted model to a file
#saveRDS(fit, file = "./data/1128model_science_Weighted_fit.rds")

# Load the saved model
fit <- readRDS(file = "./data/1128model_science_Weighted_fit.rds")

post_smp <- as.data.frame(fit)
colnames(post_smp)[1:5] <- colnames(X)

mcmc_areas(post_smp[, 1:4], pars = colnames(X)[1:4], prob = 0.8)
mcmc_dens(post_smp[, 5:6], alpha = 0.2)

plot(post_smp$gender, type = 'l')

# Trace plots for all parameters
mcmc_trace(post_smp[1:4])

# Density plots for all parameters
mcmc_dens(post_smp[1:4])

# model check
y_rep = rstan::extract(fit)$y_rep
sg = post_smp$sigma

ppd_intervals(y = y_rep, x = y) + geom_abline(intercept = 0, slope = 1) +
  ggplot2::labs(y = "Predicted y's", x = "Observed y's")

ppd_intervals(t(t(y_rep) - y), x = y) + geom_abline(intercept = 0, slope = 0) +
  ggplot2::labs(y = "Errors in predicted y's", x = "Observed y's")



