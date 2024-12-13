library(tidyverse)
library(rstan)
library(bayesplot)


rm(list = ls())
df_raw <- readRDS(file = "./data/cleaned/cleaned_data.Rds")
df_clean <- df_raw

str(df_clean)

# Create new variables based on existing ones
df_clean <- df_clean %>%
  mutate(
    # Assign a numeric ID for each country
    country_id = as.numeric(factor(country)),  # Converts country to a numeric ID
    school_id = as.numeric(factor(school_id)),  # Re-index school_id
    
    # Gender mapping (female = 0, male = 1)
    gender = ifelse(gender == "female", 0, 1),
    
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
lm_model <- lm(
  read ~ gender + computer + internet + family_class,
  data = df_clean,
  weights = stu_wgt # Including weights
)

# Summary of the model
summary(lm_model)


# Bayesian Model
# Prepare data for Stan model
stan_data <- list(
  N = nrow(df_clean),       # number of students
  J = length(unique(df_clean$school_id)),  # number of schools
  K = length(unique(df_clean$country_id)), # number of countries
  read = df_clean$read,      # dependent variable
  gender = df_clean$gender,        # gender (0 for female, 1 for male)
  computer = df_clean$computer,    # computer (0 for no, 1 for yes)
  internet = df_clean$internet,    # internet (0 for no, 1 for yes)
  family_class = df_clean$family_class,  # family_class as numeric
  school_id = as.numeric(df_clean$school_id), # school IDs
  country_id = df_clean$country_id,  # country IDs
  stu_wgt = df_clean$stu_wgt       # student weights
)

str(stan_data)


# Updated Stan model code without school and country effects
stan_code <- "
data {
  int<lower=0> N;          // number of students
  real read[N];         // read scores
  int gender[N];           // gender (0 for female, 1 for male)
  int computer[N];         // computer access (0 for no, 1 for yes)
  int internet[N];         // internet access (0 for no, 1 for yes)
  int family_class[N];     // family class
  real<lower=0> stu_wgt[N]; // student weights
}

parameters {
  real alpha;                    // global intercept
  real beta_gender;              // effect of gender
  real beta_computer;            // effect of computer
  real beta_internet;            // effect of internet
  real beta_family_class;        // effect of family class
  real<lower=0> sigma;           // residual standard deviation
}

model {
  // Priors
  alpha ~ normal(0, 10);
  beta_gender ~ normal(0, 10);
  beta_computer ~ normal(0, 10);
  beta_internet ~ normal(0, 10);
  beta_family_class ~ normal(0, 10);
  sigma ~ normal(0, 1);

  // Weighted likelihood
  for (n in 1:N) {
    target += stu_wgt[n] * normal_lpdf(
      read[n] | 
      alpha + 
      beta_gender * gender[n] + 
      beta_computer * computer[n] + 
      beta_internet * internet[n] + 
      beta_family_class * family_class[n],
      sigma
    );
  }
}
"

# Compile the Stan model
stan_model <- stan_model(model_code = stan_code)

# Fit the model using the sampling function
fit <- sampling(stan_model, data = stan_data, iter = 2000, chains = 4)

# Print the summary of the fit
print(fit)

# Save the fitted model to a file
saveRDS(fit, file = "./data/1128model_read_Weighted_fit.rds")

# Load the saved model
fit <- readRDS(file = "./data/1128model_read_Weighted_fit.rds")


# Extract posterior samples
posterior <- extract(fit)

# Check for divergences
divergent <- get_divergent_iterations(fit)
print(paste("Number of divergent transitions:", sum(divergent)))

# Check for maximum treedepth exceeded
max_treedepth <- get_max_treedepth_iterations(fit)
print(paste("Number of iterations that exceeded max treedepth:", sum(max_treedepth)))

# Check Rhat values
rhat <- summary(fit)$summary[, "Rhat"]
print(paste("Maximum Rhat:", max(rhat)))

# Check effective sample sizes
n_eff <- summary(fit)$summary[, "n_eff"]
print(paste("Minimum n_eff:", min(n_eff)))

# Create trace plots
mcmc_trace(fit, pars = c("alpha", "beta_gender", "beta_computer", "beta_internet", "beta_family_class", "sigma"))

# Plot posterior distributions
mcmc_areas(fit, pars = c("beta_gender", "beta_computer", "beta_internet", "beta_family_class"))

# Number of observations (students)
N <- stan_data$N

# Number of posterior samples (from all chains)
S <- length(posterior$alpha)  # Each element in posterior$alpha corresponds to one chain

# Initialize a matrix for posterior predictions
y_rep <- matrix(NA, nrow = S, ncol = N)  

# Loop through each posterior sample (combine across chains)
for (i in 1:S) {
  y_rep[i, ] <- posterior$alpha[i] + 
    posterior$beta_gender[i] * stan_data$gender + 
    posterior$beta_computer[i] * stan_data$computer + 
    posterior$beta_internet[i] * stan_data$internet + 
    posterior$beta_family_class[i] * stan_data$family_class
}

# Check posterior predictive fit using density overlay plot
ppc_dens_overlay(stan_data$read, y_rep[1:50, ])

# Or check using test statistics (mean, standard deviation, etc.)
ppc_stat(stan_data$read, y_rep, stat = "mean")
ppc_stat(stan_data$read, y_rep, stat = "sd")

# Calculate residuals
y_mean <- apply(y_rep, 2, mean)  # Mean prediction for each student
residuals <- stan_data$read - y_mean  # Residuals (observed - predicted)

# Plot residuals
plot(y_mean, residuals, xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Q-Q plot of residuals
qqnorm(residuals)
qqline(residuals)


