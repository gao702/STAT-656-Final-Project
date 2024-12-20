rm(list = ls())
data <- readRDS(file = "./data/cleaned/cleaned_data.Rds")
library(rstan)
library(bayesplot)
library(ggplot2)
library(coda)
library(tidyverse)

# Step 1: Identify 50 schools per country and public/private type
set.seed(123)
selected_schools <- data %>%
  distinct(country, public_private, school_id) %>% # Get unique schools
  group_by(country, public_private) %>%
  slice_sample(n = 50) %>% # Sample 50 schools per group
  ungroup()

# Step 2: Filter the original data to include all students from the selected schools
data <- data %>%
  semi_join(selected_schools, by = c("country", "public_private", "school_id"))

# Prepare the dataset
data_math <- data %>%
  select(country, school_id, student_id, gender, computer, internet, family_class, math) %>%
  mutate(
    gender = factor(gender, levels = c("female", "male")),
    computer = factor(computer, levels = c("no", "yes")),
    internet = factor(internet, levels = c("no", "yes")),
    family_class = factor(family_class, levels = c("working class", "middle class")),
    country = factor(country, levels = c("AUS","CAN","GBR")),
    school_id = factor(as.integer(factor(school_id)))
  )
saveRDS(data_math,file = "./data/cleaned/cleaned_data_math.Rds")

schools <- unique(data_math[, c("school_id", "country")])  # Unique school-country pairs

# Convert country to numeric IDs
schools$country_id <- as.numeric(schools$country)

math_code <- "
data {
  int<lower=1> N;                // Number of students
  int<lower=1> J;                // Number of schools
  int<lower=1> K;                // Number of countries
  int<lower=1> school_id[N];     // School ID for each student
  int<lower=1> country_id[J];    // Country ID for each school
  vector[N] y;                   // Outcome variable (math scores)
  vector[N] gender;              // Predictor 1: Gender (encoded as numeric)
  vector[N] family_class;        // Predictor 2: Family socioeconomic class
  vector[N] computer;            // Predictor 2: Computer access
  vector[N] internet;            // Predictor 3: Internet access
}
parameters {
  real beta0;                    // Global intercept
  real beta_gender;              // Coefficient for gender
  real beta_family_class;        // Coefficient for family class
  real beta_computer;            // Coefficient for computer
  real beta_internet;            // Coefficient for internet
  vector[J] alpha_school;        // School-level effects
  vector[K] gamma_country;       // Country-level effects
  real<lower=0> sigma;           // Student-level variance
  real<lower=0> tau;             // School-level variance
  real<lower=0> kappa;           // Country-level variance
}
model {
  // Priors
  beta0 ~ normal(0, 10);
  beta_gender ~ normal(0, 5);
  beta_family_class ~ normal(0, 5);
  beta_computer ~ normal(0, 5);
  beta_internet ~ normal(0, 5);
  sigma ~ cauchy(0, 2.5);
  tau ~ cauchy(0, 2.5);
  kappa ~ cauchy(0, 2.5);
  
  for (k in 1:K) {
    gamma_country[k] ~ normal(0, kappa);
  }
  for (j in 1:J) {
    alpha_school[j] ~ normal(gamma_country[country_id[j]], tau);
  }
  // Likelihood
  for (i in 1:N) {
    y[i] ~ normal(
      beta0 +
      beta_gender * gender[i] +
      beta_family_class * family_class[i] +
      beta_computer * computer[i] +
      beta_internet * internet[i] +
      alpha_school[school_id[i]],
      sigma
    );
  }
}
generated quantities {
  vector[N] y_pre;  // Posterior predictive distribution

  for (i in 1:N) {
    y_pre[i] = normal_rng(
      beta0 +
      beta_gender * gender[i] +
      beta_family_class * family_class[i] +
      beta_computer * computer[i] +
      beta_internet * internet[i] +
      alpha_school[school_id[i]],
      sigma
    );
  }
}
"
math_model<- stan_model(model_code = math_code)

math_data <- list(
  N = nrow(data_math),
  J = length(unique(data_math$school_id)),
  K = length(unique(data_math$country)),
  school_id = as.numeric(factor(data_math$school_id)),
  country_id = schools$country_id,
  y = data_math$math,
  gender = as.numeric(data_math$gender == "female"), # Binary encoding
  family_class = as.numeric(data_math$family_class=="middle class"), # Assuming it's numeric
  computer = as.numeric(data_math$computer == "yes"), # Binary encoding
  internet = as.numeric(data_math$internet == "yes") # Binary encoding
)

fit <- sampling(math_model,
                data = math_data,
                iter = 10000,
                warmup = 2000,
                # init_r = 0.01,
                chains = 4,
                thin = 4
                
)

saveRDS(fit,file = "./result/math_fit.Rds")
