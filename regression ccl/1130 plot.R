#1130 plot
library(rstan)
library(bayesplot)
library(ggplot2)
library(tidyverse)

math_fit <- readRDS(file = "./data/1128model_math_Weighted_fit.rds")
read_fit <- readRDS(file = "./data/1128model_read_Weighted_fit.rds")
science_fit <- readRDS(file = "./data/1128model_Science_Weighted_fit.rds")

# Extract posterior samples from all models
posterior_math <- rstan::extract(math_fit)
posterior_read <- rstan::extract(read_fit)
posterior_science <- rstan::extract(science_fit)

# remove
rm(math_fit, read_fit, science_fit)

# Create data frames to store the parameters for each model
posterior_df_math <- as.data.frame(posterior_math)
posterior_df_read <- as.data.frame(posterior_read)
posterior_df_science <- as.data.frame(posterior_science)

# Add a column to indicate the model source
posterior_df_math$model <- "Math"
posterior_df_read$model <- "Reading"
posterior_df_science$model <- "Science"

# Combine the data frames into one long-format data frame
posterior_all <- bind_rows(posterior_df_math, posterior_df_read, posterior_df_science)

#colnames(head(posterior_all), 10)

# Get the current column names
current_colnames <- colnames(posterior_all)

# Change the first five column names
current_colnames[1:5] <- c("gender", "computer", "internet", "family_class", "intercept")

# Apply the new column names
colnames(posterior_all) <- current_colnames

# Plot posterior distributions for the parameter 'beta.1' from all models
ggplot(posterior_all, aes(x = gender, fill = model)) +
  geom_density(alpha = 0.5) +   # Density plot with transparency
  facet_wrap(~ model) +         # Facet by model (Math, Reading, Science)
  theme_minimal() +             # Clean theme
  labs(title = "Posterior Distributions of Gender",
       x = "Gender",
       y = "Density") + 
  theme(legend.position = "none")

# Example for plotting 'beta.2'
ggplot(posterior_all, aes(x = computer, fill = model)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ model) +
  theme_minimal() +
  labs(title = "Posterior Distributions of Computer",
       x = "Computer",
       y = "Density") + 
  theme(legend.position = "none")


# Reshape the data for multiple parameters 
posterior_long <- posterior_all %>%
  select(gender, computer, internet, family_class, intercept, sigma2, model) %>%
  pivot_longer(cols = c(intercept, gender, computer, internet, family_class, sigma2),
               names_to = "parameter",
               values_to = "value")

# Plot the posterior distributions for all parameters
ggplot(posterior_long, aes(x = value, fill = model)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ parameter + model, scales = "free") +
  theme_minimal() +
  labs(title = "Posterior Distributions of Parameters",
       x = "Value",
       y = "Density") + 
  theme(legend.position = "none")



# Reshape the data for multiple parameters (e.g., alpha, beta_gender)
posterior_long <- posterior_all %>%
  select(intercept,gender, computer, internet, family_class, sigma2, model) %>%
  pivot_longer(cols = c(intercept,gender, computer, internet, family_class, sigma2),
               names_to = "parameter",
               values_to = "value")

# Plot the posterior distributions for all parameters, grouped by model
ggplot(posterior_long, aes(x = value, fill = model, color = model)) +
  geom_density(alpha = 0.5) +       # Semi-transparent density plots
  facet_wrap(~ parameter, scales = "free") +  # Facet by parameter
  theme_minimal() +
  labs(title = "Posterior Distributions of Parameters",
       x = "Value",
       y = "Density") + 
  theme(legend.position = "none")  # Optionally hide the legend


## plot
posterior_long2 <- posterior_all %>%
  # Create a new variable 'sigma' as the square root of 'sigma2'
  mutate(sigma = sqrt(sigma2)) %>%
  select(intercept, gender, computer, internet, family_class, sigma, model) %>%
  pivot_longer(cols = c(intercept, gender, computer, internet, family_class, sigma),
               names_to = "parameter",
               values_to = "value") %>%
  mutate(parameter = recode(parameter,
                            "intercept" = "Intercept",
                            "gender" = "Gender",
                            "computer" = "Computer Access",
                            "internet" = "Internet Access",
                            "family_class" = "Family Class",
                            "sigma" = "Sigma"),  # Rename sigma to Sigma
         parameter = factor(parameter, levels = c("Intercept", "Gender", "Family Class", "Computer Access", "Internet Access", "Sigma")))


ggplot(posterior_long2, aes(x = value, fill = model, color = model)) +
  geom_density(alpha = 0.5) +       # Semi-transparent density plots
  facet_wrap(~ parameter, scales = "free") +  # Facet by parameter
  theme_minimal() +
  labs(#title = "Posterior Distributions of Parameters in Bayesian Linear Model",
       x = "Value",
       y = "Density") +
  scale_fill_discrete(name = "Subject") +  # Rename fill legend to "Subject"
  scale_color_discrete(name = "Subject") +  # Rename color legend to "Subject"
  theme() 



# Calculate credible intervals (2.5% and 97.5% percentiles) for each parameter and model
credible_intervals_by_model <- posterior_long2 %>%
  group_by(model, parameter) %>%
  summarise(
    lower = quantile(value, 0.025),   # 2.5% quantile (lower bound)
    upper = quantile(value, 0.975),   # 97.5% quantile (upper bound)
    .groups = "drop"                  # Avoid retaining the grouping structure
  )

# View the credible intervals table by model
print(credible_intervals_by_model)

gc()
