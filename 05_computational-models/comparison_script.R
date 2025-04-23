library(tidyverse)
library(brms)
library(tidybayes)
library(bayesplot)
library(patchwork)
library(modelr)

# Load the model if it hasn't been loaded yet
# If already loaded in your environment, you can skip this
if(!exists("foul_binomial_model_vs_mo_sim_big")) {
  foul_binomial_model_vs_mo_sim_big <- readRDS("foul_binomial_model_vs_mo_sim_big_2.rds")
}

# Load the simulated data
if(!exists("big_sim_unsummarized")) {
  source(here::here("00_R", "simulate_shots_3.R"))
  big_sim_unsummarized <- simulate_shots(
    n_observations = 2000, 
    n_defenders = 50, 
    n_teams = 5, 
    foul_count_transform = "ordered", 
    summarize_data = "FALSE"
  )
}

# 1. First, extract the posterior draws using tidybayes
posterior_draws <- foul_binomial_model_vs_mo_sim_big %>%
  gather_draws(b_Intercept, bsp_modefender_foul_count) %>%
  median_qi(.width = c(0.50, 0.80, 0.95))

# 2. Extract random effects for defender_id
defender_effects <- foul_binomial_model_vs_mo_sim_big %>%
  spread_draws(r_defender_id[defender_id,term]) %>%
  filter(term == "Intercept") %>%
  median_qi() %>%
  select(defender_id, r_defender_id, .lower, .upper)

# 3. Extract the monotonic effect coefficients
monotonic_effects <- foul_binomial_model_vs_mo_sim_big %>%
  gather_draws(simo_modefender_foul_count1[i]) %>%
  median_qi() %>%
  mutate(foul_level = i) %>%
  arrange(foul_level)

# 4. Get true values from simulated data
# First, summarize the true relationship between foul count and shot probability
true_foul_effect <- big_sim_unsummarized %>%
  mutate(foul_count_numeric = as.numeric(as.character(defender_foul_count))) %>%
  group_by(foul_count_numeric) %>%
  summarize(
    shots_taken = n(),
    shots_made = sum(shot_made),
    true_prob = mean(shot_made),
    true_logit = log(true_prob / (1 - true_prob))
  )

# 5. Compare estimated vs true defender effects
true_defender_effects <- big_sim_unsummarized %>%
  group_by(defender_id) %>%
  summarize(
    true_defender_effect = first(player_defensive_effect),
    true_defensive_skill = first(defensive_skill),
    shots_taken = n(),
    shots_made = sum(shot_made),
    true_prob = mean(shot_made)
  )

# Join estimated defender effects with true values
defender_comparison <- defender_effects %>%
  mutate(defender_id = as.numeric(as.character(defender_id))) %>%
  left_join(true_defender_effects, by = "defender_id") %>%
  # Calculate correlation
  mutate(within_ci = true_defender_effect >= .lower & true_defender_effect <= .upper)

# 6. Create comparison plots

# Plot 1: Foul count effect
# Create a dataset with predicted probabilities for each foul count level
newdata <- data.frame(
  defender_foul_count = factor(0:5, ordered = TRUE),
  # Use average values for other variables or set them to reference levels
  shot_movement = "stationary",
  shot_distance_category = "10_through_23_ft",
  position = "SG",
  defender_id = "1",  # Reference defender
  defensive_team_id = "1",  # Reference team
  offensive_team_id = "2",
  shots_taken = 100  # Dummy value
)

# Get posterior predictions
pred_draws <- foul_binomial_model_vs_mo_sim_big %>%
  add_predicted_draws(newdata = newdata, re_formula = NULL) %>%
  mutate(
    foul_count = as.numeric(as.character(defender_foul_count)),
    pred_prob = .prediction / 100  # Convert to probability
  )

# Calculate mean predicted probability for each foul count
pred_summary <- pred_draws %>%
  group_by(foul_count) %>%
  summarize(
    mean_pred_prob = mean(pred_prob),
    lower_95 = quantile(pred_prob, 0.025),
    upper_95 = quantile(pred_prob, 0.975)
  )

# Plot of foul count effect
p1 <- ggplot() +
  # Add true values from simulation
  geom_point(data = true_foul_effect, aes(x = foul_count_numeric, y = true_prob),
             size = 3, color = "darkred") +
  # Add model predictions with confidence intervals
  geom_line(data = pred_summary, aes(x = foul_count, y = mean_pred_prob),
            size = 1, color = "steelblue") +
  geom_ribbon(data = pred_summary, 
              aes(x = foul_count, ymin = lower_95, ymax = upper_95),
              alpha = 0.2, fill = "steelblue") +
  # Add labels for true values
  geom_text(data = true_foul_effect, 
            aes(x = foul_count_numeric, y = true_prob, 
                label = sprintf("n=%d", shots_taken)),
            vjust = -0.5, hjust = 0.5, size = 3) +
  labs(title = "Foul Count Effect on Shot Success Rate",
       subtitle = "Model Estimates (blue) vs. True Values (red)",
       x = "Number of Fouls",
       y = "Probability of Shot Success") +
  theme_minimal()

# Plot 2: Defender effects
p2 <- ggplot(defender_comparison, aes(x = r_defender_id, y = true_defender_effect)) +
  geom_point(aes(size = shots_taken, color = within_ci)) +
  geom_errorbar(aes(ymin = .lower, ymax = .upper), alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("FALSE" = "red", "TRUE" = "blue")) +
  labs(title = "Defender Random Effects vs. True Defender Effects",
       subtitle = paste0("Correlation: ", round(cor(defender_comparison$r_defender_id, 
                                                  defender_comparison$true_defender_effect), 3)),
       x = "Estimated Defender Effect",
       y = "True Defender Effect",
       color = "Within 95% CI",
       size = "Sample Size") +
  theme_minimal()

# Plot 3: MCMC trace and density plots for key parameters
p3 <- mcmc_trace(foul_binomial_model_vs_mo_sim_big, pars = c("b_Intercept", "bsp_modefender_foul_count")) +
  labs(title = "MCMC Trace for Main Parameters")

p4 <- mcmc_areas(foul_binomial_model_vs_mo_sim_big, pars = c("b_Intercept", "bsp_modefender_foul_count")) +
  labs(title = "Posterior Distributions for Main Parameters")

# Plot 5: Monotonic effect visualization
p5 <- ggplot(monotonic_effects, aes(x = foul_level, y = simo_modefender_foul_count1)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = .lower, ymax = .upper), width = 0.2) +
  geom_line() +
  labs(title = "Monotonic Effect of Foul Count",
       subtitle = "How the effect increases with foul level",
       x = "Foul Level",
       y = "Cumulative Effect") +
  theme_minimal()

# Combine plots in a grid
(p1 | p2) / (p4 | p5)

# Print summary statistics
cat("\nModel Parameters vs. True Values Summary:\n")
cat("---------------------------------------\n")
cat("Correlation between estimated and true defender effects:", 
    round(cor(defender_comparison$r_defender_id, defender_comparison$true_defender_effect), 3), "\n")

# Calculate percentage of true values within confidence intervals
coverage_rate <- mean(defender_comparison$within_ci) * 100
cat("Percentage of true defender effects within 95% CI:", round(coverage_rate, 1), "%\n")

# Compare model's monotonic effect to the true effect structure
cat("\nTrue Foul Count Effect Structure:\n")
print(true_foul_effect)

cat("\nModel's Monotonic Effect Structure:\n")
print(monotonic_effects)

# Create a posterior predictive check plot
ppc_plot <- pp_check(foul_binomial_model_vs_mo_sim_big, ndraws = 50) +
  labs(title = "Posterior Predictive Check",
       subtitle = "How well the model predicts the observed data")

print(ppc_plot) 