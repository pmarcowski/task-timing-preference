# Title: Data analysis for the temporal task scheduling project (Experiment 2)
# Author: Przemyslaw Marcowski, PhD
# Email: p.marcowski@gmail.com
# Date: 2023-01-04
# Copyright (c) 2023 Przemyslaw Marcowski

# This code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Load packages
library(tidyverse)
library(brms)
library(easystats)
library(patchwork)

set.seed(42) # for reproducibility

# Read prepared data from Experiment 2
exp2_delay_data <- readRDS("./data/prepared/exp2.Rds")

# Inspect sample characteristics
exp2_delay_data %>%
  group_by(id) %>%
  slice(1) %>%
  ungroup() %>%
  summarize(
    n = n_distinct(id),
    mean_age = mean(age),
    sd_age = sd(age),
    n_male = n_distinct(id[gender == "Male"]),
    n_female = n_distinct(id[gender == "Female"])
  )

# Count follow-up task preferences
exp2_delay_data %>%
  group_by(id) %>%
  slice(1) %>%
  ungroup() %>%
  select(id, which_now, which_later) %>%
  gather(task, pref, which_now, which_later) %>%
  group_by(task, pref) %>%
  count() %>%
  ungroup() %>%
  spread(pref, n)

# Fit Gaussian model
exp2_model_formula <- bf(paste0("log_t ~ difficulty * attractiveness + (1|id)"))
exp2_model_family <- brmsfamily("gaussian")
exp2_model_priors <- get_prior(exp2_model_formula, data = exp2_delay_data, family = exp2_model_family)
exp2_model_fit <-
  brm(
    exp2_model_formula,
    data = exp2_delay_data, family = exp2_model_family, prior = exp2_model_priors,
    iter = 4000, warmup = 2000, chains = 4, cores = 4,
    file = paste0("./output/exp2_gaussian_model_fit")
  )

# Inspect model
summary(exp2_model_fit)
exp2_posterior_desc <- describe_posterior(exp2_model_fit)
write_csv(exp2_posterior_desc, "./output/exp2_model_coefficients.csv")

# Calculate posterior means
exp2_marginal_means <- estimate_means(exp2_model_fit, at = c("difficulty", "attractiveness"), transform = "response", ci = 0.99)
write_csv(exp2_marginal_means, "./output/exp2_marginal_means.csv")

# Visualize means across difficulty and attrativeness
exp2_means_plot <-
  exp2_marginal_means %>%
  mutate(
    log_t = Mean
  ) %>%
  ggplot(aes(x = difficulty, y = log_t, group = attractiveness, color = attractiveness)) +
  geom_point(shape = 1, stroke = 1) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.1) +
  geom_line() +
  labs(
    title = "Preferences in Task Delay",
    x = "Task Difficulty", y = "log(t)",
    color = "Task Appeal"
  ) +
  coord_cartesian(ylim = range(exp2_delay_data$log_t)) +
  scale_color_grey() +
  scale_fill_grey() +
  see::theme_modern() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.margin = unit(c(0, 3, 0, 0), "lines")
  )

# Calculate and visualize attractiveness contrasts
exp2_contrasts_attr <- estimate_contrasts(exp2_model_fit, contrast = "attractiveness", at = "difficulty", ci = 0.99, p_adjust = "mvt")
write_csv(exp2_contrasts_attr, "./output/exp2_attractiveness_contrasts.csv")

exp2_contrasts_attr_plot <-
  exp2_contrasts_attr %>%
  ggplot(aes(x = difficulty, y = Difference, fill = difficulty)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_col(color = "black", width = 0.25) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.1) +
  labs(
    title = "Differences in Task Delay\nPreference by Difficulty",
    x = "Task Difficulty", y = "log(t) Boring - Interesting",
    fill = "Task Difficulty"
  ) +
  guides(fill = "none") +
  coord_flip(ylim = c(-2, 2)) +
  scale_fill_manual(values = c("darkgrey", "white")) +
  see::theme_modern() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# Calculate and visualize difficulty contrasts
exp2_contrasts_diff <- estimate_contrasts(exp2_model_fit, contrast = "difficulty", at = "attractiveness", ci = 0.99, p_adjust = "mvt")
write_csv(exp2_contrasts_diff, "./output/exp2_difficulty_contrasts.csv")

exp2_contrasts_diff_plot <-
  exp2_contrasts_diff %>%
  ggplot(aes(x = attractiveness, y = Difference, fill = attractiveness)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_col(color = "black", width = 0.25) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.1) +
  labs(
    title = "Differences in Task Delay\nPreference by Attractiveness",
    x = "Task Attractiveness", y = "log(t) Easy - Hard",
    fill = "Attractiveness"
  ) +
  guides(fill = "none") +
  coord_flip(ylim = c(-2, 2)) +
  scale_fill_manual(values = c("darkgrey", "white")) +
  see::theme_modern() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# Create and save figure 2
fig2 <-
  exp2_means_plot + exp2_contrasts_attr_plot + exp2_contrasts_diff_plot +
  plot_annotation(tag_levels = "a") &
  theme(plot.title = element_text(size = 12)) &
  theme(legend.position = "bottom")

print(fig2)
ggsave("./output/fig2.png", fig2, width = 10, height = 4)
