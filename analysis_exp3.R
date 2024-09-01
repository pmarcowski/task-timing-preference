# Title: Data analysis for the temporal task scheduling project (Experiment 3)
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

# Read prepared data from Experiment 3
exp3_task_data <- readRDS("./data/prepared/exp3.Rds")

# Inspect sample characteristics
exp3_task_data %>%
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

# Inspect empirical choice proportions
props3 <-
  exp3_task_data %>%
  group_by(difficulty, attractiveness, log_t, chosen) %>%
  count(chosen) %>%
  ungroup() %>%
  group_by(difficulty, attractiveness, log_t) %>%
  mutate(
    prop = n / sum(n),
    CI_low = lapply(n, prop.test, n = sum(n)),
    CI_high = sapply(CI_low, function(x) x$conf.int[2]),
    CI_low = sapply(CI_low, function(x) x$conf.int[1])
  ) %>%
  filter(chosen > 0)

# Fit Bernoulli model with logit link
exp3_model_formula <- bf(paste0("chosen ~ difficulty*attractiveness*log_t + (1|id)"))
exp3_model_family <- brmsfamily("bernoulli", "logit")
exp3_model_priors <- get_prior(exp3_model_formula, data = exp3_task_data, family = exp3_model_family)
exp3_model_fit <-
  brm(
    exp3_model_formula,
    data = exp3_task_data, family = exp3_model_family, prior = exp3_model_priors,
    iter = 4000, warmup = 2000, chains = 4, cores = 4,
    file = "./output/exp3_bernoulli_model_fit"
  )

# Inspect model
summary(exp3_model_fit)
exp3_posterior_desc <- bayestestR::describe_posterior(exp3_model_fit)
write_csv(exp3_posterior_desc, "./output/exp3_model_coefficients.csv")

# Calculate marginal means and contrasts
exp3_marginal_means_time <- estimate_means(exp3_model_fit, at = c("difficulty", "attractiveness", "log_t"), length = 100, transform = "response", ci = 0.99)
exp3_emmeans <- emmeans::emmeans(exp3_model_fit, pairwise ~ attractiveness | difficulty, regrid = "response", infer = TRUE, level = 0.99, adjust = "mvt")
exp3_marginal_means <- as.data.frame(exp3_emmeans$emmeans)
write_csv(exp3_marginal_means, "./output/exp3_marginal_means.csv")

exp3_contrasts <- as.data.frame(exp3_emmeans$contrasts)
exp3_contrasts$sig <- with(exp3_contrasts, ifelse(lower.HPD <= 0 & upper.HPD >= 0, "", "*"))
write_csv(exp3_contrasts, "./output/exp3_pairwise_contrasts.csv")

# Visualize means across task delay
exp3_means_time_plot <-
  exp3_marginal_means_time %>%
  ggplot(aes(x = log_t, y = Probability, group = difficulty, color = difficulty, fill = difficulty)) +
  geom_line() +
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high), color = "transparent", alpha = 0.5) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_vline(xintercept = max(exp3_task_data$log_t) / 2, linetype = "dashed") +
  labs(
    title = "Task Acceptance by Delay",
    x = "log(t)", y = "P(Choose)",
    color = "Task Difficulty",
    fill = "Task Difficulty"
  ) +
  coord_cartesian(
    xlim = range(exp3_task_data$log_t),
    ylim = c(0, 1)
  ) +
  scale_color_grey() +
  scale_fill_grey() +
  see::theme_modern() +
  facet_wrap(~attractiveness) +
  theme(
    legend.position = "bottom",
    legend.justification = c(0.5, 0.5),
    plot.title = element_text(hjust = 0.5),
    strip.background = element_rect(color = "transparent", fill = "transparent")
  )

# Visualize means across conditions
exp3_means_plot <-
  exp3_marginal_means %>%
  ggplot(aes(x = difficulty, y = response, group = attractiveness, color = attractiveness)) +
  geom_line() +
  geom_errorbar(aes(ymin = lower.HPD, ymax = upper.HPD), width = 0.05) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  labs(
    title = "Marginal Task Acceptance",
    x = "Task Difficulty", y = "P(Choose)",
    color = "Task Appeal"
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_color_grey() +
  see::theme_modern() +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.background = element_rect(color = "transparent", fill = "transparent")
  )

# Visualize attractiveness contrasts
exp3_contrasts_plot <-
  exp3_contrasts %>%
  ggplot(aes(x = difficulty, y = estimate, fill = difficulty)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_col(color = "black", width = 0.25) +
  geom_errorbar(aes(ymin = lower.HPD, ymax = upper.HPD), width = 0.1) +
  labs(
    title = "Differences in Task Acceptance",
    x = "Task Difficulty", y = "P(Choose) Boring - Interesting",
    fill = "Task Difficulty"
  ) +
  guides(fill = "none") +
  coord_flip(ylim = c(-1, 1)) +
  scale_fill_manual(values = c("darkgrey", "white")) +
  see::theme_modern() +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.background = element_rect(color = "transparent", fill = "transparent")
  )

# Calculate and visualize trends for task delay across conditions
exp3_slopes <- standardize(estimate_slopes(exp3_model_fit, trend = "log_t", at = c("difficulty", "attractiveness"), ci = 0.99))
write_csv(exp3_slopes, "./output/exp3_slope_estimates.csv")

exp3_slopes_plot <-
  exp3_slopes %>%
  ggplot(aes(x = difficulty, y = Coefficient, color = attractiveness)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), position = position_dodge(width = 0.5), width = 0.2) +
  coord_flip(ylim = c(-0.5, 0.5)) +
  labs(
    title = "Effects of Task Delay on Preference",
    x = "Task Difficulty", y = bquote(beta),
    color = "Task Appeal"
  ) +
  scale_color_grey() +
  scale_fill_grey() +
  see::theme_modern() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# Create and save figure 3
fig3 <-
  exp3_means_time_plot + exp3_slopes_plot + exp3_means_plot + exp3_contrasts_plot +
  plot_annotation(tag_levels = "a") &
  theme(plot.title = element_text(size = 12)) &
  theme(legend.position = "bottom")

print(fig3)
ggsave("./output/fig3.png", fig3, width = 10, height = 8)
