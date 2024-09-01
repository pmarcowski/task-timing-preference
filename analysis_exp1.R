# Title: Data analysis for the temporal task scheduling project (Experiment 1)
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

# Read prepared data from Experiment 1
exp1_freq_data <- readRDS("./data/prepared/exp1.Rds")

# Descriptive analysis
n_distinct(exp1_freq_data$id)
mean(exp1_freq_data$age)
sd(exp1_freq_data$age)

exp1_freq_data %>%
  group_by(sex) %>%
  summarize(
    n = n_distinct(id),
    mean_age = mean(age),
    sd_age = sd(age)
  )

# Calculate preference proportions
exp1_prop_table <-
  exp1_freq_data %>%
  count(time, choice) %>%
  group_by(time) %>%
  mutate(
    total = sum(n),
    prop = n / total
  ) %>%
  arrange(time)

# Fit a binomial model with identity link
exp1_model_formula <- bf(paste0("n | trials(total) ~ choice * time"))
exp1_model_family <- brmsfamily("binomial", "logit")
exp1_model_priors <- get_prior(exp1_model_formula, data = exp1_prop_table, family = exp1_model_family)
exp1_model_fit <-
  brm(
    exp1_model_formula,
    data = exp1_prop_table, family = exp1_model_family, prior = exp1_model_priors,
    iter = 4000, warmup = 2000, chains = 4, cores = 4,
    file = "./output/exp1_binomial_model_fit"
  )

# Inspect model
summary(exp1_model_fit)
exp1_posterior_desc <- bayestestR::describe_posterior(exp1_model_fit)
write_csv(exp1_posterior_desc, "./output/exp1_model_coefficients.csv")

# Calculate marginal means and contrasts
exp1_emmeans <- emmeans::emmeans(exp1_model_fit, pairwise ~ choice | time, regrid = "response", infer = TRUE, level = 0.99, adjust = "mvt")
exp1_marginal_means <- as.data.frame(exp1_emmeans$emmeans)
write_csv(exp1_marginal_means, "./output/exp1_marginal_means.csv")

exp1_contrasts <- as.data.frame(exp1_emmeans$contrasts)
exp1_contrasts$sig <- with(exp1_contrasts, ifelse(lower.HPD <= 0 & upper.HPD >= 0, "", "*"))
write_csv(exp1_contrasts, "./output/exp1_pairwise_contrasts.csv")

# Visualize means across conditions
exp1_means_plot <-
  exp1_marginal_means %>%
  ggplot(aes(x = time, y = prob, group = choice, color = choice)) +
  geom_line() +
  geom_errorbar(aes(ymin = lower.HPD, ymax = upper.HPD), width = 0.1) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  labs(
    title = "Marginal Task Preference",
    x = "Task Delay", y = "P(Choose)",
    color = "Task Type"
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_color_grey(labels = c("Easy" = "Easy and Boring", "Hard" = "Hard and Interesting")) +
  see::theme_modern() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.margin = unit(c(0, 3, 0, 0), "lines"),
    strip.background = element_rect(color = "transparent", fill = "transparent")
  )

# Visualize contrasts
exp1_contrasts_plot <-
  exp1_contrasts %>%
  ggplot(aes(x = time, y = estimate, fill = time)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_col(color = "black", width = 0.25) +
  geom_errorbar(aes(ymin = lower.HPD, ymax = upper.HPD), width = 0.1) +
  labs(
    title = "Differences in Task Preference",
    x = "Task Delay", y = "P(Choose) Easy and Boring - Hard and Interesting",
    fill = "Task Delay"
  ) +
  guides(fill = "none") +
  coord_flip(ylim = c(-1, 1)) +
  scale_fill_manual(values = c("darkgrey", "white")) +
  see::theme_modern() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.margin = unit(c(0, 2, 0, 0), "lines"),
    strip.background = element_rect(color = "transparent", fill = "transparent")
  )

# Create and save figure 1
fig1 <-
  exp1_means_plot + exp1_contrasts_plot +
  plot_annotation(tag_levels = "a") &
  theme(plot.title = element_text(size = 12)) &
  theme(legend.position = "bottom")

print(fig1)
ggsave("./output/fig1.png", fig1, width = 10, height = 5)
