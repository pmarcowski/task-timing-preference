# Author: Przemyslaw Marcowski, PhD
# Email: p.marcowski@gmail.com
# Date: 2023-01-04
# Copyright (c) 2023 Przemyslaw Marcowski

# This code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# This script performs a priori power analyses for Experiments 1, 2, and 3.
# It examines the effects of task timing, difficulty, and appeal on task choices.
# It calculates required sample sizes for different effect sizes and power levels
# using simplified designs for each experiment:
# - Experiment 1: 2x2 chi-square test of independence
# - Experiment 2: ANOVA (2x2 design)
# - Experiment 3: Logistic regression (odds ratio)

# Load packages
library(tidyverse)
library(pwr)
library(glmmTMB)
library(simr)

set.seed(42) # for reproducibility

# Experiment 1 ------------------------------------------------------------

# Define effect sizes
effect_sizes <- c(small = 0.1, medium = 0.3, large = 0.5)

# Define power levels
power_levels <- seq(0.7, 0.95, by = 0.05)

# Calculate sample sizes for different power levels and effect sizes
sample_sizes <- sapply(effect_sizes, function(w) {
  sapply(power_levels, function(power) {
    ceiling(pwr.chisq.test(w = w, df = 1, sig.level = 0.05, power = power)$N)
  })
})

# Create table of sample sizes
sample_size_table <- as.data.frame(sample_sizes)
rownames(sample_size_table) <- power_levels
colnames(sample_size_table) <- names(effect_sizes)

# Save sample sizes table
write.csv(sample_size_table, "./output/exp1_power_curve.csv")

# Experiment 2 ------------------------------------------------------------

# Parameters
n_participants <- 50  # starting sample size
n_conditions <- 4  # 2 (difficulty) x 2 (attractiveness)
medium_effect_size <- 0.15  # Cohen's f for within-subjects design

# Convert Cohen's f to f^2
f2 <- medium_effect_size^2

# Degrees of freedom
df_num <- n_conditions - 1
df_denom <- (n_participants - 1) * (n_conditions - 1)

# Calculate power for initial sample size
initial_power <- pwr.f2.test(u = df_num, v = df_denom, f2 = f2, sig.level = 0.05)$power

# Calculate power curve
sample_sizes <- seq(n_participants, 300, by = 10)
powers <- sapply(sample_sizes, function(n) {
  df_denom <- (n - 1) * (n_conditions - 1)
  pwr.f2.test(u = df_num, v = df_denom, f2 = f2, sig.level = 0.05)$power
})

# Create power curve data frame
power_curve_data <- data.frame(
  sample_size = sample_sizes,
  power = powers
)

# Save power curve data
write.csv(power_curve_data, "./output/exp2_power_curve.csv", row.names = FALSE)

# Plot power curve
ggplot(power_curve_data, aes(x = sample_size, y = power)) +
  geom_line() +
  geom_hline(yintercept = 0.9, linetype = "dashed", color = "red") +
  labs(
    title = "Power Curve for 2x2 Within-Subjects Design",
    subtitle = "Factors: Difficulty and Attractiveness",
    x = "Sample Size", y = "Power"
    ) +
  see::theme_modern() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Calculate sample size for 90% power
sample_size_90 <- min(power_curve_data$sample_size[power_curve_data$power >= 0.9])

# Experiment 3 ------------------------------------------------------------

# Parameters
n_participants <- 50

# Create a basic data structure (without responses)
design <- expand.grid(
  participant = factor(1:n_participants),
  difficulty = factor(c("Easy", "Hard")),
  attractiveness = factor(c("Boring", "Interesting")),
  delay = c(0, 3, 7, 20, 60, 150)
)
design$log_t <- scale(log(design$delay + 1))

# Specify expected effect sizes
b_intercept <- 0
b_difficulty <- log(1.15)  # log odds ratio for difficulty
b_attractiveness <- log(1.15)  # log odds ratio for attractiveness
b_log_t <- log(1.15)

# Create a formula for the model
formula <- response ~ difficulty + attractiveness + log_t + (1|participant)

# Create artificial mixed model object
model <- makeGlmer(
  formula = formula,
  family = binomial,
  fixef = c(b_intercept, b_difficulty, b_attractiveness, b_log_t),
  VarCorr = list(participant = 0.1),
  data = design
)

# Extend model to desired sample size range
extended_model <- extend(model, along = "participant", n = 300)

# Perform power analysis
power_curve <- powerCurve(
  extended_model,
  test = fixed("difficulty"),
  along = "participant",
  nsim = 1000,
  breaks = seq(n_participants, 300, by = 10)
)

# Create power curve data frame 
power_data <- as.data.frame(summary(power_curve))

# Save power curve data
write.csv(power_data, "./output/exp3_power_curve.csv", row.names = FALSE)

# Plot power curve
plot(power_curve)

# Find minimum sample size for 90% power
min_sample_size <- min(power_data$participants[power_data$power >= 0.9])
