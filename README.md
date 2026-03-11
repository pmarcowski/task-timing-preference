![Preview](preview.png)

# Task timing and preference analysis

**[Companion Website](https://pmarcowski.github.io/task-timing-preference/)**

This repository contains the data and analysis notebooks for a research project investigating temporal task scheduling and preferences. The study examines how task timing, difficulty, and attractiveness influence decision-making across three experiments.

## Overview

This project investigates how individuals make decisions about task scheduling based on factors such as:

1. Task timing (immediate vs. future)
2. Task difficulty (easy vs. hard)
3. Task attractiveness (boring vs. interesting)

The research uses Bayesian statistical models to demonstrate how these factors interact to influence task preferences and scheduling decisions.

## Directory Structure

```
task-timing-preference/
│
├── R/
│   ├── power_analysis.R       # A priori power analyses
│   ├── prepare_data.R         # Data preprocessing
│   └── render_all.R           # Batch rendering utility
│
├── analysis/
│   ├── analysis_pilot.qmd     # Pilot study analysis
│   ├── analysis_exp1.qmd      # Experiment 1 (binomial model)
│   ├── analysis_exp2.qmd      # Experiment 2 (Gaussian mixed model)
│   └── analysis_exp3.qmd      # Experiment 3 (Bernoulli mixed model)
│
├── data/
│   └── prepared/              # Preprocessed .Rds files
│
├── output/                    # Figures and fitted model objects
│
├── _quarto.yml                # Website configuration
├── index.qmd                  # Website landing page
├── LICENSE
└── README.md
```

## Installation

To run the analyses, you need R and the following packages:

```r
install.packages(c("tidyverse", "brms", "easystats", "patchwork", "see",
                    "BayesFactor", "bayestestR", "pwr", "glmmTMB", "simr"))
```

## Usage

The analysis notebooks in `analysis/` are Quarto documents that can be rendered individually or as a website:

```bash
# Render the full website
quarto render

# Preview locally
quarto preview
```

## Data

The project includes data from a pilot study and three experiments:

- **Pilot**: Task difficulty and interest ratings in experimental vs. survey contexts
- **Experiment 1**: Choice between easy/boring and hard/interesting tasks with immediate or future timing
- **Experiment 2**: Preferences for task delay based on difficulty and attractiveness
- **Experiment 3**: Task acceptance probabilities based on difficulty, attractiveness, and delay

Preprocessed data is stored in `data/prepared/`.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Citation

Citation information to be added upon publication.

## Contact

For any questions or feedback, please contact the author directly.
