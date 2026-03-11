# Render all .qmd documents in the /analysis directory.

library(here)
library(quarto)
library(furrr)
library(purrr)
library(pagedown)

# Find files to render
files_to_render <- list.files(
  path = here("analysis"),
  pattern = "\\.qmd$",
  full.names = TRUE
)

# Configure workers and render in parallel
workers_to_use <- min(length(files_to_render), max(1, availableCores() - 1))
plan(multisession, workers = workers_to_use)
future_walk(files_to_render, quarto_render, .progress = TRUE)

# Shut down workers
plan(sequential)

# Convert to PDF
files_to_convert <- list.files(
  path = here("analysis"),
  pattern = "\\.html$",
  full.names = TRUE
)

walk(files_to_convert, chrome_print)
