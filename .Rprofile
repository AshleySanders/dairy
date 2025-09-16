# Project-level .Rprofile
# Runs whenever you open this .Rproj

if (requireNamespace("here", quietly = TRUE)) {
  helpers_path <- here::here("lib", "helpers.R")
  if (file.exists(helpers_path)) {
    source(helpers_path, local = FALSE)
    message("Loaded helper functions from lib/helpers.R")
  }
}
