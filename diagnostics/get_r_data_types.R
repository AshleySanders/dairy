# ------------------------------------------------------------------------------
# Script: get_r_data_types.R
# Purpose: Utility to inspect a DB table, infer R data types, and copy results.
# Notes:  Pulls 1 row to detect types; optionally copies summary to clipboard.
# ------------------------------------------------------------------------------


# Check Missing Values and Data Types in a Database Table

# Load libraries
library(DBI)
library(glue)
library(dplyr)
library(clipr)


# Function to get R data types from a database table
get_r_data_types <- function(prod, table_name, schema = "public", copy_clip = TRUE) {
   # Pull 1 row to infer types
  query <- glue::glue("SELECT * FROM {schema}.{table_name} LIMIT 1")
  df_sample <- dbGetQuery(prod, query)

  # Create summary of variable and type
  types_tbl <- tibble::tibble(
    Variable = names(df_sample),
    R_Data_Type = sapply(df_sample, function(x) class(x)[1])  # [1] in case of multiple classes
  )

  if (copy_clip && requireNamespace("clipr", quietly = TRUE)) {
    clipr::write_clip(types_tbl)
    message("Copied to clipboard. Ready to paste into your data dictionary.")
  }

  return(types_tbl)
}
