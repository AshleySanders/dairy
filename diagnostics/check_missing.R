

# Identify missing values that are NA or ""
summarize_missing <- function(df) {
  tibble(
    Variable = names(df),
    Missing_Count = sapply(df, function(x) sum(is.na(x) | x == "")),
    Total_Rows = nrow(df)
  ) %>%
    mutate(
      Percent_Missing = round((Missing_Count / Total_Rows) * 100, 2)
    ) %>%
    arrange(desc(Percent_Missing))
}

# Apply to the dataframe
print(n=23, summarize_missing(mk_animals_lactations))
