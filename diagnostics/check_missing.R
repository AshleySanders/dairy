check_table_qa <- function(con, table_name, schema = "public") {
  message(glue("Checking table: {schema}.{table_name}"))

  # Get column names and types
  column_info <- dbGetQuery(con, glue("
    SELECT column_name, data_type
    FROM information_schema.columns
    WHERE table_schema = '{schema}' AND table_name = '{table_name}'
  "))

  columns <- column_info$column_name
  types   <- column_info$data_type

  total_rows <- dbGetQuery(con, glue("SELECT COUNT(*) FROM {schema}.{table_name}"))[[1]]

  # Build dynamic query
  queries <- mapply(function(col, type) {
    col_quoted <- DBI::dbQuoteIdentifier(con, col)

    # For text columns: count NULL, empty, and bad placeholders
    if (grepl("char|text", type, ignore.case = TRUE)) {
      glue("
        COUNT(*) FILTER (
          WHERE {col_quoted} IS NULL
             OR TRIM({col_quoted}) = ''
             OR LOWER({col_quoted}) IN ('null', 'n/a', 'na', 'unknown')
        ) AS missing_{col},
        COUNT(DISTINCT {col_quoted}) AS distinct_{col}
      ")
    } else {
      # For non-text columns, just count actual NULLs
      glue("
        COUNT(*) FILTER (WHERE {col_quoted} IS NULL) AS missing_{col},
        COUNT(DISTINCT {col_quoted}) AS distinct_{col}
      ")
    }
  }, col = columns, type = types)

  full_query <- glue("SELECT {paste(queries, collapse = ', ')} FROM {schema}.{table_name}")
  result <- dbGetQuery(con, full_query)

  # Build tidy result
  df <- tibble(
    Variable = columns,
    Data_Type = types,
    Missing_Values = as.integer(result[1, grep("missing_", names(result))]),
    Distinct_Values = as.integer(result[1, grep("distinct_", names(result))]),
    Total_Rows = total_rows
  ) %>%
    mutate(
      Percent_Missing = round((Missing_Values / Total_Rows) * 100, 2),
      Is_Constant = Distinct_Values == 1
    )

  return(df)
}