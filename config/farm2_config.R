# farm2_config.R

farm_id <- "farm2"
farm_prefix <- "fm2"
farm2_customer_id <- "29e78d45-24b0-4bd2-ada6-545a8c5d7c13"

# Connect to Lely DB (farm-specific)
lely <- dbConnect(odbc(),
                  Driver = "ODBC Driver 17 for SQL Server",
                  Server = "localhost,1433",
                  Database = "GD_Lely",
                  UID = Sys.getenv("ODBC_UID"),
                  PWD = Sys.getenv("ODBC_PW"),
                  Port = 1433)

# Explicitly load .Renviron if not already loaded
readRenviron(".Renviron")

# Connect using environment variables
prod <- dbConnect(
  RPostgres::Postgres(),
  dbname   = Sys.getenv("PG_DBNAME"),
  host     = Sys.getenv("PG_HOST"),
  port     = as.integer(Sys.getenv("PG_PORT")),
  user     = Sys.getenv("PG_USER"),
  password = Sys.getenv("PG_PASSWORD"),
  sslmode  = "require"
)

# Check the connection
dbListTables(prod)

# Make the prod connection available in the config list
config$pg_prod <- prod