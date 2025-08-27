# farm5_config.R

farm_id <- "farm5"
farm_prefix <- "fm5"
farm5_customer_id <- "f3128d08-41c4-4584-bb55-71816726277b"

# Connect to Lely DB (farm-specific)
lely <- dbConnect(odbc(),
                  Driver = "ODBC Driver 17 for SQL Server",
                  Server = "localhost,1433",
                  Database = "Lely_Marais",
                  UID = "sa",
                  PWD = "1tsMyL1feNow",
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