# farm2_config.R

farm_id <- "farm2"
farm_prefix <- "fm2"
customer_id <- "29e78d45-24b0-4bd2-ada6-545a8c5d7c13"

# Connect to Lely DB (farm-specific)
lely <- dbConnect(odbc(),
                  Driver = "ODBC Driver 17 for SQL Server",
                  Server = "localhost,1433",
                  Database = "GD_Lely",
                  UID = "sa",
                  PWD = "1tsMyL1feNow",
                  Port = 1433)

# Connect to shared Supabase DB (same for all farms)
prod <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = "supabase",
  host = "db.supabase.example.com",
  port = 5432,
  user = Sys.getenv("SUPABASE_USER"),
  password = Sys.getenv("SUPABASE_PWD")
)