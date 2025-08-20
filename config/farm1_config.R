# farm1_config.R

farm_id <- "farm1"
farm_prefix <- "fm1"
customer_id <- "16450bc2-f930-4052-a3f7-a602646e64cc"

# Connect to Lely DB (farm-specific)
lely <- dbConnect(odbc(),
                  Driver = "ODBC Driver 17 for SQL Server",
                  Server = "localhost,1433",
                  Database = "LelyDB",
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