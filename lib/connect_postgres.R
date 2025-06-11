setwd("~/Dropbox/Code/DataSci-R/01-Client-Projects/Cockpit/dairy")

# Load the project
load.project()
# Load the necessary libraries
library(DBI)
library(RPostgres)
library(httr)

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


# Disconnect from the database
# dbDisconnect(dev)
# dbDisconnect(prod)