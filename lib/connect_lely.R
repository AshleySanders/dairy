# Connect to Lely milking machine data in Visual Studio Code via Docker
library(DBI)
library(odbc)

lely <- dbConnect(odbc(),
                  Driver = "ODBC Driver 17 for SQL Server",
                  Server = "localhost,1433",
                  Database = "LelyDB",
                  UID = "sa",
                  PWD = "1tsMyL1feNow",
                  Port = 1433)

# Check the connection
dbListTables(lely)
