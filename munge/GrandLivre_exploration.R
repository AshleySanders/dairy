# This script is an exploration of the data available in ledger entries to determine how much data we have access to in this table versus needing to gather details from invoices.

gl_entries_farm1 <- dbGetQuery(prod, "
                         SELECT
    *
  FROM
    gl_entries
  WHERE
    customer_id = '16450bc2-f930-4052-a3f7-a602646e64cc'
  ORDER BY
    date DESC")

head(gl_entries_farm1$label)
# Check the unique labels to understand what types of entries we have
unique(gl_entries_farm1$label)
