# 02_ingest_global_data.R

# Save animals_history table from Supabase for later use

animals_history <- dbGetQuery(prod, "
  SELECT
  	animal,
		category,
		customer_id,
		date,
		entry_code,
		entry_date,
		exit_code,
		exit_date,
		month,
		year
	FROM animals_history
	ORDER BY animal")

cache("animals_history")

animals <- dbGetQuery(prod, "
  SELECT
    created_at,
    customer_id,
    birth_date,
    country_code,
    national_number,
    race,
    mother
  FROM animals
  ORDER BY national_number")

cache("animals")


# Save animals_slaughter table from Supabase for later use
animals_slaughter <- dbGetQuery(prod, "
  SELECT
    national_number,
    date,
    weight,
    created_at,
    customer_id,
    selling_price,
    classification,
    category
  FROM animals_slaughter
  ORDER BY national_number, date")

cache("animals_slaughter")

milk_invoice <- dbGetQuery(prod, "
  SELECT *
  FROM milk_invoice")


# --- Supabase Tables ---
gl_entries <- dbGetQuery(prod, "
  SELECT
    journal,
		identifier,
		date,
		counterpart,
		label,
		number,
		quantity,
		unit_price,
		debit,
		credit,
		balance,
		gl_account_number,
		customer_id,
		balance_num,
		start_fiscal_year,
		end_fiscal_year,
		document_id,
		invoice_id
	FROM gl_entries")

# Grand Livre entries for the first farm in the analysis
