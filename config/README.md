# Config Directory

This directory contains **farm-specific configuration files** and the **global project configuration** used by ProjectTemplate.

### Contents
- **farmX_config.R**  
  Farm-specific connection settings and identifiers.  
  Each file defines:
  - `farm_id` – canonical farm identifier (e.g., `"farm1"`).
  - `farm_prefix` – short prefix used in table names or joins (e.g., `"fm1"`).
  - `customer_id` – internal/customer reference ID.  
  - `lely` – database connection to the farm’s **local Lely SQL Server** instance.  
  - `prod` – connection to the shared **Supabase Postgres database** (same for all farms).

  ⚠️ **Sensitive information** (usernames, passwords, hostnames) should **never be hard-coded** here. Instead:
  - Store credentials in your local `~/.Renviron` file or use a credential manager.
  - Access them via `Sys.getenv("VAR_NAME")`.

  Example:
  user = Sys.getenv("SUPABASE_USER")
  password = Sys.getenv("SUPABASE_PWD")
