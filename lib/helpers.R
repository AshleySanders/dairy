# helper function to strip spaces and convert animal IDs
clean_ani <- function(x) {
  stringr::str_replace_all(stringr::str_trim(as.character(x)), " ", "")
}

# assign a named object with farm-specific prefix and cache it
assign_and_cache <- function(obj_name, value, prefix = farm_prefix) {
  full_name <- paste0(prefix, "_", obj_name)
  assign(full_name, value, envir = .GlobalEnv)
  cache(full_name)
}
