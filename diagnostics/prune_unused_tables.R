# ---- Unused object pruner (dry-run first!) ----
prune_unused_objects <- function(
    paths = c("munge", "src", "R"),
    include_ext = c("R","r","Rmd","Rmarkdown","qmd"),
    env = .GlobalEnv,
    exclude = character(),        # objects to keep even if unreferenced
    dry_run = TRUE,
    verbose = TRUE
){
  stopifnot(is.environment(env))
  # 1) Gather files
  pat <- paste0("\\.(", paste(include_ext, collapse="|"), ")$")
  files <- unique(unlist(lapply(paths, function(p) {
    if (dir.exists(p)) list.files(p, pattern = pat, recursive = TRUE, full.names = TRUE) else character()
  })))
  if (length(files) == 0 && verbose) message("No project files found in: ", paste(paths, collapse=", "))

  # 2) Extract referenced symbols
  referenced <- character()
  for (f in files) {
    ext <- tools::file_ext(f)
    if (tolower(ext) == "r") {
      # Prefer parser-based symbol extraction for .R files
      src <- try(parse(f, keep.source = TRUE), silent = TRUE)
      if (!inherits(src, "try-error")) {
        pd <- try(getParseData(src), silent = TRUE)
        if (!inherits(pd, "try-error") && is.data.frame(pd)) {
          syms <- pd$text[pd$token %in% c("SYMBOL","SYMBOL_FUNCTION_CALL","SYMBOL_SUB","SYMBOL_PACKAGE")]
          referenced <- c(referenced, syms)
          next
        }
      }
      # Fallback to regex if parse fails
      txt <- try(readLines(f, warn = FALSE), silent = TRUE)
      if (!inherits(txt, "try-error")) referenced <- c(referenced, unlist(regmatches(txt, gregexpr("[.A-Za-z]\\w*", txt))))
    } else {
      # For Rmd/qmd/etc., do a simple regex scan (good enough for object names)
      txt <- try(readLines(f, warn = FALSE), silent = TRUE)
      if (!inherits(txt, "try-error")) referenced <- c(referenced, unlist(regmatches(txt, gregexpr("[.A-Za-z]\\w*", txt))))
    }
  }
  referenced <- unique(referenced)

  # 3) Objects in the environment
  env_objs <- ls(envir = env, all.names = FALSE)
  env_objs <- setdiff(env_objs, exclude)

  # 4) Consider backticked names too (rare but possible)
  backticked <- gsub("^`|`$", "", referenced[grepl("^`.*`$", referenced)])
  referenced <- unique(c(referenced, backticked))

  # 5) Heuristic: keep objects that look like functions by default (optional)
  is_func <- vapply(env_objs, function(x) is.function(get(x, envir = env)), logical(1))
  objs_to_check <- env_objs[!is_func] # you can do env_objs[!is_func] if you want to skip functions

  # 6) Decide which are unreferenced (exact name match)
  unref <- setdiff(objs_to_check, referenced)

  # 7) Report sizes
  sizes <- if (length(unref)) {
    sapply(unref, function(x) object.size(get(x, envir = env)), USE.NAMES = TRUE)
  } else numeric(0)

  report <- data.frame(
    object = names(sizes),
    bytes  = as.numeric(sizes),
    size   = utils::format.object_size(sizes, "auto"),
    row.names = NULL
  )
  report <- report[order(report$bytes, decreasing = TRUE), , drop = FALSE]

  if (verbose) {
    if (nrow(report)) {
      message("Candidates to remove (NOT referenced in project code):")
      print(report, row.names = FALSE)
    } else {
      message("No unreferenced objects detected.")
    }
  }

  # 8) Optional backup + removal
  if (!dry_run && nrow(report)) {
    # Backup first (safety net)
    backup_path <- file.path(tempdir(), paste0("env_backup_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".RData"))
    save(list = env_objs, file = backup_path, envir = env)
    if (verbose) message("Backed up current environment to: ", backup_path)

    rm(list = report$object, envir = env)
    invisible(gc())
    if (verbose) message("Removed ", nrow(report), " objects and called gc().")
  }

  invisible(report)
}

# Usage
# 1) Dry run – see what would be removed
prune_unused_objects(
  paths   = c("munge","src","lib","R"),
  exclude = c("fm1_","fm5_", "farm1_", "farm5_", "cow_features","config", "helper"),   # keep these even if unreferenced
  dry_run = TRUE
)

# 2) If the report looks right, actually remove:
prune_unused_objects(
  paths = c("munge","src","lib","R"),
  exclude = c("fm1_","fm5_", "farm1_", "farm5_", "cow_features","config", "helper"),
                     dry_run = FALSE)
