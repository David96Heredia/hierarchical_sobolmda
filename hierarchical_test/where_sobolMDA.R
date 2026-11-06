# =============================================================
# find_sobol_mda.R: Locate sobolMDA references in C++ source files
# =============================================================

# 1. Automatically detect script location
get_script_dir <- function() {
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
    if (nzchar(p)) return(dirname(normalizePath(p, winslash = "/")))
  }
  for (frame in sys.frames()) {
    if (exists("ofile", envir = frame, inherits = FALSE)) {
      return(dirname(normalizePath(get("ofile", envir = frame), winslash = "/")))
    }
  }
  normalizePath(getwd(), winslash = "/")
}

test_dir <- get_script_dir()
repo_src <- normalizePath(file.path(test_dir, "..", "src"), winslash = "/")

message("Searching in source directory: ", repo_src, "\n")

# 2. Search for lines handling SOBOL_MDA
cpp_files <- list.files(repo_src, pattern = "\\.(cpp|h)$", full.names = TRUE)

for (f in cpp_files) {
  lines <- readLines(f, warn = FALSE)
  hits  <- grep("SOBOL_MDA|sobol", lines, ignore.case = TRUE)
  if (length(hits) > 0) {
    cat("File:", basename(f), "\n")
    for (h in hits) {
      cat("  Line", h, ":", trimws(lines[h]), "\n")
    }
    cat("\n")
  }
}