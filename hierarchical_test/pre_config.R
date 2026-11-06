# =============================================================
# pre_config.R: Auto-resolving path configuration & compilation
# =============================================================

# 1. Automatically detect script location regardless of working directory
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

test_dir   <- get_script_dir()
pkg_root   <- normalizePath(file.path(test_dir, ".."), winslash = "/")
custom_lib <- file.path(test_dir, "custom_lib")

message("Working in: ", test_dir)
message("Package root located at: ", pkg_root)

if (!dir.exists(custom_lib)) {
  dir.create(custom_lib, recursive = TRUE)
}

# 2. Apply C++ GCC 14 make_unique patch to src/utility.h
util_path <- file.path(pkg_root, "src", "utility.h")
if (file.exists(util_path)) {
  lines <- readLines(util_path, warn = FALSE)
  start_idx <- grep("namespace detail \\{", lines)[1]
  end_idx   <- grep("make_unique\\(Args&&\\.\\.\\.\\) = delete;", lines)[1]
  
  if (!is.na(start_idx) && !is.na(end_idx)) {
    lines <- c(
      lines[1:(start_idx - 1)],
      "using std::make_unique;",
      lines[(end_idx + 1):length(lines)]
    )
    writeLines(lines, util_path)
    message("Applied make_unique patch to utility.h")
  } else {
    message("utility.h already patched or pattern not found.")
  }
} else {
  stop("Cannot find src/utility.h at: ", util_path)
}

# 3. Ensure Makevars and Makevars.win exist
makevars_content <- c(
  "PKG_CPPFLAGS = -DR_NO_REMAP -DNDEBUG",
  "PKG_CXXFLAGS = -Wno-ignored-attributes"
)

writeLines(makevars_content, con = file.path(pkg_root, "src", "Makevars"))
writeLines(makevars_content, con = file.path(pkg_root, "src", "Makevars.win"))
message("Configured Makevars and Makevars.win")

# 4. Compile and install into custom_lib
message("Installing ranger into custom_lib...")
r_bin <- file.path(R.home("bin"), "R.exe")
if (!file.exists(r_bin)) {
  r_bin <- file.path(R.home("bin"), "R") # Fallback for Linux/macOS
}

install_status <- system2(
  r_bin,
  c("CMD", "INSTALL", "--no-multiarch", "--no-lock", paste0("--library=", custom_lib), shQuote(pkg_root))
)

if (install_status == 0) {
  message("\nBuild successful. Installed library path: ", custom_lib)
} else {
  stop("Installation failed with exit code: ", install_status)
}