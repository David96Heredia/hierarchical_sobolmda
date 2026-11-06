# =============================================================
# pre_config.R: Patch and install ranger from root into custom_lib
# =============================================================

# Determine directory paths relative to this script
test_dir   <- getwd() # Expected to run from inside hierarchical_test
pkg_root   <- normalizePath(file.path(test_dir, ".."), winslash = "/")
custom_lib <- file.path(test_dir, "custom_lib")

if (!dir.exists(custom_lib)) {
  dir.create(custom_lib, recursive = TRUE)
}

# 1. Apply C++ GCC 14 make_unique patch to src/utility.h
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

# 2. Ensure Makevars and Makevars.win exist
makevars_content <- c(
  "PKG_CPPFLAGS = -DR_NO_REMAP -DNDEBUG",
  "PKG_CXXFLAGS = -Wno-ignored-attributes"
)

writeLines(makevars_content, con = file.path(pkg_root, "src", "Makevars"))
writeLines(makevars_content, con = file.path(pkg_root, "src", "Makevars.win"))
message("Configured Makevars and Makevars.win")

# 3. Compile and install into custom_lib
message("Installing ranger into custom_lib...")
r_bin <- file.path(R.home("bin"), "R.exe")
if (!file.exists(r_bin)) {
  r_bin <- file.path(R.home("bin"), "R") # Fallback for Linux/macOS cloud
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