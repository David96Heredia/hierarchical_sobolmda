# =============================================================
# test_sobolmda.R: Run sobolMDA simulation using custom_lib build
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
custom_lib <- file.path(test_dir, "custom_lib")

# 2. Detach and unload any active ranger instances
if ("package:ranger" %in% search()) {
  detach("package:ranger", unload = TRUE, force = TRUE)
}
if ("ranger" %in% loadedNamespaces()) {
  unloadNamespace("ranger")
}

# 3. Load explicitly from custom_lib
if (!file.exists(file.path(custom_lib, "ranger"))) {
  stop("ranger is not installed in custom_lib at: ", custom_lib, 
       "\nPlease run pre_config.R first.")
}

library(ranger, lib.loc = custom_lib)
message("Active ranger installation: ", find.package("ranger"))

# 4. Simulate Ishigami-type benchmark data
set.seed(42)
n <- 400
p <- 4

X <- as.data.frame(matrix(runif(n * p), nrow = n, ncol = p))
colnames(X) <- paste0("X", 1:p)

# X1 and X2 contain signal; X3 and X4 are independent noise
y <- sin(pi * X$X1) + 2 * (X$X2 - 0.5)^2 + rnorm(n, sd = 0.1)
df <- data.frame(y = y, X)

# 5. Fit model with sobolMDA
message("Fitting forest with importance = 'sobolMDA'...")
rf_sobol <- ranger(
  formula = y ~ .,
  data = df,
  num.trees = 300,
  importance = "sobolMDA",
  seed = 42
)

# 6. Fit model with standard permutation MDA
message("Fitting forest with importance = 'permutation'...")
rf_perm <- ranger(
  formula = y ~ .,
  data = df,
  num.trees = 300,
  importance = "permutation",
  seed = 42
)

# 7. Print comparison table
results <- data.frame(
  Variable        = names(rf_sobol$variable.importance),
  SobolMDA        = as.numeric(rf_sobol$variable.importance),
  Permutation_MDA = as.numeric(rf_perm$variable.importance)
)

message("\nVariable Importance Scores:")
print(results)