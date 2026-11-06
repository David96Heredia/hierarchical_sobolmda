function (formula = NULL, data = NULL, num.trees = 500, mtry = NULL, 
          importance = "none", write.forest = TRUE, probability = FALSE, 
          min.node.size = NULL, max.depth = NULL, replace = TRUE, sample.fraction = ifelse(replace, 
                                                                                           1, 0.632), case.weights = NULL, class.weights = NULL, 
          splitrule = NULL, num.random.splits = 1, alpha = 0.5, minprop = 0.1, 
          split.select.weights = NULL, always.split.variables = NULL, 
          respect.unordered.factors = NULL, scale.permutation.importance = FALSE, 
          local.importance = FALSE, regularization.factor = 1, regularization.usedepth = FALSE, 
          keep.inbag = FALSE, inbag = NULL, holdout = FALSE, quantreg = FALSE, 
          oob.error = TRUE, num.threads = NULL, save.memory = FALSE, 
          verbose = TRUE, seed = NULL, dependent.variable.name = NULL, 
          status.variable.name = NULL, classification = NULL, x = NULL, 
          y = NULL, ...) 
{
  if (length(list(...)) > 0) {
    warning(paste("Unused arguments:", paste(names(list(...)), 
                                             collapse = ", ")))
  }
  snp.data <- as.matrix(0)
  gwa.mode <- FALSE
  if (is.null(data)) {
    if (is.null(x) | is.null(y)) {
      stop("Error: Either data or x and y is required.")
    }
  }
  else {
    if (inherits(data, "gwaa.data")) {
      snp.names <- data@gtdata@snpnames
      snp.data <- data@gtdata@gtps@.Data
      data <- data@phdata
      if ("id" %in% names(data)) {
        data$id <- NULL
      }
      gwa.mode <- TRUE
      save.memory <- FALSE
    }
    if (is.null(formula)) {
      if (is.null(dependent.variable.name)) {
        if (is.null(y) | is.null(x)) {
          stop("Error: Please give formula, dependent variable name or x/y.")
        }
      }
      else {
        if (is.null(status.variable.name)) {
          y <- data[, dependent.variable.name, drop = TRUE]
          x <- data[, !(colnames(data) %in% dependent.variable.name), 
                    drop = FALSE]
        }
        else {
          y <- survival::Surv(data[, dependent.variable.name], 
                              data[, status.variable.name])
          x <- data[, !(colnames(data) %in% c(dependent.variable.name, 
                                              status.variable.name)), drop = FALSE]
        }
      }
    }
    else {
      formula <- formula(formula)
      if (!inherits(formula, "formula")) {
        stop("Error: Invalid formula.")
      }
      data.selected <- parse.formula(formula, data, env = parent.frame())
      y <- data.selected[, 1]
      x <- data.selected[, -1, drop = FALSE]
    }
  }
  if (inherits(x, "Matrix")) {
    if (!inherits(x, "dgCMatrix")) {
      stop("Error: Currently only sparse data of class 'dgCMatrix' supported.")
    }
    if (!is.null(formula)) {
      stop("Error: Sparse matrices only supported with alternative interface. Use dependent.variable.name or x/y instead of formula.")
    }
  }
  if (any(is.na(x))) {
    offending_columns <- colnames(x)[colSums(is.na(x)) > 
                                       0]
    stop("Missing data in columns: ", paste0(offending_columns, 
                                             collapse = ", "), ".", call. = FALSE)
  }
  if (any(is.na(y))) {
    stop("Missing data in dependent variable.", call. = FALSE)
  }
  if (is.factor(y)) {
    if (nlevels(y) != nlevels(droplevels(y))) {
      dropped_levels <- setdiff(levels(y), levels(droplevels(y)))
      warning("Dropped unused factor level(s) in dependent variable: ", 
              paste0(dropped_levels, collapse = ", "), ".", 
              call. = FALSE)
    }
  }
  if (is.factor(y) || is.logical(y)) {
    if (probability) {
      treetype <- 9
    }
    else {
      treetype <- 1
    }
  }
  else if (is.numeric(y) && (is.null(ncol(y)) || ncol(y) == 
                             1)) {
    if (!is.null(classification) && classification && !probability) {
      treetype <- 1
    }
    else if (probability) {
      treetype <- 9
    }
    else {
      treetype <- 3
    }
  }
  else if (inherits(y, "Surv") || is.data.frame(y) || is.matrix(y)) {
    treetype <- 5
  }
  else {
    stop("Error: Unsupported type of dependent variable.")
  }
  if (quantreg && treetype != 3) {
    stop("Error: Quantile prediction implemented only for regression outcomes.")
  }
  independent.variable.names <- colnames(x)
  if (is.null(respect.unordered.factors)) {
    if (!is.null(splitrule) && splitrule == "extratrees") {
      respect.unordered.factors <- "partition"
    }
    else {
      respect.unordered.factors <- "ignore"
    }
  }
  if (respect.unordered.factors == TRUE) {
    respect.unordered.factors <- "order"
  }
  else if (respect.unordered.factors == FALSE) {
    respect.unordered.factors <- "ignore"
  }
  if (!is.matrix(x) && !inherits(x, "Matrix") && ncol(x) > 
      0) {
    character.idx <- sapply(x, is.character)
    if (respect.unordered.factors == "order") {
      ordered.idx <- sapply(x, is.ordered)
      factor.idx <- sapply(x, is.factor)
      recode.idx <- character.idx | (factor.idx & !ordered.idx)
      if (any(recode.idx) & (importance == "impurity_corrected" || 
                             importance == "impurity_unbiased")) {
        warning("Corrected impurity importance may not be unbiased for re-ordered factor levels. Consider setting respect.unordered.factors to 'ignore' or 'partition' or manually compute corrected importance.")
      }
      if (is.factor(y)) {
        num.y <- as.numeric(y)
      }
      else {
        num.y <- y
      }
      if (quantreg) {
        x_orig <- x
      }
      x[recode.idx] <- lapply(x[recode.idx], function(xx) {
        if (!is.factor(xx)) {
          xx <- as.factor(xx)
        }
        if (length(levels(xx)) == 1) {
          levels.ordered <- levels(xx)
        }
        else if (inherits(y, "Surv")) {
          levels.ordered <- largest.quantile(y ~ xx)
          levels.missing <- setdiff(levels(xx), levels.ordered)
          levels.ordered <- c(levels.missing, levels.ordered)
        }
        else if (is.factor(y) & nlevels(y) > 2) {
          levels.ordered <- pca.order(y = y, x = xx)
        }
        else {
          means <- sapply(levels(xx), function(y) {
            mean(num.y[xx == y])
          })
          levels.ordered <- as.character(levels(xx)[order(means)])
        }
        factor(xx, levels = levels.ordered, ordered = TRUE, 
               exclude = NULL)
      })
      covariate.levels <- lapply(x, levels)
    }
    else {
      x[character.idx] <- lapply(x[character.idx], factor)
    }
  }
  if (gwa.mode) {
    all.independent.variable.names <- c(independent.variable.names, 
                                        snp.names)
  }
  else {
    all.independent.variable.names <- independent.variable.names
  }
  if (length(all.independent.variable.names) < 1) {
    stop("Error: No covariates found.")
  }
  if (!is.numeric(num.trees) || num.trees < 1) {
    stop("Error: Invalid value for num.trees.")
  }
  if (is.function(mtry)) {
    nv <- length(all.independent.variable.names)
    if (length(formals(mtry)) > 1) {
      stop("Error: Given mtry function requires single argument (the number of independent variables in the model).")
    }
    mtry <- try(mtry(nv), silent = TRUE)
    if (inherits(mtry, "try-error")) {
      message("The mtry function produced the error: ", 
              mtry)
      stop("Error: mtry function evaluation resulted in an error.")
    }
    if (!is.numeric(mtry) || length(mtry) != 1) {
      stop("Error: Given mtry function should return a single integer or numeric.")
    }
    else {
      mtry <- as.integer(mtry)
    }
    if (mtry < 1 || mtry > nv) {
      stop("Error: Given mtry function should evaluate to a value not less than 1 and not greater than the number of independent variables ( = ", 
           nv, " )")
    }
  }
  if (is.null(mtry)) {
    mtry <- 0
  }
  else if (!is.numeric(mtry) || mtry < 0) {
    stop("Error: Invalid value for mtry")
  }
  if (is.null(seed)) {
    seed <- runif(1, 0, .Machine$integer.max)
  }
  if (!is.logical(keep.inbag)) {
    stop("Error: Invalid value for keep.inbag")
  }
  if (is.null(num.threads)) {
    num.threads = 0
  }
  else if (!is.numeric(num.threads) || num.threads < 0) {
    stop("Error: Invalid value for num.threads")
  }
  if (is.null(min.node.size)) {
    min.node.size <- 0
  }
  else if (!is.numeric(min.node.size) || min.node.size < 0) {
    stop("Error: Invalid value for min.node.size")
  }
  if (is.null(max.depth)) {
    max.depth <- 0
  }
  else if (!is.numeric(max.depth) || max.depth < 0) {
    stop("Error: Invalid value for max.depth. Please give a positive integer.")
  }
  if (!is.numeric(sample.fraction)) {
    stop("Error: Invalid value for sample.fraction. Please give a value in (0,1] or a vector of values in [0,1].")
  }
  if (length(sample.fraction) > 1) {
    if (!(treetype %in% c(1, 9))) {
      stop("Error: Invalid value for sample.fraction. Vector values only valid for classification forests.")
    }
    if (any(sample.fraction < 0) || any(sample.fraction > 
                                        1)) {
      stop("Error: Invalid value for sample.fraction. Please give a value in (0,1] or a vector of values in [0,1].")
    }
    if (sum(sample.fraction) <= 0) {
      stop("Error: Invalid value for sample.fraction. Sum of values must be >0.")
    }
    if (length(sample.fraction) != nlevels(y)) {
      stop("Error: Invalid value for sample.fraction. Expecting ", 
           nlevels(y), " values, provided ", length(sample.fraction), 
           ".")
    }
    if (!replace & any(sample.fraction * length(y) > table(y))) {
      idx <- which(sample.fraction * length(y) > table(y))[1]
      stop("Error: Not enough samples in class ", names(idx), 
           "; available: ", table(y)[idx], ", requested: ", 
           (sample.fraction * length(y))[idx], ".")
    }
    if (!is.null(case.weights)) {
      stop("Error: Combination of case.weights and class-wise sampling not supported.")
    }
    sample.fraction <- sample.fraction[as.numeric(unique(y))]
  }
  else {
    if (sample.fraction <= 0 || sample.fraction > 1) {
      stop("Error: Invalid value for sample.fraction. Please give a value in (0,1] or a vector of values in [0,1].")
    }
  }
  if (all(regularization.factor == 1)) {
    regularization.factor <- c(0, 0)
    use.regularization.factor <- FALSE
  }
  else {
    if (num.threads != 1) {
      num.threads <- 1
      warning("Parallelization deactivated (regularization used).")
    }
    use.regularization.factor <- TRUE
  }
  if (use.regularization.factor) {
    if (max(regularization.factor) > 1) {
      stop("The regularization coefficients cannot be greater than 1.")
    }
    if (max(regularization.factor) <= 0) {
      stop("The regularization coefficients cannot be smaller than 0.")
    }
    p <- length(all.independent.variable.names)
    if (length(regularization.factor) != 1 && length(regularization.factor) != 
        p) {
      stop("You must use 1 or p (the number of predictor variables)\n      regularization coefficients.")
    }
    if (length(regularization.factor) == 1) {
      regularization.factor = rep(regularization.factor, 
                                  p)
    }
  }
  if (is.null(importance) || importance == "none") {
    importance.mode <- 0
  }
  else if (importance == "impurity") {
    importance.mode <- 1
  }
  else if (importance == "impurity_corrected" || importance == 
           "impurity_unbiased") {
    importance.mode <- 5
  }
  else if (importance == "permutation") {
    if (local.importance) {
      importance.mode <- 6
    }
    else if (scale.permutation.importance) {
      importance.mode <- 2
    }
    else {
      importance.mode <- 3
    }
  }
  else if (importance == "sobolMDA") {
    importance.mode <- 7
  }
  else {
    stop("Error: Unknown importance mode.")
  }
  if (is.null(case.weights) || length(unique(case.weights)) == 
      1) {
    case.weights <- c(0, 0)
    use.case.weights <- FALSE
    if (holdout) {
      stop("Error: Case weights required to use holdout mode.")
    }
  }
  else {
    use.case.weights <- TRUE
    if (holdout) {
      sample.fraction <- sample.fraction * mean(case.weights > 
                                                  0)
    }
    if (!replace && sum(case.weights > 0) < sample.fraction * 
        nrow(x)) {
      stop("Error: Fewer non-zero case weights than observations to sample.")
    }
  }
  if (is.null(inbag)) {
    inbag <- list(c(0, 0))
    use.inbag <- FALSE
  }
  else if (is.list(inbag)) {
    use.inbag <- TRUE
    if (use.case.weights) {
      stop("Error: Combination of case.weights and inbag not supported.")
    }
    if (length(sample.fraction) > 1) {
      stop("Error: Combination of class-wise sampling and inbag not supported.")
    }
    if (length(inbag) != num.trees) {
      stop("Error: Size of inbag list not equal to number of trees.")
    }
  }
  else {
    stop("Error: Invalid inbag, expects list of vectors of size num.trees.")
  }
  if (is.null(class.weights)) {
    class.weights <- rep(1, nlevels(y))
  }
  else {
    if (!(treetype %in% c(1, 9))) {
      stop("Error: Argument class.weights only valid for classification forests.")
    }
    if (!is.numeric(class.weights) || any(class.weights < 
                                          0)) {
      stop("Error: Invalid value for class.weights. Please give a vector of non-negative values.")
    }
    if (length(class.weights) != nlevels(y)) {
      stop("Error: Number of class weights not equal to number of classes.")
    }
    class.weights <- class.weights[unique(as.numeric(y))]
  }
  if (is.null(split.select.weights)) {
    split.select.weights <- list(c(0, 0))
    use.split.select.weights <- FALSE
  }
  else if (is.numeric(split.select.weights)) {
    if (length(split.select.weights) != length(all.independent.variable.names)) {
      stop("Error: Number of split select weights not equal to number of independent variables.")
    }
    split.select.weights <- list(split.select.weights)
    use.split.select.weights <- TRUE
  }
  else if (is.list(split.select.weights)) {
    if (length(split.select.weights) != num.trees) {
      stop("Error: Size of split select weights list not equal to number of trees.")
    }
    use.split.select.weights <- TRUE
  }
  else {
    stop("Error: Invalid split select weights.")
  }
  if (is.null(always.split.variables)) {
    always.split.variables <- c("0", "0")
    use.always.split.variables <- FALSE
  }
  else {
    use.always.split.variables <- TRUE
  }
  if (use.split.select.weights && use.always.split.variables) {
    stop("Error: Please use only one option of split.select.weights and always.split.variables.")
  }
  if (is.null(splitrule)) {
    if (treetype == 5) {
      splitrule <- "logrank"
    }
    else if (treetype == 3) {
      splitrule <- "variance"
    }
    else if (treetype %in% c(1, 9)) {
      splitrule <- "gini"
    }
    splitrule.num <- 1
  }
  else if (splitrule == "logrank") {
    if (treetype == 5) {
      splitrule.num <- 1
    }
    else {
      stop("Error: logrank splitrule applicable to survival data only.")
    }
  }
  else if (splitrule == "gini") {
    if (treetype %in% c(1, 9)) {
      splitrule.num <- 1
    }
    else {
      stop("Error: Gini splitrule applicable to classification data only.")
    }
  }
  else if (splitrule == "variance") {
    if (treetype == 3) {
      splitrule.num <- 1
    }
    else {
      stop("Error: variance splitrule applicable to regression data only.")
    }
  }
  else if (splitrule == "auc" || splitrule == "C") {
    if (treetype == 5) {
      splitrule.num <- 2
    }
    else {
      stop("Error: C index splitrule applicable to survival data only.")
    }
  }
  else if (splitrule == "auc_ignore_ties" || splitrule == "C_ignore_ties") {
    if (treetype == 5) {
      splitrule.num <- 3
    }
    else {
      stop("Error: C index splitrule applicable to survival data only.")
    }
  }
  else if (splitrule == "maxstat") {
    if (treetype == 5 || treetype == 3) {
      splitrule.num <- 4
    }
    else {
      stop("Error: maxstat splitrule applicable to regression or survival data only.")
    }
  }
  else if (splitrule == "extratrees") {
    splitrule.num <- 5
  }
  else if (splitrule == "beta") {
    if (treetype == 3) {
      splitrule.num <- 6
    }
    else {
      stop("Error: beta splitrule applicable to regression data only.")
    }
    if (min(y) < 0 || max(y) > 1) {
      stop("Error: beta splitrule applicable to regression data with outcome between 0 and 1 only.")
    }
  }
  else if (splitrule == "hellinger") {
    if (treetype %in% c(1, 9)) {
      splitrule.num <- 7
    }
    else {
      stop("Error: Hellinger splitrule only implemented for binary classification.")
    }
    if ((is.factor(y) && nlevels(y) > 2) || (length(unique(y)) > 
                                             2)) {
      stop("Error: Hellinger splitrule only implemented for binary classification.")
    }
  }
  else {
    stop("Error: Unknown splitrule.")
  }
  if (alpha < 0 || alpha > 1) {
    stop("Error: Invalid value for alpha, please give a value between 0 and 1.")
  }
  if (minprop < 0 || minprop > 0.5) {
    stop("Error: Invalid value for minprop, please give a value between 0 and 0.5.")
  }
  if (splitrule == "maxstat" & use.regularization.factor) {
    stop("Error: Regularization cannot be used with 'maxstat' splitrule.")
  }
  if (!is.numeric(num.random.splits) || num.random.splits < 
      1) {
    stop("Error: Invalid value for num.random.splits, please give a positive integer.")
  }
  if (splitrule.num == 5 && save.memory && respect.unordered.factors == 
      "partition") {
    stop("Error: save.memory option not possible in extraTrees mode with unordered predictors.")
  }
  if (num.random.splits > 1 && splitrule.num != 5) {
    warning("Argument 'num.random.splits' ignored if splitrule is not 'extratrees'.")
  }
  if (respect.unordered.factors == "partition") {
    ordered.idx <- sapply(x, is.ordered)
    factor.idx <- sapply(x, is.factor)
    unordered.factor.variables <- independent.variable.names[factor.idx & 
                                                               !ordered.idx]
    if (length(unordered.factor.variables) > 0) {
      use.unordered.factor.variables <- TRUE
      num.levels <- sapply(x[, factor.idx & !ordered.idx, 
                             drop = FALSE], nlevels)
      max.level.count <- .Machine$double.digits
      if (max(num.levels) > max.level.count) {
        stop(paste("Too many levels in unordered categorical variable ", 
                   unordered.factor.variables[which.max(num.levels)], 
                   ". Only ", max.level.count, " levels allowed on this system. Consider using the 'order' option.", 
                   sep = ""))
      }
    }
    else {
      unordered.factor.variables <- c("0", "0")
      use.unordered.factor.variables <- FALSE
    }
  }
  else if (respect.unordered.factors == "ignore" || respect.unordered.factors == 
           "order") {
    unordered.factor.variables <- c("0", "0")
    use.unordered.factor.variables <- FALSE
  }
  else {
    stop("Error: Invalid value for respect.unordered.factors, please use 'order', 'partition' or 'ignore'.")
  }
  if (use.unordered.factor.variables && !is.null(splitrule)) {
    if (splitrule == "maxstat") {
      stop("Error: Unordered factor splitting not implemented for 'maxstat' splitting rule.")
    }
    else if (splitrule %in% c("C", "auc", "C_ignore_ties", 
                              "auc_ignore_ties")) {
      stop("Error: Unordered factor splitting not implemented for 'C' splitting rule.")
    }
    else if (splitrule == "beta") {
      stop("Error: Unordered factor splitting not implemented for 'beta' splitting rule.")
    }
  }
  if (respect.unordered.factors == "order") {
    if (treetype == 3 && splitrule == "maxstat") {
      warning("Warning: The 'order' mode for unordered factor handling with the 'maxstat' splitrule is experimental.")
    }
    if (gwa.mode & ((treetype %in% c(1, 9) & nlevels(y) > 
                     2) | treetype == 5)) {
      stop("Error: Ordering of SNPs currently only implemented for regression and binary outcomes.")
    }
  }
  prediction.mode <- FALSE
  predict.all <- FALSE
  prediction.type <- 1
  loaded.forest <- list()
  if (inherits(x, "dgCMatrix")) {
    sparse.x <- x
    x <- matrix(c(0, 0))
    use.sparse.data <- TRUE
  }
  else {
    sparse.x <- Matrix(matrix(c(0, 0)))
    use.sparse.data <- FALSE
    if (is.data.frame(x)) {
      x <- data.matrix(x)
    }
  }
  if (treetype == 5) {
    y.mat <- as.matrix(y)
  }
  else {
    y.mat <- as.matrix(as.numeric(y))
  }
  if (respect.unordered.factors == "order") {
    order.snps <- TRUE
  }
  else {
    order.snps <- FALSE
  }
  if (treetype == 5) {
    if (!all(y.mat[, 2] %in% 0:1)) {
      stop("Error: Competing risks not supported yet. Use status=1 for events and status=0 for censoring.")
    }
  }
  result <- rangerCpp(treetype, x, y.mat, independent.variable.names, 
                      mtry, num.trees, verbose, seed, num.threads, write.forest, 
                      importance.mode, min.node.size, split.select.weights, 
                      use.split.select.weights, always.split.variables, use.always.split.variables, 
                      prediction.mode, loaded.forest, snp.data, replace, probability, 
                      unordered.factor.variables, use.unordered.factor.variables, 
                      save.memory, splitrule.num, case.weights, use.case.weights, 
                      class.weights, predict.all, keep.inbag, sample.fraction, 
                      alpha, minprop, holdout, prediction.type, num.random.splits, 
                      sparse.x, use.sparse.data, order.snps, oob.error, max.depth, 
                      inbag, use.inbag, regularization.factor, use.regularization.factor, 
                      regularization.usedepth)
  if (length(result) == 0) {
    stop("User interrupt or internal error.")
  }
  if (importance.mode != 0) {
    names(result$variable.importance) <- all.independent.variable.names
    if (importance.mode == 6) {
      result$variable.importance.local <- matrix(result$variable.importance.local, 
                                                 byrow = FALSE, ncol = length(all.independent.variable.names), 
                                                 dimnames = list(rownames(data), all.independent.variable.names))
    }
  }
  if (treetype == 1 && oob.error) {
    if (is.factor(y)) {
      result$predictions <- integer.to.factor(result$predictions, 
                                              levels(y))
    }
    result$confusion.matrix <- table(y, result$predictions, 
                                     dnn = c("true", "predicted"), useNA = "ifany")
  }
  else if (treetype == 5 && oob.error) {
    if (is.list(result$predictions)) {
      result$predictions <- do.call(rbind, result$predictions)
    }
    if (is.vector(result$predictions)) {
      result$predictions <- matrix(result$predictions, 
                                   nrow = 1)
    }
    result$chf <- result$predictions
    result$predictions <- NULL
    result$survival <- exp(-result$chf)
  }
  else if (treetype == 9 && oob.error) {
    if (is.list(result$predictions)) {
      result$predictions <- do.call(rbind, result$predictions)
    }
    if (is.vector(result$predictions)) {
      result$predictions <- matrix(result$predictions, 
                                   nrow = 1)
    }
    colnames(result$predictions) <- unique(y)
    if (is.factor(y)) {
      result$predictions <- result$predictions[, levels(droplevels(y)), 
                                               drop = FALSE]
    }
  }
  result$splitrule <- splitrule
  if (splitrule == "extratrees") {
    result$num.random.splits <- num.random.splits
  }
  if (treetype == 1) {
    result$treetype <- "Classification"
  }
  else if (treetype == 3) {
    result$treetype <- "Regression"
  }
  else if (treetype == 5) {
    result$treetype <- "Survival"
  }
  else if (treetype == 9) {
    result$treetype <- "Probability estimation"
  }
  if (treetype == 3) {
    result$r.squared <- 1 - result$prediction.error/var(y)
  }
  result$call <- sys.call()
  result$importance.mode <- importance
  if (use.sparse.data) {
    result$num.samples <- nrow(sparse.x)
  }
  else {
    result$num.samples <- nrow(x)
  }
  result$replace <- replace
  if (write.forest) {
    if (is.factor(y)) {
      result$forest$levels <- levels(y)
    }
    result$forest$independent.variable.names <- independent.variable.names
    result$forest$treetype <- result$treetype
    class(result$forest) <- "ranger.forest"
    if (respect.unordered.factors == "order" && ncol(x) > 
        0) {
      result$forest$covariate.levels <- covariate.levels
    }
  }
  class(result) <- "ranger"
  if (quantreg) {
    if (respect.unordered.factors == "order" && !is.null(x_orig)) {
      terminal.nodes <- predict(result, x_orig, type = "terminalNodes")$predictions + 
        1
    }
    else {
      terminal.nodes <- predict(result, x, type = "terminalNodes")$predictions + 
        1
    }
    n <- result$num.samples
    result$random.node.values <- matrix(nrow = max(terminal.nodes), 
                                        ncol = num.trees)
    for (tree in 1:num.trees) {
      idx <- sample(1:n, n)
      result$random.node.values[terminal.nodes[idx, tree], 
                                tree] <- y[idx]
    }
    if (!is.null(result$inbag.counts)) {
      inbag.counts <- simplify2array(result$inbag.counts)
      random.node.values.oob <- randomObsNode(terminal.nodes, 
                                              y, inbag.counts)
      minoob <- min(rowSums(inbag.counts == 0))
      if (minoob < 10) {
        stop("Error: Too few trees for out-of-bag quantile regression.")
      }
      result$random.node.values.oob <- t(apply(random.node.values.oob, 
                                               1, function(x) {
                                                 sample(x[!is.na(x)], minoob)
                                               }))
    }
  }
  return(result)
}