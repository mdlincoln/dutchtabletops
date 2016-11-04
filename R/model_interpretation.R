# Exported functions ----

#' Extract error dataframe from randomForest model
#' @export
error_only <- function(cf) {
  if(cf$type == "regression") {
    warning("This is a regression forest, not a classification forest. error_only will return NULL")
    return(NULL)
  }

  cf %>%
    long_confusion() %>%
    select(class.error, actual) %>%
    distinct()
}

#' Extract votes dataframe from randomForest model
#' @export
rf_votes <- function(rf) {
  tibble::rownames_to_column(as.data.frame(rf$votes), var = "painting_code")
}

#' Build a confusion data frame for the test component of a random forest
#'
#' @param rf A randomForest object with a \code{test} component
#' @param rownames The name of the column rownames will be written to
#'
#' @export
test_confusion <- function(rf, rownames = "actual") {
  stopifnot(!is.null(rf$test$votes))
  rf$test$confusion %>%
    as.data.frame() %>%
    rownames_to_column(var = rownames) %>%
    gather_(key_col = "predicted", value_col = "ratio", gather_cols = setdiff(names(.), rownames))
}

#' Get test error
#'
#' @export
test_error <- function(rf) {
  test_confusion(rf) %>%
    filter(predicted == "class.error" & !is.nan(ratio)) %>%
    select(actual, ratio)
}

# %>%
#   ungroup() %>%
#   mutate(actual = as.character(ytest), correct = predicted == actual)

#' Create a PCA of local importance measures from a random forest
#' @export
pca_rf <- function(rf, .filter = function(x) return(x)) {
  rf_local_importance(rf) %>%
    .filter %>%
    stats::prcomp()
}

#' Local Importance as a data frame
#' @export
rf_local_importance <- function(rf) {
  data.frame(t(rf$localImp)) %>%
    tibble::rownames_to_column()
}

#' Calculate local variable loadings
#' @export
pca_loadings_df <- function(rf_pca, m = 200) {
  data.frame(rf_pca$rotation)[,1:2] %>%
    tibble::rownames_to_column() %>%
    mutate(power = sqrt(PC1^2 + PC2^2)) %>%
    filter(min_rank(desc(power)) <= m)
}

#' Return observation locations in PCA space
#' @export
pca_obs_df <- function(rf_pca, rownames = "rownames") {
  data.frame(rf_pca$x)[,1:2] %>%
    tibble::rownames_to_column(var = rownames)
}

#' The top n most important terms for a class
#'
#' @param rf RandomForest object
#' @param class Name of the category
#' @param n Integer. Number of topmost terms to return
#'
#' @return A data frame with term names and importance values
#'
#' @export
top_n_importance <- function(rf, class, n) {
  as.data.frame(rf[["importance"]]) %>%
    tibble::rownames_to_column(var = "term") %>%
    gather(category, importance, -term) %>%
    filter(category == class) %>%
    select(-category) %>%
    arrange(desc(importance)) %>%
    slice(seq_len(n))
}

#' @describeIn top_n_importance Return the names as a vector only
#' @export
top_n_importance_names <- function(rf, class, n) {
  top_n_importance(rf, class, n)[["term"]]
}

# Internal functions ----

confusion <- function(f) {
  as.data.frame(f$confusion)
}

long_confusion <- function(rf) {
  rf %>%
    confusion() %>%
    tibble::rownames_to_column("actual") %>%
    tidyr::gather(projected, count, -actual, -class.error)
}
