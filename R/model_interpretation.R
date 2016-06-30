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
