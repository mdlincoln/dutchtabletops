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
  add_rownames(as.data.frame(rf$votes), var = "painting_code")
}

# Internal functions ----

confusion <- function(f) {
  as.data.frame(f$confusion)
}

long_confusion <- function(rf) {
  rf %>%
    confusion() %>%
    add_rownames("actual") %>%
    tidyr::gather(projected, count, -actual, -class.error)
}

rf_local_importance <- function(rf) {
  data.frame(t(rf$localImp))
}

pca_rf <- function(rf) {
  stats::prcomp(rf_local_importance(rf))
}

