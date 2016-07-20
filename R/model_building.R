# Exported functions ----

#' Vector of motif variable names
#'
#' @return Character vector of motif variable names
#' @export
motif_vars <- function() {
  c("B", "B1", "B2", "B3", "B4", "B5", "B6", "Br", "Br1", "Br2", "Br3", "Br4",
    "Br5", "C", "C1", "C2", "C3", "C4", "C5", "CD", "CD1", "CD2", "CD3", "CD4",
    "CD5", "CD6", "CD7", "CD8", "Ch", "Ch1", "Ch2", "Ch3", "Ch4", "Ch5", "Ch6",
    "Ch7", "Ch8", "CO1", "CO1a", "CO1b", "CO1c", "CO2", "CO3", "CO4", "CO5",
    "CO6", "CO7", "CO8", "F", "F1", "F2", "F3", "Fi", "Fi1", "Fi2", "Fi3",
    "Fi4", "Fi5", "Fic", "H", "H1", "H2", "H3", "LV", "LV1", "LV2", "LV3", "MP",
    "MP1", "MP2", "MP3", "N", "N1", "N2", "N3a", "N4", "N4b", "N5", "N5a", "OG",
    "OG1", "OG2", "OG3", "OG4", "OG5", "OG6", "OG7", "PP", "PP1", "PP2", "R",
    "R1", "R2", "R3", "R4", "Sh", "Sh1", "Sh2", "Sh3", "Sh4", "T", "T1", "T2",
    "T3", "T4", "T5", "T6", "V", "V3", "W", "W1", "W2", "W3", "W4", "W5")
}

#' Compositional variable names
#'
#' @return Character vector of compositional variable names
#' @export
comp_vars <- function() {
  c("compositional_format", "compositional_disposition",
    "compositional_cropping", "compositional_viewpoint", "height", "width",
    "is_fragment", "support", "illusionistic_signature", "has_inscribed_date")
}

#' Cross two named lists, preserving names
#'
#' @param l A named list of named lists
#' @param ... Arguments to pass to \link[purrr]{cross_n}
#'
#' @return Returns a named list of named lists with all combinations of the
#'   first limits, their elements, named by combining the original list names
#'   with \code{sep}.
#'
#' @export
cross_named_lists <- function(l, ...) {
  cross_names <- map(l, names) %>%
    cross_n() %>%
    map_chr(paste, collapse = ".")

  cross_n(.l = l, ...) %>%
    set_names(cross_names)
}

#' Run a randomForest model with helpful defaults for this particular dataset
#'
#' This convenience wrapper allows easy specification of different response and
#' predictor variables.
#'
#' @param data Dataframe to model.
#' @param response Quoted name of the response variable column.
#' @param predictors A character vector of predictor variables. This will be
#'   combined with \code{response} in the format \code{as.function(response ~
#'   predictor[1] + predictor[2] + ...)}. The value of \code{response} will be
#'   removed from \code{predictors} if present.
#' @param rownames Quoted name of the column containing names of observations.
#' @param ntree Number of trees to build. Defaults to 2000.
#' @param proximity Whether to save proximity data or not. Defaults to TRUE.
#' @param localImp Whether to save local importance data. Defaults to TRUE.
#' @param ... Other arguments to pass to
#'   \code{\link[randomForest]{randomForest}}
#'
#' @return An object of class \code{randomForest}. See
#'   \code{\link[randomForest]{randomForest}}.
#'
#' @export
run_rf <- function(data, response, predictors, rownames = "painting_code", portion, tests, ntree = 2000, proximity = TRUE, localImp = TRUE, ...) {

  # Produce a dataframe with required columns, rownames, and converting
  # characters to factor (necessary for randomForest to recognize them as
  # categorical data)

  df <- data %>%
    select(one_of(c(response, rownames, setdiff(predictors, response)))) %>%
    prep_vars() %>%
    na.roughfix()

  x <- df %>%
    filter(row_number() %in% portion) %>%
    tibble::column_to_rownames(rownames)
  # Re-factor the response variable so that it has no empty levels (otherwise
  # randomForest will throw a tantrum.)
  y <- x[[response]]
  x[[response]] <- NULL

  tx <- df %>%
    filter(row_number() %in% tests) %>%
    tibble::column_to_rownames(rownames)
  ty <- tx[[response]]
  tx[[response]] <- NULL


  rf <- randomForest::randomForest(x = x, y = y, xtest = tx, ytest = ty, ntree = ntree, proximity = proximity, localImp = localImp, keep.forest = TRUE, keep.inbag = TRUE, ...)
  attr(rf, "response") <- response
  attr(rf, "predictors") <- predictors
  attr(rf, "rownames") <- rownames
  attr(rf, "x") <- x
  attr(rf, "y") <- y
  attr(rf, "tx") <- tx
  attr(rf, "ty") <- ty
  return(structure(rf, class = c("bundledRandomForest", "randomForest")))
}

#' Prepare data frames for model building and prediction
#'
#' Cast strings and logical vectors as factors, and move specified column name to rownames
#'
#' @param x Data frame
#' @param rownames Quoted column name containting rownames. If NULL, no rownames are added.
#'
#' @export
prep_vars <- function(x) {
  x %>%
    purrr::dmap_if(function(x) is.character(x) | is.logical(x), as.factor)
}

# Model access ----

#' @export
rf_response <- function(rf) {
  stopifnot(inherits(rf, "bundledRandomForest"))
  return(attr(rf, "response"))
}

#' @export
rf_predictors <- function(rf) {
  stopifnot(inherits(rf, "bundledRandomForest"))
  return(attr(rf, "predictors"))
}

#' @export
rf_rownames <- function(rf) {
  stopifnot(inherits(rf, "bundledRandomForest"))
  return(attr(rf, "rownames"))
}

#' @export
rf_x <- function(rf) {
  stopifnot(inherits(rf, "bundledRandomForest"))
  return(attr(rf, "x"))
}

#' @export
rf_y <- function(rf) {
  stopifnot(inherits(rf, "bundledRandomForest"))
  return(attr(rf, "y"))
}

#' @export
rf_tx <- function(rf) {
  stopifnot(inherits(rf, "bundledRandomForest"))
  return(attr(rf, "tx"))
}

#' @export
rf_ty <- function(rf) {
  stopifnot(inherits(rf, "bundledRandomForest"))
  return(attr(rf, "ty"))
}

# Internal functions ----
