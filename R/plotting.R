#' Render ggplot while also writing multiple versions to disk
#'
#' @param p Plot object
#' @param filetypes Types of files to save
#' @param filename Filenames
#' @param ... Options to pass to \code{\link[ggplot2]{ggsave}}
#'
#' @export
multiplot <- function(p, filetypes = c("pdf", "svg", "png"), postfix = NULL, filenames = paste0("inst/plots/", filetypes, "/", deparse(substitute(p)), ifelse(is.null(postfix), "", paste0("_", postfix)), ".", filetypes), ...) {
  purrr::walk(filenames, function(x) ggplot2::ggsave(filename = x, plot = p, ...))
  plot(p)
}

#' Error plot ggplot defaults
#'
#' Adds a number of layers to a ggplot base to create a suitable dual-error comparison plot.
#'
#' @param xname The name of the predictor type on the x-axis
#' @param yname The name of the predictor type on the y-axis
#' @param padding Margin of quadrant labels.
#'
#' @import ggplot2
#'
#' @export
theme_error <- function(xname, yname, padding = 0.1, high = 0.9, low = 0.1) {
  high <- 1 - padding
  low <- padding

  list(
    annotate("rect", xmin = 0.5, xmax = Inf, ymin = 0.5, ymax = Inf, alpha = 0.2),
    annotate("rect", xmin = -Inf, xmax = 0.5, ymin = -Inf, ymax = 0.5, alpha = 0.2),
    annotate("text", label = paste0("Unpredictable ", xname, "\nUnpredictable ", yname), x = high, y = high),
    annotate("text", label = paste0("Predictable ", xname, "\nUnpredictable ", yname), x = low, y = high),
    annotate("text", label = paste0("Predictable ", xname, "\nPredictable ", yname), x = low, y = low),
    annotate("text", label = paste0("Unpredictable ", xname, "\nPredictable ", yname), x = high, y = low),
    theme_bw(),
    scale_colour_brewer(palette = "Dark2", guide = FALSE),
    theme(legend.position = "top"),
    xlim(0, 1), ylim(0, 1),
    labs(x = paste0(xname, "-based error"), y = paste0(yname, "-based error")))
}