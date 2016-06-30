#' Render ggplot while also writing multiple versions to disk
#'
#' @param p Plot object
#' @param filetypes Types of files to save
#' @param filename Filenames
#' @param ... Options to pass to \code{\link[ggplot2]{ggsave}}
#'
#' @export
multiplot <- function(p, filetypes = c("pdf", "svg", "png"), filenames = paste0("inst/plots/", filetypes, "/", deparse(substitute(p)), ".", filetypes), ...) {
  purrr::walk(filenames, function(x) ggplot2::ggsave(filename = x, plot = p, ...))
  plot(p)
}
