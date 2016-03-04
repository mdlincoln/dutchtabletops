#' Dutch tabletop paintings
#'
#' These paintings
#'
#' @format A data frame with 629 rows and 24 variables:
#' \describe{
#'   \item{\code{painting_code}}{character. Unique ID.}
#'   \item{\code{artist}}{character. Attributed artist. See \code{is_after} and \code{is_uncertain}.}
#'   \item{\code{is_after}}{logical. Is the artwork after the listed artist?}
#'   \item{\code{artist_is_uncertain}}{logical. Is the artist attribution uncertain?}
#'   \item{\code{painting_description}}{character. Free text description of the painting.}
#'   \item{\code{thematic_format}}{character. ###}
#'   \item{\code{compositional_format}}{character. ###}
#'   \item{\code{compositional_disposition}}{character. ###}
#'   \item{\code{compositional_cropping}}{character. ###}
#'   \item{\code{compositional_viewpoint}}{character. ###}
#'   \item{\code{significant_motifs}}{character. ###}
#'   \item{\code{location}}{character. ###}
#'   \item{\code{bibliography}}{character. ###}
#'   \item{\code{date_string}}{character. ###}
#'   \item{\code{year_early}}{character. ###}
#'   \item{\code{year_late}}{character. ###}
#'   \item{\code{dimensions}}{character. ###}
#'   \item{\code{height}}{double. ###}
#'   \item{\code{width}}{double. ###}
#'   \item{\code{is_fragment}}{logical. ###}
#'   \item{\code{support}}{character. ###}
#'   \item{\code{inscriptions}}{character. ###}
#'   \item{\code{illusionistic_signature}}{logical. ###}
#'   \item{\code{has_inscribed_date}}{logical. ###}
#' }
"dt_paintings"

#' Dutch tabletop motif labels
#'
#' DATASET DETAILS
#'
#' @format A data frame with 197 rows and 2 variables:
#' \describe{
#'   \item{\code{motif_code}}{character. ###}
#'   \item{\code{motif_label}}{character. ###}
#' }
"dt_motif_labels"

#' DATASET TITLE
#'
#' DATASET DETAILS
#'
#' @format A data frame with 394 rows and 2 variables:
#' \describe{
#'   \item{\code{motif_code}}{character. ###}
#'   \item{\code{parent}}{character. ###}
#' }
"dt_motif_taxonomy"

#' DATASET TITLE
#'
#' DATASET DETAILS
#'
#' @format A data frame with 1572 rows and 2 variables:
#' \describe{
#'   \item{\code{painting_code}}{character. ###}
#'   \item{\code{motif_code}}{character. ###}
#' }
"dt_painting_motifs"
