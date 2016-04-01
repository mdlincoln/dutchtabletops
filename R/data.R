#' DATASET TITLE
#'
#' DATASET DESCRIPTION
#'
#' @format A data frame with 629 rows and 23 variables:
#' \describe{
#'   \item{\code{painting_code}}{character. DESCRIPTION.}
#'   \item{\code{artist}}{character. DESCRIPTION.}
#'   \item{\code{painting_description}}{character. DESCRIPTION.}
#'   \item{\code{thematic_format}}{character. DESCRIPTION.}
#'   \item{\code{compositional_format}}{character. DESCRIPTION.}
#'   \item{\code{compositional_disposition}}{character. DESCRIPTION.}
#'   \item{\code{compositional_cropping}}{character. DESCRIPTION.}
#'   \item{\code{compositional_viewpoint}}{character. DESCRIPTION.}
#'   \item{\code{location}}{character. DESCRIPTION.}
#'   \item{\code{bibliography}}{character. DESCRIPTION.}
#'   \item{\code{date_string}}{character. DESCRIPTION.}
#'   \item{\code{year_early}}{integer. DESCRIPTION.}
#'   \item{\code{year_late}}{integer. DESCRIPTION.}
#'   \item{\code{dimensions}}{character. DESCRIPTION.}
#'   \item{\code{height}}{double. DESCRIPTION.}
#'   \item{\code{width}}{double. DESCRIPTION.}
#'   \item{\code{is_fragment}}{logical. DESCRIPTION.}
#'   \item{\code{support}}{character. DESCRIPTION.}
#'   \item{\code{inscriptions}}{character. DESCRIPTION.}
#'   \item{\code{illusionistic_signature}}{logical. DESCRIPTION.}
#'   \item{\code{has_inscribed_date}}{logical. DESCRIPTION.}
#'   \item{\code{year}}{integer. DESCRIPTION.}
#'   \item{\code{is_approx_date}}{logical. DESCRIPTION.}
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

#' DATASET TITLE
#'
#' DATASET DESCRIPTION
#'
#' @format A data frame with 37 rows and 5 variables:
#' \describe{
#'   \item{\code{artist}}{character. DESCRIPTION.}
#'   \item{\code{birth_early_year}}{integer. DESCRIPTION.}
#'   \item{\code{birth_late_year}}{integer. DESCRIPTION.}
#'   \item{\code{death_early_year}}{integer. DESCRIPTION.}
#'   \item{\code{death_late_year}}{integer. DESCRIPTION.}
#' }
"dt_artist_attributes"

#' DATASET TITLE
#'
#' DATASET DESCRIPTION
#'
#' @format A data frame with 120 rows and 3 variables:
#' \describe{
#'   \item{\code{painting_code}}{character. DESCRIPTION.}
#'   \item{\code{artist}}{character. DESCRIPTION.}
#'   \item{\code{artist_relationship}}{character. DESCRIPTION.}
#' }
"dt_painting_artist"
