#' Dutch Tabletop Paintings
#'
#' A set of 629 paintings made between by Haarlem-based artists.
#'
#' @seealso \link{dt_painting_motifs}, \link{dt_motif_taxonomy},
#'   \link{dt_artist_attributes}, \link{dt_painting_artist}.
#'
#' @format A data frame with 629 rows and 20 variables: \describe{
#'   \item{\code{painting_code}}{character. Unique identifier for each painting.}
#'   \item{\code{painting_description}}{character. A free-text description of the format and content of the painting.}
#'   \item{\code{thematic_format}}{character. A general category for the type of content in the painting, e.g. "Meal w/ One Main Dish", "Tabackjes".}
#'   \item{\code{compositional_format}}{character. Horizontal or vertical.}
#'   \item{\code{compositional_disposition}}{character. Category of the compositional scheme, e.g. "off-centered pyramid", "right wedge", "left wedge".}
#'   \item{\code{compositional_cropping}}{character. The amount of space around the main tabletop surface, may either be paritcularly "tight", "normal", or particularly "expanded"}
#'   \item{\code{compositional_viewpoint}}{character. The viewpoint height over the table: "low", "normal", or "high"}
#'   \item{\code{location}}{character. The colleciton or institution where the painting currently resides.}
#'   \item{\code{bibliography}}{character. Bibliography citing the painting.}
#'   \item{\code{date_string}}{character. A free-text date of creation.}
#'   \item{\code{year_early}}{integer. The earliest year of the paitning.}
#'   \item{\code{year_late}}{integer. The latest year of the painting.}
#'   \item{\code{dimensions}}{character. Free-text field with the dimensions of the painting.}
#'   \item{\code{height}}{double. Height in centimeters.}
#'   \item{\code{width}}{double. Width in centimeters.}
#'   \item{\code{is_fragment}}{logical. Is the painting a fragment of a larger original?}
#'   \item{\code{support}}{character. Is the painting on canvas or wood panel?}
#'   \item{\code{inscriptions}}{character. Free-text field transcribing any inscriptions on the painting.}
#'   \item{\code{illusionistic_signature}}{logical. Does the painting have an illusionistic signature - one that has been painted onto an object as if it is part of the depicted scene?}
#'   \item{\code{has_inscribed_date}}{logical. Does the painting have a date in the inscription.}
#'}
#'
#' @source Gregory, Henry Duval. "Tabletop Still Lifes in Haarlem, C. 1610-1660: A Study of the Relationships Between Form and Meaning." PhD diss., University of Maryland, 2003. \url{http://search.proquest.com/docview/305326380}


"dt_paintings"

#' Dutch tabletop motif labels
#'
#' @format A data frame with 197 rows and 2 variables:
#' \describe{
#'   \item{\code{motif_code}}{character. A unique motif identifier.}
#'   \item{\code{motif_label}}{character. An English label for the motif code.}
#' }
"dt_motif_labels"

#' Motif Taxonomy
#'
#' @format A data frame with 394 rows and 2 variables:
#' \describe{
#'   \item{\code{motif_code}}{character. A unique motif identifier.}
#'   \item{\code{parent}}{character. The parent motif. For example, the motif code \code{Sh1a} (Oysters as a main course) is a child of \code{Sh1} (Oysters), and \code{Sh} (Shellfish).}
#' }
"dt_motif_taxonomy"

#' DATASET TITLE
#'
#' DATASET DETAILS
#'
#' @format A data frame with 1572 rows and 2 variables:
#' \describe{
#'   \item{\code{painting_code}}{character}
#'   \item{\code{motif_code}}{character}
#' }
"dt_painting_motifs"

#' Artist Attributes
#'
#' @format A data frame with 37 rows and 5 variables:
#' \describe{
#'   \item{\code{artist}}{character. Artist name.}
#'   \item{\code{birth_early_year}}{integer. Earliest year of artist's birth.}
#'   \item{\code{birth_late_year}}{integer. Latest year of artist's birth.}
#'   \item{\code{death_early_year}}{integer. Earliest year of artist's death.}
#'   \item{\code{death_late_year}}{integer. Latest year of artist's death.}
#' }
"dt_artist_attributes"

#' Artist-Painting relationships
#'
#' @format A data frame with 120 rows and 3 variables:
#' \describe{
#'   \item{\code{painting_code}}{character. Join with \link{dt_paintings} \code{painting_code}.}
#'   \item{\code{artist}}{character. Join with \link{dt_artist_attributes} \code{artist}.}
#'   \item{\code{artist_relationship}}{character. The type of relationship: "Definite", "After", "Possible", "Co-painter".}
#' }
"dt_painting_artist"
