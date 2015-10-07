library(dplyr)
library(readr)
library(tidyr)
library(stringr)
import::from(plyr, revalue)

# Read in the original CSV file ----

sl_names <- c(
  "painting_code",
  "artist",
  "painting_description",
  "thematic_format",
  "compositional_format",
  "compositional_disposition",
  "compositional_cropping",
  "compositional_viewpoint",
  "significant_motifs",
  "location",
  "bibliography",
  "date",
  "dimensions",
  "support",
  "inscriptions"
)

raw_sldb <- read_csv(system.file("extdata", "sldb.csv", package = "dutchtabletops"), skip = 1, col_names = sl_names)
devtools::use_data(raw_sldb, overwrite = TRUE)

sldb <- raw_sldb

# Parse dimensions

sldb <- sldb %>%
  mutate(
    # Fix one badly-formatted dimension cell
    dimensions = revalue(dimensions, replace = c("29.5 39 cm (oval)" = "29.5 x 39 cm (oval)")),
    height = str_match(dimensions, "(\\d+\\.?\\d*) x")[,2] %>% as.numeric(),
    width = str_match(dimensions, "x (\\d+\\.?\\d*)")[,2] %>% as.numeric(),
    # Dimensions can be modified in one of two ways - either the work is an
    # oval, or it is a fragment
    dim_mod = ifelse(
      str_detect(dimensions, "oval"),
      "oval",
      ifelse(
        str_detect(dimensions, "cut") | str_detect(dimensions, "fragment"),
        "fragment",
        NA
      )
    )
  )

sldb %>% filter(is.na(height)) %>% View()

# Parse year

sldb <- sldb %>%
  mutate(
    # Fix two oddly formatted years
    date = revalue(date, replace = c(
      "161(5 or 6)" = "1615",
      "mid-17th cent." = "1650"
    )),
    # Extract the first 4 digit string and convert to integer
    year = str_extract(date, "\\d{4}") %>% as.integer(),
    # Create a logical column indicating modifiers like c., mid, early, or late
    is_approx_date = str_detect(date, "c\\.") |
      str_detect(date, "mid") |
      str_detect(date, "early") |
      str_detect(date, "late")
  )

# Create motifs table ----

sl_motifs <- sldb %>%
  select(painting_code, significant_motifs) %>%
  separate(significant_motifs, into = paste0("motif_", 1:7), sep = "; ", extra = "drop") %>%
  gather(code_no, code_string, contains("motif"), na.rm = TRUE) %>%
  select(-code_no) %>%
  mutate(ex_code = str_match(code_string, "\\((\\S+)\\)")[,2])


devtools::use_data(sldb, sl_motifs)

