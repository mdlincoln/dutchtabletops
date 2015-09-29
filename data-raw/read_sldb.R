library(dplyr)
library(readr)
library(tidyr)
library(stringr)

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
#devtools::use_data(raw_sldb, overwrite = TRUE)

sldb <- raw_sldb

# Parse dimensions

sldb <- sldb %>%
  mutate(
    height = str_match(dimensions, "(\\d+\\.?\\d*) x")[,2] %>% as.numeric(),
    width = str_match(dimensions, "x (\\d+\\.?\\d*)")[,2] %>% as.numeric()
    )

# Parse year

sldb <- sldb %>%
  mutate(
    year = str_extract(date, "\\d{4}") %>% as.integer()
  )

# Create motifs table ----

sl_motifs <- sldb %>%
  select(painting_code, significant_motifs) %>%
  separate(significant_motifs, into = paste0("motif_", 1:7), sep = "; ", extra = "drop") %>%
  gather(code_no, code_string, contains("motif"), na.rm = TRUE) %>%
  select(-code_no) %>%
  mutate(ex_code = str_match(code_string, "\\((\\S+)")[,2])

sldb <- sldb %>% select(-significant_motifs)

devtools::use_data(sldb, sl_motifs)
