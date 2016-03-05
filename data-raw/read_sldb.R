library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(googlesheets)
import::from(plyr, revalue)

sldb <- gs_title("sldb")

# Read in the original CSV file ----
raw_paintings <- sldb %>% gs_read(ws = "paintings")
raw_motifs <- sldb %>% gs_read(ws = "motifs")
raw_artists <- sldb %>% gs_read(ws = "artists")
dt_artist_attributes <- sldb %>% gs_read(ws = "artist_attributes")

# Create paintings tables ----

# Parse year
dt_paintings <- raw_paintings %>%
  mutate(
    # Extract the first 4 digit string and convert to integer
    year = as.integer(str_extract(date_string, "\\d{4}")),
    # Create a logical column indicating modifiers like c., mid, early, or late
    is_approx_date = str_detect(date_string, "c\\.") |
      str_detect(date_string, "mid") |
      str_detect(date_string, "early") |
      str_detect(date_string, "late")
  )

# Create motifs tables ----

# A many-to-many table
dt_painting_motifs <- dt_paintings %>%
  select(painting_code, significant_motifs) %>%
  separate(significant_motifs, into = paste0("motif_", 1:7), sep = "; ", extra = "drop", fill = "right") %>%
  gather(code_no, motif_code, contains("motif"), na.rm = TRUE) %>%
  select(-code_no)

dt_paintings <- dt_paintings %>% select(-significant_motifs)

# Unique table of motif codes and their labels
dt_motif_labels <- raw_motifs %>% select(motif_code, motif_label)

# Many to many table of motif codes and their parent values
dt_motif_taxonomy <- raw_motifs %>%
  select(-motif_label) %>%
  gather(parent_no, parent, p1:p2) %>%
  select(-parent_no)

# Create artists tables ----

# Make an artist attributes table with life dates
dt_painting_artist <- bind_rows(
  select(raw_artists, painting_code, artist = artist1, artist_relationship = artist1_relationship),
  select(raw_artists, painting_code, artist = artist2, artist_relationship = artist2_relationship),
  select(raw_artists, painting_code, artist = artist3, artist_relationship = artist3_relationship)
) %>%
  filter(!is.na(artist) & !is.na(artist_relationship)) %>%
  distinct()


devtools::use_data(dt_paintings, dt_painting_motifs, dt_motif_labels, dt_motif_taxonomy, dt_painting_artist, dt_artist_attributes, overwrite = TRUE)
