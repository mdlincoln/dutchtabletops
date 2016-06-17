library(dplyr)
library(tidyr)
library(stringr)
library(googlesheets)
import::from(plyr, revalue)
library(assertr)

sldb <- gs_title("sldb")

# Read in the original CSV file ----
raw_paintings <- sldb %>% gs_read(ws = "paintings")
raw_motifs <- sldb %>% gs_read(ws = "motifs")
raw_artists <- sldb %>% gs_read(ws = "artists")
dt_artist_attributes <- sldb %>% gs_read(ws = "artist_attributes")

# Create paintings tables ----

# Parse year
dt_paintings <- raw_paintings

# Create motifs tables ----

# A many-to-many table
dt_painting_motifs <- dt_paintings %>%
  select(painting_code, significant_motifs) %>%
  separate(significant_motifs, into = paste0("motif_", 1:7), sep = "; ", extra = "drop", fill = "right") %>%
  gather(code_no, motif_code, contains("motif"), na.rm = TRUE) %>%
  select(-code_no) %>%
  distinct() %>%
  assert(not_na, painting_code, motif_code)

# Create a predicate that returns false if a column has duplicated values
is_unique <- function(x) anyDuplicated(x) == 0

dt_paintings <- dt_paintings %>%
  select(-significant_motifs, -artist) %>%
  assert(not_na, painting_code, compositional_format, compositional_disposition, compositional_cropping, compositional_viewpoint, year_early, year_late, is_fragment, support, illusionistic_signature, has_inscribed_date) %>%
  verify(is_unique(painting_code))

# Unique table of motif codes and their labels
dt_motif_labels <- raw_motifs %>%
  left_join(select(raw_motifs, motif_code, p1_label = motif_label), by = c("p1" = "motif_code")) %>%
  left_join(select(raw_motifs, motif_code, p2_label = motif_label), by = c("p2" = "motif_code")) %>%
  mutate(full_motif_label = ifelse(is.na(p1_label), motif_label, ifelse(is.na(p2_label), paste(p1_label, motif_label, sep = " - "), paste(p1_label, p2_label, motif_label, sep = " - ")))) %>%
  mutate(wrapped_motif_label = str_wrap(full_motif_label, width = 30)) %>%
  select(motif_code, motif_label = full_motif_label, wrapped_motif_label) %>%
  verify(is_unique(motif_code)) %>%
  assert(not_na, motif_code, motif_label, wrapped_motif_label)

# Many to many table of motif codes and their parent values
dt_motif_taxonomy <- raw_motifs %>%
  select(-motif_label) %>%
  gather(parent_no, parent, p1:p2) %>%
  mutate(is_top_level = parent_no == "p1" & is.na(parent)) %>%
  select(-parent_no) %>%
  assert(not_na, motif_code)

# Create artists tables ----

# Make an artist attributes table with life dates
dt_painting_artist <- raw_artists %>%
  select(-artist_string) %>%
  filter(!is.na(artist1)) %>%
  assert(not_na, painting_code, artist1, artist1_relationship) %>%
  verify(is_unique(painting_code))

# This is a compiled table that merges the dt_paintings_, dt_motif_, and
# dt_artist_ tables above into one table with one row per painting, and all
# available variables for that painting.
dt_compiled <- dt_painting_motifs %>%
  left_join(dt_motif_taxonomy, by = "motif_code") %>%
  mutate(parent = ifelse(is.na(parent), motif_code, parent)) %>%
  select(-motif_code, -is_top_level) %>%
  mutate(is_present = TRUE) %>%
  distinct() %>%
  filter(!is.na(parent)) %>%
  spread(key = parent, value = is_present, fill = FALSE, drop = FALSE) %>%
  verify(is_unique(painting_code)) %>%
  inner_join(select(dt_painting_artist, painting_code, artist = artist1, artist_relationship = artist1_relationship), by = "painting_code") %>%
  unite(artist_union, artist, artist_relationship, sep = " - ", remove = FALSE) %>%
  verify(is_unique(painting_code)) %>%
  # Attach compositional data
  inner_join(dt_paintings, by = "painting_code") %>%
  verify(is_unique(painting_code))

# Write all the tables to the package data directory
devtools::use_data(dt_paintings, dt_painting_motifs, dt_motif_labels, dt_motif_taxonomy, dt_painting_artist, dt_artist_attributes, dt_compiled, overwrite = TRUE)
