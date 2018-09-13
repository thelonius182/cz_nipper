# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# Geïmporteerde track-info van de audiofiles op de uitzendmac schoonmaken
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Packages 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(magrittr)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Filter is een overloaded function; neem standaard die uit dplyr
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
filter <- dplyr::filter

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Lees track-info, geïmporteerd uit Uitzendmac
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
uzmTrackInfo <-
  read_delim(
    "F:/cz_salsa_output/tree_walker/archief01_tracks.txt",
    "\t",
    escape_double = FALSE,
    trim_ws = TRUE,
    col_names = c("track", "dir", "lengte")
  )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Track-info van klassieke albums verwijderen als ze uit de Wereldmuziek-folder. 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
uzmTrackInfo %<>% 
  filter(str_detect(dir, pattern = "^((?!\\!WERELDMUZIEK).)*$")) %>%
  arrange(dir, track)
