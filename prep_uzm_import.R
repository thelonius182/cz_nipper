# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# Geïmporteerde track-info van de audiofiles op de uitzendmac schoonmaken
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Packages 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(magrittr)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Filter is een overloaded function; neem standaard die uit dplyr.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
filter <- dplyr::filter

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Lees track-info, geïmporteerd uit Uitzendmac met de java treewalker.
# Sorteer weer op foldernaam/tracknaam; tijdens exporteren zijn ze door elkaar gegooid.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
uzmTrackInfo <-
  read_delim(
    "F:/cz_salsa_output/tree_walker/archief01_tracks.txt",
    "\t",
    escape_double = FALSE,
    trim_ws = TRUE,
    col_names = c("track", "dir", "lengte")
  ) %>% 
  arrange(dir, track)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Track-info verwijderen van cd's die uit de Wereldmuziek-folder komen. Dat zijn Wereldmuziek-cd's die
# met een verkeerde czID in de CZ-fonotheek gezet zijn (dwz als C[nummer] ipv CW[nummer]) of Klassieke
# cd's die onterecht in de Wereldmuziek-folder staan.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
uzmTrackInfo %<>% 
  filter(str_detect(dir, pattern = "^((?!\\!WERELDMUZIEK).)*$")) # negative lookahead

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Isoleer de "happy case" czID's en tracknummers.
# In de "sad case" gaat het om een cd-box; schijfnummers daarvan zijn niet eenduidig geregistreerd in
# Filemaker. Een cd-box wordt daarom niet meegenomen.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
uzmTrackInfo %<>% 
  mutate(czID = sub("^.*/C((\\d+(-\\d{1,2})?)|(\\d+)[ _]).*$", "\\2", dir, perl=TRUE)) %>% 
  mutate(trackNr = sub("^((\\d{1,3}) |(\\d{1,3}-\\d{1,3}) |(\\d{1,3})-\\D).*$", "\\2\\3\\4", track, perl=TRUE)) 
  # filter(nchar(czID) < 7 | nchar(trackNr) < 7)

