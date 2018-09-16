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
# Isoleer de czID's en tracknummers. Behoud alleen die track-info's waarbij dat lukt.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
uzmTrackInfo %<>% 
  mutate(czID_albumnaam = sub("^.*/C0*(\\d+( ?(-|CD) ?\\d+)?)( |_ )(.*)$", "\\1¶\\5", dir, perl=TRUE)) %>% 
  mutate(diskNr_trackNr = sub("^((\\d{1,3}) |(\\d{1,3}-\\d{1,3}) |(\\d{1,3})-\\D).*$", "\\2\\3\\4", track, perl=TRUE)) %>% 
  filter(track != diskNr_trackNr & dir != czID_albumnaam)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Scheid czID en albumnaam
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
uzmTrackInfo %<>% 
  separate(czID_albumnaam, c("czID", "albumnaam"), sep = "¶")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Splits track-info in 2 stukken: met en zonder standaard czID. Vb. van std: 123, 123-1, 123-10
# Normaliseer vervolgens de non-std czID's en voeg ze weer samen
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
uzmTrackInfo_std <- 
  filter(uzmTrackInfo, str_detect(czID, "^\\d+(-\\d+)?$"))

uzmTrackInfo_nonStd <- 
  filter(uzmTrackInfo, !str_detect(czID, "^\\d+(-\\d+)?$")) %>% 
  mutate(czID = str_replace_all(czID, pattern = " ", replacement = ""),
         czID = str_replace_all(czID, pattern = "CD", replacement = "-")) 

uzmTrackInfo <- bind_rows(uzmTrackInfo_std, uzmTrackInfo_nonStd)

rm(uzmTrackInfo_std, uzmTrackInfo_nonStd)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Albumnaam verfraaien
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
uzmTrackInfo %<>% 
  mutate(albumnaam = str_replace_all(albumnaam, pattern = "_", replacement = " "))

# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
# Knip het schijfnummer uit de albumnaam. 
# 
# Voorbeeld: v1 <- getDiskNr_in_albumNaam(c("CD-1", "Beethoven", "cd2", "Fantasy [Disc 3]"))
#            v1 : "1" NA  "2" "3"
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
getDiskNr_in_albumNaam <- function(albumnaam){
  result <- sub("^.*(cd[- ]?(\\d+)|dis[ck] ?(\\d+)).*$", "\\2\\3", albumnaam, perl=TRUE, ignore.case=TRUE)
  ifelse(result == albumnaam, NA, result)
}

uzmTrackInfo %<>% 
  mutate(diskNr_in_albumnaam = getDiskNr_in_albumNaam(albumnaam))
