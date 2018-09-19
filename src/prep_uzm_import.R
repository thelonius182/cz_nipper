# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# Geïmporteerde track-info van de audiofiles op de uitzendmac schoonmaken
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Lees track-info Uitzendmac ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
uzmTrackInfo <- read_delim("resources/archief01_tracks.txt",
                           "\t",
                           escape_double = FALSE,
                           trim_ws = TRUE,
                           col_names = c("track", "dir", "lengte")
                           ) %>% 
  arrange(dir, track)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Skip audiofiles in dir Wereldmuziek ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
uzmTrackInfo %<>% 
  filter(!str_detect(dir, pattern = "/.*WERELDMUZIEKARCHIEF/"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Splits dir: catalogusNr (+ disk_volgNr) + ¶ + album ----
# Skip track-info's waarbij dat niet lukt.
# 
# Bv. "/Volumes/cz archief 01/C01143-2 L_Oeuvre Integral Pour Piano (Setrak) 2" 
#     dir_splits1 = "1143-2¶L_Oeuvre Integral Pour Piano (Setrak) 2"
#     dir_splits2 = "1143-2", album = "L_Oeuvre Integral Pour Piano (Setrak) 2"
#     vervolgens: catNr = "1143", diskNr_dir = "2" 
#     NB - er zitten ook diskNr's in album en track, vandaar hier de sfx "_dir".
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
uzmTrackInfo %<>% 
  mutate(dir_splits1 = sub("^.*/C0*(\\d+( ?(-|CD) ?\\d+)?)( |_ )(.*)$", "\\1¶\\5", dir, perl=TRUE)) %>% 
  filter(dir_splits1 != dir) %>% 
  separate(dir_splits1, c("dir_splits2", "album"), sep = "¶")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Harmoniseer catalogusnummer en schijfnummer in de naam vd uitzendmac-directory
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
uzmTrackInfo %<>% 
  mutate(catdskNr = harmoniseer_catdskNr(dir_splits2)) %>% 
  separate(catdskNr, c("catNr", "diskNr_dir"), sep = "¶") %>% 
  select(-dir_splits2)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Knip diskNrs uit albumnaam ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
uzmTrackInfo %<>% 
  mutate(diskNr_album = getDiskNr_in_albumNaam(album))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Knip disk/trackNr's uit de tracknaam ----
# Behoud alleen die track-info's waarbij dat lukt.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
uzmTrackInfo %<>% 
  mutate(disk_track_combi = sub("^(\\d{1,3}(-\\d{1,3})?).*$", "\\1", track, perl=TRUE)) %>% 
  filter(disk_track_combi != track)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Albumnaam verfraaien
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
uzmTrackInfo %<>% 
  mutate(albumnaam = str_replace_all(albumnaam, pattern = "_", replacement = " "))