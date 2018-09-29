# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Compile playlist
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Lees geselecteerde werken op GD ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
library(googlesheets)
nip_xpr_GD_reg <- gs_title("Nipper Express") 

gd_playlists <- nip_xpr_GD_reg %>% 
  gs_read(ws = "playlists")

gaco_pl_gathered <- gd_playlists %>% 
  gather(key = datum, value = opnameNr, na.rm = TRUE) %>% 
  mutate(datum = dmy(datum))

playlist_opnameNrs <- gaco_pl_gathered %>% 
  filter(datum == "2018-10-04" & !str_detect(opnameNr, pattern = "-")) %>% 
  mutate(opnameNr = as.integer(opnameNr)) %>% 
  select(opnameNr)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Lees tracks van geselecteerde werken ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
playlist_tracks <- left_join(playlist_opnameNrs, nipper_track, by = "opnameNr")

playlist_werken <- left_join(playlist_opnameNrs, nipper_werk, by = "opnameNr")
