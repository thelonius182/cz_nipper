# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Compile playlist
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Lees welke werken gekozen zijn op GD ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
library(googlesheets)
nip_xpr_GD_reg <- gs_title("Nipper Express") 

gd_playlists <- nip_xpr_GD_reg %>% 
  gs_read(ws = "playlists")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Playlist-referenties samenstellen ----
# In de spreadsheet lopen horizontale en vertikale vectoren door elkaar: bv plid en datum: hor.
#                                                                           opnameNr's: vert. 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
playlist_refs <- gd_playlists %>% 
  filter(str_detect(plid, pattern = "dat|ur|pro")) %>% # refs staan in de rijen 2:4; verwijder de rest
  gather(key = "playlist_id", value = "opnameNr", starts_with("NIP")) %>% # values naar variables (tidy)
  spread(key = plid, value = opnameNr) %>% # transponeer datum/programma/uren van rij naar kolom
  mutate(weekdag = str_sub(as.character(datum), start = 1, end = 2),
         datum_kal = dmy(datum),
         datum_jmd_chr = str_replace_all(as.character(datum_kal), pattern = "-", replacement = "")
  ) %>% 
  separate(col = uren, into = c("van", "tot"), sep = "-") %>% 
  mutate(van = as.integer(van), tot = as.integer(tot), duur = 60 * (tot - van),
         van_chr = formatC(van, width = 2, flag = "0"),
         tot_chr = formatC(tot, width = 2, flag = "0"),
         datum_gd = datum) %>% 
  select(playlist_id, datum_gd, van, tot, duur, programma, datum_kal, datum_jmd_chr, weekdag, van_chr, tot_chr)

playlist_names <- playlist_refs %>% 
  unite(playlist_name, datum_jmd_chr, weekdag, van_chr, tot_chr, programma, sep = "_") %>% 
  select(playlist_id, playlist_name)

playlist_refs <- inner_join(playlist_refs, playlist_names, by = "playlist_id")

rm(playlist_names)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Verzamel opnameNr's per playlist
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
opnameNrs_per_playlist <- gd_playlists[-(1:3), ] %>% # skip reference-info 
  gather(key = "playlist_id", value = "opnameNr", starts_with("NIP"), na.rm = T) %>% 
  mutate(opnameNr = as.integer(opnameNr)) %>% 
  select(-plid)
