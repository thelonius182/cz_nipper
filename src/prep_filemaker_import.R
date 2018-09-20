# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Geïmporteerde track-info uit Filemaker schoonmaken
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Lees composities, geïmporteerd uit DB-klassiek 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
filemakerTrackInfo <-
  read_delim("resources/export_filemaker_20180523.tab",
             "\t",
             escape_double = FALSE,
             trim_ws = TRUE
  ) %>% 
  filter(!is.na(`cz-catalogusnummer`) 
         & !is.na(tracknummers) 
         & !is.na(componist)
         & !str_detect(componist, "-|--|\\(")
         & str_detect(`cz-catalogusnummer`, "^C\\d+")
  ) %>% 
  select(
    componist,
    titel,
    czid = `cz-catalogusnummer`,
    album = `cd-of-lp-naam`,
    tracks = tracknummers,
    hh = h,
    mm = m,
    ss = s,
    bezetting,
    uitvoerenden = `uitvoerenden-1`,
    label,
    labelcode
  ) %>% 
  arrange(
      componist,
      titel,
      czid,
      tracks
  )

filemakerTrackInfo_czid_type <- filemakerTrackInfo %>% 
  mutate(czid2 = str_replace_all(czid, pattern = "\\d", replacement = "0"),
         czid2 = str_replace_all(czid2, pattern = "\\.$",  replacement = ""),
         czid2 = str_replace_all(czid2, pattern = " ", replacement = ""),
         czid2 = str_replace_all(czid2, pattern = "[.,/:;]",  replacement = "-")) %>% 
  group_by(czid2)
  

str(filemakerTrackInfo_czid_type)
