# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Geïmporteerde track-info uit Filemaker schoonmaken
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Lees composities, geïmporteerd uit DB-klassiek ----
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

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# czID's schoonmaken ----
#   schoonmaken, dan classificeren om de probleemgevallen te kunnen opsporen
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
filemakerTrackInfo %<>% 
  mutate(czid  = str_replace_all(czid, pattern = "\\.$",  replacement = ""), # EINDIGT op punt! 
         czid  = str_replace_all(czid, pattern = "[ .,/:;]", replacement = "-"),
         czid2 = str_replace_all(czid, pattern = "\\d", replacement = "0")
  )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Schone compositielijst maken ----
# Verzamel de unieke czid-types + aantal. Behoud alleen composities met een geldig type (in czID[1:3]) 
# of een trackreeks-met-schijfnummer waarmee een ongeldig type te repareren valt.
#
#   czID[1:3]  type       aantal
#'  ---------  ---------  ------
#   1          C00000     124160
#   2          C00000-0   703
#   3          C00000-00  123
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
count_czid_type <- count(filemakerTrackInfo, czid2, sort = TRUE) # dwz sort by "n"!

filemakerTrackInfo %<>% 
  filter(czid2 %in% count_czid_type$czid2[1:3] | str_detect(tracks, pattern = ":"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Schijfnummers uitknippen ----
# czID splitsen in catNr en diskNr
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
filemakerTrackInfo %<>% 
  mutate(czid = harmoniseer_catDskNr_in_FM_czID(czid)) %>% 
  separate(czid, c("catNr", "diskNr_czid"), sep = " ")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# tracks splitsen in diskNr, trackBeg, trackEnd
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
filemakerTrackInfo %<>% 
  mutate(tracks_splits1 = harmoniseer_catTrcks_in_FM_tracks(tracks)) %>% 
  separate(tracks_splits1, c("diskNr_tracks", "trackBeg", "trackEnd"), sep = "¶")

