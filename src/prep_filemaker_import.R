# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Geïmporteerde track-info uit Filemaker schoonmaken
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Packages
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# library(readr)
# library(stringr)
# library(dplyr)
# library(tidyr)
# library(magrittr)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Filter is een overloaded function; neem standaard die uit dplyr.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# filter <- dplyr::filter

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Lees composities, geïmporteerd uit DB-klassiek 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
filemakerTrackInfo <-
  read_delim(config$import_filemaker,
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


