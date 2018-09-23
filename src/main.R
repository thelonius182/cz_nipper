# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# cz_nipper MAIN ----
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Init ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
filter <-
  dplyr::filter # don't use stats::filter unless so qualified

config <- read_yaml("config.yaml")

source(config$toolbox, encoding = "UTF-8")
source("src/prep_uzm_import.R", encoding = "UTF-8")
source("src/prep_filemaker_import.R", encoding = "UTF-8")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Filemaker en audiofiles koppelen
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
uzm_fm_vuil <-
  left_join(uzmTrackInfo,
            filemakerTrackInfo,
            by = c("catNr",
                   "diskNr",
                   "trackNr" = "trackBeg"))

uzm_fm_bruikbaar <- uzm_fm_vuil %>%
  filter(trackNr == 1 & !is.na(trackEnd)) %>%
  distinct(catNr, diskNr) %>%
  mutate(heeftMetadata = TRUE)

uzm_fm_schoon <-
  left_join(uzm_fm_vuil, uzm_fm_bruikbaar, by = c("catNr", "diskNr")) %>%
  filter(heeftMetadata) %>%
  select(
    opnameNr,
    catNr,
    diskNr,
    trackBeg = trackNr,
    trackEnd,
    componist,
    titel,
    lengte,
    album,
    bezetting,
    uitvoerenden,
    uzm_locatie,
    label,
    labelcode
  )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# OpnameNr's completeren & kolommen vervangen/hernoemen
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
num_sav <- NULL
i1 <- 0
onr_compl <- NULL

for (num in uzm_fm_schoon$opnameNr) {
  i1 <- i1 + 1
  
  if (!is.na(num)) {
    num_sav <- num
  }
  
  onr_compl[i1] <- num_sav
}

uzm_fm_schoon %<>% select(-opnameNr, -trackEnd)

uzm_fm_schoon <- bind_cols(as.data.frame(onr_compl), uzm_fm_schoon)

uzm_fm_schoon %<>% rename(opnameNr = onr_compl, trackNr = trackBeg)

rm(uzm_fm_bruikbaar, uzm_fm_vuil, count_czid_type)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Opnamelengtes berekenen
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
opnamelengte <- uzm_fm_schoon %>% 
  group_by(opnameNr) %>% 
  summarise(tot_lengte = sum(lengte))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Opnamelengtes berekenen
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

