# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# =                                                M A I N
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Init
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
config <- read_yaml("config.yaml")

source(config$toolbox, encoding = "UTF-8")

filter <-
  dplyr::filter # don't use stats::filter unless so qualified

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Er valt alleen iets te preppen als er een nieuwe import van uitzendmac of filemaker is
# <TODO>analyseer de file-properties om dit vast te stellen</TODO>
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
if (config$uitzendmac_gewijzigd | config$filemaker_gewijzigd) {
  source("src/prep_all.R", encoding = "UTF-8")
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Lees gematchte Componisten op GD ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
library(googlesheets)
nip_componisten_GD_reg <- gs_title("Nipper Componisten") 

gd_componisten <- nip_componisten_GD_reg %>% 
  gs_read(ws = "componisten")

gd_componisten_db <- nip_componisten_GD_reg %>% 
  gs_read(ws = "db_componisten") %>% 
  select(-starts_with("X"), -starts_with("peil"), -starts_with("land"), -delta) %>% 
  filter(!is.na(id))

ref_componisten <- left_join(gd_componisten, gd_componisten_db, by = "componist_key")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Voeg componist-data toe aan track-info ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
nipper <- left_join(uzm_fm_schoon, ref_componisten, by = c("componist" = "componist_FM")) %>% 
  select(
    opnameNr,
    catNr,
    diskNr,
    trackNr,
    tijdvak,
    nationaliteit,
    componist_key,
    componist_lbl,
    titel,
    bezetting,
    uitvoerenden,
    lengte,
    album,
    label,
    labelcode,
    van,
    tot,
    uzm_locatie,
    -id,
    - componist
  ) %>% 
  arrange(
    opnameNr,
    catNr,
    diskNr,
    trackNr
  ) %>% 
  mutate(trackNr = factor(trackNr, levels = unique(trackNr))) %>% 
  group_by(opnameNr) %>% 
  mutate(opnameVlgNr = dense_rank(trackNr)) 

nipper_werk_links <- nipper %>% 
  select(
    opnameNr,
    opnameVlgNr,
    tijdvak,
    nationaliteit,
    componist_key,
    componist_lbl,
    titel,
    bezetting,
    uitvoerenden,
    album
  ) %>% 
  filter(opnameVlgNr == 1) %>% 
  select(-opnameVlgNr)

nipper_werk <- left_join(nipper_werk_links, opnamelengte, by = "opnameNr") %>% 
  select(
    opnameNr,
    tot_lengte_in_seconden,
    tijdvak,
    nationaliteit,
    componist_key,
    componist_lbl,
    titel,
    bezetting,
    uitvoerenden,
    album
  ) %>% 
  mutate(lengte = as.character(hms::as.hms(round(tot_lengte_in_seconden, digits = 0)))) %>% 
  select(-tot_lengte_in_seconden)

nipper_track <- nipper %>% 
  select(
    opnameNr,
    opnameVlgNr,
    uzm_locatie
  )

nipper_todo <- nipper_werk %>% filter(tijdvak == "?") %>% 
  group_by(componist_key) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n))

nipper_werk %<>% 
  filter(tijdvak != "?") %>% 
  select(tijdvak,
         componist_lbl,
         nationaliteit,
         titel,
         lengte,
         bezetting,
         uitvoerenden,
         album,
         opnameNr
  ) %>% 
  mutate(tijdvak = factor(tijdvak, levels = c("Middeleeuwen",
                                              "Renaissance",
                                              "Barok",
                                              "Klassiek",
                                              "Romantiek",
                                              "Modern")
                          )
  ) %>% 
  arrange(tijdvak,
          componist_lbl,
          titel,
          lengte
  ) 
