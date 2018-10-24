# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Compile playlists
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Init
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
config <- read_yaml("config.yaml")

source(config$toolbox, encoding = "UTF-8")

filter <-
  dplyr::filter # voorkom verwarring met stats::filter

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Nipper Express spreadsheet op GD openen
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
library(googlesheets)
# zonodig: change to new user; er opent een browser dialogue
# gs_auth(new_user = TRUE)
gd_nip_xpr <- gs_title(config$nip_xpr_gd_reg)

for (seg1 in 1:1) { # zorgt voor een script-segment dat met "break" verlaten kan worden

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Init config
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  library(RCurl)
  host <- config$host
  home_vt_audio <- case_when(host == "asusp8" ~ config$home_vt_audio.asusp8,
                             host == "marcomac" ~ config$home_vt_audio.marcomac,
                             host == "macserver" ~ config$home_vt_audio.macserver,
                             TRUE ~ "onbekend"
  ) %>% curlEscape %>% str_replace_all(pattern = "\\%2F", replacement = "/")
  
  if(home_vt_audio == "onbekend") {
    print(paste0(host, " is onbekend."))
    break
  }
  
  home_radiologik <- case_when(host == "asusp8" ~ config$home_radiologik.asusp8,
                               host == "marcomac" ~ config$home_radiologik.marcomac,
                               host == "macserver" ~ config$home_radiologik.macserver,
                               TRUE ~ "onbekend"
  ) %>% curlEscape %>% str_replace_all(pattern = "\\%2F", replacement = "/")
  
  home_fonotheek <- case_when(host == "asusp8" ~ config$home_fonotheek.asusp8,
                              host == "marcomac" ~ config$home_fonotheek.marcomac,
                              host == "macserver" ~ config$home_fonotheek.macserver,
                              TRUE ~ "onbekend"
  ) %>% curlEscape %>% str_replace_all(pattern = "\\%2F", replacement = "/")
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Kijk in werkblad "playlists" welke nieuwe playlists er moeten komen
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  pl_nieuw <- gd_nip_xpr %>% 
    gs_read(ws = "playlists") %>% 
    filter(is.na(samengesteld_op), !is.na(playlist))
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Haal de werken op 
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  pl_werken <- gd_nip_xpr %>% 
    gs_read(ws = "nipper-select") %>% 
    filter(playlist %in% pl_nieuw$playlist) %>% 
    filter(!is.na(vt_blok)) %>% 
    # splits de voice-tracking blokken in letter en volgnummer, om bij sorteren te verhinderen 
    # dat blok A10 meteen na blok A1 komt
    mutate(vt_blok_letter = str_sub(vt_blok, start = 1, end = 1), vt_blok_nr = as.integer(str_sub(vt_blok, start = 2))) %>% 
    select(-tot_time, -op_huidige_pl, -keuze, -vt_blok) %>% 
    arrange(playlist, vt_blok_letter, vt_blok_nr)
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Alleen playlists maken waar ook echt wat in staat
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  pl_nieuw %<>% filter(playlist %in% pl_werken$playlist) %>% 
    select(playlist_id, playlist, programma, start, anchor)
  
  if(nrow(pl_nieuw) == 0){
    print("Er zijn geen nieuwe playlists.")
    break
  }
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Alleen playlists maken als alle blokken uniek genummerd zijn
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  dubbele_blokken <- pl_werken %>% 
    group_by(playlist, vt_blok_letter, vt_blok_nr) %>% 
    summarise(n_dubbel = n()) %>% 
    filter(n_dubbel > 1) %>% select(-n_dubbel)
  
  if(nrow(dubbele_blokken) > 0){
    print("Sommige blokken zijn dubbel benoemd")
    unite(data = dubbele_blokken, col = regel, sep = " ")
    break
  }
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Bepaal de playlist lengtes
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  pl_duur <- pl_werken %>% 
    group_by(playlist, vt_blok_letter) %>% 
    summarise(blokduur = sum(lengte)) %>% 
    # blokduur omzetten in seconden: seconds(hms = 00:05:00) = 300S
    #                                as.integer(300S) = 300
    mutate(blokduur_sec = as.integer(seconds(blokduur))) %>% 
    group_by(playlist) %>% 
    summarise(blokken = n(),
              muzieklengte = sum(blokduur_sec)) %>% 
    # 'speling': 00:50 tune+uitzending_aan/af
    #            00:30 minimum aanvulling, 
    #            05:00 maximum aanvulling, 
    #            00:40 presentatie per blok af+aan
    mutate(speling_min = 50 +  30 + 40 * blokken, 
           speling_max = 50 + 300 + 40 * blokken,
           slotlengte = 60 * as.integer(str_sub(playlist, start = 15, end = 17)),
           muziek_min = slotlengte - speling_max,
           muziek_max = slotlengte - speling_min,
           vulling = case_when(muzieklengte > muziek_max ~ paste0("rood (+", muzieklengte - muziek_max, "s)"),
                               muzieklengte > muziek_min ~ "groen",
                               TRUE                      ~ paste0("geel (-", muziek_min - muzieklengte, "s)")
           )
    )
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Haal de tracks op
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  nipper_tracks <- readRDS("resources/nipper_tracks.rds")
  
  pl_tracks <- pl_werken %>% 
    select(playlist, vt_blok_letter, vt_blok_nr, opnameNr) %>% 
    left_join(., nipper_tracks, by = "opnameNr") %>% 
    mutate(uzm_locatie = curlEscape(uzm_locatie),
           uzm_locatie = paste0(home_fonotheek, uzm_locatie),
           uzm_locatie = str_replace_all(uzm_locatie, pattern = "\\%2F", replacement = "/"))
           
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # RL-playlist samenstellen
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  programma_sleutels <- gd_nip_xpr %>% gs_read(ws = "programma_sleutels")
  
  audio_locaties <- gd_nip_xpr %>% gs_read(ws = "audio_locaties")

  source("src/compile_schedulerscript.R")
  
  for (cur_pl in pl_nieuw$playlist) {
    
    cur_duur <- pl_duur %>% filter(playlist == cur_pl) %>% 
      mutate(cur_duur_parm = paste0("Duration:", muzieklengte)) %>% 
      select(cur_duur_parm) %>% 
      unite(col = regel, sep = "\t")

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # audiofile vd tune: staat onder de naam vd tune in audio_locaties; de naam vd tune staat onder de
    # programma-titel in programma_sleutels; de programma-titel staat onder de playlist-naam in pl_nieuw
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    cur_pl_nieuw <- pl_nieuw %>% filter(playlist == cur_pl)
    tune_sleutel <- programma_sleutels %>% filter(programma == cur_pl_nieuw$programma) %>% select(sleutel_tune)
    tune_file <- audio_locaties %>% 
      filter(sleutel == tune_sleutel$sleutel_tune, functie == "tune") %>% 
      select(locatie) %>% 
      mutate(locatie = paste0(home_vt_audio, locatie))
    
    cur_tune <- cur_pl %>% as_tibble %>% 
      mutate(
        duur = "",
        audiofile = paste0("file://localhost", tune_file$locatie),
        const_false = "FALSE",
        start_sec_sinds_middernacht = as.integer(cur_pl_nieuw$start) * 3600,
        fwdtab1 = "",
        fwdtab2 = "",
        fwdtab3 = "",
        speler_regel01 = "tune",
        opname_hfd_sub = "",
        speler_regel02 = "") %>% 
      select(-value) %>% 
      unite(col = regel, sep = "\t")
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # audiofiles vd aan/afkondiging van het programma als geheel: vt_uza_ + <programmatitel> + <aan/af>
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    sleutel_vt_uza <- paste0("vt_uza_", cur_pl_nieuw$programma)
    vt_uza_file <- audio_locaties %>% filter(sleutel == sleutel_vt_uza, functie == "pres_alg_aan") %>% 
      select(locatie) %>% 
      mutate(locatie = paste0(home_vt_audio, locatie)) %>% 
      str_replace_all(pattern = "<sleutel>", replacement = cur_pl_nieuw$programma)
    
    cur_uitzending_aan <- cur_pl %>% as_tibble %>% 
      mutate(
        duur = "",
        audiofile = paste0("file://localhost", vt_uza_file),
        const_false = "FALSE",
        start_sec_sinds_middernacht = -1, # "direct erna afspelen"
        fwdtab1 = "",
        fwdtab2 = "",
        fwdtab3 = "",
        speler_regel01 = "uitzending aan",
        opname_hfd_sub = "",
        speler_regel02 = ""
      ) %>% 
      select(-value) %>% 
      unite(col = regel, sep = "\t")
    
    rlprg_file <- bind_rows(cur_duur, cur_tune, cur_uitzending_aan)
    
    slot <- "slot" %>% as_tibble %>% setNames("vt_blok_letter")
    blokken <- pl_tracks %>% filter(playlist == cur_pl) %>% distinct(vt_blok_letter) %>% 
      bind_rows(slot)
    playlist_id <- pl_nieuw %>% filter(playlist == cur_pl) %>% select(playlist_id) %>% slice(1)
    vt_blok_pad <- audio_locaties %>% filter(sleutel == "vt_blok", functie == "pres_blok") %>% 
      mutate(locatie = paste0(home_vt_audio, locatie)) %>% 
      select(locatie) %>% str_replace_all(pattern = "<playlist-naam>", replacement = cur_pl) 
    
    for (blok in blokken$vt_blok_letter) {
      cur_pres <- cur_pl %>% as_tibble %>% 
        mutate(
          duur = "",
          audiofile = paste0("file://localhost", vt_blok_pad, playlist_id, "_", blok, ".aif"),
          const_false = "FALSE",
          start_sec_sinds_middernacht = -1, # "direct erna afspelen"
          fwdtab1 = "",
          fwdtab2 = "",
          fwdtab3 = "",
          speler_regel01 = "voicetracking",
          opname_hfd_sub = "",
          speler_regel02 = paste0("blok ", blok)
        ) %>% 
        select(-value) %>% 
        unite(col = regel, sep = "\t")
    
      cur_tracks_in_blok <- pl_tracks %>% filter(playlist == cur_pl, vt_blok_letter == blok) %>% 
        left_join(., pl_werken, by = c("playlist", "vt_blok_letter", "vt_blok_nr")) %>% 
        mutate(
          duur = "",
          audiofile = paste0("file://localhost", uzm_locatie),
          const_false = "FALSE",
          start_sec_sinds_middernacht = -1, # "direct erna afspelen"
          fwdtab1 = "",
          fwdtab2 = "",
          fwdtab3 = "",
          speler_regel01 = componist_lbl,
          opname_hfd_sub = paste0(opnameNr.x, "-", opnameVlgNr),
          speler_regel02 = titel
        ) %>% 
        select(duur, audiofile, const_false, start_sec_sinds_middernacht, 
               fwdtab1, fwdtab2, fwdtab3, speler_regel01, opname_hfd_sub, speler_regel02) %>% 
        unite(col = regel, sep = "\t")
      
      rlprg_file %<>% bind_rows(cur_pres, cur_tracks_in_blok)
    }
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # audiofiles vd aan/afkondiging van het programma als geheel: vt_uza_ + <programmatitel> + <aan/af>
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    vt_uza_file <- audio_locaties %>% filter(sleutel == sleutel_vt_uza, functie == "pres_alg_af") %>% 
      select(locatie) %>% 
      mutate(locatie = paste0(home_vt_audio, locatie)) %>% 
      str_replace_all(pattern = "<sleutel>", replacement = cur_pl_nieuw$programma)
    
    cur_uitzending_af <- cur_pl %>% as_tibble %>% 
      mutate(
        duur = "",
        audiofile = paste0("file://localhost", vt_uza_file),
        const_false = "FALSE",
        start_sec_sinds_middernacht = -1, # "direct erna afspelen"
        fwdtab1 = "",
        fwdtab2 = "",
        fwdtab3 = "",
        speler_regel01 = "uitzending af",
        opname_hfd_sub = "",
        speler_regel02 = ""
      ) %>% 
      select(-value) %>% 
      unite(col = regel, sep = "\t")
    
    rlprg_file %<>% bind_rows(cur_uitzending_af)
    
    write.table(x = rlprg_file, file = paste0("resources/playlists/", cur_pl, ".rlprg"), 
                row.names = FALSE, col.names = FALSE, sep = "\t", quote = FALSE, fileEncoding = "UTF-8") 
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    # scheduler-script samenstellen
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    build_rl_script(cur_pl)
  }

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Nieuwe playlists afstempelen met aanmaakdatum. gs_edit_cells zou dat in 1 keer kunnen doen, mits de
  # rijen 1 aaneengesloten blok vormen - wat niet zo is bij nieuwe playlists in het GD-tabblad
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  pl_anchor <- pl_nieuw %>% select(anchor)
  samengesteld_op <- today(tzone = "Europe/Amsterdam")
  
  for (a1 in 1:nrow(pl_nieuw)) {
    cur_anchor <- pl_anchor[a1, ] %>% as.character
    gs_edit_cells(gd_nip_xpr, ws = "playlists", anchor = cur_anchor, input = samengesteld_op)
  }
  
}
