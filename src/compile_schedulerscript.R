# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Compile scheduler script
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

build_rl_script <- function(playlist) {
  
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Info types samenstellen - zie tabblad "schedule_radiologik"
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # const_0
  sch01_C0 <- "Radiologik Schedule Segment" %>% as_tibble
  
  # dag
  sch01_dag <- rls_dagletter(playlist)  
  
  # lengte
  sch01_lengte <- rls_lengte(playlist)
  
  # dj_voorkeur
  sch01_dj_voorkeur <- "Default" %>% as_tibble
  
  # stuur_naar_dj
  sch01_stuur_naar_dj <- "ProgramTo=0" %>% as_tibble
  
  # start
  sch01_start <- rls_30m_blokken(playlist)
  
  # const_4
  sch01_C4 <- "0" %>% as_tibble
  
  # leeg_1
  sch01_leeg_1 <- "" %>% as_tibble
  
  # venster_van
  sch01_venster_van <- c(rls_venster(playlist, "van"), "0") %>% as_tibble %>% unite(col = regel, sep = "\t")
  
  # venster_tot
  sch01_venster_tot <- c(rls_venster(playlist, "tot"), "0") %>% as_tibble %>% unite(col = regel, sep = "\t")
  
  # const_8
  sch01_C8 <- "ProgramCopyPath=nopath" %>% as_tibble
  
  # color
  # const_10
  # leeg_2
  # const_12
  # const_13
  # const_14
  # const_15
  # const_16
  # const_17
  # const_18
  <- pl_duur %>% filter(playlist == cur_pl) %>% 
    mutate(cur_duur_parm = paste0("Duration:", muzieklengte)) %>% 
    select(cur_duur_parm) %>% 
    unite(col = regel, sep = "\t")

  script_file <- bind_rows(sch01, sch02)
}
