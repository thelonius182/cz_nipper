library(googledrive)
library(keyring)
library(readxl)
library(yaml)

cz_extract_sheet <- function(ss_name, sheet_name) {
  read_xlsx(ss_name,
            sheet = sheet_name,
            .name_repair = ~ ifelse(nzchar(.x), .x, LETTERS[seq_along(.x)]))
}

cz_get_url <- function(cz_ss) {
  cz_url <- paste0("url_", cz_ss)
  paste0("https://", config$url_pfx, config[[cz_url]])
}

# config <- read_yaml("config.yaml")

# downloads GD ----
# aanmelden bij GD loopt via de procedure die beschreven is in "Operation Missa > Voortgang > 4.Toegangsrechten GD".

# Nipper-spreadsheet ophalen bij GD
path_wp_nipper_express <- paste0(config$gs_downloads, "/", "nipper_express.xlsx")
gd_file <- cz_get_url("nipper_express")
drive_download(file = gd_file, overwrite = T, path = path_wp_nipper_express)

# sheets als df ----
tbl_nipper_playlists <- cz_extract_sheet(path_wp_nipper_express, sheet_name = "playlists")
tbl_nipper_select <- cz_extract_sheet(path_wp_nipper_express, sheet_name = "nipper-select")
tbl_nipper_keys <- cz_extract_sheet(path_wp_nipper_express, sheet_name = "programma_sleutels")
tbl_nipper_audiolocaties <- cz_extract_sheet(path_wp_nipper_express, sheet_name = "audio_locaties")

# persist tables ----
# saveRDS(tbl_nipper_playlists, file = paste0(config$cz_rds_store, "nipper_playlists.RDS"))
# saveRDS(tbl_nipper_select, file = paste0(config$cz_rds_store, "nipper_select.RDS"))
# saveRDS(tbl_nipper_keys, file = paste0(config$cz_rds_store, "nipper_keys.RDS"))
# saveRDS(tbl_nipper_audiolocaties, file = paste0(config$cz_rds_store, "nipper_audiolocaties.RDS"))
