context("cz-gids muurkloktijden")

# source(paste0(dir_project, "/load_initial_history.R"))


test_that("year/month-series", {
  date_range_start <- ymd("2008-12-01")
  date_range_stop <- ymd("2009-02-31")
  files <- prep_initial_history(date_range_start, date_range_stop)
  expect_match(object = files[17], regexp = "/Input/PINFHKG_200[89][.]csv")
  expect_match(object = files[6], regexp = "/Output data/hkg_200[89][01][0-9][.]rds")
})

get_wallclock <- function(pm_cum_tijd, pm_playlist) {
  cum_tijd_ts <- paste0("2019-01-01 ", pm_cum_tijd) %>% ymd_hms
  start_clock <- pm_playlist %>% str_sub(12, 13) %>% as.integer
  wallclcok_ts <- cum_tijd_ts + hours(start_clock)
  wallclock_ts_rounded <- wallclcok_ts %>% round_date("minute")
  wallclock <- wallclock_ts_rounded %>% as.character %>% str_sub(12, 16)
}

gids1 <- drb_gids %>% mutate(wallclock = get_wallclock(cum_tijd, playlist))
