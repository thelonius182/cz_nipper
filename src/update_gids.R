source("src/shared_functions.R", encoding = "UTF-8")

for (seg2 in 1:1) {
  # Connect to database ----
  wp_conn <- get_wp_conn()
  
  # connection type S4 indicates a valid connection; other types indicate failure
  if (typeof(wp_conn) != "S4") { 
    flog.error("Nipper: invalid db-connection (non-S4)", name = "nipperlog")
    break
  }
  
  # set post data ----
  dummy_vt <- pl_werken %>% filter(vt_blok_nr == 1) %>% 
    mutate(vt_blok_nr = 0, lengte = hms::as_hms(40))
  
  drb_gids <- rbind(pl_werken, dummy_vt) %>%
    arrange(playlist, vt_blok_letter, vt_blok_nr) %>% group_by(playlist) %>%
    mutate(
      cum_tijd = np_sec2hms(cumsum(as.duration(lengte))),
      # cum_tijd_secs = seconds(cumsum(as.duration(lengte)) %% 60),
      # cum_tijd_secs2min = ifelse(cum_tijd_secs > 30, 1, 0),
      cum_tijd = lag(cum_tijd, n = 1),
      # cum_tijd_secs = lag(cum_tijd_secs, n = 1),
      # cum_tijd_secs2min = lag(cum_tijd_secs2min, n = 1)
      wallclock = get_wallclock(pm_cum_tijd = cum_tijd, pm_playlist = playlist)
    ) %>% filter(vt_blok_nr != 0) %>% 
    select(-bezetting, -album, -opnameNr, -starts_with("vt_"))
  
  # for each playlist ----
  for (cur_pl in pl_nieuw$playlist) {
    
    #+ set post date ----
    sql_post_date <- playlist2postdate(cur_pl) %>% as.character
    drb_gids_pl <- drb_gids %>% filter(playlist == cur_pl)
    
    #+ set intro ----
    koptekst <- drb_gids_pl %>% select(playlist, componist_lbl) %>% distinct %>% 
      group_by(playlist) %>% summarise(werken_van = paste(componist_lbl, collapse = ", "))
    
    regel <- sprintf('Werken van %s.\n<!--more-->\n', koptekst$werken_van)
    
    #+... escape the string ----
    sql_gidstekst <- paste0(dbEscapeStrings(wp_conn, enc2native(regel)), "\n")
    
    regel <- '<style>td {padding: 6px; text-align: left;}</style>\n<table style="width: 100%;"><tbody>'
    
    #+... add to post lines ----
    sql_gidstekst <- paste0(sql_gidstekst, dbEscapeStrings(wp_conn, enc2native(regel)), "\n")
    
    #+ + for each track ----
    for (q1 in 1:nrow(drb_gids_pl)) {
      
      #+ +... set time ----
      regel <-
        sprintf(
          '<tr>\n<td>[track tijd="%s" text="%s %s"]\n<span>',
          drb_gids_pl$cum_tijd[q1],
          drb_gids_pl$wallclock[q1],
          drb_gids_pl$titel[q1]
        )
      
      #+ +... add to post lines ----
      sql_gidstekst <- paste0(sql_gidstekst, dbEscapeStrings(wp_conn, enc2native(regel)))
      
      #+ +... set composer ----
      regel <- drb_gids_pl$componist_lbl[q1]
      
      #+ +... add to post lines ----
      sql_gidstekst <- paste0(sql_gidstekst, dbEscapeStrings(wp_conn, enc2native(regel)), "\n")
      
      #+ +... set artists ----
      regel <- sprintf('%s</span></td>\n</tr>',
                       drb_gids_pl$uitvoerenden[q1])
      
      #+ +... add to post lines ----
      sql_gidstekst <-
        paste0(sql_gidstekst, dbEscapeStrings(wp_conn, enc2native(regel)), "\n")
    }
    
    #....+ add spacer ----
    regel <- '</tbody>\n</table>'
    
    #....+ add to post lines ----
    sql_gidstekst <- paste0(sql_gidstekst, dbEscapeStrings(wp_conn, enc2native(regel)), "\n")
    
    #....+ get WP-post-id's NL/EN ----
    upd_stmt01 <- sprintf(
      "select id from wp_posts where post_date = '%s' and post_type = 'programma';",
      sql_post_date
    )
    
    dsSql01 <- dbGetQuery(wp_conn, upd_stmt01)
    
    #....+ update posts ----
    for (u1 in 1:nrow(dsSql01)) {
      upd_stmt02 <- sprintf(
        "update wp_posts set post_content = '%s' where id = %i;",
        sql_gidstekst,
        dsSql01$id[u1]
      )
      
      dbGetQuery(wp_conn, upd_stmt02)
      
      #....+ add image & update ----
      # upd_stmt03 <- sprintf(
      #   "insert into wp_postmeta (post_id, meta_key, meta_value) values(%i, '_thumbnail_id', %i);",
      #   dsSql01$id[u1],
      #   463848
      # )
      # 
      # dbGetQuery(wp_conn, upd_stmt03)
    }
    
    #....+ replace replay-posts with recycled OE's ---- 
    for (seg_oe in 1:1) {
      # !TEST! # cur_pl <- "20200112_zo07-180_ochtendeditie"
      
      if (!str_detect(string = cur_pl, pattern = "_ochtendeditie")){
        break
      }
      
      #....+ . get recycle-OE's date ----
      # details are on GD: kringloopherhalingen ochtendeditie
      cur_pl_date <- playlist2postdate(cur_pl)
      
      replay_date <- cur_pl_date + days(7)
      
      oe_offset <- 
        case_when(cur_pl_date >= ymd_hms("2020-06-22 07:00:00")              ~ 175L,
                  str_detect(string = cur_pl, pattern = "_(ma|di|wo|do)\\d") ~ 175L, 
                  TRUE                                                       ~ 182L)
      
      oe_date <- replay_date - days(oe_offset)
      
      flog.info("Kringloopherhaling: op %s klinkt die van %s", 
                replay_date, oe_date, name = "nipperlog")
      
      #....+ . get replay_date's post-id ----
      upd_stmt06 <- sprintf(
        "select min(id) from wp_posts where post_date = '%s' and post_type = 'programma';",
        replay_date
      )
      
      replay_pgm_id <- dbGetQuery(wp_conn, upd_stmt06)
      
      #....+ . get recycle-OE's post-id ----
      upd_stmt04 <- sprintf(
        "select min(id) from wp_posts where post_date = '%s' and post_type = 'programma';",
        oe_date
      )
      
      oe_pgm_id <- dbGetQuery(wp_conn, upd_stmt04) 

      #....+ . update post-id's NL/EN ----
      for (r1 in 1:2) {
        upd_stmt05 <-
          sprintf(
            "update wp_postmeta set meta_value = %s where post_id = %s and meta_key = 'pr_metadata_orig';",
            oe_pgm_id + r1 - 1L,
            replay_pgm_id + r1 - 1L
          )
        
        flog.info("SQL: %s", upd_stmt05, name = "nipperlog")
        
        dbGetQuery(wp_conn, upd_stmt05)
      }
    }
    
    flog.info("Gids bijgewerkt: %s", cur_pl, name = "nipperlog")
  }
  
  #.... disconnect from db ----
  on.exit(dbDisconnect(wp_conn))
}
# a_post_date <- "2019-01-01 17:00:00"
  # 
  # gidstekst <- paste("Werken van Haydn, Grieg en Goebajdoelina<!--more--><br>\n",
  #                    "<em>[track tijd=\"01:49\" text=\"01:49 - Sofia Goebajdoelina: Hell und Dunkel\"]</em>",
  #                    "German: Falsches Üben von Xylophonmusik quält jeden größeren Zwerg.",
  #                    "Koninklijke Academie voor Muziek te Kopenhagen. \nHij is wat je noemt “een groot pleitbezorger van hedendaagse muziek”.<br>",
  #                    "<em>[track tijd=\"23:46\" text=\"23:46 - Sofia Goebajdoelina: Hell und Dunkel\"]</em>",
  #                    "Finnish: (5) Törkylempijävongahdus\nPolish: Pchnąć w tę łódź jeża lub osiem skrzyń fig.<br>",
  #                    "<em>[track tijd=\"45:19\" text=\"45:19 - Sofia Goebajdoelina: Hell und Dunkel\"]</em>",
  #                    "Finnish: (5) Törkylempijävongahdus\nPolish: Pchnąć w tę łódź jeża lub osiem skrzyń fig.\n",
  #                    "Esperanto: Eĥoŝanĝo ĉiuĵaŭde.\n",
  #                    "Euro Symbol: €."
  # ) 
  
  # mydb <-  dbConnect(drv = MySQL(), user = db_user, password = db_password,
  # dbname = db_name, host = db_host, port = db_port)
# tmp <- sprintf("SELECT * FROM emp WHERE lname = %s", "O'Reilly")
# stmt <- "update wp_posts set post_content = 'Hello CZ-World trial!' where id = 405929;"
# stmt <- sprintf("update wp_posts set post_content = '%s' where id = %s", content_upd, 405929)

# stmt_esc <- dbEscapeStrings(mydb, stmt)
# rs <- dbSendQuery(mydb, stmt)
# 
# 
# df <-  fetch(rs, n = -1)
# 
# pd1 <- playlist2postdate(c("20181231_ma14.060_liederen", "20180213_ma14.060_liederen")) %>% as.character
# = = = = = = = = = = = = = = = = = = = = = = = = wpProd = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# 1. Library
# library(RMySQL)

# 2. Settings
# db_user <- key_get(service = "sql-wpprd_user")
# db_password <- key_get(service = "sql-wpprd_pwd")
# db_name <- key_get(service = "sql-wpprd_db")
# db_host <-  key_get(service = "sql-wpprd_host")
# db_port <- 3306
# db_table <- "cz.wp_posts"

# 3. Read data from db
# mydb <-  dbConnect(MySQL(), user = db_user, password = db_password,
# dbname = db_name, host = db_host, port = db_port)
# s2 <- "SELECT * FROM wp_posts where lower(post_title) = 'ochtendeditie' and post_type = 'programma';"
# rs2 <- dbSendQuery(mydb, s2)
# df2 <-  fetch(rs2, n = -1)
# on.exit(dbDisconnect(mydb))
