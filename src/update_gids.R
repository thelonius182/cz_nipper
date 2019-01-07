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

# = = = = = = = = = = = = = = = = = = = = = = = = wpdev2 = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

get_wp_conn <- function() {
  db_user <- key_get(service = "sql-wpdev_user")
  db_password <- key_get(service = "sql-wpdev_pwd")
  db_name <- key_get(service = "sql-wpdev_db")
  db_host <-  "127.0.0.1"
  db_port <- 3306
  db_table <- "cz.wp_posts"
  flog.appender(appender.file("/Users/nipper/Logs/nipper.log"), name = "nipperlog")
  
  result <- tryCatch(
    {
      grh_conn <- dbConnect(drv = MySQL(), user = db_user, password = db_password,
                            dbname = db_name, host = db_host, port = db_port)
    },
    error = function(cond) {
      flog.error("Wordpress database onbereikbaar (dev: check PuTTY)", name = "nipperlog")
      return("connection-error")
    }
  )    
  return(result)
}

wp_conn <- get_wp_conn()

# type S4 indicates a valid connection; type "character" indicates failure
if (typeof(wp_conn) == "S4") { 
  res_1 <- dbGetQuery(conn = wp_conn, 
                    statement = "select post_content from wp_posts where id = 405929;")
  on.exit(dbDisconnect(mydb))
}

  
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
# playlist2postdate <- function(playlist) {
#   tmp_date <- playlist %>% str_sub(0, 8)
#   tmp_time <- playlist %>% str_sub(11, 13) %>% paste0(":00:00")
#   result <- paste0(tmp_date, " ", tmp_time) %>% ymd_hms
# }
# 
# pd1 <- playlist2postdate(c("20181231_ma14.060_liederen", "20180213_ma14.060_liederen")) %>% as.character
