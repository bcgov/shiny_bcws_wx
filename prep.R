


# library(future.apply)
# date_download_end <- as.Date(now())-1
# dates <- seq(as.Date("2022-04-14"),date_download_end,by="1 day")
# plan(multisession)
# df <- do.call(bind_rows, future_lapply(dates, function(d){
#   fread(paste0(url,d,".csv"), header = T) %>% as_tibble() %>%
#     mutate(date = as.Date.character(DATE_TIME, format = "%Y%m%d"),
#            time = substr(DATE_TIME, 9,10),
#            datetime = as_datetime(paste0(date, time, ":00:00")))}))
# file.remove("data/df.rds")
# saveRDS(df, "data/df.rds")

# READ DATASET(LOCAL) 
# df <- readRDS("data/df.rds")

# # UPDATE DATASET (LOCAL)
# max_date <- max(df$date)
# df <- df %>% filter(date != max_date)
# update_dates <- seq(as.Date(max_date),as.Date(format(now(), format = "%Y-%m-%d")), by = "1 day")





# df <- readr::read_csv(list_csv_files, id = "file_name") %>%
#   mutate(date = as.Date.character(DATE_TIME, format = "%Y%m%d"),
#          time = substr(DATE_TIME, 9,10),
#          datetime = as_datetime(paste0(date, time, ":00:00")))

# file=list_csv_files[1]
# df <- bind_rows(lapply(list_csv_files, function(file){
#   t <- tempfile(fileext = ".csv")
#   curl::curl_download(file, destfile = t)
#   # Sys.sleep(1)
#   read.csv(t) 
#   }))  %>%
#   mutate(date = as.Date.character(DATE_TIME, format = "%Y%m%d"),
#          time = substr(DATE_TIME, 9,10),
#          datetime = as_datetime(paste0(date, time, ":00:00")))

# df_update <- readr::read_csv(paste0(url, update_dates,".csv")) %>%
#       mutate(date = as.Date.character(DATE_TIME, format = "%Y%m%d"),
#              time = substr(DATE_TIME, 9,10),
#              datetime = as_datetime(paste0(date, time, ":00:00")))
# df_update <- paste0(url, update_dates,".csv") %>%
#   purrr::map_df(~fread(.)) %>%
#   mutate(date = as.Date.character(DATE_TIME, format = "%Y%m%d"),
#          time = substr(DATE_TIME, 9,10),
#          datetime = as_datetime(paste0(date, time, ":00:00")))
# df_update <- do.call(bind_rows, lapply(update_dates[1], function(d){
#   print("G")
#   f <- tempfile(fileext = ".csv")
#   curl::curl_download(paste0(url,d,".csv"), f)
#   q <- fread(f, header = T) %>% as_tibble() %>%
#   # d <- update_dates[3]
#   # print(d)
#   # fread(paste0(url,d,".csv"), header = T) %>% as_tibble() %>%
#     mutate(date = as.Date.character(DATE_TIME, format = "%Y%m%d"),
#            time = substr(DATE_TIME, 9,10),
#            datetime = as_datetime(paste0(date, time, ":00:00")))
#   file.remove(f)
#   q
#   }))
# df <- bind_rows(df, df_update)




# JOIN 


# df %>% names
# df <- df %>% full_join(stn_name)


# IDS 
# df <- df %>% 
#   mutate(STATION_ID = paste0(STATION_NAME, " ID:",STATION_CODE))


# NO DATA STATIONS
# stn_name_no_data <- df %>% filter(is.na(DATE_TIME)) %>% select(STATION_ID) %>% pull()

# NO COORDS STATIONS 
# stn_name_nocoords = full_join(stn %>% st_drop_geometry() %>% select(STATION_ID) %>% mutate(stn = "stn"),
#                               df %>% select(STATION_ID, STATION_CODE) %>% group_by(STATION_ID, STATION_CODE) %>% summarize() %>% mutate(df = "df")) %>% 
#   filter(is.na(stn)) %>% select(STATION_CODE) %>% pull() %>% sort()

# GOOD STATIONS 
# df <- df %>% filter(!is.na(DATE_TIME))
# 
# # NRD
# 
# # 
# # st_write(nrd, "data/nrd.gpkg")
# # nrd <- st_read("data/nrd.gpkg")
# 
# # COLORS
# mycolors <- c("#4E79A7","#A0CBE8","#F28E2B","#FFBE7D","#59A14F","#8CD17D","#B6992D",
#               "#F1CE63","#499894","#86BCB6","#E15759","#FF9D9A","#79706E","#BAB0AC",
#               "#D37295","#FABFD2","#B07AA1","#D4A6C8","#9D7660","#D7B5A6")

