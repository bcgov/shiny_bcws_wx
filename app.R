rm(list=ls())

library(RcppRoll)
library(tmap)
library(sf)
library(dplyr)
library(rbokeh)
library(lubridate)
library(leaflet)
library(curl)
library(bcmaps)
library(data.table)
library(DT)

# LOAD TIME ####

  today <- as.Date(Sys.Date())
  year <- substr(today,0,4)

# LOAD NRD ####

# nrd <- bcmaps::nr_districts() %>%
#   mutate(ORG_UNIT_NAME = gsub(" Natural Resource District", "",ORG_UNIT_NAME)) %>%
#   select(ORG_UNIT_NAME) %>%
#   mutate(n = row_number())  %>%
#   st_transform(4326)
# saveRDS(nrd, "nrd.rds")
nrd <- readRDS("nrd.rds")

# LOAD STATIONS ####

# stn <- readr::read_csv("https://www.for.gov.bc.ca/ftp/HPR/external/!publish/BCWS_DATA_MART/2022/2022_BCWS_WX_STATIONS.csv", id = "file_name") %>%
#   st_as_sf(coords = c("LONGITUDE","LATITUDE"), crs = 4326) %>% st_intersection(nrd %>% select(-n))
# saveRDS(stn, "stn.rds")
stn <- readRDS("stn.rds")

stn_name <- stn %>% st_drop_geometry() %>% select(STATION_CODE, STATION_NAME)
ids <- sort(unique(stn$STATION_NAME))

# LOAD WX DATA ####

  get_bcws_data <- function(y, dates){

    url <- paste0("https://www.for.gov.bc.ca/ftp/HPR/external/!publish/BCWS_DATA_MART/",y,"/") # url <- "ftp://ftp.for.gov.bc.ca/HPR/external/!publish/BCWS_DATA_MART/2022/"
    list_csv_files <- paste0(url, dates,".csv")

    df <- bind_rows(lapply(list_csv_files, function(file){
      t <- tempfile(fileext = ".csv")
      curl::curl_download(file, destfile = t)
      read.csv(t) })) %>%
      as_tibble() %>%
      mutate(DATE_TIME = as_datetime(paste0(substr(DATE_TIME,1,4),"-",
                                            substr(DATE_TIME,5,6),"-",
                                            substr(DATE_TIME,7,8)," ",
                                            substr(DATE_TIME,9,10),":00:00"))+1*60*60) %>%
      mutate(date = as.Date(DATE_TIME, format = "%Y%m%d"),
             time = substr(DATE_TIME, 12,13),
             datetime = DATE_TIME) %>%
      filter(!is.na(DATE_TIME))

  }

  # dates <- seq(as.Date("2023-01-01"), as.Date("2023-10-01"), "1 day")
  # hist <- get_bcws_data(year, dates)
  # saveRDS(hist, "hist.rds")
  hist <- readRDS("hist.rds")
  dates <- seq(as.Date("2023-10-01"), today, "1 day")
  cur <- get_bcws_data(year, dates)
  df <- bind_rows(hist,cur)
  rm(hist, cur)

# LOAD COLORS ####

  mycolors <- rep(c("#4E79A7","#A0CBE8","#F28E2B","#FFBE7D","#59A14F","#8CD17D","#B6992D",
                "#F1CE63","#499894","#86BCB6","#E15759","#FF9D9A","#79706E","#BAB0AC",
                "#D37295","#FABFD2","#B07AA1","#D4A6C8","#9D7660","#D7B5A6"), 100)

# UI ####

ui <- navbarPage("BCWS WX",
             theme = "css/bcgov.css",
             id = "bcws_wx",
             header = HTML("<h2>BC Wildfire Service Weather Station Rainfall Intensity</h2>"),
             tabPanel(
               "Realtime Data",
               fluidRow(
                 column(3,
                        selectInput(
                          "siteID",
                          label="Add/remove station(s):",
                          choices=ids,
                          selected = "SUMMIT",
                          multiple = TRUE),
                        selectInput(
                          "nrdID",
                          label="Select all stations from district (optional):",
                          choices=c("",sort(nrd$ORG_UNIT_NAME)),
                          selected = "",
                          multiple = TRUE),
                        selectInput(
                          "variableID",
                          label="Select variable:",
                          choices=vars <- c("HOURLY_PRECIPITATION",
                                            "HOURLY_TEMPERATURE",
                                            "HOURLY_RELATIVE_HUMIDITY",
                                            "HOURLY_WIND_SPEED",
                                            "HOURLY_WIND_DIRECTION",
                                            # "FINE_FUEL_MOISTURE_CODE",
                                            # "INITIAL_SPREAD_INDEX",
                                            # "FIRE_WEATHER_INDEX",
                                            # "DUFF_MOISTURE_CODE",
                                            # "DROUGHT_CODE",
                                            # "BUILDUP_INDEX",
                                            # "DANGER_RATING",
                                            # "RN_1_PLUVIO1",
                                            "SNOW_DEPTH"#,
                                            # "SNOW_DEPTH_QUALITY",
                                            # "PRECIP_PLUVIO1_STATUS",
                                            # "PRECIP_PLUVIO1_TOTAL",
                                            # "RN_1_PLUVIO2",
                                            # "PRECIP_PLUVIO2_STATUS",
                                            # "PRECIP_PLUVIO2_TOTAL",
                                            # "RN_1_RIT",
                                            # "PRECIP_RIT_STATUS",
                                            # "PRECIP_RIT_TOTAL",
                                            # "PRECIP_RGT",
                                            # "SOLAR_RADIATION_LICOR",
                                            # "SOLAR_RADIATION_CM3"
                                            ),
                          selected = "HOURLY_PRECIPITATION"
                          ),
                        selectInput(
                          "aggregateID",
                          label="Rolling time window (lower plot):",
                          choices=c("1 hour", "12 hours", "1 day (24h)", "2 days (48h)", "3 days (72h)"),
                          selected = "1 day (24h)"),
                        selectInput(
                          "statisticID",
                          label="Rolling statistic (lower plot):",
                          choices=c("mean","max","min","sum"),
                          selected = "sum"),
                        dateRangeInput(
                          min = paste0(year,"-01-01"),
                          max = today,
                          "datesID",
                          label = "Date range:",
                          start = as.Date(Sys.Date())-5,
                          end = as.Date(Sys.Date())),
                        p(),strong("Click to add or remove stations:"),
                        leafletOutput("mapStns")
                 ),
                 column(9,
                        rbokehOutput("distPlot"),
                        rbokehOutput("rollingPlot"),
                        p(),strong("Most Recent Total Precipitation (Rainfall Intensity):"),
                        DT::dataTableOutput("shutdownOutput"),
                        downloadButton("downloadData", paste("Download Hourly CSV")),
                        p(),strong("Source:"),em(paste0("https://www.for.gov.bc.ca/ftp/HPR/external/!publish/BCWS_DATA_MART/",year,"/")),
                        p("Data comes with no guarantees of quality or reliability. Use at your own risk.")
                        )
                 )
               ),
             tabPanel("About")
    )

server <- function(input, output, session) {

  source("prep.R")

  output$distPlot <- renderRbokeh({

    df_sub <- df %>%
      filter(STATION_NAME %in% input$siteID) %>%
      filter(date >= input$datesID[1]) %>%
      filter(date <= input$datesID[2]) %>%
      rename(var = input$variableID) %>%
      select(STATION_NAME, datetime, var) %>%
      filter(!is.na(var))

    df_sub$STATION_NAME <- factor(df_sub$STATION_NAME, levels = input$siteID, ordered = T)
    df_sub$color <- mycolors[match(df_sub$STATION_NAME, input$siteID)]

    if(nrow(df_sub)>=1){
      figure(title = paste("Hourly Data (most recent:", max(df_sub$datetime),")"),
             width = 1400, height = 600,
             legend_location = NULL,
             tools = c("pan", "wheel_zoom", "box_zoom", "reset", "save")) %>%
        ly_lines(datetime, var, data = df_sub, group = STATION_NAME) %>%
        ly_points(datetime, var, data = df_sub,
                  hover = list(datetime, STATION_NAME, var),
                  color = color) %>%
        theme_legend(label_text_font_size = "9pt") %>%
        x_axis(label = "Date / Time (PDT)") %>%
        y_axis(paste(input$variableID))}
  })


  output$rollingPlot <- renderRbokeh({

    hours <- tibble(id = c("1 hour", "12 hours", "1 day (24h)", "2 days (48h)", "3 days (72h)"),
                    hours = c(1,12,24,48,72)) %>%
      filter(id == input$aggregateID) %>%
      select(hours) %>% pull()

    df_sub <- df %>%
      filter(STATION_NAME %in% input$siteID) %>%
      filter(date >= input$datesID[1]) %>%
      filter(date <= input$datesID[2]) %>%
      rename(var = input$variableID) %>%
      select(STATION_NAME, datetime, var)

    df_sub_adj <- df_sub %>%
      arrange(datetime) %>%
      group_by(STATION_NAME) %>%
      filter(!is.na(var)) %>%
      mutate(
        mean = roll_mean(var, hours, align = "right", fill = NA, na.rm = T),
        min = roll_min(var, hours, align = "right", fill = NA, na.rm = T),
        max = roll_max(var, hours, align = "right", fill = NA, na.rm = T),
        sum = roll_sum(var, hours, align = "right", fill = NA, na.rm = T)) %>%
      rename(myvar = input$statisticID) %>%
      select(STATION_NAME, datetime, myvar)

    df_sub_adj$STATION_NAME <- factor(df_sub_adj$STATION_NAME, levels = input$siteID, ordered = T)
    df_sub_adj$color <- mycolors[match(df_sub_adj$STATION_NAME, input$siteID)]

    if(nrow(df_sub_adj)>=1){
      figure(title = paste("Rolling Statistics", input$aggregateID, input$statisticID, "(most recent:", max(df_sub$datetime),")"),
             width = 1400, height = 600,
             legend_location = NULL, #"top_left",
             tools = c("pan", "wheel_zoom", "box_zoom", "reset", "save")) %>%
        ly_lines(datetime, myvar, data = df_sub_adj, group = STATION_NAME) %>%
        ly_points(datetime, myvar, data = df_sub_adj,
                  hover = list(datetime, STATION_NAME, myvar),
                  color = color) %>%
        # set_palette(discrete_color = pal_color(mycolors)) %>%
        theme_legend(label_text_font_size = "9pt") %>%
        x_axis(label = "Date / Time (PDT)") %>%
        y_axis(paste(input$variableID))}

  })


  # TABLE


  output$shutdownOutput <- DT::renderDataTable({

    df_sub <- df %>%
      filter(STATION_NAME %in% input$siteID) %>%
      rename(var = "HOURLY_PRECIPITATION") %>%
      select(STATION_NAME, datetime, var) %>%
      filter(!is.na(var)) %>%
      filter(!is.na(datetime))

    temp <- df_sub %>%
      arrange(datetime) %>%
      group_by(STATION_NAME) %>%
      filter(!is.na(var)) %>%
      mutate(sum12 = round(roll_sum(var, 12, align = "right", fill = NA, na.rm = T),2),
             sum24 = round(roll_sum(var, 24, align = "right", fill = NA, na.rm = T),2),
             sum48 = round(roll_sum(var, 48, align = "right", fill = NA, na.rm = T),2),
             sum72 = round(roll_sum(var, 72, align = "right", fill = NA, na.rm = T),2)) %>%
      filter(datetime == max(datetime)) %>%
      select(-var)

    DT::datatable(temp, colnames = c("Station", "Date/Time (PDT)", "12hr", "24hr","48hr","72hr"))
  })


  # STN MAP
  output$mapStns <- renderLeaflet({

    stn_map <- stn %>%
      filter(STATION_NAME %in% ids) %>%
      mutate(group = case_when(STATION_NAME %in% input$siteID ~ "selected",
                               TRUE ~ ""))

    stn_map_selected <- select(stn_map, c("STATION_NAME","group", "ELEVATION_M")) %>%
      filter(group == "selected")

    stn_map_not <- select(stn_map, c("STATION_NAME","group", "ELEVATION_M")) %>%
      filter(group == "")

    stn_map_selected$STATION_NAME <- factor(stn_map_selected$STATION_NAME, levels = input$siteID, ordered = T)

    if(nrow(stn_map_selected)>=1){
      tmap_leaflet(
        tm_shape(nrd, bbox = stn_map_selected %>% st_buffer(50000) %>% st_bbox()) +
          tm_borders() +
          tm_text("ORG_UNIT_NAME", scale=1) +
          tm_shape(stn_map_selected) +
          tm_symbols(col = "STATION_NAME", palette = mycolors, size = 1.5, legend.col.show = FALSE,
                     popup.vars = FALSE) +
          tm_shape(stn_map_not) +
          tm_symbols(size = 1.5, popup.vars = FALSE), in.shiny = TRUE)}else{
            tmap_leaflet(
              tm_shape(stn_map_not) +
                tm_symbols(size = 1.5, popup.vars = FALSE))
          }

  })

  output$downloadData <- downloadHandler(

    filename = function() {
      gsub("ID:","",gsub("-","",paste("BCWS_WX_DOWNLOAD_",
                                      input$variableID, "_",
                                      input$datesID[1], "_",
                                      input$datesID[2],
                                      ".csv", sep = "")))
    },
    content = function(file) {
      write.csv(df %>%
                  filter(STATION_NAME %in% input$siteID) %>%
                  filter(date >= input$datesID[1]) %>%
                  filter(date <= input$datesID[2]) %>%
                  select(STATION_NAME, datetime, input$variableID) %>%
                  arrange(datetime),
                file, row.names = FALSE)
    }
  )


  # CLICK ON MAP TO ADD/REMOVE SITES
  observeEvent(input$mapStns_shape_click, {
    p <- input$mapStns_shape_click  # typo was on this line
    i = gsub("."," ",gsub("ID.","ID:",p$id), fixed = T)

    if(i %in% input$siteID){
      updateSelectizeInput(inputId = "siteID", selected = input$siteID[input$siteID!=i])
    }else{
      updateSelectizeInput(inputId = "siteID", selected = c(input$siteID,i))}
  })

  observeEvent(input$nrdID, {

    if(length(input$nrdID)>0){

      nrd_stn <- stn %>% filter(ORG_UNIT_NAME %in% input$nrdID)#[stn %>% st_intersects(nrd %>% filter(ORG_UNIT_NAME %in% input$nrdID) %>% summarize(), sparse = F),]
      # nrd_stn <- stn[stn %>% st_intersects(nrd[1,], sparse = F),]

      updateSelectizeInput(inputId = "siteID", selected = nrd_stn$STATION_NAME)}
  })

  # stn[stn %>% st_intersects(nrd %>% filter(ORG_UNIT_NAME %in% c("Peace","Campbell River")) %>% summarize, sparse = F),] %>% mapview::mapview()

}

shinyApp(ui = ui, server = server)
