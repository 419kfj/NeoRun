# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'
#
#fname <- "20170801-20170801.csv"
#.run <- NeoRun_data(fname)
#
#.run$Lap
#.run$base
#.run$Graph

NeoRun_data <- function(fname){
  library(tidyverse)
  library(readr)
  library(stringr)

  ## Read CSV data from Backup
  .d <- read_lines(fname,locale=locale(encoding="CP932"))
  for(i in 1:length(.d)){
    if(.d[i] == "[TrainingResult]") p.TrainingResult = i
    if(.d[i] == "[TrainingData]") p.TrainingData = i
    if(.d[i] == "[GraphData]") p.GraphData = i
    if(.d[i] == "[GPSData]") p.GPSData = i
    if(.d[i] == "[LapData]") p.LapData = i
    if(.d[i] == "[TrainingSettingData]") p.TrainingSettingData = i
    if(.d[i] == "[[end]]") p.end = i
  }
  ## Read basic Info
  .d.TrainingName <- read_csv(fname,locale=locale(encoding="CP932"),
                              skip=2,n_max = 1,col_names = FALSE)
  .d.TrainingName
  .d.TrainingInfo <- read_csv(fname,locale=locale(encoding="CP932"),
                              skip=5,n_max = 1,col_names = TRUE)
  .d.TrainingInfo
  .d.TrainingInfo %>% select(StartDay,StartTime,EndDay,EndTime) -> .d.Date
  # get Graph Data
  .d.Graph.Interval <- read_csv(fname,locale=locale(encoding="CP932"),
                                skip=p.GraphData,n_max = 2)
  .d.Graph.0 <- read_csv(fname,locale=locale(encoding="CP932"),
                         skip=p.GraphData + 2 , n_max = p.GPSData - p.GraphData -3,
                         col_names = FALSE)

  .d.Graph.0[,1] %>% unlist() -> cnames
  .d.Graph.0[,-1] %>% t() -> .d.Graph
  colnames(.d.Graph) <- cnames
  rownames(.d.Graph) <- 1:nrow(.d.Graph)
  .d.Graph.Interval
  .d.Graph %>% tbl_df() -> .d.Graph.td ###%>% mutate(.d.Date[1,1])

  ## Get LAP data
  begin <- p.LapData + 1
  end <- p.TrainingSettingData-1
  .d[begin:end] %>% read.csv(text=.) -> .d.lap # AdvR p24

  ## Get GPS String
  # [GPSData]     GPSstring[]
  # Gpstime       2
  # GpsLatitude   3
  # GpsLongitude  4
  # GpsAltitude   5
  # GpsDirection  6
  # GpsSpeed      7
  # GpsStatus     8
  # [LapData]

  start <- p.GPSData +1
  end <- p.LapData -1
  cmd <- NULL
  for(i in start:end) cmd <- paste0(cmd,".d[",i,"],")
  cmd
  Gpsall <- eval(parse(text=paste0("paste0(",cmd,"NULL)")))
  Gpsall %>% str_split("Gps") -> GPSstring

  ## GPS Time
  GPSstring[[1]][2] %>% stringr::str_split(",") -> Gpstime.0
  stringr::str_split(Gpstime.0[[1]][2],";") -> Gpstime.1
  Gpstime.1 %>% unlist()  -> Gtime
  #parse_time("00:00:0") # HM は二桁必要 sprintf で桁を合わせる。
  for(i in 1:length(Gtime)){
    Gtime[i] %>% str_split(pattern = ":") %>% unlist() %>% as.numeric() -> .t
    sprintf("%02s:%02s:%02s",.t[1],.t[2],.t[3]) -> Gtime[i]
  }
  Gtime %>% parse_time() -> GPSTime

  ## GpsLatitude   3
  GPSstring[[1]][3] %>% str_split(",") -> Gps_tmp
  str_split(Gps_tmp[[1]][2],";") %>% unlist() %>% as.numeric() -> GPSlatitude

  ## GpsLongitude  4
  GPSstring[[1]][4] %>% str_split(",") -> Gps_tmp
  str_split(Gps_tmp[[1]][2],";") %>% unlist() %>% as.numeric() -> GPSLongitude

  # GpsAltitude   5
  GPSstring[[1]][5] %>% str_split(",") -> Gps_tmp
  str_split(Gps_tmp[[1]][2],";") %>% unlist() %>% as.numeric() -> GPSAltitude

  # GpsDirection  6
  GPSstring[[1]][6] %>% str_split(",") -> Gps_tmp
  str_split(Gps_tmp[[1]][2],";") %>% unlist() %>% as.numeric() -> GPSDirection

  ## GPSSpeed / 7
  GPSstring[[1]][7] %>% str_split(",") -> Gps_tmp
  str_split(Gps_tmp[[1]][2],";") %>% unlist() %>% as.numeric() -> GPSSpeed


  # GpsStatus     8
  GPSstring[[1]][8] %>% str_split(",") -> Gps_tmp
  str_split(Gps_tmp[[1]][2],";") %>% unlist() %>% as.numeric() -> GPSStatus

  GPS <- data.frame(GPSTime,GPSlatitude,GPSLongitude,GPSAltitude,GPSDirection,GPSSpeed,GPSStatus)
  #GPS %>% tbl_df()

  return(list(base=.d[3],Graph=.d.Graph.td,GPS=GPS,Lap=.d.lap))
}
