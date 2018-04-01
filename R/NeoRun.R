# NeoRun backup data conversion!
#
# 2017-08-14 kazuo fujimoto
# kazuo.fujimoto2007@gmail.com

# Unit of data
# - GrapDistance m
# - GraphSpeed m/hour (to convert it to pace、1000*60 / x  min/Km)
# - GraphPitch step / min (spm)
# - GraphStride cm
# - HeartRate beat / min (bpm)

# sample data = "20170801-20170801.csv"

#fname <- "inst/extdata/20170801-20170801.csv"
#res <-  NeoRun_data(fname)
#
#res$Lap
#res$base
#res$Graph

NeoRun_data <- function(fname,runmemo=NULL){
  library(dplyr)
  library(readr)
  library(stringr)

  ## Read CSV data from Backup fname="~/Dropbox/RStudio/RunNeorun/20180401-20180401.csv"
  .d <- read_lines(fname,locale=locale(encoding="CP932"))
  tag <- substr(str_split(fname,"/")[[1]][length(str_split(fname,"/")[[1]])],1,8)
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
  .d.TrainingMemo <- read_csv(fname,locale=locale(encoding="CP932"),
                              skip=5,n_max = 1,col_names = TRUE)

  .d.TrainingData <- read_csv(fname,locale=locale(encoding="CP932"),
                              skip=8,n_max = 1,col_names = TRUE)

  .d.TrainingName <- read_csv(fname,locale=locale(encoding="CP932"),
                              skip=2,n_max = 1,col_names = FALSE)
#  .d.TrainingName
  .d.TrainingInfo <- read_csv(fname,locale=locale(encoding="CP932"),
                              skip=5,n_max = 1,col_names = TRUE)
#  .d.TrainingInfo
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
  .d.Graph %>% tbl_df() %>% cbind(.,Tag=tag) -> .d.Graph.td ###%>% mutate(.d.Date[1,1])

  ## Get LAP data
  begin <- p.LapData + 1
  end <- p.TrainingSettingData-1
  .d[begin:end] %>% read.csv(text=.) %>% tbl_df() %>% mutate(Tag=tag) -> .d.lap # AdvR p24

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

  GPS <- data.frame(GPSTime,GPSlatitude,GPSLongitude,GPSAltitude,GPSDirection,
                    GPSSpeed,GPSStatus) %>% mutate(Tag=tag)
  #GPS %>% tbl_df()

  return(list(base=.d[3],
              Graph=.d.Graph.td,
              GPS=GPS,
              Lap=.d.lap,
              Tag=tag,
              Memo=runmemo,
              TrainMemo = .d.TrainingMemo,
              TrainData = .d.TrainingData))
}

####
# Make Lap summary Table
#
# 2018-04-01

#fname="20180401-20180401.csv"
lap_table <- function(fname){
.dd <- NeoRun_data(fname)
.d <- .dd$Lap
names(.d)[1:18] <- c("No.","EndPoint","Time",
                     "距離","カロリー","平均ペース",
                     "平均速度","平均ピッチ","平均ストライド",
                     "Kind","Step","SplitTime",
                     "SplitDist","登り","下り",
                     "MaxHR","MinHR","AveHR")

.d %>% select(1,3,8,9,16:18) %>% mutate(Time=paste(Time %/% 60,":",Time %% 60,sep="")) -> .res
return(.res)
}
