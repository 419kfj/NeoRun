# EPSON NeoRun data converterの使い方
kazuo.fujimoto2007@gmail.com  
`r Sys.Date()`  


```r
library(psych)
library(tidyverse)
library(NeoRun)
# rmarkdown::html_vignette
#vignette: >
#  %\VignetteIndexEntry{Vignette Title}
#  %\VignetteEngine{knitr::rmarkdown}
#  %\VignetteEncoding{UTF-8}
```

# sample data を読み込み、datafarmeに変換する

"20170801-20170801.csv"^[A これはNeoRunのバックアップ機能を用いて保存したCSVデータそのものです。]


```r
fname <- "./inst/extdata/20170801-20170801.csv"
res <-  NeoRun_data(fname)

res$base # Data Description
```

```
## [1] "TrainingName,workout 394 マイチャリ出勤 小雨"
```

```r
names(res$Lap) # 要約されたLapデータ
```

```
##  [1] "LapNo"                "EndPoint"             "LapTime"             
##  [4] "LapDistance"          "LapCalorie"           "LapPaceAve"          
##  [7] "LapSpeedAve"          "LapPitchAve"          "LapStrideAve"        
## [10] "LapKind"              "LapSteps"             "LapSplitTime"        
## [13] "LapSplitDistance"     "LapAscentAltitude"    "LapDescentAltitude"  
## [16] "LapMaxHeartRate"      "LapMinHeartRate"      "LapAverageHeartRate" 
## [19] "LapHrzoneStayTime"    "LapHrzoneArrivalTime" "LapHrzoneUpperLimit" 
## [22] "LapHrzoneLowerLimit"  "LapTimeCentisecond"   "Tag"
```

```r
nrow(res$Lap)
```

```
## [1] 10
```

```r
names(res$Graph) # GPS data
```

```
## [1] "GraphAltitude" "GraphSpeed"    "GraphPitch"    "GraphStride"  
## [5] "GraphDistance" "HeartRate"     "Slope"         "HeartRateFlag"
## [9] "Tag"
```

```r
nrow(res$Graph)
```

```
## [1] 1966
```

```r
summary(res$Graph)
```

```
##  GraphAltitude   GraphSpeed      GraphPitch     GraphStride   
##  Min.   : 92   Min.   :    0   Min.   :  0.0   Min.   :  0.0  
##  1st Qu.:102   1st Qu.:15381   1st Qu.:129.0   1st Qu.:153.0  
##  Median :105   Median :18976   Median :166.0   Median :194.0  
##  Mean   :108   Mean   :17333   Mean   :149.1   Mean   :174.7  
##  3rd Qu.:113   3rd Qu.:21746   3rd Qu.:191.0   3rd Qu.:223.0  
##  Max.   :152   Max.   :30351   Max.   :253.0   Max.   :255.0  
##  GraphDistance    HeartRate         Slope          HeartRateFlag  
##  Min.   :   0   Min.   : 71.0   Min.   :-44.0000   Min.   :0.000  
##  1st Qu.:2188   1st Qu.:121.0   1st Qu.: -2.0000   1st Qu.:0.000  
##  Median :4501   Median :137.0   Median :  0.0000   Median :0.000  
##  Mean   :4706   Mean   :132.6   Mean   : -0.5773   Mean   :1.194  
##  3rd Qu.:7528   3rd Qu.:152.0   3rd Qu.:  1.0000   3rd Qu.:4.000  
##  Max.   :9534   Max.   :166.0   Max.   : 99.0000   Max.   :4.000  
##        Tag      
##  20170801:1966  
##                 
##                 
##                 
##                 
## 
```


```r
names(res$Graph)
```

```
## [1] "GraphAltitude" "GraphSpeed"    "GraphPitch"    "GraphStride"  
## [5] "GraphDistance" "HeartRate"     "Slope"         "HeartRateFlag"
## [9] "Tag"
```

```r
res$Graph %>% select("GraphDistance","GraphSpeed","HeartRate") %>%
  pairs.panels(main=res$base)
```

![](How_to_use_NeoRun_data_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

# 宇都宮マラソン2016のデータ

```r
fname = "./inst/extdata/20151115-20151115.csv"
res <-  NeoRun_data(fname)
```


```r
names(res$Graph)
```

```
## [1] "GraphAltitude" "GraphSpeed"    "GraphPitch"    "GraphStride"  
## [5] "GraphDistance" "HeartRate"     "Slope"         "HeartRateFlag"
## [9] "Tag"
```

```r
res$base %>% str_split(",") %>% unlist %>% .[2] %>% paste(.,"2016") -> title
res$Graph %>% select("GraphDistance","GraphSpeed","GraphPitch","GraphStride","HeartRate") %>%
  pairs.panels(main=title)
```

![](How_to_use_NeoRun_data_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

