# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
setwd("S:/GitRepository/Reproducible Research - Project 1/RepData_PeerAssessment1")

if (!dir.exists("./data/")){
  dir.create("./data/")
  unzip("activity.zip", exdir = "./data")
}

activity <- read.csv("./data/activity.csv", header = TRUE, colClasses = c("numeric","Date","numeric"))
```

## What is mean total number of steps taken per day?


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
stepsperday.total <- activity %>% group_by(date) %>% summarise(steps=sum(steps))
stepsperday.mean <- mean(stepsperday.total$steps, na.rm = TRUE)
stepsperday.median <- median(stepsperday.total$steps, na.rm = TRUE)
agg.summary <- as.data.frame(rbind(stepsperday.mean, stepsperday.median))
colnames(agg.summary) <- "agg"
```


```r
p1 <- ggplot(data = stepsperday.total, aes(x = steps)) +
  geom_histogram(fill = "white", colour="black")+
  geom_vline(data = agg.summary, aes(xintercept = agg), colour = "red")
p1
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

The **Mean** steps per day is 10766.19.  
The **Median** steps per day is 10765.

## What is the average daily activity pattern?


```r
library(data.table)
```

```
## -------------------------------------------------------------------------
```

```
## data.table + dplyr code now lives in dtplyr.
## Please library(dtplyr)!
```

```
## -------------------------------------------------------------------------
```

```
## 
## Attaching package: 'data.table'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     between, first, last
```

```r
intdata <- activity %>% group_by(interval) %>% summarise(steps = mean(steps, na.rm=TRUE))
qplot(x = interval , y = steps, data = intdata, geom = "line", main ="Average steps per interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
maxstep <- intdata[intdata$steps == max(intdata$steps), "interval"]
```

The interval with the highest average number of steps is **835**.

## Imputing missing values

```r
sumna <- sum(is.na(activity$steps))
```
There are a total of 2304 missing step values.

Using the average of the intervals to impute the missing values.

```r
library(gridExtra)
```

```
## 
## Attaching package: 'gridExtra'
```

```
## The following object is masked from 'package:dplyr':
## 
##     combine
```

```r
activity.impute <- activity
narows <- which(is.na(activity$steps))
activity.impute[is.na(activity$steps)==TRUE,"steps"] <- intdata[intdata$interval == activity[narows,"interval"],"steps"]

p2<- ggplot(data = activity.impute, aes(x = steps))+
  geom_histogram(fill = "white", colour="black")
grid.arrange(p2,p1, ncol=1)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
activity.impute[narows,"steps"]
```

```
##    [1]   1.7169811   0.3396226   0.1320755   0.1509434   0.0754717
##    [6]   2.0943396   0.5283019   0.8679245   0.0000000   1.4716981
##   [11]   0.3018868   0.1320755   0.3207547   0.6792453   0.1509434
##   [16]   0.3396226   0.0000000   1.1132075   1.8301887   0.1698113
##   [21]   0.1698113   0.3773585   0.2641509   0.0000000   0.0000000
##   [26]   0.0000000   1.1320755   0.0000000   0.0000000   0.1320755
##   [31]   0.0000000   0.2264151   0.0000000   0.0000000   1.5471698
##   [36]   0.9433962   0.0000000   0.0000000   0.0000000   0.0000000
##   [41]   0.2075472   0.6226415   1.6226415   0.5849057   0.4905660
##   [46]   0.0754717   0.0000000   0.0000000   1.1886792   0.9433962
##   [51]   2.5660377   0.0000000   0.3396226   0.3584906   4.1132075
##   [56]   0.6603774   3.4905660   0.8301887   3.1132075   1.1132075
##   [61]   0.0000000   1.5660377   3.0000000   2.2452830   3.3207547
##   [66]   2.9622642   2.0943396   6.0566038  16.0188679  18.3396226
##   [71]  39.4528302  44.4905660  31.4905660  49.2641509  53.7735849
##   [76]  63.4528302  49.9622642  47.0754717  52.1509434  39.3396226
##   [81]  44.0188679  44.1698113  37.3584906  49.0377358  43.8113208
##   [86]  44.3773585  50.5094340  54.5094340  49.9245283  50.9811321
##   [91]  55.6792453  44.3207547  52.2641509  69.5471698  57.8490566
##   [96]  56.1509434  73.3773585  68.2075472 129.4339623 157.5283019
##  [101] 171.1509434 155.3962264 177.3018868 206.1698113 195.9245283
##  [106] 179.5660377 183.3962264 167.0188679 143.4528302 124.0377358
##  [111] 109.1132075 108.1132075 103.7169811  95.9622642  66.2075472
##  [116]  45.2264151  24.7924528  38.7547170  34.9811321  21.0566038
##  [121]  40.5660377  26.9811321  42.4150943  52.6603774  38.9245283
##  [126]  50.7924528  44.2830189  37.4150943  34.6981132  28.3396226
##  [131]  25.0943396  31.9433962  31.3584906  29.6792453  21.3207547
##  [136]  25.5471698  28.3773585  26.4716981  33.4339623  49.9811321
##  [141]  42.0377358  44.6037736  46.0377358  59.1886792  63.8679245
##  [146]  87.6981132  94.8490566  92.7735849  63.3962264  50.1698113
##  [151]  54.4716981  32.4150943  26.5283019  37.7358491  45.0566038
##  [156]  67.2830189  42.3396226  39.8867925  43.2641509  40.9811321
##  [161]  46.2452830  56.4339623  42.7547170  25.1320755  39.9622642
##  [166]  53.5471698  47.3207547  60.8113208  55.7547170  51.9622642
##  [171]  43.5849057  48.6981132  35.4716981  37.5471698  41.8490566
##  [176]  27.5094340  17.1132075  26.0754717  43.6226415  43.7735849
##  [181]  30.0188679  36.0754717  35.4905660  38.8490566  45.9622642
##  [186]  47.7547170  48.1320755  65.3207547  82.9056604  98.6603774
##  [191] 102.1132075  83.9622642  62.1320755  64.1320755  74.5471698
##  [196]  63.1698113  56.9056604  59.7735849  43.8679245  38.5660377
##  [201]  44.6603774  45.4528302  46.2075472  43.6792453  46.6226415
##  [206]  56.3018868  50.7169811  61.2264151  72.7169811  78.9433962
##  [211]  68.9433962  59.6603774  75.0943396  56.5094340  34.7735849
##  [216]  37.4528302  40.6792453  58.0188679  74.6981132  85.3207547
##  [221]  59.2641509  67.7735849  77.6981132  74.2452830  85.3396226
##  [226]  99.4528302  86.5849057  85.6037736  84.8679245  77.8301887
##  [231]  58.0377358  53.3584906  36.3207547  20.7169811  27.3962264
##  [236]  40.0188679  30.2075472  25.5471698  45.6603774  33.5283019
##  [241]  19.6226415  19.0188679  19.3396226  33.3396226  26.8113208
##  [246]  21.1698113  27.3018868  21.3396226  19.5471698  21.3207547
##  [251]  32.3018868  20.1509434  15.9433962  17.2264151  23.4528302
##  [256]  19.2452830  12.4528302   8.0188679  14.6603774  16.3018868
##  [261]   8.6792453   7.7924528   8.1320755   2.6226415   1.4528302
##  [266]   3.6792453   4.8113208   8.5094340   7.0754717   8.6981132
##  [271]   9.7547170   2.2075472   0.3207547   0.1132075   1.6037736
##  [276]   4.6037736   3.3018868   2.8490566   0.0000000   0.8301887
##  [281]   0.9622642   1.5849057   2.6037736   4.6981132   3.3018868
##  [286]   0.6415094   0.2264151   1.0754717          NA          NA
##  [291]          NA          NA          NA          NA          NA
##  [296]          NA          NA          NA          NA          NA
##  [301]          NA          NA          NA          NA          NA
##  [306]          NA          NA          NA          NA          NA
##  [311]          NA          NA          NA          NA          NA
##  [316]          NA          NA          NA          NA          NA
##  [321]          NA          NA          NA          NA          NA
##  [326]          NA          NA          NA          NA          NA
##  [331]          NA          NA          NA          NA          NA
##  [336]          NA          NA          NA          NA          NA
##  [341]          NA          NA          NA          NA          NA
##  [346]          NA          NA          NA          NA          NA
##  [351]          NA          NA          NA          NA          NA
##  [356]          NA          NA          NA          NA          NA
##  [361]          NA          NA          NA          NA          NA
##  [366]          NA          NA          NA          NA          NA
##  [371]          NA          NA          NA          NA          NA
##  [376]          NA          NA          NA          NA          NA
##  [381]          NA          NA          NA          NA          NA
##  [386]          NA          NA          NA          NA          NA
##  [391]          NA          NA          NA          NA          NA
##  [396]          NA          NA          NA          NA          NA
##  [401]          NA          NA          NA          NA          NA
##  [406]          NA          NA          NA          NA          NA
##  [411]          NA          NA          NA          NA          NA
##  [416]          NA          NA          NA          NA          NA
##  [421]          NA          NA          NA          NA          NA
##  [426]          NA          NA          NA          NA          NA
##  [431]          NA          NA          NA          NA          NA
##  [436]          NA          NA          NA          NA          NA
##  [441]          NA          NA          NA          NA          NA
##  [446]          NA          NA          NA          NA          NA
##  [451]          NA          NA          NA          NA          NA
##  [456]          NA          NA          NA          NA          NA
##  [461]          NA          NA          NA          NA          NA
##  [466]          NA          NA          NA          NA          NA
##  [471]          NA          NA          NA          NA          NA
##  [476]          NA          NA          NA          NA          NA
##  [481]          NA          NA          NA          NA          NA
##  [486]          NA          NA          NA          NA          NA
##  [491]          NA          NA          NA          NA          NA
##  [496]          NA          NA          NA          NA          NA
##  [501]          NA          NA          NA          NA          NA
##  [506]          NA          NA          NA          NA          NA
##  [511]          NA          NA          NA          NA          NA
##  [516]          NA          NA          NA          NA          NA
##  [521]          NA          NA          NA          NA          NA
##  [526]          NA          NA          NA          NA          NA
##  [531]          NA          NA          NA          NA          NA
##  [536]          NA          NA          NA          NA          NA
##  [541]          NA          NA          NA          NA          NA
##  [546]          NA          NA          NA          NA          NA
##  [551]          NA          NA          NA          NA          NA
##  [556]          NA          NA          NA          NA          NA
##  [561]          NA          NA          NA          NA          NA
##  [566]          NA          NA          NA          NA          NA
##  [571]          NA          NA          NA          NA          NA
##  [576]          NA          NA          NA          NA          NA
##  [581]          NA          NA          NA          NA          NA
##  [586]          NA          NA          NA          NA          NA
##  [591]          NA          NA          NA          NA          NA
##  [596]          NA          NA          NA          NA          NA
##  [601]          NA          NA          NA          NA          NA
##  [606]          NA          NA          NA          NA          NA
##  [611]          NA          NA          NA          NA          NA
##  [616]          NA          NA          NA          NA          NA
##  [621]          NA          NA          NA          NA          NA
##  [626]          NA          NA          NA          NA          NA
##  [631]          NA          NA          NA          NA          NA
##  [636]          NA          NA          NA          NA          NA
##  [641]          NA          NA          NA          NA          NA
##  [646]          NA          NA          NA          NA          NA
##  [651]          NA          NA          NA          NA          NA
##  [656]          NA          NA          NA          NA          NA
##  [661]          NA          NA          NA          NA          NA
##  [666]          NA          NA          NA          NA          NA
##  [671]          NA          NA          NA          NA          NA
##  [676]          NA          NA          NA          NA          NA
##  [681]          NA          NA          NA          NA          NA
##  [686]          NA          NA          NA          NA          NA
##  [691]          NA          NA          NA          NA          NA
##  [696]          NA          NA          NA          NA          NA
##  [701]          NA          NA          NA          NA          NA
##  [706]          NA          NA          NA          NA          NA
##  [711]          NA          NA          NA          NA          NA
##  [716]          NA          NA          NA          NA          NA
##  [721]          NA          NA          NA          NA          NA
##  [726]          NA          NA          NA          NA          NA
##  [731]          NA          NA          NA          NA          NA
##  [736]          NA          NA          NA          NA          NA
##  [741]          NA          NA          NA          NA          NA
##  [746]          NA          NA          NA          NA          NA
##  [751]          NA          NA          NA          NA          NA
##  [756]          NA          NA          NA          NA          NA
##  [761]          NA          NA          NA          NA          NA
##  [766]          NA          NA          NA          NA          NA
##  [771]          NA          NA          NA          NA          NA
##  [776]          NA          NA          NA          NA          NA
##  [781]          NA          NA          NA          NA          NA
##  [786]          NA          NA          NA          NA          NA
##  [791]          NA          NA          NA          NA          NA
##  [796]          NA          NA          NA          NA          NA
##  [801]          NA          NA          NA          NA          NA
##  [806]          NA          NA          NA          NA          NA
##  [811]          NA          NA          NA          NA          NA
##  [816]          NA          NA          NA          NA          NA
##  [821]          NA          NA          NA          NA          NA
##  [826]          NA          NA          NA          NA          NA
##  [831]          NA          NA          NA          NA          NA
##  [836]          NA          NA          NA          NA          NA
##  [841]          NA          NA          NA          NA          NA
##  [846]          NA          NA          NA          NA          NA
##  [851]          NA          NA          NA          NA          NA
##  [856]          NA          NA          NA          NA          NA
##  [861]          NA          NA          NA          NA          NA
##  [866]          NA          NA          NA          NA          NA
##  [871]          NA          NA          NA          NA          NA
##  [876]          NA          NA          NA          NA          NA
##  [881]          NA          NA          NA          NA          NA
##  [886]          NA          NA          NA          NA          NA
##  [891]          NA          NA          NA          NA          NA
##  [896]          NA          NA          NA          NA          NA
##  [901]          NA          NA          NA          NA          NA
##  [906]          NA          NA          NA          NA          NA
##  [911]          NA          NA          NA          NA          NA
##  [916]          NA          NA          NA          NA          NA
##  [921]          NA          NA          NA          NA          NA
##  [926]          NA          NA          NA          NA          NA
##  [931]          NA          NA          NA          NA          NA
##  [936]          NA          NA          NA          NA          NA
##  [941]          NA          NA          NA          NA          NA
##  [946]          NA          NA          NA          NA          NA
##  [951]          NA          NA          NA          NA          NA
##  [956]          NA          NA          NA          NA          NA
##  [961]          NA          NA          NA          NA          NA
##  [966]          NA          NA          NA          NA          NA
##  [971]          NA          NA          NA          NA          NA
##  [976]          NA          NA          NA          NA          NA
##  [981]          NA          NA          NA          NA          NA
##  [986]          NA          NA          NA          NA          NA
##  [991]          NA          NA          NA          NA          NA
##  [996]          NA          NA          NA          NA          NA
## [1001]          NA          NA          NA          NA          NA
## [1006]          NA          NA          NA          NA          NA
## [1011]          NA          NA          NA          NA          NA
## [1016]          NA          NA          NA          NA          NA
## [1021]          NA          NA          NA          NA          NA
## [1026]          NA          NA          NA          NA          NA
## [1031]          NA          NA          NA          NA          NA
## [1036]          NA          NA          NA          NA          NA
## [1041]          NA          NA          NA          NA          NA
## [1046]          NA          NA          NA          NA          NA
## [1051]          NA          NA          NA          NA          NA
## [1056]          NA          NA          NA          NA          NA
## [1061]          NA          NA          NA          NA          NA
## [1066]          NA          NA          NA          NA          NA
## [1071]          NA          NA          NA          NA          NA
## [1076]          NA          NA          NA          NA          NA
## [1081]          NA          NA          NA          NA          NA
## [1086]          NA          NA          NA          NA          NA
## [1091]          NA          NA          NA          NA          NA
## [1096]          NA          NA          NA          NA          NA
## [1101]          NA          NA          NA          NA          NA
## [1106]          NA          NA          NA          NA          NA
## [1111]          NA          NA          NA          NA          NA
## [1116]          NA          NA          NA          NA          NA
## [1121]          NA          NA          NA          NA          NA
## [1126]          NA          NA          NA          NA          NA
## [1131]          NA          NA          NA          NA          NA
## [1136]          NA          NA          NA          NA          NA
## [1141]          NA          NA          NA          NA          NA
## [1146]          NA          NA          NA          NA          NA
## [1151]          NA          NA          NA          NA          NA
## [1156]          NA          NA          NA          NA          NA
## [1161]          NA          NA          NA          NA          NA
## [1166]          NA          NA          NA          NA          NA
## [1171]          NA          NA          NA          NA          NA
## [1176]          NA          NA          NA          NA          NA
## [1181]          NA          NA          NA          NA          NA
## [1186]          NA          NA          NA          NA          NA
## [1191]          NA          NA          NA          NA          NA
## [1196]          NA          NA          NA          NA          NA
## [1201]          NA          NA          NA          NA          NA
## [1206]          NA          NA          NA          NA          NA
## [1211]          NA          NA          NA          NA          NA
## [1216]          NA          NA          NA          NA          NA
## [1221]          NA          NA          NA          NA          NA
## [1226]          NA          NA          NA          NA          NA
## [1231]          NA          NA          NA          NA          NA
## [1236]          NA          NA          NA          NA          NA
## [1241]          NA          NA          NA          NA          NA
## [1246]          NA          NA          NA          NA          NA
## [1251]          NA          NA          NA          NA          NA
## [1256]          NA          NA          NA          NA          NA
## [1261]          NA          NA          NA          NA          NA
## [1266]          NA          NA          NA          NA          NA
## [1271]          NA          NA          NA          NA          NA
## [1276]          NA          NA          NA          NA          NA
## [1281]          NA          NA          NA          NA          NA
## [1286]          NA          NA          NA          NA          NA
## [1291]          NA          NA          NA          NA          NA
## [1296]          NA          NA          NA          NA          NA
## [1301]          NA          NA          NA          NA          NA
## [1306]          NA          NA          NA          NA          NA
## [1311]          NA          NA          NA          NA          NA
## [1316]          NA          NA          NA          NA          NA
## [1321]          NA          NA          NA          NA          NA
## [1326]          NA          NA          NA          NA          NA
## [1331]          NA          NA          NA          NA          NA
## [1336]          NA          NA          NA          NA          NA
## [1341]          NA          NA          NA          NA          NA
## [1346]          NA          NA          NA          NA          NA
## [1351]          NA          NA          NA          NA          NA
## [1356]          NA          NA          NA          NA          NA
## [1361]          NA          NA          NA          NA          NA
## [1366]          NA          NA          NA          NA          NA
## [1371]          NA          NA          NA          NA          NA
## [1376]          NA          NA          NA          NA          NA
## [1381]          NA          NA          NA          NA          NA
## [1386]          NA          NA          NA          NA          NA
## [1391]          NA          NA          NA          NA          NA
## [1396]          NA          NA          NA          NA          NA
## [1401]          NA          NA          NA          NA          NA
## [1406]          NA          NA          NA          NA          NA
## [1411]          NA          NA          NA          NA          NA
## [1416]          NA          NA          NA          NA          NA
## [1421]          NA          NA          NA          NA          NA
## [1426]          NA          NA          NA          NA          NA
## [1431]          NA          NA          NA          NA          NA
## [1436]          NA          NA          NA          NA          NA
## [1441]          NA          NA          NA          NA          NA
## [1446]          NA          NA          NA          NA          NA
## [1451]          NA          NA          NA          NA          NA
## [1456]          NA          NA          NA          NA          NA
## [1461]          NA          NA          NA          NA          NA
## [1466]          NA          NA          NA          NA          NA
## [1471]          NA          NA          NA          NA          NA
## [1476]          NA          NA          NA          NA          NA
## [1481]          NA          NA          NA          NA          NA
## [1486]          NA          NA          NA          NA          NA
## [1491]          NA          NA          NA          NA          NA
## [1496]          NA          NA          NA          NA          NA
## [1501]          NA          NA          NA          NA          NA
## [1506]          NA          NA          NA          NA          NA
## [1511]          NA          NA          NA          NA          NA
## [1516]          NA          NA          NA          NA          NA
## [1521]          NA          NA          NA          NA          NA
## [1526]          NA          NA          NA          NA          NA
## [1531]          NA          NA          NA          NA          NA
## [1536]          NA          NA          NA          NA          NA
## [1541]          NA          NA          NA          NA          NA
## [1546]          NA          NA          NA          NA          NA
## [1551]          NA          NA          NA          NA          NA
## [1556]          NA          NA          NA          NA          NA
## [1561]          NA          NA          NA          NA          NA
## [1566]          NA          NA          NA          NA          NA
## [1571]          NA          NA          NA          NA          NA
## [1576]          NA          NA          NA          NA          NA
## [1581]          NA          NA          NA          NA          NA
## [1586]          NA          NA          NA          NA          NA
## [1591]          NA          NA          NA          NA          NA
## [1596]          NA          NA          NA          NA          NA
## [1601]          NA          NA          NA          NA          NA
## [1606]          NA          NA          NA          NA          NA
## [1611]          NA          NA          NA          NA          NA
## [1616]          NA          NA          NA          NA          NA
## [1621]          NA          NA          NA          NA          NA
## [1626]          NA          NA          NA          NA          NA
## [1631]          NA          NA          NA          NA          NA
## [1636]          NA          NA          NA          NA          NA
## [1641]          NA          NA          NA          NA          NA
## [1646]          NA          NA          NA          NA          NA
## [1651]          NA          NA          NA          NA          NA
## [1656]          NA          NA          NA          NA          NA
## [1661]          NA          NA          NA          NA          NA
## [1666]          NA          NA          NA          NA          NA
## [1671]          NA          NA          NA          NA          NA
## [1676]          NA          NA          NA          NA          NA
## [1681]          NA          NA          NA          NA          NA
## [1686]          NA          NA          NA          NA          NA
## [1691]          NA          NA          NA          NA          NA
## [1696]          NA          NA          NA          NA          NA
## [1701]          NA          NA          NA          NA          NA
## [1706]          NA          NA          NA          NA          NA
## [1711]          NA          NA          NA          NA          NA
## [1716]          NA          NA          NA          NA          NA
## [1721]          NA          NA          NA          NA          NA
## [1726]          NA          NA          NA          NA          NA
## [1731]          NA          NA          NA          NA          NA
## [1736]          NA          NA          NA          NA          NA
## [1741]          NA          NA          NA          NA          NA
## [1746]          NA          NA          NA          NA          NA
## [1751]          NA          NA          NA          NA          NA
## [1756]          NA          NA          NA          NA          NA
## [1761]          NA          NA          NA          NA          NA
## [1766]          NA          NA          NA          NA          NA
## [1771]          NA          NA          NA          NA          NA
## [1776]          NA          NA          NA          NA          NA
## [1781]          NA          NA          NA          NA          NA
## [1786]          NA          NA          NA          NA          NA
## [1791]          NA          NA          NA          NA          NA
## [1796]          NA          NA          NA          NA          NA
## [1801]          NA          NA          NA          NA          NA
## [1806]          NA          NA          NA          NA          NA
## [1811]          NA          NA          NA          NA          NA
## [1816]          NA          NA          NA          NA          NA
## [1821]          NA          NA          NA          NA          NA
## [1826]          NA          NA          NA          NA          NA
## [1831]          NA          NA          NA          NA          NA
## [1836]          NA          NA          NA          NA          NA
## [1841]          NA          NA          NA          NA          NA
## [1846]          NA          NA          NA          NA          NA
## [1851]          NA          NA          NA          NA          NA
## [1856]          NA          NA          NA          NA          NA
## [1861]          NA          NA          NA          NA          NA
## [1866]          NA          NA          NA          NA          NA
## [1871]          NA          NA          NA          NA          NA
## [1876]          NA          NA          NA          NA          NA
## [1881]          NA          NA          NA          NA          NA
## [1886]          NA          NA          NA          NA          NA
## [1891]          NA          NA          NA          NA          NA
## [1896]          NA          NA          NA          NA          NA
## [1901]          NA          NA          NA          NA          NA
## [1906]          NA          NA          NA          NA          NA
## [1911]          NA          NA          NA          NA          NA
## [1916]          NA          NA          NA          NA          NA
## [1921]          NA          NA          NA          NA          NA
## [1926]          NA          NA          NA          NA          NA
## [1931]          NA          NA          NA          NA          NA
## [1936]          NA          NA          NA          NA          NA
## [1941]          NA          NA          NA          NA          NA
## [1946]          NA          NA          NA          NA          NA
## [1951]          NA          NA          NA          NA          NA
## [1956]          NA          NA          NA          NA          NA
## [1961]          NA          NA          NA          NA          NA
## [1966]          NA          NA          NA          NA          NA
## [1971]          NA          NA          NA          NA          NA
## [1976]          NA          NA          NA          NA          NA
## [1981]          NA          NA          NA          NA          NA
## [1986]          NA          NA          NA          NA          NA
## [1991]          NA          NA          NA          NA          NA
## [1996]          NA          NA          NA          NA          NA
## [2001]          NA          NA          NA          NA          NA
## [2006]          NA          NA          NA          NA          NA
## [2011]          NA          NA          NA          NA          NA
## [2016]          NA          NA          NA          NA          NA
## [2021]          NA          NA          NA          NA          NA
## [2026]          NA          NA          NA          NA          NA
## [2031]          NA          NA          NA          NA          NA
## [2036]          NA          NA          NA          NA          NA
## [2041]          NA          NA          NA          NA          NA
## [2046]          NA          NA          NA          NA          NA
## [2051]          NA          NA          NA          NA          NA
## [2056]          NA          NA          NA          NA          NA
## [2061]          NA          NA          NA          NA          NA
## [2066]          NA          NA          NA          NA          NA
## [2071]          NA          NA          NA          NA          NA
## [2076]          NA          NA          NA          NA          NA
## [2081]          NA          NA          NA          NA          NA
## [2086]          NA          NA          NA          NA          NA
## [2091]          NA          NA          NA          NA          NA
## [2096]          NA          NA          NA          NA          NA
## [2101]          NA          NA          NA          NA          NA
## [2106]          NA          NA          NA          NA          NA
## [2111]          NA          NA          NA          NA          NA
## [2116]          NA          NA          NA          NA          NA
## [2121]          NA          NA          NA          NA          NA
## [2126]          NA          NA          NA          NA          NA
## [2131]          NA          NA          NA          NA          NA
## [2136]          NA          NA          NA          NA          NA
## [2141]          NA          NA          NA          NA          NA
## [2146]          NA          NA          NA          NA          NA
## [2151]          NA          NA          NA          NA          NA
## [2156]          NA          NA          NA          NA          NA
## [2161]          NA          NA          NA          NA          NA
## [2166]          NA          NA          NA          NA          NA
## [2171]          NA          NA          NA          NA          NA
## [2176]          NA          NA          NA          NA          NA
## [2181]          NA          NA          NA          NA          NA
## [2186]          NA          NA          NA          NA          NA
## [2191]          NA          NA          NA          NA          NA
## [2196]          NA          NA          NA          NA          NA
## [2201]          NA          NA          NA          NA          NA
## [2206]          NA          NA          NA          NA          NA
## [2211]          NA          NA          NA          NA          NA
## [2216]          NA          NA          NA          NA          NA
## [2221]          NA          NA          NA          NA          NA
## [2226]          NA          NA          NA          NA          NA
## [2231]          NA          NA          NA          NA          NA
## [2236]          NA          NA          NA          NA          NA
## [2241]          NA          NA          NA          NA          NA
## [2246]          NA          NA          NA          NA          NA
## [2251]          NA          NA          NA          NA          NA
## [2256]          NA          NA          NA          NA          NA
## [2261]          NA          NA          NA          NA          NA
## [2266]          NA          NA          NA          NA          NA
## [2271]          NA          NA          NA          NA          NA
## [2276]          NA          NA          NA          NA          NA
## [2281]          NA          NA          NA          NA          NA
## [2286]          NA          NA          NA          NA          NA
## [2291]          NA          NA          NA          NA          NA
## [2296]          NA          NA          NA          NA          NA
## [2301]          NA          NA          NA          NA
```

## Are there differences in activity patterns between weekdays and weekends?


