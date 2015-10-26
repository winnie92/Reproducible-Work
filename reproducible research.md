
Reproducible Research-Peer Assessment 2
---


Qustion
---
From the NOAA Storm Database, answer the following questions:  

- Across the United States, which types of weather events are most harmful with respect to population health?

- Across the United States, which types of weather events have the greatest economic consequences?  

Analyse Method and ideas
---
- first, pick up the relevant variables indicating the consequences of the two catogories--human injures and economical damages.  
- second, respectively do some statiscal measurements(which event contributes most of the influence on the human health and economy/which event contribute the greatest growth rate?) on the damages data by the groups of different events to indicate the influence of different types of weather events.  
- last, print out or plot out the results.  
 
Data Processing
---
- first, look into the code book and find out the relevant variables:  
1. EVTYPE = Type of storm event.  
2. PROPCASH = Combines the Property damage in whole numbers and hundredths and multiplier where Hundred (H), Thousand (K), Million (M), Billion (B) to create a numeric value.  
3. TOTCASH = combine the total Crop damage.  
4. FATALITIES = Number directly killed.  
5. INJURIES = Number directly injured.
6. BGN_DATE = the date of the event(can be used in calculating the growth rate)
- second, understand what we use.
EVTYPE--the categorical variable we are going to use,  PROPCASH + TOTCASH--the economical impact, FATALITIES + INJURIES--human health impact.  
- third, load the data in R, subset the relevant colums and make their classes and names right.  


```r
setwd("F:/")
data0<- read.csv("StormData.csv",stringsAsFactor=FALSE,header=TRUE,quote="")
names(data0)
```

```
##  [1] "X.STATE__."    "X.BGN_DATE."   "X.BGN_TIME."   "X.TIME_ZONE." 
##  [5] "X.COUNTY."     "X.COUNTYNAME." "X.STATE."      "X.EVTYPE."    
##  [9] "X.BGN_RANGE."  "X.BGN_AZI."    "X.BGN_LOCATI." "X.END_DATE."  
## [13] "X.END_TIME."   "X.COUNTY_END." "X.COUNTYENDN." "X.END_RANGE." 
## [17] "X.END_AZI."    "X.END_LOCATI." "X.LENGTH."     "X.WIDTH."     
## [21] "X.F."          "X.MAG."        "X.FATALITIES." "X.INJURIES."  
## [25] "X.PROPDMG."    "X.PROPDMGEXP." "X.CROPDMG."    "X.CROPDMGEXP."
## [29] "X.WFO."        "X.STATEOFFIC." "X.ZONENAMES."  "X.LATITUDE."  
## [33] "X.LONGITUDE."  "X.LATITUDE_E." "X.LONGITUDE_." "X.REMARKS."   
## [37] "X.REFNUM."
```

- we can see that we should pick up the relevant cols and clean up their names and classes.  


```r
data1 <- data0[c("X.BGN_DATE.","X.EVTYPE.","X.FATALITIES.","X.INJURIES.","X.PROPDMG.","X.PROPDMGEXP.","X.CROPDMG.","X.CROPDMGEXP.")]
str(data1)
```

```
## 'data.frame':	1773320 obs. of  8 variables:
##  $ X.BGN_DATE.  : chr  "4/18/1950 0:00:00" "4/18/1950 0:00:00" "2/20/1951 0:00:00" "6/8/1951 0:00:00" ...
##  $ X.EVTYPE.    : chr  "\"TORNADO\"" "\"TORNADO\"" "\"TORNADO\"" "\"TORNADO\"" ...
##  $ X.FATALITIES.: chr  "0.00" "0.00" "0.00" "0.00" ...
##  $ X.INJURIES.  : chr  "15.00" "0.00" "2.00" "2.00" ...
##  $ X.PROPDMG.   : chr  "25.00" "2.50" "25.00" "2.50" ...
##  $ X.PROPDMGEXP.: chr  "\"K\"" "\"K\"" "\"K\"" "\"K\"" ...
##  $ X.CROPDMG.   : chr  "0.00" "0.00" "0.00" "0.00" ...
##  $ X.CROPDMGEXP.: chr  "" "" "" "" ...
```

```r
names(data1) <- c("date","type","fatalities","injuries","property","proexp","crop","cropexp")

ty <- read.csv("type.txt")
ty1 <- as.character(ty[,1])
class(ty1)
```

```
## [1] "character"
```

```r
ty2 <- toupper(ty1)

for(i in 1:length(data1$type)) {
  r <- nchar(data1$type[i])-2
  data1$type[i] <- substr(data1$type[i],2,r)
}
```

```
## Error in nchar(data1$type[i]): invalid multibyte string, element 1
```

```r
data1$type[50:80]
```

```
##  [1] "TORNAD"   "TORNAD"   "TORNAD"   "TORNAD"   "TSTM WIN" "HAI"     
##  [7] "HAI"      "TSTM WIN" "HAI"      "TSTM WIN" "TSTM WIN" "HAI"     
## [13] "HAI"      "HAI"      "TSTM WIN" "TSTM WIN" "TSTM WIN" "HAI"     
## [19] "TORNAD"   "TSTM WIN" "TORNAD"   "TSTM WIN" "TSTM WIN" "TSTM WIN"
## [25] "HAI"      "TSTM WIN" "TORNAD"   "TORNAD"   "TORNAD"   "TORNAD"  
## [31] "TORNAD"
```

```r
for(i in 1:length(ty2)){
  data1$type <- ifelse(grepl(ty2[i],data1$type),ty2[i],data1$type)
}
data1$type[50:80]
```

```
##  [1] "TORNAD"   "TORNAD"   "TORNAD"   "TORNAD"   "TSTM WIN" "HAI"     
##  [7] "HAI"      "TSTM WIN" "HAI"      "TSTM WIN" "TSTM WIN" "HAI"     
## [13] "HAI"      "HAI"      "TSTM WIN" "TSTM WIN" "TSTM WIN" "HAI"     
## [19] "TORNAD"   "TSTM WIN" "TORNAD"   "TSTM WIN" "TSTM WIN" "TSTM WIN"
## [25] "HAI"      "TSTM WIN" "TORNAD"   "TORNAD"   "TORNAD"   "TORNAD"  
## [31] "TORNAD"
```

```r
ty3 <- as.factor(data1$type)
ty3[50:80]
```

```
##  [1] TORNAD   TORNAD   TORNAD   TORNAD   TSTM WIN HAI      HAI     
##  [8] TSTM WIN HAI      TSTM WIN TSTM WIN HAI      HAI      HAI     
## [15] TSTM WIN TSTM WIN TSTM WIN HAI      TORNAD   TSTM WIN TORNAD  
## [22] TSTM WIN TSTM WIN TSTM WIN HAI      TSTM WIN TORNAD   TORNAD  
## [29] TORNAD   TORNAD   TORNAD  
## 24021 Levels:  however a strong speed sheer existed.  Lifted indices were around -5\xb0 C with a nearly 80 kt wind speed in the middle levels.  Helicity values increased to around 350 M2/S2 in the pre-storm environment.  A line of thunderstorms fired up during the mid to late afternoon hours.  The storms quickly became severe as they moved into western Iowa.  The hardest hit area was the west central counties where many areas were pelted with large hail.  Monona and Crawford Counties were especially hard hit as a series of severe storms moved northeast along the main line of storms.  There were numerous reports of golf ball size hail.  One storm produced baseball size hail as it passed over the Ute area of Monona County.  Baseball size hail also fell a short time later east of Soldier.  WSR-88D radar VIL values with this storm were in the 60-65 Kg/M-2 range with the storm.  A deep meso circulation was detected over northern Shelby and southern Crawford Counties with one of the storms.  A tornado warning was issued.  A funnel cloud was reported east of Defiance in Shelby County.  It became a tornado briefly south of Manilla near the Shelby/Crawford County line.  It passed through fields ...
```

```r
length(levels(ty3))
```

```
## [1] 24021
```
- using for loops to summarize the factors in the data to 48 types from type.txt  














