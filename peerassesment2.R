---
title: "PeerAssesment2"
output: html_document


````
library(R.utils)
library(ggplot2)


```{r}

if (! file.exists("C:/Users/538321/Documents/DataManagement/ReproducibleResearch/assignment2/repdata_data_StormData.csv.bz2")){
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2")
  
}

if (!file.exists("repdata_data_StormData.csv")){
  bunzip2("C:/Users/538321/Documents/DataManagement/ReproducibleResearch/assignment2/repdata_data_StormData.csv.bz2", overwrite = "FALSE", remove = FALSE)
}
stormData <- read.csv("C:/Users/538321/Documents/DataManagement/ReproducibleResearch/assignment2/repdata_data_StormData.csv")
# storm <- read.csv(bzfile("C:/Users/538321/Documents/DataManagement/ReproducibleResearch/assignment2/repdata_data_StormData.csv.bz2"))                            

```
```{r}
head(stormData)
```
```{r}



str(stormData)

colOfInterest <- c("EVTYPE","FATALITIES", "INJURIES", "PROPDMG","PROPDMGEXP",
"CROPDMG", "CROPDMGEXP" )

eventAndFatality <- stormData[colOfInterest]

head(eventAndFatality)
unique(eventAndFatality$PROPDMGEXP)

eventAndFatality$PROPEXP[eventAndFatality$PROPDMGEXP == "K"] <- 1000

eventAndFatality$PROPEXP[eventAndFatality$PROPDMGEXP == "M"] <- 1e+06


eventAndFatality$PROPEXP[eventAndFatality$PROPDMGEXP == ""] <- 1

eventAndFatality$PROPEXP[eventAndFatality$PROPDMGEXP == "B"] <- 1e+09

eventAndFatality$PROPEXP[eventAndFatality$PROPDMGEXP == "m"] <- 1
eventAndFatality$PROPEXP[eventAndFatality$PROPDMGEXP == "+"] <- 0
eventAndFatality$PROPEXP[eventAndFatality$PROPDMGEXP == "O"] <- 1
eventAndFatality$PROPEXP[eventAndFatality$PROPDMGEXP == "5"] <- 1e+05
eventAndFatality$PROPEXP[eventAndFatality$PROPDMGEXP == "6"] <- 1e+6
eventAndFatality$PROPEXP[eventAndFatality$PROPDMGEXP == "4"] <- 10000
eventAndFatality$PROPEXP[eventAndFatality$PROPDMGEXP == "2"] <- 100
eventAndFatality$PROPEXP[eventAndFatality$PROPDMGEXP == "3"] <- 1000
eventAndFatality$PROPEXP[eventAndFatality$PROPDMGEXP == "h"] <- 100
eventAndFatality$PROPEXP[eventAndFatality$PROPDMGEXP == "7"] <- 1e+7
eventAndFatality$PROPEXP[eventAndFatality$PROPDMGEXP == "H"] <- 100
eventAndFatality$PROPEXP[eventAndFatality$PROPDMGEXP == "-"] <- 0
eventAndFatality$PROPEXP[eventAndFatality$PROPDMGEXP == "1"] <- 100
eventAndFatality$PROPEXP[eventAndFatality$PROPDMGEXP == "8"] <- 1e+08
eventAndFatality$PROPEXP[eventAndFatality$PROPDMGEXP == "?"] <- 0

eventAndFatality$PROPDMGVAL <- eventAndFatality$PROPEXP*eventAndFatality$PROPDMG

```````

-----
Assess Crop Damage
----


```{r}
unique(eventAndFatality$CROPDMGEXP)
eventAndFatality$CROPEXP[eventAndFatality$CROPDMGEXP == "M"] <- 1e+06
eventAndFatality$CROPEXP[eventAndFatality$CROPDMGEXP == "K"] <- 1000
eventAndFatality$CROPEXP[eventAndFatality$CROPDMGEXP == "m"] <- 1
eventAndFatality$CROPEXP[eventAndFatality$CROPDMGEXP == "B"] <- 1e+09
eventAndFatality$CROPEXP[eventAndFatality$CROPDMGEXP == "?"] <- 0
eventAndFatality$CROPEXP[eventAndFatality$CROPDMGEXP == "2"] <- 100
eventAndFatality$CROPEXP[eventAndFatality$CROPDMGEXP == "O"] <- 1
eventAndFatality$CROPEXP[eventAndFatality$CROPDMGEXP == ""] <- 1
eventAndFatality$CROPEXP[eventAndFatality$CROPDMGEXP == "k"] <- 1000

eventAndFatality$CROPDMGVAL <- eventAndFatality$CROPEXP*eventAndFatality$CROPDMG



```

---
Aggregate Data By Event
----
```{r}

fatalities <- aggregate(FATALITIES~EVTYPE, data=eventAndFatality, FUN = sum )
injury <- aggregate(INJURIES~EVTYPE, data=eventAndFatality, FUN = sum)
propdmg <- aggregate(PROPDMG ~EVTYPE, data=eventAndFatality, FUN = sum)
cropdmg <- aggregate(CROPDMG ~EVTYPE, data=eventAndFatality, FUN = sum)

fatalitiesByOrder <- fatalities[order(-fatalities$FATALITIES),]
##Injuries in Ascending Order
InjuriesByOrder <- injury[order(-injury$INJURIES),]
topTenInjuries <- InjuriesByOrder[1:10,]
topTenFatalities <- fatalitiesByOrder[1:10,]

par(mfrow = c(1,2), mar = c(12,4,3,4), mgp= c(3,1,0), cex = 0.9)
barplot(topTenFatalities$FATALITIES, names.arg = topTenFatalities$EVTYPE, las = 3,  main = "Weather Event Realated Fatalities", ylab = "Number of Fatalities")

barplot(topTenInjuries$INJURIES, names.arg = topTenInjuries$EVTYPE, las = 3,  main = "Weather Event Realated INJURIES", ylab = "Number of INJURIES")

```
----
Across the United States, Which types of events have greates property damage
--------------------
```{r}

propdmg <- aggregate(PROPDMG ~EVTYPE, data=eventAndFatality, FUN = sum)
cropdmg <- aggregate(CROPDMG ~EVTYPE, data=eventAndFatality, FUN = sum)

propertyDamageByOrder <- propdmg[order(-propdmg$PROPDMG),]
topTenPropertyDamages <- propertyDamageByOrder[1:10,]

cropDamageByOrder <- cropdmg[order(-cropdmg$CROPDMG),]
topTenCropDamages <- cropDamageByOrder[1:10,]

par(mfrow = c(1,2), mar = c(5,3,2,0), mgp= c(4,1,0), cex = 0.9)
barplot(topTenPropertyDamages$PROPDMG, names.arg = topTenPropertyDamages$EVTYPE, las = 3,  main = "Weather Event Realated Property Damages", ylab = "Number of Property Damages")

barplot(topTenCropDamages$CROPDMG, names.arg = topTenCropDamages$EVTYPE, las = 3,  main = "Weather Event Realated Crop Damages", ylab = "Number of CROP DAMAGES")

```