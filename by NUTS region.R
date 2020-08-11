library(tidyverse)
library(stringr)
library(dplyr)
library(rgdal)
library(networkD3)
library(leaflet)
library(rnaturalearth) 
library(rgeos)
library(geosphere)
library(RColorBrewer)
library(sp)
library(sf)

rm(list = ls())
#next bit of code is needed later for NUTS centroids
temp <- tempfile(fileext = ".zip")
download.file("http://ec.europa.eu/eurostat/cache/GISCO/geodatafiles/NUTS_2013_01M_SH.zip", tempfile(fileext = ".zip"))
unzip(temp)
map_nuts2 <- readOGR(dsn = "./NUTS_2013_01M_SH/data", layer = "NUTS_RG_01M_2013") %>%
  subset(STAT_LEVL_ == 2)


NutsData<-read_tsv('G:/Tran/Transport Networks Logistics/WP6/ITC+Transport Division/Rail SC2/2020 int rail passengers/tran_r_rapa.tsv')
#####basic data manipulation#####
NutsData$`2015`<-as.numeric(NutsData$`2015`)
colnames(NutsData)<-c("Variables","value2015","2010","2005")
NutsData<-separate(NutsData,Variables,c('unit','region1','region2','Country'),',')
NutsData$unit<-NULL
NutsData$`2010`<-NULL
NutsData$`2005`<-NULL
NutsData<-na.omit(NutsData)
NutsData<-separate(NutsData,region1,c('LoadingCountry','LoadingRegion'),2)
NutsData<-separate(NutsData,region2,c('UnloadingCountry','UnloadingRegion'),2)
IntNuts<-NutsData[NutsData$LoadingCountry!=NutsData$UnloadingCountry,]
testx<-filter(IntNuts,LoadingCountry=="BY"|UnloadingCountry=="BY"|LoadingCountry=="RU"|UnloadingCountry=="RU")
IntNuts<-filter(IntNuts,value2015>100000)

#Below liner assumes the importing country has better data than the exporting country. possibly not???
IntNuts<-IntNuts[IntNuts$UnloadingCountry==IntNuts$Country,]
IntNuts$NewVar<-ifelse(IntNuts$LoadingCountry<IntNuts$UnloadingCountry,
                       paste(IntNuts$LoadingCountry,IntNuts$LoadingRegion,IntNuts$UnloadingCountry,IntNuts$UnloadingRegion),
                       paste(IntNuts$UnloadingCountry,IntNuts$UnloadingRegion,IntNuts$LoadingCountry,IntNuts$LoadingRegion))
IntNuts$NewVar<-gsub(" ", "", IntNuts$NewVar, fixed = TRUE)
IntNuts<-IntNuts %>%
  group_by(NewVar) %>%
  summarize(maxpass=max(value2015)*2)

IntNuts<-separate(IntNuts,NewVar,c('LoadingNUTS','UnloadingNUTS'),4)
IntNuts[IntNuts$UnloadingNUTS=="SEXX",2]<-"SE11"
IntNuts[IntNuts$UnloadingNUTS=="PLXX",2]<-"PL11"
IntNuts[IntNuts$UnloadingNUTS=="ITXX",2]<-"ITI4"
IntNuts[IntNuts$UnloadingNUTS=="NLXX",2]<-"NL32"
IntNuts[IntNuts$UnloadingNUTS=="FRXX",2]<-"FR10"
IntNuts[IntNuts$UnloadingNUTS=="HUXX",2]<-"HU10"
IntNuts[IntNuts$UnloadingNUTS=="PTXX",2]<-"PT17"

IntNuts[IntNuts$LoadingNUTS=="ESXX",1]<-"ES30"
IntNuts[IntNuts$LoadingNUTS=="ATXX",1]<-"AT11"
IntNuts[IntNuts$LoadingNUTS=="DEXX",1]<-"DE11"
IntNuts[IntNuts$LoadingNUTS=="FRXX",1]<-"FR10"
IntNuts[IntNuts$LoadingNUTS=="RU",1]<-"RU01"
IntNuts[IntNuts$UnloadingNUTS=="RU",2]<-"RU01"

IntNuts<-filter(IntNuts,LoadingNUTS!="CHXX")
IntNuts$LoadingNUTS<-gsub("XX", "01", IntNuts$LoadingNUTS, fixed = TRUE)
IntNuts$UnloadingNUTS<-gsub("XX", "01", IntNuts$UnloadingNUTS, fixed = TRUE)
IntNuts$LoadingNUTS<-gsub("LU", "LU00", IntNuts$LoadingNUTS, fixed = TRUE)
IntNuts$UnloadingNUTS<-gsub("LU", "LU00", IntNuts$UnloadingNUTS, fixed = TRUE)
IntNuts$LoadingNUTS<-gsub("BYLT", "BY01", IntNuts$LoadingNUTS, fixed = TRUE)
IntNuts[IntNuts$LoadingNUTS=="BY01",2]<-"LT00"

centroids <- getSpPPolygonsLabptSlots(map_nuts2)
centroids<-as.tibble(centroids)
centroids$ID<-map_nuts2$NUTS_ID
#add coordinates for St bPetersburg 9for Finland)
centroids<-add_row(centroids,V1=30.3609,V2=59.9311,ID="RU01")
centroids<-add_row(centroids,V1=27.559,V2=53.9006,ID="BY01")

IntNuts<-merge(IntNuts,centroids,by.x="LoadingNUTS",by.y="ID")
IntNuts<-merge(IntNuts,centroids,by.x="UnloadingNUTS",by.y="ID")
colnames(IntNuts)<-c("LoadingNUTS","UnloadingNUTS","maxpass","o_long","o_lat","d_long","d_lat")

flows <- gcIntermediate(IntNuts[,4:5], IntNuts[,6:7], sp = TRUE, addStartEnd = TRUE)
flows$counts <- IntNuts$maxpass
flows$origins <- IntNuts$LoadingNUTS
flows$destinations <- IntNuts$UnloadingNUTS
hover <- paste0(flows$origins, " to ", 
                flows$destinations, ': ', 
                as.character(flows$counts))

pal <- colorFactor(brewer.pal(4, 'Set2'), flows$origins)
leaflet() %>%
  addProviderTiles('CartoDB.Positron') %>%
  addPolylines(data = flows, weight = ~(counts/10000)^0.5, label = hover, fillOpacity = 4,
               group = ~origins, color = ~pal(origins)) %>%
  addLayersControl(overlayGroups = unique(flows$origins), 
                   options = layersControlOptions(collapsed = FALSE))

filter(IntNuts,UnloadingCountry=="BG"&value2015>50000)

