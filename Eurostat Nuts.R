library(tidyverse)
library(stringr)
library(dplyr)
library(rgdal)
library(networkD3)
library(leaflet)
library(rnaturalearth) 
library(countrycode)
library(geosphere)
library(RColorBrewer)

rm(list = ls())
NutsData<-read_tsv('G:/Tran/Transport Networks Logistics/WP6/ITC+Transport Division/Rail SC2/2020 int rail passengers/tran_r_rapa.tsv')
#####1 basic data manipulation#####
NutsData$`2015`<-as.numeric(NutsData$`2015`)
colnames(NutsData)<-c("Variables","value2015","2010","2005")
Nuts<-separate(NutsData,Variables,c('unit','region1','region2','Country'),',')
Nuts$unit<-NULL
Nuts$`2010`<-NULL
Nuts$`2005`<-NULL
Nuts<-na.omit(Nuts)
Nuts<-separate(Nuts,region1,c('LoadingCountry','LoadingRegion'),2)
Nuts<-separate(Nuts,region2,c('UnloadingCountry','UnloadingRegion'),2)
International<-Nuts[Nuts$LoadingCountry!=Nuts$UnloadingCountry,]
#lets assume that countries can report what is coming into their country better than what is going out
Reporter<-International[International$UnloadingCountry==International$Country,]

#####2 Lets build simple country flow data first, with a sankey#####
CountryFlows<-Reporter %>%
  group_by(LoadingCountry,UnloadingCountry,Country) %>%
  summarize(totalcount=sum(value2015))
#CountryFlows<-filter(CountryFlows,totalcount>200000)
#above line is just for simplicity. Below line is to c
CountryFlows$Newvar<-ifelse(CountryFlows$LoadingCountry<CountryFlows$UnloadingCountry,
                            paste(CountryFlows$LoadingCountry,CountryFlows$UnloadingCountry),
                            paste(CountryFlows$UnloadingCountry,CountryFlows$LoadingCountry))

FinalValues<-CountryFlows %>%
  group_by(Newvar) %>%
  summarize(grandtotal=max(totalcount))

Split<-separate(FinalValues,Newvar,c("origin","destination")," ")
Split$origin<-countrycode(Split$origin,origin='eurostat',destination='country.name')
Split$destination<-countrycode(Split$destination,origin='eurostat',destination='country.name')
Split<-na.omit(Split)

name_vec <- c(unique(Split$origin), unique(Split$destination))
uniq<-unique(name_vec)
#the 0:13 below is based on limiting the sankey to  more than half a million passengers a day, change if needed
nodes <- data.frame(name = uniq, id = 0:42)
links <- Split %>%
  left_join(nodes, by = c('origin' = 'name')) %>%
  rename(origin_id = id) %>%
  left_join(nodes, by = c('destination' = 'name')) %>%
  rename(dest_id = id)

sankeyNetwork(Links=links,Nodes=nodes,Source='origin_id',Target="dest_id",Value='grandtotal',NodeID='name',fontSize=16)

#####3 Now let's put it on a map!#####
#code stolen from http://personal.tcu.edu/kylewalker/interactive-flow-visualization-in-r.html
countries <- ne_countries()
countries$o_longitude<-coordinates(countries)[,1]
countries$o_latitude<-coordinates(countries)[,2]
countries_xy <- countries@data %>%
  select(admin, o_longitude, o_latitude)

#to fix the czech republic/czechia problem
countries_xy[41,1]<-"Czechia"
countries_xy[19,1]<-"Bosnia & Herzegovina"
#countries_xy[177,]<-c("Liechtenstein",9.5554,47.1660)
countries_xy[104,1]<-"North Macedonia"
countries_xy[148,1]<-"Serbia"

df3<-Split %>%
  left_join(countries_xy, by = c('origin' = 'admin'))

df3<-df3 %>%
  left_join(countries_xy,by= c('destination'='admin'))
colnames(df3)<-c("origin","destination","Passengers","o_longitude","o_latitude","d_longitude","d_latitude")

df3<-na.omit(df3)
df3$o_longitude<-as.numeric(df3$o_longitude)
df3$d_longitude<-as.numeric(df3$d_longitude)
flows <- gcIntermediate(df3[,4:5], df3[,6:7], sp = TRUE, addStartEnd = TRUE)
flows$counts <- df3$Passengers
flows$origins <- df3$origin
flows$destinations <- df3$destination

hover <- paste0(flows$origins, " to ", 
                flows$destinations, ': ', 
                as.character(flows$counts))

pal <- colorFactor(brewer.pal(4, 'Set2'), flows$origins)

leaflet() %>%
  addProviderTiles('CartoDB.Positron') %>%
  addPolylines(data = flows, weight = ~counts/200000, label = hover, fillOpacity = 4,
               group = ~origins, color = ~pal(origins)) %>%
  addLayersControl(overlayGroups = unique(flows$origins), 
                   options = layersControlOptions(collapsed = FALSE))

