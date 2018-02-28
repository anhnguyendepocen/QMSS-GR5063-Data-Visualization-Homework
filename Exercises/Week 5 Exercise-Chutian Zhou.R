#Replication Codes for Week 5 Exercise of Data Visualization
#Chutian Zhou 2/27/2018

#---------------------------------------------------------------------------#

#World Development Indicators (WDI)

install.packages("WDI")
library(WDI)
library(dplyr)
library(ggplot2)
install.packages("countrycode")
library(countrycode)
library(stringr)
install.packages("ggrepel")
library(ggrepel)

#Select the data

WDIsearch('health expenditure')
df = WDI(indicator = "SH.XPD.TOTL.ZS" ,
         start = 2014, end = 2014, extra = F)
df = df %>% filter(!is.na(SH.XPD.TOTL.ZS))
df<-df[-c(1:46),]
df <- dplyr::rename(df, code = iso2c, expgdp = SH.XPD.TOTL.ZS)

#Questions

#(1)
library(ggthemes)
install.packages("ggmap")
library(ggmap)
world <- map_data("world")
world <- dplyr::rename(world, country=region)
world$subregion = NULL
world$country <- str_to_title(world$country)
world <- mutate(world, code = countrycode(world$country, "country.name", "iso2c"))

world.merged<-left_join(world, df, by = "code")

ggplot(world.merged,aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill = expgdp),color="white")+
  coord_map()+theme_map()+
  scale_fill_gradientn(colours=c("#fee8c8","#fdbb84","#e34a33"))

#(2)
world.merged$region<-countrycode(world.merged$code,'iso2c','continent')

ggplot(world.merged,aes(x=long,y=lat,group=group,fill = expgdp))+
  geom_polygon(color="white")+
  coord_map()+theme_classic()+
  facet_wrap(~region)+
  scale_fill_gradientn(colours=c("#fee8c8","#fdbb84","#e34a33"))

#---------------------------------------------------------------------------------------#

#Locations of Fortune 500 companies

#Addresses
library(XML)
library(RCurl)
fortune500_url <- getURL("https://www.geolounge.com/fortune-500-list-by-state-for-2015/",.opts = list(ssl.verifypeer = FALSE) )  # We needs this because the site is https
fortune500 = readHTMLTable(fortune500_url, header = TRUE, which = 1)
colnames(fortune500) <- tolower(colnames(fortune500))
fortune500 <- subset(fortune500, select=c("company","streetadd","place","state","zip"))
write.csv(fortune500, "fortune500.csv")

#Task 1
library(dplyr)
library(tidyverse)
library(ggthemes)

bystate<-fortune500%>%group_by(state)%>%summarise(count=n())
bystate$state<-as.character(bystate$state)
bystate[8,1]="District Of Columbia"

us.states<-map_data("state")
us.states <-as_data_frame(us.states)
us.states<-rename(us.states, state = region)
us.states$state<-str_to_title(us.states$state)
us.states<-left_join(us.states,bystate,by="state")
us.states<-us.states[,-6]
us.states[is.na(us.states)]<-0

ggplot(us.states,aes(x = long, y = lat, group=group)) + 
  geom_polygon(aes(fill = count), color="white")+theme_map()+
  coord_map(projection = "mercator")+
  scale_fill_gradientn(colours=c("gray88","lightblue3","deepskyblue1","dodgerblue3"))

#Task 2
library(readxl)
tax<-read_excel("State_Corporate_Income_Tax_Rates_2015.xlsx",sheet=2)
tax[51,1]="District Of Columbia"

us.states<-left_join(us.states,tax,by="state")

ggplot(us.states,aes(x = long, y = lat, group=group)) + 
  geom_polygon(aes(fill = topcorpinctax), color="white")+theme_map()+
  coord_map(projection = "mercator")+
  scale_fill_gradientn(colours = terrain.colors(10))

#Task 3
#(1)
tax_count=us.states[!duplicated(us.states$state),]

ggplot(tax_count,aes(topcorpinctax,count))+geom_point(size=4)+
  theme_classic()+geom_smooth(method=loess,size=1.5)+
  scale_y_continuous(name="Count",limits=c(0,40))+
  scale_x_continuous(name="Corporate Income Tax Rates")

#(2)
#Don't know how to import online table...

#Task 4

#(1)
fortune500$address<-paste(fortune500$streetadd, 
                          fortune500$place, 
                          fortune500$state, 
                          fortune500$zip) #create a new column with all previous column's info
geocodes <- geocode(fortune500$address, output = "latlon" , source = "google")

fortune500$lon <- geocodes$lon
fortune500$lat <- geocodes$lat

ggplot(us.states,aes(x = long, y = lat, group=group)) + 
  geom_polygon(aes(fill = topcorpinctax), color="white")+theme_map()+
  coord_map(projection = "mercator")+
  scale_fill_gradientn(colours = terrain.colors(10))+
  geom_point(aes(x=lon,y=lat),data=fortune500,size=3,alpha=0.3,col="red",inherit.aes = FALSE)

#(2)
ggplot(us.states,aes(x = long, y = lat, group=group)) + 
  geom_polygon(aes(fill = topcorpinctax), color="white")+theme_map()+
  coord_map(projection = "mercator")+
  scale_fill_gradientn(colours = terrain.colors(10))+
  geom_point(aes(lon,lat),data=fortune500,size=3,alpha=0.3,col="red",inherit.aes = FALSE)+
  geom_text_repel(aes(label=company,x=lon,y=lat),data=fortune500,size=3,alpha=0.3,col="black",inherit.aes = FALSE,direction = "y")

#(3)
fortune500$rank<-as.numeric(rownames(fortune500))

ggplot(us.states,aes(x = long, y = lat, group=group)) + 
  geom_polygon(aes(fill = topcorpinctax), color="white")+theme_map()+
  coord_map(projection = "mercator")+
  scale_fill_gradientn(colours = terrain.colors(10))+
  geom_point(aes(lon,lat,size=rank),data=fortune500,alpha=0.3,col="red",inherit.aes = FALSE)+
  geom_text_repel(aes(label=company,x=lon,y=lat),data=fortune500,size=3,alpha=0.3,col="black",inherit.aes = FALSE,direction = "y")