# This is a working code testing plotting a very large dataset at different geographic level i.e. country,state,city.
# The input data contains latitude and longitude with a aggregated metric attached the geographic location
# P.S : This is a work in progress and some of the visualizations are not very pretty

require(ggmap)
require(ggplot2)
require(maptools)
require(maps)
require(Hmisc)

getwd()
setwd('C:/Users/avenkata/Desktop/RStuff')

vti_ca_data <- read.csv('ca_vti_sample.csv',header=TRUE,na.strings='.')
names(vti_ca_data)
describe(vti_ca_data$specialty_v1)
#vti_ca_data$specialty_v1 <- cut(vti_ca_data$specialty_v1,breaks=c(0,25,50,75,100,125,150,175,1000))
vti_ca_data_v2 = subset(vti_ca_data,vti_ca_data$specialty_v1 <= 250)

# Plotting with no predefined breaks in the data
state_map = map_data("state","california")
test_map_v2 = ggplot(state_map, aes(x=long, y=lat, group=group)) +
  geom_point(data=vti_ca_data_v2,aes(x=x,y=y,group=NULL, fill=NULL,colour=specialty_v1),alpha = 2/10) + 
  scale_colour_gradientn(colours = rainbow(7))+
  geom_polygon(fill="transparent",bg="black",colour="black") 
test_map_v2

# Color ramping for the heat map
pal <- colorRampPalette(c("red","green","blue"))
colors <- pal(8)

# Custom binning of data
vti_ca_data$vti_class = cut(vti_ca_data$specialty_v1,breaks=c(0,50,100,150,200,1000))
vti_ca_data$size = c(0.1)

state_map = map_data("state","california")
test_map_v3 = ggplot(state_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(bg="#262626",colour="#00000030") +
  geom_point(data=vti_ca_data,aes(x=x,y=y,group=NULL, fill=NULL,colour=vti_class),size=0.1,alpha = 6/10,position="jitter") + 
  #scale_colour_gradientn(colours = topo.colors(10))+
  #scale_colour_manual(values=c("0xD73027","0xF46D43","0xFDAE61","0xFEE08B","0xD9EF8B","0xA6D96A","0x66BD63","0x1A9850")) +
  scale_colour_manual(values=c("#1A7CBA","#EBEB1C","#F972B4","#F76100","#EE0E10")) +
  #scale_colour_manual(values=c("0xFFF5F0","0xFEE0D2","0xFCBBA1","0xFC9272","0xFB6A4A","0xEF3B2C","0xCB181D","0x99000D")+
  theme(panel.background = element_rect(fill='#575757',colour='#575757'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
test_map_v3
ggsave(file="test_map_v5.png", plot=test_map_v3)

# More refined breaks in the data and increasing granularity 
# Map only the city of san francisco

vti_ca_data$specialty_v2 <- cut(vti_ca_data$specialty_v1,breaks=c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,1000))
str(vti_ca_data$specialty_v2)
center <- geocode('san francisco, california')
center <- as.numeric(center)
center
sf_map = get_googlemap('san francisco',zoom=12,maptype="hybrid")
test_map_v4 = ggmap(sf_map)
test_map_v4
test_map_v4 = test_map_v4 +
  geom_point(data=vti_ca_data_v2,aes(x=x,y=y,group=NULL, fill=NULL,color=specialty_v1),alpha = 2/10) + 
  scale_colour_gradientn(colours = rainbow(7))+
test_map_v4

# Testing different Map Types
sf_map = get_googlemap('san francisco',zoom=12,maptype="hybrid")
test_map_v5 = ggmap(sf_map)
vti_ca_data_v3 = subset(vti_ca_data_v2,x >= -122.4124 & x <= -122.3609  &  y >=37.70062  &  y<=37.81522)
test_map_v5 = test_map_v5 +
  geom_point(data=vti_ca_data_v3,aes(x=x,y=y,group=NULL, fill=NULL,colour=specialty_v1),alpha = 2/10) + 
  scale_colour_gradientn(colours = rainbow(7))
test_map_v5 