# This is a working code testing plotting a very large dataset at different geographic level i.e. country,state,city.
# The input data contains latitude and longitude with a aggregated metric attached the geographic location
# P.S : This is a work in progress and some of the visualizations are not very pretty

require(ggmap)
require(ggplot2)
require(maptools)
require(maps)
require(Hmisc)

require(limma)

getwd()
setwd('C:/Users/avenkata/Desktop/RStuff')

vti_usa_data <- read.columns("vti.csv", c("dept_stores_v1", "y", "x"), sep=",")
names(vti_usa_data)
describe(vti_usa_data$dept_stores_v1)

state_map = map_data("state")

#describe(state_map$long)
#describe(state_map$lat)

# Plot only contiguos USA 
#vti_usa_data = subset(vti_usa_data,vti_usa_data$dept_stores_v1 > 15)
#vti_usa_data = subset(vti_usa_data,vti_usa_data$dept_stores_v1 <=200)
vti_usa_data = subset(vti_usa_data,vti_usa_data$x >=-124.68)
vti_usa_data = subset(vti_usa_data,vti_usa_data$x <=-67.01)
vti_usa_data = subset(vti_usa_data,vti_usa_data$y >= 25.13)
vti_usa_data = subset(vti_usa_data,vti_usa_data$y <= 49.38)

vti_usa_data$vti_class = cut(vti_usa_data$dept_stores_v1,breaks=c(0,25,50,100,200,999))
describe(vti_usa_data$vti_class)

# Switching order in which data is plotted for better visibility of smaller sized datapoints.
# This can be especially useful when geom_point is size is varied based on the magnitude of the metric plotted
# Using the order of plotting with transparency and jitter functions can make data points more readable
 
vti_usa_data= vti_usa_data[order(-vti_usa_data$vti_class),]

vti_usa_data = subset(vti_usa_data,select = -dept_stores_v1)

# Test maps using GGPLOT

all_usa = ggplot(state_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(bg="white",colour="#E8E8E8")+
  geom_point(data=vti_usa_data,aes(x=x,y=y,group=NULL, fill=NULL,colour=vti_class),size=0.1,alpha = 5/10,position="jitter") + 
  #scale_colour_gradientn(colours = topo.colors(10))+
  #scale_colour_manual(values=c("0xD73027","0xF46D43","0xFDAE61","0xFEE08B","0xD9EF8B","0xA6D96A","0x66BD63","0x1A9850")) +
  #scale_colour_manual(values=c("#73B2FF","#9FD400","#996633","#FFAA00","#FF0000")) +
  scale_colour_manual(values=c("#27FF14","#F9FA16","#73B2FF","#117ABA","#FF0000")) +
  #scale_colour_manual(values=c("#9BFF0D","#E8D00C","#FFAA00","#E85F0C","#FF1104")) +
  #scale_colour_manual(values=c("0xFFF5F0","0xFEE0D2","0xFCBBA1","0xFC9272","0xFB6A4A","0xEF3B2C","0xCB181D","0x99000D")+
  #scale_size_manual(values = c(0.1,0.5,0.5,0.5,0.1)) +
  theme(#panel.background = element_rect(fill='#141414',colour='#141414'),
    panel.background = element_rect(fill='white',colour='white'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()) +    
  guides(colour = guide_legend(override.aes = list(alpha = 1,size=1))) 
                                 
#all_usa
ggsave(file="all_usa_v12.png", plot=all_usa)

all_usa = ggplot(state_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(bg="#000000",colour="#262626")+\
  geom_point(data=vti_usa_data,aes(x=x,y=y,group=NULL, fill=NULL,colour=vti_class,size=vti_class),alpha = 6/10,position="jitter") + 
  #scale_colour_gradientn(colours = topo.colors(10))+
  #scale_colour_manual(values=c("0xD73027","0xF46D43","0xFDAE61","0xFEE08B","0xD9EF8B","0xA6D96A","0x66BD63","0x1A9850")) +
  scale_colour_manual(values=c("#1A7CBA","#EBEB1C","#F972B4","#F76100","#EE0E10")) +
  #scale_colour_manual(values=c("0xFFF5F0","0xFEE0D2","0xFCBBA1","0xFC9272","0xFB6A4A","0xEF3B2C","0xCB181D","0x99000D")+
  scale_size_manual(values = c(0.1,0.2,0.3,0.4,0.5)) +
  theme(#panel.background = element_rect(fill='#141414',colour='#141414'),
        panel.background = element_rect(fill='white',colour='white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
#all_usa
ggsave(file="all_usa_v2.png", plot=all_usa)


all_usa_v2 = ggplot(vti_usa_data, aes(x=x,y=y, group=NULL)) +
  geom_point(data=vti_usa_data,aes(x=x,y=y,group=NULL, fill=NULL,colour=vti_class),size=0.1,alpha = 6/10,position="jitter") + 
  #scale_colour_gradientn(colours = topo.colors(10))+
  #scale_colour_manual(values=c("0xD73027","0xF46D43","0xFDAE61","0xFEE08B","0xD9EF8B","0xA6D96A","0x66BD63","0x1A9850")) +
  scale_colour_manual(values=c("#1A7CBA","#EBEB1C","#F972B4","#F76100","#EE0E10")) +
  #scale_colour_manual(values=c("0xFFF5F0","0xFEE0D2","0xFCBBA1","0xFC9272","0xFB6A4A","0xEF3B2C","0xCB181D","0x99000D")+
  theme(panel.background = element_rect(fill='#575757',colour='#575757'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
ggsave(file="all_usa_v5.png", plot=all_usa_v2)

test_map = ggplot(state_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(bg="white",colour="#DBDBDB")+
  theme(panel.background = element_rect(fill='white',colour='white'),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank())
test_map
