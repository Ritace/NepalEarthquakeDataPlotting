#this file going to display the casaulties map 
allCasaulties <- read.csv(file = "./data/earthquakecasultiesdata.csv")
library(plyr)
library(rgeos)
library(maptools)
library(gpclib)
library(ggplot2)
library(scales)
library(RColorBrewer)
# MAP
np_dist <- readShapeSpatial("data/NPL_adm/NPL_adm3.shp")
# VERIFY IT LOADED PROPERLY
np_dist <- fortify(np_dist, region = "NAME_3")
# print(head(allCasaulties))
  # print(np_dist$id)
  ##### checking is the provided data maps the district name if not change to capital letters
gs.pal <- colorRampPalette(c("#FFFFFF","#000000"),bias=.1,space="rgb")
#this np_dist is sPdf format map
district <-as.character(allCasaulties[[3]][4:75])
# append(district,c("Baitadi","Dadeldhura","Kanchanpur"))
district[73] <- "Baitadi"
district[74] <- "Dadeldhura"
district[75] <- "Kanchanpur"

casaulties<-(allCasaulties[[5]][4:75])
casaulties<-as.numeric(levels(casaulties))[casaulties]
casaulties[73]<-0
casaulties[74]<-0
casaulties[75]<-0
# print(mean(casaulties))
print(casaulties)
data_dead = data.frame(district = district, dead = casaulties)
#here plotting map of nepal is done by download a shape file which loaded and then can be combined with ggplot :D
#downloaded from gadm.org
#calculates centroid of the districts
distcenters <- ddply(np_dist, .(id), summarize, clat = mean(lat), clong = mean(long))
mp<-ggplot() + geom_map(data = data_dead, aes(map_id = district, fill = dead), 
                    map = np_dist,color = "black") + expand_limits(x = np_dist$long, y = np_dist$lat)+
  labs(x = "Lat", y = "Long",title="Nepal Earthquake Death Tolls")+
  theme(axis.title = element_text(size = 15),axis.title.x = element_text(size = 12, ),axis.title.y = element_text(size = 12))+
  guides(fill=guide_colorbar(barwidth = 0.3, barheight = 15))+
  scale_fill_gradientn(colours=c("#04C7E2","#0624FF","#5FFE45","#146F05","#DEF416","#F4B116","#F49616","#FF0606","#A11212"),guide = "colourbar",
                       values=rescale(c(0,20,50,100,500,1000,3000,4634)),limits=c(0,5000),na.value = "white",
                       breaks=c(0,20,50,100,500,1000,3000,4634))
#   scale_fill_gradient2(low = ("#032256"), mid = ("white"),
#                        high =muted("#F8808C"),midpoint = 2000, limits = c(0, 4700),breaks=c(100,500,1000,1500,2500,3000,4700))
  
  
#   geom_text(data = distcenters, aes(x = clong, y = clat, label = id, size = 0.02))
  ####this map_id is the mapping field to in the spdf format map
####expand limits making sure the boundariesare correct
print(mp)
# print(head(np_dist))
####this last part is very important to fill the holes created in the map 
# namesInData <- (district)
# namesInMap <- levels(factor(np_dist$id))
# 
# print(namesInData[which(!namesInData %in% namesInMap)])
# 
# print(namesInMap[which(!namesInMap %in% namesInData)])


