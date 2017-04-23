library(ggmap)
library(readr)
library(lubridate)

qmap(location = "boston university") 
qmap(location = "boston university", zoom = 14) 
qmap(location = "boston university", zoom = 14, source = "osm")
qmap(location = "Lima", zoom = 16, source = "osm") 
map <- get_stamenmap(c(-74.1, 40.55, -73.8, 40.85), zoom=12,
                     maptype='toner')
ggmap(map)

map <- get_stamenmap(c(12.04, 77.00, 11.8, 77.45), zoom=12,
                     maptype='toner')
ggmap(map)
peruLocations <- data.frame()
peruLocations <- rbind(peruLocations, geocode("Lima, Peru"))
peruLocations

map <- get_map("Peru", zoom=6, color="color", source="google", maptype="terrain")
myMap <- ggmap(map, extent="device", legend="top")
myMap + geom_point(aes(x=as.numeric(lon), y=as.numeric(lat)), col="white", fill="dodgerblue", data=peruLocations, shape=21, size=6)

url <- "http://earthquake.usgs.gov/fdsnws/event/1/query.csv?starttime=2010-1-1%2000%3A00%3A00&endtime=2016-11-25%2023%3A59%3A59&maxlatitude=1.889&minlatitude=-18.896&maxlongitude=-68.071&minlongitude=-83.276&minmagnitude=2.5&eventtype=earthquake&orderby=time"
geonet <- read.csv(url, stringsAsFactors = FALSE)
geonet$Date <- as.Date(geonet$time, format = "%Y-%m-%dT%H:%M:%S", tz = "America/Lima")
geonet$Year <- year(geonet$Date)

library(jpeg)
library(png)
library(plyr)
Peru.map <- get_stamenmap(bbox = c(left = -83.276, bottom = -25.896, right = -60.071, 
                                   top = 5), zoom = 6, maptype = "toner")
Peru.map
p <- ggmap(Peru.map)
p
p <- p + stat_density_2d(bins = 20, geom = "polygon", size = 2, data = geonet, 
                         aes(x = longitude, y = latitude, alpha = ..level.., fill = ..level..))
p
p <- p + scale_fill_gradient(low = "yellow", high = "red", guide = FALSE) + 
  scale_alpha(range = c(0.02, 0.8), guide = FALSE) + xlab("") + ylab("")
p

p <- ggmap(Peru.map)
p <- p + geom_point(data = subset(geonet, mag >= 4), aes(x = longitude, y = latitude, 
                                                         color = mag), alpha = 0.5) + scale_color_continuous(low = "yellow", high = "red")
p




