# Ensure that libcurl4-openssl-dev libssl-dev libfontconfig1-dev libcairo2-dev libxml2-dev packages (in Debian, or ther respective versions on other flavors) are installed
# Your json location history should be under data/raw
# https://martijnvanvreeden.nl/analysing-google-location-data/

library(jsonlite)
library(tidyverse)
library(lubridate)
library(anytime)
library(hrbrthemes)
library(ggmap)
library(geosphere)


install.packages(c("jsonlite",
"tidyverse",
"lubridate",
"anytime",
"hrbrthemes",
"ggmap"))


#load data
system.time(df_list <- fromJSON("data/raw/history_20210319/Location History/Location History.json"))
df <- df_list$location
str(df)
#maybe throw away inaccurate positions
mean(df$acc>60) #percent af acc worse than 60
df = df[!df$acc>60,
        c("timestampMs",
          "latitudeE7",
          "longitudeE7",
          "accuracy")]
#convert lat and long to decimal:
df$latitude=df$latitude/1e7
df$longitude=df$longitude/1e7

Budapest = c(longitude=19.03991,latitude=47.49801)

#getting distance from Budapest:
library(geosphere)
system.time(
distance<-mapply(function(lon, lat){distm(c(lon, lat),
                                Budapest,
                                fun=distHaversine)},
       lat=df$latitude,
       lon=df$longitude)
)
str(distance)
mean(distance>20000) #larger distance than 20 km
df$distance <- distance
df$inbp <- df$distance<20000
ggplot(df, aes(latitude, longitude, col=inbp)) +
  geom_point()

bp_coords = df[df$inbp,]
ggplot(bp_coords, aes(latitude, longitude, col=distance)) +
  geom_point()

ggplot(bp_coords[bp_coords$distance<7500,],
       aes(longitude, latitude, col=distance)) +
  geom_point() +
  scale_fill_distiller(palette=4, direction=-1)+
  stat_density_2d(aes(fill = ..level..),
                  geom = "polygon",
                  colour="white")


write.csv(bp_coords[bp_coords$distance<7500,],
        "data/edited/bp_coords.csv")