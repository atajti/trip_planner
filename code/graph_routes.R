# oh hey! just search for trips containing the starting point,
# trips containing the endpoint,
# create a graph and get the shortest path,
# get time of each trip-part, then add them together.

library(data.table)
library(igraph)
lat_chg_per_meter = 8.983152840696227042251e-06 # (see get_distances.R)
long_chg_per_meter = 1.329383083419093512941e-05 # (see get_distances.R)
trips <- fread("data/edited/trips.csv")
trips[,`:=`(inbp=NULL,
            distance=NULL,
            range_before=NULL,
            range_after=NULL)]
origin <- c(latitude=47.528242, longitude=19.026056)
target <- c(latitude=47.515176, longitude=19.078548)



get_containing_trips <- function(data, point,
                                 max_distance=50){
    return(
      data[
        data$latitude<=point["latitude"]+(max_distance*lat_chg_per_meter) &
        data$latitude>=point["latitude"]-(max_distance*lat_chg_per_meter) &
        data$longitude<=point["longitude"]+(max_distance*long_chg_per_meter) &
        data$longitude>=point["longitude"]-(max_distance*long_chg_per_meter),
        unique(trip)])
}

relevant_trips <- unique(c(get_containing_trips(trips, origin),
                           get_containing_trips(trips, target)))
plot(x = trips[trip %in% relevant_trips,
               longitude],
     y = trips[trip %in% relevant_trips,
               latitude],
     col=trips[trip %in% relevant_trips,
               as.integer(as.factor(trip))])
points(x = c(origin["longitude"], target["longitude"]),
       y = c(origin["latitude"], target["latitude"]),
       pch=4,
       cex=5,
       col="red")



# create edgelist based on trips