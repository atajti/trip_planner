# get trip intersections (close points of different trips)
# to find plausible routes between points
library(data.table)
lat_chg_per_meter = 8.983152840696227042251e-06 # (see get_distances.R)
long_chg_per_meter = 1.329383083419093512941e-05 # (see get_distances.R)
trip_points <- fread("data/edited/trips_2.csv")[order(timestamp)]

# check for hints to get default distance for same-trip distance
tripwise_mean_distances <- trip_points[order(timestamp),
  .(mean_dist=mean(sqrt(((latitude-shift(latitude))/lat_chg_per_meter)^2 +
                        ((longitude-shift(longitude))/long_chg_per_meter)^2),
                   na.rm=TRUE),
    min_dist=min(sqrt(((latitude-shift(latitude))/lat_chg_per_meter)^2 +
                        ((longitude-shift(longitude))/long_chg_per_meter)^2),
                   na.rm=TRUE),
    q25=quantile(sqrt(((latitude-shift(latitude))/lat_chg_per_meter)^2 +
                        ((longitude-shift(longitude))/long_chg_per_meter)^2),
                 probs=.25,
                 na.rm=TRUE),
    q5=quantile(sqrt(((latitude-shift(latitude))/lat_chg_per_meter)^2 +
                        ((longitude-shift(longitude))/long_chg_per_meter)^2),
                 probs=.5,
                 na.rm=TRUE)),
  by=trip]

sort(table(tripwise_mean_distances$mean_dist), T)[1:7]
sort(table(tripwise_mean_distances$min_dist), T)[1:7]
sort(table(tripwise_mean_distances$q25), T)[1:7]
sort(table(tripwise_mean_distances$q5), T)[1:7]
#lots of trips with no distances... see number of obs:
View(trip_points[order(timestamp),
  .(mean_dist=mean(sqrt(((latitude-shift(latitude))/lat_chg_per_meter)^2 +
                        ((longitude-shift(longitude))/long_chg_per_meter)^2),
                   na.rm=TRUE),
    cnt=.N),
  by=trip][mean_dist==0])
# I need to remove trips with zero distance.
# ofc those are 2point trips.
trip_points_2 <- trip_points[!(trip %in% tripwise_mean_distances[mean_dist==0, trip]),]
nrow(trip_points_2)/nrow(trip_points) # 99.9% stays, hooray!

trips <- trip_points_2
trips[,`:=`(inbp=NULL,
            distance=NULL,
            range_before=NULL,
            range_after=NULL)]
trips[, `:=`(spat_distance=sqrt(((latitude-shift(latitude))/lat_chg_per_meter)^2 +
                                     ((longitude-shift(longitude))/long_chg_per_meter)^2),
             time_distance=timestamp - shift(timestamp),
             direction=atan((latitude-shift(latitude))/(longitude-shift(longitude))),
             trip_nr=as.integer(gsub("trip_", "", trip, fixed=TRUE))),
        by=trip]


# how far points are in each trip?
# how close should different trips'
# points be to be counted as intersection?

tripwise_stats <- trips[order(timestamp),
  .(mean_dist=mean(spat_distance,
                   na.rm=TRUE),
    min_dist=min(spat_distance,
                   na.rm=TRUE),
    q25=quantile(spat_distance,
                 probs=.25,
                 na.rm=TRUE),
    q5=quantile(spat_distance,
                 probs=.5,
                 na.rm=TRUE),
    q75=quantile(spat_distance,
                 probs=.75,
                 na.rm=TRUE),
    q95=quantile(spat_distance,
                 probs=.95,
                 na.rm=TRUE)),
  by=trip]

library(ggplot2)
colors <- c("mean_dist"="green",
            "min_dist"="black",
            "q25"="blue",
            "q5"="darkgreen",
            "q75"="orange",
            "q95"="red")
ggplot(data=melt(tripwise_stats,
                 id.vars="trip"))+
  stat_ecdf(aes(x=value,
                group=variable,
                color = variable))+
  xlim(0,1000)+
  ylim(.75, 1)+
  labs(title = "ECDF of trips with between-point quantiles",
       x = "Distance (m)",
       y = "Ratio of trips with point distance quantiles lower",
       subtitle="Verical lines are at 50, 100, 125, 150 meters")+
  geom_vline(aes(xintercept=50), size=0.5)+
  geom_vline(aes(xintercept=100), size=0.5)+
  geom_vline(aes(xintercept=125), size=0.5)+
  geom_vline(aes(xintercept=150), size=0.5)

# Ok. 95% of trips have close points under 100m.
# Later I can check intersections depending on
# distances around a given point,
# but for now lets sick with this
radius=50 # (100m difference between two points)

# non-equi merge:
# intersection is a point, where there's an A point in radius
# relative to B, but A's neigbours are not in B's neighbours radius
trips[,
      `:=`(max_lat=latitude+lat_chg_per_meter*radius,
           min_lat=latitude-lat_chg_per_meter*radius,
           max_long=longitude+long_chg_per_meter*radius,
           min_long=longitude-long_chg_per_meter*radius)][,
      `:=`(prev_max_lat=shift(max_lat),
           prev_min_lat=shift(min_lat),
           prev_max_long=shift(max_long),
           prev_min_long=shift(min_long),
           prev_lat=shift(latitude),
           prev_long=shift(longitude),
           next_max_lat=shift(max_lat, n=-1),
           next_min_lat=shift(min_lat, n=-1),
           next_max_long=shift(max_long, n=-1),
           next_min_long=shift(min_long, n=-1),
           next_lat=shift(latitude, n=-1),
           next_long=shift(longitude, n=-1)),
      by = trip]


#-- test the intersecion point logic:
# what about trips on the same route, like tram or bike or walk or car on Margaret bridge?
# search for similar trips e.g. on Nagykörút:
ref_point <- c(latitude=47.494629, longitude=19.070960)
get_nearest_points <- function(data, point,
                   max_distance=50#,
                   # max_number_of_points=50
                   ){
  return(
    data[
      data$latitude<=point["latitude"]+(max_distance*lat_chg_per_meter) &
      data$latitude>=point["latitude"]-(max_distance*lat_chg_per_meter) &
      data$longitude<=point["longitude"]+(max_distance*long_chg_per_meter) &
      data$longitude>=point["longitude"]-(max_distance*long_chg_per_meter),
      ])
}

routes_around <- unique(get_nearest_points(trips, ref_point)$trip)
intersecting_trip_1  <- routes_around[3]
intersecting_trip_2  <- routes_around[4]
plot(trips[trip %in% c(intersecting_trip_1, intersecting_trip_2),
           longitude],
     trips[trip %in% c(intersecting_trip_1, intersecting_trip_2), latitude],
     col=c("blue", "orange", "green", "black", "magenta")[trips[trip %in% c(intersecting_trip_1, intersecting_trip_2), as.integer(as.factor(trip))]])
# these are intersecting at 2 points but run paralel on Nagykörút
trip_1 <- trips[trip == intersecting_trip_1,
                .(longitude_1=longitude,
                  latitude_1=latitude,
                  max_lat_1=max_lat,
                  min_lat_1=min_lat,
                  max_long_1=max_long,
                  min_long_1=min_long,
                  trip,
                  uuid_1=uuid)] 
trip_2 <- trips[trip==intersecting_trip_2,
                .(longitude_2=longitude,
                  latitude_2=latitude,
                  latitude,
                  longitude,
                  trip,
                  uuid_2=uuid)]

intersections <- trip_1[trip_2,
                        on=.(max_lat_1 > latitude,
                             min_lat_1 < latitude,
                             max_long_1 > longitude,
                             min_long_1 < longitude)][
                        !is.na(longitude_2) & !is.na(latitude_2) &
                        !is.na(longitude_1) & !is.na(latitude_1)]
plot(trips[trip %in% c(intersecting_trip_1, intersecting_trip_2),
           longitude],
     trips[trip %in% c(intersecting_trip_1, intersecting_trip_2), latitude],
     col=c("blue", "orange")[trips[trip %in% c(intersecting_trip_1, intersecting_trip_2), as.integer(as.factor(trip))]])
points(intersections$longitude_1,
       intersections$latitude_2,
       col="red",
       pch=4)

str(intersections)
# Seems to work reasonably.
# Try to get the 4 distinct intersections
# get components:
library(igraph)
intersection_g <- graph_from_data_frame(intersections[, .(uuid_1, uuid_2)],
                                        directed=FALSE)
intersection_comps <- components(intersection_g)
uuid_intersection <- data.table(uuid=V(intersection_g)$name,
                               intersection=paste0("intersection_",
                                                   intersection_comps$membership))
mean_intersection_points <- uuid_intersection[trips[trip %in% c(intersecting_trip_1, intersecting_trip_2)],
                                              .(uuid, intersection,
                                                longitude, latitude),
                                              on=.(uuid)][!is.na(intersection),
                                              .(latitude=mean(latitude),
                                                longitude=mean(longitude)),
                                              by=intersection]
plot(mean_intersection_points$longitude,
     mean_intersection_points$latitude)
# Create a function from it
# and wrap its end up with one dt with lat, long, trip_id.
find_intersections <- function(trip_1, trip_2,
                               radius=50){
  # trips are data.tables with uuid, longitude, latitude
  # if trips does not contain max and min latitude and longitude,
  # those are computed with radius
  #if(all.equal(trip_1, trip_2)){
  #  stop("It's the same route twice!")
  #}
  if(max(trip_1$longitude)<min(trip_2$longitude) | # 1 is west to 2
     max(trip_2$longitude)<min(trip_1$longitude) | # 2 is west to 1
     max(trip_1$latitude)<min(trip_2$latitude) | # 1 is south to 2
     max(trip_2$latitude)<min(trip_1$latitude)){ # 2 is south to 1
    return(NULL)
  }
  trip_1 <- copy(trip_1)
  trip_2 <- copy(trip_2)
  # I do this to reduce sid efefcts, also trips should not be really large tables
  setnames(trip_1,
           c("longitude", "latitude", "uuid"),
           c("longitude_1","latitude_1", "uuid_1"))
  if(!all(c("min_latitude", "max_latitude",
           "min_longitude", "max_latitude") %in%
         names(trip_1))){
    trip_1[,
           `:=`(max_latitude  = latitude_1+lat_chg_per_meter*radius,
                min_latitude  = latitude_1-lat_chg_per_meter*radius,
                max_longitude = longitude_1+long_chg_per_meter*radius,
                min_longitude = longitude_1-long_chg_per_meter*radius)]
  }
  trip_2[, `:=`(longitude_2 = longitude,
                latitude_2 = latitude,
                uuid_2 = uuid)]
  if(!all(c("min_latitude", "max_latitude",
           "min_longitude", "max_latitude") %in%
         names(trip_2))){
    trip_2[,
           `:=`(max_latitude  = latitude_2+lat_chg_per_meter*radius,
                min_latitude  = latitude_2-lat_chg_per_meter*radius,
                max_longitude = longitude_2+long_chg_per_meter*radius,
                min_longitude = longitude_2-long_chg_per_meter*radius)]
  }
  intersections <- rbindlist(list(trip_1[trip_2,
                          on=.(max_latitude > latitude,
                               min_latitude < latitude,
                               max_longitude > longitude,
                               min_longitude < longitude)]# ,
                                 #trip_2[trip_1, #This is if distance is not fixed
                          # on=.(max_latitude > latitude_1,
                               # min_latitude < latitude_1,
                               # max_longitude > longitude_1,
                               # min_longitude < longitude_1)]
                               ))[
                          !is.na(longitude_2) & !is.na(latitude_2) &
                          !is.na(longitude_1) & !is.na(latitude_1)]
  if(nrow(intersections)==0){
    return(NULL)
  }
  intersection_g <- igraph::graph_from_data_frame(intersections[, .(uuid_1, uuid_2)],
                                          directed=FALSE)
  intersection_comps <- igraph::components(intersection_g)
  uuid_intersection <- data.table(uuid=igraph::V(intersection_g)$name,
                                 intersection=paste0("intersection_",
                                                     intersection_comps$membership))
  mean_intersection_points <- uuid_intersection[rbindlist(list(trip_1[,.(uuid=uuid_1,
                                                                         longitude=longitude_1,
                                                                         latitude=latitude_1)],
                                                               trip_2[,.(uuid=uuid_2,
                                                                         longitude=longitude_2,
                                                                         latitude=latitude_2)])),
                                                .(uuid, intersection,
                                                  longitude, latitude),
                                                on=.(uuid)][!is.na(intersection),
                                                .(latitude=mean(latitude),
                                                  longitude=mean(longitude)),
                                                by=intersection]
  return(mean_intersection_points)
}
ints <- find_intersections(trips[trip=="trip_528"],
                           trips[trip=="trip_1679"])

plot(trips[trip %in% c("trip_528","trip_1679"),
           longitude],
     trips[trip %in% c("trip_528","trip_1679"), latitude],
     col=c("blue", "orange")[trips[trip %in% c("trip_528","trip_1679"), as.integer(as.factor(trip))]],
     main = "intersections of different trips")
points(ints$longitude,
       ints$latitude,
       col="red",
       pch=4)


ints_2 <- find_intersections(trips[trip=="trip_528"],
                           trips[trip=="trip_528"])
plot(trips[trip %in% c("trip_528","trip_1679"),
           longitude],
     trips[trip %in% c("trip_528","trip_1679"), latitude],
     col=c("blue", "orange")[trips[trip %in% c("trip_528","trip_1679"), as.integer(as.factor(trip))]],
     main = "intersections of parallel trips")
points(ints_2$longitude,
       ints_2$latitude,
       col="red",
       pch=4)

# get longest route, get timing with it.
longest_trip <- trips[, .(length=.N), by=trip][length==max(length), trip] 
trip_l <- trips[trip==longest_trip]
system.time(find_intersections(trip_l, trip_l)) # <15s.

# plan (still brute force):
# find close trips to each point
get_nearest_points <- function(data, point,
                   max_distance=50#,
                   # max_number_of_points=50
                   ){
  return(
    data[
      data$latitude<=point["latitude"]+(max_distance*lat_chg_per_meter) &
      data$latitude>=point["latitude"]-(max_distance*lat_chg_per_meter) &
      data$longitude<=point["longitude"]+(max_distance*long_chg_per_meter) &
      data$longitude>=point["longitude"]-(max_distance*long_chg_per_meter),
      ])
}
find_close_trips <- function(data, point){
  get_nearest_points(data, point)[, (unique(trip))]
}

# this takes several hours and 16Gigs RAM, but works
unique_points  <- unique(trips[, .(uuid, longitude, latitude, trip)])
close_trips <- mapply(function(lat, long){
                        get_nearest_points(trips, c(latitude=lat, longitude=long))[, unique(trip)]
                        },
                      unique_points$latitude,
                      unique_points$longitude)
gc()
names(close_trips) <- unique_points$uuid
saveRDS(close_trips,
     "data/edited/close_trip_list.RDS")
fwrite(trips,
       "data/edited/trips_3.csv")


#---------------------------------------------------------#
#                                                         #
#      Intersection points from route intersections       #
#                                                         #
#---------------------------------------------------------#

library(data.table)

trips <- fread("data/edited/trips_3.csv")
close_trips <- readRDS("data/edited/close_trip_list.RDS")[trips[, uuid]]
# not parallel routes (only colse to a point, not to its neghbours)
# 2 hours long task!
close_trips_diff <- sapply(2:(length(close_trips)-1),
                           function(i){
                                setdiff(close_trips[i],
                                        intersect(close_trips[i-1],
                                                  close_trips[i+1]))
                            },
                           simplify=FALSE,
                           USE.NAMES=TRUE)
for(i in 2:(length(close_trips)-1)){
  if(length(close_trips_diff[[i-1]])>0){
    names(close_trips_diff[[i-1]]) <- names(close_trips)[i]
  }
}
close_trips_diff <- unlist(close_trips_diff, recursive=FALSE)
close_trips_diff[[names(close_trips)[1]]] <- close_trips[[1]]
close_trips_diff[[names(close_trips)[length(close_trips)]]] <- close_trips[[length(close_trips)]]
saveRDS(close_trips_diff,
        "data/edited/intersecting_trips.RDS")

#---------------------------------------------------------#
#                                                         #
#       Finding intersection ponts themselves             #
#            (not just intersecting trips)                #
#---------------------------------------------------------#


library(data.table)

trip_intersection_data <- readRDS("data/edited/intersecting_trips.RDS")
# create a {trip: points} list
# then for each trip, get all unique trips from intersection data
trips <- fread("data/edited/trips_3.csv",
               select=c("uuid", "trip"))


get_intersecting_trip <- function(trip_to_check){
  if(length(trip_to_check)>1){
    stop(paste("trip is:", trip_to_check))
  }
  unique(unlist(trip_intersection_data[trips[trip==trip_to_check, uuid]]))
}
# 20  min long task
trip_pairs <- trips[,
                    .(trip2 = get_intersecting_trip(trip)),
                    by=trip]
trip_pairs_2 <- unique(trip_pairs[trip<trip2,])
# this is still 27 million pairs :(
fwrite(trip_pairs_2,
       "data/edited/trip_pairings.csv")








# what about trips on the same route, like tram or bike or walk or car on Margaret bridge?
# search for similar trips e.g. on Nagykörút:
ref_point <- c(latitude=47.494629, longitude=19.070960)
get_nearest_points <- function(data, point,
                   max_distance=50#,
                   # max_number_of_points=50
                   ){
  return(
    data[
      data$latitude<=point["latitude"]+(max_distance*lat_chg_per_meter) &
      data$latitude>=point["latitude"]-(max_distance*lat_chg_per_meter) &
      data$longitude<=point["longitude"]+(max_distance*long_chg_per_meter) &
      data$longitude>=point["longitude"]-(max_distance*long_chg_per_meter),
      ])
}

routes_around <- unique(get_nearest_points(trips, ref_point)$trip)
View(trips[trip %in% routes_around[1:4]][order(timestamp),
     .(latitude, longitude, timestamp, trip)])
plot(trips[trip %in% routes_around[1:4], longitude],
     trips[trip %in% routes_around[1:4], latitude],
     col=c("blue", "orange", "green", "black", "magenta")[trips[trip %in% routes_around[1:4], as.integer(as.factor(trip))]])
points(x=ref_point["longitude"],
       y=ref_point["latitude"],
       col="red",
       pch=4)

# Ok, lets define intersections:
# Two trip has an intersection,
# if there's a point in one close to one in the other
# such that their surrounding points are less close.
trip_1 <- trips[trip == routes_around[1],]
trip_2 <- trips[trip == routes_around[2],]
# do an unequi-join
