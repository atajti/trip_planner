# get trip intersections (close points of different trips)
# to find plausible routes between points
library(data.table)
lat_chg_per_meter = 8.983152840696227042251e-06 # (see get_distances.R)
long_chg_per_meter = 1.329383083419093512941e-05 # (see get_distances.R)
trip_points <- fread("data/edited/trips.csv")

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

View(trip_points[order(timestamp),][
     trip %in% c("trip_2519", "trip_2521", "trip_2522")])

nulltrips <- tripwise_mean_distances[mean_dist==0, unique(trip)]
length(nulltrips)/length(unique(tripwise_mean_distances$trip))
# woow, quarter of the trips :( lets ditch them for now
trips <- trip_points[!(trip %in% nulltrips),]
trips[,`:=`(inbp=NULL,
            distance=NULL,
            range_before=NULL,
            range_after=NULL)]
trips[, `:=`(spat_distance=sqrt(((latitude-shift(latitude))/lat_chg_per_meter)^2 +
                                     ((longitude-shift(longitude))/long_chg_per_meter)^2),
             time_distance=timestamp - shift(timestamp)),
        by=trip]
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
View(trips[trip %in% routes_around[1:4]][order(timestamp)])
plot(trips[trip %in% routes_around[1:4], longitude],
     trips[trip %in% routes_around[1:4], latitude],
     col=c("blue", "orange", "green", "black", "magenta")[trips[trip %in% routes_around[1:4], as.integer(as.factor(trip))]])
points(x=ref_point["longitude"],
       y=ref_point["latitude"],
       col="red",
       pch=4)

# oh hey! just search for trips containing the starting point,
# trips containing the endpoint,
# create a graph and get the shortest path,
# get time of each trip-part, then add them together.
# see grap_routes.R


