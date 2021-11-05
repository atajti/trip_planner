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
             direction=atan((latitude-shift(latitude))/(longitude-shift(longitude)))),
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
  `:=`(prev_max_lat=,
       prev_min_lat=,
       prev_max_long=,
       prev_min_long=,
       next_max_lat=,
       next_min_lat=,
       next_max_long=,
       next_min_long=,)
  by = trip]
# At the start and end poins new columns are NA
# assign values manually: imagine the next/previous point
# in the same/opposite direction and the same distance





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

# Ok, lets define intersections:
# Two trip has an intersection,
# if there's a point in one close to one in the other
# such that their surrounding points are less close.
trip_1 <- trips[trip == routes_around[1],]
trip_2 <- trips[trip == routes_around[2],]
# do an unequi-join
