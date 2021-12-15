# get trip intersections (close points of different trips)
# to find plausible routes between points
library(data.table)
lat_chg_per_meter = 8.983152840696227042251e-06 # (see get_distances.R)
long_chg_per_meter = 1.329383083419093512941e-05 # (see get_distances.R)
trips <- fread("data/edited/trips_3.csv")

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



#---------------------------------------------------------#
#                                                         #
#                  Find close trip points                 #
#                                                         #
#---------------------------------------------------------#

latorder <- trips[order(latitude, longitude),][
                  trip != shift(trip) &
                  trip != shift(trip, -1),][,
                  latdist := sqrt(((latitude-shift(latitude))/lat_chg_per_meter)^2 +
                                  ((longitude-shift(longitude))/long_chg_per_meter)^2)]
latorder[1, latdist := 0]
# distamce ECDF
ggplot(data=latorder) +
stat_ecdf(aes(x=latdist)) +
xlim(0,500) +
labs(title = "Distance between close point of different trips, when ordered by latitude",
     x = "Distance (m)",
     y = "Ratio of intertrip close points smaller than distance")
# Ok, first we'll try with matching points,
# but max 50 m is great for limit.
# 25 is also understandable in case of excess result

longorder <- trips[order(longitude, latitude),][
                  trip != shift(trip) &
                  trip != shift(trip, -1),][,
                  longdist := sqrt(((latitude-shift(latitude))/lat_chg_per_meter)^2 +
                                  ((longitude-shift(longitude))/long_chg_per_meter)^2)]
longorder[1, latdist := 0]
# distamce ECDF
ggplot(data=longorder) +
stat_ecdf(aes(x=longdist)) +
xlim(0,500) +
labs(title="Distance between close point of different trips, when ordered by longitude",
     x = "Distance (m)",
     y = "Ratio of intertrip close points smaller than distance")
# more than 50% match here for 0 distance, too.
# Max 50 m is great for limit.
# 25 is also understandable in case of excess result


#---------------------------------------------------------#
#                                                         #
#                    Exact position match                 #
#                                                         #
#---------------------------------------------------------#

library(ggplot2)

unique_points <- trips[,.(.SD,
                          coordstring=paste0(latitudeE7,
                                             longitudeE7))][,
                        .(freq=.N),
                        by=coordstring][freq==1,
                        coordstring]

notunique_points <- unique(trips[!(paste0(latitudeE7,
                                          longitudeE7) %in% 
                                          unique_points),
                                 .(uuid, longitude,
                                   latitude,trip_nr)])

trip_touch = notunique_points[notunique_points,
                              on=.(longitude,
                                   latitude,
                                   trip_nr<trip_nr),
                              nomatch=0]
trip_touch_points <- trips[uuid %in%
                           c(trip_touch$uuid,trip_touch$i.uuid)]

ggplot(data=trip_touch_points,
       aes(x=longitude, y=latitude))+
geom_point(col="black", fill="black", alpha=.05)+
labs(title = "Points on multiple trips",
     subtitle = "Emphasises regular places and walking routes")


# create intersection ids, then "replace" uuids with them

is_graph <- igraph::graph_from_data_frame(trip_touch[,.(uuid,i.uuid)],
                                          directed=FALSE)
is_components <- igraph::components(is_graph)
point_is <- data.table(uuid=igraph::V(is_graph)$name,
                       is = paste0("intersection_", is_components$membership))
is_points <- trips[point_is, on=.(uuid)]


ggplot(data=is_points[, .(int_points = .N), by = is]) +
stat_ecdf(aes(x=int_points)) +
labs(title = "Number of points in an intersection",
     y = "% of intersections with less points than x",
     x = "Number of points in the intersections",
     subtitle = paste(length(unique(is_points$is)),
                      "intersection is created from",
                      nrow(is_points),
                      "points."))

trips2 <- is_points[, .(uuid, is)][trips, on=.(uuid)]
trips2[, c(sum(!is.na(is)), length(unique(is)))] # quick check on intersection and point numbers
trips2[, single_id := uuid][!is.na(is), single_id=is]



fwrite(trips2[order(timestamp),],
       "data/edited/trips_intersections.csv")

#---------------------------------------------------------#
#                                                         #
#     Create graph for path search in graph_routes.R      #
#                                                         #
#---------------------------------------------------------#




######
#
# Next steps:
# Create graph and components from `trip_touch`: intersections.
# Examine intersections:
# - long intersectinos may be parallel trips,
# - several point from same trips may be parallel trips too.
# - optional: merge not on exact, location, but on distance < 25 or distance < 50




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
