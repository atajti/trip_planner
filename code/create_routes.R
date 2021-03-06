# Idea: create continuous routes based on time or distance,
# at intersection, set change time by closeness
lat_chg_per_meter = 8.983152840696227042251e-06 # (see get_distances.R)
long_chg_per_meter = 1.329383083419093512941e-05 # (see get_distances.R)

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


# load the data:
library(data.table)
# make sure bit64 pkg is installed
all_points=fread("data/edited/bp_coords.csv")
# data.table rolling joins will need timestamps in POSIXlt
all_points[, `:=`(timestamp=as.POSIXct(timestampMs/1000,
                                      origin="1970-01-01"),
                  uuid=uuid::UUIDgenerate(n=nrow(all_points)),
                  V1=NULL)]


# what seems to be continuous recoding?
timediffs <- all_points[order(timestamp), diff(timestamp)]
# as.numeric(timediffs) removes partial seconds. I can live with that.
plot(density(as.numeric(timediffs)[timediffs<11]))
# seems to be a clear peak at 1s, so the interval will be 2.
continuity_tdiff <- 2

all_points[, `:=`(range_before = timestamp - continuity_tdiff,
	              range_after = timestamp + continuity_tdiff)]

all_segments <- all_points[all_points,
                           on = .(timestamp<timestamp,
                           	      timestamp>range_before)][
                !is.na(uuid) & !is.na(i.uuid)]
# maybe search for connected_components via data.table recursively instead of igraph?
# better: last observation carried forward?
trip_graph <- igraph::graph_from_data_frame(all_segments[,.(uuid,i.uuid)],
                                            directed=FALSE)
trip_components <- igraph::components(trip_graph)
point_trip <- data.table(uuid=igraph::V(trip_graph)$name,
                         trip = paste0("trip_", trip_components$membership))
trip_points <- all_points[point_trip, on=.(uuid)]
# get time diffs between trips
trip_points[order(timestamp),][
            trip != shift(trip), min(diff(timestamp))]
# 2.2 sec, great! Get a feel about how much should we strech this?
# lets see a change in CDF
trip_diffs <- trip_points[order(timestamp),][
                          trip != shift(trip), diff(timestamp)]
plot(x=1:3600,
     y=lapply(1:3600,
              function(cutoff){
                mean(trip_diffs[trip_diffs>0]<cutoff)
              }
             )
    )
# seems to be an exciting point after 0.1 percent:
# several pause between trips are that long.

trip_diff_cdf <- as.numeric(
  lapply(1:3600,
      function(cutoff){
        mean(trip_diffs[trip_diffs>0]<cutoff)
      }
     )
  )
plot(which(trip_diff_cdf > 0.05 & trip_diff_cdf < 0.15),
     trip_diff_cdf[which(trip_diff_cdf > 0.05 & trip_diff_cdf < 0.15)]
     )
trip_diff_cdf[55:65]
# ok, there's a jump in trip separation 60-61-62 secs
# before this, with 1 sec as continuity, we had 2911 trips.
# check for tdiff < 63
continuity_tdiff <- 63

all_points[, `:=`(range_before = timestamp - continuity_tdiff,
                range_after = timestamp + continuity_tdiff)]

all_segments <- all_points[all_points,
                           on = .(timestamp<timestamp,
                                  timestamp>range_before)][
                !is.na(uuid) & !is.na(i.uuid)]

trip_graph <- igraph::graph_from_data_frame(all_segments[,.(uuid,i.uuid)],
                                            directed=FALSE)
trip_components <- igraph::components(trip_graph)
point_trip <- data.table(uuid=igraph::V(trip_graph)$name,
                         trip = paste0("trip_", trip_components$membership))
trip_points <- all_points[point_trip, on=.(uuid)]
# get time diffs between trips
trip_points[order(timestamp),][
            trip != shift(trip), min(diff(timestamp))]
# Wow. 6x more points, 49000 trips instead of 3000?
# Makes sense.  min time diff is 1.05 MIN! Great!
View(trip_points[order(timestamp), .(uuid, timestamp, trip)])
plot(trip_points[trip %in% c("trip_1", "trip_2", "trip_3"),
                 latitude],
     trip_points[trip %in% c("trip_1", "trip_2", "trip_3"),
                 longitude],
     col=trip_points[trip %in% c("trip_1", "trip_2", "trip_3"),
                     as.numeric(gsub("[a-z_]", "", trip))])
# these seem to be the same trip.
# there's another large gap on the plot around 0.2:
plot(x=1:3600,
     y=lapply(1:3600,
              function(cutoff){
                mean(trip_diffs[trip_diffs>0]<cutoff)
              }
             )
    )
# ok, the cdf starts to flatten after 500
# so 540 sec is 9 min as a reasonable cutoff,
# but for easier interpretation, set it to 10 mins:
continuity_tdiff <- 601

all_points[, `:=`(range_before = timestamp - continuity_tdiff,
                range_after = timestamp + continuity_tdiff)]

all_segments <- all_points[all_points,
                           on = .(timestamp<timestamp,
                                  timestamp>range_before)][
                !is.na(uuid) & !is.na(i.uuid)]

trip_graph <- igraph::graph_from_data_frame(all_segments[,.(uuid,i.uuid)],
                                            directed=FALSE)
trip_components <- igraph::components(trip_graph)
# this time 828000 points, 99.15% of all is used,
# with only 14000 trips instead of 49000.
point_trip <- data.table(uuid=igraph::V(trip_graph)$name,
                         trip = paste0("trip_", trip_components$membership))
trip_points <- all_points[point_trip, on=.(uuid)]
# get time diffs between trips
trip_points[order(timestamp),][
            trip != shift(trip), min(diff(timestamp))]
plot(trip_points[trip %in% c("trip_1", "trip_2", "trip_3"),
                 longitude],
     trip_points[trip %in% c("trip_1", "trip_2", "trip_3"),
                 latitude],
     col=trip_points[trip %in% c("trip_1", "trip_2", "trip_3"),
                     as.numeric(gsub("[a-z_]", "", trip))])
#those seem to be distinct trips.
fwrite(trip_points,
       "data/edited/trips.csv")
# next : get trip intersections

#-----------------------
# found out that a lot of the times, gps reposts regularly,
# but from the same position.  Lets do this analysis again

lat_chg_per_meter = 8.983152840696227042251e-06 # (see get_distances.R)
long_chg_per_meter = 1.329383083419093512941e-05 # (see get_distances.R)
# load the data:
library(data.table)
# make sure bit64 pkg is installed
all_points=fread("data/edited/bp_coords.csv")
# data.table rolling joins will need timestamps in POSIXlt
all_points[, `:=`(timestamp=as.POSIXct(timestampMs/1000,
                                      origin="1970-01-01"),
                  uuid=uuid::UUIDgenerate(n=nrow(all_points)),
                  V1=NULL)]
# remove points which are at the same position 
all_points_2 <- all_points[order(timestamp)][
                  !(latitude==shift(latitude) &
                    latitude==shift(latitude, -1) &
                    longitude==shift(longitude) &
                    longitude==shift(longitude, -1)),]
nrow(all_points_2)/nrow(all_points) # woow, massive 25% got deleted!
rm(all_points)
all_points <- all_points_2

timediffs <- all_points[order(timestamp), diff(timestamp)]
# as.numeric(timediffs) removes partial seconds. I can live with that.
plot(density(as.numeric(timediffs)[timediffs<11]))
# seems to be a clear peak at 1s, so the interval will be 2.
continuity_tdiff <- 2

all_points[, `:=`(range_before = timestamp - continuity_tdiff,
                range_after = timestamp + continuity_tdiff)]

all_segments <- all_points[all_points,
                           on = .(timestamp<timestamp,
                                  timestamp>range_before)][
                !is.na(uuid) & !is.na(i.uuid)]
# maybe search for connected_components via data.table recursively instead of igraph?
# better: last observation carried forward?
trip_graph <- igraph::graph_from_data_frame(all_segments[,.(uuid,i.uuid)],
                                            directed=FALSE)
trip_components <- igraph::components(trip_graph)
point_trip <- data.table(uuid=igraph::V(trip_graph)$name,
                         trip = paste0("trip_", trip_components$membership))
trip_points <- all_points[point_trip, on=.(uuid)]
# get time diffs between trips
trip_points[order(timestamp),][
            trip != shift(trip), min(diff(timestamp))]
# 2.2 sec, great! Get a feel about how much should we strech this?
# lets see a change in CDF
trip_diffs <- trip_points[order(timestamp),][
                          trip != shift(trip), diff(timestamp)]
plot(x=1:3600,
     y=lapply(1:3600,
              function(cutoff){
                mean(trip_diffs[trip_diffs>0]<cutoff)
              }
             )
    )
# now there's a clear saturation,
# the jump is around .6
# but the overall shape seems to be the same.
trip_diff_cdf <- as.numeric(
  lapply(1:3600,
      function(cutoff){
        mean(trip_diffs[trip_diffs>0]<cutoff)
      }
     )
  )
plot(which(trip_diff_cdf > 0.56 & trip_diff_cdf < 0.67),
     trip_diff_cdf[which(trip_diff_cdf > 0.56 & trip_diff_cdf < 0.67)]
     )
plot(which(trip_diff_cdf > 0.75 & trip_diff_cdf < 0.8),
     trip_diff_cdf[which(trip_diff_cdf > 0.75 & trip_diff_cdf < 0.8)]
     )
trip_cdf[600]
# yeah, lets cut again az 600, that's 78% of the points
continuity_tdiff <- 601

all_points[, `:=`(range_before = timestamp - continuity_tdiff,
                range_after = timestamp + continuity_tdiff)]

all_segments <- all_points[all_points,
                           on = .(timestamp<timestamp,
                                  timestamp>range_before)][
                !is.na(uuid) & !is.na(i.uuid)]

trip_graph <- igraph::graph_from_data_frame(all_segments[,.(uuid,i.uuid)],
                                            directed=FALSE)
trip_components <- igraph::components(trip_graph)
# 16146 trips instead of 14000 trips, seems ok: more subtrips, but not that muck more
# also 622000 points instead of 828000, matches the 75%
point_trip <- data.table(uuid=igraph::V(trip_graph)$name,
                         trip = paste0("trip_", trip_components$membership))
trip_points <- all_points[point_trip, on=.(uuid)]
# get time diffs between trips
trip_points[order(timestamp),][
            trip != shift(trip), min(diff(timestamp))]
View(trip_points[trip %in% c("trip_1", "trip_2", "trip_3")])
plot(trip_points[trip %in% c("trip_1", "trip_2", "trip_3"),
                 longitude],
     trip_points[trip %in% c("trip_1", "trip_2", "trip_3"),
                 latitude],
     col=trip_points[trip %in% c("trip_1", "trip_2", "trip_3"),
                     as.numeric(gsub("[a-z_]", "", trip))])
#those seem to be distinct trips.
fwrite(trip_points,
       "data/edited/trips_2.csv")


#-------------------------------#
#                               #
#     Remove 0-dstance trips    #
#                               #
#-------------------------------#

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

trip_points_2 <- trip_points[!(trip %in% tripwise_mean_distances[mean_dist==0, trip]),]

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

fwrite(trips,
       "data/edited/trips_3.csv")