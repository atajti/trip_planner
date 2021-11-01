library(geosphere)
library(data.table)
# make sure bit64 pkg is installed
all_points=fread("data/edited/bp_coords.csv")
# data.table rolling joins will need timestamps in POSIXlt
all_points[, timestamp:=as.POSIXct(timestampMs/1000,
                                   origin="1970-01-01")]

# get closest n points with max distance of X

get_distances <- function(coords, point){
  d <- mapply(function(lon, lat){ #can use lon/lat matrix instead of mapply
                distm(c(lon, lat),
                      c(point["longitude"], point["latitude"]),
                      fun=distHaversine)},
              lat=coords$latitude,
              lon=coords$longitude)
  return(d)
}

#correlate distance with lat-long difference
data=all_points
point = c(latitude=47.488466, longitude=19.069918)
all_distances <- get_distances(data, point)

diffs <- data.frame(lat  = data$latitude - unname(point)[1],
                    long  = data$longitude - unname(point)[2],
                    dist = all_distances)

cor(diffs$lat, diffs$dist)
cor(diffs$long, diffs$dist)
plot(diffs$lat, diffs$dist)
plot(diffs$long, diffs$dist)
#there's a clear min, max for each distance.
# approx it with a straight line (linear):
# get min and max longitude and latitude difference
# for a given distance, compute ratio.
#longitude:
# nrow(diffs[diffs$dist<3500.1 & diffs$dist > 3499.9,])
# diffs[diffs$dist<3500.5 & diffs$dist > 3499.5,]
# long_ratio<-diffs[diffs$dist==3499.995,]
# long_ratio$rate=long_ratio$long/long_ratio$dist
diffs$long_ratio=diffs$long/diffs$dist
diffs$lat_ratio=diffs$lat/diffs$dist

plot(diffs$long_ratio)
plot(diffs$lat_ratio)

long_chg_per_meter = c(min(diffs$long_ratio),
                       max(diffs$long_ratio))
lat_chg_per_meter = c(min(diffs$lat_ratio),
                       max(diffs$lat_ratio))
all.equal(abs(long_chg_per_meter[1]),
          abs(long_chg_per_meter[2]))
all.equal(abs(lat_chg_per_meter[1]),
          abs(lat_chg_per_meter[2]))

# difference relative to the ratios is negligible:
long_chg_per_meter = max(abs(long_chg_per_meter[1]),
                         abs(long_chg_per_meter[2]))
lat_chg_per_meter = max(abs(lat_chg_per_meter[1]),
                        abs(lat_chg_per_meter[2]))



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


origin <- c(latitude=47.528242, longitude=19.026056)
target <- c(latitude=47.515176, longitude=19.078548)

orig_points <- get_nearest_points(all_points, origin, max_distance=500)
orig_points[, join_ts:=timestamp]
setkey(orig_points, join_ts)

target_points <- get_nearest_points(all_points, target, max_distance=500)
target_points[, join_ts:=timestamp]
setkey(target_points, join_ts)

#testing rolling joins:
op=data.table(orig_timestamp=orig_points$timestamp)
tp=data.table(target_timestamp=target_points$timestamp)
op[, join_ts:=orig_timestamp]
tp[, join_ts:=target_timestamp]
setkey(op, join_ts)
setkey(tp, join_ts)
nrow(op)
nrow(tp)
tp[op, roll=-2*3600]



if(nrow(orig_points)<nrow(target_points)){
  trips = target_points[orig_points, roll=48*3600]
  i_is="origin_"
  j_is="target_"
} else {
  trips = trips = orig_points[target_points, roll=-48*3600]
  i_is="target_"
  j_is="origin_"
}
setnames(trips,
         names(trips)[!startsWith(names(trips), "i.")],
         paste0(j_is, names(trips)[!startsWith(names(trips), "i.")]))
setnames(trips,
         names(trips)[startsWith(names(trips), "i.")],
         sub("^i.", i_is, names(trips)[startsWith(names(trips), "i.")]))
#error if no trip found
if(all(is.na(trips$timestamp)) | all(is.na(trips$i.timestamp))){
  stop("No suitable route found!")
} 



###
#get another two points for testing existing routes:
op=copy(orig_points)
tp=copy(target_points)
op=op[, orig_timestamp:=op$timestamp]
tp=tp[, target_timestamp:=tp$timestamp]
op[, join_ts:=orig_timestamp]
tp[, join_ts:=target_timestamp]
setkey(op, join_ts)
setkey(tp, join_ts)
nrow(op)
nrow(tp)
nrow(tp[op, roll=2*3600,
        rollends=][target_timestamp!=orig_timestamp])

#non-equi joins

op[,orig_range_end:=orig_timestamp+2*3600]
tp[,target_range_start:=target_timestamp-2*3600]
tp[op,
   on=.(target_timestamp>orig_timestamp,
        target_range_start<orig_timestamp)]

tp[op,
   on=.(target_timestamp>orig_timestamp,
        target_range_start<orig_timestamp)]

#No trip in sight.... lets see another way
X <- rbind(data.table(type="origin",
                      timestamp=op$join_ts),
           data.table(type="target",
                      timestamp=tp$join_ts))
X[, range_start:=timestamp-3600]

Y <- X[X,
       on=.(timestamp<timestamp,
            timestamp>range_start)]
# at least, results!
Y[, sum(type != i.type)]
# no result.