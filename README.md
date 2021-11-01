# Trip planning based on your own data

This project aims to find a way to plan trips based
on my (or your own) historical geolocation data.
Several companies collect your location,
the best known fo this is Google, just go to
(Google Takeout)[takeout.google.com].

## Current state

I'm currently trying to find a way to find paths in Budapest.
If you have locaion data from Budapest, you may be able to follow my scipts.
To start, clone this repo, and create `data/raw` and `data/edited` folder in the project root folder, then copy your Google location history data into `data/raw`.

 - `basic_bp_map.R` filters inner Budapest data, creates a dataset accordingly.
 - `get_distances.R` find out the longiude and latitude change per meter, then tries to naively find the two endpoint in a 2-hour range. You can skip this script.
 - `create_routes.R` looks for continuous recordings in time,
 and labelling them - at the end creating a new dataset again.
 - `trip_intersection.R` attempts to find spatial intersection
 between routes.

## Future plans

If I can find intersections properly,
then I will have to find paths on based on spatial data.
If that works, I can find how long each path segment takes
from one intersection to another.
In case of multiple paths, I can find alternatives, and based on speed, I can infer travel mode.