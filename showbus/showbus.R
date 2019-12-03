library(tidytransit)
library(tidyverse)
library(tigris)
library(tmap)
library(rgeos)
library(magrittr)

setwd("G:/_Projects/HSTP/showbus")

stops <- read_csv("stops.csv")
trips <- read_csv("trips.csv")
routes <- read_csv("routes.csv")
calendar <- read_csv("calendar.csv")
stop_times <- read_csv("stop_times.csv")

sum_stops <- left_join(stop_times, stops) %>%
  left_join(trips) %>%
  group_by(trip_id) %>%
  summarize(
    stop_seq = paste0(stop_name, collapse = ", "),
    route_id = first(route_id)
    )

stops_sf <- st_as_sf(stops, coords = c("stop_lon", "stop_lat"), crs = 4326) %>%
  st_transform(3443) %>%
  mutate(stop_id = as.integer(stop_id)) %T>%
  st_write("stops.shp", delete_dsn = TRUE)
shapes_sf <- left_join(stop_times, stops) %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>%
  st_transform(3443) %>%
  group_by(trip_id) %>%
  summarize(do_union = FALSE) %>%
  left_join(sum_stops) %>%
  mutate(trip_id = as.integer(trip_id)) %>%
  st_cast("LINESTRING") %T>%
  st_write("routes.shp", delete_dsn = TRUE)



tm_shape(co) + tm_borders(col = "lightgray") + tm_shape(shapes_sf) + tm_lines() + tm_shape(stops_sf) + tm_dots() + tm_text("stop_name", size = .5)
