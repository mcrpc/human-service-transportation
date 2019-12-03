library(tigris)
library(tmap)
library(rgeos)
library(magrittr)

counties <- c("113",
              "053",
              "105",
              "075",
              "091")

co <- counties("17", TRUE, class = "sf") %>%
  subset(COUNTYFP %in% counties)

p <- places("17", TRUE, 2017, class = "sf") %>%
  st_transform(3443) %>%
  st_centroid() %>%
  select(-c(ALAND, AWATER, LSAD, AFFGEOID, PLACENS)) %>%
  mutate(stop_long = st_coordinates(.)[,1], stop_lat = st_coordinates(.)[,2])


tm_shape(co) + tm_borders() + tm_shape(p) + tm_dots()

# stops <- read_csv("stops.csv") %>%
#   select(-c(stop_lat, stop_long)) %>%
#   left_join(p, by = c("stop_name" = "NAME")) %>%
#   select(c(stop_id, stop_name, stop_lat, stop_long)) %T>%
#   write_csv("stops.csv", na = "")

stops <- st_read("stops.shp") %>%
  st_transform(4326) %>%
  mutate(stop_lon = st_coordinates(.)[,1], stop_lat = st_coordinates(.)[,2]) %>%
  as_tibble() %>%
  select(stop_id, stop_name, stop_lat, stop_lon) %T>%
  write_csv("stops.csv")


