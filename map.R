devtools::install_github("dkahle/ggmap")

library(ggmap)
library(geosphere)
library(ggmap)
library(dplyr)
library(tidyr)

setwd('/Users/gregoirelejay/dev/others/personal/air_ambulance/')

# TODO:
# - Ferry time basic
# - Better Visualisation


UK.coords <- as.numeric(geocode("United Kingdom"))
UK.map <-
  ggmap(
    get_googlemap(center = UK.coords, scale = 2, zoom = 5),
    extent = "normal"
  )

UK.bounding.box <-
  data.frame(t(data.frame(
    c(-10.8544921875, 49.82380908513249),
    c(-10.8544921875, 59.478568831926395),
    c(2.021484375, 59.478568831926395),
    c(2.021484375, 49.82380908513249)
  )))
colnames(UK.bounding.box) <- c('lon', 'lat')
rownames(UK.bounding.box) <- c()

n.samples <- 100
UK.coord.diagonal <- data.frame(
  lat = seq(from = min(UK.bounding.box$lat),
            to   = max(UK.bounding.box$lat), length.out = n.samples),
  lon = seq(from = min(UK.bounding.box$lon),
            to   = max(UK.bounding.box$lon), length.out = n.samples)
)

UK.coord.samples <-
  expand.grid(lat = UK.coord.diagonal$lat,
              lon = UK.coord.diagonal$lon) %>%
  dplyr::mutate(latlon = paste(lat, lon, sep = ','))

air.ambulances <-
  read.csv('./data/air_ambulances.csv', row.names = FALSE) %>%
  dplyr::mutate(latlon = paste(lat, lon, sep = ','))


# Distance in km
aa.sample.df <-
  expand.grid(aa_lat_lon = air.ambulances$latlon,
              ss_lat_lon = UK.coord.samples$latlon) %>%
  separate(aa_lat_lon, into = c('aa_lat', 'aa_lon'),
           sep = ',', convert = TRUE) %>%
  separate(ss_lat_lon, into = c('ss_lat', 'ss_lon'),
           sep = ',', convert = TRUE) %>%
  rowwise() %>%
  dplyr::mutate(
    distance   = 10^-3 * distHaversine(c(aa_lon, aa_lat),
                                       c(ss_lon, ss_lat)),
    speed = 285,
    time_hours = distance / speed
  ) %>% ungroup()


avg.model <-
  aa.sample.df %>%
  dplyr::group_by(ss_lat, ss_lon) %>%
  dplyr::summarise(
    avg_time_hours = min(time_hours)
  )

test <- FALSE
if (test) {
  ll <-
    aa.sample.df %>%
    dplyr::left_join(air.ambulances, by = c("aa_lat" = 'lat', 'aa_lon' = 'lon')) %>%
    dplyr::filter(member == 'Air Ambulance Northern Ireland')

  ggplot(ll) +
    geom_point(aes(x = ss_lon, y = ss_lat, color = time_hours))
}

isochrone.map <-
UK.map +
  stat_contour(data = avg.model,
               aes(x = ss_lon, y = ss_lat, z = avg_time_hours, colour = ..level..),
               binwidth = 0.05, alpha = 1) +
  geom_point(data = air.ambulances, aes(x = lon, y = lat), color = 'black', size = 1)





