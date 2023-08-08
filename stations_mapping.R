library("this.path")
library("ggplot2")

path <- this.dir()
load(file.path(path, "data", "rain_aut.Rda"))

stations = data.frame()

for (i in seq_along(rain_aut)) {
  stations[i, 1] = rain_aut[[i]]$meta$name
  stations[i, 2] = rain_aut[[i]]$meta$lon
  stations[i, 3] = rain_aut[[i]]$meta$lat
  stations[i, 4] = length(rain_aut[[i]]$max_data$year)
}

names(stations) = c("Station", "lon", "lat", "timeseries_len")

substations_rest = subset(stations, timeseries_len < 50)
substations_point = subset(stations, timeseries_len >= 100)
substations_spatial = subset(stations, 
                             (timeseries_len >= 50) & (timeseries_len < 100))

####
data("Austria", package = "bamlss")
austria <- as.data.frame(coordinates(AustriaTopo))
names(austria) <- c("lon", "lat", "alt")

ggplot(austria, aes(lon, lat)) +
  geom_raster(aes(fill = alt), alpha = 1) +
  geom_point(data = substations_rest,
             mapping = aes(x = lon, y = lat), 
             col = "black", fill = "grey", shape = 21, size = 0.7) +
  geom_point(data = substations_spatial,
             mapping = aes(x = lon, y = lat), 
             col = "black", fill = "blue", shape = 24, size = 0.7) +
  geom_point(data = substations_point,
             mapping = aes(x = lon, y = lat),
             col = "black", fill = "red", shape = 22, size = 0.7) +
  scale_fill_gradientn(colours = terrain.colors(100)) +
  theme_bw() +
  labs(x = "Longitude [°]", y = "Latitude [°]", fill = "Altitude [m]")