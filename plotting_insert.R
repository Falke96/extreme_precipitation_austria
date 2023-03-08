library("raster")
library("ggplot2")

plot_stations_insert = function(stations_data, color = "grey70", size = 0.3) {
  austriaraw <- getData(country = "Austria", level = 0)
  austria <- fortify(austriaraw)
  
  ggplot(data = stations_data, aes(lon,lat)) +
    geom_point(color = color, size = size) +
    geom_polygon(data =  austria, aes(x = long, y = lat, group = group),
                 fill = NA, color = "black") +
    theme_void()
}
