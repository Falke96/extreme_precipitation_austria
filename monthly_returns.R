library("lubridate")
library("bamlss")
library("ggplot2")
library("cowplot")
library("viridis")
library("clock")
library("this.path")
library("dplyr")
library("doParallel")

path <- this.dir()
load(file.path(path, "data", "rain_aut.Rda"))

source(file.path(path,"tmev_package", "mevr", "R", "mevr.R"))
source(file.path(path, "ccc.R"))
source(file.path(path, "plotting_insert.R"))

Sys.setlocale("LC_ALL", "en_GB.UTF-8")
dates <- seq.Date(from = as.Date("2012-01-01"), length.out = 12, by = "month")
format(dates, "%B")

# Monthly return levels
rlmonthly = function(df) {
  nd <- df[[2]]
  nd$val <- nd$val / 10
  nd$dates <- as.Date(with(nd, paste(year, month, day, sep = "-")), "%Y-%m-%d")
  data <- nd %>% 
    dplyr::select(dates, val)
  para_data <- ftmev(data, day_year_interaction = TRUE)$data
  para_data$month <- month(para_data$groupvar)
  return(para_data)
}

Anras = rlmonthly(rain_aut[[2]])
Mader = rlmonthly(rain_aut[[7]])

Anrasmonths = data.frame(month = dates)
Madermonths = data.frame(month = dates)

p <- 1 - 1 / RETURN_PERIOOD

for (i in 1:12) {
  Anrassubnd = subset(Anras, month == i)
  Anrasmonths[i, 2] = qtmev(p, Anrassubnd)
  
  Madersubnd = subset(Mader, month == i)
  Madermonths[i, 2] = qtmev(p, Madersubnd)
}

df <- rbind(Anrasmonths, Madermonths)
df$dataset = c(rep("Anras", nrow(Anrasmonths)), 
               rep("Mader", nrow(Madermonths)))

datasets.labs <- c("Anras [1971-2017]", "Mäder [1971-2015]")
names(datasets.labs) <- c("Anras", "Mader")

monthly_examples <- ggplot(df, aes(x = month, y = V2 )) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 100, col = "red", linetype = "dashed") +
  facet_wrap(~dataset, ncol = 1, labeller = labeller(dataset = datasets.labs)) +
  labs(x= "Month", y= "50-year daily rainfall return level [mm]") +
  scale_x_date(date_breaks = "2 month", date_labels = "%B") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())

Anras_map <- plot_stations_insert(as.data.frame(rain_aut[[2]]$meta), 
                                  color = "red", size = 4)
Mader_map <- plot_stations_insert(as.data.frame(rain_aut[[7]]$meta), 
                                  color = "red", size = 4)

fig_size <- .2
map_with_inset <-
  ggdraw() +
  draw_plot(monthly_examples) +
  draw_plot(Anras_map, x=0.1, y=0.91, width=fig_size, height=fig_size, vjust=1) +
  draw_plot(Mader_map, x=0.1, y=0.45, width=fig_size, height=fig_size, vjust=1)

map_with_inset

############ Monthly returns map

stationnames = data.frame()

for (i in seq_along(rain_aut)) {
  stationnames[i, 1] = rain_aut[[i]]$meta$name
  stationnames[i, 2] = rain_aut[[i]]$meta$lon
  stationnames[i, 3] = rain_aut[[i]]$meta$lat
}

names(stationnames) = c("Station", "lon", "lat")

registerDoParallel(NR_CORES)

stations = foreach (i = seq_along(rain_aut), .combine = rbind) %dopar% {
  nd <- rlmonthly(rain_aut[[i]])
  monat = data.frame(month = dates, rl = 0)
  for (j in 1:12) {
    subnd = subset(nd, month == j)
    monat[j, 2] = qtmev(p, subnd)
  }
  monat[which.max(monat$rl),]
}

plotdf = cbind(stationnames, stations)
plotdf <- plotdf %>%
  mutate(
    monat = date_month_factor(month),
    season = factor(case_when(
      month(month) %in%  9:11 ~ "Fall",
      month(month) %in%  c(12, 1, 2)  ~ "Winter",
      month(month) %in%  3:5  ~ "Spring",
      TRUE ~ "Summer"), levels = c("Winter", "Spring", "Summer", "Fall")))

data("Austria", package = "bamlss")
austria <- as.data.frame(coordinates(AustriaTopo))
names(austria) <- c("lon", "lat", "alt")

ggplot(austria, aes(lon, lat)) +
  geom_raster(aes(fill = alt), alpha = 0.5) +
  geom_point(data = plotdf,
             mapping = aes(x = lon, y= lat, col = season, size = rl)) +
  scale_color_viridis(option = "plasma", discrete = TRUE) +
  scale_fill_gradientn(colours = terrain.colors(100)) +
  scale_size(range = c(0, 5)) +
  labs(size="50-years-returns [mm]", colour="Month") +
  theme_bw() +
  labs(x = "Longitude [°]", y = "Latitude [°]", fill = "Altitude [m]")
