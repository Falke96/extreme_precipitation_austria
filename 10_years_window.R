library("lubridate")
library("bamlss")
library("reshape")
library("dplyr")
library("this.path")
library("ggplot2")
library("cowplot")
library("pbapply")

path <- this.dir()
source(file.path(path,"tmev_package", "mevr", "R", "mevr.R"))
source(file.path(path, "plotting_insert.R"))
source(file.path(path, "ccc.R"))

#Load data
load(file.path(path, "data", "rain_aut.Rda"))

#Calculate windows
tmev_window <- function(df, t = c(10, 50, 100), window = 10) {
  nd <- df[[2]]
  nd$dates <- as.Date(with(nd, paste(year, month, day, sep = "-")), "%Y-%m-%d")
  #Entferne erstes und letzes Jahr
  recyears <- head(unique(nd$year), -1)[-1]
  timeseries<- data.frame()
  
  for (i in 1:(length(recyears) - window + 1)){
    years <- recyears[i:(i + window - 1)]
    sub_data <- nd %>% 
      filter(year %in% years) %>%
      dplyr::select(dates, val)
    
    para_data <- ftmev(sub_data)$data
    timeseries[i, 1] <- max(years)
    timeseries[i, seq_along(t) + 1] <- qtmev(p = 1 - 1 / t, data = para_data)
  }
  colnames(timeseries) <- c("year", paste("tmev", t, sep = ""))
  return(timeseries)
}

rain_sub <- vector(mode = "list", length = length(rain_aut))
for (i in seq_along(rain_aut)){
  if (length(rain_aut[[i]][[3]]$year) < 50) {
    next
  }
  names(rain_sub)[i] <- rain_aut[[i]][[1]]$name
  rain_sub [[i]] <- rain_aut[[i]]
}

rain_sub <- rain_sub[lengths(rain_sub) != 0]

tmev_timeseries <- pblapply(rain_sub, tmev_window, cl=NR_CORES)

##Calculate medians
dat <- Reduce(function(x, y) merge(x, y, all = TRUE, by = "year"), tmev_timeseries)
colnames(dat)[seq(2, ncol(dat), 3)] = paste("tmev10", names(tmev_timeseries), seq_along(rain_sub), sep = "")
colnames(dat)[seq(3, ncol(dat), 3)] = paste("tmev50", names(tmev_timeseries), seq_along(rain_sub), sep = "")
colnames(dat)[seq(4, ncol(dat), 3)] = paste("tmev100", names(tmev_timeseries), seq_along(rain_sub), sep = "")

median_data <- data.frame(year = dat$year,
                rl10 = apply(dat %>% dplyr::select(starts_with("tmev10")), 1, median, na.rm = TRUE) / 10,
                rl50 = apply(dat %>% dplyr::select(starts_with("tmev50")), 1, median, na.rm = TRUE) / 10,
                rl100 = apply(dat %>% dplyr::select(starts_with("tmev100")), 1, median, na.rm = TRUE) / 10, 
                count = (apply(dat, 1, function(x) sum(!is.na(x)))- 1) / 3
) %>%
  dplyr::filter(year >= 1900,
         year < 2017)


#Create window plot
colors <- c("10 years" = "green", "50 years" = "blue", "100 years" = "black")
coeff <- 32 / 7.5

window_stations <- ggplot(median_data, aes(x = year)) +
  geom_segment(
    mapping = aes(x = year, y =  count / coeff + 75, xend = year, yend = -5), 
    linewidth = 1, lineend = "butt", colour = gray(0.8)) +
  geom_line(aes(y = rl10, color = "10 years")) +
  geom_line(aes(y = rl50, color = "50 years")) +
  geom_line(aes(y = rl100, color = "100 years")) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank()) +
  scale_color_manual(values = colors,
                     limits=c("10 years", "50 years", "100 years")) +
  scale_x_continuous(breaks=c(1900, 1925, 1950, 1980, 1990, 2000, 2010, 2020)) + 
  scale_y_continuous(sec.axis=sec_axis(~ . * coeff - 320, 
                                       name="Number of available stations",
                                       breaks = c(0, 100, 200, 300))) +
  labs(x = "Year",
       y = "50-year daily rainfall return levels [mm]",
       color = "") +
  coord_cartesian(ylim = c(70, 150))

#Create insert
stations_df = data.frame()
for (i in seq_along(rain_aut)) {
  stations_df[i,1] = rain_aut[[i]]$meta$name
  stations_df[i,2] = rain_aut[[i]]$meta$lon
  stations_df[i,3] = rain_aut[[i]]$meta$lat
  stations_df[i,4] = tail(as.numeric(rain_aut[[i]]$max_data$year), n= 1)
  stations_df[i,5] = length(rain_aut[[i]]$max_data$year)
}
names(stations_df) = c("Name", "lon", "lat", "endjahr", "num_years")

stations_data = subset(stations_df, num_years >= 50)

stations_insert = plot_stations_insert(stations_data = stations_data)


#Create actual plot
fig_size <- .25
map_with_inset <-
  ggdraw() +
  draw_plot(window_stations) +
  draw_plot(stations_insert, x=0.1, y=0.9, width=fig_size, height=fig_size, vjust=1)

map_with_inset



