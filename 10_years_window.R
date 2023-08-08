library("boot")
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
median_confint = function(row, return_level){
  b <- boot(data = as.numeric(row), statistic = function(x,i) median(x[i], na.rm = TRUE),R = 1000)
  bconf <- boot.ci(b, type = "basic")
  ret <- c(bconf$t0, bconf$basic[4], bconf$basic[5])
  names(ret) = paste("rl", return_level, c("_median", "_low", "_high"), sep = "")
  return(ret)
  }

dat <- Reduce(function(x, y) merge(x, y, all = TRUE, by = "year"), tmev_timeseries)
colnames(dat)[seq(2, ncol(dat), 3)] = paste("tmev10_", names(tmev_timeseries), seq_along(rain_sub), sep = "")
colnames(dat)[seq(3, ncol(dat), 3)] = paste("tmev50_", names(tmev_timeseries), seq_along(rain_sub), sep = "")
colnames(dat)[seq(4, ncol(dat), 3)] = paste("tmev100_", names(tmev_timeseries), seq_along(rain_sub), sep = "")

dat <- dat %>% 
  dplyr::filter(year >= 1900, year < 2017)

median_data = data.frame(year= dat$year,
                         t(apply((dat %>% dplyr::select(starts_with("tmev10_")))[,-1], 
                                 MARGIN = 1, FUN = median_confint, return_level = 10)) / 10,
                         t(apply((dat %>% dplyr::select(starts_with("tmev50_")))[,-1], 
                                 MARGIN = 1, FUN = median_confint, return_level = 50)) / 10,
                         t(apply((dat %>% dplyr::select(starts_with("tmev100_")))[,-1], 
                                 MARGIN = 1, FUN = median_confint, return_level = 100)) / 10,
                         count = (apply(dat, 1, function(x) sum(!is.na(x)))- 1) / 3) %>% 
  dplyr::filter(count >= 20)


#Create window plot
colors <- c("10 years" = "green", "50 years" = "blue", "100 years" = "black")
coeff <- 32 / 9.5
intercept <- - 55 * 32 / 9.5

window_stations <- ggplot(median_data, aes(x = year)) +
  geom_segment(
    mapping = aes(x = year, y =  (count - intercept) / coeff, xend = year, yend = -5), 
    linewidth = 1, lineend = "butt", colour = gray(0.8)) +
  geom_line(aes(y = rl10_median, color = "10 years")) +
  geom_line(aes(y = rl50_median, color = "50 years")) +
  geom_line(aes(y = rl100_median, color = "100 years")) +
  geom_ribbon(aes(year, ymin = rl10_low, ymax = rl10_high), fill = "green", alpha = 0.2) +
  geom_ribbon(aes(year, ymin = rl50_low, ymax = rl50_high), fill = "blue", alpha = 0.2) +
  geom_ribbon(aes(year, ymin = rl100_low, ymax = rl100_high), fill = "black", alpha = 0.2) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank()) +
  scale_color_manual(values = colors,
                     limits=c("10 years", "50 years", "100 years")) +
  scale_x_continuous(breaks=c(1900, 1925, 1950, 1980, 1990, 2000, 2010, 2020)) + 
  scale_y_continuous(sec.axis=sec_axis(~ . * coeff + intercept, 
                                       name="Number of available stations",
                                       breaks = c(0, 100, 200, 300))) +
  labs(x = "Year",
       y = "Daily rainfall return levels [mm]",
       color = "") +
  coord_cartesian(ylim = c(55, 150))

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
fig_size <- .20
map_with_inset <-
  ggdraw() +
  draw_plot(window_stations) +
  draw_plot(stations_insert, x=0.15, y=0.95, width=fig_size, height=fig_size, vjust=1)

map_with_inset


# Visual the length of timeseries
series_len = data.frame(station = 1:319, startyear = 0, endyear = 0)

for (i in seq_along(rain_sub)) {
  series_len[i, 2] = max(rain_sub[[i]]$max_data$year[1], 1900)
  series_len[i, 3] = min(rain_sub[[i]]$max_data$year[length(rain_sub[[i]]$max_data$year)], 2016)
}


series_len = data.frame(startyear = 0, endyear = 0)

for (i in seq_along(rain_sub)) {
  series_len[i, 1] = max(rain_sub[[i]]$max_data$year[1], 1900)
  series_len[i, 2] = min(rain_sub[[i]]$max_data$year[length(rain_sub[[i]]$max_data$year)], 2016)
}


##station network over time
for (i in seq_along(rain_sub)){
  series_len[i, 3] = ifelse(1980 %in% rain_sub[[i]]$max_data$year, "yes", "no")
  series_len[i, 4] = ifelse(1990 %in% rain_sub[[i]]$max_data$year, "yes", "no")
  series_len[i, 5] = ifelse(2000 %in% rain_sub[[i]]$max_data$year, "yes", "no")
  series_len[i, 6] = ifelse(2010 %in% rain_sub[[i]]$max_data$year, "yes", "no")
  series_len[i, 7] = ifelse(2016 %in% rain_sub[[i]]$max_data$year, "yes", "no")
}

colnames(series_len) = c("startyear", "endyear", "active1980",
                         "active1990", "active2000", "active2010", "active2016")

stat1980 = subset(series_len, active1980 == "yes")
stat1990 = subset(series_len, active1980 == "yes" & active1990 == "yes")
stat2000 = subset(series_len, active1980 == "yes" & active1990 == "yes" & active2000 == "yes")
stat2010 = subset(series_len, active1980 == "yes" & active1990 == "yes" 
                  & active2000 == "yes" & active2010 == "yes")
stat2016 = subset(series_len, active1980 == "yes" & active1990 == "yes" 
                  & active2000 == "yes" & active2010 == "yes" & active2016 == "yes")


##Arange by Startyear and endyear
series_len = arrange(series_len, startyear, endyear)
series_len$station = 1:319

ggplot(series_len)+
  geom_segment(aes(y=station, yend = station, x = startyear, xend = endyear),
               size = 0.2 , alpha=0.6) +
  xlab("Year") + ylab("") +
  scale_x_continuous(breaks=c(1900, 1925, 1950, 1980, 1990, 2000, 2010, 2020)) +
  scale_y_continuous(breaks = NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())