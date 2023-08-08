library("this.path")
library("dplyr")
library("bamlss")
library("lubridate")
library("reshape")
library("ggplot2")
library("cowplot")
library("pbapply")

path <- this.dir()
load(file.path(path, "data", "rain_aut.Rda"))

source(file.path(path,"tmev_package", "mevr", "R", "mevr.R"))
source(file.path(path, "plotting_insert.R"))
source(file.path(path, "ccc.R"))

tmev_params <- function(df) {
  nd <- df[[2]]
  nd$dates <- as.Date(with(nd, paste(year, month, day, sep = "-")), "%Y-%m-%d")
  recyears <- head(unique(nd$year), -1)[-1]
  sub_data <- nd %>% 
    dplyr::filter(year %in% recyears) %>%
    dplyr::mutate(val = val / 10) %>%
    dplyr::select(dates, val)
  model = ftmev(sub_data)$x
  
  nd2 <- data.frame(year = recyears)
  nd2$lambda <- predict(model, newdata = nd2, model = "lambda", term = "year",
                        intercept = TRUE, type = "parameter")
  nd2$alpha <- predict(model, newdata = nd2, model = "alpha", term = "year",
                       intercept = TRUE, type = "parameter")
  nd2$mu <-  nd2$lambda * gamma(1 + 1 / nd2$alpha)
  nd2$sigma <- with(nd2, lambda * sqrt(gamma(1 + 2 / alpha) - (gamma(1 + 1 / alpha))^2))
  return(nd2)
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

params_timeseries <- pblapply(rain_sub, tmev_params, cl=NR_CORES)
dat <- Reduce(function(x, y) merge(x, y, all = TRUE, by = "year"), params_timeseries)
colnames(dat)[seq(2, ncol(dat), 4)] = paste("lambda", names(params_timeseries), seq_along(rain_sub), sep = "")
colnames(dat)[seq(3, ncol(dat), 4)] = paste("alpha", names(params_timeseries), seq_along(rain_sub), sep = "")
colnames(dat)[seq(4, ncol(dat), 4)] = paste("mu", names(params_timeseries), seq_along(rain_sub), sep = "")
colnames(dat)[seq(5, ncol(dat), 4)] = paste("sigma", names(params_timeseries), seq_along(rain_sub), sep = "")

params_data <- data.frame(year = dat$year,
                          lambda = apply(dat %>% dplyr::select(starts_with("lambda")), 1, mean, na.rm = TRUE),
                          alpha = apply(dat %>% dplyr::select(starts_with("alpha")), 1, mean, na.rm = TRUE),
                          mu = apply(dat %>% dplyr::select(starts_with("mu")), 1, mean, na.rm = TRUE), 
                          sigma = apply(dat %>% dplyr::select(starts_with("sigma")), 1, mean, na.rm = TRUE),
                          count = (apply(dat, 1, function(x) sum(!is.na(x)))- 1) / 4
) %>%
  dplyr::filter(year >= 1900,
                year < 2017, 
                count >= 20)

#Create Plot
colors <- c("Mean" = "green", "Standard deviation" = "blue")
coeff <- 64

window_stations_paras <- ggplot(params_data, aes(x = year)) +
  geom_segment(
    mapping = aes(x = year, y =  (count + 320) / coeff, xend = year, yend = -1), 
    linewidth = 1, lineend = "butt", colour = gray(0.8)) +
  geom_line(aes(y = mu, color = "Expectation")) +
  geom_line(aes(y = sigma, color = "Standard deviation")) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank()) +
  scale_x_continuous(breaks=c(1900, 1925, 1950, 1980, 1990, 2000, 2010, 2020)) + 
  scale_y_continuous(sec.axis=sec_axis(~ . * coeff - 320, 
                                       name="Number of available stations",
                                       breaks = c(0, 100, 200, 300))) +
  labs(x = "Year",
       y = "",
       color = "") +
  coord_cartesian(ylim = c(5, 10))


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
  draw_plot(window_stations_paras) +
  draw_plot(stations_insert, x=0.1, y=0.96, width=fig_size, height=fig_size, vjust=1)

map_with_inset