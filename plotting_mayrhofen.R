#-------------------------------------------------------------------------
# paper plots fig 10, 11, 12
# 
# tmev paper, hsc, 01.2023
#-------------------------------------------------------------------------
library("this.path")
library("tidyverse")
library("lubridate")
library("bamlss")
library("scales")

path <- this.dir()
DATA_PATH <- file.path(path, "data")
FIG_PATH <- file.path(path, "paper", "figures")

source(file.path(path, "tmev_package", "mevr", "R", "mevr.R"))
load(file.path(path, "data", "rain_aut.Rda"))  # Loads rain_aut

GG_BASE_SIZE <- 12
GG_PLOT_HEIGHT <- 203
GG_PLOT_UNIT <- "mm"
GG_PLOT_WIDTH <- 146
IDX = 306 # Index of selected station (306: Mayrhofen)

meta <- tibble(
  rain_aut_idx = IDX,
  mrisc_stnr = rain_aut[[IDX]]$meta$mrisc_stnr,
  name = rain_aut[[IDX]]$meta$name,
  lon = rain_aut[[IDX]]$meta$lon,
  lat = rain_aut[[IDX]]$meta$lat,
  alt = rain_aut[[IDX]]$meta$alt
)

# trends of 50a return levels
plot_trends_example <- function(rain_aut) {
  austria0.spdf <- readRDS(file.path(DATA_PATH, "AUT_adm0.rds"))
  rl_list <- readRDS(file.path(DATA_PATH, "window_mev_tmev.Rds"))

  trends <- rl_list %>% as_tibble()
  
  x <- right_join(meta, trends, by = "mrisc_stnr")

  st_name <- x$name

  statcoords <- tibble(
                  lon = rain_aut[[IDX]]$meta$lon,
                  lat = rain_aut[[IDX]]$meta$lat
                )

  bg <- rl_list %>% 
    as_tibble() %>% 
    rename(MEVD = "mev", TMEV = "tmev") %>% 
    pivot_longer(cols = c("MEVD", "TMEV")) %>% 
    mutate(value = value / 10) %>% 
    filter(name == "TMEV") %>% 
    dplyr::select(years, value) %>% 
    mutate(season = factor("Full year"))

  year_breaks <- c(1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020)
  y_breaks <- c(0, 30, 60, 80, 100, 120)

  seasonal_rls <- readRDS(file.path(DATA_PATH, "trend_seasonality_example.Rds"))

  seasonal_rls <- seasonal_rls %>% 
                    mutate(month = str_to_title(month)) %>% 
                    mutate(season = fct_relevel(month, "Winter", "Spring", "Summer", "Fall"), val = val / 10) %>% 
                    rename(value = val, years = year) %>% 
                    dplyr::select(years, value, season)

  all_data <- rbind(bg, seasonal_rls)
  
  colors <- sequential_hcl(n = 5, h = c(-30, 200), c = c(70, 75, 35), l = c(30, 92), power = c(0.3, 1))
  rp <- 50

  mainplot <- all_data %>% 
                ggplot() +
                  geom_line(aes(years, value, group = season, color = season, linewidth = season)) +
                  scale_y_continuous(limits = range(y_breaks),
                                    breaks = y_breaks) +
                  scale_x_continuous(limits = range(year_breaks),
                                    breaks = year_breaks) +
                  scale_color_manual(values = colors) +
                  scale_discrete_manual("linewidth", values = c(0.8, rep(0.5, 4))) +
                  labs(x = "", y = paste0(rp, "-year daily rainfall [mm]"), color = "") +
                  guides(
                    linewidth = "none", 
                    color = guide_legend(override.aes = list(linewidth = c(0.8, rep(0.5, 4))))
                  ) +
                  ggtitle(paste0(str_to_title(st_name), " (", x$alt,"m)")) +
                  theme_bw(base_size = GG_BASE_SIZE) +
                  theme(
                    panel.grid.minor = element_blank(),
                    legend.title = element_blank()
                  )

  austria0 <- fortify(austria0.spdf)

  insertplot <- ggplot(austria0, aes(long, lat)) + 
                  geom_polygon(fill = NA, col = "black", linewidth = 0.1) +
                  geom_point(data = statcoords, aes(x = lon, y = lat)) +
                  guides(shape = "none", fill = "none", size = "none") +
                  theme_bw(base_size = GG_BASE_SIZE) +
                  theme(
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border     = element_blank(),
                    axis.line        = element_blank(),
                    axis.text        = element_blank(),
                    axis.ticks       = element_blank(),
                    axis.title       = element_blank()
                  )

  p <- mainplot + 
        annotation_custom(
          ggplotGrob(insertplot), 
          xmin = 1940, 
          xmax = 1960, 
          ymin = 0, 
          ymax = 30
        )

  ggsave(p, file = file.path(FIG_PATH, "trends_example.pdf"), width = GG_PLOT_WIDTH, height = GG_PLOT_HEIGHT, units = GG_PLOT_UNIT)
}


# monthly trend
plot_trends_year_yday_example <- function(rain_aut) {
  rr <- rain_aut[[IDX]]$data %>% 
          mutate(date = ymd(paste(year, month, day, sep = "-")), val = val / 10) %>%
          dplyr::select(date, val) %>%
          rename(rr = val)

  fittmev <- ftmev(rr, day_year_interaction = TRUE)

  paras_tmev_daily <- predict(fittmev) %>% 
                        as_tibble() %>%
                        mutate(
                          mu = c.pred * gamma(1 + 1 / w.pred),
                          sigma = c.pred * sqrt(gamma(1 + 2 / w.pred) - (gamma(1 + 1 / w.pred))^2)
                        )

  paras_tmev_daily$date <- fittmev$data$groupvar

  Sys.setenv(LANG = "en_US.UTF-8")
  day_mon_breaks <- cumsum(c(0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))
  day_mon_labels <- c("", "Jan", "", "Mar", "", "May", "",
                      "Jul", "", "Sep", "", "Nov", "")

  facet_labels <- c("TMEV expectation", "TMEV standard deviation")
  names(facet_labels) <- c("mu", "sigma")

  p <- paras_tmev_daily %>% 
          mutate(month = as.POSIXlt(date)$mon + 1,
                short_month = factor(format(date, "%b")),
                short_month_fact = fct_relevel(short_month, "Jan", "Feb", "Mrz", "Apr", "Mai", "Jun",
                                                "Jul", "Aug", "Sep", "Okt", "Nov", "Dez")) %>% 
          pivot_longer(cols = c("mu", "sigma", "c.pred", "w.pred")) %>% 
          filter(name %in% c("mu", "sigma")) %>% 
          ggplot() +
            geom_line(aes(yday, value, group = year, alpha = year, color = year)) +
            scale_colour_gradient2(midpoint = 1981,
                                  low = muted("blue"),
                                  high = muted("red"),) + 
            scale_x_continuous(limits = c(0, 366),
                              breaks = day_mon_breaks,
                              labels = day_mon_labels) +
            labs(x = "", 
                y = "[mm]",
                color = "Year") +
            guides(alpha = "none") +                   
            facet_wrap(~ name,
                      labeller = labeller(name = facet_labels)) +
            ggtitle(paste0(str_to_title(meta$name), " (", meta$alt,"m)")) +
            theme_bw(base_size = GG_BASE_SIZE) +
            theme(
              panel.grid.minor = element_blank(),
              legend.key.height = unit(0.5,"cm"),
              legend.key.width = unit(0.25,"cm")
            )
  
  ggsave(p, file = file.path(FIG_PATH, "trends_year_yday_example.pdf"), width = GG_PLOT_WIDTH, height = GG_PLOT_HEIGHT, units = GG_PLOT_UNIT)
}


plots_trends_monthly_maxima_example <- function() { 
  short_months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  year_breaks <- c(1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020)

  rls_change = readRDS(file.path(DATA_PATH,"trend_monthly_max_example.Rds"))

  res <- rls_change %>% 
            mutate(season = case_when(
              month %in% 6:8 ~ "Summer",
              month %in% 9:11 ~ "Fall",
              month %in% c(12, 1, 2) ~ "Winter",
              month %in% 3:5 ~ "Spring")) %>%
            mutate(season1 = case_when(
              month %in% 5:10 ~ "Summer",
              month %in% c(1:4, 11, 12) ~ "Winter"
            )) %>%
            mutate(season = fct_relevel(season, "Spring", "Summer", "Fall", "Winter"))

  p <- res %>% 
        ggplot(aes(year, month, group = mrisc_stnr)) +
          geom_point(shape = 1) +
          geom_smooth(aes(year, month), se = TRUE, linetype = "dashed", size = 0.5) +
          geom_hline(yintercept = c(6, 8), size = 0.1, color = "grey30") +
          geom_hline(yintercept = c(9, 11), size = 0.1, color = "grey30") +
          scale_y_continuous(breaks = 1:12,
                            limits = c(1, 12),
                            labels = short_months) +
          scale_x_continuous(limits = range(year_breaks),
                            breaks = year_breaks) +
          labs(x = "", y = "Month of maximum 50-year daily rainfall return level") +
          ggtitle(paste0(str_to_title(meta$name), " (", meta$alt,"m)")) +
          theme_bw(base_size = GG_BASE_SIZE) +
          theme(
            panel.grid.minor = element_blank(),
            legend.key.height = unit(0.5,"cm"),
            legend.key.width = unit(0.25,"cm")
          )
  
  ggsave(p, file = file.path(FIG_PATH, "trends_monthly_maxima_example.pdf"), width = GG_PLOT_WIDTH, height = GG_PLOT_HEIGHT, units = GG_PLOT_UNIT)
}

plot_trends_example(rain_aut)
plot_trends_year_yday_example(rain_aut)
plots_trends_monthly_maxima_example()