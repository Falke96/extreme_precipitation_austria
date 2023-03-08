library(tidyverse)
library(lubridate)
library(mevr)
library(foreach)
library(doParallel)
cl <- makeCluster(4)
registerDoParallel(cl)

path <- this.dir()

source(file.path(path, "tmev_package", "mevr", "R", "mevr.R"))
load(file.path(path, "data", "rain_aut.Rda"))

WINDOW <- 10
RP <- 50
IDX = 306 # Mayrhofen

#----------------------------------
# window_mev_tmev.Rds
#----------------------------------

calc_window <- function(rp, window){
  data <- rain_aut[[IDX]][["data"]] %>%
    mutate(dates = ymd(paste0(year, sprintf("%02d", month), sprintf("%02d", day)))) %>%
    dplyr::select(dates, val) %>%
    as_tibble()
  
  r <- range(data$dates)
  years <- year(ymd(r[1])):year(ymd(r[2]))
  nr <- rain_aut[[IDX]]$meta$mrisc_stnr
  l <- foreach(j = 1:(length(years) - window + 1),
               .packages = c("lubridate", "tidyverse")) %do% {
    yr_idx <- j + window - 1
    sample <- years[j:yr_idx]
    d <- data[which(year(data$dates) %in% sample), ]
    
    all_years <- unique(year(d$dates))
    for(y in all_years){
      idx <- which(year(d$dates) == y)
      if(length(idx) < 366.25 * 0.1){
        d <- d[-idx, ]
      }
    }
    
    if(length(unique(year(d$dates))) < 5){
      c("mrisc_stnr" = nr, "years" = years[yr_idx], "mev" = NA, "tmev" = NA)
    } else {

      rl_mev <- tryCatch({
        fit_mev <- fmev(d)
        return.levels.mev(fit_mev, return.periods = rp)$rl
      },
      error = function(cond) {
        message(cond)
        return(NA)
      },
      warning = function(cond) {
        message(cond)
      })

      rl_tmev <- tryCatch({
        fit_tmev <- ftmev(d, minyears = 5)
        return.levels.mev(fit_tmev, return.periods = rp)$rl
      },
      error = function(cond) {
        message(cond)
        return(NA)
      },
      warning = function(cond) {
        message(cond)
      })
    }
    c("mrisc_stnr" = nr, "years" = years[yr_idx], "mev" = rl_mev, "tmev" = rl_tmev)
  }
  rl_list <- do.call(rbind, l)
  

  saveRDS(rl_list, file.path(path, "data", "window_mev_tmev.Rds"))
}

#----------------------------------
# trend_monthly_max_example.Rds
#----------------------------------

trend_monthly_max <- function(rp, window){
  data <- rain_aut[[IDX]][["data"]] %>% 
    mutate(dates = ymd(paste0(year, sprintf("%02d", month), sprintf("%02d", day)))) %>% 
    dplyr::select(dates, val) %>% 
    as_tibble()
  years <- unique(as.POSIXlt(data$dates)$year + 1900)
  rl_change <- list()
  for (y in 1:(length(years) - window + 1)){
    yr_idx <- y + window - 1
    sample <- years[y:yr_idx]
    data_sample <- data %>% 
      mutate(year = as.POSIXlt(dates)$year + 1900) %>% 
      filter(year %in% sample) %>% 
      dplyr::select(dates, val)
  
    fit <- ftmev(data_sample, day_year_interaction = TRUE)
    
    data_sample$c <- fit$c
    data_sample$w <- fit$w
    
    m_rls <- foreach (m = 1:12,
                      .export=c("ftmev", "qtmev", "ptmev"),
                      .packages = c("lubridate", "tidyverse")) %dopar% {
      d <- data_sample %>% 
        filter(as.POSIXlt(dates)$mon + 1 == m) %>% 
        mutate(year = as.POSIXlt(dates)$year + 1900,
               yday = as.POSIXlt(dates)$yday)
      rl <- qtmev(1 - 1 / rp, d)
      tibble(month = m, value = rl)
    }
    
    rls <- do.call(rbind, m_rls) %>% 
      as_tibble %>% 
      na.omit()
    rls <- rls %>% 
      mutate(month = 1:nrow(rls)) %>% 
      arrange(desc(value)) 
    rl_change[[y]] <- tibble(y_idx = IDX, 
                             name = rain_aut[[IDX]]$meta$name, 
                             mrisc_stnr = rain_aut[[IDX]]$meta$mrisc_stnr, 
                             year = years[yr_idx], 
                             month = rls$month[1], 
                             val = rls$value[1])
    
  }
  rls_change <- do.call(rbind, rl_change)
  saveRDS(rls_change, file = file.path(path, "data", "trend_monthly_max_example.Rds"))
}

#----------------------------------
# trend_seasonality_example.Rds
#----------------------------------

trend_seasonality <- function(rp, window){
  seasons <- list(
    "spring" = c(3, 4, 5), 
    "summer" = c(6, 7, 8), 
    "fall" = c(9, 10, 11), 
    "winter" = c(12, 1, 2) 
  )
  data <- rain_aut[[IDX]][["data"]] %>% 
    mutate(dates = ymd(paste0(year, sprintf("%02d", month), sprintf("%02d", day)))) %>% 
    dplyr::select(dates, val) %>% 
    as_tibble()
  years <- unique(as.POSIXlt(data$dates)$year + 1900)
  rl_change <- list()
  for (y in 1:(length(years) - window + 1)){
    yr_idx <- y + window - 1
    sample <- years[y:yr_idx]
    data_sample <- data %>% 
      mutate(year = as.POSIXlt(dates)$year + 1900) %>% 
      filter(year %in% sample) %>% 
      dplyr::select(dates, val)
    
    fit <- ftmev(data_sample, day_year_interaction = TRUE)
    
    data_sample$c <- fit$c
    data_sample$w <- fit$w
    
    m_rls <- foreach (m = seq_along(seasons),
                      .export=c("ftmev", "qtmev", "ptmev"),
                      .packages = c("lubridate", "tidyverse")) %dopar% {
      d <- data_sample %>% 
        filter((as.POSIXlt(dates)$mon + 1) %in% seasons[[m]]) %>% 
        mutate(year = as.POSIXlt(dates)$year + 1900,
               yday = as.POSIXlt(dates)$yday)
      rl <- qtmev(1 - 1 / rp, d)
      tibble(season = names(seasons)[m], value = rl)
    }
    
    rls <- do.call(rbind, m_rls) %>% 
      as_tibble %>% 
      na.omit()
    rl_change[[y]] <- tibble(name = rain_aut[[IDX]]$meta$name, 
                             mrisc_stnr = rain_aut[[IDX]]$meta$mrisc_stnr, 
                             year = years[yr_idx], 
                             month = rls$season, 
                             val = rls$value)
    
  }
  rls_change <- do.call(rbind, rl_change)
  saveRDS(rls_change, file = file.path(path, "data", "trend_seasonality_example.Rds"))
}

calc_window(rp = RP, window = WINDOW)
trend_monthly_max(rp = RP, window = WINDOW)
trend_seasonality(rp = RP, window = WINDOW)