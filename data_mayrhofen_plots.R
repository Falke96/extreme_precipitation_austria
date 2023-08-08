library("doParallel")
library("foreach")
library("lubridate")
library("this.path")
library("tidyverse")
library("bamlss")

cl <- makeCluster(4)
registerDoParallel(cl)

path <- this.dir()

source(file.path(path, "tmev_package", "mevr", "R", "mevr.R"))
load(file.path(path, "data", "rain_aut.Rda"))

WINDOW <- 10
RP <- 50
NITER <- 1000
ALPHA <- 0.05
IDX <- 306 # Mayrhofen

#----------------------------------
# window_mev_tmev.Rds
#----------------------------------

calc_window <- function(rp, window){
  data <- rain_aut[[IDX]][["data"]]  |> 
    mutate(dates = ymd(paste0(year, sprintf("%02d", month), sprintf("%02d", day)))) |>
    dplyr::select(dates, val) |>
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
  
  saveRDS(rl_list, file.path(DATA_PATH, "window_mev_tmev.Rds"))
}

#----------------------------------
# trend_monthly_max_example.Rds
#----------------------------------

trend_monthly_max <- function(rp, window){
  data <- rain_aut[[IDX]][["data"]] |> 
    mutate(dates = ymd(paste0(year, sprintf("%02d", month), sprintf("%02d", day)))) |> 
    dplyr::select(dates, val) |> 
    as_tibble()
  years <- unique(as.POSIXlt(data$dates)$year + 1900)
  rl_change <- list()
  for (y in 1:(length(years) - window + 1)){
    yr_idx <- y + window - 1
    sample <- years[y:yr_idx]
    data_sample <- data |> 
      mutate(year = as.POSIXlt(dates)$year + 1900) |> 
      filter(year %in% sample) |> 
      dplyr::select(dates, val)
  
    fit <- ftmev(data_sample, day_year_interaction = TRUE)
    
    data_sample$c <- fit$c
    data_sample$w <- fit$w
    
    m_rls <- foreach (m = 1:12,
                      .export=c("ftmev", "qtmev", "ptmev"),
                      .packages = c("lubridate", "tidyverse")) %dopar% {
      d <- data_sample |> 
        filter(as.POSIXlt(dates)$mon + 1 == m) |> 
        mutate(year = as.POSIXlt(dates)$year + 1900,
               yday = as.POSIXlt(dates)$yday)
      rl <- qtmev(1 - 1 / rp, d)
      tibble(month = m, value = rl)
    }
    
    rls <- do.call(rbind, m_rls) |> 
      as_tibble() |> 
      na.omit()
    rls <- rls |> 
      mutate(month = 1:nrow(rls)) |> 
      arrange(desc(value)) 
    rl_change[[y]] <- tibble(y_idx = IDX, 
                             name = rain_aut[[IDX]]$meta$name, 
                             mrisc_stnr = rain_aut[[IDX]]$meta$mrisc_stnr, 
                             year = years[yr_idx], 
                             month = rls$month[1], 
                             val = rls$value[1])
    
  }
  rls_change <- do.call(rbind, rl_change)
  saveRDS(rls_change, file = file.path(DATA_PATH, "trend_monthly_max_example.Rds"))
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
  data <- rain_aut[[IDX]][["data"]] |> 
    mutate(dates = ymd(paste0(year, sprintf("%02d", month), sprintf("%02d", day)))) |> 
    dplyr::select(dates, val) |> 
    as_tibble()
  years <- unique(as.POSIXlt(data$dates)$year + 1900)
  rl_change <- list()
  for (y in 1:(length(years) - window + 1)){
    yr_idx <- y + window - 1
    sample <- years[y:yr_idx]
    data_sample <- data |> 
      mutate(year = as.POSIXlt(dates)$year + 1900) |> 
      filter(year %in% sample) |> 
      dplyr::select(dates, val)
    
    fit <- ftmev(data_sample, day_year_interaction = TRUE)
    
    data_sample$c <- fit$c
    data_sample$w <- fit$w
    
    m_rls <- foreach (m = seq_along(seasons),
                      .export=c("ftmev", "qtmev", "ptmev"),
                      .packages = c("lubridate", "tidyverse")) %dopar% {
      d <- data_sample |> 
        filter((as.POSIXlt(dates)$mon + 1) %in% seasons[[m]]) |> 
        mutate(year = as.POSIXlt(dates)$year + 1900,
               yday = as.POSIXlt(dates)$yday)
      rl <- qtmev(1 - 1 / rp, d)
      tibble(season = names(seasons)[m], value = rl)
    }
    
    rls <- do.call(rbind, m_rls) |> 
      as_tibble() |> 
      na.omit()
    rl_change[[y]] <- tibble(name = rain_aut[[IDX]]$meta$name, 
                             mrisc_stnr = rain_aut[[IDX]]$meta$mrisc_stnr, 
                             year = years[yr_idx], 
                             month = rls$season, 
                             val = rls$value)
    
  }
  rls_change <- do.call(rbind, rl_change)
  saveRDS(rls_change, file = file.path(DATA_PATH, "trend_seasonality_example.Rds"))
}


#---------------------------------------------------------------
# trend_seasonality_example_", window, "a_", NITER, "niter.Rds
#---------------------------------------------------------------

trend_seasonality_ci <- function(idx, rp, window, niter, alpha) {
  data <- rain_aut[[idx]][["data"]]  |>  
    mutate(dates = ymd(paste0(year, sprintf("%02d", month), sprintf("%02d", day)))) |> 
    dplyr::select(dates, val, year) |>  
    as_tibble()
  years <- unique(as.POSIXlt(data$dates)$year + 1900)
  
  rl_change <- list()
  for (y in 1:(length(years) - window + 1)){
    yr_idx <- y + window - 1
    message(y, " / ", (length(years) - window + 1), ", ", years[yr_idx])
    
    sample <- years[y:yr_idx]
    data_sample <- data |> 
      mutate(#month = as.POSIXlt(dates)$mon + 1, 
        year = as.POSIXlt(dates)$year + 1900) |> 
      filter(year %in% sample) |> 
      dplyr::select(dates, val, year)
    
    # randomly sample niter times and calculate rl uncertainties
    rls <- foreach (i = 1:niter, .export = c("ftmev", "qtmev"), .packages = c("lubridate", "dplyr", "bamlss")) %dopar% {
      sample_years <- unique(data_sample$year)
      new_sample <- sample(sample_years, window, replace = TRUE)
      
      start_year <- 2000 # arbitrary
      sam_list <- list()
      for (j in seq_along(new_sample)) {
        sam_list[[j]] <- data_sample |> 
          filter(year(dates) == new_sample[j]) |> 
          mutate(mon = month(dates),
                 day = day(dates),
                 year = start_year + j) |> 
          mutate(dates1 = ymd(paste0(year, sprintf("%02d", mon), sprintf("%02d", day), sep = "-"))) |> 
          dplyr::select(dates1, val) |> 
          rename(dates = "dates1") |> 
          na.omit()
      }
      sam <- do.call(rbind, sam_list)
      
      fit <- ftmev(sam)
      qtmev(1 - 1 / rp, fit$data)
    }
    rls <- do.call("c", rls)
    sd_rls <- sd(rls)
    cis <- quantile(rls, probs = c(alpha / 2, 0.5, 1 - alpha / 2)) 
    sds <- c(cis[2] - sd_rls, cis[2], cis[2] + sd_rls)
    
    fit <- ftmev(data_sample |> dplyr::select(-year), minyears = 5)
    est <- qtmev(1 - 1 / rp, fit$data)
    data_sample$c <- fit$c
    data_sample$w <- fit$w
    
    seasons <- list(
      "spring" = c(2, 3, 4), 
      "summer" = c(6, 7, 8), 
      "fall" = c(9, 10, 11), 
      "winter" = c(12, 1, 2) 
    )
    
    # calculate seasonal contributions
    m_rls <- foreach (m = seq_along(seasons), .packages = c("lubridate", "tidyverse")) %do% {
      d <- data_sample |> 
        filter((as.POSIXlt(dates)$mon + 1) %in% seasons[[m]]) |> 
        mutate(year = as.POSIXlt(dates)$year + 1900,
               yday = as.POSIXlt(dates)$yday)
      rl <- qtmev(1 - 1 / rp, d)
      tibble(season = names(seasons)[m], value = rl)
    }
    rls_seas <- do.call(rbind, m_rls) |> 
      as_tibble() |> 
      na.omit()
    
    rl_change[[y]] <- tibble(name = rain_aut[[idx]]$meta$name, 
                             mrisc_stnr = rain_aut[[idx]]$meta$mrisc_stnr, 
                             year = years[yr_idx], 
                             month = rls_seas$season, 
                             val = rls_seas$value,
                             rl = est,
                             rl_ci = cis[2],
                             cil = cis[1],
                             ciu = cis[3],
                             sdl = sds[1],
                             sdu = sds[3])
    
  }
  rls_change <- do.call(rbind, rl_change)
  saveRDS(rls_change, file = file.path(DATA_PATH, "trend_seasonality_example_", window, "a_", niter, "niter.Rds"))
  stopCluster(cl)
}


#------------------------------------------------------------------
# trend_seasonality_seas_example_", window, "a_", niter, "niter.Rds
#------------------------------------------------------------------

trend_seasonality_ci_seas <- function(idx, rp, window, niter, alpha) {
  data <- rain_aut[[idx]][["data"]]  |>  
    mutate(dates = ymd(paste0(year, sprintf("%02d", month), sprintf("%02d", day)))) |> 
    dplyr::select(dates, val, year) |>  
    as_tibble()
  years <- unique(as.POSIXlt(data$dates)$year + 1900)
  
  rl_change <- list()
  for (y in 1:(length(years) - window + 1)){
    yr_idx <- y + window - 1
    message(y, " / ", (length(years) - window + 1), ", ", years[yr_idx])
    
    sample <- years[y:yr_idx]
    data_sample <- data %>% 
      mutate(#month = as.POSIXlt(dates)$mon + 1, 
        year = as.POSIXlt(dates)$year + 1900) %>% 
      filter(year %in% sample) %>% 
      dplyr::select(dates, val, year)
    
    fit <- ftmev(data_sample |> dplyr::select(-year), minyears = 5)
    est <- qtmev(1 - 1 / rp, fit$data)
    data_sample$c <- fit$c
    data_sample$w <- fit$w
    
    seasons <- list(
      "spring" = c(2, 3, 4), 
      "summer" = c(6, 7, 8), 
      "fall" = c(9, 10, 11), 
      "winter" = c(12, 1, 2) 
    )
    
    # randomly sample niter times and calculate sd of rl
    m_rls <- foreach (m = seq_along(seasons), .packages = c("lubridate", "tidyverse")) %do% {
      rls_seas <- foreach (i = 1:niter, .export = c("ftmev", "qtmev"), .packages = c("lubridate", "dplyr", "bamlss")) %dopar% {
        sample_years <- unique(data_sample$year)
        new_sample <- sample(sample_years, window, replace = TRUE)
        start_year <- 2000
        sam_list <- list()
        for (j in seq_along(new_sample)) {
          sam_list[[j]] <- data_sample |>
            filter(year(dates) == new_sample[j]) |>
            mutate(mon = month(dates),
                   day = day(dates),
                   year = start_year + j) |>
            mutate(dates1 = ymd(paste0(year, sprintf("%02d", mon), sprintf("%02d", day), sep = "-"))) |>
            dplyr::select(dates1, val, year, c, w) |>
            rename(dates = "dates1") |>
            na.omit()
        }
        sam <- do.call(rbind, sam_list)
        d <- sam %>%
          filter((as.POSIXlt(dates)$mon + 1) %in% seasons[[m]]) %>%
          mutate(year = as.POSIXlt(dates)$year + 1900,
                 yday = as.POSIXlt(dates)$yday)
        rl <- qtmev(1 - 1 / rp, d)
        tibble(season = names(seasons)[m], value = rl)
      }
      rls_seas <- do.call("rbind", rls_seas)
      sd_rls_seas <- sd(rls_seas$value)
      cis <- quantile(rls_seas$value, probs = c(alpha / 2, 0.5, 1 - alpha / 2))
      sds <- c(cis[2] - sd_rls_seas, cis[2], cis[2] + sd_rls_seas)
      tibble(season = unique(rls_seas$season),
             rl_ci = cis[2],
             cil = cis[1],
             ciu = cis[3],
             sdl = sds[1],
             sdu = sds[3])
  
    }
    
    m_rls <- do.call(rbind, m_rls)
    rl_change[[y]] <- tibble(name = rain_aut[[idx]]$meta$name,
                             mrisc_stnr = rain_aut[[idx]]$meta$mrisc_stnr,
                             year = years[yr_idx],
                             m_rls)
   
    
  }
  rls_change <- do.call(rbind, rl_change)
  saveRDS(rls_change, file = paste0("trend_seasonality_seas_example_", window, "a_", niter, "niter.Rds"))
  stopCluster(cl)
}
  
calc_window(rp = RP, window = WINDOW)
trend_monthly_max(rp = RP, window = WINDOW)
trend_seasonality(rp = RP, window = WINDOW)
trend_seasonality_ci(idx = IDX, rp = RP, window = WINDOW, niter = NITER, alpha = ALPHA)
trend_seasonality_ci_seas(idx = IDX, rp = RP, window = WINDOW, niter = NITER, alpha = ALPHA)
