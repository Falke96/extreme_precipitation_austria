library(tidyverse)

#' Weibull tail test
#' 
#' Adopted from Matlab Code of F. Marra (2023)
#' Null-Hyothesis: block maxima are samples from a parent distribution with
#' Weibull tail (tail defined by a given left censoring threshold)
#'
#' @param samples Data sample (data.frame) with all independent ordinary events, i.e. wet days. 
#' @param left_cens_prctile_thr left censoring threshold (percentile of samples).
#' @param p_test probability to be used for the test (test will count the block 
#'   maxima out of the Y = 1-p_out confidence interval).
#' @param niter number of stochastic realizations.
#' @param censorbm logical (\code{censorbm = TRUE} to censor the block maxima.
#'
#' @return \code{is_rejected} outcome of the test (true means that the assumption of
#'   Weibull tails for the given left-censoring threshold is rejected).
#'   
#'   \code{p_out} fraction of block maxima outside of the Y = 1-p_out confidence interval 
#'   \code{p_hi} fraction of block maxima above the Y = 1 - p_out confidence interval 
#'   \code{p_lo} fraction of block maxima below the Y = 1 - p_out confidence interval 
#'   \code{scale} scale parameter of the Weibull distribution describing the non-censored samples
#'   \code{shape} shape parameter of the Weibull distribution describing the non-censored samples

#' @export
#'
#' @examples
weibull_tail_test <- function(samples, left_cens_prctile_thr, p_test, niter, censorbm) {
  samples1 <- samples |> 
    as_tibble() |> 
    mutate(vdate = as.Date(paste(year, month, day, sep = "-")),
           bm = FALSE) |> 
    rename(block = year) |> 
    dplyr::select(vdate, block, val, bm)
  bm <- samples1 |> 
    group_by(block) |>
    group_modify(~ {
      .x |>
        arrange(desc(val)) |>
        first()
    }) |> 
    pull(vdate)
  samples1$bm[which(samples1$vdate %in% bm)] <- TRUE
  samples1 <- samples1 |> 
    arrange(val)
  
  
  # estimate Weibull parameters censoring the sample that does not exceed the
  # given left-censoring & the block maxima  
  to_use <- max(1, floor(nrow(samples1) * left_cens_prctile_thr)):nrow(samples1) #censors sample below threshold
  #censored <- samples1[to_use, ]
  # censored$to_use <- to_use
  # if (isTRUE(censorbm)) {
  #   censored <- censored |>
  #     filter(bm == FALSE)
  #   to_use <- censored$to_use
  # }
  
  # parameter estimation (least square linear regression in Weibull-transformed coordinates) 
  ECDF <- (1:nrow(samples1)) / (nrow(samples1) + 1)      # empirical probabilities computed with Weibull plotting positions
  X <- log(log(1 / (1 - ECDF[to_use])))                  # Weibull-tranformation for probabilities
  Y <- log(samples1$val[to_use])                                 # Weibull-tranformation for samples
  tmp <- lm(Y ~ X)                                       # linear regression
  scale <- exp(tmp$coefficients[1])                      # Weibull scale parameter
  shape <-  1/tmp$coefficients[2]                        # Weibull shape parameter
  
  #istest <- which(censored$bm == TRUE) # values over which the hypothesis is tested, i.e. block maxima
  istest <- samples1$bm[to_use] # values over which the hypothesis is tested, i.e. block maxima
  
  # weibull-distributed stochastic samples
  randy <- apply(matrix(rnorm(nrow(samples1)), nrow = nrow(samples1), ncol = niter), 2, function(x) {
    y <- rweibull(x, shape, scale)
    sort(y)
  })
  
  # fraction of block maxima below/above the (1-p) CI
  p_lo <- mean(samples1$val[istest] < quantile(randy[istest, ], p_test / 2), na.rm = TRUE)
  p_hi <- mean(samples1$val[istest] > quantile(randy[istest, ], 1 - p_test / 2), na.rm = TRUE)
  #p_lo <- mean(censored$val[istest] < quantile(randy[istest, ], p_test / 2), na.rm = TRUE)
  #p_hi <- mean(censored$val[istest] > quantile(randy[istest, ], 1 - p_test / 2), na.rm = TRUE)
  
  # fraction of block maxima out of the (1-p) CI
  p_out <- p_hi + p_lo 
  
  # outcome of the test
  is_rejected <- p_out > p_test
  
  list(is_rejected = is_rejected, 
       p_out = p_out, 
       p_hi = p_hi, 
       p_lo = p_lo,
       scale = scale, 
       shape = shape)
  
}



rain <- get(load("../rain_aut.Rda"))
rm(rain_aut)

#all_files <- list.files("../data_csv/longer_than_50/", pattern = "*.csv")

p_test <- 0.1
niter <- 500
censorbm <- TRUE
thresholds <- seq(0, 0.95, 0.05)

s_list <- c(5015471, 5022013,5022014,5022029,5022034,5022040,5022041,
            5022042,5022054,5022099)
all_nrs <- lapply(rain, function(x) {
  x$meta$mrisc_stnr
})
all_nrs <- do.call("c", all_nrs)
s_list_idx <- list()
for (i in seq_along(s_list)) {
  s_list_idx[[i]] <- which(s_list[i] == all_nrs)
}
s_list_idx <- do.call("c", s_list_idx)

res <- list()
#for (s in 1:length(rain)) {
for (s in s_list_idx) {
  d <- rain[[s]]
  samples <- d$data  
  
  if (!d$meta$mrisc_stnr %in% s_list) {
    next
  }
  
  #samples <- read_csv2(paste0("../data_csv/longer_than_50/", all_files[s]), show_col_types = FALSE)
  #samples <- samples |> 
  #  mutate(month = 1, day = 1) |> 
  #  relocate(c("val", "year", "month", "day"))
    
  is_rejected <- list()
  test_res <- list()
  for (i in seq_along(thresholds)) {
    left_cens_prctile_thr <- thresholds[i]
    wei_test <- weibull_tail_test(samples, left_cens_prctile_thr, p_test, niter, censorbm)
    is_rejected[[i]] <- wei_test$is_rejected
    test_res[[i]] <- wei_test
  }
  is_rejected <- do.call("c", is_rejected)
  
  
  # find the optimal left-censoring thresholds
  # (last TRUE)
  #i_thr <- which(is_rejected) + 1
  # if (length(i_thr) == 0) {
  #   i_thr <- 1
  # }
  i_thr <- last(which(is_rejected))
  if (is.na(i_thr)) {
    i_thr <- 1
  }
  
  # no censored weibull distribution, is able to provide observed maxima from its samples
  # i.e. even if we restrict the data series to only the tail values, 
  # the observed maxima do likely not come from this tail
  # if (any(i_thr > length(thresholds))) { 
  #   #warning("the assumption of Weibull tail is rejected")
  #   i_thr <- NA
  #   optimal_thr <- .95
  #   rejected <- TRUE # max are NOT from weibul tail
  # } else {
  #   optimal_thr = thresholds[last(i_thr)]
  #   rejected <- FALSE # max are from weibull tail
  # }
  if (i_thr == length(thresholds)) {
    optimal_thr <- 0.95
    rejected <- TRUE # max are NOT from weibul tail
  } else {
    optimal_thr = thresholds[i_thr]
    rejected <- FALSE # max are from weibull tail
  }
  
  print(paste0("station: ", s, " optimal_threshold = ", optimal_thr, ", rejected = ", rejected))
  res[[s]] <- list(rejected = rejected, is_rejected = is_rejected, res = test_res)
}
save(res, file = "wei_tail_test.Rds")


rejected <- lapply(res, function(x) {x[["rejected"]]})
rejected <- do.call("c", rejected)
mean(rejected) # 52% have non weibull tails

min_length <- 50
rejected50 <- list()
j <- 1
for (i in seq_along(rain)) {
  x <- rain[[i]]
  if (nrow(x$max_data) > min_length) {
    rejected50[[j]] <- tibble(meta_idx = i,
                              rejected = rejected[i], 
                              lon = x$meta$lon,
                              lat = x$meta$lat,
                              alt = x$meta$alt)
    j <- j + 1
  }
}
rejected50 <- do.call(rbind, rejected50)
mean(rejected50$rejected)  # 52% of series with > 50 years exhibit non weibull tails

rejected50 |> 
  ggplot(aes(lon, lat)) + geom_point(aes(color = rejected))

rejected50 |> 
  ggplot(aes(meta_idx, alt)) + geom_point(aes(color = rejected))


# rate of optimal thresholds
opt_threshs <- lapply(res, function(x) {
  i_thr <- which(x$is_rejected) + 1
  if (length(i_thr) == 0) {
    i_thr <- 1
  }
  if (any(i_thr > length(thresholds))) { # there is no tail with weibull samples0 
    #warning("the assumption of Weibull tail is rejected")
    i_thr <- NA
    optimal_thr <- 0.95
    rejected <- TRUE
  } else {
    optimal_thr = thresholds[last(i_thr)]
    rejected <- FALSE
  }
  optimal_thr
})
opt_threshs <- do.call("c", opt_threshs)





