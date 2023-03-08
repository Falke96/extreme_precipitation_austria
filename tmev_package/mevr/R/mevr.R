# package mevr

# generate data for etsting purposes
# 50 years of daily rainfall data
# each year has 365 values
# w <- 0.87
# c <- 75
# l <- list()
# years <- 1950:1970
# for(i in seq_along(years)){
#   y <- years[i]
#   dates <- seq(as.Date(paste0(y, "-01-01")), as.Date(paste0(y, "-12-31")), by = "1 day")
#   vals <- rweibull(length(dates), shape = w, scale = c)
#   l[[i]]<- data.frame(dates, vals)
# }
# data <- do.call(rbind, l)
# 
# # padova
# pado <- get(load("F:/eigene_dateien/ownCloud/diss/projects/tmev/padova/padova_precipitation.Rds"))
# data <- pado %>% 
#   na.omit()

# load("../rain_aut.Rda")
# dailyrainfall <- rain_aut[[1]]$data %>%
#   filter(year < 1991) %>%
#   mutate(dates = ymd(paste(year, month, day, sep = "-"))) %>% 
#   dplyr::select(dates, val)
# save(dailyrainfall, file = "tmev_package/mevr/data/dailyrainfall.RData")


#' @importFrom graphics abline hist legend lines par points title
#' @importFrom stats aggregate cov na.omit quantile runif rweibull pweibull dweibull sd  uniroot
#' @importFrom EnvStats eweibull 
#' @importFrom parallel detectCores makeCluster stopCluster 
#' @importFrom bamlss bamlss weibull_bamlss opt_bfit samplestats results.bamlss.default 
#' @importFrom mgcv s ti
NULL


#' @import dplyr 
#' @import EnvStats 
#' @import rlang
#' @import bamlss
#' @import mgcv
#' @import foreach
#' @import doParallel
NULL


#' Daily rainfall data
#' 
#' A dataset containing daily rainfall 
#' intended to be used with the package \code{mevr}
#' 
#' @docType data
#' @keywords datasets
#' @name dailyrainfall
#' @usage data(dailyrainfall)
#' 
#' @format The dataset contains real world daily rainfall observations from a station 
#' in the northern Alps. The series contains values from 1971 to 1985 and are assumed to be 
#' Weibull distributed. This data series is intended to be used as is as input data 
#' for the package \code{mevr} to fit the metastatistical extreme value 
#' distribution and its variants with different estimation methods.
#' 
#' The dataset is a dataframe with two columns, dates and val:
#'  
#' \describe{
#' \item{dates}{ Days of class \code{Date} in the format YYYY-MM-DD}
#' \item{val}{ Rainfall observations corresponding to the date in the row. 
#' The value is the 24 hour sum from the morning hours of day-1 to the morning hours of day. }
#' }
#' 
#' @examples 
#' ## Load example data
#' data(dailyrainfall)
#' 
#' ## explore dataset
#' head(dailyrainfall)
#' hist(dailyrainfall$val)
#' plot(dailyrainfall$val, type = "o")
NULL



#' Fitting the simplified Metastatistical Extreme Value Distribution (SMEV)
#' 
#' Fit the SMEV distribution to rainfall observations with different estimation methods.
#'
#' The SMEV was introduced by (Marra et al., 2019) as a simplified version of the MEVD 
#' (Marani and Ignaccolo, 2015) with the assumption of a stationary parent Weibull distribution 
#' as 
#' \deqn{F = [1 - exp(-x/C)^w]^n}
#' for \eqn{w > 0} and \eqn{C > 0} being the Weibull shape and scale parameter and 
#' \eqn{n > 0} being the mean number of wet days over all years. 
#' Wet days are defined as rainfall events > threshold. As it was shown by 
#' e.g. Schellander et al., 2019, probability weighted moments should be preferred over 
#' maximum likelihood for the estimation of the Weibull parameters w and C. Therefore 
#' \code{method = 'pwm'} is the default.
#' 
#' Confidence intervals of the SMEV distribution can be calculated using a non parametric 
#' bootstrap technique. Note that this very slow.
#'
#' @param data The data to which the SMEV should be fitted to. \code{data} must be a data.frame with two columns. 
#' The first column must contain dates of class  \code{Date}, the second or last column must contain the rainfall 
#' values corresponding to datums in the rows. No NA values are allowed.
#' @param threshold A numeric that is used to define wet days as values > threshold. 
#' \eqn{data <= threshold} is set to NA.  
#' @param method Character string describing the method that is used to estimate the 
#' Weibull parameters c and w. Possible options are probability weighted moments (\code{method='pwm'}), 
#' maximum likelihood (\code{method='mle'}) or least squares (\code{method='ls'}). 
#' The \code{default} is \code{pwm}. (see details).
#' @param sd If \code{sd=TRUE}, confidence intervals of the SMEV distribution are calculated (see details). 
#' @param sd.method Currently only a non parametric bootstrap technique can be used to calculate SMEV confidence intervals with \code{sd.method='boot'}. The default is \code{sd=FALSE}.
#' @param R The number of samples drawn from the SMEV distribution to calculate the confidence intervals with \code{sd.method='boot'}
#'
#' @return A list of class \code{mevr} with components:
#' \item{c}{ Single value of the Weibull scale parameter of the SMEV.}
#' \item{w}{ Single value of the Weibull shape parameter of the SMEV.}
#' \item{n}{ Mean number of wet events, averaged over all years. Wet events are defined as rainfall > \code{threshold}.}
#' \item{params}{ A named vector of the fitted parameters. }
#' \item{maxima}{ Maximum values corresponding to each year.}
#' \item{std}{ Standard error of fitted parameters (if \code{sd=TRUE}).}
#' \item{varcov}{ Covariance matrix of fitted parameters (if \code{sd=TRUE}).}
#' \item{data}{ \eqn{data >= threshold} used to fit the SMEV and additional components which may be useful for further analysis.}
#' \item{years}{ Vector of years as YYYY.}
#' \item{threshold}{ The chosen threshold.}
#' \item{method}{ Method used to fit the MEVD.}
#' \item{type}{ The type of distribution ("SMEV")}
#' 
#' @export
#'
#' @references 	Marra, F. et al. (2019) 'A simplified MEV formulation to model extremes emerging from multiple nonstationary underlying processes', Advances in Water Resources. Elsevier Ltd, 127(April), pp. 280-290. doi: 10.1016/j.advwatres.2019.04.002.
#' @examples
#' data(dailyrainfall)
#' 
#' fit <- fsmev(dailyrainfall)
#' fit
#' plot(fit)
#' 
#' @author Harald Schellander, Alexander Lieb
#' 
#' @seealso \code{\link{fmev}}, \code{\link{ftmev}}
fsmev <- function(data, threshold = 0, method = c("pwm", "mle", "ls"), sd = FALSE, sd.method = "boot", R = 502){
  # data must be data.frame for yearly parameters
  # data must be in last/second column
  # col1 must hold the group variable
  # col2 must contain the data
  # col1 must be kind of date 

  if(!inherits(data, "data.frame"))
    stop("data must be of class 'data.frame'")
    
  method <- match.arg(method)

  colnames(data) <- c("groupvar", "val")
  d <- try(as.Date(data$groupvar, format="%Y"))
  if("try-error" %in% class(d) || any(is.na(d))) {
    stop("date column must be of format %Y-%m-%d")
  }
  
  if(any(data$val < 0))
    stop("data must not contain values < 0")
  if(any(is.na(data$val)))
    stop("data must not contain NA")
  
  # only wet days: remove data smaller than threshold
  data_pot <- data %>%
    dplyr::filter(.data$val > threshold) %>%
    dplyr::mutate(nvar = format(.data$groupvar, "%Y"))
  n_vec <- data_pot %>%
    group_by(.data$nvar) %>%
    count %>%
    ungroup %>% 
    pull(n)
  years <- as.numeric(unique(data_pot$nvar))
  data_pot$nvar <- NULL
  theta <- data_pot %>%
    group_modify(~ fit.mev(.x$val, method)) %>%
    ungroup
  theta$n <- mean(n_vec)
  
  maxima <- data_pot %>%
    dplyr::mutate(year = format(.data$groupvar, "%Y")) %>%
    group_by(.data$year) %>%
    summarise(max = max(.data$val, na.rm = TRUE)) %>%
    pull(max) 
  
  params <- c("c" = theta$c, "w" = theta$w, "n" = theta$n)
  
  if(sd){
    if(sd.method == "boot"){
    err <- smev.boot(data_pot, method = method, R = R)
    res <- list(c = theta$c, w = theta$w, n = theta$n, params = params, maxima = maxima, std = err$std, varcov = err$varcov, data = data_pot, years = years, threshold = threshold, method = method, type = "SMEV")
    }
  } else {
    res <- list(c = theta$c, w = theta$w, n = theta$n, params = params, maxima = maxima, data = data_pot, years = years, threshold = threshold, method = method, type = "SMEV")
    
  }
  class(res) <- "mevr"
  return(res)
}



#' Fitting the Metastatistical Extreme Value Distribution (MEVD)
#' 
#' Fit the MEVD distribution to rainfall observations with different estimation methods.
#'
#' With the aim of weakening the requirement of an asymptotic assumption for the GEV distribution, 
#' a metastatistical approach was proposed by Marani and Ignaccolo (2015). The MEVD is defined 
#' in terms of the distribution of the statistical parameters describing "ordinary" daily rainfall 
#' occurrence and intensity. The MEVD accounts for the random process of event occurrence in each 
#' block and the possibly changing probability distribution of event magnitudes across different blocks, 
#' by recognizing the number of events in each block, n, and the values of the shape and scale 
#' parameters w and C of the parent Weibull distribution to be realisations of stochastic variables. 
#' The MEVD can then be written as
#'
#' \deqn{F = \frac{1}{T} \sum_{j=1}^T \prod_{k \in A_j} \left( 1-e^{-\left(\frac{x}{C(j,k)}\right)^{w(j,k)}} \right)}
#' 
#' for \eqn{w > 0} and \eqn{C > 0}. With T fully recorded years, yearly C and w can be estimated 
#' by fitting a Weibull distribution to the values x of this year, and n is the number of ordinary 
#' events per year, i.e. all rainfall events larger than a threshold. 
#'
#' If the probability distribution of daily rainfall is assumed to be time-invariant, the MEVD can be simplified to 
#'
#' \deqn{F = [1 - exp(-x/C)^w]^n}
#'
#'with single values for the shape and scale parameters w and C. n is then the mean number of wet 
#'days at this location (Marra et al., 2019; Schellander et al., 2019).
#'
#' As is shown e.g. Schellander et al., 2019, probability weighted moments should be preferred 
#' over maximum likelihood for the estimation of the Weibull parameters w and C. 
#' Therefore \code{method = 'pwm'} is the default.
#'
#'The MEVD can also be used for sub-daily precipitation (Marra et al., 2019). 
#'In that case n has to be adapted accordingly to the 'mean number of wet events' per year.
#'
#' @param data The data to which the MEVD should be fitted to. \code{data} must be a data.frame with two columns. 
#' The first column must contain dates of class  \code{Date}, the second or last column must contain the rainfall 
#' values corresponding to datums in the rows. No NA values are allowed.
#' @param threshold A numeric that is used to define wet days as values > threshold. 
#' \eqn{data <= threshold} is set to NA.  
#' @param method Character string describing the method that is used to estimate the 
#' Weibull parameters c and w. Possible options are probability weighted moments (\code{method='pwm'}), 
#' maximum likelihood (\code{method='mle'}) or least squares (\code{method='ls'}). 
#' The \code{default} is \code{pwm}. (see details).
#'
#' @return A list of class \code{mevr} with the fitted Weibull parameters and other helpful ingredients.
#' \item{c}{ vector of Weibull scale parameters of the MEVD, each component refers to one year.}
#' \item{w}{ vector of Weibull shape parameters of the MEVD, each component refers to one year.}
#' \item{n}{ Number of wet events per year. Wet events are defined as rainfall > \code{threshold}.}
#' \item{params}{ A named vector of the fitted parameters. }
#' \item{maxima}{ Maximum values corresponding to each year.}
#' \item{data}{ \eqn{data >= threshold} used to fit the MEVD and additional components which may be useful for further analysis.}
#' \item{years}{ Vector of years as YYYY.}
#' \item{threshold}{ The chosen threshold.}
#' \item{method}{ Method used to fit the MEVD.}
#' \item{type}{ The type of distribution ("MEVD")}
#' 
#' @export
#' 
#' @references Marani, M. and Ignaccolo, M. (2015) 'A metastatistical approach to rainfall extremes', Advances in Water Resources. Elsevier Ltd, 79(Supplement C), pp. 121-126. doi: 10.1016/j.advwatres.2015.03.001.
#' @references Schellander, H., Lieb, A. and Hell, T. (2019) 'Error Structure of Metastatistical and Generalized Extreme Value Distributions for Modeling Extreme Rainfall in Austria', Earth and Space Science, 6, pp. 1616-1632. doi: 10.1029/2019ea000557.
#'
#' @examples
#' data(dailyrainfall)
#' fit <- fmev(dailyrainfall, method = "mle")
#' fit
#' plot(fit)
#' 
#' @author Harald Schellander, Alexander Lieb
#' 
#' @seealso \code{\link{fsmev}}, \code{\link{ftmev}}
fmev <- function(data, threshold = 0, method = c("pwm", "mle", "ls")){
  # data must be data.frame for yearly parameters
  # data must be in last/second column
  # col1 must hold the group variable
  # col2 must contain the data
  # # col1 must be kind of date 
  
  if(!inherits(data, "data.frame"))
    stop("data must be of class 'data.frame'")
  
  method <- match.arg(method)
  # if(method != "pwm" & threshold > 0)
  #   stop("Threshold can only be used for pwm")
  
  colnames(data) <- c("groupvar", "val")
  d <- try(as.Date(data$groupvar, format = "%Y"))
  if("try-error" %in% class(d) || any(is.na(d))) {
    stop("date column must be of format %Y-%m-%d")
  }
  
  if(any(data$val < 0))
    stop("data must not contain values < 0")
  if(any(is.na(data$val)))
    stop("data must not contain NA")
  
  # only wet days: remove data smaller than threshold
  data_pot <- data %>%
    dplyr::filter(.data$val > threshold) %>%
    dplyr::mutate(year = format(.data$groupvar, "%Y"))
  n_vec <- data_pot %>%
    group_by(.data$year) %>%
    count %>%
    ungroup %>% 
    pull(n)
  theta <- data_pot %>%
    group_by(.data$year) %>%
    group_modify(~ fit.mev(.x$val, method)) %>%
    ungroup
  theta$n <- n_vec
  
  maxima <- data_pot %>%
    dplyr::mutate(year = format(.data$groupvar, "%Y")) %>%
    group_by(.data$year) %>%
    summarise(max = max(.data$val, na.rm = TRUE)) %>%
    pull(max) 

  params <- data.frame("c" = theta$c, "w" = theta$w, "n" = theta$n)
  years <- as.numeric(unique(data_pot$year))
  
  res <- list(c = theta$c, w = theta$w, n = theta$n, params = params, maxima = maxima, data = data_pot, years = years, threshold = threshold, method = method, type = "MEVD")
  class(res) <- "mevr"
  return(res)
}



#' Fitting the temporal Metastatistical Extreme Value Distribution (TMEV)
#' 
#' Fit the temporal MEVD distribution TMEV to rainfall observations with a cyclic spline to account for seasonality.
#'
#' With the aim of exploiting the full temporal information for parameter estimation, 
#' Falkensteiner et al., (2023) introduced the TMEV, which is an explicitly 
#' non-stationary formulation of the MEVD (Marani and Ignaccolo, 2015). Adopting a 
#' Weibull distribution for ordinary rainfall events, the assumption of yearly 
#' constant coefficients is relaxed by allowing the Weibull
#' parameters to fluctuate with time. The TMEV can then be written as  
#' 
#'  \deqn{F = \frac{1}{T} \sum_{j=1}^T \prod_{k \in A_j} \left( 1-e^{-\left(\frac{x}{C(j,k)}\right)^{w(j,k)}} \right)}
#'
#' with \eqn{w > 0} and \eqn{C > 0} being the Weibull shape and scale parameters, and 
#' $A_j \\subseteq (1, ..., 366)$ being the wet days in year j. The temporal and 
#' the superimposed seasonal dependence on w and c is modeled with a cyclic seasonal 
#' effect on the day of the year. 
#' 
#' Technically this is accomplished by fitting a cyclic spline to the daily rainfall 
#' values. The following formula is used for the fitting procedure of both the Weibull scale and 
#' shape parameter with the function \code{\link[bamlss]{bamlss}} from package \code{bamlss}: 
#' 
#' \deqn{parameter = x \sim s(year) + ti(yday, bs = "cc", k = 10)}
#' 
#' The first effect models the long-term temporal trend of the parameter with a thin-plate spline. 
#' The second effect models the superimposed seasonal fluctuations of the parameter 
#' with the 'day of the year' with a cyclic cubic regression spline and 10 knots, 
#' to ensure a smooth transition between December and January. 
#' 
#' For data series with lengths < 10 years, the first temporal effect is changed to 
#' a simple linear time trend. 
#' 
#' For trend analysis, an additional interaction term can be added to the model formula. 
#' The following term models the relationship between the seasonality as day of the year 
#' and the year itself with a combination of a thin plate and a cyclic cubic spline:
#' 
#'  \deqn{ti(year, yday, bs = c("tp", "cc"), d = c(1, 1), k = c(10, 10))}
#'
#' @param data The data to which the TMEV should be fitted to. \code{data} must be a data.frame with two columns. 
#' The first column must contain dates of class  \code{Date}, the second or last column must contain the rainfall 
#' values corresponding to datums in the rows. No NA values are allowed.
#' @param threshold A numeric that is used to define wet days as values > threshold. 
#' \eqn{data <= threshold} is set to NA.  
#' @param minyears Minimum number of available years for fitting a cyclic spline to the non-stationary data series (see details).
#' @param day_year_interaction Logical. Should an additional year vs day of the year 
#' interaction be used for the calculation of the temporal trend in seasonality?  (see details). Default is \code{FALSE}. 
#' @param verbose Logical. If \code{TRUE}, verbose output of the temporal fitting process is shown during runtime.
#'
#' @return A list of class \code{mevr} with components:
#' \item{c}{ Vector of Weibull scale parameters of the TMEV, each row refers to one event, which is a day for daiyl rainfall.}
#' \item{w}{ Vector of Weibull shape parameters of the TMEV, each row refers to one event, which is a day for daiyl rainfall.}
#' \item{n}{ Number of wet events per year. Wet events are defined as rainfall > \code{threshold}.}
#' \item{maxima}{ Maximum values corresponding to each year.}
#' \item{data}{ A data frame with the data used to fit the TMEV and the fitted Weibull parameters \code{c} and \code{w}. }
#' \item{years}{ Vector of years as YYYY.}
#' \item{threshold}{ The chosen threshold.}
#' \item{x}{ The fitted \code{bamlss} object.}
#' \item{type}{ The type of distribution ("TMEV").}
#' 
#' @export
#' @references Marani, M. and Ignaccolo, M. (2015) 'A metastatistical approach to rainfall extremes', Advances in Water Resources. Elsevier Ltd, 79(Supplement C), pp. 121-126. doi: 10.1016/j.advwatres.2015.03.001.
#' @references Falkensteiner, M., Schellander, H., Hell, T. (2023) 'A non-stationary approach to the Metastatistical Extreme Value Distribution', (to appear).
#'
#' @examples 
#' data(dailyrainfall)
#' fit <- ftmev(dailyrainfall)
#' plot(fit, type = "rl")
#' 
#' # temporal trend of the Weibull parameters 
#' pred <- predict(fit)
#' pred_year <- predict(fit, term = "year")
#' boxplot(c.pred ~ year, data = pred)
#' with(pred_year, lines(year - 1970, c.pred.year, type = "b", pch = 20, col = "red"))
#' 
#' @author Marc-Andre Falkensteiner, Harald Schellander
#' 
#' @seealso \code{\link{fmev}}, \code{\link{fsmev}}
ftmev <- function(data, threshold = 0, minyears = 10, day_year_interaction = FALSE, verbose = FALSE){
  
  if(!inherits(data, "data.frame"))
    stop("data must be of class 'data.frame'")
  
  colnames(data) <- c("groupvar", "val")
  if(any(data$val < 0))
    stop("data must not contain values < 0")
  if(any(is.na(data$val)))
    stop("data must not contain NA")
  
  # only wet days: remove data smaller than threshold
  data_pot <- data %>%
    dplyr::filter(.data$val > threshold) %>%
    dplyr::mutate(year = as.numeric(format(.data$groupvar, "%Y")),
                  yday = as.POSIXlt(.data$groupvar)$yday + 1)
  n_vec <- data_pot %>%
    group_by(.data$year) %>%
    count %>%
    ungroup %>% 
    pull(n)
  nyears <- length(n_vec) 
  if (nyears < minyears){
    stop(paste0("data must have at least ", minyears, " years, but has only ", nyears))
  }
  
  if(nyears < 10){
    fy <- list("lambda" = val ~ year + ti(yday, bs = "cc", k = 10),
               "alpha" = ~ year + ti(yday, bs = "cc", k = 10))
  } else {
    if (isTRUE(day_year_interaction)) {
      fy <- list(
        "lambda" = val ~ s(year) + 
          ti(yday, bs = "cc", k = 10) +
          ti(year, yday, bs = c("tp", "cc"), d = c(1, 1), k = c(10, 10)),
        "alpha" = ~  s(year) + 
          ti(yday, bs = "cc", k = 10) +
          ti(year, yday, bs = c("tp", "cc"), d = c(1, 1), k = c(10, 10))
      )
      
      # fy <- list(
      #   "lambda" = val ~ s(year) + 
      #     ti(ymonth, bs = "cc", k = 10) +
      #     ti(year, ymonth, bs = c("tp", "cc"), d = c(1, 1), k = c(10, 10)),
      #   "alpha" = ~  s(year) + 
      #     ti(ymonth, bs = "cc", k = 10) +
      #     ti(year, ymonth, bs = c("tp", "cc"), d = c(1, 1), k = c(10, 10))
      # )
      
    } else if (isFALSE(day_year_interaction)){
      fy <- list(
        "lambda" = val ~ s(year) + ti(yday, bs = "cc", k = 10),
        "alpha" = ~ s(year) + ti(yday, bs = "cc", k = 10)
      )  
    } else {
      stop("Only logical values are allowed for day_year_interaction") 
    }
  }
  bamy <- bamlss(formula = fy, 
                 data = data_pot, 
                 family = weibull_bamlss(),
                 optimizer = opt_bfit,
                 samplestats = samplestats,
                 results = results.bamlss.default,
                 sampler = FALSE, 
                 x = FALSE,
                 verbose = verbose)
  data_pot$c <- exp(bamy$fitted.values$lambda)
  data_pot$w <- exp(bamy$fitted.values$alpha)
  
  maxima <- data_pot %>%
    dplyr::mutate(year = format(.data$groupvar, "%Y")) %>%
    group_by(.data$year) %>%
    summarise(max = max(.data$val, na.rm = TRUE)) %>%
    pull(max) 
  
  years <- unique(data_pot$year)
  
  res <- list(c = data_pot$c, w = data_pot$w, n = n_vec, maxima = maxima, 
              data = data_pot, years = years, threshold = threshold, 
              x = bamy, type = "TMEV")
  
  class(res) <-"mevr"
  return(res)
}


fit.mev <- function(data, method){
  if(method == "pwm"){
    data <- sort(data)
    
    M0hat <- mean(data)
    M1hat <- 0
    N <- length(data) # sample size
    for (i in 1:N){
      M1hat <- M1hat + data[i] * (N - i)
    }
    M1hat = M1hat / (N * (N - 1))
    c = M0hat / gamma(log(M0hat / M1hat) / log(2)) 
    w = log(2) / log(M0hat / (2 * M1hat)) 
    
  } else if (method == "mle"){
    # data above threshold
    x <- data
    n <- length(x)
    
    # derivative of the log likelihood function with respect to w
    # like <- function(w) n * (1 / w - sum((x^w) * log(x)) / sum(x^w)) + sum(log(x))
    # w <- uniroot(like, lower = 0, upper = 100)$root
    # c <- (sum(x^w_hat) / n )^(1.0 / w_hat)
    
    est <- EnvStats::eweibull(x, method = "mle")
    w <- as.double(est$parameters[1])
    c <- as.double(est$parameters[2])
    
    
    #if std:
    ##parhat = c(c_hat, w_hat)
    #  varcov = gev.hess(wei_negloglike, parhat, sample)
    #parstd = np.sqrt( np.diag(varcov) )
    #return n,c_hat,w_hat , parstd, varcov
    #else:
    #  return n,c_hat,w_hat

    
  } else if (method == "ls"){
    xi <- sort(data)
    N <- length(xi)
    Fi <- 1:N / (N + 1)
    yr <- log(-log(1 - Fi))
    xr <- log(xi)
    
    xrbar <- mean(xr)
    yrbar <- mean(yr)
    xrstd <- sd(xr)
    yrstd <- sd(yr)
    
    w <- yrstd / xrstd 
    c <- exp(xrbar - 1 / w * yrbar)
  }
         
  return(data.frame(w = w, c = c))
}


smev.boot <- function(data, method = c("pwm", "mle", "ls"), R = 502){
  # orig enrico: 
  # '''non parametric bootstrap technique 
  # for computing confidence interval for a distribution
  # (when I do not know the asymptotic properties of the distr.)
  # return std and optional pdf of fitted parameters  
  # and their covariance matrix varcov
  # fit to a sample of a distribution using the fitting function fitfun
  # with a number of parameters npar 
  # ONLY FOR WEIBULL
  # Ignore the first output parameter - N'''
  method <- match.arg(method)
  weisample <- data$val
  N <- length(weisample)
  theta.hat <- matrix(NA, R, 2)
  colnames(theta.hat) <- c("c", "w")
  for (i in seq_len(R)){
    replaced <- sample(weisample, N, replace = TRUE)
    theta <- fit.mev(replaced, method)
    theta.hat[i, "c"] <- theta$c
    theta.hat[i, "w"] <- theta$w
  }
  std <- apply(theta.hat, 2, sd)
  varcov <- cov(theta.hat)
  return(list(std = std, varcov = varcov))
}
 

#' The Metastatistical Extreme Value Distribution
#' 
#' Density, distribution function, quantile function and random generation for the 
#' MEV distribution with shape parameter 'w', scale parameter 'c'. 
#' Parameter 'n' refers either to the mean number of wet days per year in case 
#' of the simplified MEV, or to the number of wet days for each year.
#' 
#' @aliases dmev pmev qmev rmev
#' 
#' @param x,q  numeric vector or single values of quantiles for \code{dmev} 
#' and \code{pmev}.
#' @param p vector or single value of probabilities for \code{qmev}.
#' @param N Number of observations to sample from the MEVD or SMEV.
#' @param w,c vector or single values of shape and scale parameter of the 
#' MEV distribution. If a vector, w and c must have the same length as n.
#' @param n Either mean number of wet events per year for the SMEV, 
#' or a vector for yearly MEVD calculations, i.e. one value per year (see details). 
#' If a vector, n must have the same length as w and c.
#'
#' @return \code{dmev} gives the density function, \code{pmev} gives the distribution function, 
#' \code{qmev} gives the quantile function and \code{rmev} provides random realizations of 
#' the SMEV and MEVD. 
#' 
#' @export
#'
#' @examples
#' # SMEV
#' dmev(1200:1300, 0.7, 20, 110)
#' pmev(1200:1300, 0.7, 70, 110)
#' qmev(1 - 1 / seq(5,50,5), 0.7, 70, 110)
#' 
#' # MEVD: 50-year event of 10 years Weibull series
#' w <- rnorm(10, 0.8, 0.1) # shape parameter
#' c <- rnorm(10, 200, 30) # scale parameter
#' n <- rnorm(10, 200, 50) # number of wet days
#' qmev(1 - 1 / 50, w, c, n)
#' 
#' # rl-plot
#' rp <- seq(5, 50, 5)
#' rl <- qmev(1 - 1 / rp, w, c, n)
#' pp <- (1:length(rp)) / (length(rp) + 1)
#' plot(pp, rl, type = "o")
dmev <- function(x, w, c, n){
  nyears <- length(n)
  ret <- c()
  for(y in x){
    if(y > 0){
      val <- n * w * (y^(w - 1) / (c^w)) * exp(-y^w / c^w) * (1 - exp(-y^w / c^w))^(n - 1)
      val <- sum(val) / nyears
    }
    else{
      val <- 0
    }
    ret <- c(ret, val)
  }
  return(ret)
}


#' @describeIn dmev distribution quantile function
#' @export
pmev <- function(q, w, c, n){
  nyears <- length(n)
  ret = c()
  for(y in q){
    if(y >= 0){
      val <- sum((1 - exp(-y^w / c^w))^n) / nyears
    } else {
      val <- 0
    }
    ret <- c(ret, val)
  }
  return(ret)
}


#' @describeIn dmev quantile function
#' @export
qmev <- function(p, w, c, n){
  ret <- list()
  # SMEV
  if(length(w) == 1){
    for(i in 1:length(p)){
      if(p[i] == 0){
        val <- -Inf
      }
      else if(p[i] == 1){
        val <- Inf
      } else {
        val <- c * (-log(1 - p[i]^(1 / n)))^(1 / w)
      }
      ret[[i]] <- val
    }
  }
  # MEVD
  else if(length(w) > 1){ 
    for(i in 1:length(p)){
      if(p[i] == 0){
        val <- -Inf
      }
      else if(p[i] == 1){
        val <- Inf
      }
      else{
        min_fun <- function(x){
          return(pmev(x, w, c, n) - p[i])
        }
        val <- uniroot(min_fun,lower = 0, upper = 10^10)$root
      }
      ret[[i]] <- val
    }
  }
  ret <- do.call("c", ret)
  return(ret)
}

#' @describeIn dmev random generation function
#' @export
rmev <- function(N, w, c, n){
  x <- runif(N)
  ret <- qmev(x, w, c, n)
  return(ret)
}

#Return levels MEV
#q: return period(Vector)
rlmev <- function(q, w, c, n){
  if(all(q > 1)){
    ret <- qmev(1 - 1 / q, w, c, n)
    return(ret)
  }
  else{
    stop("return period 'q' has to be greater than 1")
  }
}




#' The non-stationary Metastatistical Extreme Value Distribution
#' 
#' Quantile function for the TMEV distribution with a Weibull parent distribution.
#' 
#' @aliases dtmev ptmev qtmev
#' 
#' @param x,q Numeric vector or single value of probabilities for \code{dtmev}.
#' @param p Numeric vector or single value of probabilities for \code{qtmev}.
#' @param data A data frame with at least columns \code{c}, \code{w} and \code{year}. 
#' Can be taken from the output of the fitted TMEV object, i.e. \code{x$data} (see \code{\link{ftmev}}). 
#'
#' @return \code{dtmev} gives the density function, \code{ptmev} gives the distribution function, 
#' and \code{qtmev} gives the quantile function of the TMEV. 
#' @export
#'
#' @examples
#' data(dailyrainfall)
#' fit <- ftmev(dailyrainfall)
#' rp <- pp.weibull(fit$maxima)
#' rl <- qtmev(1 - 1 / rp, fit$data)
#' plot(rp, sort(fit$maxima), main = "TMEV", ylab = "return level", xlab = "return period (years)")
#' lines(rp, rl, type = "l")
#' 
#' 
dtmev <- function(x, data) {
  #data$px <- pweibull(x, shape = data$w.pred, scale = data$c.pred)
  #data$dx <- dweibull(x, shape = data$w.pred, scale = data$c.pred)
  data$px <- pweibull(x, shape = data$w, scale = data$c)
  data$dx <- dweibull(x, shape = data$w, scale = data$c)
  
  recyear <- unique(data$year)
  nyears <- length(recyear)
  val <- 0
  
  if (x > 0) {
    for (k in seq_along(recyear)) {
      singledata <- subset(data, data$year == recyear[k])
      j <- seq_along(singledata$px)
      u <- sapply(j, function(i) singledata$px[-i])
      val <- val + sum(apply(u, 2, prod) * singledata$dx[j])
    }
  } 
  
  ret <- val / nyears
  return(ret)
}



#' @describeIn dtmev distribution quantile function
#' @export
ptmev <- function(q, data) {
  #Create distribution function value for each single day
  data$x <- 1 - exp(-(q / data$c)^data$w)
  
  #Group by years and calculate the product
  ret <- sum(aggregate(x ~ year, data, prod)[2]) / length(unique(data$year))
  return(ret)
}


#' @describeIn dtmev distribution quantile function
#' @export
qtmev <- function(p, data) {
  ret <- list()
  for(i in 1:length(p)){
    if (p[i] == 0) {
      val <- 0
    }
    else if (p[i] == 1) {
      val <- Inf
    }
    else {
      min_fun <- function(x) {
        return(ptmev(x, data = data) - p[i])
      }
      val <- uniroot(min_fun, lower = 0, upper = 10^10)$root
    }
    ret[[i]] <- val
  }
  ret <- do.call("c", ret)
  return(ret)
}





#' Return Levels for the MEVD/SMEV/TMEV extreme value distributions
#' 
#' Calculate return levels for a MEVD, SMEV or TMEV extreme value distributions 
#' from an object of class \code{mevr}.
#' 
#' Note that bootstraping the confidence intervals is very slow. 
#'
#' @param x An object of class \code{mevr}, either fitted with the MEVD, SMEV or TMEV
#' @param return.periods A vector of return periods in years, excluding 1.
#' @param ci  If \code{ci=TRUE}, confidence intervals are calculated depending on the type of distribution (only for MEVD or SMEV).
#' @param alpha Number between zero and one giving the \code{1 - alpha} confidence level. Defaults to \code{alpha=0.05}. 
#' @param method Character string giving the method for confidence interval calculation. Option \code{method='boot'} employs a 
#' parametric bootstrap that simulates data from the fitted model, and then fits the chosen MEVD type to each simulated data set 
#' to obtain a sample of parameters or return levels (very slow).
#' @param R The number of bootstrap iterations.
#'
#' @return A list with return levels, chosen return periods and, if applicable, 
#' \code{alpha/2} and \code{1 - alpha/2} confidence intervals
#' @export
#'
#' @examples
#' data(dailyrainfall)
#' 
#' fit <- fmev(dailyrainfall)
#' return.levels.mev(fit)
#' plot(fit)
#'  
return.levels.mev <- function(x, return.periods = c(2, 10, 20, 30, 50, 75, 100, 150, 200), ci = FALSE, alpha = 0.05, method = "boot", R = 502){
  if(!inherits(x, "mevr"))
    stop("x must be object of class 'mevr'")
  
  if(any(return.periods <= 1))
    stop("All return periods have to be > 1")
  
  
  if (tolower(x$type) != "tmev") {
    w <- x$w
    c <- x$c
    n <- x$n
    q <- return.periods
    
    rls <- rlmev(q, w, c, n)
    
    if(isTRUE(ci)) {
      if(length(w) == 1){
        cis <- ci.smev(x, method = c("boot"), alpha = alpha, return.periods = return.periods, R = R)  
      } else {
        cis <- ci.mev(x, method = c("boot"), alpha = alpha, return.periods = return.periods, R = R)  
      }
      res <- list(rl = rls, rp = return.periods, ci = cis)
    } else {
      res <- list(rl = rls, rp = return.periods)
    }
  } else {
    rls <- qtmev(1 - 1 / return.periods, x$data)
    if(isTRUE(ci)) {
      cis <- ci.tmev(x, method = c("boot"), alpha = alpha, return.periods = return.periods, R = R)  
      res <- list(rl = rls, rp = return.periods, ci = cis)
    } else {
      res <- list(rl = rls, rp = return.periods)  
    }
      
  }
  return(res)
}


ci.mev <- function(x, method = c("boot"), alpha = 0.05, 
                   return.periods = c(2, 10, 20, 30, 50, 75, 100, 150, 200), R = 502,
                   ncores = 1, subsize = 20){
  
  if(!inherits(x, "mevr"))
    stop("x must be object of class 'mevr'")
  
  #if(x$method == "mle")
  #  stop("ci caculation does not support 'mle' as parameter estimation method")
  
  if(x$type != "MEVD")
    stop("x must be of type MEVD")
  
  # non-parametric bootstrapping
  # for daily values and wet days
  if(method == "boot_old"){
    w <- x$w 
    c <- x$c 
    n <- x$n 
    #l <- nrow(x$data)  #n
    theta.hat <- tibble(shape = w, scale = c, n = n)
    
    years <- x$years
    all_samples <- list()
    for(i in 1:R){
      single_sample <- list()
      for(j in seq_len(length(years))){
        sample_year <- sample.int(n = length(years), size = 1)
        sample_n <- x$n[sample_year]

        sample_dates <- seq(as.Date(paste0(years[sample_year], "-01-01")), as.Date(paste0(years[sample_year], "-12-31")), by = "1 day")

        sample_val <- rep(0, length(sample_dates))
        #sample_year_val <- x$data %>%
        #  dplyr::filter(format(.data$groupvar, "%Y") == years[sample_year]) %>%
        #  pull(.data$val)
        #  sample_val[1:sample_n] <- sample(sample_year_val, size = sample_n, replace = TRUE)
        sample_year_val <- rweibull(n = sample_n, shape = w[sample_year], scale = c[sample_year])
        n_zero <- length(sample_dates) - sample_n
        sample_val <- sample(c(sample_year_val, rep(0, n_zero)), replace = TRUE)
        single_sample[[j]] <- tibble(
          groupvar = sample_dates,
          val = sample_val
        )
      }
      all_samples[[i]] <- do.call("rbind", single_sample)
    }
    
    bfun <- function(z){
      fit <- fmev(z, threshold = x$threshold, method = x$method) 
      return(list(shape = fit$w, scale = fit$c, n = fit$n))
    }
    pars <- lapply(all_samples, bfun)
    
    # compute return levels from R w, c, and n parameters
    th.est <- theta.hat
    theta.hat <- rlmev(q = return.periods, w = th.est$shape, c = th.est$scale, n = th.est$n)
    
    # compute quantiles of simulated return levels
    rlfun <- function(theta, q) rlmev(q = q, w = theta$shape, c = theta$scale, n = theta$n)
    sam <- lapply(pars, rlfun, q = return.periods)
    sam <- do.call("cbind", sam)
    rownames(sam) <- paste0(return.periods, "-year")
    out <- apply(sam, 1, quantile, probs = c(alpha/2,1 - alpha/2))
    out.names <- rownames(out)
    out <- rbind(out[1, ], theta.hat, out[2, ])
    rownames(out) <- c(out.names[1], "Estimate", out.names[2])
    colnames(out) <- rownames(sam)
    out <- t(out)
    
  } else if (method == "boot") {
    if (subsize > length(x$years)) {
      subsize <- floor(length(x$years) * 0.75)
    }
    ncores <- detectCores() - floor(detectCores() / 2)
    runsWindows <- (Sys.info()['sysname'] == "Windows")
    if (runsWindows) {
      cl <- makeCluster(ncores, type = "PSOCK")
      registerDoParallel(cl)
    } else {
      registerDoParallel(ncores)
    }
    
    sam <- foreach(i = 1:R, .combine = cbind, .export = c("fmev", "fit.mev", "qmev", "pmev"), .packages = c("lubridate", "dplyr")) %dopar% {
      sampleyears <- sample(x$years, size = subsize)
      nd <- x$data %>% 
        #filter(year(.data$groupvar) %in% sampleyears) %>% 
        filter(as.numeric(format(.data$groupvar, "%Y")) %in% sampleyears) %>% 
        dplyr::select(.data$groupvar, .data$val)
      fitdf <- fmev(nd)
      qmev(1 - 1 / return.periods, fitdf$w, fitdf$c, fitdf$n)
    }
    
    if (runsWindows) {
      stopCluster(cl)
    }
    
    theta.hat <- return.levels.mev(x, return.periods = return.periods)$rl
    rownames(sam) <- paste0(return.periods, "-year")
    out <- apply(sam, 1, quantile, probs = c(alpha/2,1 - alpha/2))
    out.names <- rownames(out)
    out <- rbind(out[1, ], theta.hat, out[2, ])
    rownames(out) <- c(out.names[1], "Estimate", out.names[2])
    colnames(out) <- rownames(sam)
    out <- t(out)
  } 
  return(out)
}
  

ci.tmev <- function(x, method = c("boot"), alpha = 0.05, 
                    return.periods = c(2, 10, 20, 30, 50, 75, 100, 150, 200), R = 100, 
                    ncores = 1, subsize = 20){
  
  if(!inherits(x, "mevr"))
    stop("x must be object of class 'mevr'")
  
  if(tolower(x$type) != "tmev")
    stop("x must be of type TMEV")
  
  if (method == "boot") {
    if (subsize > length(x$years)) {
      subsize <- floor(length(x$years) * 0.75)
    }
    ncores <- detectCores() - floor(detectCores() / 2)
    runsWindows <- (Sys.info()['sysname'] == "Windows")
    if (runsWindows) {
      cl <- makeCluster(ncores, type = "PSOCK")
      registerDoParallel(cl)
    } else {
      registerDoParallel(ncores)
    }
    
    sam <- foreach(i = 1:R, .combine = cbind, .export = c("ftmev", "qtmev", "ptmev"), .packages = c("lubridate", "dplyr", "bamlss")) %dopar% {
      sampleyears <- sample(x$years, size = subsize)
      nd <- x$data %>% 
        #filter(year(.data$groupvar) %in% sampleyears) %>% 
        filter(as.numeric(format(.data$groupvar, "%Y")) %in% sampleyears) %>% 
        dplyr::select(.data$groupvar, .data$val)
      fitdf <- ftmev(nd)
      qtmev(1 - 1 / return.periods, fitdf$data)
    }
    
    if (runsWindows) {
      stopCluster(cl)
    }
    
    theta.hat <- return.levels.mev(x, return.periods = return.periods)$rl
    rownames(sam) <- paste0(return.periods, "-year")
    out <- apply(sam, 1, quantile, probs = c(alpha/2,1 - alpha/2))
    out.names <- rownames(out)
    out <- rbind(out[1, ], theta.hat, out[2, ])
    rownames(out) <- c(out.names[1], "Estimate", out.names[2])
    colnames(out) <- rownames(sam)
    out <- t(out)
    
  }
  
  return(out)
}

  
ci.smev <- function(x, method = c("boot"), alpha = 0.05, 
                    return.periods = c(2, 10, 20, 30, 50, 75, 100, 150, 200), R = 502,
                    ncores = 1, subsize = 20){
  
  if(!inherits(x, "mevr"))
    stop("x must be object of class 'mevr'")
  
  if(x$type != "SMEV")
    stop("x must be of type SMEV")
  
  # parametric bootstrapping
  # with weibull sampling
  if(method == "boot_old"){
    w <- x$w 
    c <- x$c 
    n <- x$n 
    l <- nrow(x$data)  
    theta.hat <- c(shape = w, scale = c, n = n)
    
    
    # draw R samples of length l with w and C
    Z <- rweibull(n = l * R, shape = w, scale = c) 
    #Z <- rmev(l * R, w, c, n)
    Z <- matrix(Z, l, R)
    
    
    # fit MEVD to simulated samples
    bfun <- function(z, n){
      z <- tibble(groupvar = x$data$groupvar, val = z)
      fit <- fsmev(z, threshold = x$threshold, method = x$method) 
      #return(c(shape = fit$w, scale = fit$c, n = fit$n))
      return(c(shape = fit$w, scale = fit$c, n = fit$n))
    }
    pars <- apply(Z, 2, bfun, n = n)
    shape <- pars["shape",]
    scale <- pars["scale",]
    n <- pars["n",]
    
    
    # compute return levels from R w and C parameters
    th <- rbind(shape, scale, n)
    th.est <- theta.hat
    rlfun <- function(theta, q) rlmev(q = q, w = theta[1], c = theta[2], n = theta[3])
    sam <- apply(th, 2, rlfun, q = return.periods)
    rownames(sam) <- paste0(return.periods, "-year")
    theta.hat <- rlmev(q = return.periods, w = th.est[1], c = th.est[2], n = th.est[3])
    
    # compute quantiles of simulated return levels
    out <- apply(sam, 1, quantile, probs = c(alpha/2, 1 - alpha/2))
    out.names <- rownames(out)
    out <- rbind(out[1, ], theta.hat, out[2, ])
    rownames(out) <- c(out.names[1], "Estimate", out.names[2])
    colnames(out) <- rownames(sam)
    out <- t(out)
  # } else if (method == "delta"){
  #   w.lower <- w - x$std[2]
  #   w.upper <- w + x$std[2]
  #   c.lower <- c - x$std[1]
  #   c.upper <- c + x$std[1]
  #   
  #   q <- rlmev(q = return.periods, w = w, c = c, n = n)
  #   q.upper <- rlmev(q = return.periods, w = w.lower, c = c.lower, n = n)
  #   q.lower <- rlmev(q = return.periods, w = w.upper, c = c.upper, n = n)
  #   out <- cbind(q.lower, q, q.upper)
  #   rownames(out) <- paste0(return.periods, "-year")
  #   colnames(out) <- c("0.05%", "Estimate", "97.5%")
  #   return(out)
  #   # Fi <- 1 - 1/return.periods
  #   # q <- (-log(1 - Fi))^(1 / w) * c
  #   # for(i in 1:length(Fi)){
  #   #   yr <- 1 - Fi[i]
  #   #   DEL <- c((-log(yr))^(1/w),c*(-log(yr))^(1/w)*log(-log(1-Fi[i])))
  #   #   prod1 = DEL %*% x$varcov
  #   #   varz = DEL %*% prod1
  #   #   stdz = sqrt(varz)
  #   #   ql[ii] = q[ii] - 1.96*stdz
  #   #   qu[ii] = q[ii] + 1.96*stdz  
  #   # }
  } else if (method == "boot") {
    if (subsize > length(x$years)) {
      subsize <- floor(length(x$years) * 0.75)
    }
    ncores <- detectCores() - floor(detectCores() / 2)
    runsWindows <- (Sys.info()['sysname'] == "Windows")
    if (runsWindows) {
      cl <- makeCluster(ncores, type = "PSOCK")
      registerDoParallel(cl)
    } else {
      registerDoParallel(ncores)
    }
    
    sam <- foreach(i = 1:R, .combine = cbind, .export = c("fsmev", "fit.mev", "qmev", "pmev"), .packages = c("lubridate", "dplyr")) %dopar% {
      sampleyears <- sample(x$years, size = subsize)
      nd <- x$data %>% 
        #filter(year(.data$groupvar) %in% sampleyears) %>% 
        filter(as.numeric(format(.data$groupvar, "%Y")) %in% sampleyears) %>% 
        dplyr::select(.data$groupvar, .data$val)
      fitdf <- fsmev(nd)
      qmev(1 - 1 / return.periods, fitdf$w, fitdf$c, fitdf$n)
    }
    
    if (runsWindows) {
      stopCluster(cl)
    }
    
    theta.hat <- return.levels.mev(x, return.periods = return.periods)$rl
    rownames(sam) <- paste0(return.periods, "-year")
    out <- apply(sam, 1, quantile, probs = c(alpha/2,1 - alpha/2))
    out.names <- rownames(out)
    out <- rbind(out[1, ], theta.hat, out[2, ])
    rownames(out) <- c(out.names[1], "Estimate", out.names[2])
    colnames(out) <- rownames(sam)
    out <- t(out)
  } 
  
  # if (method == "normal"){
  #   cat("\n", "Using Normal Approximation Method.\n")
  #   z.alpha <- qnorm(alpha/2, lower.tail = FALSE)
  #   cov.theta <- x$varcov
  #   if (is.null(cov.theta)) 
  #     stop("ci: Sorry, unable to calculate the parameter covariance matrix.  Maybe try a different method.")
  #   var.theta <- diag(cov.theta)
  #   if (any(var.theta < 0)) 
  #     stop("ci: negative Std. Err. estimates obtained.  Not trusting any of them.")
  #   ###
  #   #grads <- rlgrad.fevd(x, period = return.periods)
  #   rlgrad <- function(x, period = 100){
  #     p <- c(shape = x$w,scale = x$c)
  #     yp <- -log(1 - 1/period)
  #     res <- cbind(1, (-1/p["shape"]) * (1 - yp^(-p["shape"])), 
  #                  p["scale"] * (p["shape"])^(-2) * 
  #                    (1 - yp^(-p["shape"])) - (p["scale"]/p["shape"]) * 
  #                    yp^(-p["shape"]) * log(yp))
  #   }
  #   grads <- rlgrad(x, period = return.periods)
  #   grads <- t(grads)
  #   lam <- 1
  #   var.theta <- t(grads) %*% cov.theta %*% grads
  #   ###
  #   
  # }
  
  
  return(out)
}





#' Weibull plotting position
#' 
#' Calculates the weibull plotting position for the given maxima
#'
#' @param x Numeric vector of block maxima 
#'
#' @return A numeric vector of Weibull plotting positions corresponding to the given maxima \code{x}
#' @export
#'
#' @examples
#' data(dailyrainfall)
#' fit <- fsmev(dailyrainfall)
#' rp <- pp.weibull(fit$maxima)
#' rl <- return.levels.mev(fit, return.periods = rp)
#' plot(rp, sort(fit$maxima), xlab = "Return period (years)", ylab = "Return level", main = fit$type)
#' lines(rp, rl$rl)
pp.weibull <- function(x){
  x <- na.omit(x)
  n <- length(x)
  Fi <- (1:n) / (n + 1)
  wpp <- 1 / (1 - Fi)
  return(wpp)
}



#' Plot graphs of MEVD, SMEV or TMEV fit
#' 
#' A return level plot, qq-plot, pp-plot nd a histogram wit the fitted density is produced
#'
#' @param x An object of class\code{mevr}, whose \code{type} argument is one of MEVD, SMEV or TMEV
#' @param q vector of return periods, \eqn{q > 1}.
#' @param ci if \code{ci=TRUE}, confidence intervals will be computed.
#' @param type if omitted a panel with a return level plot (\code{type='rl'}, 
#' a density plot (\code{type='hist'}), a qq-plot (\code{type='qq'}) and a 
#' probability plot (\code{tpe='pp'}) are shown. 
#' @param ... Further parameters may also be supplied as arguments. 
#' See e.g. \link[base]{plot}.
#'
#' @method plot mevr
#' @export
#'
#' @examples
#' data(dailyrainfall)
#' 
#' # fit a simplified MEVD
#' fit <- fsmev(dailyrainfall)
#' fit
#' plot(fit)
#' 
#' # fit MEVD
#' fit <- fmev(dailyrainfall, method = "ls")
#' fit
#' plot(fit)
plot.mevr <- function(x, q = c(2, 10, 20, 30, 50, 75, 100, 150, 200),
                      ci = FALSE, 
                      type = c("all", "rl", "qq", "pp", "hist"), ...){
  
  if(!inherits(x, "mevr"))
    stop("x must be object of class 'mevr'")
  
  type <- match.arg(type)
  
  obs.y <- x$maxima
  obs.x <- pp.weibull(obs.y)
  if (tolower(x$type) != "tmev"){
    rls <- rlmev(q, x$w, x$c, x$n)
  } else {
    rls <- qtmev(1 - 1 / q, x$data)
  }
  
  if(type == "all"){
    par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
  }
  
  if (is.element(type, c("all", "rl"))) {
    if(ci == TRUE){
      if(tolower(x$type) == "smev"){
        ci <- ci.smev(x, ...)  
      } else if (tolower(x$type) == "mevd") {
        ci <- ci.mev(x, ...)
      } else if (tolower(x$type) == "tmev") {
        ci <- ci.tmev(x, ...)
      } else {
        stop(paste0("confidence intervals are supported with type", x$type))
      }
      plot(q, rls, type = "l", xlab = "Return Period [years]", ylab = "Return Level", ylim = c(0, max(rls) * 1.2), log = "x")
      points(obs.x, sort(obs.y))
      lines(q, ci[, 1], lty = 2, col = "grey")
      lines(q, ci[, 3], lty = 2, col = "grey")
    } else {
      plot(q, rls, type = "l", xlab = "Return Period [years]", ylab = "Return Level", ylim = c(0, max(rls) * 1.2), log = "x")
      points(obs.x, sort(obs.y))
    }
  }
  
  if (is.element(type, c("all", "hist"))) {
    # density plot
    h <- hist(obs.y, plot = FALSE)
    dens.x <- seq(min(h$breaks), min(max(h$breaks)), length = 100)
    if (tolower(x$type) != "tmev"){
      dens.y <- dmev(dens.x, x$w, x$c, x$n)
    } else {
     dens.y <- c()
     for (i in 1:length(dens.x)) {
       dens.y <- c(dens.y, dtmev(dens.x[i], x$data))
     }
    }
    hist(obs.y, prob = TRUE, main = "Observed yearly maxima", xlab = paste("N =", length(obs.y)))
    lines(dens.x, dens.y, lty = 2, col = "blue", lwd = 1.5)
    legend("topright", legend = c("Empirical", "Modeled"), 
           col = c("black", "blue"), lty = c(1, 2), lwd = c(1, 1.5), bty = "n")
  }
  
  
  if (is.element(type, c("all", "qq"))) {
    # quantile plot
    if (tolower(x$type) != "tmev"){
      q.m <- rlmev(obs.x, x$w, x$c, x$n)
    } else {
      q.m <- qtmev(1 - 1 / obs.x, x$data)
    }
    plot(q.m, sort(obs.y), main = "Quantile plot", xlab = "Model Quantiles", ylab = "Empirical Quantiles")
    abline(c(0,1))
  }
  
  if (is.element(type, c("all", "pp"))) {
    # probability plot
    N <- length(obs.y)
    Fi <- (1:N) / (N + 1)
    if (tolower(x$type) != "tmev"){
      p.m <- pmev(sort(obs.y), x$w, x$c, x$n)
    } else {
      p.m <- c()
      for (i in 1:length(obs.y)){
        p.m <- c(p.m, ptmev(sort(obs.y)[i], x$data))  
      }
    }
    plot(Fi, p.m, main = "Probability plot", xlab = "Model Probabilities", ylab = "Empirical Probabilities", xlim = c(0, 1), ylim = c(0, 1))
    abline(c(0,1))
  }
  
  if(tolower(x$type) != "tmev"){
    title_strg <- paste(x$type, "/", x$method)
  } else {
    title_strg <- paste(x$type)
  }
  title(title_strg, outer = TRUE)
  par(mfrow = c(1, 1))
}


#' TMEV prediction
#' 
#' Takes a \code{mevr} object where the TMEV has been fitted to rainfall data and calculates 
#' \code{bamlss} predictions for the distributional parameters and the model terms. Basically 
#' a wrapper to the corresponding function \code{predict.bamlss}
#' 
#' See also the details of \code{\link{ftmev}} for an explanation of the model terms used to fit the temporal trend 
#' of the Weibull parameters.
#' 
#' @param object Object of class \code{mevr}, fitted with the TMEV. 
#' @param newdata A data frame with the model covariates (year, yday) at which predictions are required. 
#' Note that depending on argument term, only covariates that are needed by the corresponding model terms need to be supplied.
#' If not supplied, predictions are made on the data supplied by the fitted object \code{x}.
#' @param term Character of the model terms for which predictions shall be calculated. 
#' Can only be \code{"year"} or \code{"yday"}. If not specified, predictions for all terms are calculated.
#' @param ... Arguments passed to prediction functions that are part of a bamlss.family object, i.e., the objects has a $predict() function that should be used instead.
#' 
#' @return A data.frame with the supplied covariables and the predicted parameters.
#' @export 
#'
#' @examples 
#' data(dailyrainfall)
#' 
#' # restrict for the sake of speed
#' idx <- which(as.POSIXlt(dailyrainfall$date)$year + 1900 < 1976)
#' data <- dailyrainfall[idx, ]
#' 
#' f <- ftmev(data, minyears = 5)
#' predict(f, term = "year")
#' 
#' @seealso \code{\link{ftmev}}, \code{\link{predict.bamlss}}
predict.mevr <- function(object, newdata, term, ...){
  
  if(!inherits(object, "mevr"))
    stop("object must be object of class 'mevr'")
  
  if(tolower(object$type) != "tmev")
    stop("object must be of type 'tmev'")
  
  if (missing(newdata)) 
    newdata <- object$data %>% 
      dplyr::select(.data$year, .data$yday)
  
  if(missing(term)){
    term <- "all"
  } else {
    term <- match.arg(term, choices = c("year", "yday")) 
  }

  bamfit <- object$x
  
  if(term == "year"){
    param_pred <- list()
    years <- unique(newdata$year)
    for(i in seq_along(years)){
      y <- years[i]
      d <- newdata %>% 
        filter(.data$year == y)
      c.pred.year <- predict(bamfit, newdata = d, model = "lambda", type = "parameter", term = "year")
      w.pred.year <- predict(bamfit, newdata = d, model = "alpha", type = "parameter", term = "year")
      param_pred[[i]] <- cbind(year = y, c.pred.year, w.pred.year)
    }
    param_pred <- do.call(rbind, param_pred) %>% 
      unique() %>% 
      as.data.frame()
  } else if(term == "yday"){
    param_pred <- list()
    ydays <- unique(newdata$yday)
    for(i in seq_along(ydays)){
      dy <- ydays[i]
      d <- newdata %>% 
        filter(.data$yday == dy)
      c.pred.yday <- predict(bamfit, newdata = d, model = "lambda", type = "parameter", term = "yday")
      w.pred.yday <- predict(bamfit, newdata = d, model = "alpha", type = "parameter", term = "yday")
      param_pred[[i]] <- cbind(yday = dy, c.pred.yday, w.pred.yday)
    }
    param_pred <- do.call(rbind, param_pred) %>% 
      unique() %>% 
      as.data.frame()
  } else if(term == "all"){
      c.pred <- predict(bamfit, newdata = newdata, model = "lambda", type = "parameter")
      w.pred <- predict(bamfit, newdata = newdata, model = "alpha", type = "parameter")
      param_pred <- cbind(newdata, c.pred, w.pred)    
  }
  
  return(param_pred)
}



#' Print method for object of class mevr
#'
#' Print nicely formatted output of the fit to the MEVD and its variants
#'
#' @param x Object of class \code{mevr}, fitted with MEVD, SMEV or TMEV.
#' @param digits Number of digits.
#' @param ... Additional parameters.
#'
#' @return A nicely formatted output of the fitting results.
#'
#' @export
#'
#' @examples
#' data(dailyrainfall)
#'
#' # fit a simplified MEVD
#' fit <- fsmev(dailyrainfall)
#' print(fit)
print.mevr <- function(x, digits = max(3, getOption("digits") - 3), ...){
  if(!inherits(x, "mevr"))
    stop("x must be object of class 'mevr'")
  
  cat("MEVD fitting\n\n")
  cat(paste0("Type: ", x$type,"\n"))
  if(tolower(x$type) != "tmev"){
    cat(paste0("Estimator: ",x$method,"\n"))
  }

  cat("\nParameters:")
  cat("\nScale C:\n")
  scale <- x$c
  print.default(format(scale, digits = digits), print.gap = 2, quote = FALSE)
  cat("\nShape w:\n")
  shape <- x$w
  print.default(format(shape, digits = digits), print.gap = 2, quote = FALSE)

  if(length(x$w) == 1){
    cat("\nMean number of wet days n:\n")
  } else {
    cat("\nWet days n:\n")
  }
  n <- x$n
  print.default(format(n, digits = digits), print.gap = 2, quote = FALSE)

  if(length(x$w) > 1){
    cat("\nYears:\n")
    nyears <- length(n)
    print.default(format(nyears, digits = digits), print.gap = 2, quote = FALSE)
  }

  cat("\nThreshold:\n")
  t <- x$threshold
  print.default(format(t, digits = digits), print.gap = 2, quote = FALSE)

  invisible(x)
}



# wei_negloglike <- function(parhat, data){
#   # compute Weibull neg log likelihood function
#   # for a given sample xi and estimated parameters C, w
#   C = parhat[1]
#   w = parhat[2]
#   xi = data[data> 0]
#   N = length(xi)
#   - N * log(w / C) - (w - 1) * sum(log(xi / C)) + sum((xi / C)^w)
# }
# 
# 
# hess <- function(fun, y, data){
#   # numeric Hessian matrix
#   # for estimating MLE parameters confidence intervals
#   ep = 0.0001
#   #x = np.array(y)
#   x <- y
#   eps = ep * x
#   n = np.size(x)
#   m = np.zeros((n,n))
#   x1 = np.zeros(n)
#   x2 = np.zeros(n)
#   x3 = np.zeros(n)
#   x4 = np.zeros(n)
#   for i in range(n):
#     for j in range(n):
#     x1[:] = x[:]
#   # I modify the original array as well - it is a view!
#   x1[i] = x1[i] + eps[i]
#   x1[j] = x1[j] + eps[j]
#   x2[:] = x[:]
#   x2[i] = x2[i] + eps[i]
#   x2[j] = x2[j] - eps[j]
#   x3[:] = x[:]
#   x3[i] = x3[i] - eps[i]
#   x3[j] = x3[j] + eps[j]
#   x4[:] = x[:]
#   x4[i] = x4[i] - eps[i]
#   x4[j] = x4[j] - eps[j]
#   m[i,j] = (fun(x1, data) -fun(x2, data) - fun(x3, data) + fun(x4, data))/(4*eps[i]*eps[j]) 
#   M = np.asmatrix(m)
#   return inv(M)
# }

