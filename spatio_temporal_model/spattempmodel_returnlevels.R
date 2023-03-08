# %%
library("dplyr")
library("pbapply")
# %%

# %%
NR_CORES_LIST_PARALLELIZATION <- 16

# Welche Jaehrlichkeit soll berechnet werden?
returnyear <- 50
returnquantil <- 1 - 1 / returnyear

# Monthly or Yearly return levels
MONTHLY_RETURN_LEVELS = TRUE
# %%

# %%
## Generische Funktionen fuer TMEV
ptmev <- function(x, data) {
  data$ret1 <- (1 - exp(- (x / data$lambda) ^ (data$alpha)))
  ret <- sum(aggregate(ret1~year, data, prod)[2]) / length(unique(data$year))
  return(ret)
}

qtmev <- function(p, data) {
  if (p == 0) {
    val <- 0
  } else if (p == 1) {
    val <- Inf
  } else {
    min_fun <- function(x) {
      return(ptmev(x, data=data) - p)
    }
    val <- uniroot(min_fun, lower=0, upper=50000, check.conv=TRUE)$root
  }

  return(val)
}
# %%

# %%
## Loading data and model
load(file.path("data", "spat_bamlssnd.Rda"))  # -> bamlssnd
# %%

# %%
llfunqtmev_yearly <- function(data) {
  return(qtmev(returnquantil, data=data))
}
# %%

# %%
llfunqtmev_monthly <- function(data) {
  nr_months = 12
  ret = numeric(nr_months)

  for (m in 1:nr_months) {
    dfMonth <- dplyr::filter(data, month == get("m"))
    ret[m] <- ifelse(nrow(dfMonth) > 0, qtmev(returnquantil, data=dfMonth), -1)
  }

  return(ret)
}
# %%

# %%
returnlevels <- pblapply(bamlssnd, ifelse(MONTHLY_RETURN_LEVELS, llfunqtmev_monthly, llfunqtmev_yearly), cl=NR_CORES_LIST_PARALLELIZATION)
# %%

# %%
postfix = ifelse(MONTHLY_RETURN_LEVELS, "_monthly", "")
save(returnlevels, file=file.path("data", paste0("spat_returnlevels", postfix, ".Rda")))
# %%