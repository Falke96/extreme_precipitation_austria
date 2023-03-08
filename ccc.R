#Number of cores for list parallelizations
NR_CORES = 8

#Standard return period
RETURN_PERIOOD = 50

#Model formulas
fseason <- list(
  "lambda" = val ~ ti(yday, bs = "cc", k = 10),
  "alpha" = ~ ti(yday, bs = "cc", k = 10)
)

fconst <- list(
  "lambda" = val ~ 1,
  "alpha" = ~ 1
)