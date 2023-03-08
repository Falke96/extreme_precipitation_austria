# %%
library("bamlss")
library("dplyr")
library("pbapply")
# %%

# %%
NR_CORES_PRED <- 2
NR_CORES_LIST_PARALLELIZATION <- 8
# %%

# %%
## Loading data and model
load(file.path(".", "data", "spat_spartacus_data.Rda"))  # -> spartacus
load(file.path(".", "data", "spat_temp_model.Rda"))      # -> spattemp
# %%

# %%
###Vorhersagen
lfun <- function(x) {
  nd <- x$data
  nd$lon <- x$meta$lon
  nd$lat <- x$meta$lat
  nd$alt <- x$meta$alt
  
  nd$lambda <- predict(spattemp, newdata=nd, cores=NR_CORES_PRED,
                      model="lambda", intercept=TRUE, type="parameter")
  nd$alpha <- predict(spattemp, newdata=nd, cores=NR_CORES_PRED,
                     model="alpha", intercept=TRUE, type="parameter")

  return(nd)
}
# %%

# %%
bamlssnd <- pblapply(spartacus, lfun, cl=NR_CORES_LIST_PARALLELIZATION)
# %%

# %%
save(bamlssnd, file=file.path(".", "data", "spat_bamlssnd.Rda"))
# %%