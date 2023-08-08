# %%
library("bamlss")
library("dplyr")
library("lubridate")
library("pbapply")

## Data processing options
NR_CORES = 16
CHUNKS = 100 * NR_CORES

## Which data should be used?
START_YEAR = 1960
END_YEAR = 2013
START_DAY = 1
END_DAY = 365
# %%


# %%
## Create dataframe with all available rain observations
load(file.path(".", "data", "rain_aut.Rda"))

namen <- lapply(seq_len(length(rain_aut)), function(x) rain_aut[[x]][[1]]$name)
altitude <- lapply(seq_len(length(rain_aut)), function(x) rain_aut[[x]][[1]]$alt)
longitude <- lapply(seq_len(length(rain_aut)), function(x) rain_aut[[x]][[1]]$lon)
latitude <- lapply(seq_len(length(rain_aut)), function(x) rain_aut[[x]][[1]]$lat)
years <- unlist(lapply(seq_len(length(rain_aut)), function(x) rain_aut[[x]][[2]]$year))
month <- unlist(lapply(seq_len(length(rain_aut)), function(x) rain_aut[[x]][[2]]$month))
days <- unlist(lapply(seq_len(length(rain_aut)), function(x) rain_aut[[x]][[2]]$day))

precipt <- lapply(seq_len(length(rain_aut)), function(x) rain_aut[[x]][[2]]$val)

df <- data.frame(unlist(namen), unlist(latitude), unlist(longitude), unlist(altitude))

newdf <- data.frame()
for (i in seq_len(nrow(df))){
  meta <- df[i, ]
  rain <- precipt[[i]]
  frame <- data.frame(meta, rain)
  names(frame) <- c("name", "lat", "lon", "alt", "rain")
  newdf <- rbind(newdf, frame)
}
# %%

# %%
finaldf <- cbind(newdf, years, month, days)
finaldf$date <- as.Date(with(finaldf, paste(years, month, days, sep = "-")), "%Y-%m-%d")
finaldf$yday <- yday(finaldf$date)
save(finaldf, file =  file.path(".", "data", "finaldf.Rda"))
# %%

# %%
## Erstelle Gitter
load(file.path("data", "SPARTACUS", "Tagesniederschlag", "1960", "RR19600101.rda"))
NonNAindex <- which(!is.na(as.vector(t(prediction))))
spartacus_indices <- seq_along(NonNAindex)

load(file.path("data", "SPARTACUS", "spartacus.grid.Rda"))
lonlat <- cov.sparta[NonNAindex, ]
# %%

# %%
### Befuelle die Liste mit Regeninformationen
print("Loading precipitation data into individual data frames split by year and day")
collected_datas = vector("list", (END_DAY - START_DAY + 1) * (END_YEAR - START_YEAR + 1))
coll_data_idx = 1

for (j in START_YEAR:END_YEAR) {
  print(j)
  currfolder <- file.path("data", "SPARTACUS", "Tagesniederschlag", j)
  myfiles <- list.files(path=currfolder)

  for (d in START_DAY:END_DAY) {
    load(file.path(currfolder, myfiles[d]))
    unlisted <- as.vector(t(prediction))

    prec_indices_on_grid <- which(unlisted[NonNAindex] > 0)

    if (length(prec_indices_on_grid) > 0) {
      m = month(as.Date(d - 1, origin = paste0(j, "-01-01")))
      collected_datas[[coll_data_idx]] <- data.frame(prec_indices_on_grid, j, d, m)
      coll_data_idx <- coll_data_idx + 1
    }
  }
}
# %%

# %%
print("Merging dataframes")
valid_collected_datas <- collected_datas[!sapply(collected_datas, is.null)]
datas_full <- rbind(data.frame(matrix(NA, ncol = 4, nrow = 0)), do.call(rbind, valid_collected_datas))
colnames(datas_full) <- c("grid_k", "year", "yday", "month")
# %%

# %%
### Add corresponding dataframe subset to spartacus output
print("Splitting dataframes into individual dataframes by gridpoint")
tmpdata_full <- datas_full
splitgroups = split(spartacus_indices, ceiling(seq_along(spartacus_indices) / CHUNKS))
nriter <- 1

spartacus_data_chunks <- vector(mode="list", length=length(splitgroups))

for (splitgroup in splitgroups) {
  print(paste(nriter, "of", length(splitgroups)))
  tmpdata_oneiter <- dplyr::filter(tmpdata_full, grid_k %in% splitgroup)

  spartacus_data_chunks[[nriter]] <- pblapply(splitgroup,
    function(k) {
      ret <- tmpdata_oneiter[tmpdata_oneiter$grid_k == k,][c("year", "yday", "month")]
      return(ret)
    },
    cl=NR_CORES
  )

  tmpdata_full <- dplyr::filter(tmpdata_full, !grid_k %in% splitgroup)
  nriter <- nriter + 1
  gc()
}
# %%

#%%
print("Finalize spartacus data")
spartacus <- vector(mode="list", length=length(spartacus_indices))

spartacus_data_chunks_ul <- unlist(spartacus_data_chunks, recursive=FALSE)

for (k in spartacus_indices) {
  print(paste(k, "of", length(spartacus_indices)))
  spartacus[[k]]$meta$lon <- lonlat$lon[k]
  spartacus[[k]]$meta$lat <- lonlat$lat[k]
  spartacus[[k]]$meta$alt <- lonlat$alt[k]
  spartacus[[k]]$data <- spartacus_data_chunks_ul[[k]]
}
#%%

# %%
print("Save file")
save(spartacus, file = file.path(".", "data", "spat_spartacus_data.Rda"))
# %%
