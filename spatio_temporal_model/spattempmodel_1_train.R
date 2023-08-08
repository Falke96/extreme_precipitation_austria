# %%
library("bamlss")
library("dplyr")
# %%

# %%
#Daten zum Trainieren
load(file.path(".", "data", "finaldf.Rda"))
# %%

# %%
DOWN_SAMPLE <- FALSE
DOWN_SAMPLE_SIZE <- 1000000
START_YEAR <- 1960
END_YEAR <- 2013

NR_CORES_BAMLSS <- 16
# %%

# %%
train_df <- subset(finaldf, (years >= START_YEAR) & (years <= END_YEAR))
train_df <- rename(train_df, year=years)

if (DOWN_SAMPLE) {
  train_ind <- sample(seq_len(nrow(train_df)), size=DOWN_SAMPLE_SIZE)
  train_df <- train_df[train_ind, ]
}
# %%

# %%
#Erstelle raumlich-zeitliches Modell
f <- list(
  "lambda" = rain ~ s(year) + ti(yday, bs="cc", k=10) + ti(lon, lat, bs="tp", d=2, k=30) +
    ti(yday, lon, lat, bs=c("cc", "tp"), d=c(1, 2), k=c(8, 30)) +
    ti(year, lon, lat, bs=c("ps", "tp"), d=c(1, 2), k=c(8, 30)) + s(alt),
  "alpha" = ~ s(year) + ti(yday, bs="cc", k=10) + ti(lon, lat, bs="tp", d=2, k=30) +
    ti(yday, lon, lat, bs=c("cc", "tp"), d = c(1, 2), k = c(8, 30)) +
    ti(year, lon, lat, bs=c("ps", "tp"), d = c(1, 2), k = c(8, 30)) + s(alt)
)

spattemp <- bamlss(formula = f, data = train_df, eps = 0.001, cores = NR_CORES_BAMLSS,
                  family = weibull_bamlss(), binning = TRUE, sampler = FALSE)
# %%

# %%
save(spattemp, file = file.path(".", "data", "spat_temp_model.Rda"))
# %%