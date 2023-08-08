# %%
library("colorspace")
library("comprehenr")
library("foreach")
library("ggplot2")
library("sf")
library("raster")
library("ramify")
# %%

# %%
NR_CORES_LIST_PARALLELIZATION <- 16

PLOT_SCALING = 14

# Monthly or Yearly return levels
MONTHLY_RETURN_LEVELS = TRUE
# %%

# %%
# internal constants
NR_MONTHS <- 12
# %%

# %%
prepareReturnlevelsDataframeForPlotting <- function(returnlevels, lonlat, austriaraw) {
    nd <- data.frame(lon=lonlat$lon, lat=lonlat$lat, rl=(unlist(returnlevels) / 10))

    b <- c(0, 60, 80, 100, 120, 150, 175, 200, 260, 320)

    nd$cat <- cut(nd$rl, breaks=b, labels=b[-1])
    
    ndsf <- st_set_crs(st_as_sf(nd, coords=c("lon", "lat")), 4326)

    return(st_as_sf(intersect(as_Spatial(ndsf), austriaraw)))
}


plot_returnlevels <- function(data, title, austria_data) {
    plt <- ggplot(data) + 
            geom_sf(aes(color=cat), shape=15) +
            scale_color_discrete_diverging(palette="Blue-Red 2", nmax=length(unique(data$cat)), c1=100, l1=30, l2=90, h1=260, h2=0, p1=0.7, p2=0.7) + 
            labs(color = "50-year daily rainfall [mm]", x = "Longitude", y = "Latitude") +
            ggtitle(title) +
            geom_polygon(data=austria_data, aes(x=long, y=lat, group=group), fill=NA, color="black") +
            theme_bw()
    
    return(plt)
}
# %%

# %%
print("Load data frame and grid")
if (MONTHLY_RETURN_LEVELS) {
    load(file=file.path("data", "spat_returnlevels_monthly.Rda"))   # variable name: returnlevels
} else {
    load(file=file.path("data", "spat_returnlevels.Rda"))   # variable name: returnlevels
}

## Erstelle Gitter
load(file.path("data", "SPARTACUS", "Tagesniederschlag", "1960", "RR19600101.rda"))
NonNAindex <- which(!is.na(as.vector(t(prediction))))

load(file.path("data", "SPARTACUS", "spartacus.grid.Rda"))
lonlat <- cov.sparta[NonNAindex, ]
# %%

# %%
print("Prepare map of Austria")
austriaraw <- getData(country = "Austria", level=0)
austria <- fortify(austriaraw)
# %%

# %%
if (MONTHLY_RETURN_LEVELS) {
    print("Reshape data for monthly return levels")

    prepared_returnlevels <- foreach (month=1:NR_MONTHS) %do% {
        to_list(for (onegridpoint in returnlevels) ifelse(length(onegridpoint) == 12, onegridpoint[month], -1))
    }
} else {
    prepared_returnlevels = list(returnlevels)
}
# %%

# %%
ndsfaus <- foreach (idx=1:length(prepared_returnlevels)) %do% {
    prepareReturnlevelsDataframeForPlotting(prepared_returnlevels[[idx]], lonlat, austriaraw)
}
# %%

# %%
print("Plot only data points within Austria")

returnlevelPlots <- foreach (idx=1:length(ndsfaus)) %do% {
                        title = paste0("Spatiotemporal TMEV model", ifelse(MONTHLY_RETURN_LEVELS, paste0(" Month ", idx), ""))
                        plot_returnlevels(ndsfaus[[idx]], title, austria)
}
# %%

# %%
print("Save plots")
foreach (idx=1:length(returnlevelPlots)) %do% {
    fpostfix = ifelse(MONTHLY_RETURN_LEVELS, paste0("_month_", idx), "")
    plot_scaling = 14
    ggsave(file.path("data", "figures", paste0("spatiotempmodel_with_alt", fpostfix, ".pdf")), plot=returnlevelPlots[[idx]], width=PLOT_SCALING, height=PLOT_SCALING)
}
# %%

# %%
if (MONTHLY_RETURN_LEVELS) {
    print("Estimate months with highest return level")
    rlmonths <- matrix(unlist(returnlevels), nrow=length(returnlevels), byrow=TRUE)

    strongestMonths <- argmax(rlmonths, row=TRUE)
}
# %%

# %%
if (MONTHLY_RETURN_LEVELS) {
    print("Plot strongest months")
    nd <- data.frame(lon=lonlat$lon, lat=lonlat$lat, month=factor(unlist(strongestMonths)))

    ndsf <- st_set_crs(st_as_sf(nd, coords=c("lon", "lat")), 4326)

    ndsff <- st_as_sf(intersect(as_Spatial(ndsf), austriaraw))

    plt <- ggplot(ndsff) + 
        geom_sf(aes(color=month), shape=15) +
        scale_color_discrete_qualitative(palette="Dark3", nmax=NR_MONTHS,
                                         labels = month.abb[as.numeric(levels(ndsff$month))]) +
        labs(color = "Month", x = "Longitude", y = "Latitude") +
        ggtitle("Month with highest 50-year daily rainfall return level") +
        geom_polygon(data=austria, aes(x=long, y=lat, group=group), fill=NA, color="black") +
        theme_bw()

    ggsave(file.path("data", "figures", paste0("spatiotempmodel_with_alt_strongest_month.pdf")), plot=plt, width=PLOT_SCALING, height=PLOT_SCALING)
}
# %%