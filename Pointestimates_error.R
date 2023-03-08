library("this.path")
library("dplyr")
library("bamlss")
library("pbapply")
library("reshape2")
library("akima")
library("RColorBrewer")
library("ggplot2")

path <- this.dir()
source(file.path(path,"tmev_package", "mevr", "R", "mevr.R"))
source(file.path(path, "ccc.R"))

load(file.path(path, "data", "rain_aut.Rda"))

FITLEN <- seq(4, 50, by = 2)
RETLEV <- seq(5, 100, by = 5)

get_rl_samples <- function(df, t, minyears = 1000) {
  rldf <- data.frame(matrix(NA, nrow = length(df), ncol = 6))
  q <- 1 - 1 / t
  for (i in seq_len(length(df))) {
    if (length(df[[i]][[3]]$year) < minyears) {
      next
    }
    rldf[i, 1] <- df[[i]][[1]]$name
    rldf[i, 2] <- i
    rldf[i, 3] <- df[[i]][[1]]$mrisc_stnr
    rldf[i, 4] <- df[[i]][[1]]$lon
    rldf[i, 5] <- df[[i]][[1]]$lat
    rldf[i, 6] <- quantile(df[[i]][[4]]$max, probs = q)
    sorted <- sort(df[[i]][[4]]$max)
    quants <- (length(sorted) + 1) / (length(sorted) + 1 - seq_len(length(sorted)))
    rldf[i, 7] <- spline(x = quants, y = sorted, method = "hyman", xout = t)$y
  }
  names(rldf) <- c("name", "index", "mrisc_stnr", "lon", "lat", "quantrl", "weibullrl")
  rldf <- na.omit(rldf)
  return(rldf)
}

rlemp <- data.frame(matrix(NA, nrow = 55, ncol = 3 + length(RETLEV)))
rlemp[, 1] <-  get_rl_samples(rain_aut, t = 50, minyears = 100)$name
rlemp[, 2] <-  get_rl_samples(rain_aut, t = 50, minyears = 100)$index
rlemp[, 3] <-  get_rl_samples(rain_aut, t = 50, minyears = 100)$mrisc_stnr
k <- 4
for (i in RETLEV) {
  rlemp[, k] <- get_rl_samples(rain_aut, t = i, minyears = 100)$weibullrl
  k <- k + 1
}
colnames(rlemp) <- c("name", "index", "mrisc_stnr", RETLEV)

sub_stations = rain_aut[rlemp$index]

##########Final##########

tmev_rlcalc = function(df, sample.len) {
  subyears <- subset(fityears, mrisc_stnr == df$meta$mrisc_stnr)
  sampleyears <- subyears[1, 4:(sample.len + 3)]
  nd = df$data
  nd$dates = nd$dates <- as.Date(with(nd, paste(year, month, day, sep = "-")), "%Y-%m-%d")
  
  sub_data = nd %>%
    dplyr::filter(year %in% sampleyears) %>%
    dplyr::select(dates, val)
  
  ft = ftmev(sub_data, minyears = 4)
  ret <- return.levels.mev(ft, return.periods = RETLEV)$rl
  return(ret)
}

smev_rlcalc = function(df, sample.len) {
  subyears <- subset(fityears, mrisc_stnr == df$meta$mrisc_stnr)
  sampleyears <- subyears[1, 4:(sample.len + 3)]
  nd = df$data
  nd$dates = nd$dates <- as.Date(with(nd, paste(year, month, day, sep = "-")), "%Y-%m-%d")
  
  sub_data = nd %>%
    dplyr::filter(year %in% sampleyears) %>%
    dplyr::select(dates, val)
  
  fs = fsmev(sub_data)
  ret <- return.levels.mev(fs, return.periods = RETLEV)$rl
  return(ret)
}


####
RMSSMEV <- data.frame(matrix(0, nrow = length(FITLEN), ncol = length(RETLEV)))
MRESMEV <- data.frame(matrix(0, nrow = length(FITLEN), ncol = length(RETLEV)))

RMSTMEV <- data.frame(matrix(0, nrow = length(FITLEN), ncol = length(RETLEV)))
MRETMEV <- data.frame(matrix(0, nrow = length(FITLEN), ncol = length(RETLEV)))

################

set.seed(111)
fityears <- data.frame(matrix(NA, nrow = 55, ncol = 53))
colnames(fityears)[1:53] <- c("name", "index", "mrisc_stnr", 1:50)
fityears[, 1] <- rlemp$name
fityears[, 2] <- rlemp$index
fityears[, 3] <- rlemp$mrisc_stnr

for (l in 1:10) {
  print(l)
  a <- 1
  for (i in fityears$index) {
    fityears[a, 4:53] <- sample(rain_aut[[i]][[4]]$year[2:(length(rain_aut[[i]][[4]]$year) - 1)], size = 50)
    a <- a + 1
  }
  
  fitindex = 1
  for (i in FITLEN) {
    #TMEV Teil
    tmev_est = pblapply(sub_stations, FUN = tmev_rlcalc, sample.len=i, cl=NR_CORES)
    tmev_dataframe = as.data.frame(do.call(rbind, tmev_est))
    tmev_err = (tmev_dataframe - rlemp[, 4:23]) / rlemp[, 4:23]
    RMSTMEV[fitindex, ] = RMSTMEV[fitindex, ] + sqrt(colMeans(tmev_err^2))
    MRETMEV[fitindex, ] = MRETMEV[fitindex, ] + colMeans(tmev_err)
    
    #SMEV Teil
    smev_est = pblapply(sub_stations, FUN = smev_rlcalc, sample.len=i, cl=NR_CORES)
    smev_dataframe = as.data.frame(do.call(rbind, smev_est))
    smev_err = (smev_dataframe - rlemp[, 4:23]) / rlemp[, 4:23]
    RMSSMEV[fitindex, ] = RMSSMEV[fitindex, ] + sqrt(colMeans(smev_err^2))
    MRESMEV[fitindex, ] = MRESMEV[fitindex, ] + colMeans(smev_err)
    
    fitindex = fitindex + 1
  }
}



### Plots

convert_to_long = function(data) {
  colnames(data) <- RETLEV
  data$M <- FITLEN
  
  df1 <- melt(data,id.vars = "M", measure.vars = colnames(data)[-ncol(data)] , 
              variable.name="RY", value.name="RL",na.rm = TRUE)
  df1$RY <- as.numeric(as.character(df1$RY))
  
  finermat <- with(df1, interp(x = RY, y = M, z = RL, linear = FALSE, extrap = TRUE,
                               xo=seq(5, 100, by = 0.25), 
                               yo=seq(4, 50, by = 0.25)))
  row.names(finermat$z) <- finermat$x
  colnames(finermat$z) <- finermat$y
  
  finerdf <- melt(finermat$z, value.name = "RL", varnames = c("RY", "M"))
  return(finerdf)
}

####RMS####

RMSSMEVfiner <- convert_to_long(RMSSMEV)
RMSTMEVfiner <- convert_to_long(RMSTMEV)
RMSSMEVfiner$type = "SMEV"
RMSTMEVfiner$type = "TMEV"

my_breaks <-  c(seq(5, 35, by = 2.5), 50, 100, 200)
my_colors <- colorRampPalette(rev(brewer.pal(11, "RdYlGn")), bias = 1.7)

merged_RMS <- rbind(RMSSMEVfiner, RMSTMEVfiner) 
merged_RMS$class <- cut(merged_RMS$RL * 10, breaks = my_breaks, 
                        labels = my_breaks[2:length(my_breaks)], right = FALSE)

p <- ggplot(merged_RMS, aes(x=RY, y=M,fill=class)) +
  geom_tile() +
  xlab("Q (years)") + ylab("M (years)") +
  scale_fill_manual(values = my_colors(length(my_breaks)),
                    breaks = my_breaks[1:length(my_breaks)-1],
                    name= "error (<_%)") +
  scale_x_continuous(breaks=seq(10,100,by=10) ,expand = c(0,0)) +
  scale_y_continuous(breaks=seq(5,50,by=5), expand = c(0,0)) +
  facet_wrap(~type, ncol=1) +
  coord_equal() +
  theme_bw(base_size=8) %+% 
  theme(
    strip.text.y = element_text(size=12),
    strip.text.x = element_text(size=6)
  )

#####MRE Plot

MRESMEVfiner <- convert_to_long(MRESMEV)
MRETMEVfiner <- convert_to_long(MRETMEV)
MRESMEVfiner$type <- "SMEV"
MRETMEVfiner$type <- "TMEV"

my_breaks2 <- c(seq(-9, 5, by = 1), 20, 200) 
my_colors2 <- colorRampPalette(brewer.pal(10,"BrBG"), bias = 1)

merged_MRE <- rbind(MRESMEVfiner, MRETMEVfiner) 
merged_MRE$class <- cut(merged_MRE$RL * 10, breaks = my_breaks2, 
                        labels = my_breaks2[2:length(my_breaks2)], right = FALSE)

q <- ggplot(merged_MRE, aes(x=RY, y=M,fill=class)) +
  geom_tile() +
  xlab("Q (years)") + ylab("M (years)") +
  scale_fill_manual(values = my_colors2(length(my_breaks2)),
                    breaks = my_breaks2[1:length(my_breaks2)-1],
                    name= "error (<_%)") +
  scale_x_continuous(breaks=seq(10,100,by=10) ,expand = c(0,0)) +
  scale_y_continuous(breaks=seq(5,50,by=5), expand = c(0,0)) +
  facet_wrap(~type, ncol=1) +
  coord_equal() +
  theme_bw(base_size=8) %+% 
  theme(
    strip.text.y = element_text(size=12),
    strip.text.x = element_text(size=6)
  )



###### RMS Error difference
errdiff <- RMSSMEV - RMSTMEV
errfiner <- convert_to_long(errdiff)

my_breaks3 <- c(seq(-3, 3, by = 1))
my_labels3 <- c(round(my_breaks3[2:(length(my_breaks3)-1)],digits = 1),">3%")
my_colors3 <- colorRampPalette(brewer.pal(5,"BrBG"), bias = 1)

errfiner$class <- ((cut(10 * errfiner$RL,
                        breaks = my_breaks3,
                        labels = my_breaks3[2:length(my_breaks3)],
                        right = FALSE)))

errfiner$type <- "DIVplot"
appender <- c(DIVplot = "ERR[SMEV]-ERR[TMEV]")

r <- ggplot(errfiner, aes(x=RY, y=M,fill=class)) +
  geom_tile() +
  xlab("Q (years)") + ylab("M (years)") +
  scale_fill_manual(values = my_colors3(length(my_breaks3)),
                    breaks = my_breaks3[1:length(my_breaks3)-1],
                    name= "error (<_%)") +
  scale_x_continuous(breaks=seq(10,100,by=10) ,expand = c(0,0)) +
  scale_y_continuous(breaks=seq(5,50,by=5), expand = c(0,0)) +
  facet_wrap(~type, ncol=1, 
             labeller = as_labeller(appender, default=label_parsed)) +
  coord_equal() +
  theme_bw(base_size=8) %+% 
  theme(
    strip.text.y = element_text(size=12),
    strip.text.x = element_text(size=6)
  )