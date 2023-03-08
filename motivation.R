library("this.path")
library("bamlss")
library("lubridate")
library("ggplot2")

#Lade Daten und Module
path <- this.dir()
source(file.path(path, "ccc.R"))
load(file.path(path, "data", "rain_aut.Rda"))

#Motivation

Alambda = subset(rain_aut[[1]]$data, year == 2000)
Alambda$val = Alambda$val / 10
Alambda$date = as.Date(with(Alambda, paste(year, month, day, sep = "-")),
                       "%Y-%m-%d")
bamy <- bamlss(formula = fconst, family = weibull_bamlss,
               data = Alambda, sampler = FALSE)
Alambda$param <- exp(bamy$fitted.values$lambda[[1]])

Blambda = subset(rain_aut[[1195]]$data, year == 2000)
Blambda$val = Blambda$val / 10
Blambda$date = as.Date(with(Blambda, paste(year, month, day, sep = "-")),
                       "%Y-%m-%d")
bamy2 <- bamlss(formula = fconst, family = weibull_bamlss,
                data = Blambda, sampler = FALSE)
Blambda$param <- exp(bamy2$fitted.values$lambda[[1]])

df <- rbind(Alambda, Blambda)
df$dataset = c(rep("Hochfilzen", nrow(Alambda)), 
               rep("Kötschach-Mauthen", nrow(Blambda)))

Sys.setlocale("LC_ALL", "en_GB.UTF-8")
ggplot(df, aes(x = date, y = val )) +
  geom_segment(aes(x = date, xend = date, y = 0, yend = val), 
               linewidth = 0.5, alpha = 1) + 
  geom_hline(aes(yintercept = param), col = "red", linetype = "dashed") +
  facet_wrap(~dataset, ncol = 1) +
  labs(x= "Date", y= "Daily rainfall [mm]") +
  scale_x_date(date_breaks = "2 month", date_labels = "%B") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())