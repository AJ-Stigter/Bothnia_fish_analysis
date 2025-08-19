# 0 Preparation ----

# Load necessary packages
library("here")
library("dplyr")
library("mgcv")
library("ggplot2")

# Load data
setwd(here("Data"))
tsfishdata <- read.csv("fish_env_data.csv")
#

# Clean data
common_tsfish <- tsfishdata %>%
  filter(Artbestämning %in% c("Abborre", "Gärs", "Mört", "Storspigg", "Strömming"))

#Change Fångstområde from character to factor
common_tsfish$Fångstområde1 <- as.factor(common_tsfish$Fångstområde1)


# 1 Model biomass -----
library("gratia")

# 1.1 Model temp&sal / Perch biomass ----
# GS smoother with same wiggliness
tsmodel_biomass_time_s <- gam(Beräknad_vikt1 ~   s(Mean_Temperature_env, k=9, bs='fs')
                              # s(Salinity_env, k=9, bs='tp')
                              , data = subset(common_tsfish, Artbestämning=="Abborre")
                              , family = gaussian(),method = "REML")
summary(tsmodel_biomass_time_s)
x11()
draw(tsmodel_biomass_time_s)


# 1.2 Model temp&sal / Herring biomass ----
# GS smoother with same wiggliness
tsmodel_biomass_time_s <- gam(Beräknad_vikt1 ~ #s(mean_temperature, k=9,bs='fs') 
                                s(mean_salinity, k=9, bs='fs')
                              , data = subset(common_tsfish, Artbestämning=="Strömming")
                              , family = gaussian(),method = "REML")
summary(tsmodel_biomass_time_s)
x11()
draw(tsmodel_biomass_time_s)
