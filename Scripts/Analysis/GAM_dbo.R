# 0 Preparation ----

# Load necessary packages
library("here")
library("dplyr")
library("mgcv")
library("ggplot2")

# Load data
setwd(here("Data"))
fish_dbo_data <- read.csv("fish_dbo_data.csv")
#

# Clean data
common_dbo_fish <- fish_dbo_data %>%
  filter(Artbestämning %in% c("Abborre", "Gärs", "Mört", "Storspigg", "Strömming"))

xxxxxxxxxxxxxxxxxxxxxxxxxxxx
# First group by year and calculate mean
library(lubridate)
common_chla_fish$Year <- year(common_chla_fish$Fiskedatum1)

# chla_fish_mean <- common_chla_fish %>%
  group_by(Fångstområde1, Artbestämning, Year) %>%   # Group by station, species and year
  summarise(
    mean_biomass = mean(Beräknad_vikt1, na.rm = TRUE),
    mean_cpu = mean(Antal_per_anstr1, na.rm = TRUE),
    mean_chla = mean(Mean_chla, na.rm = TRUE),
  )

  xxxxxxxxxxxxxxxxxxxxxxxxxx
  
# Change Fångstområde from character to factor
  common_dbo_fish$Fångstområde1 <- as.factor(common_dbo_fish$Fångstområde1)

# Change names to make modelling easier
common_dbo_fish <- common_dbo_fish %>%
  rename(
    TotalN = Total.N..μM.,
    NO3 = NO3..μM.,
    NO2 = NO2..μM.,
    NH4 = NH4..μM.,
    TotalP = Total.P..μM.,
    PO4 = PO4.P..μM.,
    SiO4 = SiO4..μM.,
    DOC = DOC..μM.,
    Secchidepth = Seccidepth..m.
  )

# 1 Model biomass -----
library("gratia")

# 1.1 Model parameters / Perch biomass ----
# GS smoother with same wiggliness
dbo_model_biomass_s <- gam(Beräknad_vikt1 ~  s(PO4, k=9,bs='tp') 
                          # + s(pH, k=9, bs='tp')
                          , data = subset(common_dbo_fish, Artbestämning=="Strömming") # & xxxx >= 
                          , family = gaussian(),method = "REML")
summary(dbo_model_biomass_s)
# gam.check(dbo_model_biomass_s)
x11()
draw(dbo_model_biomass_s)

# GS smoother with same wiggliness
dbo_model_biomass_s <- gam(Beräknad_vikt1 ~  s(PO4, by=Fångstområde1, k=9,bs='fs') 
                           # + s(NO3, k=9, bs='tp')
                           , data = subset(common_dbo_fish, Artbestämning=="Abborre") # & xxxx >= 
                           , family = gaussian(),method = "REML")
summary(dbo_model_biomass_s)
# gam.check(dbo_model_biomass_s)
x11()
draw(dbo_model_biomass_s)




# Global term plus group specific smoother with same wiggliness
dbo_model_biomass_gs <- gam(Beräknad_vikt1 ~ s(Year,Fångstområde1, k=9, bs='fs')
                           + s(Year, k=9, bs='tp')
                           + s(mean_temperature, k=9,bs='tp') 
                           + s(mean_salinity, k=9, bs='tp')
                          , data = subset(tsfish_mean, Artbestämning=="Abborre")
                          , family = gaussian(),method = "REML")
summary(dbo_model_biomass_gs)
x11()
draw(dbo_model_biomass_gs)
# Group specific smoother of different wiggliness
tsmodel_biomass_time_i <- gam(mean_biomass ~ s(Year, by=Fångstområde1, k=9, bs='fs')
                           + s(mean_temperature, k=9,bs='tp') 
                           + s(mean_salinity, k=9, bs='tp')
                          , data = subset(tsfish_mean, Artbestämning=="Abborre")
                          , family = Gamma(),method = "REML")
summary(tsmodel_biomass_time_i)
x11()
draw(tsmodel_biomass_time_i)
AIC(tsmodel_biomass_time_gs, tsmodel_biomass_time_i, tsmodel_biomass_time_s)

# 1.2 Model total three-spined stickleback biomass over time ----
# GS smoother with same wiggliness
chla_model_biomass_time_s <- gam(mean_biomass ~  s(mean_chla, k=9,bs='tp') 
                                 , data = subset(chla_fish_mean, Artbestämning=="Storspigg") # & mean_chla >= 
                                 , family = gaussian(),method = "REML")
summary(chla_model_biomass_time_s)
x11()
draw(chla_model_biomass_time_s)
# Global term plus group specific smoother with same wiggliness
tsmodel_biomass_time_gs <- gam(mean_biomass ~ s(Year,Fångstområde1, k=9, bs='fs')
                             + s(Year, k=9, bs='tp')
                             # + s(mean_temperature, k=9,bs='tp') 
                             # + s(mean_salinity, k=9, bs='tp')
                             , data = subset(tsfish_mean, Artbestämning=="Storspigg")
                             , family = gaussian(),method = "REML")
summary(tsmodel_biomass_time_gs)
x11()
draw(tsmodel_biomass_time_gs)
# Group specific smoother of different wiggliness
tsmodel_biomass_time_i <- gam(mean_biomass ~ s(Year, by=Fångstområde1, k=9, bs='fs')
                            # + s(mean_temperature, k=9,bs='tp') 
                            # + s(mean_salinity, k=9, bs='tp')
                            , data = subset(tsfish_mean, Artbestämning=="Storspigg")
                            , family = gaussian(),method = "REML")
summary(tsmodel_biomass_time_i)
x11()
draw(tsmodel_biomass_time_i)
AIC(tsmodel_biomass_time_gs, tsmodel_biomass_time_i, tsmodel_biomass_time_s)

# 1.3 Model total common roach biomass over time ----
# GS smoother with same wiggliness
chla_model_biomass_time_s <- gam(mean_biomass ~  s(mean_chla, k=9,bs='tp') 
                                 , data = subset(chla_fish_mean, Artbestämning=="Mört") # & mean_chla >= 
                                 , family = gaussian(),method = "REML")
summary(chla_model_biomass_time_s)
x11()
draw(chla_model_biomass_time_s)
# Global term plus group specific smoother with same wiggliness
tsmodel_biomass_time_gs <- gam(mean_biomass ~ s(Year,Fångstområde1, k=9, bs='fs')
                             + s(Year, k=9, bs='tp')
                             # + s(mean_temperature, k=9,bs='tp') 
                             # + s(mean_salinity, k=9, bs='tp')
                             , data = subset(tsfish_mean, Artbestämning=="Mört")
                             , family = gaussian(),method = "REML")
summary(tsmodel_biomass_time_gs)
x11()
draw(tsmodel_biomass_time_gs)
# Group specific smoother of different wiggliness
tsmodel_biomass_time_i <- gam(mean_biomass ~ s(Year, by=Fångstområde1, k=9, bs='fs')
                            # + s(mean_temperature, k=9,bs='tp') 
                            # + s(mean_salinity, k=9, bs='tp')
                            , data = subset(tsfish_mean, Artbestämning=="Mört")
                            , family = gaussian(),method = "REML")
summary(tsmodel_biomass_time_i)
x11()
draw(tsmodel_biomass_time_i)
AIC(tsmodel_biomass_time_gs, tsmodel_biomass_time_i, tsmodel_biomass_time_s)

# 1.4 Model total ruffe biomass over time ----
# GS smoother with same wiggliness
chla_model_biomass_time_s <- gam(mean_biomass ~  s(mean_chla, k=9,bs='tp') 
                                 , data = subset(chla_fish_mean, Artbestämning=="Gärs") # & mean_chla >= 
                                 , family = gaussian(),method = "REML")
summary(chla_model_biomass_time_s)
x11()
draw(chla_model_biomass_time_s)
# Global term plus group specific smoother with same wiggliness
tsmodel_biomass_time_gs <- gam(mean_biomass ~ s(Year,Fångstområde1, k=9, bs='fs')
                             + s(Year, k=9, bs='tp')
                             # + s(mean_temperature, k=9,bs='tp') 
                             # + s(mean_salinity, k=9, bs='tp')
                             , data = subset(tsfish_mean, Artbestämning=="Gärs")
                             , family = gaussian(),method = "REML")
summary(tsmodel_biomass_time_gs)
x11()
draw(tsmodel_biomass_time_gs)
# Group specific smoother of different wiggliness
tsmodel_biomass_time_i <- gam(mean_biomass ~ s(Year, by=Fångstområde1, k=9, bs='fs')
                            # + s(mean_temperature, k=9,bs='tp') 
                            # + s(mean_salinity, k=9, bs='tp')
                            , data = subset(tsfish_mean, Artbestämning=="Gärs")
                            , family = gaussian(),method = "REML")
summary(tsmodel_biomass_time_i)
x11()
draw(tsmodel_biomass_time_i)
AIC(tsmodel_biomass_time_gs, tsmodel_biomass_time_i, tsmodel_biomass_time_s)

# 1.5 Model total herring biomass over time ----
# GS smoother with same wiggliness
chla_model_biomass_time_s <- gam(mean_biomass ~  s(mean_chla, k=9,bs='tp') 
                                 , data = subset(chla_fish_mean, Artbestämning=="Strömming") # & mean_chla >= 
                                 , family = gaussian(),method = "REML")
summary(chla_model_biomass_time_s)
x11()
draw(chla_model_biomass_time_s)
# Global term plus group specific smoother with same wiggliness
tsmodel_biomass_time_gs <- gam(mean_biomass ~ s(Year,Fångstområde1, k=9, bs='fs')
                             + s(Year, k=9, bs='tp')
                             # + s(mean_temperature, k=9,bs='tp') 
                             # + s(mean_salinity, k=9, bs='tp')
                             , data = subset(tsfish_mean, Artbestämning=="Strömming")
                             , family = gaussian(),method = "REML")
summary(tsmodel_biomass_time_gs)
x11()
draw(tsmodel_biomass_time_gs)
# Group specific smoother of different wiggliness
tsmodel_biomass_time_i <- gam(mean_biomass ~ s(Year, by=Fångstområde1, k=9, bs='fs')
                            # + s(mean_temperature, k=9,bs='tp') 
                            # + s(mean_salinity, k=9, bs='tp')
                            , data = subset(tsfish_mean, Artbestämning=="Strömming")
                            , family = gaussian(),method = "REML")
summary(tsmodel_biomass_time_i)
x11()
draw(tsmodel_biomass_time_i)
AIC(tsmodel_biomass_time_gs, tsmodel_biomass_time_i, tsmodel_biomass_time_s)



# 1.6 Analyze results ----
# Per station
sm <- gratia::smooth_estimates(tsmodel_biomass_time_s)               # Change model if wanted
sm <- sm %>%
  add_constant(coef(tsmodel_biomass_time_s)["(Intercept)"]) %>%      # Change model if wanted
  add_confint(coverage=0.95) %>%
  transform_fun(inv_link(tsmodel_biomass_time_s))                    # Change model if wanted

GAM_timeseries <- ggplot(aes(x=mean_salinity,y=.estimate,group = Fångstområde1,colour = Fångstområde1),data=sm)+
  geom_line()+
  geom_ribbon(aes(ymin = .lower_ci,
                  ymax = .upper_ci,
                  fill = Fångstområde1), alpha = 0.2)+
  facet_wrap(~Fångstområde1) +
  geom_point(data = subset(tsfish_mean, Artbestämning == "Strömming"),  # Change Artbestämning to specific species used in model
             aes(x = mean_salinity, y = mean_biomass, colour = Fångstområde1),
             inherit.aes = FALSE) +
  labs(title = "Time series of herring biomass across stations",              # Change species name so title is correct
       x = "Year", y = "biomass")
  # + scale_y_continuous(limits = c(0, 100))  # <- Set min and max of y-axis if needed

x11()
GAM_timeseries



##############

# 2 Model CPU (count) ----

# 2.1 Model total Perch cpu over time ----
# GS smoother with same wiggliness
model_cpu_time_s <- gam(mean_cpu ~ s(Year,Fångstområde1, k=9, bs='fs') 
                            # + s(mean_temperature, k=9,bs='tp') 
                            # + s(mean_salinity, k=9, bs='tp')
                            , data = subset(fish_mean, Artbestämning=="Abborre")
                            , family = nb(),method = "REML")
summary(model_cpu_time_s)
x11()
draw(model_cpu_time_s)
# Global term plus group specific smoother with same wiggliness
model_cpu_time_gs <- gam(mean_cpu ~ s(Year,Fångstområde1, k=9, bs='fs')
                             + s(Year, k=9, bs='tp')
                             # + s(mean_temperature, k=9,bs='tp') 
                             # + s(mean_salinity, k=9, bs='tp')
                             , data = subset(fish_mean, Artbestämning=="Abborre")
                             , family = nb(),method = "REML")
summary(model_cpu_time_gs)
x11()
draw(model_cpu_time_gs)
# Group specific smoother of different wiggliness
model_cpu_time_i <- gam(mean_cpu ~ s(Year, by=Fångstområde1, k=9, bs='fs')
                            # + s(mean_temperature, k=9,bs='tp') 
                            # + s(mean_salinity, k=9, bs='tp')
                            , data = subset(fish_mean, Artbestämning=="Abborre")
                            , family = nb(),method = "REML")
summary(model_cpu_time_i)
x11()
draw(model_cpu_time_i)
AIC(model_cpu_time_gs, model_cpu_time_i, model_cpu_time_s)


# 2.2 Model total three-spined stickleback cpu over time ----
# GS smoother with same wiggliness
model_cpu_time_s <- gam(mean_cpu ~ s(Year,Fångstområde1, k=9, bs='fs') 
                            # + s(mean_temperature, k=9,bs='tp') 
                            # + s(mean_salinity, k=9, bs='tp')
                            , data = subset(fish_mean, Artbestämning=="Storspigg")
                            , family = nb(),method = "REML")
summary(model_cpu_time_s)
x11()
draw(model_cpu_time_s)
# Global term plus group specific smoother with same wiggliness
model_cpu_time_gs <- gam(mean_cpu ~ s(Year,Fångstområde1, k=9, bs='fs')
                             + s(Year, k=9, bs='tp')
                             # + s(mean_temperature, k=9,bs='tp') 
                             # + s(mean_salinity, k=9, bs='tp')
                             , data = subset(fish_mean, Artbestämning=="Storspigg")
                             , family = nb(),method = "REML")
summary(model_cpu_time_gs)
x11()
draw(model_biomass_time_gs)
# Group specific smoother of different wiggliness
model_cpu_time_i <- gam(mean_cpu ~ s(Year, by=Fångstområde1, k=9, bs='fs')
                            # + s(mean_temperature, k=9,bs='tp') 
                            # + s(mean_salinity, k=9, bs='tp')
                            , data = subset(fish_mean, Artbestämning=="Storspigg")
                            , family = nb(),method = "REML")
summary(model_cpu_time_i)
x11()
draw(model_cpu_time_i)
AIC(model_cpu_time_gs, model_cpu_time_i, model_cpu_time_s)

# 2.3 Model total common roach cpu over time ----
# GS smoother with same wiggliness
model_cpu_time_s <- gam(mean_cpu ~ s(Year,Fångstområde1, k=9, bs='fs') 
                            # + s(mean_temperature, k=9,bs='tp') 
                            # + s(mean_salinity, k=9, bs='tp')
                            , data = subset(fish_mean, Artbestämning=="Mört")
                            , family = nb(),method = "REML")
summary(model_cpu_time_s)
x11()
draw(model_cpu_time_s)
# Global term plus group specific smoother with same wiggliness
model_cpu_time_gs <- gam(mean_cpu ~ s(Year,Fångstområde1, k=9, bs='fs')
                             + s(Year, k=9, bs='tp')
                             # + s(mean_temperature, k=9,bs='tp') 
                             # + s(mean_salinity, k=9, bs='tp')
                             , data = subset(fish_mean, Artbestämning=="Mört")
                             , family = nb(),method = "REML")
summary(model_cpu_time_gs)
x11()
draw(model_cpu_time_gs)
# Group specific smoother of different wiggliness
model_cpu_time_i <- gam(mean_cpu ~ s(Year, by=Fångstområde1, k=9, bs='fs')
                            # + s(mean_temperature, k=9,bs='tp') 
                            # + s(mean_salinity, k=9, bs='tp')
                            , data = subset(fish_mean, Artbestämning=="Mört")
                            , family = nb(),method = "REML")
summary(model_cpu_time_i)
x11()
draw(model_cpu_time_i)
AIC(model_cpu_time_gs, model_cpu_time_i, model_cpu_time_s)

# 2.4 Model total ruffe cpu over time ----
# GS smoother with same wiggliness
model_cpu_time_s <- gam(mean_cpu ~ s(Year,Fångstområde1, k=9, bs='fs') 
                            # + s(mean_temperature, k=9,bs='tp') 
                            # + s(mean_salinity, k=9, bs='tp')
                            , data = subset(fish_mean, Artbestämning=="Gärs")
                            , family = nb(),method = "REML")
summary(model_cpu_time_s)
x11()
draw(model_cpu_time_s)
# Global term plus group specific smoother with same wiggliness
model_cpu_time_gs <- gam(mean_cpu ~ s(Year,Fångstområde1, k=9, bs='fs')
                             + s(Year, k=9, bs='tp')
                             # + s(mean_temperature, k=9,bs='tp') 
                             # + s(mean_salinity, k=9, bs='tp')
                             , data = subset(fish_mean, Artbestämning=="Gärs")
                             , family = nb(),method = "REML")
summary(model_cpu_time_gs)
x11()
draw(model_cpu_time_gs)
# Group specific smoother of different wiggliness
model_cpu_time_i <- gam(mean_cpu ~ s(Year, by=Fångstområde1, k=9, bs='fs')
                            # + s(mean_temperature, k=9,bs='tp') 
                            # + s(mean_salinity, k=9, bs='tp')
                            , data = subset(fish_mean, Artbestämning=="Gärs")
                            , family = nb(),method = "REML")
summary(model_cpu_time_i)
x11()
draw(model_cpu_time_i)
AIC(model_cpu_time_gs, model_cpu_time_i, model_cpu_time_s)

# 2.5 Model total herring cpu over time ----
# GS smoother with same wiggliness
model_cpu_time_s <- gam(mean_cpu ~ s(Year,Fångstområde1, k=9, bs='fs') 
                            # + s(mean_temperature, k=9,bs='tp') 
                            # + s(mean_salinity, k=9, bs='tp')
                            , data = subset(fish_mean, Artbestämning=="Strömming")
                            , family = nb(),method = "REML")
summary(model_cpu_time_s)
x11()
draw(model_cpu_time_s)
# Global term plus group specific smoother with same wiggliness
model_cpu_time_gs <- gam(mean_cpu ~ s(Year,Fångstområde1, k=9, bs='fs')
                             + s(Year, k=9, bs='tp')
                             # + s(mean_temperature, k=9,bs='tp') 
                             # + s(mean_salinity, k=9, bs='tp')
                             , data = subset(fish_mean, Artbestämning=="Strömming")
                             , family = nb(),method = "REML")
summary(model_cpu_time_gs)
x11()
draw(model_cpu_time_gs)
# Group specific smoother of different wiggliness
model_cpu_time_i <- gam(mean_cpu ~ s(Year, by=Fångstområde1, k=9, bs='fs')
                            # + s(mean_temperature, k=9,bs='tp') 
                            # + s(mean_salinity, k=9, bs='tp')
                            , data = subset(fish_mean, Artbestämning=="Strömming")
                            , family = nb(),method = "REML")
summary(model_cpu_time_i)
x11()
draw(model_cpu_time_i)
AIC(model_cpu_time_gs, model_cpu_time_i, model_cpu_time_s)



# 2.6 Analyze results ----
# Per station
sm2 <- gratia::smooth_estimates(model_cpu_time_s)             # Change model type if wanted
sm2 <- sm2 %>%
  add_constant(coef(model_cpu_time_s)["(Intercept)"]) %>%     # Change model type if wanted
  add_confint(coverage=0.95) %>%
  transform_fun(inv_link(model_cpu_time_s))                   # Change model type if wanted

GAM_timeseries_cpu <- ggplot(aes(x=Year,y=.estimate,group = Fångstområde1,colour = Fångstområde1),data=sm2)+
  geom_line()+
  geom_ribbon(aes(ymin = .lower_ci,
                  ymax = .upper_ci,
                  fill = Fångstområde1), alpha = 0.2)+
  facet_wrap(~Fångstområde1) +
  geom_point(data = subset(fish_mean, Artbestämning == "Abborre"),     # Change Artbestämning to specific species used in model
             aes(x = Year, y = mean_cpu, colour = Fångstområde1),
             inherit.aes = FALSE) +
  labs(title = "Time series of perch CPU across stations",              # Change species name so title is correct
    x = "Year", y = "CPU") #+
   # scale_y_continuous(limits = c(0, 150))  # <- Set min and max of y-axis if needed

x11()
GAM_timeseries_cpu



# 3 Model Biomass/CPU relation ----

# Calculate Biomass per CPU
fish_mean$biomass_per_cpu <- fish_mean$mean_biomass / fish_mean$mean_cpu

# 3.1 Model total Perch biomass per cpu over time ----
# GS smoother with same wiggliness
model_cpu_time_s <- gam(biomass_per_cpu ~ s(Year,Fångstområde1, k=9, bs='fs') 
                        # + s(mean_temperature, k=9,bs='tp') 
                        # + s(mean_salinity, k=9, bs='tp')
                        , data = subset(fish_mean, Artbestämning=="Abborre")
                        , family = gaussian(),method = "REML")
summary(model_cpu_time_s)
x11()
draw(model_cpu_time_s)

# 3.2 Model total three-spined stickleback biomass per cpu over time ----
# GS smoother with same wiggliness
model_cpu_time_s <- gam(biomass_per_cpu ~ s(Year,Fångstområde1, k=9, bs='fs') 
                        # + s(mean_temperature, k=9,bs='tp') 
                        # + s(mean_salinity, k=9, bs='tp')
                        , data = subset(fish_mean, Artbestämning=="Storspigg")
                        , family = nb(),method = "REML")
summary(model_cpu_time_s)
x11()
draw(model_cpu_time_s)

# 3.3 Model total common roach biomass per cpu over time ----
# GS smoother with same wiggliness
model_cpu_time_s <- gam(biomass_per_cpu ~ s(Year,Fångstområde1, k=9, bs='fs') 
                        # + s(mean_temperature, k=9,bs='tp') 
                        # + s(mean_salinity, k=9, bs='tp')
                        , data = subset(fish_mean, Artbestämning=="Mört")
                        , family = nb(),method = "REML")
summary(model_cpu_time_s)
x11()
draw(model_cpu_time_s)

# 3.4 Model total ruffe biomass per cpu over time ----
# GS smoother with same wiggliness
model_cpu_time_s <- gam(biomass_per_cpu ~ s(Year,Fångstområde1, k=9, bs='fs') 
                        # + s(mean_temperature, k=9,bs='tp') 
                        # + s(mean_salinity, k=9, bs='tp')
                        , data = subset(fish_mean, Artbestämning=="Gärs")
                        , family = nb(),method = "REML")
summary(model_cpu_time_s)
x11()
draw(model_cpu_time_s)

# 3.5 Model total herring biomass per cpu over time ----
# GS smoother with same wiggliness
model_cpu_time_s <- gam(biomass_per_cpu ~ s(Year,Fångstområde1, k=9, bs='fs') 
                        # + s(mean_temperature, k=9,bs='tp') 
                        # + s(mean_salinity, k=9, bs='tp')
                        , data = subset(fish_mean, Artbestämning=="Strömming")
                        , family = nb(),method = "REML")
summary(model_cpu_time_s)
x11()
draw(model_cpu_time_s)

# 3.6 Analyze results ----
# Per station
sm3 <- gratia::smooth_estimates(model_cpu_time_s)             # Change model type if wanted
sm3 <- sm3 %>%
  add_constant(coef(model_cpu_time_s)["(Intercept)"]) %>%     # Change model type if wanted
  add_confint(coverage=0.95) %>%
  transform_fun(inv_link(model_cpu_time_s))                   # Change model type if wanted

GAM_timeseries_bm_cpu <- ggplot(aes(x=Year,y=.estimate,group = Fångstområde1,colour = Fångstområde1),data=sm3)+
  geom_line()+
  geom_ribbon(aes(ymin = .lower_ci,
                  ymax = .upper_ci,
                  fill = Fångstområde1), alpha = 0.2)+
  facet_wrap(~Fångstområde1) +
  geom_point(data = subset(fish_mean, Artbestämning == "Storspigg"),     # Change Artbestämning to specific species used in model
             aes(x = Year, y = biomass_per_cpu, colour = Fångstområde1),   # Change Y to correct data
             inherit.aes = FALSE) +
  labs(title = "Time series of stickleback Biomass per CPU across stations",              # Change species name so title is correct
       x = "Year", y = "Biomass per CPU") #+
# scale_y_continuous(limits = c(0, 150))  # <- Set min and max of y-axis if needed

x11()
GAM_timeseries_bm_cpu
