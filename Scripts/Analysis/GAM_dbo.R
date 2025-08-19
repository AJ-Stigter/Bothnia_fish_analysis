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
dbo_model_biomass_s <- gam(Beräknad_vikt1 ~  s(PO4, k=9,bs='tp')    # Change smoother for different variables
                          , data = subset(common_dbo_fish, Artbestämning=="Abborre")
                          , family = gaussian(),method = "REML")
summary(dbo_model_biomass_s)
gam.check(dbo_model_biomass_s)
x11()
draw(dbo_model_biomass_s)

# GS smoother with same wiggliness, look at different stations
dbo_model_biomass_s <- gam(Beräknad_vikt1 ~  s(PO4, by=Fångstområde1, k=9,bs='fs') 
                           , data = subset(common_dbo_fish, Artbestämning=="Abborre")
                           , family = gaussian(),method = "REML")
summary(dbo_model_biomass_s)
# gam.check(dbo_model_biomass_s)
x11()
draw(dbo_model_biomass_s)



# 1.2 Model parameters / Herring biomass ----
# GS smoother with same wiggliness
chla_model_biomass_time_s <- gam(mean_biomass ~  s(PO4, k=9,bs='tp') # Change smoother for different variables
                                 , data = subset(common_dbo_fish, Artbestämning=="Strömming") 
                                 , family = gaussian(),method = "REML")
summary(chla_model_biomass_time_s)
x11()
draw(chla_model_biomass_time_s)
# Global term plus group specific smoother with same wiggliness
tsmodel_biomass_time_gs <- gam(mean_biomass ~ s(PO4,Fångstområde1, k=9, bs='fs')
                             + s(PO4, k=9, bs='tp')
                             , data = subset(common_dbo_fish, Artbestämning=="Strömming")
                             , family = gaussian(),method = "REML")
summary(tsmodel_biomass_time_gs)
x11()
draw(tsmodel_biomass_time_gs)
# Group specific smoother of different wiggliness
tsmodel_biomass_time_i <- gam(mean_biomass ~ s(PO4, by=Fångstområde1, k=9, bs='fs')
                            , data = subset(common_dbo_fish, Artbestämning=="Strömming")
                            , family = gaussian(),method = "REML")
summary(tsmodel_biomass_time_i)
x11()
draw(tsmodel_biomass_time_i)
AIC(tsmodel_biomass_time_gs, tsmodel_biomass_time_i, tsmodel_biomass_time_s)



# 1.3 Analyze results ----
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
  geom_point(data = subset(common_dbo_fish, Artbestämning == "Strömming"),  # Change Artbestämning to specific species used in model
             aes(x = PO4, y = mean_biomass, colour = Fångstområde1),
             inherit.aes = FALSE) +
  labs(title = "Influence of PO4 on herring biomass across stations", # Change species name so title is correct
       x = "Year", y = "biomass")
  # + scale_y_continuous(limits = c(0, 100))  # <- Set min and max of y-axis if needed

x11()
GAM_timeseries