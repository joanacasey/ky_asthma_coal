##################################################################################################################
# Code used to conduct indiv-level analyses for "Reduced coal-fired power plant emissions related to improved    # 
# community asthma-related healthcare utilization and individual symptoms in Louisville, Kentucky, USA"          #
# Code developed primarily by Joan Casey and also by Andreas Neophytou                                           #
# Updated October 8, 2019                                                                                        #
##################################################################################################################

########################################################
####INDIVIDUAL HEALTH DATA WITH DISTANCES TO PLANTS#####
########################################################
#load libraries
library(cglm)
library(tidyverse)
library(gnm)
library(mgcv)
library(zoo)
library(tidyquant)

###SET SEED###
set.seed(63018)

#Individual-level data
dati <- read_csv("Jefferson_events_assigned_so2_hyads_weather_pollen_distCRun-MCreek_SO2HarvardEmi_to11292017_revised.csv")

#207 ids that should be included
load("Louisville_IDs_207.Rdata")
ids <- tibble(ids)

##GENERATE MONTH AND YEAR IN READABLE FORMAT##
dati$Date <- as.Date(dati$Date,"%m/%d/%Y")
dati$Datec <- dati$Date-as.Date("2012-05-25")
dati$monthf  <- as.factor(months(dati$Date))
dati$month <- as.numeric(dati$monthf)
dati$year   <- as.factor(format(dati$Date, format="%Y") )
dati$dow    <- as.factor(weekdays(dati$Date))
dati$stratum <- as.factor(dati$year:dati$month:dati$dow)
dati$moyr <- as.factor(dati$year:dati$month)
dati$date2 <- as.numeric(dati$Datec)

#Restrict to the 207 ids with data in the pre and post period
dati <- left_join(ids, dati, by = c("ids" = "uid"))

#Indicator of 3 or more uses
dati$Rscu_4 <- ifelse(dati$Rscu > 3, 1, 0)

#Variable = 0 if no use, = 1 if use
dati$Rscu_bi <- ifelse(dati$Rscu > 0, 1, 0)

###Summary of follow-up###
entry <- dati %>% dplyr::select(ids, Date) %>%
  group_by(ids) %>%
  arrange(Date) %>%
  slice(1)
colnames(entry)[2] <- "entry"

exit <- dati %>% dplyr::select(ids, Date) %>%
  group_by(ids) %>%
  arrange(Date) %>%
  slice(n())
colnames(exit)[2] <- "exit"

#Add these back to dati
dati <- left_join(dati, entry)
dati <- left_join(dati, exit)

#Duration of follow-up
dati$entry_yr <- year(dati$entry)
dati$duration <- (dati$exit - dati$entry)

#Create a time since scrubber variable
dati$time_since <- (dati$date2 - 1475)

#Exposure HyADS monthly over time
dati_mo_hy <- dati %>% 
  tq_transmute(select     = SO2_Harvard,
               mutate_fun = apply.monthly,
               FUN        = mean)

dati_mo_hy <- dati_mo_hy %>% filter(Date > as.Date("2015-01-01"))
ggplot(dati_mo_hy, aes(Date, SO2_Harvard)) + 
  geom_line() +
  xlab("") +
  scale_color_manual("", values = "#6b4596ff") + scale_x_date(date_breaks = "3 months", date_labels = "%m-%Y", 
                                                              date_minor_breaks = "1 month") +
  scale_y_continuous("Coal-fired power plant exposure")+
  theme_minimal(base_size = 14) +
  geom_vline(xintercept = as.numeric(as.Date("2016-06-08")), linetype=3) +
  geom_vline(xintercept = as.numeric(as.Date("2015-05-23")), linetype=3) +
  theme(legend.position="none", axis.text.x = element_text(angle = 90, hjust = .5, vjust = 0.5)) 

#Look at exposure to Mill Creek Unit 3 specifically
dati_mo_hy <- dati %>% 
  tq_transmute(select     = Mcrk_u1364_3,
               mutate_fun = apply.monthly,
               FUN        = mean)

dati_mo_hy <- dati_mo_hy %>% filter(Date > as.Date("2015-01-01"))
ggplot(dati_mo_hy, aes(Date, Mcrk_u1364_3)) + 
  geom_line() +
  xlab("") +
  scale_color_manual("", values = "#6b4596ff") + scale_x_date(date_breaks = "3 months", date_labels = "%m-%Y", 
                                                              date_minor_breaks = "1 month") +
  scale_y_continuous("Mill Creek unit 3 exposure")+
  theme_minimal(base_size = 14) +
  geom_vline(xintercept = as.numeric(as.Date("2016-06-08")), linetype=3) +
  geom_vline(xintercept = as.numeric(as.Date("2015-05-23")), linetype=3) +
  theme(legend.position="none", axis.text.x = element_text(angle = 90, hjust = .5, vjust = 0.5)) 

#Restrict to year before and after Mill Creek event
dati_mo <- filter(dati, Date>as.Date("2015-04-30") & Date < as.Date("2017-06-1"))

############
# Figure 4A#
############
mill <- read_csv("monthy_hyads_MillCrk.csv")
mill <- filter(mill, datemonth>as.Date("2015-05-01") & datemonth<as.Date("2017-07-01"))
mill_mo <- mill %>% 
  group_by(uid) %>%
  tq_transmute(select     = hyads,
               mutate_fun = apply.monthly,
               FUN        = mean)
colfuncg <- colorRampPalette(c("lightgreen", "darkgreen"))
colos <- c(colfuncg(4))
tiff(filename = "TimeSeriesMillCreekB.tiff", type = "cairo" ,  res=300, height = 4, width = 5, units = "in")
ggplot(mill_mo, aes(datemonth, hyads, group = uid, fill=uid)) + 
  geom_area() +
  xlab("") +
  scale_fill_manual("Unit ID", values= colos) + scale_x_date(date_breaks = "3 months", 
                                                             date_labels = "%m-%Y", 
                                                             date_minor_breaks = "1 month",
                                                             limits = c(as.Date("2015-05-01"), as.Date("2017-07-01"))) +
  scale_y_continuous("Coal-fired power plant exposure")+
  theme_minimal(base_size = 14) +
  geom_vline(xintercept = as.numeric(as.Date("2016-06-08")), linetype=3, size= 1.2) +
  theme(axis.text.x = element_text(angle = 90, hjust = .5, vjust = 0.5)) 
dev.off()

#Aggregate variables to monthly level
dati_mo <- dati_mo %>% 
  group_by(ids) %>%
  tq_transmute(select     = c(RPL_THEMES, Rscu, Mold, WindSpeed, RelativeHumidity, StationPressure, Weed, Grass, Tree, SO2_Harvard, SO2PPB, time_since, DryBulbFarenheit, Mcrk_u1364_3, Rscu_bi, Rscu_4),
               mutate_fun = apply.monthly,
               FUN        = mean)

#Binary rescue inhaler use variable
dati_mo$Rscu_bi <- ifelse(dati_mo$Rscu_bi > 0.5, 1, 0)

#Add total monthly rscu use
dati_mo2 <- dati %>% 
  group_by(ids) %>%
  tq_transmute(select     = c(Rscu),
               mutate_fun = apply.monthly,
               FUN        = sum)
head(dati_mo2)
dati_mo2$Rscu_sum <- dati_mo2$Rscu
dati_mo2 <-  dplyr::select(dati_mo2, Date, Rscu_sum)
dati_mo <- left_join(dati_mo, dati_mo2, by = c("ids"="ids", "Date"  = "Date"))

#Duration of follow-up
###Summary of follow-up###
entry <- dati_mo %>% dplyr::select(ids, Date) %>%
  group_by(ids) %>%
  arrange(Date) %>%
  slice(1)
colnames(entry)[2] <- "entry"

exit <- dati_mo %>% dplyr::select(ids, Date) %>%
  group_by(ids) %>%
  arrange(Date) %>%
  slice(n())
colnames(exit)[2] <- "exit"

#Add these back to dati
dati_mo <- left_join(dati_mo, entry)
dati_mo <- left_join(dati_mo, exit)

#Duration of follow-up
dati_mo$entry_yr <- year(dati_mo$entry)
dati_mo$duration <- (dati_mo$exit - dati_mo$entry)

#Scrubber, completely shut down May 2016
dati_mo <- dati_mo %>% 
  mutate(scrub4 = ifelse(Date > "2016-04-30", 1, 0))

#Create variable for months since beginning for each person
dati_mo$number <- 1
dati_mo <- dati_mo %>%
  group_by(ids) %>%
  mutate(time = cumsum(number))

#Figure out mean HyADS in per-period to Mill Creek Unit 3
dati_mo2 <- dati %>% 
  filter(Date < as.Date("2016-05-08") & Date > as.Date("2015-05-07"))
dati_mo2 <- aggregate(dati_mo2$Mcrk_u1364_3, list(dati_mo2$ids), mean)
dati_mo2$pre_hyads <- dati_mo2$x

#Merge back pre-values onto the dati_mo2
dati_mo <- left_join(dati_mo, dati_mo2, by=c("ids"="Group.1"))
dati_mo <- dati_mo %>% 
  mutate(scrub4 = ifelse(Date > "2016-04-30", 1, 0))

#Time
dati_mo$Date <- as.Date(dati_mo$Date,"%m/%d/%Y")
dati_mo$monthf  <- as.factor(months(dati_mo$Date))
dati_mo$month <- month(dati_mo$Date)
dati_mo$year   <- year(dati_mo$Date)
dati_mo <- dati_mo %>% mutate(t = case_when(year == 2015 & month == 5 ~ 1,
                                            year == 2015 & month == 6 ~ 2,
                                            year == 2015 & month == 7 ~ 3,
                                            year == 2015 & month == 8 ~ 4,
                                            year == 2015 & month == 9 ~ 5,
                                            year == 2015 & month == 10 ~ 6,
                                            year == 2015 & month == 11 ~ 7,
                                            year == 2015 & month == 12 ~ 8,
                                            year == 2016 & month == 1 ~ 9,
                                            year == 2016 & month == 2 ~ 10,
                                            year == 2016 & month == 3 ~ 11,
                                            year == 2016 & month == 4 ~ 12,
                                            year == 2016 & month == 5 ~ 13,
                                            year == 2016 & month == 6 ~ 14,
                                            year == 2016 & month == 7 ~ 15,
                                            year == 2016 & month == 8 ~ 16,
                                            year == 2016 & month == 9 ~ 17,
                                            year == 2016 & month == 10 ~ 18,
                                            year == 2016 & month == 11 ~ 19,
                                            year == 2016 & month == 12 ~ 20,
                                            year == 2017 & month == 1 ~ 21,
                                            year == 2017 & month == 2 ~ 22,
                                            year == 2017 & month == 3 ~ 23,
                                            year == 2017 & month == 4 ~ 24,
                                            year == 2017 & month == 5 ~ 25 ))
table(dati_mo$t)

#Main adjusted model - continouus HyADS
#Rescale HyADS exposure, divide by 1000
divide.1000 <- function (x) {
  x/1000
}
dati_mo$SO2_Harvard <- divide.1000(dati_mo$SO2_Harvard)
dati_mo$SO2_Harvard_neg <- dati_mo$SO2_Harvard*-1

#gnm model for continous HyADS exposure
model <- gnm(Rscu ~ SO2_Harvard + RPL_THEMES + ns(RelativeHumidity,2) + ns(WindSpeed,2) +  ns(DryBulbFarenheit, 2) + ns(StationPressure, 2) +
               ns(Mold,2) + Weed + Tree + Grass + harmonic(month, 2, 12) , dat=dati_mo, family=quasipoisson,
             eliminate = factor(ids))

#Using Brumback autocorrelation adjustment
reslag1 <- Lag(residuals(model,type="deviance"),1)
model2 <- gnm(Rscu ~ SO2_Harvard + RPL_THEMES + ns(RelativeHumidity,2) + ns(WindSpeed,2)  +   ns(DryBulbFarenheit, 2) +ns(StationPressure, 2) +
                ns(Mold,2) + Weed + Tree + Grass  + harmonic(month, 2, 12) + reslag1, data=dati_mo, family=quasipoisson,
              eliminate = factor(ids))
summary(model2)

#Link from gnm model
fam <- family(model2)
#Inverse of link 
ilink <- fam$linkinv
ilink(coef(model2))

#Main model for interrupted time series - indicator for scrubber installation
dati_mo$t_scrub4 <- dati_mo$t * dati_mo$scrub4

#GNM Model for interrupted time series
#Unadjusted
mod_gnm_unadj <- gnm(Rscu ~ scrub4 + t + t_scrub4, data=dati_mo, family="quasipoisson",
                     eliminate=factor(ids))
#Using Brumback autocorrelation adjustment
reslag1_unadj <- Lag(residuals(mod_gnm_unadj,type="deviance"),1)
mod_gnm_unadj <- gnm(Rscu ~ scrub4 + t + t_scrub4 + reslag1_unadj, data=dati_mo, family="quasipoisson",
                     eliminate=factor(ids))
summary(mod_gnm_unadj) 

#######################
#Adjusted - Equation 5#
#######################
mod_gnm_adjust <- gnm(Rscu ~ scrub4 + t + t_scrub4 + RPL_THEMES + harmonic(month, 1, 12)  + ns(StationPressure, 2) +
                        +ns(RelativeHumidity,2) + WindSpeed  + ns(Mold,2) + Weed + Tree + ns(DryBulbFarenheit, 2) +
                        Grass, data=dati_mo, family="quasipoisson",
                      eliminate=factor(ids))
reslag1 <- Lag(residuals(mod_gnm_adjust, type="deviance"),1)
mod_gnm_adjust2 <- gnm(Rscu ~ scrub4 + t + t_scrub4 + RPL_THEMES + harmonic(month, 1, 12)  + reslag1 + ns(StationPressure, 2) +
                         +ns(RelativeHumidity,2) + WindSpeed  + ns(Mold,2) + Weed + Tree + ns(DryBulbFarenheit, 2) +
                         Grass, data=dati_mo, family="quasipoisson",
                       eliminate=factor(ids))
summary(mod_gnm_adjust2)

#Predicted values with original data
predictedSABAint<-predict(mod_gnm_adjust2, type = "response", se.fit = F)
dati_mo$predictedSABAint <- predictedSABAint 

dati_mo_plot <- dati_mo %>% group_by(t) %>% summarize(mean_rscu = mean(predictedSABAint))
dati_mo_plot <- dati_mo_plot  %>% mutate(switch=case_when(t <13  ~ "pre", 
                                                          (t>=12 ~ "post")), 
                                         t.switch = interaction(switch, t))

##################
####FIGURE 5######
##################
tiff(filename = "Ind_Reg_Discon_gnm.tiff", type = "cairo" ,  res=300, height = 5, width = 5, units = "in")
ggplot(dati_mo_plot, aes(x = t, y = mean_rscu, colour = switch, group=switch)) +
  geom_point(size = 1.4) +
  scale_x_continuous("", breaks=c(1,4,7,10,13,16,19,22,25), labels=c("2015-05", "2015-08", "2015-11", "2016-02", "2016-05", "2016-08", "2016-11", "2017-02", "2017-05")) +
  scale_color_manual("", labels=c("Pre", "Post"), values = c("pre" = "#eb8055ff",  "post" = "#13306dff")) +
  scale_y_continuous("Monthly average daily SABA use")+
  geom_vline(xintercept = 13, linetype=3) +
  guides(color=F) + theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  annotate("text", label ="Mill Creek \nscrubber", hjust = 0,
           x = 13.2, y = 2.5, color = "black", size= 3.5, fontface=2) 
dev.off()

#Sensitivity with high and binary use
#gnm model
#Binary 1+ vs <1 on average
#Unadjusted
mod_gnm_unadj <- gnm(Rscu_bi ~ scrub4 + t + t_scrub4, data=dati_mo, family="binomial",
                     eliminate=factor(ids))
fam2 <- family(mod_gnm_unadj)
ilink2 <- fam2$linkinv
#Using Brumback autocorrelation adjustment
reslag1_unadj <- Lag(residuals(mod_gnm_unadj,type="deviance"),1)
mod_gnm_unadj <- gnm(Rscu_bi ~ scrub4 + t + t_scrub4 + reslag1_unadj, data=dati_mo, family="binomial",
                     eliminate=factor(ids))
summary(mod_gnm_unadj) 
#Adjusted
mod_gnm <- gnm(Rscu_bi ~ scrub4 + t + t_scrub4 + harmonic(month, 1, 12)  + ns(StationPressure, 2) + 
                 +ns(RelativeHumidity,2) + WindSpeed  + ns(Mold,2) + Weed + Tree + ns(DryBulbFarenheit, 2) +
                 Grass + RPL_THEMES, data=dati_mo, family="binomial",
               eliminate=factor(ids))
#Using Brumback autocorrelation adjustment for high vs. not use
reslag1 <- Lag(residuals(mod_gnm,type="deviance"),1)
mod_gnm <- gnm(Rscu_bi ~ scrub4 + t + t_scrub4 + harmonic(month, 1, 12)  + reslag1 + ns(StationPressure, 2) + 
                 +ns(RelativeHumidity,2) + WindSpeed  + ns(Mold,2) + Weed + Tree + ns(DryBulbFarenheit, 2) +
                 Grass + RPL_THEMES, data=dati_mo, family="binomial",
               eliminate=factor(ids))

#Binary 4+ vs. <4 on average
#Adjusted
mod_gnm <- gnm(Rscu_4 ~ scrub4 + t + t_scrub4 + harmonic(month, 1, 12)  + ns(StationPressure, 2) + 
                 +ns(RelativeHumidity,2) + WindSpeed  + ns(Mold,2) + Weed + Tree + ns(DryBulbFarenheit, 2) +
                 Grass, data=dati_mo, family="quasipoisson",
               eliminate=factor(ids))
reslag1 <- Lag(residuals(mod_gnm,type="deviance"),1)
mod_gnm <- gnm(Rscu_4 ~ scrub4 + t + t_scrub4 + harmonic(month, 1, 12)  + reslag1 + ns(StationPressure, 2) + 
                 +ns(RelativeHumidity,2) + WindSpeed  + ns(Mold,2) + Weed + Tree + ns(DryBulbFarenheit, 2) +
                 Grass + RPL_THEMES, data=dati_mo, family="quasipoisson",
               eliminate=factor(ids))
summary(mod_gnm)