# Code used to conduct analyses for "Reduced coal-fired power plant emissions related to improved 
# community asthma-related healthcare utilization and individual symptoms in Louisville, Kentucky, USA"
# Code developed primarily by Joan Casey and also by Andreas Neophytou 
# Updated May 21, 2019

#Load libraries
library(tidyverse)
library(openxlsx)
library(RColorBrewer)
library(sjlabelled)
library(splines)
library(MASS)
library(lubridate)
library(gnm)
library(foreign)
library(tsModel)
library(lmtest)
library(Epi)
library(splines)
library(vcd)
library(mgcv)
library(data.table)
library(viridis)
library(sf)
library(clubSandwich)

###############################
# ZIP level quarterly analyses#
###############################

# HyADS and SO2 ZIP estimates
exp <- read_csv("louisville_updated_hyads_qtr.csv")

#Quarterly environmental variables
qso2 <- read.xlsx("Zip_level_assigned_quarterly_so2_weather_simplified1216.xlsx")

#Quarterly asthma
dq <- read_csv("asthma_all_zip.csv")
#Smaller version with  just yr-q, ZIP, and asthma hospitalization/ED visit count
dqwide <- dq[,3:5]
dqwide$zip <- as.factor(dqwide$zip)

#Time-varying demographics: total population, proportion poverty, education <HS, unemployment, uninsured, black race
demos <- read_csv("acs_demographics_zip.csv")

#Detrended residuals of the outcome from Ray Catalano
asthma_r <- read_csv("asthma_resid2.csv")

#Join exposure and asthma data
exp_dat <- left_join(exp, dq, by = c("yrq" = "yrq", "zip" = "zip"))
exp_dat <- dplyr::select(exp_dat, exp_qtr, zip, uid, year, yrq, qtr, count)

# Create wide dataset (each plant unit = 1 column with HyADS as value, each row = ZIP, year, quarter combination)
exp_dat$uidf <- factor(exp_dat$uid, levels = c("1363-4", "1363-5", "1363-6", "983-1",  '983-2' ,   "983-3" ,   "983-4"  , "983-5", "983-6",
                                               "1364-1","1364-2", "1364-3", "1364-4", "6166-MB1", "6166-MB2"),
                       labels = c("Cane Run, Unit 4", "Cane Run, Unit 5", "Cane Run, Unit 6", "Clifty Creek, Unit 1", 
                                  "Clifty Creek, Unit 2","Clifty Creek, Unit 3","Clifty Creek, Unit 4","Clifty Creek, Unit 5","Clifty Creek, Unit 6",
                                  "Mill Creek, Unit 1", "Mill Creek, Unit 2", "Mill Creek, Unit 3", "Mill Creek, Unit 4", "Rockport, Unit 1", "Rockport, Unit 2"))
exp_dat <- dplyr::select(exp_dat, -uid)

exp_dat2 <- spread(exp_dat, uidf, exp_qtr) %>% setNames( c("zip",  "year", "yrq","qtr", "asthma_count", "plant_1363_4", "plant_1363_5", "plant_1363_6",
                                                           "plant_983_1", "plant_983_2", "plant_983_3", "plant_983_4", "plant_983_5", "plant_983_6",
                                                           "plant_1364_1", "plant_1364_2", "plant_1364_3",  "plant_1364_4", "plant_6166_MB1", "plant_6166_MB2" ) )

#Join demographics
exp_dat2 <- left_join(exp_dat2, demos, by = c("zip" = "zcta5a", "year"="year"))

#Add weather and so2 data
exp_dat2 <- left_join(exp_dat2, qso2, by = c("zip" = "Zip2010", "qtr" = "qtrSeq", "year" = "Yr"))

#Create composite HyADS metrics (called tot_exp)
exp_dat2 <- exp_dat2 %>% mutate(tot_exp = plant_1363_4 + plant_1363_5 + plant_1363_6 + plant_1364_1 + plant_1364_2 + plant_1364_3 + plant_1364_4 +
                                  plant_6166_MB1 + plant_6166_MB2)

#Composite metric by power plant
exp_dat3 <- exp_dat2 %>% mutate(exp_1363 = plant_1363_4 + plant_1363_5 + plant_1363_6, exp_983 = plant_983_1 + plant_983_2 + plant_983_3 + plant_983_4 +
                                  plant_983_5 + plant_983_6, exp_1364 = plant_1364_1 + plant_1364_2 + plant_1364_3 + plant_1364_4, 
                                exp_6166 = plant_6166_MB1 + plant_6166_MB2)

#Read in KY ZIP codes
kyzip <- st_read("tl_2010_21_zcta510.shp")
summary(kyzip)

#Subset to jefferson county zips
jeffzip <- kyzip %>% filter(ZCTA5CE10==40023 | ZCTA5CE10==40025| ZCTA5CE10==40059 | ZCTA5CE10==40118 | 
                              ZCTA5CE10==40177 |ZCTA5CE10==40202 |ZCTA5CE10==40203 |ZCTA5CE10==40204 |
                              ZCTA5CE10==40205 |ZCTA5CE10==40206 |ZCTA5CE10==40207 |ZCTA5CE10==40208 |
                              ZCTA5CE10==40209 |ZCTA5CE10==40210 |ZCTA5CE10==40211 |ZCTA5CE10==40212 | 
                              ZCTA5CE10==40213 |ZCTA5CE10==40214  |ZCTA5CE10==40215 |ZCTA5CE10==40216|
                              ZCTA5CE10==40217 |ZCTA5CE10==40218 |ZCTA5CE10==40219  |ZCTA5CE10==40220|
                              ZCTA5CE10==40222 |ZCTA5CE10==40223 |ZCTA5CE10==40225  |ZCTA5CE10==40228|
                              ZCTA5CE10==40229 |ZCTA5CE10==40241 |ZCTA5CE10==40242  |ZCTA5CE10==40243|
                              ZCTA5CE10==40245 |ZCTA5CE10==40258  |ZCTA5CE10==40272 |ZCTA5CE10==40291|ZCTA5CE10==40299)
exp_dat$zip <- as.factor(exp_dat$zip)
#Add data to Jefferson zips
jeffzip <- left_join(jeffzip, exp_dat, by=c("ZCTA5CE10" = "zip"))

#Figure 2 - HyADS exposures by ZIP over time
#Add asthma counts to ZIP codes
jeffzipb <- left_join(jeffzip, dqwide, by=c("ZCTA5CE10" = "zip", "yrq" = "yrq"))
tot_exp_map <- exp_dat2 %>% dplyr::select(yrq, zip, tot_exp)
tot_exp_map$zip <- as.character(tot_exp_map$zip)
jeffzipc <- left_join(jeffzipb, tot_exp_map, by=c("ZCTA5CE10" = "zip", "yrq" = "yrq"))

#Plot deciles
quantile(jeffzipc$tot_exp, prob = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8,0.9,1))
jeffzipc$tot_exp10 <- ntile(jeffzipc$tot_exp, 10)

tiff(filename = "Fig2_Tot_HyADS_QTR_Map_Decile.tiff", type = "cairo" ,  res=300, height = 6, width = 5, units = "in")
jeffzipc %>%
  ggplot(aes(fill = tot_exp10)) + 
  geom_sf(color = "#d9d9d9", lwd = 0.1) +
  scale_fill_viridis("Total \nHyADS\ndeciles", option="magma", breaks = c(2, 4, 6, 8, 10)) +
  facet_wrap(~yrq, ncol= 4) + theme_minimal(base_size = 14) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())
dev.off()

#Figure 3 - Plot of count of hospitalizations by ZIP over time
tiff(filename = "Figure_3_Zip_Map_Asthma_Count.tiff", type = "cairo" ,  res=300, height = 6, width = 5, units = "in")
jeffzipb %>%
  ggplot(aes(fill = count)) + 
  geom_sf(color = "white", lwd = 0.1) +
  scale_fill_viridis("Count", option="cividis") +
  facet_wrap(~yrq, ncol= 4) + theme_minimal(base_size = 14) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())
dev.off()

#Add a variable, time and time_since, that counts up from 1 in quarter 1, 2012 to 20 in quarter 4, 2016
exp_dat4 <- exp_dat3 %>%
  group_by(zip) %>%
  mutate(time = seq(n()))

exp_dat4 <- exp_dat4 %>% mutate(time_since = dplyr::case_when((year==2012 & qtr==1) ~ 1,
                                                              (year==2012 & qtr==2) ~ 2,
                                                              (year==2012 & qtr==3) ~ 3,
                                                              (year==2012 & qtr==4) ~ 4,
                                                              (year==2013 & qtr==1) ~ 5,
                                                              (year==2013 & qtr==2) ~ 6,
                                                              (year==2013 & qtr==3) ~ 7,
                                                              (year==2013 & qtr==4) ~ 8,
                                                              (year==2014 & qtr==1) ~ 9,
                                                              (year==2014 & qtr==2) ~ 10,
                                                              (year==2014 & qtr==3) ~ 11,
                                                              (year==2014 & qtr==4) ~ 12,
                                                              (year==2015 & qtr==1) ~ 13,
                                                              (year==2015 & qtr==2) ~ 14,
                                                              (year==2015 & qtr==3) ~ 15,
                                                              (year==2015 & qtr==4) ~ 16,
                                                              (year==2016 & qtr==1) ~ 17,
                                                              (year==2016 & qtr==2) ~ 18,
                                                              (year==2016 & qtr==3) ~ 19,
                                                              (year==2016 & qtr==4) ~ 20))

#exp_dat4 <- exp_dat4 %>% filter(year<2017)

#Add asthma residuals 
exp_dat4 <- left_join(exp_dat4, asthma_r)

#Rescale exposures, divide by 1000
divide.1000 <- function (x) {
  x/1000
}
exp_dat4$tot_exp <- divide.1000(exp_dat4$tot_exp)
exp_dat4$exp_1363 <- divide.1000(exp_dat4$exp_1363)
exp_dat4$exp_1364 <- divide.1000(exp_dat4$exp_1364)
exp_dat4$exp_6166 <- divide.1000(exp_dat4$exp_6166)
exp_dat4$tot_exp <- divide.1000(exp_dat4$tot_exp)
exp_dat4$exp_983 <- divide.1000(exp_dat4$exp_983)

#Scale HyADS exposure by IQR
summary(exp_dat4$tot_exp)
exp_dat4$tot_exp_iqr <- exp_dat4$tot_exp/(10.7293- 4.6772)

#Figure 4
#Clifty Creek, compare time = 4 to time = 7
dat_2013_1 <- exp_dat5 %>% filter(time==4 | time==7)
dat_2013_1 <- dplyr::select(dat_2013_1, asthma_r, time, zip, exp_983)
pre <- dat_2013_1 %>% filter(time==4)
quantile(pre$exp_983, c(0.33, 0.67))
pre <- pre %>% mutate(tert = dplyr::case_when(exp_983 < 4.320742 ~ 1,
                                              exp_983 >= 4.320742 & exp_983 < 5.302947  ~ 2,
                                              exp_983 >= 5.302947  ~3))
pre <- dplyr::select(pre, tert, zip)
dat_2013_1 <- 
dat_2013_1 <- left_join(dat_2013_1, pre, by = c("zip" = "zip"))
dat_2013_1 <- dat_2013_1 %>% group_by(time, tert) %>% summarize(n = n(), mean = mean(asthma_r), sd = sd(asthma_r), p50 = quantile(asthma_r, 0.5), p25=quantile(asthma_r, 0.25),p75=quantile(asthma_r, 0.75))
dat_2013_1
dat_2013_1$x <- c(1,1.2)

ggplot(dat_2013_1, aes(x = factor(tert), y = mean, ymin = mean-sd, ymax = mean+sd, shape = factor(time))) + geom_pointrange(position = position_dodge(width = .3), size=1.05) + 
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_minimal(base_size = 16)+
  scale_shape_manual("", labels=c("2012-4", "2013-3"), values=c(16,17,16,17,16,17)) +
  scale_x_discrete("Clifty Creek Facility HyADS", breaks=c(1,2,3), labels=c("Tertile 3\nHigh HyADS","Tertile 2\nMedium HyADS","Tertile 1\nLow HyADS"))+
  scale_y_continuous("Asthma Hospitalizations/ED Visits\n Relative to Expected", breaks = c(-8,-6,-4,-2,0,2,4,6,8,10,12,14,16)) +
  xlab("") +
  theme(legend.position = c(.9,0.95))

#2014-4 Mill Creek (1364) scrubber
#Want to compare2014-3 to 2015-1; 11 to 13
#HyADS plot
dat_2014_4 <- exp_dat5 %>% filter(time==11 | time==13)
dat_2014_4 <- dplyr::select(dat_2014_4, asthma_r, time, zip, exp_1364)
pre <- dat_2014_4 %>% filter(time==11)
quantile(pre$exp_1364, c(0.33, 0.67))
pre <- pre %>% mutate(tert = dplyr::case_when(exp_1364 < 9.085641  ~ 1,
                                              exp_1364 >= 9.085641  & exp_1364 < 10.212114 ~ 2,
                                              exp_1364 >= 10.212114 ~3))
pre <- dplyr::select(pre, tert, zip)
dat_2014_4 <- left_join(dat_2014_4, pre, by = c("zip" = "zip"))
dat_2014_4$tert <- factor(dat_2014_4$tert, levels = c(3,2,1))
dat_2014_4 <- dat_2014_4 %>% group_by(time, tert) %>% summarize(n = n(), mean = mean(asthma_r), sd = sd(asthma_r), p50 = quantile(asthma_r, 0.5), p25=quantile(asthma_r, 0.25),p75=quantile(asthma_r, 0.75))
dat_2014_4
dat_2014_4$x <- c(1,1.2)

ggplot(dat_2014_4, aes(x = factor(tert), y = mean, ymin = mean-sd, ymax = mean+sd, shape = factor(time))) + geom_pointrange(position = position_dodge(width = .3), size=1.05) + 
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_minimal(base_size = 16)+
  scale_shape_manual("", labels=c("2014-3", "2015-1"), values=c(16,17,16,17,16,17)) +
  scale_x_discrete("Mill Creek Facility HyADS", breaks=c(3,2,1), labels=c("Tertile 3\nHigh HyADS","Tertile 2\nMedium HyADS","Tertile 1\nLow HyADS"))+
  scale_y_continuous("Asthma Hospitalizations/ED Visits\n Relative to Expected", breaks = c(-6,-4,-2,0,2,4,6,8,10)) +
  xlab("") +
  theme(legend.position = c(.9,0.95))


#2015-2 (time = 14), Cane Run (1363 retires), Mill Creek (1364 2 scrubbers), Rockport (6166 1 scrubber)
#Getting tertiles from sum of all these HyADS exposures
exp_dat5hy <-  filter(exp_dat5, time==13)
dat_2015_2hy <- exp_dat5hy %>% mutate(exp_2015_2 = plant_1363_4 + plant_1363_5 + plant_1363_6 + plant_1364_1 + plant_1364_2 + plant_6166_MB1 + plant_6166_MB2)
quantile(dat_2015_2hy$exp_2015_2, probs = seq(0, 1, 0.33))
dat_2015_2hy <-  dat_2015_2hy %>% mutate(tert = dplyr:: case_when(exp_2015_2 < 4806.625 ~ 1, exp_2015_2 >= 4806.625 & exp_2015_2 <5012.708 ~2, exp_2015_2 >=5012.708 ~ 3))       
dat_2015_2hy <- dplyr::select(dat_2015_2hy, tert, zip)

#Want to compare 2015-1 to 2015-3 HyADS
dat_2015_2 <- exp_dat5 %>% filter(time==13 | time==15)
dat_2015_2 <- dat_2015_2 %>% mutate(exp_2015_2 = plant_1363_4 + plant_1363_5 + plant_1363_6 + plant_1364_1 + plant_1364_2 + plant_6166_MB1 + plant_6166_MB2)
dat_2015_2 <- dplyr::select(dat_2015_2, asthma_r, time, zip, tot_exp)
dat_2015_2 <- left_join(dat_2015_2, dat_2015_2hy, by = c("zip"="zip"))
dat_2015_2_hyads <- dat_2015_2 %>% group_by(tert, time) %>% dplyr::summarize(n = n(), mean = mean(tot_exp), sd = sd(tot_exp), p50 = quantile(tot_exp, 0.5), p25=quantile(tot_exp, 0.25),p75=quantile(tot_exp, 0.75))
dat_2015_2_hyads$tert <- factor(dat_2015_2_hyads$tert, levels = c(1,2,3))
dat_2015_2_hyads$x <- c(1,1.4)

ggplot(dat_2015_2_hyads, aes(x = tert, y = mean, ymin = mean-sd, ymax = mean+sd, shape = factor(time))) + geom_pointrange(position = position_dodge(width = .3), size=1.05) + 
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_minimal(base_size = 16)+
  scale_shape_manual("", labels=c("2015-1", "2015-3"), values=c(16,17,16,17,16,17)) +
  scale_x_discrete("Cane Run, Mill Creek, and\nRockport Facility HyADS", breaks=c(1,2,3), labels=c("Tertile 3\nHigh HyADS","Tertile 2\nMedium HyADS","Tertile 1\nLow HyADS"))+
  scale_y_continuous("Asthma Hospitalizations/ED Visits\n Relative to Expected") +
  xlab("") +
  theme(legend.position = c(.9,0.95))


#2016-2, Mill Creek 1364
#Want to compare 2016-1 to 2016-3 (17 to 19)
#HyADS plot
dat_2016_2 <- exp_dat5 %>% filter(time==17 | time==19)
dat_2016_2 <- dplyr::select(dat_2016_2, asthma_r, time, zip, plant_1364_3)
pre <- dat_2016_2 %>% filter(time==17)
quantile(pre$plant_1364_3, c(0.33, 0.67))
pre <- pre %>% mutate(tert = dplyr::case_when(plant_1364_3 < 775.6733  ~ 3,
                                              plant_1364_3 >= 775.6733  & plant_1364_3 < 839.9140 ~ 2,
                                              plant_1364_3 >= 839.9140 ~1))
pre <- dplyr::select(pre, tert, zip)
dat_2016_2 <- left_join(dat_2016_2, pre, by = c("zip" = "zip"))
dat_2016_2$tert <- factor(dat_2016_2$tert, levels = c(3,2,1))
dat_2016_2 <- dat_2016_2 %>% group_by(time, tert) %>% summarize(n = n(), mean = mean(asthma_r), sd = sd(asthma_r), p50 = quantile(asthma_r, 0.5), p25=quantile(asthma_r, 0.25),p75=quantile(asthma_r, 0.75))
dat_2016_2
dat_2016_2$x <- c(1,1.2)

ggplot(dat_2016_2, aes(x = tert, y = mean, ymin = mean-sd, ymax = mean+sd, shape = factor(time))) + geom_pointrange(position = position_dodge(width = .3), size=1.05) + 
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_minimal(base_size = 16)+
  scale_shape_manual("", labels=c("2016-1", "2016-3"), values=c(16,17,16,17,16,17)) +
  scale_x_discrete("Mill Creek Facility HyADS", breaks=c("3", "2", "1"), labels=c("Tertile 3\nHigh HyADS","Tertile 2\nMedium HyADS","Tertile 1\nLow HyADS"))+
  scale_y_continuous("Asthma Hospitalizations/ED Visits\n Relative to Expected",  breaks = c(-10,-8,-6,-4,-2,0,2,4,6,8)) +
  xlab("") +
  theme(legend.position = c(.9,0.95))


#Results presented in text and Supplementary Table 3
#Models looking at association between IQR change in HyADS and asthma + 1 unit change in SO2 and asthma
#Unadjusted
mod3 <- lm(asthma_r ~  tot_exp_iqr + totpop + time_since, dat=exp_dat4)
coef_test(mod3, vcov = "CR2", 
          cluster = exp_dat4$zip, test = "Satterthwaite")[1:2,]
summary(mod3)
confint(mod3) # 95% CI for the coefficients

#Adjusted
mod3b <- lm(asthma_r ~  tot_exp_iqr + totpop + black_p + pov_p + unempl_p + Temp_Quarter + WD_Quarter + Pres_Quarter + RH_Quarter + WS_Quarter + time_since, dat=exp_dat4)
summary(mod3b)
coef_test(mod3b, vcov = "CR2", 
          cluster = exp_dat4$zip, test = "Satterthwaite")[1:2,]
confint(mod3b) # 95% CI for the coefficients

mod4b <- lm(asthma_r ~  Quarter_SO2_Zip + totpop + black_p + pov_p + unempl_p + Temp_Quarter + WD_Quarter + Pres_Quarter + RH_Quarter + WS_Quarter + time_since, dat=exp_dat4)
summary(mod4b)
coef_test(mod4b, vcov = "CR2", 
          cluster = exp_dat4$zip, test = "Satterthwaite")[1:2,]
confint(mod4b) # 95% CI for the coefficients

mod4 <- lm(asthma_r ~  Quarter_SO2_Zip + totpop + time_since, dat=exp_dat4)
summary(mod4)
coef_test(mod4, vcov = "CR2", 
          cluster = exp_dat4$zip, test = "Satterthwaite")[1:2,]
confint(mod4) # 95% CI for the coefficients

mod4b <- lm(asthma_r ~  Quarter_SO2_Zip + totpop + black_p + pov_p + unempl_p + Temp_Quarter  + Pres_Quarter + RH_Quarter + WS_Quarter + time_since , dat=exp_dat4)
summary(mod4b)
coef_test(mod4b, vcov = "CR2", 
          cluster = exp_dat4$zip, test = "Satterthwaite")[1:2,]
confint(mod4b) 

######################################
# Individual level quarterly analyses#
######################################

###SET SEED###
set.seed(63018)

################################
###INFORMATION ON POWER PLANTS##
################################
#Cane Run retired on May 23, 2015
#Mill Creek added one scrubber on unit 4: December 9, 2014; Unit 1&2 May 27, 2015; Unit 3 April 16, 2016

########################################################
####INDIVIDUAL HEALTH DATA WITH DISTANCES TO PLANTS#####
########################################################

dati <- read.xlsx("Jefferson_events_assigned_so2_weather_pollen_distCRun-MCreek.xlsx")
head(dati)

##GENERATE MONTH AND YEAR IN READABLE FORMAT##
dati$Date <- as.Date(dati$Date, origin = "1900-01-01")
dati$Datec <- dati$Date-as.Date("2012-05-25")
dati$month  <- as.factor(months(dati$Date))
dati$year   <- as.factor(format(dati$Date, format="%Y") )
dati$dow    <- as.factor(weekdays(dati$Date))
dati$stratum <- as.factor(dati$year:dati$month:dati$dow)
dati$moyr <- as.factor(dati$year:dati$month)
dati$date2 <- as.numeric(dati$Datec)

#Identify people with observations in the year before and after June 8, 2016
dati4 <- dati %>% mutate(coal_exp4 = dplyr::case_when((date2 > (1475-365) & date2 <= 1475 ~ 1),
                                                      date2 < (1475+365) & date2 >1475 ~ 2))
dati4$coal_exp4[is.na(dati4$coal_exp4)] <- 0
table(dati4$coal_exp4)
d4 <- dati4 %>% dplyr::select(uid, coal_exp4)
d4 <- d4 %>% group_by(uid) %>%
  dplyr::summarize(n_unique = n_distinct(coal_exp4))
table(d4$n_unique) #207 people have values in the year before and after 

d4 <- d4 %>% mutate(keep = ifelse(n_unique==2, 1, 0))
head(d4)
#Keep just IDs that have data before and after June 8, 2016
d4 <- filter(d4, keep==1)
ids <- d4$uid
ids <- tibble(ids)

#Save this dataset
save(ids, file = "Louisville_IDs_207.Rdata")

#Add exposure data to these ids
dati <- left_join(ids, dati, by = c("ids" = "uid"))

#Distance to Mill Creek from event
summary(dati$distMCrk_Home)
#Quartile 1 versus 2-4 
dati <- dati %>% mutate(close = ifelse(distMCrk_Home<21669.6, 1,0))
#Quartile 1, 2, 3, 4
dati <- dati %>% mutate(close4 = case_when(distMCrk_Home<22311.4  ~ 0,
                                           distMCrk_Home>=22311.4  & distMCrk_Home < 26565.3 ~ 1,
                                           distMCrk_Home>=26565.3 & distMCrk_Home < 32321.1 ~ 2,
                                           distMCrk_Home >= 32321.1 ~ 3))

#Mill Creek Closure Indicator
dati <- dati %>% 
  mutate(scrub4 = ifelse(date2 > 1475, 1, 0))

#Summary of rescue inhaler use
dati %>% dplyr::group_by(ids) %>% dplyr::summarize(mean=mean(Rscu), sum=sum(Rscu), q1=quantile(Rscu, 0.25), q3=quantile(Rscu, 0.75))

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

#Plot inhaler use over time, restrict to year before and after 2016 event
dati2 <- filter(dati, Date > as.Date("2015-06-01") & Date <= as.Date("2017-06-30"))
grp <- dati2 %>% 
  group_by(Date) %>% 
  summarise(Rscu = mean(Rscu, na.rm=T))
grp
ggplot(dati2, aes(Date, Rscu)) + 
  geom_line(data=grp, aes(group=1),size=1, color = "black") + 
  xlab("") +
  geom_smooth(data=grp, aes(group=1),size=1, color = "#6b4596ff", se=F)+
  scale_color_manual("", values = "#6b4596ff") + scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous("Mean Daily Rescue Inhaler Use", breaks=c(0, 2, 4))+
  theme_minimal(base_size = 14) +
  geom_vline(xintercept = as.numeric(as.Date("2016-06-08")), linetype=3) +
  annotate("text", label ="Smoothed mean", x = as.Date(c("2017-03-01")), y = 2.3, color = "#6b4596ff", size= 4, fontface=2) + 
  annotate("text", label ="Mill Creek \nscrubber", hjust = 0,
           x = as.Date(c("2016-06-15")), y = 4, color = "black", size= 4, fontface=2) + 
  theme(legend.position="none", axis.text.x = element_text(angle = 90, hjust = .5, vjust = 0.5))

#Create centered and centered-squared SO2 exposures
dati$so2c <- scale(dati$SO2PPB, scale=FALSE)
dati$so2c2 <- dati$so2c*dati$so2c
dati$so2c3 <- dati$so2c*dati$so2c*dati$so2c

#Unadjusted gnm model, linear, squared, cubic
mod1a <- gnm(Rscu ~ so2c  + date2 + harmonic(month, 2, 12), data=dati, family="quasipoisson", eliminate=factor(ids))
summary(mod1a)
plot(mod1a)
summary(mod1a)$dispersion
mod1b <- gnm(Rscu ~ so2c + so2c2  + date2 + harmonic(month, 2, 12), data=dati, family="quasipoisson", eliminate=factor(ids))
summary(mod1b)
summary(mod1b)$dispersion
mod1c <- gnm(Rscu ~ so2c + so2c2 + so2c3 + date2 + harmonic(month, 2, 12), data=dati, family="quasipoisson", eliminate=factor(ids))
summary(mod1c)
summary(mod1c)$dispersion

### compare models
anova(mod1a, mod1b, mod1c , test = "Chisq")

#Adjusted gnm model
mod2b <- gnm(Rscu ~ so2c + so2c2 + date2 + harmonic(month, 2, 12) + ns(RelativeHumidity,2) + ns(WindSpeed,2)  + 
              ns(Mold,2) + Weed + Tree + Grass , data=dati, family="quasipoisson", eliminate=factor(ids))
summary(mod2b)
summary(mod2b)$dispersion
predictedSABA <- predict(mod2b, type = "response", se.fit=T)
dati$predictedSABA <- predictedSABA$fit
dati$predictedSABAse <- predictedSABA$se.fit
dati$predictedSABAhigh <- dati$predictedSABA + 1.96*dati$predictedSABAse
dati$predictedSABAlow <- dati$predictedSABA - 1.96*dati$predictedSABAse

#Plot predicted values
p <- ggplot(da, aes(x = SO2PPB, y = predictedSABA)) 
p + geom_line()+  theme_minimal(base_size = 14) +
  geom_line(aes(x = SO2PPB, y = predictedSABAlow), linetype="dashed") +
  geom_line(aes(x = SO2PPB, y = predictedSABAhigh), linetype="dashed") +
  scale_x_continuous(expression(paste(SO[2], " concentration (ppb)")), breaks=c(0, 2,  4,  6,  8, 10, 12, 14, 16, 18)) +
  scale_y_continuous("Predicted SABA Use Per Person Per Day", breaks=c(0,1,2,3,4), labels=c(0,1,2,3,4))

#Discontinuity at Mill Creek scrubber installation (June 2016)
#scrub 4 is indicator variable for before or after installation
mod3 <- gnm(Rscu ~ scrub4 + date2 +  ns(DryBulbFarenheit, 2) + ns(RelativeHumidity,2) + ns(WindSpeed,2)  + ns(Mold,2) + Weed + Tree + Grass +
           harmonic(month, 2, 12), data=dati, family="quasipoisson", 
             eliminate=factor(ids))
summary(mod3)
confint(mod3)
termplot(mod3)

predictedSABAint<-predict(mod3, type = "response",se.fit = TRUE)
dati$predictedSABAint <- predictedSABAint$fit
dati$predictedSABAintse <- predictedSABAint$se.fit
dati$predictedSABAinthigh <- dati$predictedSABAint + 1.96*dati$predictedSABAintse
dati$predictedSABAintlow <- dati$predictedSABAint - 1.96*dati$predictedSABAintse

cols <- c("#eb8055ff", "#13306dff")
q <- ggplot(data = dati, aes(x = date2, y = predictedSABAint))
q + geom_line(aes(color = as.factor(scrub4), group = as.factor(scrub4))) + theme_minimal(base_size = 14)+
  geom_line(aes(x = date2, y = predictedSABAinthigh, color = as.factor(scrub4), group = as.factor(scrub4)), linetype="dashed") +
  geom_line(aes(x = date2, y = predictedSABAintlow, color = as.factor(scrub4), group = as.factor(scrub4)),  linetype="dashed") +
  scale_x_continuous("", breaks=c(221, 221+365.25, 221+(365.25*2), 1316, 1682), labels=c("2013-1", "2014-1", "2015-1", "2016-1", "2017-1")) +
  geom_vline(xintercept = 1475, linetype=3) +
  scale_y_continuous("Predicted SABA Use Per Person Per Day") +
  scale_color_manual("", labels=c("Pre", "Post"), values = cols) +
  annotate("text", label ="Mill Creek \nscrubber", hjust = 0,
           x = 1490, y = 1.39, color = "black", size= 4.5, fontface=2) + 
  theme(axis.text.x = element_text(angle = 90, vjust=.5), legend.position="top")

#Sensitivity analyses with binary specification of SABA use
#Indicators of SABA use
#Inhaler use greater than 3 in a single day
dati$Rscu_4 <- ifelse(dati$Rscu > 3, 1, 0)
#Variable = 0 if no use, = 1 if use
dati$Rscu_bi <- ifelse(dati$Rscu > 0, 1, 0)

#Odds of any rescue inhaler use after (daily)
#Unadjusted
mod4 <- gnm(Rscu_bi ~ scrub4 + date2 +  harmonic(month, 2, 12), data=dati, family="binomial",
              eliminate=factor(ids))
summary(mod4)
mod4cof<-exp(coef(summary(mod4)))
mod4int_tot<-exp(confint(mod4))

mod5 <- gnm(Rscu_4 ~ scrub4 + date2 +  harmonic(month, 2, 12), data=dati, family="binomial",
              eliminate=factor(ids))
summary(mod5)
mod5cof<-exp(coef(summary(mod5)))
mod5int_tot<-exp(confint(mod5))

#Adjusted
mod4b <- gnm(Rscu_bi ~ scrub4 + date2 + ns(DryBulbFarenheit, 2) + ns(RelativeHumidity,2) + ns(WindSpeed,2)  + ns(Mold,2) + Weed + Tree + Grass + harmonic(month, 2, 12), data=dati, family="binomial",
               eliminate=factor(ids))
summary(mod4b)
mod4bcof<-exp(coef(summary(mod4b)))
mod4bint_tot<-exp(confint(mod4b))

mod5b <- gnm(Rscu_4 ~ scrub4 + date2 + ns(DryBulbFarenheit, 2) + ns(RelativeHumidity,2) + ns(WindSpeed,2)  + ns(Mold,2) + Weed + Tree + Grass + harmonic(month, 2, 12), data=dati, family="binomial",
               eliminate=factor(ids))
summary(mod5b)
termplot(mod5b)
mod5bcof<-exp(coef(summary(mod5b)))
mod5bint_tot<-exp(confint(mod5b))