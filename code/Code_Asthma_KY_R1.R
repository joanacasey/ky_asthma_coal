##################################################################################################################
# Code used to conduct analyses for "Reduced coal-fired power plant emissions related to improved                #
# community asthma-related healthcare utilization and individual symptoms in Louisville, Kentucky, USA"          #
# Code developed primarily by Joan Casey and also by Andreas Neophytou                                           #
# Updated Oct 9, 2019                                                                                            #
##################################################################################################################
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

# HyADS ZIP estimates
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

#############################################
#Supplementary Figure 9, was Figure 2 in R0 #
#HyADS exposures by ZIP over time           #
#############################################

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

####################################################
#Now Figure 2, was Figure 3 in R0                  #
#Plot of count of hospitalizations by ZIP over time#
####################################################
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