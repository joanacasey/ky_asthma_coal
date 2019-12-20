##################################################################################################################
# Code used to conduct ZIP code-level analyses for "Reduced coal-fired power plant emissions related to improved # 
# community asthma-related healthcare utilization and individual symptoms in Louisville, Kentucky, USA"          #
# Code developed primarily by Joan Casey and also by Andreas Neophytou                                           #
# Updated December 20, 2019                                                                                      #
#Quarterly health outcomes in Louisville related to the three main power plants that retire/install scrubbers    #
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
library(stargazer)
library(clusterSEs)
library(ggpubr)

#Bring in quarterly exposure data from the three power plants
exp <- read_csv("louisville_updated_hyads_qtr.csv")

#Quarterly environmental variables
qso2 <- read.xlsx("Zip_level_assigned_quarterly_so2_weather_simplified1216.xlsx")

#Time-varying demographics: total population, proportion poverty, education <HS, unemployment, uninsured, black race/ethnicity
demos <- read_csv("acs_demographics_zip.csv")

#Restrict to the 35 zip codes for which we have exposure data / have populations
#Summary of events by zip quarterly
dq <- read_csv("Quarterly/asthma_all_zip.csv")
dqwide <- dq[,3:5]
dqwide$zip <- as.factor(dqwide$zip)

#Add exposure to zip, quarterly counts
exp_dat <- left_join(exp, dq, by = c("yrq" = "yrq", "zip" = "zip"))
dim(exp_dat)
head(exp_dat)
n_distinct(exp_dat$zip) #only 35 zip codes

exp_dat <- left_join(exp, dq, by = c("yrq" = "yrq", "zip" = "zip"))
exp_dat2 <- spread(exp_dat, uid, exp_qtr) %>% setNames( c("zip", "yrq", "year", "qtr", "asthma_count", "plant_1363_4", "plant_1363_5", "plant_1363_6",
                                                          "plant_983_1", "plant_983_2", "plant_983_3", "plant_983_4", "plant_983_5", "plant_983_6",
                                                          "plant_1364_1", "plant_1364_2", "plant_1364_3",  "plant_1364_4", "plant_6166_MB1", "plant_6166_MB2" ) )
dq <- dq[,1:5]
exp_dat <- left_join(exp, dq, by = c("yrq" = "yrq", "zip" = "zip"))
head(exp_dat)
exp_dat$uidf <- factor(exp_dat$uid, levels = c("1363-4", "1363-5", "1363-6", "983-1",  '983-2' ,   "983-3" ,   "983-4"  , "983-5", "983-6",
                                               "1364-1","1364-2", "1364-3", "1364-4", "6166-MB1", "6166-MB2"),
                       labels = c("Cane Run, Unit 4", "Cane Run, Unit 5", "Cane Run, Unit 6", "Clifty Creek, Unit 1", 
                                  "Clifty Creek, Unit 2","Clifty Creek, Unit 3","Clifty Creek, Unit 4","Clifty Creek, Unit 5","Clifty Creek, Unit 6",
                                  "Mill Creek, Unit 1", "Mill Creek, Unit 2", "Mill Creek, Unit 3", "Mill Creek, Unit 4", "Rockport, Unit 1", "Rockport, Unit 2"))
exp_dat <- dplyr::select(exp_dat, -uid)

exp_dat2 <- spread(exp_dat, uidf, exp_qtr) %>% setNames( c("zip", "yrq", "year", "qtr", "asthma_count", "plant_1363_4", "plant_1363_5", "plant_1363_6",
                                                           "plant_983_1", "plant_983_2", "plant_983_3", "plant_983_4", "plant_983_5", "plant_983_6",
                                                           "plant_1364_1", "plant_1364_2", "plant_1364_3",  "plant_1364_4", "plant_6166_MB1", "plant_6166_MB2" ) )

#ZIP code as numeric
demos$zcta5a <- as.numeric(demos$zcta5a)

#Remove ZIP codes with no population
exp_dat2 <- exp_dat2 %>% dplyr::filter(zip != 40177 & zip != 40225)

#Join demographics to exposure data
exp_dat2 <- left_join(exp_dat2, demos, by = c("zip" = "zcta5a", "year"="year"))

#Add weather data
exp_dat2 <- left_join(exp_dat2, qso2, by = c("zip" = "Zip2010", "year" = "Yr", "qtr" ="qtrSeq"))

#Total HyADS metric across all plants
exp_dat2 <- exp_dat2 %>% mutate(tot_exp = plant_1363_4 + plant_1363_5 + plant_1363_6 + plant_1364_1 + plant_1364_2 + plant_1364_3 + plant_1364_4 +
                                  plant_6166_MB1 + plant_6166_MB2)

#Composite metric by power plant
exp_dat3 <- exp_dat2 %>% mutate(exp_1363 = plant_1363_4 + plant_1363_5 + plant_1363_6, exp_983 = plant_983_1 + plant_983_2 + plant_983_3 + plant_983_4 +
                                  plant_983_5 + plant_983_6, exp_1364 = plant_1364_1 + plant_1364_2 + plant_1364_3 + plant_1364_4, 
                                exp_6166 = plant_6166_MB1 + plant_6166_MB2)

exp_dat4 <- exp_dat3 %>%
  group_by(zip) %>%
  mutate(time = seq(n()))

#Rescale exposures, divide by 1000
divide.1000 <- function (x) {
  x/1000
}
exp_dat4$exp_1363 <- divide.1000(exp_dat4$exp_1363)
exp_dat4$exp_1364 <- divide.1000(exp_dat4$exp_1364)
exp_dat4$exp_6166 <- divide.1000(exp_dat4$exp_6166)
exp_dat4$tot_exp <- divide.1000(exp_dat4$tot_exp)
exp_dat4$exp_983 <- divide.1000(exp_dat4$exp_983)

#Factor variable for quarters 
exp_dat4$qtr.f <- factor(exp_dat4$qtr, levels = c(2,1,3,4))

#Factor variable for ZIPs
exp_dat4$zip.f <- factor(exp_dat4$zip)
exp_dat4$zip.f<- relevel(exp_dat4$zip.f, ref = "40216")

#Add fixed effects for on/off switches
#Indicators for power plant scrubbers/closures
#Mill Creek added one scrubber on unit 4: December 9, 2014; Unit 1&2 May 27, 2015; Unit 3 June 8, 2016; EIA ID = 1364
#Cane Run, EIA ID = 1363, Unit 6 retired on March 30, 2015 and Units 4 & 5 on May 23, 2015
#6166 scrubbed May 2015
#This is March 20, 2013; Quarter 6
exp_dat5 <- exp_dat4 %>%
  mutate(scrub0a = ifelse(time > 5, 1, 0))

exp_dat5 <- exp_dat5 %>%
  mutate(scrub0b = ifelse(time > 6, 1, 0))

#Or just a scrubber for Quarter 6
exp_dat5 <- exp_dat5 %>%
  mutate(scrub0 = ifelse(time > 6, 1, 0))

#This is December 9, 2014; Quarter 12
exp_dat5 <- exp_dat5 %>%
  mutate(scrub1 = ifelse(time > 12, 1, 0))

#This is March/May 2015, Quarter 14
exp_dat5 <- exp_dat5 %>%
  mutate(scrub2 = ifelse(time > 14, 1, 0))

#This is June 2016; Quarter 18
exp_dat5 <- exp_dat5 %>%
  mutate(scrub3 = ifelse(time > 18, 1, 0))

#Indicator variables for years
exp_dat5$y2012 <- ifelse(exp_dat5$year== 2012,1,0)
exp_dat5$y2013 <- ifelse(exp_dat5$year== 2013,1,0)
exp_dat5$y2014 <- ifelse(exp_dat5$year== 2014,1,0)
exp_dat5$y2015 <- ifelse(exp_dat5$year== 2015,1,0)
exp_dat5$y2016 <- ifelse(exp_dat5$year== 2016,1,0)

#############################################################################
#Figure 2 Panel 1 - time series plot of asthma ED/hospitalizations over time#
#############################################################################
exp_dat4 %>% group_by(yrq) %>% drop_na() %>%
  summarise(sum_asthma = sum(asthma_count)) %>%
  ggplot(aes(yrq, sum_asthma, group=1)) + geom_line(size=1.5) + theme_minimal(base_size=14) +
  scale_y_continuous("Quarterly Louisville-wide\nasthma hospitalizations/ERVs, N", lim=c(0,1000)) +
  xlab("") +  theme(axis.text.x = element_text(angle = 90, hjust = .5, vjust = 0.5)) 
ggsave("Time_Series_Asthma.tiff", dpi=400, height = 4, width = 5.5, units = "in")

    
#######################################################################
#Figure 2 Panel 2 - Plot of count of hospitalizations by ZIP over time#
#######################################################################
#Read in KY ZIP codes
kyzip <- st_read("tl_2010_21_zcta510/tl_2010_21_zcta510.shp")
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

jeffzipb %>%
  ggplot(aes(fill = count.x)) + 
  geom_sf(color = "white", lwd = 0.1) +
  scale_fill_viridis("Count", option="cividis") +
  facet_wrap(~yrq, ncol= 4) + theme_minimal(base_size = 14) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) + 
  theme(legend.position="bottom", legend.box = "horizontal")
ggsave("Figure_2B_Zip_Map_Asthma_Count.tiff", dpi=300, height = 6, width = 5, units = "in")

##############################################
#                Equation 1                  #
#OLS model between events and HyADS exposures#
##############################################

model_hyads_unadj <- lm(tot_exp ~  scrub0 + scrub1 +  scrub2 + scrub3, dat=exp_dat5)
summary(model_hyads_unadj)

model_hyads_adj <- lm(tot_exp ~  scrub0 + scrub1 +  scrub2 + scrub3 + qtr.f  +
                        + zip.f + 
                        RH_Quarter + Pres_Quarter + WS_Quarter + Temp_Quarter, dat=exp_dat5)
summary(model_hyads_adj)
coef_test(model_hyads_adj, vcov = "CR2", 
          cluster = exp_dat5$zip, test = "Satterthwaite")[1:5,]

#############################################################################
#                                Equation 2                                 #
#Poisson model asthma count vs. indicators of the various power plant events#
#############################################################################
model0 <- glm(asthma_count ~  scrub0 + scrub1 +  scrub2 + scrub3 +
                as.factor(zip), 
              dat=exp_dat5, family=quasipoisson(), offset = log(totpop))
summary(model0)

model1 <- glm(asthma_count ~  scrub0 + scrub1 +  scrub2 + scrub3 + qtr.f +y2012 + y2013 + y2014 + y2015 +
                uninsur_p + pov_p + black_p + 
                RH_Quarter + Pres_Quarter + WS_Quarter + Temp_Quarter +
                as.factor(zip), 
                dat=exp_dat5, family=quasipoisson(), offset = log(totpop))
summary(model1)

#Clustered SE at ZIP level
coef_test(model1, vcov = "CR2", 
          cluster = exp_dat5$zip, test = "Satterthwaite")[1:5,]

####################################
#Difference-in-differences analyses#
####################################
exp_dat5 <- exp_dat4 %>% filter(yrq=="2014-2" | yrq=="2014-3"| yrq=="2014-4"| yrq=="2015-1"| yrq=="2015-2"| yrq=="2015-3"| yrq=="2015-4"| yrq=="2016-1"| yrq=="2016-2")
head(exp_dat5)

##Look at change in avg HyADS from pre to post period and define exposed based on that
exp_dat5 <- exp_dat5 %>% mutate(coalclose = dplyr::case_when(( yrq=="2015-3"| yrq=="2015-4"| yrq=="2016-1"| yrq=="2016-2") ~ 1,
                                                             ( yrq=="2015-2" | yrq =="2014-2"| yrq =="2014-3"| yrq == "2014-4" | yrq == "2015-1") ~ 0))

#Find HyADS from units of interest that either retire or install controls
exp_dat5 <- exp_dat5 %>% mutate(hyads_tot = plant_1363_4 + plant_1363_5 + plant_1363_6 + 
                                  plant_1364_1 + plant_1364_2 + 
                                  plant_6166_MB1 + plant_6166_MB2)


#Look at change in average HyADS from pre to post period
exp_dat5b <- exp_dat5 %>% group_by(zip, coalclose) %>%
  dplyr::summarise(hyads_c = sum(hyads_tot))

exp_dat5b <- spread(exp_dat5b, coalclose, hyads_c)
exp_dat5b$hyads_diff <- exp_dat5b$`1` - exp_dat5b$`0`

#Create indicator at 32500 in pre-period = "high exposure"
exp_dat5b <- exp_dat5b %>% mutate(exposed = case_when(`0`< 32500 ~ 0,
                                                      `0` >= 32500 ~ 1))
#Rescale HyADS
exp_dat5b$`0` <- divide.1000(exp_dat5b$`0`)

#Rename pre-values
exp_dat5b <- exp_dat5b %>% rename(pre_hyads = `0`) 

#Select pre-hyads values, zip, and indicatorof pre/post
exp_dat5b <- dplyr::select(exp_dat5b, pre_hyads, exposed, zip)

#Add back on to original dataframe
exp_dat5 <- left_join(exp_dat5, exp_dat5b, by = c("zip" = "zip"))

#Median (IQR) of pre-period
summary(exp_dat5$pre_hyads)

#Look for natural break in HyADS in the pre-period to define high v low
tiff(filename = "ZIP_Natural_Break_DiD_pre.tiff", type = "cairo" ,  res=300, height = 3, width = 5, units = "in")
ggplot(exp_dat5b, aes(pre_hyads)) + geom_histogram(bins=50) + theme_minimal() + 
  scale_x_continuous("Pre-period HyADS, 1000s", breaks=c(20, 22.5,25,27.5,30,32.5,35), labels=c("20","22.5","25","27.5","30","32.5","35"))+
  ylab("Number of ZIP codes")  #32500 looks like a natural break
dev.off()

#Equal to 1 if pre-period HyADS >32500
exp_dat5$exposed_coalclose <- exp_dat5$exposed*exp_dat5$coalclose
exp_dat5$exposed_coalclose_c <- exp_dat5$pre_hyads*exp_dat5$coalclose

##################################
#Figure 4 Panel 1 Parallel Trends#
##################################
#For entire study period
tiff(filename = "ZIP_DiD_Full_Study.tiff", type = "cairo" ,  res=300, height = 4.75, width = 5.5, units = "in")
exp_dat4  <- left_join(exp_dat4, exp_dat5b, by = c("zip" = "zip"))
asthma_ag <- exp_dat4 %>% group_by(exposed, yrq) %>% drop_na()%>%
  dplyr::summarize(mean_asthma = mean(asthma_count))
ggplot(asthma_ag, aes(yrq, mean_asthma, group = as.factor(exposed), color = as.factor(exposed))) + 
  geom_line(size=1) +
  xlab("") +
  scale_color_manual("HyADS group", values= c("#9E0142", "#FDAE61"), labels = c("Low", "High"), guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous("Average ZIP code asthma count") +
  theme_minimal(base_size = 14) +
  geom_vline(xintercept = 14, linetype=3) +
  theme(axis.text.x = element_text(angle = 90, hjust = .5, vjust = 0.5)) 
dev.off()

###################
#Figure 4, Panel 2#
###################
#########################
#      Equation 3       #
#########################
modelDiD0 <- lm(asthma_count ~ coalclose + exposed + exposed_coalclose + totpop +  as.factor(zip) , dat=exp_dat5)
summary(modelDiD0)
#Clustered SE @ZIP level
coef_test(modelDiD0, vcov = "CR2", 
          cluster = exp_dat5$zip, test = "Satterthwaite")[1:4,]

modelDiD1 <- lm(asthma_count ~ coalclose + exposed + exposed_coalclose + totpop + uninsur_p +  qtr.f + factor(year) +
                  pov_p + black_p + RH_Quarter + Pres_Quarter + WS_Quarter + Temp_Quarter +  as.factor(zip)  , dat=exp_dat5)
summary(modelDiD1)
#Clustered SE @ ZIP level
coef_test(modelDiD1, vcov = "CR2", 
          cluster = exp_dat5$zip, test = "Satterthwaite")[1:4,]

#Sensitivity for panels [2]-[3] weighting by baseline population rather than adjusting for baseline population in the model
overall_pop <- as.matrix(exp_dat5$totpop)
overall_pop <- colSums(overall_pop) #7036044
exp_dat5$overall_pop <- 7036044
exp_dat5$wts <- exp_dat5$totpop/exp_dat5$overall_pop
hist(exp_dat5$wts) 

#Panel [2] weighted
modelDiD0 <- lm(asthma_count ~ coalclose + exposed + exposed_coalclose + as.factor(zip), 
                dat=exp_dat5, weights = wts)
summary(modelDiD0)
#Clustered SE @ ZIP level
coef_test(modelDiD0, vcov = "CR2", 
          cluster = exp_dat5$zip, test = "Satterthwaite")[1:4,]

modelDiD1 <- lm(asthma_count ~ coalclose + exposed + exposed_coalclose  + uninsur_p +  qtr.f + factor(year) +
                  pov_p + black_p + RH_Quarter + Pres_Quarter + WS_Quarter + Temp_Quarter +  as.factor(zip), 
                dat=exp_dat5, weights = wts)
summary(modelDiD1)
#Clustered SE @ ZIP level
coef_test(modelDiD1, vcov = "CR2", 
          cluster = exp_dat5$zip, test = "Satterthwaite")[1:4,]

###############################
#Supplementary Figure 4       #
###############################

#Continuous HyADS exposure as the exposure
exp_dat5$exposed_coalclose_c <- exp_dat5$pre_hyads*exp_dat5$coalclose

#Unadjusted, quarterly, continuous HyADS analysis, Supplmental Figure X, Panel 2
modelDiD0c <- lm(asthma_count ~ coalclose + pre_hyads + exposed_coalclose_c + totpop +  as.factor(zip) , dat=exp_dat5)
summary(modelDiD0c)
#Clustered SE @ZIP level
coef_test(modelDiD0c, vcov = "CR2", 
          cluster = exp_dat5$zip, test = "Satterthwaite")[1:4,]

#Adjusted, quarterly, continuous HyADS analysis, Figure 3, Panel 4
modelDiD1c <- lm(asthma_count ~ coalclose + pre_hyads + exposed_coalclose_c + totpop + uninsur_p +  qtr.f + factor(year) +
                   pov_p + black_p + RH_Quarter + Pres_Quarter + WS_Quarter + Temp_Quarter +  as.factor(zip)  , dat=exp_dat5)
summary(modelDiD1c)
#Clustered SE @ ZIP level
coef_test(modelDiD1c, vcov = "CR2", 
          cluster = exp_dat5$zip, test = "Satterthwaite")[1:4,]

#Panel [4],  weighted
modelDiD0c <- lm(asthma_count ~ coalclose + pre_hyads + exposed_coalclose_c  +  as.factor(zip), 
                 dat=exp_dat5, weights = wts)
summary(modelDiD0c)
#Clustered SE @ZIP level
coef_test(modelDiD0c, vcov = "CR2", 
          cluster = exp_dat5$zip, test = "Satterthwaite")[1:4,]

modelDiD1c <- lm(asthma_count ~ coalclose + pre_hyads + exposed_coalclose_c  + uninsur_p +  qtr.f + factor(year) +
                   pov_p + black_p + RH_Quarter + Pres_Quarter + WS_Quarter + Temp_Quarter +  as.factor(zip),
                 dat=exp_dat5, weights =  wts)
summary(modelDiD1c)
#Clustered SE @ ZIP level
coef_test(modelDiD1c, vcov = "CR2", 
          cluster = exp_dat5$zip, test = "Satterthwaite")[1:4,]

#####################
##Quarterly results##
#####################
dat<-read_csv("quarterly_DiD_results.csv")
dat <- dat[1:2,]
dat$group2<- factor(dat$group2, levels = c("1"), labels = c(" "))
dat$group <- factor(dat$group, levels = c("Unadjusted", "Adjusted"))
###################
#Figure 4, Panel 2#
###################
ggplot(dat,aes(x = group2, y = p, ymin = low, ymax = high, group=group, color=group)) + 
  geom_pointrange(position = position_dodge(width = .6), size = 1.1, fatten = 1.5) + 
  geom_hline(yintercept = 0, linetype = "dotted") + 
  scale_y_continuous("\u0394 quarterly asthma hospitalizations/ERVs", limits = c(-8.2,0)) + 
  scale_color_manual("", values = c("#969696", "#252525")) +
  xlab("")+ theme_minimal(base_size = 14) +
  theme(legend.title=element_blank(),legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,0,0,-5),strip.placement = "outside")+
  theme(plot.title = element_text(hjust = 0.5))  +
  theme(plot.title = element_text(size = 14)) 
ggsave("DiD_Results_bw.tiff", dpi=400, height = 5.5, width = 3, units = "in")

ggplot(dat,aes(x = group2, y = p, ymin = low, ymax = high, group=group, color=group)) + 
  geom_pointrange(position = position_dodge(width = .6), size = 1.1, fatten = 1.5) + 
  geom_hline(yintercept = 0, linetype = "dotted") + 
  scale_y_continuous("\u0394 quarterly asthma hospitalizations/ERVs", limits = c(-8.2,0)) + 
  scale_color_manual("", values = c("#404688FF", "#20A486FF")) +
  xlab("")+ theme_minimal(base_size = 14) +
  theme(legend.title=element_blank(),legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,0,0,-5),strip.placement = "outside")+
  theme(plot.title = element_text(hjust = 0.5))  +
  ggtitle("[1] \u0394 HyADS binary\nquarterly effect estimates", ) +
  theme(plot.title = element_text(size = 14)) +
  theme(legend.position = "none") 
ggsave("DiD_Results_Panel_1_color.tiff", dpi=400, height = 5.5, width = 3, units = "in")

###############################
#S. Figure 4, continuous HyADS#
###############################
dat<-read_csv("quarterly_DiD_results.csv")
dat <- dat[3:4,]
dat$group2<- factor(dat$group2, levels = c("2"), labels = c(" "))
dat$group <- factor(dat$group, levels = c("Unadjusted", "Adjusted"))
ggplot(dat, aes(x = group2, y = p, ymin = low, ymax = high, group=group, color=group)) + 
  geom_pointrange(position = position_dodge(width = .3), size = 1.1, fatten = 1.5) + 
  geom_hline(yintercept = 0, linetype = "dotted") + 
  scale_y_continuous("\u0394 quarterly asthma hospitalizations/ERVs") + 
  scale_color_manual("", values = c("#404688FF", "#20A486FF")) +
  xlab("")+ theme_minimal(base_size = 14) +
  theme(legend.title=element_blank(),legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,0,0,-5),strip.placement = "outside")+
  ggtitle("[2] \u0394 HyADS continuous\nquarterly effect estimates", ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size = 14)) +
  theme(legend.position = "none") 
ggsave("DiD_Results_Panel_2_color.tiff", dpi=400, height = 5.5, width = 3, units = "in")

#########################
# First-difference model#
#                       #
#S Figure 4 Panel 3#    #
#########################

#Annual change data
exp_dat5 <- exp_dat5 %>% mutate(pre = case_when(yrq == "2014-2" | yrq == "2014-3" | yrq == "2014-4"| yrq == "2015-1" ~ 1,
                                                yrq == "2015-3" | yrq == "2015-4" | yrq == "2016-1"| yrq == "2016-2" ~ 0))

exp_dat7 <- exp_dat5 %>% group_by(zip, pre) %>%
  dplyr::summarise(asthma_c = sum(asthma_count))
exp_dat7 <- na.omit(exp_dat7)

#Go wide
exp_dat7 <- spread(exp_dat7, pre, asthma_c)
exp_dat7$asthma_diff <- exp_dat7$`0` - exp_dat7$`1`
exp_dat7 <- dplyr::select(exp_dat7, zip, asthma_diff)

#HyADS diff
exp_dat5 <- exp_dat5 %>% mutate(hyads_tot = plant_1363_4 + plant_1363_5 + plant_1363_6 + 
                                  plant_1364_1 + plant_1364_2 + 
                                  plant_6166_MB1 + plant_6166_MB2)
exp_dat8 <- exp_dat5 %>% group_by(zip, pre) %>%
  dplyr::summarise(hyads_c = sum(hyads_tot))
exp_dat8 <- na.omit(exp_dat8)
#Go wide
exp_dat8 <- spread(exp_dat8, pre, hyads_c)
exp_dat8$hyads_diff <- exp_dat8$`0` - exp_dat8$`1`
exp_dat8 <- dplyr::select(exp_dat8, zip, hyads_diff)

#Rescale HyADS by 1000
exp_dat8$hyads_diff <- divide.1000(exp_dat8$hyads_diff)

#Add other covariates, totpop + uninsur_p + pov_p + black_p + RH_Quarter + Pres_Quarter + WS_Quarter + Temp_Quarter
#Total population
exp_dat9 <- exp_dat5 %>% group_by(zip, pre) %>%
  dplyr::summarise(totpopc = sum(totpop))
exp_dat9 <- na.omit(exp_dat9)
#Go wide
exp_dat9 <- spread(exp_dat9, pre, totpopc)
exp_dat9$totpop_diff <- exp_dat9$`0` - exp_dat9$`1`
exp_dat9 <- dplyr::select(exp_dat9, zip, totpop_diff)

#Uninsured diff
exp_dat10 <- exp_dat5 %>% group_by(zip, pre) %>%
  dplyr::summarise(uninsurc = sum(uninsur_p))
#Go wide
exp_dat10 <- spread(exp_dat10, pre, uninsurc)
exp_dat10$uninsur_diff <- exp_dat10$`0` - exp_dat10$`1`
exp_dat10 <- dplyr::select(exp_dat10, zip, uninsur_diff)

#Black race/ethnicity individuals diff
exp_dat11 <- exp_dat5 %>% group_by(zip, pre) %>%
  dplyr::summarise(blackc = sum(black_p))
#Go wide
exp_dat11 <- spread(exp_dat11, pre, blackc)
exp_dat11$black_diff <- exp_dat11$`0` - exp_dat11$`1`
exp_dat11 <- dplyr::select(exp_dat11, zip, black_diff)

#Pov diff
exp_dat12 <- exp_dat5 %>% group_by(zip, pre) %>%
  dplyr::summarise(povc = sum(pov_p))
#Go wide
exp_dat12 <- spread(exp_dat12, pre, povc)
exp_dat12$pov_diff <- exp_dat12$`0` - exp_dat12$`1`
exp_dat12 <- dplyr::select(exp_dat12, zip, pov_diff)

#RH_Quarter diff
exp_dat13 <- exp_dat5 %>% group_by(zip, pre) %>%
  dplyr::summarise(RH_Quarterc = sum(RH_Quarter))
#Go wide
exp_dat13 <- spread(exp_dat13, pre, RH_Quarterc)
exp_dat13$RH_diff <- exp_dat13$`0` - exp_dat13$`1`
exp_dat13 <- dplyr::select(exp_dat13, zip, RH_diff)

# Pres_Quarter diff
exp_dat14 <- exp_dat5 %>% group_by(zip, pre) %>%
  dplyr::summarise(Pres_Quarterc = sum(Pres_Quarter))
#Go wide
exp_dat14 <- spread(exp_dat14, pre, Pres_Quarterc)
exp_dat14$Pres_diff <- exp_dat14$`0` - exp_dat14$`1`
exp_dat14 <- dplyr::select(exp_dat14, zip, Pres_diff)

#WS_Quarter
exp_dat15 <- exp_dat5 %>% group_by(zip, pre) %>%
  dplyr::summarise(WS_Quarterc = sum(WS_Quarter))
#Go wide
exp_dat15 <- spread(exp_dat15, pre, WS_Quarterc)
exp_dat15$WS_diff <- exp_dat15$`0` - exp_dat15$`1`
exp_dat15 <- dplyr::select(exp_dat15, zip, WS_diff)

#Temp_Quarter
exp_dat16 <- exp_dat5 %>% group_by(zip, pre) %>%
  dplyr::summarise(Temp_Quarterc = sum(Temp_Quarter))
#Go wide
exp_dat16 <- spread(exp_dat16, pre, Temp_Quarterc)
exp_dat16$Temp_diff <- exp_dat16$`0` - exp_dat16$`1`
exp_dat16 <- dplyr::select(exp_dat16, zip, Temp_diff)

#Join differenced variables back on to main asthma diff dataframe
exp_dat8 <- left_join(exp_dat8, exp_dat9, by = c("zip" = "zip"))
exp_dat8 <- left_join(exp_dat8, exp_dat10, by = c("zip" = "zip"))
exp_dat8 <- left_join(exp_dat8, exp_dat11, by = c("zip" = "zip"))
exp_dat8 <- left_join(exp_dat8, exp_dat12, by = c("zip" = "zip"))
exp_dat8 <- left_join(exp_dat8, exp_dat13, by = c("zip" = "zip"))
exp_dat8 <- left_join(exp_dat8, exp_dat14, by = c("zip" = "zip"))
exp_dat8 <- left_join(exp_dat8, exp_dat15, by = c("zip" = "zip"))
exp_dat8 <- left_join(exp_dat8, exp_dat16, by = c("zip" = "zip"))
exp_dat8 <- left_join(exp_dat8, exp_dat7, by = c("zip" = "zip"))
head(exp_dat8)
exp_dat8 <- na.omit(exp_dat8)
exp_dat8$hyads_diff <- exp_dat8$hyads_diff/10

#Look at distribution to test for non-linearity
tiff(filename = "ZIP_Natural_Break_DiD.tiff", type = "cairo" ,  res=300, height = 3.5, width = 4, units = "in")
ggplot(exp_dat8, aes(hyads_diff)) + geom_histogram(bins=50) + theme_minimal() + 
  scale_x_continuous("\u0394 HyADS", breaks = seq(-27000, -15000, by = 1000)) +
  theme(axis.text.x = element_text(angle = 90, vjust = .6)) +
  ylab("Number of ZIP codes")  #-18000, -22000, -24000 natural breaks
summary(exp_dat8$hyads_diff) #Median = -22350, Q3 = -22500
dev.off()

#Quartiles are oddly generated because large group of ZIPs with same change, create manual groups
exp_dat8<- exp_dat8 %>% mutate(hyads_group = case_when(hyads_diff >= -18 ~ 1,
                                                       hyads_diff < -18  & hyads_diff >= -22 ~ 2,
                                                       hyads_diff < -22 & hyads_diff >= -24 ~ 3,
                                                       hyads_diff < -24 ~ 4))
table(exp_dat8$hyads_group)

#Correlation between exposures and change in asthma
cor(exp_dat8$hyads_diff, exp_dat8$asthma_diff, method = "spearman")
cor(exp_dat8$hyads_group, exp_dat8$asthma_diff, method = "spearman")

#Basic shape of unadjusted analyses
j1 <- ggplot(exp_dat8, aes(hyads_diff, asthma_diff)) + geom_point() + geom_smooth(se = F) + theme_minimal()
j2 <- ggplot(exp_dat8, aes(hyads_diff, asthma_diff)) + geom_point() + geom_smooth(method="lm", se = F) + theme_minimal()
j3 <- ggplot(exp_dat8, aes(hyads_group, asthma_diff)) + geom_point() + geom_smooth(se = F) + theme_minimal() + scale_x_reverse()
j4 <- ggplot(exp_dat8, aes(hyads_group, asthma_diff)) + geom_point() + geom_smooth(method="lm", se = F) + theme_minimal() + scale_x_reverse()

#Distribution of other variables
ggplot(gather(exp_dat8), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x') + theme_minimal()

#Add baseline population size
total_pop_baseline <- dplyr::select(exp_dat5, totpop, year, zip)
total_pop_baseline <- filter(total_pop_baseline, year==2014)
total_pop_baseline <- distinct(total_pop_baseline)
exp_dat8 <- left_join(exp_dat8, total_pop_baseline, c=("zip" = "zip"))

#Correlation matrix of differences
#How correlated are covariates?
M <- dplyr::select(exp_dat8, hyads_diff, hyads_group, asthma_diff, totpop_diff, totpop, uninsur_diff, 
                   black_diff, pov_diff,
                   RH_diff, Pres_diff, WS_diff, Temp_diff)
M <- round(cor(M, method = "spearman"),2)
cor(M)

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(M)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_viridis("Spearman\nCorrelation",
                     begin = 0, end = 1, option = "A", direction = -1) +
  theme_minimal() + # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1)) +
  coord_fixed()

#Add correlations to the heatmap
tiff(filename = "Diff_2015_Correlation.tiff", type = "cairo" ,  res=300, height = 6, width = 8.5, units = "in")
ggheatmap + geom_text(aes(Var2, Var1, label = value), color = "white", size = 3.6) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.5, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))
dev.off()
#Important variables appear to be uninsured, RH, pressure, Ws, Temp
#However, wind speed, temp, and RH are highly correlated, so just adjust for Temp + pressure

#Using population size weights at baseline instead of adjusting for population diff
exp_dat5_wts <- dplyr::select(exp_dat5, zip, year, wts)
exp_dat5_wts <- dplyr::filter(exp_dat5_wts, year==2014)
exp_dat5_wts <- distinct(exp_dat5_wts)
exp_dat8 <- left_join(exp_dat8, exp_dat5_wts, by = c("zip"= "zip"))

#Unadjusted, annual, continuous HyADS analysis, Figure 3, Panel 1
did <- lm(asthma_diff ~ hyads_diff  + totpop, data = exp_dat8)
summary(did)
#Clustered SE
coef_test(did, vcov = "CR2", 
          cluster = exp_dat8$zip, test = "Satterthwaite")[1:2,]
confint(did)

#########################
#      Equation 4       #
#########################
#Adjusted, annual, continuous HyADS analysis, Figure 3, Panel 1
dif1 <- lm(asthma_diff ~ hyads_diff + uninsur_diff + totpop_diff  +
             Temp_diff + Pres_diff, data = exp_dat8)
summary(dif1)
#Clustered SE
coef_test(dif1, vcov = "CR2", 
          cluster = exp_dat8$zip, test = "Satterthwaite")[1:2,]
confint(dif1)
car::vif(dif1)

#Population weighted rather than adjusted
#Unadjusted
dif1b <- lm(asthma_diff ~ hyads_diff, data = exp_dat8, weights = wts)
summary(dif1b)
#Clustered SE
coef_test(dif1b, vcov = "CR2", 
          cluster = exp_dat8$zip, test = "Satterthwaite")[1:2,]
confint(dif1b)

dif1b <- lm(asthma_diff ~ hyads_diff + uninsur_diff + totpop_diff  +
             Temp_diff + Pres_diff, data = exp_dat8, weights = wts)
summary(dif1b)
#Clustered SE
coef_test(dif1b, vcov = "CR2", 
          cluster = exp_dat8$zip, test = "Satterthwaite")[1:2,]
confint(dif1b)

#####################
#Categories of HyADS#
#####################
#Unadjusted, annual, categorical HyADS analysis, Figure 3, Panel 2
dif_cat <- lm(asthma_diff ~ as.factor(hyads_group) + totpop, data = exp_dat8)
summary(dif_cat)
#Clustered SE
coef_test(dif_cat, vcov = "CR2", 
          cluster = exp_dat8$zip, test = "Satterthwaite")[1:4,]

#########################
#      Equation 4       #
#########################
#Adjusted, annual, categorical HyADS analysis
dif_cat2 <- lm(asthma_diff ~ as.factor(hyads_group) + totpop_diff + totpop + uninsur_diff + 
             Temp_diff + Pres_diff, data = exp_dat8)
summary(dif_cat2)
#Clustered SE
coef_test(dif_cat2, vcov = "CR2", 
          cluster = exp_dat8$zip, test = "Satterthwaite")[1:4,]

#Population weights
dif_cat <- lm(asthma_diff ~ as.factor(hyads_group) , data = exp_dat8, weights = wts)
summary(dif_cat)
#Clustered SE
coef_test(dif_cat, vcov = "CR2", 
          cluster = exp_dat8$zip, test = "Satterthwaite")[1:4,]

dif_cat2 <- lm(asthma_diff ~ as.factor(hyads_group) + totpop_diff + uninsur_diff + 
                 Temp_diff + Pres_diff, data = exp_dat8, weights = wts)
summary(dif_cat2)
#Clustered SE
coef_test(dif_cat2, vcov = "CR2", 
          cluster = exp_dat8$zip, test = "Satterthwaite")[1:4,]


#################
#First-dif plots#
#################

##################
##Annual results##
##################
dat<-read_csv("annual_Dif_results.csv")
dat <- dat[7:8,]
dat$group2<- factor(dat$group2, levels = c("1"), labels = c(" "))
dat$group <- factor(dat$group, levels = c("Unadjusted", "Adjusted"))
dat$group3 <- factor(dat$group3, levels = c("delta pre/post"),
                     labels = c("[1]"))
######################
#S. Figure 4         #
######################
ggplot(dat,aes(x = group2, y = p, ymin = low, ymax = high, group=group, color=group)) + 
  geom_pointrange(position = position_dodge(width = .3), size = 1.1, fatten = 1.5) + 
  geom_hline(yintercept = 0, linetype = "dotted") + 
  scale_y_continuous("\u0394 annual asthma hospitalizations/ERVs", limits = c(-8,0)) + 
  scale_color_manual("", values = c("#404688FF", "#20A486FF")) +
  xlab("")+ theme_minimal(base_size = 14) +
  theme(legend.title=element_blank(),legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,0,0,-5),strip.placement = "outside")+
  theme(legend.position = "none") +
  ggtitle("[3] \u0394 HyADS continuous\nannual effect estimates") +
  theme(plot.title = element_text(size = 14)) +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("Dif_Results_panel3.tiff", dpi=400, height = 5.5, width = 3, units = "in")


dat<-read_csv("dif_zip_results_ggplot_cats.csv")
dat$group2<-reorder(dat$group2,dat$ord)
dat$group <- factor(dat$group, levels = c("Unadjusted", "Adjusted"))
###################
#S Figure 4       #
###################
ggplot(dat,aes(x = group2, y = p, ymin = low, ymax = high, group=group, color=group)) + 
  geom_pointrange(position = position_dodge(width = .3), size = 1.1, fatten = 1.5) + 
  geom_hline(yintercept = 0, linetype = "dotted") + 
  scale_y_continuous("\u0394 annual asthma hospitalizations/ERVs") + 
  scale_color_manual("", values = c("#404688FF", "#20A486FF")) +
  xlab("")+ theme_minimal(base_size = 14) +
  theme(legend.title=element_blank(),legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,0,0,-5),strip.placement = "outside")+
  ggtitle("[4] \u0394 HyADS categories\nannual effect estimates") +
  theme(plot.title = element_text(hjust = 0.5))  +
  theme(plot.title = element_text(size = 14)) 
ggsave("Dif_Results_panel4.tiff", dpi=400, height = 5.5, width = 5.5, units = "in")



