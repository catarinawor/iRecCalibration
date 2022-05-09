#=======================================================================
# Base period reconstruction for CYER 
# #Norah Brown
# May 2022
#=======================================================================



# load packages -----------------------------------------------------------

# devtools::install_github("jwb133/smcfcs")
library(ggplot2)
library(dplyr)
library(grid)
library(brms)
library(bayesplot)

library(magrittr)
library(dplyr)
library(purrr)
library(forcats)
library(tidyr)
library(modelr)
library(ggdist)
library(tidybayes)
library(cowplot)
library(rstan)
library(ggrepel)
library(posterior)
library(hmi)
library(smcfcs)
library(lme4)
library(simputation)
library(naniar)
library(visdat)

library(tidyverse)
library(readxl)
library(openxlsx)


# load data -------------------------------------------------------------------------


#run  this code to get daxy: 
source(here::here("R/format-data-NB.R"))
format_data_NB()

#bcf is the calibration factor
bcf<-read.csv(here::here("data/bcf.csv"))
bcf<- bcf %>% filter(Species=="Chinook") %>% as_tibble()
names(bcf) <- tolower(names(bcf))
bcf_short<-bcf %>% select(licence.year, disposition, bcf) %>% filter(licence.year>2011)

### Load in the creel and irec data:
creel <- read.csv(here::here("data/creel_filter.csv"))
irec <- read.csv(here::here("data/iRecchinook_2012_2021.csv"))
arealu <- read.csv(here::here("data/areaLU.csv"))

#useful function
"%notin%" <- Negate("%in%")

#Edit creel data
#Sum the eastern and western portions of 23, 19, 2 - before April 2014 and Area 20 - before April 2020
#Area 20 occasionally only had W or E, not both
#The list below is only combining the estimates for months which contain BOTH east and West

creelcc <- creel %>%
  rename(AREA = PFMA) %>%
  filter(ESTIMATE_SOURCE == "Creel") %>%
  mutate(SURVEY = case_when(
    Include..20. == "Y" ~ "creel20",
    Include..15. == "Y" ~ "creel15",
    TRUE ~ "creel"
  )) %>%
  mutate(AREA = case_when(
    YEAR == 2013 & MONTH > 4 & str_detect(AREA, "Area 20") ~ "Area 20", 
    YEAR == 2014 & MONTH %in% c(3,6:9) & str_detect(AREA, "Area 20") ~ "Area 20", 
    YEAR %in% c(2015, 2016, 2018) & MONTH %in% c(6:9) & str_detect(AREA, "Area 20") ~ "Area 20", 
    YEAR %in% c(2017, 2019) & MONTH %in% c(5:9) & str_detect(AREA, "Area 20") ~ "Area 20", 
    YEAR == 2020 & MONTH < 4 & str_detect(AREA, "Area 20") ~ "Area 20",
    YEAR <2014 & str_detect(AREA, "Area 23") ~ "Area 23", 
    YEAR == 2014  & MONTH < 4 & str_detect(AREA, "Area 23") ~ "Area 23", 
    YEAR <2014 & str_detect(AREA, "Area 19") ~ "Area 19", 
    YEAR == 2014  & MONTH < 4 & str_detect(AREA, "Area 19") ~ "Area 19", 
    YEAR <2014 & str_detect(AREA, "2E|2W") ~ "Area 2", 
    YEAR == 2014 & MONTH < 4 & str_detect(AREA, "2E|2W") ~ "Area 2",
    TRUE ~ as.character(AREA)
  )) %>%
  select(AREA, YEAR, MONTH, TYPE, ESTIMATE, STANDARD_ERROR,SURVEY ) %>% 
  group_by(AREA, YEAR, MONTH, TYPE, SURVEY) %>% 
  summarise(ESTIMATE = sum(ESTIMATE),STANDARD_ERROR = sum(STANDARD_ERROR)) %>%  
  filter(AREA %notin% c("Area 20 (West)", "Area 20 (East)") ) %>% 
  left_join(arealu[, c("AREA", "LU_GROUPING3")]) %>% 
  rename(SDCREEL = STANDARD_ERROR, DISPOSITION = TYPE, CREEL = ESTIMATE)


#Edit IREC data
#Area 29 (marine) is the same as Area 29 in the CREEL data, Remove in river fisheries
irec <- irec %>% 
  filter(METHOD == "Angling from boat") %>% 
  mutate(AREA = case_when(AREA== "Area 29 (Marine)" ~ "Area 29", TRUE ~ AREA)) %>% 
  filter(AREA != "Area 29 (In River)", YEAR > 2011)



# Expand Irec data
# input 0s for missing observations on irec
# create df for all possible observations - using variables of interest only
# if other variables are considered, need to include them here
allobs <- expand.grid(list(
  AREA = unique(irec$AREA),
  YEAR = unique(irec$YEAR),
  MONTH = unique(irec$MONTH),
  DISPOSITION = unique(irec$DISPOSITION)
))

#create zero observations, with 0 variance
irecall <- left_join(allobs, irec)
irecall$ESTIMATE[is.na(irecall$ESTIMATE)] <- 0
irecall$VARIANCE[is.na(irecall$VARIANCE)]<-0
irecall <- irecall %>% left_join(arealu[, c("AREA", "LU_GROUPING3")])

ireccc <- irecall %>%
  select(c(AREA, YEAR, MONTH, DISPOSITION, ESTIMATE, VARIANCE, LU_GROUPING3)) %>%
  group_by(AREA, YEAR, MONTH, DISPOSITION, LU_GROUPING3) %>%
  summarise(ESTIMATE = sum(ESTIMATE), VARIANCE = sum(VARIANCE)) %>%
  mutate(SD = sqrt(VARIANCE)) %>%
  select(c(!VARIANCE)) %>%
  rename(IREC = ESTIMATE, SDIREC = SD)

#should we change this to June since there are 91% missing data in May for creel? Changed to 6 here
irec_creel_merged <- merge(creelcc, ireccc, all=TRUE) %>% as_tibble() %>% 
  mutate(SEASON = if_else(MONTH < 6 | MONTH > 9, "offseason", "peakseason"))

names(irec_creel_merged) <- tolower(names(irec_creel_merged))
irec_creel_merged <- rename(irec_creel_merged, region = lu_grouping3)
irec_creel_merged


irec_creel_merged<- irec_creel_merged %>% mutate(licence.year= case_when(
                                       month > 3 ~ as.numeric(year),
                                       month < 4 ~ as.numeric(year - 1 )))
#irec_creel_merged<- irec_creel_merged %>% filter(licence.year>2011)

irec_creel_merged<-merge(irec_creel_merged, bcf_short, all=TRUE)%>% as_tibble()

#create a pseudocreel, this is how it gets used
irec_creel_merged_pseudo<-irec_creel_merged %>%  mutate(irec_var = sdirec ^ 2, 
                                                        creel_var = sdcreel ^ 2, 
                                                        pseudocreel = case_when(
                                                                      month %in% c(5:9) & is.na(creel) ~ as.numeric(irec/bcf),
                                                                      month %in% c(1:4,10:12) ~ as.numeric(irec/bcf),
                                                                      TRUE ~ as.numeric(creel)), 
                                                        pseudocreel_var = case_when(
                                                                        month %in% c(5:9) & is.na(creel_var) ~ as.numeric(irec_var/bcf),
                                                                        month %in% c(1:4,10:12) ~ as.numeric(irec_var/bcf),
                                                                        TRUE ~ as.numeric(creel_var)))


#Just for completeness, I identify these areas, but they don't get used in psl/cnr 
#treaty
treaty_cbc<-c("Area 10", "Area 106", "Area 110", "Area 6", "Area 7", "Area 8", "Area 9", "Area 130")
treaty_nbc_aabm<-c("Area 2","Area 1", "Area 101", "Area 102",  "Area 142", "Area 2E", "Area 2W")
treaty_nbc_isbm<-c( "Area 103", "Area 104", "Area 105", "Area 3", "Area 4", "Area 5")
gst<-c("Area 13", "Area 14", "Area 15", "Area 16", "Area 17", "Area 18", "Area 19", "Area 19 (GS)", "Area 28", "Area 29") 
jst<-c("Area 11", "Area 111", "Area 12")
jdf<-c("Area 19", "Area 19 (JDF)", "Area 20", "Area 20 (East)", "Area 20 (West)")


irec_creel_merged_pseudo<-irec_creel_merged_pseudo %>% 
              mutate(erafishery = case_when(
              area%in%c("Area 121", "Area 122", "Area 123", "Area 124", "Area 125", "Area 126", "Area 127") ~ "WCVI AABM S",
              area%in%c("Area 21", "Area 22", "Area 24") & month%in%c(10,11,12,1,2,3,4,5,6,7) ~ "WCVI AABM S",
              grepl("Area 23", area) & month%in%c(10,11,12,1,2,3,4,5,6,7) ~ "WCVI AABM S",
              area%in%c("Area 21", "Area 22", "Area 24") & month%in%c(8,9) ~ "WCVI ISBM S",
              grepl("Area 23", area) & month%in%c(8,9) ~ "WCVI ISBM S",
              area%in%c("Area 25", "Area 26", "Area 27") & month%in%c(10,11,12,1,2,3,4,5,6) ~ "WCVI AABM S",
              area%in%c("Area 25", "Area 26", "Area 27") & month%in%c(7,8,9) ~ "WCVI ISBM S",
              area %in% treaty_cbc~ "CBC S", 
              area %in% treaty_nbc_aabm~ "NBC AABM S", 
              area %in% treaty_nbc_isbm~ "NBC ISBM S", 
              area %in% gst~ "GEO ST S",
              area %in% jst~ "JNST S",
              area %in% jdf~ "BC JF S"))


#A few different ways to summarise the data:

irec_creel_merged_pseudo_sum1<- irec_creel_merged_pseudo %>% group_by(area, year, disposition) %>% summarise(creel_sum1=ifelse(all(is.na(creel)), NA, sum(creel, na.rm=TRUE)),
                                                                                     pseudocreel_sum1=sum(pseudocreel))
p <- ggplot(irec_creel_merged_pseudo_sum1 ,aes(x=creel_sum1, y=pseudocreel_sum1))
p <- p + geom_point(size=2, alpha=.5, aes(color=as.factor(year), shape=disposition)) +  facet_wrap(~disposition, scales="free")
p <- p + geom_smooth(method = lm, formula= y~x,  size = 1, alpha  = .5, col="black") # to add regression line
p <- p + theme_bw(16)+labs( x="creel only", y="creel updated with irec - pseudocreel")
p <- p + scale_color_viridis_d(end = 0.8,option = "B")+ scale_fill_viridis_d(end = 0.8,option = "B")
p <- p + theme(legend.position="bottom")+ geom_abline(intercept = 0, slope = 1, linetype="dashed", size=1)
p

irec_creel_merged_pseudo_sum2<- irec_creel_merged_pseudo %>% group_by(erafishery, year, disposition) %>% summarise(creel_sum1=ifelse(all(is.na(creel)), NA, sum(creel, na.rm=TRUE)),
                                                                                     pseudocreel_sum1=sum(pseudocreel))


p2 <- ggplot(irec_creel_merged_pseudo_sum2 ,aes(x=creel_sum1, y=pseudocreel_sum1, color=erafishery, fill=erafishery, shape=disposition))
p2 <- p2 + geom_point(size=2, alpha=.5) +  facet_wrap(~disposition, scales="free")
p2 <- p2 + geom_smooth(method = lm, formula= y~x,  size = 1, alpha  = .5) # to add regression line
p2 <- p2 + theme_bw(16)+labs( x="creel only", y="creel updated with irec pseudocreel")
p2 <- p2 + scale_color_viridis_d(end = 0.8,option = "B")+ scale_fill_viridis_d(end = 0.8,option = "B")
p2 <- p2 + theme(legend.position="bottom")+ geom_abline(intercept = 0, slope = 1, linetype="dashed", size=1)
p2

irec_creel_merged_pseudo_sum3<- irec_creel_merged_pseudo %>% filter(erafishery %in% c("WCVI AABM S", "WCVI ISBM S")) %>% 
                                     group_by(erafishery, year, disposition) %>% summarise(creel_sum1=ifelse(all(is.na(creel)), NA, sum(creel, na.rm=TRUE)),
                                                                                           pseudocreel_sum1=sum(pseudocreel))

p3 <- ggplot(irec_creel_merged_pseudo_sum3 ,aes(x=creel_sum1, y=pseudocreel_sum1))
p3 <- p3 + geom_point(size=2, alpha=.5, aes(color=as.factor(year), shape=erafishery)) +  facet_wrap(~disposition, scales="free")
p3 <- p3 + geom_smooth(method = lm, formula= y~x,  size = 1, alpha  = .5, col="black") # to add regression line
p3 <- p3 + theme_bw(16)+labs( x="creel only", y="creel updated with irec pseudocreel")
p3 <- p3 + scale_color_viridis_d(end = 0.8,option = "B")+ scale_fill_viridis_d(end = 0.8,option = "B")
p3 <- p3 + theme(legend.position="bottom")+ geom_abline(intercept = 0, slope = 1, linetype="dashed", size=1)
p3

p3 <- ggplot(irec_creel_merged_pseudo_sum3 ,aes(x=creel_sum1, y=pseudocreel_sum1, color=erafishery, fill=erafishery))
p3 <- p3 + geom_point(size=2, alpha=.5) +  facet_wrap(~disposition+erafishery, scales="free")
p3 <- p3 + geom_smooth(method = lm, formula= y~x,  size = 1, alpha  = .5) # to add regression line
p3 <- p3 + theme_bw(16)+labs( x="creel only", y="creel updated with irec pseudocreel")
p3 <- p3 + scale_color_viridis_d(end = 0.8,option = "B")+ scale_fill_viridis_d(end = 0.8,option = "B")
p3 <- p3 + theme(legend.position="bottom")+ geom_abline(intercept = 0, slope = 1, linetype="dashed", size=1)
p3


#Compare to CNR data downloaded from CAMP May 9, 2022
#see camp downloads
#cnr<-read_excel(here::here("data/REAMCNRData.xlsx"))
# cnr is one row per year so need to compare with era
cnr_canada_sport


irec_creel_merged_pseudo_sum_erafishery<- irec_creel_merged_pseudo %>% group_by(erafishery, year, disposition) %>% 
                                                                        summarise(creel_sum1=ifelse(all(is.na(creel)), NA, sum(creel, na.rm=TRUE)), 
                                                                                  creel_var_sum1=ifelse(all(is.na(creel_var)), NA, sum(creel_var, na.rm=TRUE)),
                                                                                  creel_sd_sum1 = sqrt(creel_var_sum1),
                                                                                  pseudocreel_sum1=sum(pseudocreel), 
                                                                                  pseudocreel_var_sum1=sum(pseudocreel_var), 
                                                                                  pseudocreel_sd_sum1=sqrt(pseudocreel_var_sum1)) %>% 
                                                                        select(-pseudocreel_var_sum1, -creel_var_sum1)
              
                                                                      

irec_creel_merged_pseudo_sum_erafishery

cnr_canada_sport<- cnr_canada_sport %>% mutate(cnr_sd = 0)
irec_creel_cnr<-merge(irec_creel_merged_pseudo_sum_erafishery, cnr_canada_sport, all=TRUE) %>% as_tibble()

irec_creel_cnr<- irec_creel_cnr %>% pivot_longer(cols=c("creel_sum1", "pseudocreel_sum1", "cnr"), names_to = "source", values_to = "values") %>%   
                                    pivot_longer(cols=c("creel_sd_sum1", "pseudocreel_sd_sum1", "cnr_sd"), names_to = "sourcesd", values_to = "sd") %>% 
                                    mutate(source_correct = case_when(
                                          source == "creel_sum1" & sourcesd =="creel_sd_sum1" ~ "yes", 
                                          source == "pseudocreel_sum1" & sourcesd =="pseudocreel_sd_sum1" ~ "yes", 
                                          source == "cnr" & sourcesd =="cnr_sd" ~ "yes", 
                                          TRUE ~ "no" )) %>% 
                                    filter(source_correct == "yes") %>% 
                                    select(-source_correct, -sourcesd)

irec_creel_cnr<- irec_creel_cnr %>% filter(erafishery %in% c("NBC AABM S", "NBC ISBM S", "CBC S", "WCVI AABM S", "WCVI ISBM S")) %>% 
                                    filter(year>2008)

View(irec_creel_cnr)
theme_set(theme_bw())
pkept <- ggplot(irec_creel_cnr %>% filter(disposition=="Kept") ,aes(x=as.factor(year), y=values, color=source, group=source))
pkept <- pkept + geom_point(size=3, alpha=.5) +  geom_pointrange(aes(ymin=values-sd, ymax = values+sd))+ facet_wrap(~disposition + erafishery, scales="free") + geom_line()+theme(legend.position = "bottom")
pkept

ggsave("Plots/kept_irec_creel_cnr.tiff")

pkept2 <- ggplot(irec_creel_cnr %>% filter(disposition=="Kept") ,aes(x=as.factor(year), y=values, color=source, group=source))
pkept2 <- pkept2 + geom_point(size=3, alpha=.5) +  geom_pointrange(aes(ymin=values-sd, ymax = values+sd))+ facet_wrap(~disposition + erafishery) + geom_line()+theme(legend.position = "bottom")
pkept2

ggsave("Plots/kept_irec_creel_cnr_same_axis.tiff")


pReleased <- ggplot(irec_creel_cnr %>% filter(disposition=="Released") ,aes(x=as.factor(year), y=values, color=source, group=source))
pReleased <- pReleased + geom_point(size=3, alpha=.5) +  geom_pointrange(aes(ymin=values-sd, ymax = values+sd))+ facet_wrap(~disposition + erafishery, scales="free") + geom_line()+theme(legend.position = "bottom")
pReleased

ggsave("Plots/Released_irec_creel_cnr.tiff")

pReleased2 <- ggplot(irec_creel_cnr %>% filter(disposition=="Released") ,aes(x=as.factor(year), y=values, color=source, group=source))
pReleased2 <- pReleased2 + geom_point(size=3, alpha=.5) +  geom_pointrange(aes(ymin=values-sd, ymax = values+sd))+ facet_wrap(~disposition + erafishery) + geom_line()+theme(legend.position = "bottom")
pReleased2

ggsave("Plots/Released_irec_creel_cnr_same_axis.tiff")






pkept <- ggplot(irec_creel_cnr %>% filter(disposition=="Kept") ,aes(x=as.factor(year), y=values, color=source, group=source))
pkept <- pkept + geom_point(size=3, alpha=.5) +  facet_wrap(~disposition + erafishery, scales="free") + geom_line()+theme(legend.position = "bottom")
pkept






cnr_compare_table <-cnr_compare %>% mutate(Kept_diff_pseudo = (pseudocreel_sum1_Kept - cnr_Kept), 
                                     Released_diff_pseudo = (pseudocreel_sum1_Released - cnr_Released))

