#=======================================================================
# Base period reconstruction for CYER 
# #Norah Brown
# Feb 2022
#=======================================================================
# install.packages("devtools")
# 
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


source(here::here("R/Norah/format-data-NB.R"))
format_data_NB()

bcf<-read.csv(here::here("data/Norah/bcf.csv"))
bcf<- bcf %>% filter(Species=="Chinook") %>% as_tibble()
names(bcf) <- tolower(names(bcf))
bcf_short<-bcf %>% select(licence.year, disposition, bcf) %>% filter(licence.year>2011)



datxy<- datxy %>% mutate(licence.year= case_when(
                                       month > 3 ~ as.numeric(year),
                                       month < 4 ~ as.numeric(year - 1 )))
datxy<- datxy %>% filter(licence.year>2011)

datxy<-merge(datxy, bcf_short)
datxy<-datxy %>% as_tibble()

datxy_pseudo<-datxy %>%  mutate(pseudocreel = case_when(
                                             month %in% c(5:9) & is.na(creel) ~ as.numeric(irec/bcf),
                                             month %in% c(1:4,10:12) ~ as.numeric(irec/bcf),
                                             TRUE ~ as.numeric(creel)
                                                        ))
  unique(datxy_pseudo$area)

  #Just for completeness, I identify these areas, but they don't get used in psl 
#treaty
treaty_cbc<-c("Area 10", "Area 106", "Area 110", "Area 6", "Area 7", "Area 8", "Area 9", "Area 130")
treaty_nbc_aabm<-c("Area 2","Area 1", "Area 101", "Area 102",  "Area 142", "Area 2E", "Area 2W")
treaty_nbc_isbm<-c( "Area 103", "Area 104", "Area 105", "Area 3", "Area 4", "Area 5")
gst<-c("Area 13", "Area 14", "Area 15", "Area 16", "Area 17", "Area 18", "Area 19", "Area 19 (GS)", "Area 28", "Area 29") 
jst<-c("Area 11", "Area 111", "Area 12")
jdf<-c("Area 19", "Area 19 (JDF)", "Area 20", "Area 20 (East)", "Area 20 (West)")

  
  
  
  
datxy_pseudo<-datxy_pseudo %>% 
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

### chelsea
dat_chel<- datxy_pseudo %>% filter(erafishery %in% c("NBC AABM S", "NBC ISBM S", "CBC S")) 
View(dat_chel)

p <- ggplot(dat_chel,aes(x=year, y=irec))
p <- p + geom_point(size=2, alpha=.5, aes(color=erafishery, shape=disposition)) +  facet_wrap(~disposition, scales="free")
p <- p + geom_smooth(method = lm, formula= y~x,  size = 1, alpha  = .5, col="black") # to add regression line
p <- p + theme_bw(16)+labs( x="year", y="irec")
p <- p + scale_color_viridis_d(end = 0.8,option = "B")+ scale_fill_viridis_d(end = 0.8,option = "B")
p <- p + theme(legend.position="bottom")
p


p <- ggplot(dat_chel,aes(x=creel, y=irec))
p <- p + geom_point(size=2, alpha=.5, aes(color=erafishery, shape=disposition)) +  facet_wrap(~disposition, scales="free")
p <- p + geom_smooth(method = lm, formula= y~x,  size = 1, alpha  = .5, col="black") # to add regression line
p <- p + theme_bw(16)+labs( x="year", y="irec")
p <- p + scale_color_viridis_d(end = 0.8,option = "B")+ scale_fill_viridis_d(end = 0.8,option = "B")
p <- p + theme(legend.position="bottom")+ geom_abline(intercept = 0, slope = 1, linetype="dashed", size=1)
p

#####





View(datxy_pseudo_sum2)

datxy_pseudo_sum1<- datxy_pseudo %>% group_by(area, year, disposition) %>% summarise(creel_sum1=ifelse(all(is.na(creel)), NA, sum(creel, na.rm=TRUE)),
                                                                                     pseudocreel_sum1=sum(pseudocreel))
p <- ggplot(datxy_pseudo_sum1 ,aes(x=creel_sum1, y=pseudocreel_sum1))
p <- p + geom_point(size=2, alpha=.5, aes(color=as.factor(year), shape=disposition)) +  facet_wrap(~disposition, scales="free")
p <- p + geom_smooth(method = lm, formula= y~x,  size = 1, alpha  = .5, col="black") # to add regression line
p <- p + theme_bw(16)+labs( x="creel only", y="creel updated with irec pseudocreel")
p <- p + scale_color_viridis_d(end = 0.8,option = "B")+ scale_fill_viridis_d(end = 0.8,option = "B")
p <- p + theme(legend.position="bottom")+ geom_abline(intercept = 0, slope = 1, linetype="dashed", size=1)
p

datxy_pseudo_sum2<- datxy_pseudo %>% group_by(erafishery, year, disposition) %>% summarise(creel_sum1=ifelse(all(is.na(creel)), NA, sum(creel, na.rm=TRUE)),
                                                                                     pseudocreel_sum1=sum(pseudocreel))


p2 <- ggplot(datxy_pseudo_sum2 ,aes(x=creel_sum1, y=pseudocreel_sum1, color=erafishery, fill=erafishery, shape=disposition))
p2 <- p2 + geom_point(size=2, alpha=.5) +  facet_wrap(~disposition, scales="free")
p2 <- p2 + geom_smooth(method = lm, formula= y~x,  size = 1, alpha  = .5) # to add regression line
p2 <- p2 + theme_bw(16)+labs( x="creel only", y="creel updated with irec pseudocreel")
p2 <- p2 + scale_color_viridis_d(end = 0.8,option = "B")+ scale_fill_viridis_d(end = 0.8,option = "B")
p2 <- p2 + theme(legend.position="bottom")+ geom_abline(intercept = 0, slope = 1, linetype="dashed", size=1)
p2

datxy_pseudo_sum3<- datxy_pseudo %>% filter(erafishery %in% c("WCVI AABM S", "WCVI ISBM S")) %>% 
                                     group_by(erafishery, year, disposition) %>% summarise(creel_sum1=ifelse(all(is.na(creel)), NA, sum(creel, na.rm=TRUE)),
                                                                                           pseudocreel_sum1=sum(pseudocreel))

p3 <- ggplot(datxy_pseudo_sum3 ,aes(x=creel_sum1, y=pseudocreel_sum1))
p3 <- p3 + geom_point(size=2, alpha=.5, aes(color=as.factor(year), shape=erafishery)) +  facet_wrap(~disposition, scales="free")
p3 <- p3 + geom_smooth(method = lm, formula= y~x,  size = 1, alpha  = .5, col="black") # to add regression line
p3 <- p3 + theme_bw(16)+labs( x="creel only", y="creel updated with irec pseudocreel")
p3 <- p3 + scale_color_viridis_d(end = 0.8,option = "B")+ scale_fill_viridis_d(end = 0.8,option = "B")
p3 <- p3 + theme(legend.position="bottom")+ geom_abline(intercept = 0, slope = 1, linetype="dashed", size=1)
p3

p3 <- ggplot(datxy_pseudo_sum3 ,aes(x=creel_sum1, y=pseudocreel_sum1, color=erafishery, fill=erafishery))
p3 <- p3 + geom_point(size=2, alpha=.5) +  facet_wrap(~disposition+erafishery, scales="free")
p3 <- p3 + geom_smooth(method = lm, formula= y~x,  size = 1, alpha  = .5) # to add regression line
p3 <- p3 + theme_bw(16)+labs( x="creel only", y="creel updated with irec pseudocreel")
p3 <- p3 + scale_color_viridis_d(end = 0.8,option = "B")+ scale_fill_viridis_d(end = 0.8,option = "B")
p3 <- p3 + theme(legend.position="bottom")+ geom_abline(intercept = 0, slope = 1, linetype="dashed", size=1)
p3


#Compare to CNR data downloaded from CAMP Feb 11, 2022
cnr<-read_excel(here::here("data/Norah/REAMCNRData.xlsx"))

cnr_wcvi<-cnr %>% filter(ERAFishery %in% c(50,51)) %>% 
                  mutate(erafishery = case_when(
                         ERAFishery == 50 ~ "WCVI AABM S", 
                         ERAFishery == 51 ~ "WCVI ISBM S"), 
                         year = ERAYear) %>% 
                  rename(cnr_Kept = CNRValue1, cnr_Released= CNRValue2) %>% 
                  select(erafishery, year, cnr_Kept, cnr_Released) %>% 
                  filter(year>2011)
                  
datxy_compare<-datxy_pseudo_sum3 %>% pivot_wider(names_from = disposition, values_from = c(creel_sum1, pseudocreel_sum1))

cnr_compare<-merge(cnr_wcvi, datxy_compare)

cnr_compare_long<- cnr_compare %>% pivot_longer(cols = c(cnr_Kept, cnr_Released, creel_sum1_Kept, creel_sum1_Released, 
                                                         pseudocreel_sum1_Kept, pseudocreel_sum1_Released),  
                                                names_to = "source", 
                                                values_to = "count") %>% 
                                  mutate(disposition = case_when(
                                         grepl("Kept", source) ~ "Kept", 
                                         grepl("Released", source) ~ "Released")) %>% 
                                  mutate(data_type = case_when(
                                   grepl("cnr", source) ~ "CNR", 
                                   grepl("pseudo", source) ~ "creel updated with irec", 
                                   TRUE ~ "creel only"))


cnr_compare_long <- cnr_compare_long %>% filter(year<2021)

p4 <- ggplot(cnr_compare_long ,aes(x=as.factor(year), y=count, color=data_type))
p4 <- p4 + geom_point(size=2, alpha=.5) +  facet_wrap(~disposition + erafishery, scales="free")
p4










cnr_compare_table <-cnr_compare %>% mutate(Kept_diff_pseudo = (pseudocreel_sum1_Kept - cnr_Kept), 
                                     Released_diff_pseudo = (pseudocreel_sum1_Released - cnr_Released))

