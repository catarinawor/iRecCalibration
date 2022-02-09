#=======================================================================
# Base period reconstruction for CYER 
# #Norah Brown
# Feb 2022
#=======================================================================
# install.packages("devtools")
# 
# devtools::install_github("jwb133/smcfcs")
source(here::here("R/Norah/format-data-NB.R"))
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


source(here::here("R/format-data-NB.R"))

bcf<-read.csv(here::here("data/bcf.csv"))
bcf<- bcf %>% filter(Species=="Chinook") %>% as_tibble()
names(bcf) <- tolower(names(bcf))
bcf_short<-bcf %>% select(licence.year, disposition, bcf) %>% filter(licence.year>2012)

### datxy is the filled data with NAs filled in for creel 
#create new column in 

case_when(n bbb4
          AREA%in%c(6,7,8,9,10,106,110) ~ "CBC",
          AREA%in%c(11,12,111) ~  "JST",
          AREA%in%c(13,14,15,16,17,18,28) ~ "GST",
          AREA%in%c(19,29)~paste0(REGION), 
          AREA==20 ~ "JDF",
          AREA%in%c(21,22,23,24,25,26,27,121,122,123,124,125,126,127) ~ paste("WCVI", MANAGEMENT, sep=" ")),
#AREA%in%c(21,22,23,24) & MONTH%in%c(8,9) ~ "WCVI ISBM",

# "Area 107", "Area 108", "Area 109", not considered cbc? 

cbc<-c("Area 10", "Area 106", "Area 110", "Area 6", "Area 7", "Area 8", "Area 9")
nbc<-c("Area 2","Area 1", "Area 101", "Area 102", "Area 103", "Area 104", "Area 105", "Area 130", "Area 142", "Area 2 East", "Area 2 West", "Area 3", "Area 4", "Area 5")
gst<-c("Area 13", "Area 14", "Area 15", "Area 16", "Area 17", "Area 18", "Area 19", "Area 19 Saanich Inlet only", "Area 28", "Area 29", "Area 29 Georgia Strait", "Area 29 In River") 
jst<-c("Area 11", "Area 111", "Area 12")
wcvi<-c("Area 121", "Area 123", "Area 124", "Area 125", "Area 126", "Area 127", "Area 22","Area 21",  "Area 23 Barkley Sound",  "Area 23 Alberni Inlet",  "Area 23", "Area 24", "Area 25", "Area 26", "Area 27")
jdf<-c("Area 19", "Area 19 Main Portion", "Area 20", "Area 20 (East)", "Area 20 (West)")

irec_raw_combined<- irec_raw_combined %>% 
  mutate(region = case_when(
    area %in% cbc~ "Central BC", 
    area %in% nbc~ "Northern BC",                               
    area %in% gst~ "Georgia Strait",
    area %in% jst~ "Johnstone Strait",
    area %in% wcvi ~ "West Coast Vancouver Island",
    area %in% jdf~ "Juan de Fuca",
    TRUE ~ "other"
  ))
irec_raw_combined$region<- factor(irec_raw_combined$region, levels = c("West Coast Vancouver Island","Johnstone Strait", "Georgia Strait", "Juan de Fuca", "Central BC" , "Northern BC"), ordered = TRUE)


datxy<- datxy %>% mutate(licence.year= case_when(
                                       month > 3 ~ as.numeric(year),
                                       month < 4 ~ as.numeric(year - 1 )))
datxy<- datxy %>% filter(licence.year>2012)

datxy<-merge(datxy, bcf_short)
datxy<-datxy %>% as_tibble()

datxy_pseudo<-datxy %>% mutate(pseudocreel = case_when(
                                                is.na(creel) ~ as.numeric(irec/bcf),
                                                TRUE ~ as.numeric(creel)
                                                           ))
View(datxy_pseudo_sum1)

datxy_pseudo_sum1<- datxy_pseudo %>% group_by(area, year, disposition) %>% summarise(creel_sum1=ifelse(all(is.na(creel)), NA, sum(creel, na.rm=TRUE)),
                                                                                     pseudocreel_sum1=sum(pseudocreel))










p <- ggplot(datxy_pseudo_sum1 ,aes(x=creel_sum1, y=pseudocreel_sum1,fill=as.factor(year), color=as.factor(year)))
p <- p + geom_point(size=2, alpha=.5)
# p <- p + geom_smooth(method = lm, formula= y~x,  size = 2, alpha  = .2) # to add regression line
p <- p + theme_bw(16)+labs( x="creel only", y="irec pseudocreel")
p <- p + scale_color_viridis_d(end = 0.8,option = "B")+ scale_fill_viridis_d(end = 0.8,option = "B")
p <- p + theme(legend.position="bottom")+ geom_abline(intercept = 0, slope = 1, linetype="dashed", size=1)
p

