#=======================================================================
# iREc calibration model fit experimentations
# Catarina Wor
# October 2021 
#=======================================================================



library(ggplot2)
library(dplyr)
library(brms)

#read in data
creel<-read.csv("../data/creel_filter.csv")
irec<-read.csv("../data/iRecchinook_2012_2021.csv")


#data wrangle

#input 0s for missing observations on irec
#create df for all possible observations - using variables of interest only
#if other variables are considered, need to include them here
allobs <- expand.grid(list(AREA=unique(irec$AREA),
  YEAR=unique(irec$YEAR),
  MONTH=unique(irec$MONTH),
  DISPOSITION=unique(irec$DISPOSITION)))

irecall<-left_join(allobs,irec)
irecall$ESTIMATE[is.na(irecall$ESTIMATE)]<-0


creelc<- creel %>%
  filter(ESTIMATE_SOURCE=="Creel")%>%
  rename(AREA=PFMA, SD=STANDARD_ERROR,DISPOSITION=TYPE, CREEL=)%>%
  select(c(AREA,YEAR,MONTH,DISPOSITION,ESTIMATE,SD))%>%
  mutate(SURVEY="creel")


irecc<- irecall %>%
  #rename(IREC=ESTIMATE)%>%
  select(c(AREA,YEAR,MONTH,DISPOSITION,ESTIMATE,VARIANCE))%>%
  group_by( AREA,YEAR,MONTH,DISPOSITION) %>% summarise(ESTIMATE = sum(ESTIMATE), VARIANCE = sum(VARIANCE))%>%
  mutate(SD=sqrt(VARIANCE))%>%
  select(c(!VARIANCE))%>%
  mutate(SURVEY="iRec")

dat<-rbind(creelc,irecc)



creelcc<-creelc%>%
rename(CREEL=ESTIMATE,SDCREEL=SD)%>%
  select(c(!SURVEY))


ireccc<-irecc%>%
rename(IREC=ESTIMATE,SDIREC=SD)%>%
  select(c(!SURVEY))


datxy<-left_join(ireccc,creelcc)



#=============================
#model fit 

?brm


#simple model
fit1 <- brm(formula= IREC ~ CREEL +AREA+MONTH,
  data=datxy, family=hurdle_lognormal)




