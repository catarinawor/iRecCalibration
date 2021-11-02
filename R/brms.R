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

head(creel)
#data wrangle

#area program Logistical area table

arealookup<- creel %>% 
    select(c(PROGRAM,LOGISTICAL_AREA,PFMA)) %>%
    distinct() %>%
    rename(CREEL_LOGISTICAL_AREA=LOGISTICAL_AREA,AREA=PFMA)

creelc<- creel %>%
  filter(ESTIMATE_SOURCE=="Creel")%>%
  rename(AREA=PFMA,CREEL_LOGISTICAL_AREA=LOGISTICAL_AREA, SD=STANDARD_ERROR,DISPOSITION=TYPE, CREEL=)%>%
  select(c(PROGRAM,CREEL_LOGISTICAL_AREA, AREA,YEAR,MONTH,DISPOSITION,ESTIMATE,SD))%>%
  mutate(SURVEY="creel")



ireclookup<- irec %>% 
    select(c(LOGISTICAL_AREA,AREA)) %>%
    distinct()

#these are areas that Irec has dat for but creel does not
creelirecarea<-full_join(ireclookup,arealookup)%>%
rename(IREC_LOGISTICAL_AREA=LOGISTICAL_AREA)

write.csv(creelirecarea, file = "../data/creelirecarea.csv", row.names = F)

onlyirecarea<- creelirecarea %>% filter(is.na(CREEL_LOGISTICAL_AREA))




#aggregate creel

#input 0s for missing observations on irec
#create df for all possible observations - using variables of interest only
#if other variables are considered, need to include them here
allobs <- expand.grid(list(AREA=unique(irec$AREA),
  YEAR=unique(irec$YEAR),
  MONTH=unique(irec$MONTH),
  DISPOSITION=unique(irec$DISPOSITION)))

irecall<-left_join(allobs,irec)
irecall$ESTIMATE[is.na(irecall$ESTIMATE)]<-0



irecall<-left_join(irecall,arealookup)

irecc<- irecall %>%
  #rename(IREC=ESTIMATE)%>%
  select(c(PROGRAM,CREEL_LOGISTICAL_AREA,AREA,YEAR,MONTH,DISPOSITION,ESTIMATE,VARIANCE))%>%
  group_by(PROGRAM,CREEL_LOGISTICAL_AREA, AREA,YEAR,MONTH,DISPOSITION) %>% 
  summarise(ESTIMATE = sum(ESTIMATE, na.rm=T), VARIANCE = sum(VARIANCE, na.rm=T))%>%
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
datnna <- datxy[!is.na(datxy$CREEL),] 

#===========================
#proportion of 0's in iredc
sum(datnna$IREC==0)/length(datnna$IREC)
propzero<-function(x){sum(x==0)/length(x)}

datnna %>%
  group_by(AREA) %>%
   summarise(prop=function(x))


#=============================
#example model fit - learning brms 
#code stolen from the brms vignette
?brm


#simple model
fit1 <- brm(formula= IREC ~ CREEL +AREA+MONTH,
  data=datxynna, family=hurdle_lognormal)


### extract model code and data used to fit the model in Stan
stancode(fit1)
sdata <- standata(fit1)
names(sdata)



