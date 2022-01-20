#=======================================================================
# iREc calibration model fit experimentations
# Catarina Wor #Norah Brown
# January 2022
#=======================================================================
install.packages("devtools")

devtools::install_github("jwb133/smcfcs")

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

#read in data
creel <- read.csv("data/creel_filter.csv")
irec <- read.csv("data/iRecchinook_2012_2021.csv")
arealu <- read.csv("data/areaLU.csv") 

#data wrangle

#area program Logistical area table
creel<- creel%>%
  rename(AREA=PFMA)%>%left_join(arealu[,c("AREA","LU_GROUPING3")])%>%
  filter(YEAR>2012)

#Norah:Added in a line here to take out the methods that were diving and angling from shore
irec<- irec %>% filter(METHOD=="Angling from boat")

#Norah: moved this up here
#this needs to be before the expansion, so we don't have repeated areas
irec<-irec%>%left_join(arealu[,c("AREA","LU_GROUPING3")])


#input 0s for missing observations on irec
#create df for all possible observations - using variables of interest only
#if other variables are considered, need to include them here
allobs <- expand.grid(list(LU_GROUPING3=unique(irec$LU_GROUPING3),
                           YEAR=unique(irec$YEAR),
                           MONTH=unique(irec$MONTH),
                           DISPOSITION=unique(irec$DISPOSITION)))

#create zero observations and remove in river fisheries
irecall<- left_join(allobs,irec) %>% filter(AREA != "Area 29 (In River)", YEAR>2012)

irecall$ESTIMATE[is.na(irecall$ESTIMATE)]<-0

#added in a line here to say variance for the 0 estimates is 0 - can take this out if we want it to be NA
irecall$VARIANCE[is.na(irecall$VARIANCE)]<-0



irecc<- irecall %>%
  select(c(AREA,YEAR,MONTH,DISPOSITION,ESTIMATE,VARIANCE,LU_GROUPING3))%>%
  group_by( AREA,YEAR,MONTH,DISPOSITION,LU_GROUPING3) %>% summarise(ESTIMATE = sum(ESTIMATE), VARIANCE = sum(VARIANCE))%>%
  mutate(SD=sqrt(VARIANCE))%>%
  select(c(!VARIANCE))%>%
  mutate(SURVEY="iRec")


#wrangle creel data
creelf<- creel %>%
  filter(ESTIMATE_SOURCE=="Creel")%>%
  mutate(SURVEY=case_when(
    Include..20.=="Y"~ "creel20",
    Include..15.=="Y"~ "creel15",
    TRUE ~ "creel"))%>%
  rename(SD=STANDARD_ERROR,DISPOSITION=TYPE)%>%
  select(c(AREA,YEAR,MONTH,DISPOSITION,ESTIMATE,SD, SURVEY, LU_GROUPING3))

creelcc<-creelf%>%
  rename(CREEL=ESTIMATE,SDCREEL=SD)

ireccc<-irecc%>%
  rename(IREC=ESTIMATE,SDIREC=SD)%>%
  select(c(!SURVEY))

#I might change this to June, there is 91 % NAs in the creel database for May
datxy<-left_join(ireccc,creelcc) %>%
  mutate(SEASON=if_else(MONTH<6|MONTH>9,"offseason","peakseason"))

summary(datxy)

datnna<-datxy[!is.na(datxy$CREEL),]


### datxy is the filled data with NAs filled in for creel 
### datnna is the data without NAs for creel

library(smcfcs)

set.seed(1234)
n <- 1000
x <- rnorm(n)
w <- x+rnorm(n)
y <- x+rnorm(n)
x[(n*0.1):n] <- NA
simData <- data.frame(x,w,y)


imps <- smcfcs(simData, smtype="lm", smformula="y~x",
               method=c("norm", "", ""),m=5)

predMat <- array(0, dim=c(3,3))
predMat[1,2] <- 1


imps <- smcfcs(simData, smtype="lm", smformula="y~x",
               method=c("norm", "", ""),m=5,
               predictorMatrix=predMat)

library(mitools)
impobj <- imputationList(imps$impDatasets)
models <- with(impobj, lm(y~x))
summary(MIcombine(models))


x <- rnorm(n)
w1 <- x+rnorm(n)
w2 <- x+rnorm(n)
w2[(n*0.1):n] <- NA
y <- x+rnorm(n)
x <- rep(NA,n)
simData <- data.frame(x,w1,w2,y)