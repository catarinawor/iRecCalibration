#=======================================================================
# iREc calibration model fit experimentations
# Catarina Wor
# October 2021 
#=======================================================================




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

#read in data
creel<-read.csv("../data/creel_filter.csv")
irec<-read.csv("../data/iRecchinook_2012_2021.csv")

arealu <- read.csv("../data/areaLU.csv") 

#data wrangle

#area program Logistical area table

creel<- creel%>%
      rename(AREA=PFMA)%>%left_join(arealu[,c("AREA","LU_GROUPING3")])%>%
      filter(YEAR>2012)

#input 0s for missing observations on irec
#create df for all possible observations - using variables of interest only
#if other variables are considered, need to include them here
allobs <- expand.grid(list(AREA=unique(irec$AREA),
  YEAR=unique(irec$YEAR),
  MONTH=unique(irec$MONTH),
  DISPOSITION=unique(irec$DISPOSITION)))

#create zero observations and remove in river fisheries
irecall<- left_join(allobs,irec) %>%
#irec%>%
filter(AREA != "Area 29 (In River)", YEAR>2012)

irecall$ESTIMATE[is.na(irecall$ESTIMATE)]<-0

irecall<-irecall%>%left_join(arealu[,c("AREA","LU_GROUPING3")])


irecc<- irecall %>%
  #rename(IREC=ESTIMATE)%>%
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

datxy<-left_join(ireccc,creelcc) %>%
mutate(SEASON=if_else(MONTH<5|MONTH>9,"offseason","peakseason"))

#===========================
#proportion of 0's in iredc

prop0<-function(x){sum(x==0)/length(x)}

summary(datxy)

aggregate(datxy$IREC,by=list(datxy$MONTH),prop0)
aggregate(datxy$IREC,by=list(datxy$LU_GROUPING3),prop0)

aggregate(datxy$IREC,by=list(datxy$YEAR),prop0)



#===========================================
#plot data by area
p <- ggplot(datxy,aes(x=CREEL, y=IREC,color=LU_GROUPING3))
p <- p + geom_point(size=2, alpha=.5)
p <- p + geom_smooth(method = lm, formula= y~0+x, se     = FALSE, size   = 1, alpha  = .8) # to add regression line
p <- p + theme_bw(16)+labs( x="CREEL", y="iREC")
p <- p + scale_color_viridis_d(end = 0.8,option = "C")
p <- p + theme(legend.position="bottom")
p


#=============================
#histograms by groupings - of the data that is complete
datnna<-datxy[!is.na(datxy$CREEL),]

p <- ggplot(datnna,aes(IREC))
p <- p + geom_histogram()
p <- p + facet_wrap(~LU_GROUPING3, scales="free")
p <- p + theme_bw(16)
p <- p + theme(legend.position="bottom")
p



#=============================
#example model fit - learning brms 
#code stolen from the brms vignette

#resample the data
#Does sampling needs to be startified?
#Sampling should occur on or after the exclusion of the NAs?
dats<-datnna[sample(seq_len(nrow(datnna)), nrow(datnna), replace = TRUE),]

summary(dats)
aggregate(dats$MONTH,by=list(dats$MONTH),length)
# This model is just a first attempt to get brm working
# Do not run when rendering as the results are not meaningful and it takes a loooong time to run
##QUestion: I am not sure baout the model formulation: 
# Should it be: IREC  ~ -1 + CREEL+ (-1 + CREEL  |LU_GROUPING3)
#Also: distribution? What should I consider? Lognormal (without 0s) an dhurdle lognormal are the 
#only ones that seem to converge, but diagnostics do not look good, data seems to be overdispersed (hurdle).
# and not fitting higher values with the positive only model.
fit2 <- brm(formula= IREC  ~ -1 + s(MONTH, k=3)+ (-1 + CREEL  |LU_GROUPING3) , 
  data=dats, family=hurdle_lognormal, iter = 800,chains=2 )



summary(fit2)
pairs(fit2)
plot(fit2, ask = FALSE)
conditional_effects(fit2, method="posterior_predict")


#fit2posterior<-posterior_samples(fit2)

#fit2posteriorp<-reshape::melt(fit2posterior)




stancode(fit2)
sdata <- standata(fit2)

#how do interpret loo? 
#look at loo vignette: https://cran.r-project.org/web/packages/loo/vignettes/loo2-example.html
LOO(fit2)
loofit2<-loo(fit2, save_psis = TRUE)
plot(loofit2)

#Marginal posterior predictive checks
yrep <- posterior_predict(fit2)

#model is overdispersed - the thick line should e uniform
ppc_loo_pit_overlay(
  y = dats$IREC[!is.na(dats$CREEL)],
  yrep = yrep,
  lw = weights(loofit2$"psis_object")
)


bayes_R2(fit2)


#plot fit2
# following the guidance on https://cran.r-project.org/web/packages/tidybayes/vignettes/tidy-brms.html
get_variables(fit2)


fit2 %>%
  spread_draws(r_LU_GROUPING3[area,term]) 

fit2 %>%
  spread_draws(r_LU_GROUPING3[area,]) %>%
  median_qi()


fit2 %>%
  spread_draws(r_LU_GROUPING3[area,]) %>%
  summarise_draws()


fit2 %>%
  spread_draws(r_LU_GROUPING3[area,]) %>%
  mutate(area_mean = exp(r_LU_GROUPING3)) %>%
  ggplot(aes(y = area, x = area_mean) )+
  stat_halfeye()

# posterior predictions

dats %>%
  data_grid(LU_GROUPING3,CREEL) %>%
  add_epred_draws(fit2) %>%
  ggplot(aes(x = .epred, y = LU_GROUPING3)) +
  stat_pointinterval(.width = c(.66, .95))
