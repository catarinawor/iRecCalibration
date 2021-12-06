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
creel <- read.csv("../data/creel_filter.csv")
irec <- read.csv("../data/iRecchinook_2012_2021.csv")
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

summary(datxy)

datxy$diff <- datxy$CREEL - datxy$IREC
p <- ggplot(datxy,aes(y=diff, x=as.factor(MONTH)))
p <- p + geom_boxplot(width=1, position = position_dodge(width = 1))
p <- p +geom_hline(aes(yintercept=0),size=1.1, alpha=.5)
p <- p + scale_color_viridis_d(end = 0.8,option = "A")
p <- p + theme_bw(16)+labs( x="month", y="creel-irec")

p

#===========================
#histograms by groupings - of the data that is complete
datnna<-datxy[!is.na(datxy$CREEL),]


#proportion of 0's in iredc

prop0<-function(x){sum(x==0)/length(x)}


aggregate(datnna$CREEL,by=list(datnna$MONTH),prop0)
aggregate(datnna$CREEL,by=list(datnna$LU_GROUPING3),prop0)
aggregate(datnna$CREEL,by=list(datnna$YEAR),prop0)



#===========================================
#plot data by area
p <- ggplot(datnna,aes(y=CREEL, x=IREC,color=LU_GROUPING3))
p <- p + geom_point(size=2, alpha=.5)
p <- p + geom_smooth(method = lm, formula= y~0+x, se     = FALSE, size   = 1, alpha  = .8) # to add regression line
p <- p + theme_bw(16)+labs( y="CREEL", x="iREC")
p <- p + scale_color_viridis_d(end = 0.8,option = "C")
p <- p + theme(legend.position="bottom")
p

#plot data by month
p <- ggplot(datnna,aes(y=CREEL, x=IREC,color=as.factor(MONTH)))
p <- p + geom_point(size=2, alpha=.5)
p <- p + geom_smooth(method = lm, formula= y~0+x, se     = FALSE, size   = 1, alpha  = .8) # to add regression line
p <- p + theme_bw(16)+labs( y="CREEL", x="iREC")
p <- p + scale_color_viridis_d(end = 0.8,option = "B")
p <- p + theme(legend.position="bottom")
p

#plot data by year
p <- ggplot(datnna,aes(y=CREEL, x=IREC,color=as.factor(YEAR)))
p <- p + geom_point(size=2, alpha=.5)
p <- p + geom_smooth(method = lm, formula= y~0+x, se     = FALSE, size   = 1, alpha  = .8) # to add regression line
p <- p + theme_bw(16)+labs( x="CREEL", y="iREC")
p <- p + scale_color_viridis_d(end = 0.8,option = "B")
p <- p + theme(legend.position="bottom")
p
#=============================



p <- ggplot(datnna,aes(CREEL))
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
dats <- datnna#[sample(seq_len(nrow(datnna)), nrow(datnna), replace = TRUE),]

summary(dats)
# This model is just a first attempt to get brm working
# Do not run when rendering as the results are not meaningful and it takes a loooong time to run
##QUestion: I am not sure baout the model formulation: 
# Should it be: IREC  ~ -1 + CREEL+ (-1 + CREEL  |LU_GROUPING3)
#Also: distribution? What should I consider? Lognormal (without 0s) an dhurdle lognormal are the 
#only ones that seem to converge, but diagnostics do not look good, data seems to be overdispersed (hurdle).
# and not fitting higher values with the positive only model.
#fit2 <- brm(formula= IREC  ~ -1 + s(MONTH, k=3) + (-1 + CREEL  |LU_GROUPING3) , 
#  data=dats, family=hurdle_lognormal, iter = 800,chains=2 )


#Try the poisson model
#round response to nearest integer
dats$CREELint<- round(dats$CREEL)

#Try the poisson model without the area hierarchical effect
fit1 <- brm(formula =  CREELint ~ -1 +  IREC  , 
  data=dats, family="poisson", iter = 3000,chains=3 )


summary(fit1)
#these are not possible because there is only one parameter.
pairs(fit1)
plot(fit1, ask = FALSE)

#Something is wrong here. I think it has something to do with the log link
#as I am getting simmilar patterns for all distributions with log link.
# The one prediction for Berkely in July gets a craxy high number, lixe 2x the highest observation. 

fitted_values <- fitted(fit1)
pred1<-predict(fit1)


plot(standata(fit1)$Y,pred1[,1])
dat <- as.data.frame(cbind(Y = standata(fit1)$Y, fitted_values))
ggplot(dat) + geom_point(aes(x = Y, y = Estimate))


dat[which.max(dat$Estimate),]
dats[dats$CREELint==dat$Y[which.max(dat$Estimate)],]
aggregate(datnna$CREEL,by=list(datnna$LU_GROUPING3),length)


conditional_effects(fit1, method="posterior_predict")
loofit1<-loo(fit1, save_psis = TRUE)
plot(loofit1)

yrep <- posterior_predict(fit1)

#model is overdispersed - the thick line should e uniform
#not optimal for discrete observations though
ppc_loo_pit_overlay(
  y = dats$IREC[!is.na(dats$CREEL)],
  yrep = yrep,
  lw = weights(loofit1$"psis_object")
)
 #===================================


#now with the area effect
fit2 <- brm(formula =  CREELint ~ -1 +  IREC +(-1 +  IREC |LU_GROUPING3) , 
  data=dats, family="poisson", iter = 3000,chains=3 )


summary(fit2)
#these look pretty good.
pairs(fit2)
plot(fit2, ask = FALSE)

fitted_values2 <- fitted(fit2)
pred2<-predict(fit2)

plot(standata(fit2)$Y,pred2[,1])
dat2 <- as.data.frame(cbind(Y = standata(fit2)$Y, fitted_values2))
ggplot(dat2) + geom_point(aes(x = Y, y = Estimate))

conditional_effects(fit2, method="posterior_predict")
loofit1<-loo(fit2, save_psis = TRUE)
plot(loofit1)

yrep <- posterior_predict(fit2)

#model is overdispersed - the thick line should be uniform
#I need to get a bit more guidance on how to interpret it
#not optimal for discrete observations though
ppc_loo_pit_overlay(
  y = dats$IREC[!is.na(dats$CREEL)],
  yrep = yrep,
  lw = weights(loofit1$"psis_object")
)


#====================================================
#model with prediction 




#things to look at:
#what is the observation w=resulting in the wonky predicted value. which area is it from, 
#if from area of few obs,  exclude the area and refit the model





#hurdle lognormal model
#flip the model 
#if the s(MONTH, k=3) term is included then loo() crashes R
fit2 <- brm(formula = bf( CREEL ~ -1 +  IREC + (-1 +  IREC |LU_GROUPING3), 
  hu ~ -1 +  IREC + (-1 +  IREC |LU_GROUPING3)),
  data=dats, family=hurdle_lognormal, iter = 1000,chains=2 )



?brm
dim(dats)

summary(fit2)
pairs(fit2)
plot(fit2, ask = FALSE)

#I do not undertand what is causing the extreme outliers in the predicted values

fitted_values <- fitted(fit2)
head(fitted_values)
dat <- as.data.frame(cbind(Y = standata(fit2)$Y, fitted_values))
ggplot(dat) + geom_point(aes(x = Estimate, y = Y))



#the problematic data 
dats[which.max(fitted_values[,1]),]



#this plot makes no sense to me:
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
