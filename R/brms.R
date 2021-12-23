#=======================================================================
# iREc calibration model fit experimentations
# Catarina Wor
# October 2021 
#Updated Dec 2021 - Based on Sean's explorations
#=======================================================================

library(dplyr)
library(plyr)

library(ggplot2)
library(glmmTMB)
library(DHARMa)
library(brms)
library(tidybayes)
library(gtools)

library("bayesplot")

source(here::here("R/format-data.R"))
dat <- format_data()

#see explore.R file. Sean anderson worked on a fes modesl using the data, and he reached the conclusion 
#that the model that best describes the data is a neg binom model for instances when irec is positive 
#and a similar model without the irec effect when irec is zero

dat$creel_orig <- dat$creel
#round creel obs so we can use negative binomial data
dat$creel <- round(dat$creel)
dat$log_irec1 <- log(10 + dat$irec)


dat_pos <- filter(dat, irec > 0)
dat_pos$log_irec <- log(dat_pos$irec)


dat_zero <- filter(dat, irec == 0)

unique(dat_pos $region)

unique(dat_zero$region)



ggplot(dat_pos, aes(x = irec, y = creel, color = region)) +
  geom_point(alpha = .5) +
  scale_x_log10() + scale_y_log10() +
  geom_smooth(formula = y ~ 1 + x, method = "lm", se = FALSE)


ggplot(dat_zero, aes(x = month, y = creel, color = region)) +
  geom_jitter(alpha = .5, height = 0)



m1.1 <- glmmTMB(creel ~ 1 + log_irec + poly(month, 2) + (1 + log_irec | region), data = dat_pos, family = nbinom1(), dispformula = ~log_irec)
summary(m1.1)

r <- DHARMa::simulateResiduals(m1.1, n = 500,  testOutliers(type = 'bootstrap'))
testOutliers(r, type = 'bootstrap')
plot(r)
 
#brms 
#center irec so that it's easier to put priors on the intercept
dat_pos$log_irec_cent <- dat_pos$log_irec - mean(dat_pos$log_irec)
dat_pos$irec_cent <- dat_pos$irec - mean(dat_pos$irec)




#resample the data

fit1 <- brm(
  bf(
    creel ~ log_irec_cent + s(month, k = 3) + 
      (log_irec_cent | region), 
    shape ~ log_irec_cent), 
  data = dat_pos, 
  family = negbinomial(),
  iter = 800, chains = 3, cores = 2
)



plot(fit1)

fixef(fit1)


yrep<-posterior_predict(fit1, draws = 500)
ppc_dens_overlay(dat_pos$creel,yrep) +xlim(1,10000)
ppc_stat_grouped(dat_pos$creel,yrep, group = dat_pos$region, stat = "max")

plot(conditional_smooths(fit1), rug = TRUE, ask = FALSE)

nd <- expand.grid(log_irec_cent = seq(min(dat_pos$log_irec_cent), max(dat_pos$log_irec_cent), length.out = 100),month = unique(dat_pos$month), region = unique(dat_pos$region))

pp1 <- posterior_linpred(fit1, newdata = nd, transform = TRUE, ndraws = 10)
lu1 <- data.frame(region = unique(dat_pos$region))

x <- tidybayes::add_linpred_draws(nd, fit1, ndraws = 100, transform = TRUE)
x %>% 
  ggplot(aes((log_irec_cent+mean(dat_pos$log_irec_cent)), .linpred, group = paste(region, .draw), colour = region)) +
  geom_line(alpha = 0.2) +
  geom_point(data = dat_pos, mapping = aes(x = (log_irec_cent+mean(dat_pos$log_irec_cent)), y = creel, colour = region), inherit.aes = FALSE) +
  facet_wrap(~month, scales = "free_y")


conditional_effects(fit1, method="posterior_predict")
loofit1<-loo(fit1, save_psis = TRUE)

get_variables(fit1)


#=============================================================

postdraws<-as_draws_df(fit1)
postdraws$datasample <- 0
#resample the data
for(i in 1:3){
  dats <- dat_pos[sample(seq_len(nrow(dat_pos)), nrow(dat_pos), replace = TRUE),]

  fit1 <- brm(
    bf(
      creel ~ log_irec_cent + s(month, k = 3) + 
        (log_irec_cent | region), 
      shape ~ log_irec_cent), 
    data = dats, 
    family = negbinomial(),
    iter = 800, chains = 3, cores = 2
  )

  #I need to figure out a way of weeding models that did not converge

  pd<-as_draws_df(fit1)
  pd$datasample <- i

  rbind.fill(postdraws,pd)
}











#Fit when irec is 

fit2 <- brm(
  bf(creel ~ s(month, k = 3) + (1 | region)), 
  data = dat_zero, 
  family = negbinomial(),
  iter = 600, chains = 2, cores = 2
)





fit2

plot(conditional_smooths(fit2), rug = TRUE, ask = FALSE)

nd <- expand.grid(month = seq(min(dat_zero$month), max(dat_pos$month), length.out = 100), region = unique(dat_zero$region))

pp2 <- posterior_linpred(fit2, newdata = nd, transform = TRUE, ndraws = 10)


lu <- data.frame(region = unique(dat_zero$region))


x <- tidybayes::add_linpred_draws(nd, fit2, ndraws = 100, transform = TRUE)
x %>% 
  ggplot(aes(month, .linpred, group = paste(region, .draw), colour = region)) +
  geom_line(alpha = 0.2) +
  geom_point(data = dat_zero, mapping = aes(x = month, y = creel, colour = region), inherit.aes = FALSE) +
  facet_wrap(~region, scales = "free_y")


nd1 <- expand.grid(
  month = unique(dat_pos$month), 
  region = unique(dat_pos$region),
  log_irec_cent = seq(min(dat_pos$log_irec_cent), max(dat_pos$log_irec_cent), length.out = 100)
)
nd1$log_irec <- nd1$log_irec_cent + mean(dat_pos$log_irec)
nd1$irec <- exp(nd1$log_irec)

x1 <- tidybayes::add_linpred_draws(nd1, fit1, ndraws = 100, transform = TRUE)
x1 %>% 
  filter(month == unique(dat_pos$month)[1]) %>% 
  ggplot(aes(irec, .linpred, group = paste(region, .draw), colour = region)) +
  geom_line(alpha = 0.2) +
  facet_wrap(~region, scales = "free_y") +
  scale_x_log10() +
  scale_y_log10()

x1 <- tidybayes::add_predicted_draws(nd1, fit1, ndraws = 100)
x1 %>% 
  filter(month == unique(dat_pos$month)[1]) %>% 
  ggplot(aes(irec, .prediction, group = paste(region, .draw), colour = region)) +
  geom_line(alpha = 0.2) +
  facet_wrap(~region, scales = "free_y")



#===========================
#histograms by groupings - of the data that is complete


#proportion of 0's in iredc

prop0<-function(x){sum(x==0)/length(x)}

aggregate(dat$creel,by=list(dat$month),prop0)
aggregate(dat$creel,by=list(dat$region),prop0)
aggregate(dat$creel,by=list(dat$year),prop0)


#===========================
#data plots





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


#dats$CREELint<- round(dats$CREEL)

#Try the gaussian model with the area hierarchical effect
#same as current model but just 
fit0 <- brm(formula =  CREEL ~ -1 +  IREC + (-1 +  IREC |LU_GROUPING3 ) , 
  data=dats, family="gaussian", iter = 1000,chains=3 )


summary(fit0)

#these are not possible because there is only one parameter.
pairs(fit0)
plot(fit0, ask = FALSE)

#Something is wrong here. I think it has something to do with the log link
#as I am getting simmilar patterns for all distributions with log link.
# The one prediction for Berkely in July gets a craxy high number, lixe 2x the highest observation. 

fitted_values0 <- fitted(fit0)
pred0<-predict(fit0)


plot(standata(fit0)$Y,pred0[,1])
#Seems like a bunch of positive vaues are predicted as zeroes 
dat <- as.data.frame(cbind(Y = standata(fit0)$Y, fitted_values0))
ggplot(dat) + geom_point(aes(x = Y, y = Estimate))

conditional_effects(fit0, method="posterior_predict")
loofit0<-loo(fit0, save_psis = TRUE)

plot(loofit0)

p <- pp_check(fit0,
    type = "stat",
    ndraws = 1000,
    stat = "mean"
  )

mcmc_areas(as.matrix(fit0$fit), regex_pars = "r_[^I]", 
  point_est = "mean", prob = 0.95, prob_outer = 0.99) + 
ggtitle("Posterior densities with means and 95% intervals") +
theme_bw() +
 theme(axis.text = element_text(size = 12), panel.grid = element_blank()) +
  xlab("Coefficient size")



#=============
#Try the poisson model
#round response to nearest integer
dats$CREELint<- round(dats$CREEL)
dats$row_number<-as.factor(1:length(dats$CREEL))
#the poisson model gave Rhat of 3.59! chains did not mix, everything looked terrible.
fit1 <- brm(formula =  CREELint ~ -1 +  IREC + (-1 +  IREC |LU_GROUPING3 )  , 
  data=dats, family="poisson", iter = 3000,chains=3 )

#Same thing happened to the zero inlated poisson. 
#Try the poisson model with the area hierarchical effect
#fit1 <- brm(formula =  CREELint ~ -1 +  IREC + (-1 +  IREC |LU_GROUPING3 )  , 
#  data=dats, family = zero_inflated_poisson(), iter = 3000,chains=3 )


#summary(fit1)
#these are not possible because there is only one parameter.
#pairs(fit1)
#plot(fit1, ask = FALSE)

#Something is wrong here. I think it has something to do with the log link
#as I am getting simmilar patterns for all distributions with log link.
# The one prediction for Berkely in July gets a craxy high number, lixe 2x the highest observation. 

fitted_values <- fitted(fit1)
red1<-predict(fit1)


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


#====================================================
#hurdle model 


#things to look at:
#what is the observation w=resulting in the wonky predicted value. which area is it from, 
#if from area of few obs,  exclude the area and refit the model


#hurdle lognormal model
#flip the model 
#if the s(MONTH, k=3) term is included then loo() crashes R
#- check on default priors
# Error: Sampling from priors is not possible as some parameters have no proper priors. Error occurred for parameter 'b'.
fit2prior <- brm(formula = bf( CREEL ~ -1 +  IREC + (-1 +  IREC |LU_GROUPING3), 
  hu ~ -1 +  IREC + (-1 +  IREC |LU_GROUPING3)),
  data=dats, family=hurdle_lognormal, iter = 200,chains=6, sample_prior = "only" ,
  prior = c(
    prior(normal(0, 10), class = sigma),
    prior(normal(0, 10), class = sd),
    prior(normal(0, 10), class = b_hu),
  prior(normal(0, 10), class = b)
  ))





fit2 <- brm(formula = bf( CREEL ~ -1 +  IREC + (-1 +  IREC |LU_GROUPING3), 
  hu ~ -1 +  IREC + (-1 +  IREC |LU_GROUPING3)),
  data=dats, family=hurdle_lognormal, iter = 1500,chains=3 )




summary(fit2)
pairs(fit2)
plot(fit2, ask = FALSE)

#I do not undertand what is causing the extreme outliers in the predicted values
#saved as fittedvals_hurdle_lognormal_noprior
fitted_values <- fitted(fit2)
head(fitted_values)
dat <- as.data.frame(cbind(Y = standata(fit2)$Y, fitted_values))
ggplot(dat) + geom_point(aes(x = Estimate, y = Y))



#the problematic data 
dats[which.max(fitted_values[,1]),]



#this plot makes no sense to me:
conditional_effects(fit2, method="posterior_predict")
#marginal_effects(fit2) #deprecated
p <- pp_check(fit2,
    type = "stat",
    ndraws = 1000,
    stat = "median"
  )



names(fit2$fit)
fit2$prior
mcmc_areas(as.matrix(fit2$fit), regex_pars = "r_[^I]", 
  point_est = "mean", prob = 0.95, prob_outer = 0.99) + 
ggtitle("Posterior densities with means and 95% intervals") +
theme_bw() +
 theme(axis.text = element_text(size = 12), panel.grid = element_blank()) +
  xlab("Coefficient size")

mcmc_areas(as.matrix(fit2$fit), regex_pars = "b_IREC",#"r_[^I]", 
  point_est = "mean", prob = 0.95, prob_outer = 0.99) + 
ggtitle("Posterior densities with means and 95% intervals") +
theme_bw() +
 theme(axis.text = element_text(size = 12), panel.grid = element_blank()) +
  xlab("Coefficient size")


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
  data_grid(LU_GROUPING3,IREC) %>%
  add_epred_draws(fit2) %>%
  ggplot(aes(x = .epred, y = LU_GROUPING3)) +
  stat_pointinterval(.width = c(.66, .95))
