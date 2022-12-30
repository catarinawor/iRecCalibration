#============================================================
#Explore model fits for irec-creel calibration
# Catarina Wor summary based on Sean Anderson explore.R file
#===========================================================


library(dplyr)
library(ggplot2)
library(glmmTMB)
library(DHARMa)
library(brms)

here::here()

source(here::here("R","format-data.R"))
dat <- format_data()


#transformations to investigate
dat$creel_orig <- dat$creel
dat$creel <- round(dat$creel)


dev.new()

p <- ggplot(dat, aes(x = irec, y = creel, color = region)) +
  geom_point(aes(shape = disposition), size = 2, alpha = .5) +
  geom_smooth(formula = y ~ 0 + x, method = "lm", se = FALSE)+
  theme_bw(16) +
  labs(y = "CREEL", x = "iREC") +
  scale_color_viridis_d(end = 0.8, option = "C") +
  geom_abline(slope = 1, intercept = 0) +
  theme(legend.position = "bottom")
 
p



summary(dat)
dat$dum<-seq_len(nrow(dat))

dat_zero_irec<-dat[dat$irec==0,]
#let's exclude 0 estimates of iRec
dat_pos <- filter(dat, irec > 0&year==2019)

 ggplot(dat, aes(x = irec, y = creel, color = region)) +
  geom_point(aes(shape = disposition), size = 2, alpha = .5) +
  geom_smooth(formula = y ~   x, method = "lm", se = FALSE)+
  theme_bw(16) +
  labs(y = "CREEL", x = "iREC") +
  scale_color_viridis_d(end = 0.8, option = "C") +
  geom_abline(slope = 1, intercept = 0) +
  theme(legend.position = "bottom")
 

#base case model
m <- glmmTMB(creel ~ 0+ as.factor(year)+ (0+irec|dum),
             data = dat_pos)
summary(m)
bm <- DHARMa::simulateResiduals(m, n = 1000)
plotResiduals(bm, form = dat_pos$irec)
plot(bm)
testDispersion(bm)
testZeroInflation(bm)


#add covariates
#fit seems worse
#m2 <- glmmTMB(creel ~ 1 + poly(month, 2) + (1 + irec | region), data = dat, dispformula = ~irec)
m2 <- glmmTMB(creel ~ 1 + poly(month, 2) + ( 1+ irec | region), data = dat_pos,dispformula = ~irec)

AIC(m,m2)

bm2 <- DHARMa::simulateResiduals(m2, n = 1000)
plotResiduals(bm2, form = dat_pos$irec)

plot(bm2)
testDispersion(bm2)
testZeroInflation(bm2)

#best model based on results from Sean's explore.R


dat_pos$log_irec <- log(dat_pos$irec) 
summary(dat_pos)



ggplot(dat_pos, aes(x = irec, y = creel, color = region)) +
  geom_point(alpha = .5) +
  scale_x_log10() + scale_y_log10() +
  geom_smooth(formula = y ~ 1 + x, method = "lm", se = FALSE)



dat_pos$disposition <-as.factor(dat_pos$disposition)

m1.2 <- glmmTMB(creel ~ 1 + log_irec + poly(month, 2) + (1 + log_irec | region), 
                data = dat_pos, family = nbinom1(), dispformula = ~log_irec)


m1.2 <- glmmTMB(creel ~ 1 + log_irec + (1 + log_irec | region), 
                data = dat_pos, family = nbinom1(), dispformula = ~log_irec)




r <- DHARMa::simulateResiduals(m1.2, n = 1000)
plot(r)
testDispersion(r)
testZeroInflation(r)



#compare models with and without the cv 
dat_pos$log_irec_cent <- dat_pos$log_irec - mean(dat_pos$log_irec)
dat_pos$irec_cv <- dat_pos$sdirec/ dat_pos$irec

fit1 <- brm(
  bf(
    creel ~   me(log_irec_cent,irec_cv) +
      (me(log_irec_cent,irec_cv) | region), 
    shape ~ log_irec_cent), 
  data = dat_pos, 
  family = negbinomial(),
  iter = 600, chains = 2, cores = 2
)

fit1

nd1 <- expand.grid(   
  region = unique(dat_pos$region),
  log_irec_cent = seq(min(dat_pos$log_irec_cent), max(dat_pos$log_irec_cent), length.out = 100),
  irec_cv = mean(dat_pos$irec_cv))
  #seq(min(dat_pos$irec_cv), max(dat_pos$irec_cv), length.out = 100))
 

nd1$log_irec <- nd1$log_irec_cent + mean(dat_pos$log_irec)
nd1$irec <- exp(nd1$log_irec)

x1 <- tidybayes::add_linpred_draws(nd1, fit1, ndraws = 100, transform = TRUE)
  
  ggplot(data=x1) +
  geom_line(aes(irec, .linpred, group = paste(region, .draw), colour = region),alpha = 0.2) +
  geom_point(data=dat_pos,aes(x=irec,y=creel))+
  facet_wrap(~region) +
  scale_x_log10() +
  scale_y_log10()

x1 <- tidybayes::add_predicted_draws(nd1, fit1, ndraws = 100)
x1 %>% 
  ggplot(aes(irec, .prediction, group = paste(region, .draw), colour = region)) +
  geom_line(alpha = 0.2) +
  facet_wrap(~region, scales = "free_y")+
  scale_x_log10() +
  scale_y_log10()


#questions about this model:
#log irec is the best predictor of log creel (?) -- as opposed to irec->creel
#what should we do when irec is 0? use raw creel? and assume 0 when creel is absent?
# 


#laternative formulations
fit2 <- brm(
  bf(
    creel ~ me(log_irec_cent,irec_cv) + s(month, k = 3) + 
      (me(log_irec_cent,irec_cv) | region), 
    shape ~ log_irec_cent), 
  data = dat_pos, 
  family = negbinomial(),
  iter = 2000, chains = 4, cores = 2
)

