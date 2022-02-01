# outlier stuff

# outlier by zscores
isnt_out_z <- function(x, thres = 3, na.rm = TRUE) {
  abs(x - mean(x, na.rm = na.rm)) <= thres * sd(x, na.rm = na.rm)
}

#outlier by median absolute deviation 
isnt_out_mad <- function(x, thres = 3, na.rm = TRUE) {
  abs(x - median(x, na.rm = na.rm)) <= thres * mad(x, na.rm = na.rm)
}

# tukey
isnt_out_tukey <- function(x, k = 1.5, na.rm = TRUE) {
  quar <- quantile(x, probs = c(0.25, 0.75), na.rm = na.rm)
  iqr <- diff(quar)
  
  (quar[1] - k * iqr <= x) & (x <= quar[2] + k * iqr)
}

isnt_out_funs <- funs(
  z = isnt_out_z,
  mad = isnt_out_mad,
  tukey = isnt_out_tukey
)



### find the outliers https://www.r-bloggers.com/2017/12/combined-outlier-detection-with-dplyr-and-ruler/
IsOutlier <- function(data) {
  lowerq = quantile(data, na.rm = TRUE)[2]
  upperq = quantile(data, na.rm = TRUE)[4]
  iqr = upperq - lowerq 
  threshold_upper = (iqr * 1.5) + upperq
  # threshold_lower = lowerq - (iqr * 1.5)
  data > threshold_upper 
}

# took out | data < threshold lower

# outlier by zscores
isnt_out_z <- function(x, thres = 3, na.rm = TRUE) {
  abs(x - mean(x, na.rm = na.rm)) <= thres * sd(x, na.rm = na.rm)
}

#outlier by median absolute deviation 
isnt_out_mad <- function(x, thres = 3, na.rm = TRUE) {
  abs(x - median(x, na.rm = na.rm)) <= thres * mad(x, na.rm = na.rm)
}

# tukey
isnt_out_tukey <- function(x, k = 1.5, na.rm = TRUE) {
  quar <- quantile(x, probs = c(0.25, 0.75), na.rm = na.rm)
  iqr <- diff(quar)
  
  (quar[1] - k * iqr <= x) 
  # & (x <= quar[2] + k * iqr)
}

isnt_out_funs <- funs(
  z = isnt_out_z,
  mad = isnt_out_mad,
  tukey = isnt_out_tukey
)


# this gives you 5000 days
irec_outliers<-irec_raw_combined %>% select(licence.id, year, area, month, method, total_released_pp, total_kept_pp) %>% 
  group_by(year, area, month, method) %>% 
  mutate_if(is.numeric, isnt_out_funs)

View(irec_outliers)
#Explorations
irec_kept_outliers<-irec_raw_combined %>% filter(IsOutlier(total_kept_pp))
irec_released_outliers<-irec_raw_combined %>% group_by(month, area, method) %>% filter(IsOutlier(total_released_pp)== "TRUE") 

View(irec_released_outliers)






creel_levels<-levels(as.factor(creel$PFMA))
irec_levels<-levels(as.factor(irec$AREA))

creel_levels  %in% irec_levels
setdiff(creel_levels, irec_levels)


irec_levels %in% creel_levels

set.diff<-setdiff(irec_levels,creel_levels)
length(setdiff(irec_levels,creel_levels))

arealu %>% filter(AREA %in% set.diff) %>% arrange(LU_GROUPING3)


creel_survey<- creel %>% mutate(SURVEY = case_when(
  Include..20. == "Y" ~ 2,
  Include..15. == "Y" ~ 3,
  TRUE ~ 4
)) %>% group_by(PFMA, YEAR, MONTH, TYPE, SURVEY) %>% 
  summarise(ESTIMATE = sum(ESTIMATE),SURVEY = mean(SURVEY)) 



irec <- read.csv(here::here("data/iRecchinook_2012_2021.csv"))
irec<-irec %>% as_tibble()

p <- ggplot(irec %>% filter(AREA=="Area 2"),aes(x=ESTIMATE))
p <- p + stat_bin(colour = "gray", alpha = 0.5, position = "identity", aes(y = ..density..)) + geom_density(fill = NA, size=1)
p <- p + theme_bw(16)
p <- p + theme(legend.position="bottom")
p

p <- ggplot(irec,aes(y=ESTIMATE, x=MONTH, colour=as.factor(YEAR), shape=DISPOSITION))
p <- p + geom_point()+  facet_wrap(~AREA, scales="free")
p


p <- ggplot(irec ,aes(x=ESTIMATE))
p <- p + stat_bin(colour = "gray", alpha = 0.5, position = "identity", aes(y = ..density..)) + geom_density(fill = NA, size=1)
p <- p + theme_bw(16)+xlim(1000,60000)
p <- p + theme(legend.position="bottom")
p

p <- ggplot(irec,aes(y=ESTIMATE, x=AREA, colour=DISPOSITION))
p <- p + geom_point() + coord_flip()
p

ireccc
p <- ggplot(ireccc,aes(y=IREC, x=AREA, colour=DISPOSITION))
p <- p + geom_point() + coord_flip()
p









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