


# source(here::here("R/format-data-NB.R"))


# irec to creel -----------------------------------------------------------


# are there PFMA levels in creel not in irec? 
creel_levels<-levels(as.factor(creel$PFMA))
irec_levels<-levels(as.factor(irec$AREA))

creel_levels  %in% irec_levels
setdiff(creel_levels, irec_levels)


irec_levels %in% creel_levels

set.diff<-setdiff(irec_levels,creel_levels)
length(setdiff(irec_levels,creel_levels))

arealu %>% filter(AREA %in% set.diff) %>% arrange(LU_GROUPING3)

# survey
creel_survey<- creel %>% mutate(SURVEY = case_when(
  Include..20. == "Y" ~ 2,
  Include..15. == "Y" ~ 3,
  TRUE ~ 4
)) %>% group_by(PFMA, YEAR, MONTH, TYPE, SURVEY) %>% 
  summarise(ESTIMATE = sum(ESTIMATE),SURVEY = mean(SURVEY)) 



# Irec estimates ----------------------------------------------------------

# plotting irec estimates data
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


# Plotting kept and releases ----------------------------------------------
library(ggh4x)
library(patchwork)


problem_areas<-irec_raw_combined %>% filter(total_kept_pp>4) %>% distinct(area)
problem_areas <-as.list(problem_areas$area)

problem_areas_rele<-irec_raw_combined %>% filter(total_released_pp>20) %>% distinct(area)
problem_areas_rele <-as.list(problem_areas_rele$area)
# ~region + area, strip=strip_nested(bleed=FALSE)

# facet_nested_wrap
kept_plot <- ggplot(irec_raw_combined,aes(y=total_kept_pp, x=as.factor(month), colour=as.factor(year)))
kept_plot <- kept_plot + geom_point()+ geom_hline(yintercept = 4)+ facet_nested_wrap(vars(region, area), scales="free", ncol = 15)
kept_plot
ggsave("Plots/kept_plot.png", kept_plot)

# facet_wrap2
kept_plot <- ggplot(irec_raw_combined,aes(y=total_kept_pp, x=as.factor(month), colour=as.factor(year)))
kept_plot <- kept_plot + geom_point()+ geom_hline(yintercept = 4)+ facet_wrap2(vars(region, area), scales="free", strip=strip_nested(bleed=FALSE), ncol = 15)
kept_plot

# facet_manual
kept_plot <- ggplot(irec_raw_combined,aes(y=total_kept_pp, x=as.factor(month), colour=as.factor(year)))
kept_plot <- kept_plot + geom_point()+ geom_hline(yintercept = 4)+ facet_manual(vars(region, area), scales="free", strip=strip_nested(bleed=FALSE), design=layout4)
kept_plot

# p1<- p1  + plot_annotation(tag_levels = list(c('West Coast Vancouver Island')))

# p2<-p2 + ggtitle( 'Johnstone Strait          Georgia Strait')
# p2.1<-wrap_plots(ap_kept_jst, ncol=3)+  ggtitle(title = 'Johnstone Strait')
# p2.2<-wrap_plots(ap_kept_gst, ncol=12)+  ggtitle(title = 'Georgia Strait')

# p3<- p3 +  ggtitle(title = 'Juan de Fuca               Central BC')
# p3.1<-wrap_plots(ap_kept_jdf, ncol=5)+ ggtitle(title = 'Juan de Fuca')
# p3.2<-wrap_plots(ap_kept_cbc, ncol=10) +  ggtitle(title = 'Central BC') 
# p5<- p5 +  ggtitle(title = 'Northern BC')
# p6<-p5 + guide_area()
#patchwork<- p1 / (p2.1 + p2.2) / (p3.1 + p3.2) / p6 
patchwork<-p1 / p2 / p3 / p6 

patchwork + plot_layout(guides = 'collect') 

# +  
theme(plot.title.position = c(0, 1),
      plot.title = element_text(size = 8, hjust = 0, vjust = 0))


#hjust
?plot.title
#+ plot_annotation(tag_levels = list(c('WCVI', 'JST, GST', 'JDF,CBC','NBC'), ''))

# wrap_plots(c(p1,p2,p3,p6), nrow=4, tag_level = 'new')+ plot_layout(guides = 'collect') + plot_annotation(tag_levels = 'A')


# just zoom in on the area that contain a >4 value
kept_plot_filt <- ggplot(irec_raw_combined %>% filter(area %in% problem_areas),aes(y=total_kept_pp, x=as.factor(month), colour=as.factor(year)))
kept_plot_filt <- kept_plot_filt + geom_point()+ geom_hline(yintercept = 4)+ facet_wrap(~area, scales="free")
kept_plot_filt
ggsave("Plots/kept_plot_filt.png", kept_plot_filt)


released_plot <- ggplot(irec_raw_combined,aes(y=total_released_pp, x=as.factor(month), colour=as.factor(year)))
released_plot <- released_plot + geom_point()+ geom_hline(yintercept = 20)+ facet_wrap(~area, scales="free")
released_plot
ggsave("Plots/released_plot.png", released_plot)


released_plot_filt <- ggplot(irec_raw_combined %>% filter(area %in% problem_areas_rele),aes(y=total_released_pp, x=as.factor(month), colour=as.factor(year)))
released_plot_filt <- released_plot_filt + geom_point()+ geom_hline(yintercept = 20)+ facet_wrap(~area, scales="free")
released_plot_filt
ggsave("Plots/released_plot_filt.png", released_plot_filt)

sublegal_released_plot <- ggplot(irec_raw_combined,aes(y=sublegal_released_pp, x=as.factor(month), colour=as.factor(year)))
sublegal_released_plot <- sublegal_released_plot + geom_point()+ geom_hline(yintercept = 20)+ facet_wrap(~area, scales="free")
sublegal_released_plot

legal_released_plot <- ggplot(irec_raw_combined,aes(y=legal_released_pp, x=as.factor(month), colour=as.factor(year)))
legal_released_plot <- legal_released_plot + geom_point()+ geom_hline(yintercept = 20)+ facet_wrap(~area, scales="free")
legal_released_plot


p <- ggplot(irec_raw_combined,aes(y=total_released_pp, x=as.factor(month), colour=as.factor(year)))
p <- p + geom_point()+  facet_wrap(~area, scales="free")
p

p <- ggplot(irec_raw_combined,aes(y=total_kept_pp, x=as.factor(year), colour=as.factor(year)))
p <- p + geom_boxplot()
p

p <- ggplot(irec_raw_combined,aes(y=total_released_pp, x=as.factor(year), colour=as.factor(year)))
p <- p + geom_boxplot()
p


p <- ggplot(irec_raw_summed_all %>% filter(disposition=="Released"),aes(y=response, x=month, colour=as.factor(year)))
p <- p + geom_boxplot()+  facet_wrap(~area, scales="free")
p


# Outlier analysis --------------------------------------------------------



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


# Plotting layout ---------------------------------------------------------


layout <- '
ABCDEFGHIJKLMNO
ABCDEFGHIJKLMNO
ABCD#FGHIJKLMNO
ABCDEFGHIJKLMNO
'
layout2 <- '
AAAAAAAAAAAAAAA
BBBCCCCCCCCCCCC
DDDD#EEEEEEEEEE
FFFFFFFFFFFFFFF
'
layout3<- layout(matrix(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                          2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,
                          4,4,4,4,0,5,5,5,5,5,5,5,5,5,5,
                          6,6,6,6,6,6,6,6,6,6,6,6,6,6,0
), 4, 15, byrow = TRUE))

layout4<- layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
                          16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,
                          31,32,33,34,0,35,36,37,38,39,40,41,42,43,44,
                          45,46,47,48,49,50,51,52,53,54,55,56,57,58, 0), 4, 15, byrow = TRUE))




# Imputation --------------------------------------------------------------




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
