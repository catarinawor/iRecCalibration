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
library(stringr)
library(ruler)
library(rstatix)

# source

source(here::here("R/format-data-NB.R"))

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


# irec data
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

# irec raw data, comes in three separate sheets for different dates
#the sheets are in different formats so need to standardize
irec_raw <- read.csv(here::here("data/chinook responses_sheet1.csv")) %>% as_tibble()
irec_raw2 <- read.csv(here::here("data/chinook responses_sheet2.csv"))%>% as_tibble()
irec_raw3 <- read.csv(here::here("data/chinook responses_sheet3.csv"))%>% as_tibble()
irec_raw3 <- rename(irec_raw3, Licence_ID = SurveyKey)

# can combine these two sheets since they are of the same format
irec_raw2<-bind_rows(irec_raw2, irec_raw3)

# Standardizing format
irec_raw2<- irec_raw2 %>% mutate(Area = case_when(
                                Area == "A001" ~ "Area 1",
                                Area == "A002E" ~ "Area 2 East", 
                                Area == "A002W" ~ "Area 2 West",
                                Area == "A003" ~ "Area 3", 
                                Area == "A004" ~ "Area 4",
                                Area == "A005" ~ "Area 5", 
                                Area == "A006" ~ "Area 6",
                                Area == "A007" ~ "Area 7", 
                                Area == "A008" ~ "Area 8",
                                Area == "A009" ~ "Area 9", 
                                Area == "A010" ~ "Area 10",
                                Area == "A011" ~ "Area 11", 
                                Area == "A012" ~ "Area 12", 
                                Area == "A013" ~ "Area 13", 
                                Area == "A014" ~ "Area 14", 
                                Area == "A015" ~ "Area 15", 
                                Area == "A016" ~ "Area 16", 
                                Area == "A017" ~ "Area 17", 
                                Area == "A018" ~ "Area 18", 
                                Area == "A019MP" ~ "Area 19 Main Portion", 
                                Area == "A019SI" ~ "Area 19 Saanich Inlet only", 
                                Area == "A019" ~ "Area 19", 
                                Area == "A020" ~ "Area 20", 
                                Area == "A020E" ~ "Area 20 (East)", 
                                Area == "A020W" ~ "Area 20 (West)", 
                                Area == "A021" ~ "Area 21", 
                                Area == "A022" ~ "Area 22", 
                                Area == "A023AI" ~ "Area 23 Alberni Inlet", 
                                Area == "A023BS" |  str_detect(Area, "a023bs") ~ "Area 23 Barkley Sound", 
                                Area == "A023" ~ "Area 23", 
                                Area == "A024" ~ "Area 24", 
                                Area == "A025" ~ "Area 25", 
                                Area == "A026" ~ "Area 26", 
                                Area == "A027" ~ "Area 27", 
                                Area == "A028" ~ "Area 28", 
                                Area == "A029GS" ~ "Area 29 Georgia Strait", 
                                Area == "A029IR" ~ "Area 29 In River", 
                                Area == "A101" ~ "Area 101", 
                                Area == "A102" ~ "Area 102", 
                                Area == "A103" ~ "Area 103", 
                                Area == "A104" ~ "Area 104",
                                Area == "A105" ~ "Area 105", 
                                Area == "A106" ~ "Area 106",
                                Area == "A107" ~ "Area 107", 
                                Area == "A108" ~ "Area 108",
                                Area == "A109" ~ "Area 109", 
                                Area == "A110" ~ "Area 110",
                                Area == "A111" ~ "Area 111",
                                Area == "A121" ~ "Area 121",                                
                                Area == "A123" ~ "Area 123",
                                Area == "A124" ~ "Area 124",
                                Area == "A125" ~ "Area 125",
                                Area == "A126" ~ "Area 126",
                                Area == "A127" ~ "Area 127",
                                Area == "A130" ~ "Area 130",
                                Area == "A142" ~ "Area 142",
                                TRUE ~ as.character(Area))) %>% 
                     mutate(Method = case_when(Method=="angleboat" ~ "Angling from boat", 
                                               Method=="angleshore" ~ "Angling from shore",
                                               Method== "dive" ~ "Dive-based or other",
                                               TRUE ~ as.character(Method)))


irec_raw<- irec_raw %>% mutate(Month = case_when(
                               Month == "January" ~ "1",
                               Month == "February" ~ "2",
                               Month == "March" ~ "3",
                               Month == "April" ~ "4",
                               Month == "May" ~ "5",
                               Month == "June" ~ "6",
                               Month == "july" |Month == "July" ~ "7",
                               Month == "August" ~ "8",
                               Month == "september" |Month == "September" ~ "9",
                               Month == "October" ~ "10",
                               Month == "November" ~ "11",
                               Month == "December" ~ "12",
                               TRUE ~ as.character(Month)))
irec_raw$day<-row.names(irec_raw)

names(irec_raw) <- tolower(names(irec_raw))
names(irec_raw2) <- tolower(names(irec_raw2))
irec_raw2<- irec_raw2 %>% rename(licence.id = licence_id, juveniles = totaljuveniles, lodge=fishedfromlodge, guided=fishedwithguide)
irec_raw$month<-as.integer(irec_raw$month)
irec_raw$licence.id<-as.character(irec_raw$licence.id)
irec_raw$day<-as.integer(irec_raw$day)


# combining sheets post format changes, so they are all comparable
irec_raw_combined <-bind_rows(irec_raw, irec_raw2)
irec_raw_combined

irec_raw_combined$adult<-1
irec_raw_combined$num_people<-irec_raw_combined$adult+irec_raw_combined$juveniles
irec_raw_combined$total_released_pp<- (irec_raw_combined$salmon_chinook_subl_rele +  irec_raw_combined$salmon_chinook_hatch_rele + irec_raw_combined$salmon_chinook_wild_rele + irec_raw_combined$salmon_chinook_unk_rele)/irec_raw_combined$num_people
irec_raw_combined$total_kept_pp<- (irec_raw_combined$salmon_chinook_hatch_kept + irec_raw_combined$salmon_chinook_wild_kept + irec_raw_combined$salmon_chinook_unk_kept)/irec_raw_combined$num_people

# this needs to be done by day. 
# incorporate juveniles

View(irec_raw_combined)

p <- ggplot(irec_raw_combined,aes(y=total.chinook.caught, x=month, colour=as.factor(year)))
p <- p + geom_boxplot()+  facet_wrap(~area, scales="free")
p
 
p <- ggplot(irec_raw_combined,aes(y=total_kept_pp, x=as.factor(month), colour=as.factor(year)))
p <- p + geom_point()+  facet_wrap(~area, scales="free")
p

p <- ggplot(irec_raw_combined,aes(y=total_released_pp, x=as.factor(month), colour=as.factor(year)))
p <- p + geom_point()+  facet_wrap(~area, scales="free")
p

p <- ggplot(irec_raw_combined,aes(y=total_kept_pp, x=as.factor(year), colour=as.factor(year)))
p <- p + geom_boxplot()
p
## problem is outlier analysis is saying everything is an outlier.... 
#might just be easier to say over this amount kept etc... 


p <- ggplot(irec_raw_summed_all %>% filter(disposition=="Released"),aes(y=response, x=month, colour=as.factor(year)))
p <- p + geom_boxplot()+  facet_wrap(~area, scales="free")
p

# QC
kept_high<-irec_raw_combined %>% filter(total_kept_pp>4)  %>% arrange(desc(total_kept_pp))
released_high<-irec_raw_combined %>% filter(total_released_pp>20)  %>% arrange(desc(total_released_pp))


#Summary table
explore_summary_irec <- data.frame(Issue_ID=character(), Issue=character(), Count=integer(),
                              Definition=character(), 
                              stringsAsFactors=FALSE)

explore_summary_irec <- explore_summary_irec  %>% 
  add_row(Issue_ID="1", Issue="High # Kept", Count=nrow(kept_high), Definition="Number of total chinook kept per person is over 4") %>% 
  add_row(Issue_ID="2", Issue="High # Released", Count=nrow(released_high), Definition="Number of total chinook released per person is over 20")
  
sheet_list_irec<-list(Summary=explore_summary_irec,
                 "1 - High_Kept"=kept_high, 
                 "1 - High_Released"=released_high
                                                )

writexl::write_xlsx(sheet_list_irec, path="irec_QC.xlsx")

quantile(irec_raw_combined$total_kept_pp, na.rm=TRUE)

# Is extreme?
IsExtreme <- function(data) {
  lowerq = quantile(data, na.rm = TRUE)[2]
  upperq = quantile(data, na.rm = TRUE)[4]
  iqr = upperq - lowerq 
  threshold_upper = (iqr * 1.5) + upperq
  # threshold_lower = lowerq - (iqr * 1.5)
  data > threshold_upper 
}


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

# might need tot ake out zeros for this to work.... 
# this gives you 5000 days
irec_outliers<-irec_raw_combined %>% select(licence.id, year, area, month, method, total_released_pp, total_kept_pp) %>% 
                      group_by(year, area, month, method) %>% 
                      mutate_if(is.numeric, isnt_out_funs)

View(irec_outliers)
#Explorations
irec_kept_outliers<-irec_raw_combined %>% filter(identify_outliers(total_kept_pp))
irec_released_outliers<-irec_raw_combined %>% filter() %>% group_by(month, area, method) %>% filter(IsOutlier(total_released_pp)== "TRUE") 

View(irec_released_outliers)

# identify_outliers() from the rstatix package is good, also have is.extreme
irec_kept_outliers<-irec_raw_combined %>%  filter(total_kept_pp>1) %>% group_by(year, month, area, method) %>%  filter(is_extreme(total_kept_pp))
irec_released_outliers<-irec_raw_combined %>%  filter(total_released_pp>1) %>% group_by(year, month, area, method) %>%  filter(is_extreme(total_released_pp))



View(irec_kept_outliers)


# Comparing irec raw to irec estimated:
irec_raw_summed<-irec_raw_combined %>% filter(method == "Angling from boat") %>% group_by(year, month, area) %>% 
                                       summarise(Kept = sum(total_kept), 
                                                 Released = sum(total_released)) 
                                                

irec_raw_summed<-irec_raw_summed %>% pivot_longer(c(Kept, Released), names_to="disposition", values_to="response")


irec_raw_summed <- irec_raw_summed %>% mutate(area = case_when(
                                              area == "Area 19 Main Portion" ~ "Area 19 (JDF)", 
                                              area == "Area 19 Saanich Inlet only" ~ "Area 19 (GS)", 
                                              area == "Area 23 Alberni Inlet" ~ "Area 23 (Alberni Canal)", 
                                              area == "Area 23 Barkley Sound" ~ "Area 23 (Barkley)", 
                                              area == "Area 2 East" ~ "Area 2E", 
                                              area == "Area 2 West" ~ "Area 2W", 
                                              area == "Area 29 Georgia Strait" ~ "Area 29 (Marine)", 
                                              area == "Area 29 In River" ~ "Area 29 (In River)", 
                                              TRUE ~ as.character(area)))
                                              
###need to expand the irec_raw_summed to zeros
allobs_raw <- expand.grid(list(
  area = unique(irec_raw_summed$area),
  year = unique(irec_raw_summed$year),
  month = unique(irec_raw_summed$month),
  disposition = unique(irec_raw_summed$disposition)
))

#create zero observations, with 0 variance
irec_raw_summed_all <- left_join(allobs_raw, irec_raw_summed)
irec_raw_summed_all$response[is.na(irec_raw_summed_all$response)] <- 0
irec_raw_summed_all<- as_tibble(irec_raw_summed_all)
irec_raw_summed_all

#ireccc - this is the expanded one with all the zeros - make sure to include 2012
names(ireccc) <- tolower(names(ireccc))
irec_calculated_summed<-ireccc %>% select (- lu_grouping3) %>% group_by(year, month, area, disposition) %>% 
  summarise(estimate = sum(irec), estimate_var = sum(sdirec)) 
irec_calculated_summed
# Combine the two expanded ones
irec_compare<-merge(irec_calculated_summed, irec_raw_summed_all, all=TRUE) %>% as_tibble()

p <- ggplot(irec_compare) +
  geom_point(aes(x = estimate, y = total.chinook.caught, color=as.factor(year)), size = 2, alpha = .5) +
  theme_bw(16)+
  scale_color_viridis_d(end = 0.8, option = "C") +
  geom_abline(slope = 1, intercept = 0) +
  theme(legend.position = "bottom")
p


p <- ggplot(irec_compare ,aes(y=response, x=estimate,fill=as.factor(year), color=as.factor(year), shape=disposition, linetype=disposition))
p <- p + geom_point(size=2, alpha=.5)
p <- p + geom_smooth(method = lm, formula= y~x,  size = 2, alpha  = .2) # to add regression line
p <- p + theme_bw(16)+labs( y="irec response", x="irec estimate expanded")
p <- p + scale_color_viridis_d(end = 0.8,option = "B")+ scale_fill_viridis_d(end = 0.8,option = "B")
p <- p + theme(legend.position="bottom")
p












# Imputation

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