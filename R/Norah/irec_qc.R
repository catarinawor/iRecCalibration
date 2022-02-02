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



# irec raw data, comes in three separate sheets for different dates
#the sheets are in different formats so need to standardize
irec_raw <- read.csv(here::here("data/chinook responses_sheet1.csv")) %>% as_tibble()
irec_raw2 <- read.csv(here::here("data/chinook responses_sheet2.csv"))%>% as_tibble()
irec_raw3 <- read.csv(here::here("data/chinook responses_sheet3.csv"))%>% as_tibble()

# qc on raw3 and raw 2
names(irec_raw3) <- tolower(names(irec_raw3))
names(irec_raw2) <- tolower(names(irec_raw2))
irec_raw3 <- rename(irec_raw3, licence_id = surveykey)
irec_raw3$salmon_chinook_us_hatchery_kept<-as.integer(irec_raw3$salmon_chinook_us_hatchery_kept)
irec_raw3$salmon_chinook_us_hatchery_rele<-as.integer(irec_raw3$salmon_chinook_us_hatchery_rele)
irec_raw3$salmon_chinook_us_hatchery_rele[is.na(irec_raw3$salmon_chinook_us_hatchery_rele)] <- 0
irec_raw3$salmon_chinook_us_hatchery_kept[is.na(irec_raw3$salmon_chinook_us_hatchery_kept)] <- 0

irec_raw2$salmon_chinook_us_hatchery_kept<-0
irec_raw2$salmon_chinook_us_hatchery_rele<-0
irec_raw2$salmon_chinook_us_wild_kept<-0
irec_raw2$salmon_chinook_us_wild_rele<-0
irec_raw2$salmon_chinook_us_unkown_kept<-0
irec_raw2$salmon_chinook_us_unkown_rele<-0

  
# can combine these two sheets since they are of the same format
irec_raw2<-bind_rows(irec_raw2, irec_raw3)
irec_raw2<- irec_raw2 %>% rename(licence.id = licence_id, juveniles = totaljuveniles, lodge=fishedfromlodge, guided=fishedwithguide)


# Standardizing format
irec_raw2<- irec_raw2 %>% mutate(area = case_when(
                                area == "A001" ~ "Area 1",
                                area == "A002E" ~ "Area 2 East", 
                                area == "A002W" ~ "Area 2 West",
                                area == "A003" ~ "Area 3", 
                                area == "A004" ~ "Area 4",
                                area == "A005" ~ "Area 5", 
                                area == "A006" ~ "Area 6",
                                area == "A007" ~ "Area 7", 
                                area == "A008" ~ "Area 8",
                                area == "A009" ~ "Area 9", 
                                area == "A010" ~ "Area 10",
                                area == "A011" ~ "Area 11", 
                                area == "A012" ~ "Area 12", 
                                area == "A013" ~ "Area 13", 
                                area == "A014" ~ "Area 14", 
                                area == "A015" ~ "Area 15", 
                                area == "A016" ~ "Area 16", 
                                area == "A017" ~ "Area 17", 
                                area == "A018" ~ "Area 18", 
                                area == "A019MP" ~ "Area 19 Main Portion", 
                                area == "A019SI" ~ "Area 19 Saanich Inlet only", 
                                area == "A019" ~ "Area 19", 
                                area == "A020" ~ "Area 20", 
                                area == "A020E" ~ "Area 20 (East)", 
                                area == "A020W" ~ "Area 20 (West)", 
                                area == "A021" ~ "Area 21", 
                                area == "A022" ~ "Area 22", 
                                area == "A023AI" ~ "Area 23 Alberni Inlet", 
                                area == "A023BS" |  str_detect(area, "a023bs") ~ "Area 23 Barkley Sound", 
                                area == "A023" ~ "Area 23", 
                                area == "A024" ~ "Area 24", 
                                area == "A025" ~ "Area 25", 
                                area == "A026" ~ "Area 26", 
                                area == "A027" ~ "Area 27", 
                                area == "A028" ~ "Area 28", 
                                area == "A029GS" ~ "Area 29 Georgia Strait", 
                                area == "A029IR" ~ "Area 29 In River", 
                                area == "A101" ~ "Area 101", 
                                area == "A102" ~ "Area 102", 
                                area == "A103" ~ "Area 103", 
                                area == "A104" ~ "Area 104",
                                area == "A105" ~ "Area 105", 
                                area == "A106" ~ "Area 106",
                                area == "A107" ~ "Area 107", 
                                area == "A108" ~ "Area 108",
                                area == "A109" ~ "Area 109", 
                                area == "A110" ~ "Area 110",
                                area == "A111" ~ "Area 111",
                                area == "A121" ~ "Area 121",                                
                                area == "A123" ~ "Area 123",
                                area == "A124" ~ "Area 124",
                                area == "A125" ~ "Area 125",
                                area == "A126" ~ "Area 126",
                                area == "A127" ~ "Area 127",
                                area == "A130" ~ "Area 130",
                                area == "A142" ~ "Area 142",
                                TRUE ~ as.character(area))) %>% 
                     mutate(method = case_when(method=="angleboat" ~ "Angling from boat", 
                                               method=="angleshore" ~ "Angling from shore",
                                               method== "dive" ~ "Dive-based or other",
                                               TRUE ~ as.character(method)))

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



# Qc irec raw sheet 1
names(irec_raw) <- tolower(names(irec_raw))
irec_raw$day<-row.names(irec_raw)
irec_raw$month<-as.integer(irec_raw$month)
irec_raw$licence.id<-as.character(irec_raw$licence.id)
irec_raw$day<-as.integer(irec_raw$day)
irec_raw$salmon_chinook_us_hatchery_kept<-0
irec_raw$salmon_chinook_us_hatchery_rele<-0
irec_raw$salmon_chinook_us_wild_kept<-0
irec_raw$salmon_chinook_us_wild_rele<-0
irec_raw$salmon_chinook_us_unkown_kept<-0
irec_raw$salmon_chinook_us_unkown_rele<-0

# combining sheets post format changes, so they are all comparable
irec_raw_combined <-bind_rows(irec_raw, irec_raw2)
irec_raw_combined

# Incorporating Juveniles
irec_raw_combined$adult<-1
irec_raw_combined$num_people<-irec_raw_combined$adult+irec_raw_combined$juveniles
irec_raw_combined$total_released_pp<- (irec_raw_combined$salmon_chinook_subl_rele +  
                                       irec_raw_combined$salmon_chinook_hatch_rele + 
                                       irec_raw_combined$salmon_chinook_wild_rele + 
                                       irec_raw_combined$salmon_chinook_unk_rele + 
                                       irec_raw_combined$salmon_chinook_us_hatchery_rele + 
                                       irec_raw_combined$salmon_chinook_us_wild_rele +
                                       irec_raw_combined$salmon_chinook_us_unkown_rele)/
                                       irec_raw_combined$num_people

irec_raw_combined$total_kept_pp<- (irec_raw_combined$salmon_chinook_hatch_kept + 
                                   irec_raw_combined$salmon_chinook_wild_kept + 
                                   irec_raw_combined$salmon_chinook_unk_kept + 
                                   irec_raw_combined$salmon_chinook_us_hatchery_kept + 
                                   irec_raw_combined$salmon_chinook_us_wild_kept +
                                   irec_raw_combined$salmon_chinook_us_unkown_kept)/
                                   irec_raw_combined$num_people

irec_raw_combined$total_caught_pp<- (irec_raw_combined$salmon_chinook_subl_rele +  
                                     irec_raw_combined$salmon_chinook_hatch_rele + 
                                     irec_raw_combined$salmon_chinook_wild_rele + 
                                     irec_raw_combined$salmon_chinook_unk_rele + 
                                     irec_raw_combined$salmon_chinook_us_hatchery_rele + 
                                     irec_raw_combined$salmon_chinook_us_wild_rele +
                                     irec_raw_combined$salmon_chinook_us_unkown_rele +
                                     irec_raw_combined$salmon_chinook_hatch_kept + 
                                     irec_raw_combined$salmon_chinook_wild_kept + 
                                     irec_raw_combined$salmon_chinook_unk_kept + 
                                     irec_raw_combined$salmon_chinook_us_hatchery_kept + 
                                     irec_raw_combined$salmon_chinook_us_wild_kept +
                                     irec_raw_combined$salmon_chinook_us_unkown_kept)/
                                     irec_raw_combined$num_people

# Plotting 
p <- ggplot(irec_raw_combined,aes(y=total.chinook.caught, x=month, colour=as.factor(year)))
p <- p + geom_boxplot()+  facet_wrap(~area, scales="free")
p
 

problem_areas<-irec_raw_combined %>% filter(total_kept_pp>4) %>% distinct(area)
problem_areas <-as.list(problem_areas$area)

problem_areas_rele<-irec_raw_combined %>% filter(total_released_pp>20) %>% distinct(area)
problem_areas_rele <-as.list(problem_areas_rele$area)

kept_plot <- ggplot(irec_raw_combined,aes(y=total_kept_pp, x=as.factor(month), colour=as.factor(year)))
kept_plot <- kept_plot + geom_point()+ geom_hline(yintercept = 4)+ facet_wrap(~area, scales="free")
kept_plot
ggsave("Plots/kept_plot.png", kept_plot)

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


ggsave()

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

# QC
kept_high<-irec_raw_combined %>% filter(total_kept_pp>4)  %>% arrange(desc(total_kept_pp))
released_high<-irec_raw_combined %>% filter(total_released_pp>20)  %>% arrange(desc(total_released_pp))
total_high<-irec_raw_combined %>% filter(total_caught_pp>20)  %>% arrange(desc(total_caught_pp))

irec_liscences_flag<- irec_raw_combined %>% mutate(kept_high= case_when(total_kept_pp>4 ~ 1, TRUE ~ 0), 
                                                 released_high = case_when(total_released_pp>20 ~1, TRUE ~ 0), 
                                                 total_high = case_when(total_caught_pp>20 ~ 1, TRUE ~0), 
                                                 flag_count = kept_high + released_high + total_high, 
                                                 flag_day = case_when(flag_count >0 ~ 1, TRUE ~0)) %>% 
                                                 select (licence.id, kept_high, released_high, total_high, flag_count, flag_day) %>% 
                                                 group_by(licence.id) %>% 
                                                 summarise_if(is.numeric, sum) %>%       
                                                 filter(flag_count > 0) %>%       
                                                 arrange(desc(flag_count)) 
 

length(unique(irec_raw_combined$licence.id))
hist(irec_liscences_flag$flag_day)

#Summary table
explore_summary_irec <- data.frame(Issue_ID=character(), Issue=character(), Count=integer(),
                              Definition=character(), 
                              stringsAsFactors=FALSE)

explore_summary_irec <- explore_summary_irec  %>% 
  add_row(Issue_ID="1", Issue="High # Kept", Count=nrow(kept_high), Definition="Number of total chinook kept per person is over 4") %>% 
  add_row(Issue_ID="2", Issue="High # Released", Count=nrow(released_high), Definition="Number of total chinook released per person is over 20") %>% 
  add_row(Issue_ID="3", Issue="High # Caught", Count=nrow(total_high), Definition="Number of total chinook caught per person is over 20") %>% 
  add_row(Issue_ID="4", Issue="Licences w flags", Count=nrow(irec_liscences_flag), Definition="Licence.id has at least one of: high # kept, high # released or high # caught")

sheet_list_irec<-list(Summary=explore_summary_irec,
                 "1 - High_Kept"=kept_high, 
                 "2 - High_Released"=released_high,
                 "3 - High_Caught" = total_high,
                 "4 - Licence_flags" = irec_liscences_flag                               
                 )

writexl::write_xlsx(sheet_list_irec, path="irec_QC.xlsx")

# quantile(irec_raw_combined$total_kept_pp, na.rm=TRUE)

## problem is outlier analysis is saying everything is an outlier.... 
#might just be easier to say over this amount kept etc... 



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
irec_released_outliers<-irec_raw_combined  %>% filter(IsOutlier(total_released_pp)== "TRUE") 


irec_raw_combined  %>% anomalize(total_kept_pp) %>% filter(anomaly=="yes")

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