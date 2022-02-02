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


# Loading -----------------------------------------------------------------

# irec raw data, comes in three separate sheets for different dates
#the sheets are in different formats so need to standardize
irec_raw <- read.csv(here::here("data/chinook responses_sheet1.csv")) %>% as_tibble()
irec_raw2 <- read.csv(here::here("data/chinook responses_sheet2.csv"))%>% as_tibble()
irec_raw3 <- read.csv(here::here("data/chinook responses_sheet3.csv"))%>% as_tibble()


# Formatting --------------------------------------------------------------

# formatting raw 1
names(irec_raw) <- tolower(names(irec_raw))
irec_raw<- irec_raw %>%  mutate_at(c("lodge", "guided", "month"), function(x) tolower(as.character(x)))
irec_raw<- irec_raw %>% mutate(month = case_when(
                                       month == "january" ~ "1",
                                       month == "february" ~ "2",
                                       month == "march" ~ "3",
                                       month == "april" ~ "4",
                                       month == "may" ~ "5",
                                       month == "june" ~ "6",
                                       month == "july"  ~ "7",
                                       month == "august" ~ "8",
                                       month == "september" ~ "9",
                                       month == "october" ~ "10",
                                       month == "november" ~ "11",
                                       month == "december" ~ "12",
                                       TRUE ~ as.character(month)))
irec_raw$month<-as.integer(irec_raw$month)
irec_raw$licence.id<-as.character(irec_raw$licence.id)
irec_raw$day<-row.names(irec_raw)
irec_raw$day<-as.integer(irec_raw$day)
irec_raw$salmon_chinook_us_hatchery_kept<-0
irec_raw$salmon_chinook_us_hatchery_rele<-0
irec_raw$salmon_chinook_us_wild_kept<-0
irec_raw$salmon_chinook_us_wild_rele<-0
irec_raw$salmon_chinook_us_unkown_kept<-0
irec_raw$salmon_chinook_us_unkown_rele<-0


# formatting raw 2 and 3
names(irec_raw3) <- tolower(names(irec_raw3))
names(irec_raw2) <- tolower(names(irec_raw2))
irec_raw2<- irec_raw2 %>%  mutate_if(is.character, function(x) tolower(as.character(x)))
irec_raw3<- irec_raw3 %>%  mutate_if(is.character, function(x) tolower(as.character(x)))
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

  
# Combining raw 2 and raw 3 - now same format
irec_raw23<-bind_rows(irec_raw2, irec_raw3)
irec_raw23<- irec_raw23 %>% rename(licence.id = licence_id, juveniles = totaljuveniles, lodge=fishedfromlodge, guided=fishedwithguide)

# Standardizing format
irec_raw23<- irec_raw23 %>% mutate(area = case_when(
                                area == "a001" ~ "Area 1",
                                area == "a002e" ~ "Area 2 East", 
                                area == "a002w" ~ "Area 2 West",
                                area == "a003" ~ "Area 3", 
                                area == "a004" ~ "Area 4",
                                area == "a005" ~ "Area 5", 
                                area == "a006" ~ "Area 6",
                                area == "a007" ~ "Area 7", 
                                area == "a008" ~ "Area 8",
                                area == "a009" ~ "Area 9", 
                                area == "a010" ~ "Area 10",
                                area == "a011" ~ "Area 11", 
                                area == "a012" ~ "Area 12", 
                                area == "a013" ~ "Area 13", 
                                area == "a014" ~ "Area 14", 
                                area == "a015" ~ "Area 15", 
                                area == "a016" ~ "Area 16", 
                                area == "a017" ~ "Area 17", 
                                area == "a018" ~ "Area 18", 
                                area == "a019mp" ~ "Area 19 Main Portion", 
                                area == "a019si" ~ "Area 19 Saanich Inlet only", 
                                area == "a019" ~ "Area 19", 
                                area == "a020" ~ "Area 20", 
                                area == "a020e" ~ "Area 20 (East)", 
                                area == "a020w" ~ "Area 20 (West)", 
                                area == "a021" ~ "Area 21", 
                                area == "a022" ~ "Area 22", 
                                area == "a023ai" ~ "Area 23 Alberni Inlet", 
                                area == "a023bs" |  str_detect(area, "a023bs") ~ "Area 23 Barkley Sound", 
                                area == "a023" ~ "Area 23", 
                                area == "a024" ~ "Area 24", 
                                area == "a025" ~ "Area 25", 
                                area == "a026" ~ "Area 26", 
                                area == "a027" ~ "Area 27", 
                                area == "a028" ~ "Area 28", 
                                area == "a029gs" ~ "Area 29 Georgia Strait", 
                                area == "a029ir" ~ "Area 29 In River", 
                                area == "a101" ~ "Area 101", 
                                area == "a102" ~ "Area 102", 
                                area == "a103" ~ "Area 103", 
                                area == "a104" ~ "Area 104",
                                area == "a105" ~ "Area 105", 
                                area == "a106" ~ "Area 106",
                                area == "a107" ~ "Area 107", 
                                area == "a108" ~ "Area 108",
                                area == "a109" ~ "Area 109", 
                                area == "a110" ~ "Area 110",
                                area == "a111" ~ "Area 111",
                                area == "a121" ~ "Area 121",                                
                                area == "a123" ~ "Area 123",
                                area == "a124" ~ "Area 124",
                                area == "a125" ~ "Area 125",
                                area == "a126" ~ "Area 126",
                                area == "a127" ~ "Area 127",
                                area == "a130" ~ "Area 130",
                                area == "a142" ~ "Area 142",
                                TRUE ~ as.character(area))) %>% 
                     mutate(method = case_when(method=="angleboat" ~ "Angling from boat", 
                                               method=="angleshore" ~ "Angling from shore",
                                               method== "dive" ~ "Dive-based or other",
                                               TRUE ~ as.character(method)))


# combining sheets post format changes, so they are all comparable
irec_raw_combined <-bind_rows(irec_raw, irec_raw23)
irec_raw_combined<-irec_raw_combined %>% mutate_if(is.numeric, ~replace(., is.na(.), 0))

# Incorporating Juveniles into accounting, make catches per person
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

# Plotting ----------------------------------------------------------------

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

# Qc report ---------------------------------------------------------------
#useful functions
merge.all <- function(x, y) {
  merge(x, y, all=TRUE, by="id_ignore")
}

# Issue 1 - Kept
kept_high<-irec_raw_combined %>% filter(total_kept_pp>4)  %>% arrange(desc(total_kept_pp))
# Issue 1.1 Kept investigations
kept_high_area<- kept_high %>% group_by(area) %>% summarise(n_area= n()) %>% arrange(desc(n_area)) %>% mutate(id_ignore = row_number())
kept_high_year<- kept_high %>% group_by(year) %>% summarise(n_year= n()) %>% arrange(desc(n_year)) %>% mutate(id_ignore = row_number())
kept_high_guide<- kept_high %>% group_by(guided) %>% summarise(n_guide= n()) %>% arrange(desc(n_guide)) %>% mutate(id_ignore = row_number())
kept_high_lodge<- kept_high %>% group_by(lodge) %>% summarise(n_lodge= n()) %>% arrange(desc(n_lodge)) %>% mutate(id_ignore = row_number())
kept_high_juv<- kept_high %>% group_by(juveniles) %>% summarise(n_juv= n()) %>% arrange(desc(n_juv)) %>% mutate(id_ignore = row_number())
DataList_kept<-list(kept_high_area, kept_high_year,kept_high_guide, kept_high_lodge, kept_high_juv)
kept_investigate <- Reduce(merge.all, DataList_kept)
kept_investigate <-kept_investigate %>% select(-id_ignore) %>% as_tibble()

# Issue 2 - Released
released_high<-irec_raw_combined %>% filter(total_released_pp>20)  %>% arrange(desc(total_released_pp))
# Issue 2.1 - released investigations
released_high_area<- released_high %>% group_by(area) %>% summarise(n_area= n()) %>% arrange(desc(n_area)) %>% mutate(id_ignore = row_number())
released_high_year<- released_high %>% group_by(year) %>% summarise(n_year= n()) %>% arrange(desc(n_year)) %>% mutate(id_ignore = row_number())
released_high_guide<- released_high %>% group_by(guided) %>% summarise(n_guide= n()) %>% arrange(desc(n_guide)) %>% mutate(id_ignore = row_number())
released_high_lodge<- released_high %>% group_by(lodge) %>% summarise(n_lodge= n()) %>% arrange(desc(n_lodge)) %>% mutate(id_ignore = row_number())
released_high_juv<- released_high %>% group_by(juveniles) %>% summarise(n_juv= n()) %>% arrange(desc(n_juv)) %>% mutate(id_ignore = row_number())
DataList_released<-list(released_high_area, released_high_year,released_high_guide, released_high_lodge, released_high_juv)
released_investigate <- Reduce(merge.all, DataList_released)
released_investigate <-released_investigate %>% select(-id_ignore) %>% as_tibble()

# Issue 3 - Total caught (kept + released)
total_high<-irec_raw_combined %>% filter(total_caught_pp>20)  %>% arrange(desc(total_caught_pp))
# Issue 3.1 - total caught investigations
total_high_area<- total_high %>% group_by(area) %>% summarise(n_area= n()) %>% arrange(desc(n_area)) %>% mutate(id_ignore = row_number())
total_high_year<- total_high %>% group_by(year) %>% summarise(n_year= n()) %>% arrange(desc(n_year)) %>% mutate(id_ignore = row_number())
total_high_guide<- total_high %>% group_by(guided) %>% summarise(n_guide= n()) %>% arrange(desc(n_guide)) %>% mutate(id_ignore = row_number())
total_high_lodge<- total_high %>% group_by(lodge) %>% summarise(n_lodge= n()) %>% arrange(desc(n_lodge)) %>% mutate(id_ignore = row_number())
total_high_juv<- total_high %>% group_by(juveniles) %>% summarise(n_juv= n()) %>% arrange(desc(n_juv)) %>% mutate(id_ignore = row_number())
DataList_total<-list(total_high_area, total_high_year,total_high_guide, total_high_lodge, total_high_juv)
total_investigate <- Reduce(merge.all, DataList_total)
total_investigate <-total_investigate %>% select(-id_ignore) %>% as_tibble()


# Issue 4 - Licences flagged
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
 # length(unique(irec_raw_combined$licence.id))
# hist(irec_liscences_flag$flag_day)

#Summary table
explore_summary_irec <- data.frame(Issue_ID=character(), Issue=character(), Count=integer(),
                              Definition=character(), 
                              stringsAsFactors=FALSE)

explore_summary_irec <- explore_summary_irec  %>% 
  add_row(Issue_ID="1", Issue="High # Kept", Count=nrow(kept_high), Definition="Number of total chinook kept per person is over 4") %>% 
  add_row(Issue_ID="1.1", Issue="High # Kept summaries", Count=nrow(kept_high), Definition="Number of total chinook kept per person is over 4, summarized by area, year, guide, lodge, and juveniles") %>% 
  add_row(Issue_ID="2", Issue="High # Released", Count=nrow(released_high), Definition="Number of total chinook released per person is over 20") %>% 
  add_row(Issue_ID="2.1", Issue="High # Released summaries", Count=nrow(kept_high), Definition="Number of total chinook released per person is over 20, summarized by area, year, guide, lodge, and juveniles") %>% 
  add_row(Issue_ID="3", Issue="High # Caught", Count=nrow(total_high), Definition="Number of total chinook caught per person is over 20") %>% 
  add_row(Issue_ID="3.1", Issue="High # Caught summaries", Count=nrow(total_high), Definition="Number of total chinook caught per person is over 20, summarized by area, year, guide, lodge, and juveniles") %>% 
  add_row(Issue_ID="4", Issue="Licences w flags", Count=nrow(irec_liscences_flag), Definition="Licence.id has at least one of: high # kept, high # released or high # caught")

sheet_list_irec<-list(Summary=explore_summary_irec,
                 "1 - High_Kept"=kept_high,
                 "1.1 - High_Kept_sum"=kept_investigate,
                 "2 - High_Released"=released_high,
                 "2.1 - High_Released_sum"=released_investigate,
                 "3 - High_Caught" = total_high,
                 "3.1 - High_Caught_sum" = total_investigate,
                 "4 - Licence_flags" = irec_liscences_flag                               
                 )

writexl::write_xlsx(sheet_list_irec, path="irec_QC.xlsx")


# Extra code for next steps -----------------------------------------------




#
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