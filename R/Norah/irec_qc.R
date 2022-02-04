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
# doesn't include unknown
irec_raw_combined$legal_released_pp<- (  irec_raw_combined$salmon_chinook_hatch_rele + 
                                         irec_raw_combined$salmon_chinook_wild_rele + 
                                         irec_raw_combined$salmon_chinook_us_hatchery_rele + 
                                         irec_raw_combined$salmon_chinook_us_wild_rele+
                                          irec_raw_combined$salmon_chinook_us_unkown_rele +
                                         irec_raw_combined$salmon_chinook_unk_rele)/
                                         irec_raw_combined$num_people
# doesn't include unknown
irec_raw_combined$sublegal_released_pp<- (irec_raw_combined$salmon_chinook_subl_rele)/
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
cbc<-c("Area 10", "Area 106","Area 107", "Area 108", "Area 109", "Area 110", "Area 6", "Area 7", "Area 8", "Area 9")
nbc<-c("Area 2","Area 1", "Area 101", "Area 102", "Area 103", "Area 104", "Area 105", "Area 130", "Area 142", "Area 2 East", "Area 2 West", "Area 3", "Area 4", "Area 5")
gst<-c("Area 13", "Area 14", "Area 15", "Area 16", "Area 17", "Area 18", "Area 19", "Area 19 Saanich Inlet only", "Area 28", "Area 29", "Area 29 Georgia Strait", "Area 29 In River") 
jst<-c("Area 11", "Area 111", "Area 12")
wcvi<-c("Area 121", "Area 123", "Area 124", "Area 125", "Area 126", "Area 127", "Area 22","Area 21",  "Area 23 Barkley Sound",  "Area 23 Alberni Inlet",  "Area 23", "Area 24", "Area 25", "Area 26", "Area 27")
jdf<-c("Area 19", "Area 19 Main Portion", "Area 20", "Area 20 (East)", "Area 20 (West)")

irec_raw_combined<- irec_raw_combined %>% 
                    mutate(region = case_when(
                    area %in% cbc~ "Central BC", 
                    area %in% nbc~ "Northern BC",                               
                    area %in% gst~ "Georgia Strait",
                    area %in% jst~ "Johnstone Strait",
                    area %in% wcvi ~ "West Coast Vancouver Island",
                    area %in% jdf~ "Juan de Fuca",
                    TRUE ~ "other"
                    ))
irec_raw_combined$region<- factor(irec_raw_combined$region, levels = c("West Coast Vancouver Island","Johnstone Strait", "Georgia Strait", "Juan de Fuca", "Central BC" , "Northern BC"), ordered = TRUE)

# Plotting ----------------------------------------------------------------

# Looped plots
irec_raw_combined$year<-as.factor(irec_raw_combined$year)
irec_raw_combined$month<-as.factor(irec_raw_combined$month)

areas = unique(irec_raw_combined$area)
area_plots_kept = list()
for(area_ in areas) {
  area_plots_kept[[area_]] = ggplot(irec_raw_combined %>% filter(area == area_), aes(y=total_kept_pp,x=month, colour=year)) + 
    ggtitle(irec_raw_combined$area[irec_raw_combined$area == area_])+ theme_classic()+
    xlab("Month") + ylab("Chinook kept per person")+ scale_color_viridis_d(end = 0.8, option = "C", drop=FALSE)+ 
    geom_point()+ geom_hline(yintercept = 4) + theme(axis.title = element_text(size = 8))+
    theme(plot.title = element_text(size=8))
  
}

ap_kept_cbc<-area_plots_kept[cbc]
ap_kept_nbc<-area_plots_kept[nbc]
ap_kept_gst<-area_plots_kept[gst]
ap_kept_jst<-area_plots_kept[jst]
ap_kept_wcvi<-area_plots_kept[wcvi]
ap_kept_jdf<-area_plots_kept[jdf]

p1<-wrap_plots(ap_kept_wcvi, ncol=5) +  plot_annotation(title = 'West Coast Vancouver Island')+ plot_layout(guides = 'collect') 
ggsave("Plots/kept_plot_wcvi.png", p1)

p2<-wrap_plots(c(ap_kept_jst, ap_kept_gst,ap_kept_jdf), ncol=5) +  plot_annotation(title = 'Johnstone Strait & Georgia Strait & Juan de Fuca')+ plot_layout(guides = 'collect') 
ggsave("Plots/kept_plot_jst_gst.png", p2)

p3<-wrap_plots(ap_kept_cbc, ncol=5) + plot_annotation(title= 'Central BC')+ plot_layout(guides = 'collect') 
ggsave("Plots/kept_plot_cbc.png", p3)

p5<-wrap_plots(ap_kept_nbc, ncol=5) + plot_annotation(title= 'Northern BC')  + plot_layout(guides = 'collect')     
ggsave("Plots/kept_plot_nbc.png", p5)


area_plots_released = list()
for(area_ in areas) {
  area_plots_released[[area_]] = ggplot(irec_raw_combined %>% filter(area == area_), aes(y=total_released_pp,x=month, colour=year)) + 
    ggtitle(irec_raw_combined$area[irec_raw_combined$area == area_])+ theme_classic()+
    xlab("Month") + ylab("Chinook released per person")+ scale_color_viridis_d(end = 0.8, option = "C", drop=FALSE)+ 
    geom_point()+ geom_hline(yintercept = 20) + theme(axis.title = element_text(size = 8))+
    theme(plot.title = element_text(size=8))
  
}

area_plots_released["Area 20 (East)"]

ap_released_cbc<-area_plots_released[cbc]
ap_released_nbc<-area_plots_released[nbc]
ap_released_gst<-area_plots_released[gst]
ap_released_jst<-area_plots_released[jst]
ap_released_wcvi<-area_plots_released[wcvi]
ap_released_jdf<-area_plots_released[jdf]

p1_r<-wrap_plots(ap_released_wcvi, ncol=5) +  plot_annotation(title = 'West Coast Vancouver Island')+ plot_layout(guides = 'collect') 
ggsave("Plots/released_plot_wcvi.png", p1_r)
p2_r<-wrap_plots(c(ap_released_jst, ap_released_gst,ap_released_jdf), ncol=5) +  plot_annotation(title = 'Johnstone Strait & Georgia Strait & Juan de Fuca')+ plot_layout(guides = 'collect') 
ggsave("Plots/released_plot_jst_gst.png", p2_r)
p3_r<-wrap_plots(ap_released_cbc, ncol=5) + plot_annotation(title= 'Central BC')+ plot_layout(guides = 'collect') 
ggsave("Plots/released_plot_cbc.png", p3_r)
p5_r<-wrap_plots(ap_released_nbc, ncol=5) + plot_annotation(title= 'Northern BC')  + plot_layout(guides = 'collect')     
ggsave("Plots/released_plot_nbc.png", p5_r)


# Qc report ---------------------------------------------------------------
#useful functions
"%notin%" <- Negate("%in%")
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
# Issue 2.2 - sublegal
sublegal_released_high<-irec_raw_combined %>% filter(sublegal_released_pp>20)  %>% arrange(desc(sublegal_released_pp))
# Issue 2.3 legal
legal_released_high<-irec_raw_combined %>% filter(legal_released_pp>20)  %>% arrange(desc(legal_released_pp))



# Issue 3 - Total caught (kept + released)
total_high<-irec_raw_combined %>% filter(total_caught_pp>20, total_released_pp <21, total_kept_pp<5) %>% arrange(desc(total_caught_pp))


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

#Summary table
explore_summary_irec <- data.frame(Issue_ID=character(), Issue=character(), Count=integer(),
                              Definition=character(), 
                              stringsAsFactors=FALSE)

explore_summary_irec <- explore_summary_irec  %>% 
  add_row(Issue_ID="1", Issue="High # Kept", Count=nrow(kept_high), Definition="Number of total chinook kept per person is over 4") %>% 
  add_row(Issue_ID="1.1", Issue="High # Kept summaries", Count=nrow(kept_high), Definition="Number of total chinook kept per person is over 4, summarized by area, year, guide, lodge, and juveniles") %>% 
  add_row(Issue_ID="2", Issue="High # Total Released", Count=nrow(released_high), Definition="Number of total chinook released (sublegal, legal, and unknown) per person is over 20") %>% 
  add_row(Issue_ID="2.1", Issue="High # Released summaries", Count=nrow(released_high), Definition="Number of total chinook released per person is over 20, summarized by area, year, guide, lodge, and juveniles") %>% 
  add_row(Issue_ID="2.2", Issue="High # Sublegal Released", Count=nrow(sublegal_released_high), Definition="Number of sublegal chinook released per person is over 20, summarized by area, year, guide, lodge, and juveniles") %>% 
  add_row(Issue_ID="2.3", Issue="High # Legal Released", Count=nrow(legal_released_high), Definition="Number of legal chinook released per person is over 20, summarized by area, year, guide, lodge, and juveniles") %>% 
  add_row(Issue_ID="3", Issue="High # Caught", Count=nrow(total_high), Definition="Number of total chinook caught per person is over 20") %>% 
  add_row(Issue_ID="3.1", Issue="High # Caught summaries", Count=nrow(total_high), Definition="Number of total chinook caught per person is over 20, summarized by area, year, guide, lodge, and juveniles") %>% 
  add_row(Issue_ID="4", Issue="Licences w flags", Count=nrow(irec_liscences_flag), Definition="Licence.id has at least one of: high # kept, high # released or high # caught")

sheet_list_irec<-list(Summary=explore_summary_irec,
                 "1 - High_Kept"=kept_high,
                 "1.1 - High_Kept_sum"=kept_investigate,
                 "2 - High_Released"=released_high,
                 "2.1 - High_Released_sum"=released_investigate,
                 "2.2 - High Sublegals" = sublegal_released_high,
                 "2.3 - High Legals" = legal_released_high,
                 "3 - High_Caught" = total_high,
                 "3.1 - High_Caught_sum" = total_investigate,
                 "4 - Licence_flags" = irec_liscences_flag                               
                 )

writexl::write_xlsx(sheet_list_irec, path="irec_QC.xlsx")











