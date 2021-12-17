library(dplyr)

format_data <- function() {
  creel <- read.csv(here::here("data/creel_filter.csv"))
  irec <- read.csv(here::here("data/iRecchinook_2012_2021.csv"))
  arealu <- read.csv(here::here("data/areaLU.csv"))
  
  creel <- creel %>%
    rename(AREA = PFMA) %>%
    left_join(arealu[, c("AREA", "LU_GROUPING3")]) %>%
    filter(YEAR > 2012)
  
  irec <- irec %>% filter(METHOD == "Angling from boat")
  # input 0s for missing observations on irec
  # create df for all possible observations - using variables of interest only
  # if other variables are considered, need to include them here
  allobs <- expand.grid(list(
    AREA = unique(irec$AREA),
    YEAR = unique(irec$YEAR),
    MONTH = unique(irec$MONTH),
    DISPOSITION = unique(irec$DISPOSITION)
  ))
  
  # create zero observations and remove in river fisheries
  irecall <- left_join(allobs, irec) %>%
    filter(AREA != "Area 29 (In River)", YEAR > 2012)
  
  irecall$ESTIMATE[is.na(irecall$ESTIMATE)] <- 0
  
  irecall <- irecall %>% left_join(arealu[, c("AREA", "LU_GROUPING3")])
  
  irecc <- irecall %>%
    select(c(AREA, YEAR, MONTH, DISPOSITION, ESTIMATE, VARIANCE, LU_GROUPING3)) %>%
    group_by(AREA, YEAR, MONTH, DISPOSITION, LU_GROUPING3) %>%
    summarise(ESTIMATE = sum(ESTIMATE), VARIANCE = sum(VARIANCE)) %>%
    mutate(SD = sqrt(VARIANCE)) %>%
    select(c(!VARIANCE)) %>%
    mutate(SURVEY = "iRec")
  
  # wrangle creel data
  creelf <- creel %>%
    filter(ESTIMATE_SOURCE == "Creel") %>%
    mutate(SURVEY = case_when(
      Include..20. == "Y" ~ "creel20",
      Include..15. == "Y" ~ "creel15",
      TRUE ~ "creel"
    )) %>%
    rename(SD = STANDARD_ERROR, DISPOSITION = TYPE) %>%
    select(c(AREA, YEAR, MONTH, DISPOSITION, ESTIMATE, SD, SURVEY, LU_GROUPING3))
  
  creelcc <- creelf %>%
    rename(CREEL = ESTIMATE, SDCREEL = SD)
  
  ireccc <- irecc %>%
    rename(IREC = ESTIMATE, SDIREC = SD) %>%
    select(c(!SURVEY))
  
  datxy <- left_join(ireccc, creelcc) %>%
    mutate(SEASON = if_else(MONTH < 5 | MONTH > 9, "offseason", "peakseason"))
  
  datnna<-datxy[!is.na(datxy$CREEL),]
  names(datnna) <- tolower(names(datnna))
  datnna <- rename(datnna, region = lu_grouping3)
  datnna
}
