# load libraries ----------------------------------------------------------

library(odbc)
library(dplyr)
# remotes::install_git("https://github.com/Pacific-salmon-assess/tagFisheryMapping")
library(tagFisheryMapping)
library(dbplyr)
library(janitor)
library(xlsx)
library(purrr)
library(writexl)
library(stringr)

# Option 1: Open local connection to CAMP -------------------------------

openCasConnection <- function(db_filename) {
  if (!requireNamespace("odbc", quietly = TRUE)) {
    stop("The package 'odbc' is needed for this function to work -
         and may only work in MS Windows. Please install it.",
         call. = FALSE)
  }

  driver_name <-
    paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};",
           "DBQ=",
           db_filename)

  db_conn <- dbConnect(odbc(), .connection_string = driver_name)
  return(db_conn)
}

selectAccessDb <- function(display_caption = "Select MS Access Database") {
  msaccess_filter <- rbind(cbind("All files (*.*)", "*.*"),
                           cbind("MS Access database (*.mdb, *.accdb)", "*.mdb;*.accdb"))

  if (exists('choose.files', where = asNamespace("utils"), mode = 'function')) {
    db_filename <- utils::choose.files(default = "",
                                       caption = display_caption,
                                       multi = FALSE,
                                       filters = msaccess_filter,
                                       index = nrow(msaccess_filter))
  } else {
    stop("Can not use the selectAccessDb because the choose.files function is not available in utils package")
  }

  return(db_filename)
}

#Choose the .mdb file or the .accdb file (change default), should be in your working directory
#campdb<-openCasConnection(selectAccessDb())

# You can also specify the exact file you want like this:
# casdb<-openCasConnection(file.path(getwd(), "CAMP2022BE.accdb"))


# Option 2: Open cloud connection to CAMP database ------------------------------------------------
#need to have text file camp.config in your working (project) directory
fileName <- 'camp.config'
conn_string<-readChar(fileName, file.info(fileName)$size)

campdb <- dbConnect(odbc::odbc(),
                    .connection_string = conn_string)

str(campdb)
# Pull tables from CAS

cnr<-tbl(campdb, "REAMCNRData")

View(cnr)

fishery.era<-tbl(campdb, "CAMPFisheryERA")
fishery.era<- fishery.era %>% rename(ERAFishery = FisheryERAID)
fishery.era

cnr<- merge(cnr, fishery.era) %>% as_tibble() %>% rename(erafishery=Name, year = ERAYear)
cnr_canada_sport<- cnr %>% filter(erafishery %in% c("NBC AABM S", "NBC ISBM S", "CBC S", "WCVI AABM S", "WCVI ISBM S"))   
cnr_canada_sport<- cnr_canada_sport %>% rename(Kept = CNRValue1, Released= CNRValue2) %>% select(erafishery, year, Kept, Released) 

cnr_canada_sport<- cnr_canada_sport %>% pivot_longer(cols=c("Kept", "Released"), names_to = "disposition", values_to = "cnr")
cnr_canada_sport








#some tidying functions to make the explorations work
"%notin%" <- Negate("%in%")
cwdbrecovery<- cwdbrecovery %>% as_tibble()
cwdbrecovery_unq<-cwdbrecovery %>% select(TagCode) %>% distinct()

wiretagcode_unq <-
  tbl(campdb, "WireTagCode") %>%
  as_tibble() %>%
  filter(Included == TRUE) %>%
  distinct(TagCode)


# Open connection to MRP  -------------------------------------------------
#this takes a while if you load everything, you can select only current year
mrp_recoveries<-getDfoTagRecoveries(1950:2022)

#filter MRP database by indicator tags only
mrp_recoveries_ind<-mrp_recoveries %>% filter(tag_code %in% wiretagcode_unq$TagCode)

