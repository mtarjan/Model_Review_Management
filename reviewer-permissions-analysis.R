## Creating reviewer permissions table for upload to SpeciesbyReviewers raster on ArcGIS online
## Created by GR; modified by MT
### Load libraries
library(tidyverse)
library(readxl)
library(googlesheets4)
library(arcgisbinding)
arc.check_product()

specieslookup_url <- "https://services.arcgis.com/EVsTT4nNRCwmHNyb/arcgis/rest/services/SpeciesMasterLookupRaster/FeatureServer/0"
SpeciesMasterLookupRaster <- arc.open(specieslookup_url) %>% arc.select()

### Load data
reviewer_signup <- googlesheets4::read_sheet(ss = "https://docs.google.com/spreadsheets/d/14oq_KQxD8KiOjg-RxrPdMplNUBHhBq3QJk3bU57xByU/edit?usp=sharing") %>% data.frame()
#reviewer_signup <- read.csv("Data/Model Reviewer Sign Up Form - Responses - Sheet1.csv")
#SpeciesMasterLookupRaster <- read.csv("Data/SpeciesMasterLookupRaster-20221003.csv", fileEncoding="UTF-8-BOM") %>% dplyr::mutate(species = strsplit((strsplit(`Scientific.Name`, " \\(") %>% purrr::map(1)) %>% unlist(), ",") %>% purrr::map(1) %>% unlist())
SpeciesMasterLookupRaster <- SpeciesMasterLookupRaster %>%
  dplyr::mutate(species = strsplit((strsplit(`Scientific.Name`, " \\(") %>% purrr::map(1)) %>% unlist(), ",") %>% purrr::map(1) %>% unlist())

### Generate reviewer permissions table
#### Format required: ELEMENT_GLOBAL_ID, cutecode, Reviewer, Reviewer email, SpeciesMasterLookupKey, 
#### Remove incomplete Sign Up rows - chase up on those individually via email
#reviewer_signup <- reviewer_signup %>% 
#  dplyr::filter(complete.cases(.))
#### Identiy unique species
review_species <- purrr::map(reviewer_signup$`Selected.Species`, function(s) strsplit(s, ","))
unique_review_species <- review_species %>% unlist() %>% unique()
unique_review_species_Lookup <- SpeciesMasterLookupRaster %>% dplyr::filter(grepl(paste(unique_review_species, collapse = "|"), SpeciesMasterLookupRaster$species))
reviewers <- data.frame(Review_species = review_species %>% unlist()) %>% 
  dplyr::mutate(Reviewer = rep(paste(reviewer_signup$`First.Name`, reviewer_signup$`Last.Name`), times = (purrr::map(purrr::map(review_species, 1), length) %>% unlist())),
                `Reviewer email` = rep(reviewer_signup$Email, times = (purrr::map(purrr::map(review_species, 1), length) %>% unlist()))
  )
reviewer_permissions_table <- inner_join(unique_review_species_Lookup, reviewers, by = c("species" = "Review_species")) %>% 
  dplyr::select(ELEMENT_GLOBAL_ID, cutecode, Reviewer, `Reviewer email`, SpeciesMasterLookupKey) %>% 
  arrange(Reviewer) %>% unique()

##remove permissions that already exist
#mrt<-read_excel("Data/SpeciesByReviewersRaster_20211220.xlsx") %>% data.frame()
mrt.url <- "https://services.arcgis.com/EVsTT4nNRCwmHNyb/arcgis/rest/services/SpeciesbyReviewersRaster/FeatureServer/0"
mrt <- arc.open(mrt.url) %>% arc.select()
mrt<-mrt[,1:5]
colnames(mrt)<-colnames(reviewer_permissions_table)

reviewer_permissions_table_new<-anti_join(x= reviewer_permissions_table, y = mrt) %>% data.frame()

### Write out data
write_csv(reviewer_permissions_table_new, paste0("Outputs/reviewer_permissions_table_", Sys.Date(), ".csv"))
