## Creating reviewer permissions table for upload to SpeciesbyReviewers raster on ArcGIS online
## Created by GR; modified by MT
### Load libraries
library(tidyverse)
#library(googlesheets4)

### Load data
#reviewer_signup <- googlesheets4::read_sheet(ss = "https://docs.google.com/spreadsheets/d/14oq_KQxD8KiOjg-RxrPdMplNUBHhBq3QJk3bU57xByU/edit?usp=sharing") 
reviewer_signup <- read.csv("Data/Model Reviewer Sign Up Form - Responses - Sheet1.csv")
SpeciesMasterLookupRaster <- read_csv("Data/SpeciesMasterLookupRaster.csv") %>% 
  dplyr::mutate(species = strsplit((strsplit(`Scientific Name`, " \\(") %>% purrr::map(1)) %>% unlist(), ",") %>% purrr::map(1) %>% unlist())

### Generate reviewer permissions table
#### Format required: ELEMENT_GLOBAL_ID, cutecode, Reviewer, Reviewer email, SpeciesMasterLookupKey, 
#### Remove incomplete Sign Up rows - chase up on those individually via email
reviewer_signup <- reviewer_signup %>% 
  dplyr::filter(complete.cases(.))
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
  arrange(Reviewer)

### Write out data
write_csv(reviewer_permissions_table, paste0("Outputs/reviewer_permissions_table_", Sys.Date(), ".csv"))
