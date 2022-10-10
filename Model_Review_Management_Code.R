##Management of Species Habitat Model Reviewers and Reviews
##Novmeber 4, 2021
##M Tarjan

##when need to update github credentials
##https://rfortherestofus.com/2021/02/how-to-use-git-github-with-r/

library(tidyverse)
library(googlesheets4)

##connecting to AGOL tables
##First, download the "arcgisbinding" package manually from GitHub: https://github.com/R-ArcGIS/r-bridge/releases/tag/v1.0.1.232. Second, install this package in Rstudio using the menu "Tools">"install packages", and then choose the downloaded package archive
#install.packages("arcgisbinding", repos="https://r.esri.com", type="win.binary")
library(arcgisbinding)
arc.check_product()

## Identify the appropriate Table URL
### URL for the OverallFeedbackRaster table
overall_feedback_url <- "https://services.arcgis.com/EVsTT4nNRCwmHNyb/arcgis/rest/services/OverallFeedbackRaster/FeatureServer/0"
### URL for the OverallFeedbackRaster table
detailed_feedback_url <- "https://services.arcgis.com/EVsTT4nNRCwmHNyb/arcgis/rest/services/DetailedFeedbackRaster/FeatureServer/0"

## Read in MRT tables
### Read in OverallFeedbackRaster table
overall_feedback_table <- arc.open(overall_feedback_url) %>% arc.select()
### Read in OverallFeedbackRaster table
detailed_feedback_table <- arc.open(detailed_feedback_url) %>% arc.select()

##Import all models that need/needed review
##start with FWS SE project
library(readxl)

##add species from all existing projects
species.fws<-read_excel("Data/USFWS_SE_Species_Model_Status_Report_JonO_20211208.xlsx", sheet =
             NULL) %>% data.frame()
species.fws$Project<-"FWS SE"

##add BLM Year 1 species list
species.blm<-read_excel("Data/BLMSSS-Year1-models-delivered_20211005.xlsx", sheet =
                      NULL) %>% data.frame()
species.blm$Project<-"BLM SSS"
colnames(species.blm)<-c("Taxa", "Scientific.Name", "Common.Name", "cutecode", "Project")

species<-rbind(subset(species.fws, select = names(species.blm)), species.blm)

##add DoD species
species.dod<-read_excel("Data/DoD-Species-20211213.xlsx", sheet =
                          NULL) %>% data.frame()
colnames(species.dod)<-c("Group", "Scientific.Name", "Common.Name", "G.rank","cutecode", "Project")
species.dod$Taxa<-NA
species<-rbind(species, subset(species.dod, select = names(species)))

##remove pinyon jay and pygmy rabbit
species<- subset(species, !(Common.Name %in% c("Pinyon Jay", "Pygmy Rabbit")))

##replace scientific name of elko rockcress
#species$Scientific.Name[which(species$Common.Name=="Elko Rockcress")] <- "Boechera falcifructa"
#species$cutecode[which(species$Common.Name=="Elko Rockcress")] <- "boecfalc"
species$cutecode[which(species$Common.Name=="Elko Rockcress")] <- "arabfalc2"

##add list of species to see if there is overlap with mobi models or other projects
#spp<-read_excel("Data/WildlifeConservationInitiative-species-20220131.xlsx") %>% data.frame()
#spp<-data.frame(Taxa=NA, Scientific.Name=spp$Scientific.Name, Common.Name=spp$Common.Name, cutecode=spp$cutecode, Project="WCI")

#species<-rbind(species, spp)

##Import reviewer sign ups from reviewer sign up tool
##Import models in MRT2
##SpeciesMasterLookupRaster on arcgis online; open in arcgis pro; Analysis > Tools > table to excel
#mrt.models<-read_excel("Data/MRT2-SpeciesMasterLookupRaster-29112021.xlsx") %>% data.frame() ##these models have not necessarily been uploaded to MRT2, just entered
#mrt.models<-read_excel("Data/DataLoadDateRaster-20220309.xls") %>% data.frame()
##alternative is to connect directly to online AGOL table
mrt.models.url <- "https://services.arcgis.com/EVsTT4nNRCwmHNyb/arcgis/rest/services/DataLoadDateRaster/FeatureServer/0"
## Read in MRT tables
### Read in OverallFeedbackRaster table
mrt.models <- arc.open(mrt.models.url) %>% arc.select()

mrt.models$cutecode.model<-mrt.models$cutecode
mrt.models$cutecode<-str_split(mrt.models$cutecode.model, pattern = "_", simplify = T)[,1]
mrt.models$mrt2<-T
mrt.models.sub<-subset(mrt.models, select = c("cutecode", "mrt2", "ModelVersion")) %>% unique()
##Import reviewer assignments from Model Review Tool
##open in arcgis pro; Analysis > Tools > table to excel
#mrt<-read_excel("Data/SpeciesByReviewersRaster_20220309.xls") %>% data.frame()
mrt.url <- "https://services.arcgis.com/EVsTT4nNRCwmHNyb/arcgis/rest/services/SpeciesbyReviewersRaster/FeatureServer/0"
mrt <- arc.open(mrt.url) %>% arc.select()
mrt<-mrt[,1:4] %>% unique()
colnames(mrt)<-c("ELEMENT_GLOBAL_ID", "cutecode.model","Reviewer","Reviewer_email")
mrt$cutecode<-str_split(mrt$cutecode.model, pattern = "_", simplify = T)[,1]

##READ IN REVIEWS FROM SHINY MORT
mort_reviews <- googlesheets4::read_sheet(ss = "https://docs.google.com/spreadsheets/d/1OKpMQfHfU6TDXFwFb9Mnb4uQRiNq2ziD3MN4WUhhE1U/edit?usp=sharing") %>% data.frame()

##ADD MODEL REVIEWS; HOW MANY MODELS HAVE REVIEWS IN MRT2?
#reviews<-read_excel("Data/OverallFeedbackRaster-20220309.xls", sheet = NULL) %>% data.frame()
reviews<-overall_feedback_table
reviews$cutecode.model<-reviews$Species
reviews$cutecode<-str_split(reviews$cutecode.model, pattern = "_", simplify = T)[,1]
reviews$reviewed<-TRUE

##add mrt assignments to species
assigned.reviewers<-dplyr::full_join(x = species, y = subset(mrt, cutecode %in% species$cutecode))

##summarize the number of model reviewers assigned per species
n.reviewers<- assigned.reviewers %>% group_by(cutecode) %>% summarise(n.reviewer=length(unique(na.omit(Reviewer)))) %>% data.frame()

##summarize number of reviews per species (but might be for multiple model versions)
#n.reviews<- reviews %>% group_by(cutecode) %>% summarise(n.reviews=length(unique(na.omit(UserID)))) %>% data.frame() ##older version that grouped by species instead of model version
n.reviews<- reviews %>% group_by(ModelVersion, cutecode) %>% summarise(n.reviews=length(unique(na.omit(UserID)))) %>% data.frame()

##write out reviewer info for hannah
reviewer.contact <- reviews %>% subset(subset = cutecode %in% species$cutecode, select= c(UserID, cutecode, ModelVersion)) %>% data.frame()
reviewer.contact$app<-"AGOL"
##reviewer id, and which model version they've reviewed, shiny vs agol, project that it's part of, review count for model version

reviewer.contact.temp<-subset(mort_reviews, select = c(user, taxon_code, model_version))
names(reviewer.contact.temp)<-names(reviewer.contact)[1:3]
reviewer.contact.temp$app<-"Shiny"
reviewer.contact<-rbind(reviewer.contact, reviewer.contact.temp); head(reviewer.contact)
##read in SHM tracking database
#shm<-read_excel("Data/Network-SHM-database-20221010.xlsx", sheet = "model_status_table") %>% data.frame() %>% rename(cutecode=taxon_code, ModelVersion = model_version)
##add project
#reviewer.contact <- left_join(reviewer.contact, subset(shm, select = c(ModelVersion, created_for_projects)));  head(reviewer.contact)
##join to species as alternative to pull in project
reviewer.contact <- left_join(reviewer.contact, subset(species, select = c(cutecode, Project)));  head(reviewer.contact)
##replace NA project with "not in tracking db"
#reviewer.contact$created_for_projects[which(is.na(reviewer.contact$created_for_projects))]<-"modelversion missing from SHM database"
write.csv(reviewer.contact, paste0("Outputs/completedreviews-", Sys.Date(), ".csv"), row.names = F)

species.reviews<-left_join(x=species, y = n.reviewers)
species.reviews<-full_join(x=species.reviews, y=subset(mrt.models.sub, cutecode %in% species$cutecode))
species.reviews<-left_join(x=species.reviews, y=unique(subset(reviews, select=c("ModelVersion","reviewed"))))
species.reviews<-left_join(x=species.reviews, y=n.reviews)
##add false for models that are not in MRT2 or not reviewed
species.reviews$mrt2[which(is.na(species.reviews$mrt2))]<-F
species.reviews$reviewed[which(is.na(species.reviews$reviewed))]<-F
##add 0 for species with no reviews
species.reviews$n.reviews[which(is.na(species.reviews$n.reviews))]<-0

subset(species.reviews, select=c("Project", "Scientific.Name", "ModelVersion", "mrt2", "n.reviewer", "n.reviews"))

#write.csv(subset(species.reviews, select=c("Scientific.Name", "ModelVersion", "mrt2", "n.reviewer", "n.reviews"), Project=="FWS SE"), "Outputs/FWSSE_modelreviewcounts_20220715.csv", row.names = F)

##Output
##for each model: which reviewers are assigned? which reviewers completed their review? model feedback? next step?
##dataframe. record is a model
#model.status<-data.frame(model.name=NA, project = NA, reviewer.su = NA, reviewer.mrt = NA, av.star= NA, next.step = NA)
##metadata
#metadata<-rbind(
#  data.frame(data.name = "model.name" , description = "model name"),
#  data.frame(data.name = "project" , description = "project"),
#  data.frame(data.name = "reviewer.su" , description = "reviewers in sign up tool"),
#  data.frame(data.name = "reviewer.mt" , description = "reviewer in model review tool"),
#  data.frame(data.name = "av.star" , description = "mean star rating"),
#  data.frame(data.name = "next.step" , description = "next step. options = accept, model modifications, reviewer follow up, assign new reviewer")
#)

##PLOTS OF PROGRESS
library(ggplot2)

##UPLOADED ON MRT
#data.plot<-species.reviews$mrt2 %>% table() %>% data.frame()
#colnames(data.plot)<-c("mrt2", "Freq")
#data.plot$prop<-data.plot$Freq/sum(data.plot$Freq)

##alternative to include project
data.plot <- species.reviews %>% group_by(Project) %>% count(mrt2) %>% data.frame()
data.plot2 <- species.reviews %>% count(Project) %>% data.frame()
colnames(data.plot2)<-c("Project", "sum")
data.plot <- left_join(x=data.plot, y=data.plot2)
data.plot$prop<-data.plot$n/data.plot$sum

##get the label positions
data.plot <- data.plot %>%
  group_by(Project) %>%
  arrange(desc(mrt2)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop) %>%
  data.frame()
data.plot

mycols <- c("gold", "#0073C2FF")
fig.mrt <- ggplot(data.plot, aes(x = 2, y = prop, fill = mrt2)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste0("n = ", n, ", \n", round(prop*100,0), "%")), color = "white")+
  facet_wrap(.~Project, ncol=3) +
  scale_fill_manual(values = mycols, name="Uploaded to MRT") +
  theme_void() +
  xlim(.5, 2.5)
fig.mrt

##ASSIGNED REVIEWERS
#fig <- ggplot(data = species.reviews, aes(x = n.reviewer)) +
#  geom_histogram(binwidth = 0.5) +
#  theme_classic()
#fig

species.reviews$n.reviewer.cat<-species.reviews$n.reviewer ##categorize number of reviewers
species.reviews$n.reviewer.cat[which(species.reviews$n.reviewer.cat>2)]<-"3+"
##summarize data to get count of each reviewer number category and proportion
data.plot <- species.reviews %>% group_by(Project) %>% count(n.reviewer.cat) %>% data.frame()
data.plot2 <- species.reviews %>% count(Project) %>% data.frame()
colnames(data.plot2)<-c("Project", "sum")
data.plot <- left_join(x=data.plot, y=data.plot2)
data.plot$prop<-data.plot$n/data.plot$sum

data.plot <- data.plot %>%
  group_by(Project) %>%
  arrange(desc(n.reviewer.cat)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
data.plot

mycols <- c("gold", "chartreuse3", "green4", "darkgreen")
fig.reviewers <- ggplot(data.plot, aes(x = 2, y = prop, fill = n.reviewer.cat)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste0("n = ", n, ", \n", round(prop*100,0), "%")), color = "white")+
  facet_wrap(facets=.~Project, ncol=3)+
  scale_fill_manual(values = mycols, name="Number of\nReviewers Assigned") +
  theme_void() +
  xlim(.5, 2.5)
fig.reviewers

##COMPLETED REVIEW (T/F)
data.plot <- species.reviews %>% group_by(Project) %>% count(reviewed) %>% data.frame()
data.plot2 <- species.reviews %>% count(Project) %>% data.frame()
colnames(data.plot2)<-c("Project", "sum")
data.plot <- left_join(x=data.plot, y=data.plot2)
data.plot$prop<-data.plot$n/data.plot$sum

##get the label positions
data.plot <- data.plot %>%
  group_by(Project) %>%
  arrange(desc(reviewed)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
data.plot

#mycols <- c("#EFC000FF", "#0073C2FF", "#868686FF", "#CD534CFF")
mycols <- c("gold", "#0073C2FF")
fig.review <- ggplot(data.plot, aes(x = 2, y = prop, fill = reviewed)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste0("n = ", n, ", \n", round(prop*100,0), "%")), color = "white")+
  facet_wrap(facets=.~Project, ncol=3) +
  scale_fill_manual(values = mycols, name="Reviewed") +
  theme_void() +
  xlim(.5, 2.5)
fig.review

##NUMBER OF COMPLETE REVIEWS (# OF USERIDS THAT COMPLETED REVIEWS)
##categorize n.reviews
species.reviews$n.reviews.cat<-species.reviews$n.reviews ##categorize number of reviewers
species.reviews$n.reviews.cat[which(species.reviews$n.reviews.cat>2)]<-"3+"
data.plot <- species.reviews %>% group_by(Project) %>% count(n.reviews.cat) %>% data.frame()
data.plot2 <- species.reviews %>% count(Project) %>% data.frame()
colnames(data.plot2)<-c("Project", "sum")
data.plot <- left_join(x=data.plot, y=data.plot2)
data.plot$prop<-data.plot$n/data.plot$sum

data.plot <- data.plot %>%
  group_by(Project) %>%
  arrange(desc(n.reviews.cat)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
data.plot

mycols <- c("gold", "chartreuse3", "green4", "darkgreen")
fig.n.reviews <- ggplot(data.plot, aes(x = 2, y = prop, fill = as.character(n.reviews.cat))) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste0("n = ", n, ", \n", round(prop*100,0), "%")), color = "white")+
  facet_wrap(facets=.~Project, ncol=3)+
  scale_fill_manual(values = mycols, name="Number of\nReviews Completed") +
  theme_void() +
  xlim(.5, 2.5)
fig.n.reviews

#write.csv(species.reviews, "Outputs/Fed-project-species-reviews-20220624.csv", row.names=F)

##add list of mobi models
#mobimodels<-read_excel("G:/tarjan/Species-select/Data/MoBI Modeling Summary by Species January 2021.xlsx", sheet = "MoBI_Model_Assessment", skip = 2) %>% data.frame()
mobimodels<-read_excel("C:/Users/max_tarjan/NatureServe/Map of Biodiversity Importance - Summary Tables/MoBI Modeling Summary by Species January 2021.xlsx", sheet = "MoBI_Model_Assessment", skip = 2) %>% data.frame()
colnames(mobimodels)[3:7]<-c("cutecode", "Broad Group", "Taxonomic Group", "Scientific Name", "Common Name")

##view particular mobimodels
subset(mobimodels, cutecode %in% c("faxohart", "procreim", "orcohart"), select = c(cutecode, Overal.All.Confidence, Model.Review, Preliminary.Model.Assessment))

##add whether species are associated with mobi or another project
spp<-subset(species.reviews, Project=="WCI", select=-c(Project, Taxa, n.reviewer.cat, n.reviews.cat))
spp<-left_join(x=spp, y=subset(species, select=c("Scientific.Name", "Project"), subset = Project !="WCI"))
spp<-left_join(x=spp, y=subset(mobimodels, select=c("Scientific Name", "Included.in.MoBI","Count.of.Reviews", "Preliminary.Model.Assessment")), by = c("Scientific.Name"="Scientific Name"))

#write.csv(spp, "Outputs/WildlifeConservationInitiative-species-20220131.csv", row.names = F, na="")
