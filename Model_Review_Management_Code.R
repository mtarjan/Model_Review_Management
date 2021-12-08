##Management of Species Habitat Model Reviewers and Reviews
##Novmeber 4, 2021
##M Tarjan

##when need to update github credentials
##https://rfortherestofus.com/2021/02/how-to-use-git-github-with-r/

##Import all models that need/needed review
##start with FWS SE project
library(readxl)
library(tidyverse)
species.fws<-read_excel("Data/USFWS_SE_Species_Model_Status_Report_JonO_20211208.xlsx", sheet =
             NULL) %>% data.frame()
species.fws$Project<-"FWS SE"

##add BLM Year 1 species list
species.blm<-read_excel("Data/BLMSSS-Year1-models-delivered_20211005.xlsx", sheet =
                      NULL) %>% data.frame()
species.blm$Project<-"BLM SSS"
colnames(species.blm)<-c("Taxa", "Scientific.Name", "Common.Name", "cutecode", "Project")

species<-rbind(subset(species.fws, select = names(species.blm)), species.blm)

##Import reviewer sign ups from reviewer sign up tool
##Import models in MRT2
##SpeciesMasterLookupRaster on arcgis online; open in arcgis pro; Analysis > Tools > table to excel
#mrt.models<-read_excel("Data/MRT2-SpeciesMasterLookupRaster-29112021.xlsx") %>% data.frame() ##these models have not necessarily been uploaded to MRT2, just entered
mrt.models<-read_excel("Data/MRT2-DataLoadDateRaster-29112021.xlsx") %>% data.frame()
mrt.models$cutecode.model<-mrt.models$cutecode
mrt.models$cutecode<-str_split(mrt.models$cutecode.model, pattern = "_", simplify = T)[,1]
mrt.models$mrt2<-T
mrt.models.sub<-subset(mrt.models, select = c("cutecode", "mrt2")) %>% unique()
##Import reviewer assignments from Model Review Tool
##open in arcgis pro; Analysis > Tools > table to excel
mrt<-read_excel("Data/SpeciesByReviewersRaster_20211207.xlsx") %>% data.frame()
mrt<-mrt[,1:4]
colnames(mrt)<-c("ELEMENT_GLOBAL_ID", "cutecode.model","Reviewer","Reviewer_email")
mrt$cutecode<-str_split(mrt$cutecode.model, pattern = "_", simplify = T)[,1]

##ADD MODEL REVIEWS; HOW MANY MODELS HAVE REVIEWS IN MRT2?
reviews<-read_excel("Data/OverallFeedbackRaster-20211208.xlsx", sheet = NULL) %>% data.frame()
reviews$cutecode.model<-reviews$Species
reviews$cutecode<-str_split(reviews$cutecode.model, pattern = "_", simplify = T)[,1]
reviews$reviewed<-TRUE

##add mrt assignments to species
assigned.reviewers<-dplyr::full_join(x = species, y = subset(mrt, cutecode %in% species$cutecode))

##summarize the number of model reviewers assigned per species
n.reviewers<- assigned.reviewers %>% group_by(cutecode) %>% summarise(n.reviewer=length(unique(na.omit(Reviewer)))) %>% data.frame()

##summarize number of reviews per species
n.reviews<- reviews %>% group_by(cutecode) %>% summarise(n.reviews=length(unique(na.omit(UserID)))) %>% data.frame()

species.reviews<-left_join(x=species, y = n.reviewers)
species.reviews<-left_join(x=species.reviews, y=mrt.models.sub)
species.reviews<-left_join(x=species.reviews, y=subset(reviews, select=c("cutecode","UserID", "reviewed")))
species.reviews<-left_join(x=species.reviews, y=n.reviews)
##add false for models that are not in MRT2 or not reviewed
species.reviews$mrt2[which(is.na(species.reviews$mrt2))]<-F
species.reviews$reviewed[which(is.na(species.reviews$reviewed))]<-F
##add 0 for species with no reviews
species.reviews$n.reviews[which(is.na(species.reviews$n.reviews))]<-0

subset(species.reviews, select=c("Scientific.Name", "mrt2", "n.reviewer", "reviewed", "n.reviews"))

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
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
data.plot

mycols <- c("gold", "#0073C2FF")
fig.mrt <- ggplot(data.plot, aes(x = 2, y = prop, fill = mrt2)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste0("n = ", n, ", \n", round(prop*100,0), "%")), color = "white")+
  facet_wrap(facets=.~Project) +
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
  facet_wrap(facets=.~Project)+
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
  facet_wrap(facets=.~Project) +
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
  facet_wrap(facets=.~Project)+
  scale_fill_manual(values = mycols, name="Number of\nReviews Completed") +
  theme_void() +
  xlim(.5, 2.5)
fig.n.reviews