##Management of Species Habitat Model Reviewers and Reviews
##Novmeber 4, 2021
##M Tarjan

##when need to update github credentials
##https://rfortherestofus.com/2021/02/how-to-use-git-github-with-r/

##Import all models that need/needed review
##start with FWS SE project
library(readxl)
library(tidyverse)
species<-read_excel("Data/USFWS_SE_Modeling_Status_JonO_Nov2021.xlsx", sheet =
             NULL) %>% data.frame()
species$Project<-"FSW SE"

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
mrt<-read_excel("Data/SpeciesByReviewersRaster_20211112.xls") %>% data.frame()
mrt<-mrt[,1:4]
colnames(mrt)<-c("ELEMENT_GLOBAL_ID", "cutecode.model","Reviewer","Reviewer_email")
mrt$cutecode<-str_split(mrt$cutecode.model, pattern = "_", simplify = T)[,1]

model.reviewers<-dplyr::full_join(x = species, y = subset(mrt, cutecode %in% species$cutecode))

##summarize the number of model reviewers assigned per species
n.reviewers<- model.reviewers %>% group_by(cutecode) %>% summarise(n.reviewer=length(unique(na.omit(Reviewer)))) %>% data.frame()

species.reviews<-left_join(x=species, y = n.reviewers)
species.reviews<-left_join(x=species.reviews, y=mrt.models.sub)
##add false for models that are not in MRT2
species.reviews$mrt2[which(is.na(species.reviews$mrt2))]<-F

subset(species.reviews, select=c("Scientific.Name", "mrt2", "n.reviewer", "CURRENT.STATUS"))
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
data.plot<-species.reviews$mrt2 %>% table() %>% data.frame()
colnames(data.plot)<-c("mrt2", "Freq")
data.plot$prop<-data.plot$Freq/sum(data.plot$Freq)

data.plot <- data.plot %>%
  arrange(desc(mrt2)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
data.plot

mycols <- c("#EFC000FF", "#0073C2FF", "#868686FF", "#CD534CFF")
fig.mrt <- ggplot(data.plot, aes(x = 2, y = prop, fill = mrt2)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste0("n = ", Freq, ", \n", round(prop*100,0), "%")), color = "white")+
  scale_fill_manual(values = mycols, name="Uploaded to MRT") +
  theme_void() +
  xlim(.5, 2.5)
fig.mrt

##ASSIGNED REVIEWERS
#fig <- ggplot(data = species.reviews, aes(x = n.reviewer)) +
#  geom_histogram(binwidth = 0.5) +
#  theme_classic()
#fig

data.plot<-species.reviews
data.plot$n.reviewer.cat<-data.plot$n.reviewer
data.plot$n.reviewer.cat[which(data.plot$n.reviewer.cat>2)]<-"3+"
data.plot<-data.plot$n.reviewer.cat %>% table() %>% data.frame()
colnames(data.plot)<-c("n.reviewer", "Freq")
data.plot$prop<-data.plot$Freq/sum(data.plot$Freq)

data.plot <- data.plot %>%
  arrange(desc(n.reviewer)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
data.plot

mycols <- c("#EFC000FF", "#0073C2FF", "#868686FF", "#CD534CFF")
fig.reviewers <- ggplot(data.plot, aes(x = 2, y = prop, fill = n.reviewer)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste0("n = ", Freq, ", \n", round(prop*100,0), "%")), color = "white")+
  scale_fill_manual(values = mycols, name="Number of\nReviewers Assigned") +
  theme_void() +
  xlim(.5, 2.5)
fig.reviewers
