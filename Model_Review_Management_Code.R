##Management of Species Habitat Model Reviewers and Reviews
##Novmeber 4, 2021
##M Tarjan

##when need to update github credentials
##https://rfortherestofus.com/2021/02/how-to-use-git-github-with-r/

##Import all models that need/needed review
##start with FWS SE project
library(readxl)
species<-read_excel("Data/USFWS_SE_Modeling_Status_JonO_Nov2021.xlsx", sheet =
             NULL) %>% data.frame()

##Import reviewer sign ups from reviewer sign up tool
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

subset(species.reviews, select=c("Scientific.Name", "CURRENT.STATUS", "n.reviewer"))
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