##Query species models
##November 8, 2021
##M Tarjan

##find species for modeling

##read in list of BLM species
library(tidyverse)
library(readxl)
#library(dplyr)
blm.spp<-read_excel("BLM - Information for T & E Strategic Decision-Making - April 2021.xlsx", sheet =
                     "BLM SSS Information by State", col_names = T, skip = 1) %>% data.frame()
blm.spp<-blm.spp[,1:11]

##read Regan's spreadsheet of Mobi models
mobi<-read_excel("MoBI Modeling Summary by Species January 2021.xlsx", sheet =
                   "MoBI_Model_Assessment", col_names = T, skip = 2) %>% data.frame()
mobi<-mobi[,1:30]
colnames(mobi)[1:6] <- c("Element.Global.ID", "global.id.2018", "Cutecode", "broad.group", "taxonomic.Group", "scientific.Name")

##read Year 1 BLM species list
blm.spp.y1<-read_excel("Year1-models-delivered_20211005.xlsx", sheet =
                         "Sheet1", col_names = T, skip = 0) %>% data.frame()

##add mobi model info to blm species list
blm.mobi<-dplyr::left_join(x = blm.spp, y = mobi)

##get list of animal species for modeling

animal.list <- subset(blm.mobi, 
       Taxonomic.Group != "Plant"
       #& Global.Rank..18July2020. %in% c("G1", "G2", "G3", "G4", "G5")
       & as.numeric(Occurrences.on.BLM.Lands..West....Total.Occurrences.Rangewide) >= 0.2
       #& !(Scientific.Name %in% unique(blm.spp.y1$scientific_name))
       #& Total.Occurrences.Rangewide > 5 ##or 10
       & !(ESA.Status..18Jul2020. %in% c("-", "DL"))
       #& Overal.All.Confidence == "Medium"
       #& Model.Review %in% c("High", "Medium", "Low")
       & Validation.Stats %in% c("High")
       )
dim(animal.list)

write.csv(animal.list, file = "BLM_animal_models_list_8Nov2021.csv", row.names=F)
