##Query species models
##November 8, 2021
##M Tarjan

##find species for modeling

##read in list of BLM species
library(tidyverse)
library(readxl)
#library(dplyr)
blm.spp<-read_excel("Data/BLM - Information for T & E Strategic Decision-Making - April 2021.xlsx", sheet =
                     "BLM SSS Information by State", col_names = T, skip = 1) %>% data.frame()
##add column showing which states the species intersects with
spp.states<-blm.spp %>% tidyr::gather("state", "presence", 12:53) %>% subset(presence !="-") %>% group_by(Scientific.Name) %>% summarize(states=str_c(unique(str_sub(state, 1, 2)), collapse=", ")) %>% data.frame()
blm.spp<-left_join(blm.spp, spp.states)

blm.spp<-subset(blm.spp, select = c("Element.Global.ID", "Taxonomic.Group", "Scientific.Name", "Common.Name", "Global.Rank..18July2020.", "ESA.Status..18Jul2020.", "Occurrences.on.BLM.Lands..West....Total.Occurrences.Rangewide", "states", "Total.Occurrences.Rangewide"))
names(blm.spp)[names(blm.spp) == 'Occurrences.on.BLM.Lands..West....Total.Occurrences.Rangewide'] <- 'Prop.on.BLM.Lands.West'

##read Regan's spreadsheet of Mobi models
mobi<-read_excel("Data/MoBI Modeling Summary by Species January 2021.xlsx", sheet =
                   "MoBI_Model_Assessment", col_names = T, skip = 2) %>% data.frame()
mobi<-mobi[,1:30]
colnames(mobi)[1:6] <- c("Element.Global.ID", "global.id.2018", "Cutecode", "broad.group", "taxonomic.Group", "scientific.Name")

##read Year 1 BLM species list
blm.spp.y1<-read_excel("Data/BLMSSS-Year1-models-delivered_20211005.xlsx", sheet =
                         "Sheet1", col_names = T, skip = 0) %>% data.frame()

##add mobi model info to blm species list
blm.mobi<-dplyr::left_join(x = blm.spp, y = mobi)

##get a list of species to model in Year 2
yr2.spp <- subset(blm.mobi, 
                      (str_detect(Global.Rank..18July2020., "G1") | str_detect(Global.Rank..18July2020., "G2") | str_detect(Global.Rank..18July2020., "G3"))
                      & Taxonomic.Group != "Plant"
                      #& str_length(states) >2
                      & (str_length(states) >2 | Taxonomic.Group != "Plant")
                      & as.numeric(Prop.on.BLM.Lands.West) >= 0.2
                      & !(Scientific.Name %in% unique(blm.spp.y1$scientific_name))
                      & Total.Occurrences.Rangewide >= 3 ##guessing at the number
                      & !(ESA.Status..18Jul2020. %in% c("-", "DL"))
                      #& Included.in.MoBI == "yes"
                      #& !is.na(Included.in.MoBI)
                      #& Overal.All.Confidence == "Medium"
                      #& Model.Review %in% c("High", "Medium", "Low")
                      #& Validation.Stats %in% c("High", "Medium")
)
dim(yr2.spp)
dim(subset(yr2.spp, Taxonomic.Group != "Plant"))
subset(yr2.spp, select=c("Taxonomic.Group", "Common.Name", "Global.Rank..18July2020.", "ESA.Status..18Jul2020.", "Prop.on.BLM.Lands.West", "Included.in.MoBI", "states")) %>% arrange(Common.Name)

data.write<-subset(yr2.spp, select=c("Element.Global.ID", "Taxonomic.Group", "Scientific.Name", "Common.Name", "Global.Rank..18July2020.", "ESA.Status..18Jul2020.", "Prop.on.BLM.Lands.West", "states")) %>% arrange(Common.Name)
names(data.write)<-c("Element.Global.ID", "Taxonomic.Group", "Scientific.Name", "Common.Name", "Global.Rank", "ESA.Status", "Prop.on.BLM.Lands.West", "States")

write.csv(data.write, file = "BLM_year2_model_animals_20220210.csv", row.names=F)
