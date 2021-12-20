##BLM SSS
##Conservation Status Update
##Create a list of priority species for update
##mtarjan
##Dec 20, 2021

library(readxl)
library(tidyverse)

sss<-read_excel("Data/BLM - Information for T & E Strategic Decision-Making - April 2021.xlsx", sheet = "BLM SSS Information by State", skip = 1) %>% data.frame()
reviews<-read_excel("Data/GRank_Review_Dates_20211214.xlsx", sheet = "U.S. EGT GRank Review info") %>% data.frame()

sss.grank.date<-left_join(x=sss[,1:11], y = reviews[,c(1,2,3,9,13,14,15,17,20)], by = c("Element.Global.ID" = "ELEMENT_GLOBAL_ID"))

##Inactive Global IDs; add IDs that have become inactive since the start of the BLM SSS project; it is not possible to match the Grank update date for these species
inactive.ids<-data.frame(Element.Global.ID = c(102436, 102200, 104141, 100144,107797,160730,106265,806561,105893,101770), Inactive=T)
sss.grank.date<-left_join(x=sss.grank.date, y = inactive.ids)

##get subset of plants, listed, longtime without status reviewed
sss.review<-subset(sss.grank.date, Taxonomic.Group=="Plant" & USESA %in% c("E, PDL: Endangered; Proposed for delisting", "E, PT: Endangered; Proposed threatened", "E, T: Endangered; Threatened", "E: Endangered", "T: Threatened"), select=c("Element.Global.ID", "Scientific.Name", "Common.Name", "USESA","G_RANK", "G_RANK_REVIEW_DATE", "Inactive")) %>% arrange(G_RANK_REVIEW_DATE)

write.csv(sss.review, "Outputs/BLM-SSS-listed-plants-GRank-review-date-20211220.csv", row.names = F)
