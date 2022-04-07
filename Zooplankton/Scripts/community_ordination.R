############
#Drought-MAST analysis ordination
############
rm( list = ls()) #clear env

library(tidyverse)
library(tibble)
library(dplyr)
library(ggplot2)
library(plotrix)
library(purrr)
library(cowplot)
library(plyr)
library(readxl)
library(vegan)

WY<-read_excel("Data/Water years.xlsx", sheet="yearassignments")
data<-read.csv("Data/Drought_taxa_BPUEmatrix.csv")%>%
  left_join(WY, by=c("water_year"="Year"))%>%
  mutate(Yr_type=factor(Yr_type, levels=c("Wet", "Above Normal", "Below Normal", "Dry", "Critical")),
         Index_s=(Index-mean(Index))/sd(Index))%>%
  pivot_longer(cols = c(-water_year, -Index, -Index_s, -Yr_type, -Drought), names_to="Taxa", values_to="BPUE")

#restructure as community matrix for each water_year
data_wide<-pivot_wider(data,names_from = Taxa, values_from = BPUE)
data_wide<-data_wide%>%filter(water_year>1994) #filter after 1994 to catch all taxa
data_wide<-data_wide[ , colSums(is.na(data_wide)) == 0]
matrix<-as.matrix(data_wide)
rownames(matrix)<-data_wide$Drought
env_cols<-c("water_year","Index","Yr_type","Drought","Index_s","All.taxa")

community_matrix<-matrix[, !colnames(matrix) %in% env_cols]
community_matrix<-matrix(as.numeric(community_matrix),    # Convert to numeric matrix
       ncol = ncol(community_matrix),dimnames = list(dimnames(community_matrix)[[1]],dimnames(community_matrix)[[2]]))
env_matrix<-matrix[, colnames(matrix) %in% env_cols]

#following along at https://jonlefcheck.net/2012/10/24/nmds-tutorial-in-r/

example_NMDS=metaMDS(community_matrix, # Our community-by-species matrix
                     k=2,na.rm=T,autotransform = T) # The number of reduced dimensions
stressplot(example_NMDS)
plot(example_NMDS)
ordiplot(example_NMDS,type="n")
orditorp(example_NMDS,display="species",col="red",air=0.01)
orditorp(example_NMDS,display="sites",cex=1.25,air=0.01)

treat=env_matrix[,4]
ordiplot(example_NMDS,type="n")
ordihull(example_NMDS,groups=treat,draw="polygon",col=c("#FDE725FF","#55C667FF","#33638DFF"),alpha=200,label=F)
orditorp(example_NMDS,display="species",col="red",air=0.01,cex=1.5)
orditorp(example_NMDS,display="sites",
         air=0.01,cex=2)

#run a permanova9
community_df<-as.data.frame(community_matrix)
env_df<-as.data.frame(env_matrix)
pm1<-adonis2(sqrt(wisconsin(community_df))~Drought,data=env_df)
pm1
capture.output(pm1,file="Outputs/permanova_1.txt")
