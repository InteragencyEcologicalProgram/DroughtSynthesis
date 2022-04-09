############
#Drought-MAST regime analysis
############
rm( list = ls()) #clear env

library(tidyverse)
library(tibble)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotrix)
library(purrr)
library(cowplot)
library(plyr)
library(readxl)
library(broom)

WY<-read_excel("Data/Water years.xlsx", sheet="yearassignments")
WY$water_year<-WY$Year

Drought_LT_bpue_year<-read.csv("Data/zoop_drought_lt_bpue_year.csv")
Drought_LT_bpue_year<-Drought_LT_bpue_year%>%inner_join(WY)
Drought_LT_bpue_reg<-read.csv("Data/zoop_drought_lt_bpue_reg.csv")
Drought_LT_bpue_reg<-Drought_LT_bpue_reg%>%inner_join(WY)
Drought_LT_bpue_szn<-read.csv("Data/zoop_drought_lt_bpue_szn.csv")
Drought_LT_bpue_szn<-Drought_LT_bpue_szn%>%inner_join(WY)
Drought_ST_bpue<-read.csv("Data/zoop_drought_st_bpue.csv")
Drought_ST_cpue<-read.csv("Data/zoop_drought_st_cpue.csv")

#set up regimes
t1<-data.frame(Year=1959:1986, Regime="Pre-Clam (-1986)")
t2<-data.frame(Year=1987:2001, Regime="Pre-POD (1987-2001)")
t3<-data.frame(Year=2002:2012, Regime="POD (2002-2012)")
t4<-data.frame(Year=2013:2020, Regime="Climate Shift (2013-)")
regimes<-t1%>%
  rbind(t2,t3,t4)
regimes$Regime<-factor(regimes$Regime,levels=c("Pre-Clam (-1986)","Pre-POD (1987-2001)","POD (2002-2012)","Climate Shift (2013-)"))


#Drought LT BPUE year
Drought_LT_bpue_year<-Drought_LT_bpue_year%>%inner_join(regimes)
m1<-aov(BPUE_ug~Drought*Regime,data=Drought_LT_bpue_year)
capture.output(summary(m1),file="Outputs/regime_m1.txt")
pm1<-ggplot(Drought_LT_bpue_year, aes(x = Drought, y = BPUE_ug)) +
  geom_boxplot(aes())+
  theme_classic()+
  facet_grid(~Regime)+
  theme(text = element_text(size=24))
pm1
save_plot("Figures/regime_pm1.png",pm1,base_height = 10,base_width = 16)

#Drought LT BPUE season
Drought_LT_bpue_szn<-Drought_LT_bpue_szn%>%inner_join(regimes)
m2<-aov(BPUE_ug~Drought+Season,data=Drought_LT_bpue_szn)
capture.output(summary(m2),file="Outputs/regime_m2.txt")
pm2<-ggplot(Drought_LT_bpue_szn, aes(x = Drought, y = BPUE_ug)) +
  geom_boxplot(aes())+
  theme_classic()+
  facet_grid(Season~Regime)+
  theme(text = element_text(size=24))
pm2
save_plot("Figures/regime_pm2.png",pm2,base_height = 10,base_width = 16)

#Drought LT BPUE region
Drought_LT_bpue_reg<-Drought_LT_bpue_reg%>%inner_join(regimes)
m2<-aov(BPUE_ug~Drought*Region,data=Drought_LT_bpue_reg)
capture.output(summary(m2),file="Outputs/regime_m2.txt")
pm3<-ggplot(Drought_LT_bpue_reg, aes(x = Drought, y = BPUE_ug)) +
  geom_boxplot(aes())+
  theme_classic()+
  facet_grid(Region~Regime,scales="free")+
  theme(text = element_text(size=24))
pm3
save_plot("Figures/regime_pm3.png",pm3,base_height = 10,base_width = 16)

####################################
#DROUGHT LT BPUE by taxa and regime
####################################
WY<-read_excel("Data/Water years.xlsx", sheet="yearassignments")
data<-read.csv("Data/Drought_taxa_BPUEmatrix.csv")%>%
  left_join(WY, by=c("water_year"="Year"))%>%
  mutate(Yr_type=factor(Yr_type, levels=c("Wet", "Above Normal", "Below Normal", "Dry", "Critical")),
         Index_s=(Index-mean(Index))/sd(Index))%>%
  pivot_longer(cols = c(-water_year, -Index, -Index_s, -Yr_type, -Drought), names_to="Taxa", values_to="BPUE")
data$Year<-data$water_year

nonlog_taxa<-c("Acartiella.sinensis.Adult","All.taxa")

taxa_data<-dplyr::select(data,-water_year)%>%inner_join(regimes)
taxa_data$Taxa<-as.factor(taxa_data$Taxa)

#need to figure out which taxa and regime combos don't have enough data:
#Acartiella.sinensis.Adult Pre-clam and Pre-Pod
#Eurytemora.affinis.Juvenile Pre-clam
#Hyperacanthomysis.longirostris.Adult Pre-clam and Pre-pod
#Limnoithona.tetraspina.Adult Pre-clam
#	Neomysis.kadiakensis.Adult Tortanus.Adult
#Pseudodiaptomus.forbesi.Adult pre-clam
#Pseudodiaptomus.Juvenile Pre-clam
#Tortanus.Adult Pre-clam and Pre-pod

#count number of drought values per taxa and year
group_count<-taxa_data%>%
  drop_na(BPUE)%>%
  group_by(Taxa,Regime,Drought)%>%
  tally()
exclude_groups<-group_count%>%
  filter(n==1)%>%
  select(Taxa,Regime)%>%unique()

test<-taxa_data%>%anti_join(exclude_groups)%>%
  drop_na(BPUE)

test1<-test%>%
  nest(-Regime,-Taxa)%>%
  mutate(fit=map(data,~aov(BPUE~Drought,data=.)),
         results=map(fit,tidy))%>%
  unnest(results)

pvalues<-unique(select(test1,Taxa,Regime,p.value))%>%
  drop_na(p.value)

taxalist<-unique(test$Taxa)
for(t in 1:length(unique(test$Taxa))){
  taxa=taxalist[[t]]
  data<-test%>%filter(Taxa==taxa)
  p <- ggplot(data, aes(x = Index, y = BPUE)) +
    geom_point(aes())+
    geom_smooth(se = T, method = "lm") +
    ggtitle(paste("Drought BPUE by regime", taxa,sep=" "))+
    theme_classic()+
    facet_grid(~Regime,scales = "free_y")
  p
  save_plot(paste("Figures/taxa_regime/",taxa,".png",sep=""), p,base_height = 10,base_width = 16)
}

