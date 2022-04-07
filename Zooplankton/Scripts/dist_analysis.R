############
#Drought-MAST analysis
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

WY<-read_excel("Data/Water years.xlsx", sheet="yearassignments")
data<-read.csv("Data/distribution_matrix.csv")%>%
  left_join(WY, by=c("water_year"="Year"))%>%
  mutate(Yr_type=factor(Yr_type, levels=c("Wet", "Above Normal", "Below Normal", "Dry", "Critical")),
         Index_s=(Index-mean(Index))/sd(Index))%>%
  pivot_longer(cols = c(-water_year, -Index, -Index_s, -Yr_type, -Drought), names_to="Taxa", values_to="center_km")

# distributions
ggplot(data, aes(x=center_km))+
  geom_histogram(bins=10)+
  facet_wrap(~Taxa, scales = "free")+
  theme_bw()
ggsave("Figures/centerkm_hist.png",width = 10,height = 10)

#Let's do facet regression model outputs. 
#first use this function to pull out the p-value
lmp <- function (modelobject) {
  coeffs<- coef(summary(modelobject))
  p<-if (dim(coeffs)[[1]]==1) # if only one row of pvalues then NA
    p.value <- NA
  else 
    p.value <- coeffs[2,4]
  return(p)
}

#center of distribution by water year index
lm_eqn1 = function(taxa){
  m = lm(center_km~Index, data=filter(data, Taxa==taxa))
  eq <- substitute(~~italic(r)^2~"="~r2*","~~italic(p-value)*"="~pvalue,
                   list(intercept = format(coef(m)[1], digits = 2), 
                        slope = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3),
                        pvalue=format(lmp(m),digits=4)))
  eq<-as.character(as.expression(eq));                 
}
#italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2*","~~italic(p-value)*"="~pvalue

ck_index_models<-map(set_names(unique(data$Taxa)), lm_eqn1)
ck_index_eq<- pivot_longer(as.data.frame(ck_index_models),cols=everything(),names_to = "Taxa",values_to="equation")

p1 <- ggplot(data, aes(x = Index, y = center_km)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  geom_point(aes())+
  geom_smooth(se = T, method = "lm") +
  ggtitle(paste("Center of Distribution by Water Year Index",sep=" "))+
  theme_classic()+
  theme(plot.title = element_text(size=22))+
  geom_text(data=ck_index_eq,size=3,aes(-Inf,Inf,vjust="top",hjust="left",label=equation), parse = TRUE, inherit.aes=FALSE) + 
  facet_wrap(~Taxa,scales = "free_y")
p1
save_plot("Figures/dist_index.png",p1,base_height = 10,base_width = 16)

#distribution by W/N/D

#First a series of code to run models, pull out p-values, and assign to a dataset for plotting
#I couldn't figure out how to get this all to work off of one function, so did separate ones for each value I wanted
aov_eqn1 = function(taxa){
  m = (aov(center_km~Drought, data=filter(data, Taxa==taxa)))
  eq <- substitute(~~italic(p-value)*"="~pvalue,
                   list(pvalue = format(summary(m)[[1]][["Pr(>F)"]][1], digits = 3)))
  eq<-as.character(as.expression(eq))
}

aov_ND= function(taxa){
  m =(aov(center_km~Drought, data=filter(data, Taxa==taxa)))
  t<-TukeyHSD(m)
  t<-data.frame(t$Drought)
  tND1<-t[1,4]
  tND<-substitute(~~italic(paste("ND ",p-value))*"="~pvalue,
                  list(pvalue=format(tND1,digits=3)))
  tND<-as.character(as.expression(tND))
}

aov_WD= function(taxa){
  m =(aov(center_km~Drought, data=filter(data, Taxa==taxa)))
  t<-TukeyHSD(m)
  t<-data.frame(t$Drought)
  tWD1<-t[2,4]
  tWD<-substitute(~~italic(paste("WD ",p-value))*"="~pvalue,
                  list(pvalue=format(tWD1,digits=3)))
  tWD<-as.character(as.expression(tWD))
}

aov_WN= function(taxa){
  m =(aov(center_km~Drought, data=filter(data, Taxa==taxa)))
  t<-TukeyHSD(m)
  t<-data.frame(t$Drought)
  tWN1<-t[3,4]
  tWN<-substitute(~~italic(paste("WN ",p-value))*"="~pvalue,
                  list(pvalue=format(tWN1,digits=3)))
  tWN<-as.character(as.expression(tWN))
}

center_km_drought_eq1<-map(set_names(unique(data$Taxa)), aov_eqn1)
center_km_drought_ND<-map(set_names(unique(data$Taxa)), aov_ND)
center_km_drought_WD<-map(set_names(unique(data$Taxa)), aov_WD)
center_km_drought_WN<-map(set_names(unique(data$Taxa)), aov_WN)

center_km_drought_eq1<-data.frame(center_km_drought_eq1)
center_km_drought_ND<-data.frame(center_km_drought_ND)
center_km_drought_WD<-data.frame(center_km_drought_WD)
center_km_drought_WN<-data.frame(center_km_drought_WN)

dist_drought_eq1<- pivot_longer(as.data.frame(center_km_drought_eq1),cols=everything(),names_to = "Taxa",values_to="p1")
dist_drought_ND<- pivot_longer(as.data.frame(center_km_drought_ND),cols=everything(),names_to = "Taxa",values_to="pND")
dist_drought_WD<- pivot_longer(as.data.frame(center_km_drought_WD),cols=everything(),names_to = "Taxa",values_to="pWD")
dist_drought_WN<- pivot_longer(as.data.frame(center_km_drought_WN),cols=everything(),names_to = "Taxa",values_to="pWN")

dist_drought_pvalues<-dist_drought_eq1%>%
  inner_join(dist_drought_ND)%>%
  inner_join(dist_drought_WD)%>%
  inner_join(dist_drought_WN)

data<-data%>%filter(water_year<2021)
p2 <- ggplot(data, aes(x = Drought, y = center_km)) +
  geom_boxplot(aes())+
  ggtitle(paste("Center of Distribution (km) by Water Year Type",sep=" "))+
  theme_classic()+
  geom_text(data=dist_drought_pvalues,size=3,aes(-Inf,Inf,vjust="top",hjust="left",label=p1), parse = TRUE, inherit.aes=FALSE) + 
  geom_text(data=dist_drought_pvalues,size=3,aes(3,Inf,label=pND),vjust=1,hjust=.7, parse = TRUE, inherit.aes=FALSE) + 
  geom_text(data=dist_drought_pvalues,size=3,aes(3,Inf,label=pWD),vjust=2,hjust=.7, parse = TRUE, inherit.aes=FALSE) + 
  geom_text(data=dist_drought_pvalues,size=3,aes(3,Inf,label=pWN),vjust=3,hjust=.7, parse = TRUE, inherit.aes=FALSE) + 
  theme(plot.title = element_text(size=22))+
  facet_wrap(~Taxa,scales = "free_y")
p2
save_plot("Figures/dist_drought.png",p2,base_height = 10,base_width = 16)
