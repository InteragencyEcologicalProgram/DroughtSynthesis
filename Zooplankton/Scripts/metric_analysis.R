############
#Drought metric analysis
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
library(car)

library(emmeans)
library(multcomp)
library(rsq)
library(readxl)
WY<-read_excel("Zooplankton/Data/Water years.xlsx", sheet="yearassignments")
WY$water_year<-WY$Year

Drought_LT_bpue_year<-read.csv("Zooplankton/Data/zoop_drought_lt_bpue_year.csv")
Drought_LT_bpue_year<-Drought_LT_bpue_year%>%inner_join(WY)
Drought_LT_bpue_reg<-read.csv("Zooplankton/Data/zoop_drought_lt_bpue_reg.csv")
Drought_LT_bpue_reg<-Drought_LT_bpue_reg%>%inner_join(WY)
Drought_LT_bpue_szn<-read.csv("Zooplankton/Data/zoop_drought_lt_bpue_szn.csv")
Drought_LT_bpue_szn<-Drought_LT_bpue_szn%>%inner_join(WY)
Drought_ST_bpue<-read.csv("Zooplankton/Data/zoop_drought_st_bpue.csv")%>%inner_join(WY)

#Drought_LT_BPUE analysis
#histograms first
hist(Drought_LT_bpue_year$y_BPUE)
hist(log(Drought_LT_bpue_szn$s_BPUE))
hist(Drought_LT_bpue_reg$r_BPUE)
hist(log(Drought_LT_bpue_reg$r_BPUE))
hist(Drought_ST_bpue$BPUE_ug)
hist(log(Drought_ST_bpue$BPUE_ug))


#models
#BPUE versus drought
#Anova works better than aov for unbalanced designs
m1b = lm(y_BPUE~Drought,data=Drought_LT_bpue_year)
m1ba = Anova(m1b)
rsq(m1b)

#pairwise comparisons
emmeans(m1b, pairwise ~ Drought)

m1<-aov(y_BPUE~Drought,data=Drought_LT_bpue_year)
capture.output(summary(m1),file="Outputs/m1.txt")


#Now include season
m2<-aov(log(s_BPUE)~Drought+Season,data=Drought_LT_bpue_szn)

#let's check out the interaction too, cause why not?
m2a = lm(log(s_BPUE)~Drought*Season,data=Drought_LT_bpue_szn)
Anova(m2a)

#pairwise comparisons
m2em = emmeans(m2a, pairwise ~ Drought*Season)

#get the groups that are not significantly different from each other
tukzoopseason = cld(m2em$emmeans, Letters = letters)
rsq(m2a)

capture.output(summary(m2),file="Outputs/m2.txt")

#Now do the same thing by region.
m3<-aov(log(r_BPUE)~Drought+Region+Drought*Region,data=Drought_LT_bpue_reg)
m3a<-lm(log(r_BPUE)~Drought+Region+Drought*Region,data=Drought_LT_bpue_reg)
m3b<-Anova(m3a)
m3b

m3em = emmeans(m3a, pairwise ~ Drought*Region)
m3test = emmeans(m3a, pairwise ~ Drought)
tukzoop = cld(m3em$emmeans, Letters = letters)
rsq(m3a)

capture.output(summary(m3),file="Outputs/m3.txt")
TukeyHSD(m3)
lm3<-lm(log(r_BPUE)~Drought+Region+Drought*Region,data=Drought_LT_bpue_reg)
anova(lm3)
capture.output(anova(lm3),file="Outputs/anovalm3.txt")
capture.output(summary(lm3),file="Outputs/summarylm3.txt")

summary(lm3)

#Drought_ST_BPUE analysis
m4<-aov(log(BPUE_ug)~factor(water_year)*Region+month,data=Drought_ST_bpue)
capture.output(summary(m4),file="Outputs/m4.txt")

#####################################################################
###Drought_LT_CPUE analysis
###Commenting out because no longer using CPUE
#Drought_LT_cpue_year<-read.csv("Data/zoop_drought_lt_cpue_year.csv")
#Drought_LT_cpue_year<-Drought_LT_cpue_year%>%inner_join(WY)
#Drought_LT_cpue_reg<-read.csv("Data/zoop_drought_lt_cpue_reg.csv")
#Drought_LT_cpue_reg<-Drought_LT_cpue_reg%>%inner_join(WY)
#Drought_LT_cpue_szn<-read.csv("Data/zoop_drought_lt_cpue_szn.csv")
#Drought_LT_cpue_szn<-Drought_LT_cpue_szn%>%inner_join(WY)

#m5<-aov(log(CPUE)~Drought,data=Drought_LT_cpue_year)
#capture.output(summary(m5),file="Outputs/m5.txt")
#m6<-aov(log(CPUE)~Drought+Season,data=Drought_LT_cpue_szn)
#capture.output(summary(m6),file="Outputs/m6.txt")
#m7<-aov(log(CPUE)~Drought+Region,data=Drought_LT_cpue_reg)
#capture.output(summary(m7),file="Outputs/m7.txt")

#Drought_ST_CPUE analysis
#m8<-aov(log(CPUE)~factor(st_index)+month+Region,data=Drought_ST_cpue)
#capture.output(summary(m8),file="Outputs/m8.txt")
#####################################################################

############
#Plots
###########
library(viridis)
library(DroughtData)

pm1<-ggplot(Drought_LT_bpue_year, aes(x = Drought, y = y_BPUE, fill=Drought)) +
  geom_boxplot(aes())+
  theme_bw()+
  drt_color_pal_drought(aes_type = "fill")+
  theme(text = element_text(size=24))+
  xlab("Year Type")+ylab("Avg Yearly BPUE")
pm1
save_plot("Figures/pm1.png",pm1,base_height = 4,base_width = 6)
pm2<-ggplot(Drought_LT_bpue_szn, aes(x = Drought, y = s_BPUE,fill=Drought)) +
  geom_boxplot(aes())+
  geom_text(data = tukzoopseason, aes(x = Drought, y = 10, label = .group))+
  facet_wrap(~Season)+
  theme_bw()+
  drt_color_pal_drought(aes_type = "fill")+
  theme(axis.text.x = element_text(angle = 45, hjust=1),text=element_text(size=24))+
  xlab("Year Type")+ylab("Avg Seasonal BPUE")
pm2
save_plot("Zooplankton/pm2.png",pm2,base_height = 4,base_width = 8)

#regional BPUE
pm3<-ggplot(Drought_LT_bpue_reg, aes(x = Drought, y = log(r_BPUE),fill=Drought)) +
  geom_boxplot(aes())+facet_grid(cols=vars(Region))+
  geom_text(data = tukzoop, aes(x = Drought, y = 10, label = .group))+
  theme_bw()+
  drt_color_pal_drought(aes_type = "fill")+
  theme(axis.text.x = element_text(angle = 45, hjust=1),text=element_text(size=20))+
  xlab("Year Type")+ylab("log(Avg Regional BPUE)")
pm3
save_plot("Zooplankton/pm3.png",pm3,base_height = 4,base_width = 8)

#short term BPUE
pm4<-ggplot(Drought_ST_bpue, aes(x = factor(water_year), y = BPUE_ug,fill=Yr_type)) +
  geom_boxplot()+facet_grid(~Region)+
  theme_bw()+
  drt_color_pal_yrtype(aes_type = "fill")+
  theme(axis.text.x = element_text(angle = 45, hjust=1),text=element_text(size=20))+
  xlab("Water Year")+ylab("Avg Monthly BPUE")
pm4
save_plot("Figures/pm4.png",pm4,base_height = 8,base_width = 12)

#pm5<-ggplot(Drought_LT_cpue_year, aes(x = Drought, y = CPUE)) +
#  geom_boxplot(aes())+
#  theme_classic()+
#  theme(text = element_text(size=24))
#save_plot("Figures/pm5.png",pm5,base_height = 4,base_width = 6)
#pm6<-ggplot(Drought_LT_cpue_szn, aes(x = Drought, y = CPUE)) +
#  geom_boxplot(aes())+facet_wrap(~Season)+
#  theme_classic()+
#  theme(axis.text.x = element_text(angle = 45, hjust=1),text=element_text(size=24))
#save_plot("Figures/pm6.png",pm6,base_height = 4,base_width = 6)
#pm7<-ggplot(Drought_LT_cpue_reg, aes(x = Drought, y = CPUE)) +
#  geom_boxplot(aes())+facet_wrap(~Region)+  theme_classic()+
#  theme(axis.text.x = element_text(angle = 45, hjust=1),text=element_text(size=24))
#save_plot("Figures/pm7.png",pm7,base_height = 4,base_width = 6)
#pm8<-ggplot(Drought_ST_cpue, aes(x = st_index, y = CPUE)) +
#  geom_boxplot(aes())+facet_grid(~Region)+  theme_classic()+
#  theme(axis.text.x = element_text(angle = 45, hjust=1),text=element_text(size=24))
#save_plot("Figures/pm8.png",pm8,base_height = 8,base_width = 12)

