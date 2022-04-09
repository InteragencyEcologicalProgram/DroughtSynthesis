#cpue analysis
############
#Drought-MAST analysis
############
rm( list = ls()) #clear env

library(tidyverse)
library(readxl)
library(tibble)
library(dplyr)
library(ggplot2)
library(plotrix)
library(purrr)
library(cowplot)

annual_cpue<-read.csv("Data/zoop_drought_lt_cpue.csv")
regional_cpue<-read.csv("Data/zoop_drought_lt_REG.csv")
seasonal_cpue<-read.csv("Data/zoop_drought_lt_seasonal.csv")

WY<-read_excel("Data/Water years.xlsx", sheet="yearassignments")
WY$water_year<-WY$Year

################################################################
#Long-term analysis
################################################################

#annual model
#CPUE~Drought metric
annual_cpue<-annual_cpue%>%inner_join(WY)
hist(annual_cpue$an_CPUE)
hist(log(annual_cpue$an_CPUE))

m1<-aov(log(an_CPUE)~Drought,data=annual_cpue)
summary(m1)

ggplot(annual_cpue,aes(x = Drought, y = log(an_CPUE)))+
  geom_boxplot()

#CPUE~Drought + season
seasonal_cpue<-seasonal_cpue%>%inner_join(WY)
hist(seasonal_cpue$)