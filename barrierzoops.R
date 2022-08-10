#Zooplankton barrier analyssi

library(zooper)
library(tidyverse)
library(lubridate)
library(sf)
library(readxl)
library(RColorBrewer)
library(lme4)
library(lmerTest)
library(emmeans)
library(multcomp)
library(glmmTMB)
library(DHARMa)
library(vegan)

mypal = c(brewer.pal(8, "Dark2"), brewer.pal(8, "Set2"), "black", "white")

#pull zooplankton 2014-2021 from zooper

MyZoops <- Zoopsynther(Data_type = "Community",
                       Sources = c("EMP", "20mm","STN", "FMWT"),
                       Size_class = "Meso",
                       Date_range = c("2014-01-01", "2021-12-30"))

#subset just regions surrounding the Barrier. 
regs = read_sf("EDB/Spatial_data/EDB_Regions.shp") %>%
  st_make_valid()

MyZoopssf = st_as_sf(MyZoops, coords = c("Longitude", "Latitude"), crs = 4326)

MyZoopssf = st_join(MyZoopssf, regs) %>%
  st_drop_geometry() %>%
  filter(!is.na(Regions))

#group taxonomic names into larger categories
crosswalk = read_excel("data/zoopcrosswalk.xlsx")

MyZoops = mutate(MyZoopssf, Taxname = str_remove(Taxname, "_UnID")) %>%
  left_join(crosswalk) %>%
  filter(!Undersampled)

#stacked bar plot by region and year

MyZoopssub = group_by(MyZoops, Source, SampleID, Regions, Year, Date, Station, Analy2) %>%
  summarize(CPUE = sum(CPUE))

samps = group_by(MyZoops, Regions, Year) %>%
  summarize(N = length(unique(SampleID)))

ggplot(MyZoopssub, aes(x = Regions, y = CPUE, fill = Analy2))+ geom_col()+
  geom_text(data = samps, aes(x = Regions, y = 4000000, label = N), inherit.aes = FALSE)+
  scale_fill_manual(values = mypal)+
  facet_wrap(~Year)


#it looks like we only have EMP for 2021, so maybe stick to that survey? Also, mayb ejust hte summer months

EMPzoops = filter(MyZoopssub, Source == "EMP")
EMPsamps = group_by(EMPzoops, Regions, Year) %>%
  summarize(N = length(unique(SampleID)))

ggplot(EMPzoops, aes(x = Regions, y = CPUE, fill = Analy2))+ geom_col()+
  geom_text(data = EMPsamps, aes(x = Regions, y = 900000, label = N), inherit.aes = FALSE)+
  facet_wrap(~Year)

#Need averages rather than totals
EMPzoopsave = group_by(EMPzoops, Regions, Year, Analy2) %>%
  summarize(CPUEm = mean(CPUE), sdzoop = sd(CPUE), se = sdzoop/n())

ggplot(EMPzoopsave, aes(x = Regions, y = CPUEm, fill = Analy2))+ geom_col()+
  geom_text(data = EMPsamps, aes(x = Regions, y = 5000, label = N), inherit.aes = FALSE)+
  scale_fill_manual(values = mypal)+
  facet_wrap(~Year)

###############################################
#now total zooplankton catch, with error bars.

EMPtots = group_by(EMPzoops, Regions, Year, SampleID, Station) %>%
  summarize(CPUE = sum(CPUE), rCPUE = round(CPUE),Yearf = as.factor(Year)) %>%
  distinct()


EMPtotsave = group_by(EMPtots, Regions, Year) %>%
summarize(CPUEm = mean(CPUE), sdzoop = sd(CPUE), se = sdzoop/sqrt(n()))

ggplot(EMPtotsave, aes(x = Regions, y = CPUEm, fill = Regions)) + geom_col()+
  geom_errorbar(aes(ymin = CPUEm - se, ymax = CPUEm + se))+
  facet_wrap(~Year)

#What's the distribution like?
ggplot(EMPtots, aes(x = CPUE)) + geom_histogram()
ggplot(EMPtots, aes(x = log(CPUE))) + geom_histogram()

#linear model based on year, region, and interaction, with station as random effect

zoop1 = lmer(log(CPUE) ~ Yearf*Regions  + (1|Station), data = EMPtots)
summary(zoop1)
plot(zoop1)
emmeans(zoop1, pairwise ~ Yearf:Regions)

emmeans(zoop1, pairwise ~ Regions, by = "Yearf")

#OK, now try it with a negative binomial instead
zoop2 = glmmTMB(rCPUE~ Yearf*Regions  + (1|Station), family = nbinom2(), data = EMPtots)
summary(zoop2)
#I like that better! Looks like what I was expecting

res = simulateResiduals(zoop2)
plot(res)
testOutliers(res, type = "bootstrap")

#Beautiful. Sacramenot is lower, 2017 is differnet in the Sacramento
#no clear impact of the Barrier

#####################################################################
#Multivariate stats

#Create a species matrix
EMPmat = pivot_wider(EMPzoops, id_cols = c(Regions, Year, SampleID, Station, Date), 
                     names_from = Analy2, values_from = CPUE)
EMPenv = dplyr::select(EMPmat, Regions, Year, SampleID, Station, Date) %>%
  mutate(Yearf = as.factor(Year), Regions  = as.factor(Regions))
EMPspecs = dplyr::select(ungroup(EMPmat), -Regions, -Year, -SampleID, -Station, -Date)

a1 = adonis2(EMPspecs ~ Regions*Yearf, data = EMPenv)
a1
#region and year are significant, but interaction is not.


#NMDS
metaMDS(EMPspecs, trymax = 200)
#Hm. Not working. Let's try relative abundance

EMPspecs2 = EMPspecs/rowSums(EMPspecs)


adonis2(EMPspecs2 ~ Regions*Yearf, data = EMPenv)
znmds = metaMDS(EMPspecs2, trymax = 200)

source("plotNMDS.R")
PlotNMDS(znmds, EMPenv, group = "Regions")
#that's weird and I dont like it. I think we can say not that much differnece bewteen regions
PlotNMDS(znmds, EMPenv, group = "Yearf")
