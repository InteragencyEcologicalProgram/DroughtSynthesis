#Short term analysis for clams
#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("lubridate")
#install.packages("sf")
#install.packages("devtools")
#install.packages("car")
#install.packages("emmeans")
#install.packages("MASS")
#install.packages("psc1")
#install.packages("boot")
#devtools::install_github("InteragencyEcologicalProgram/deltamapr")
#install.packages("glmmTMB")
############library####
library(dplyr)
library(tidyverse)
library(sf)
library(lubridate)
library(car)
library(emmeans)
library(MASS)
library(glmmTMB)
library(pscl)
library(boot)
GRTSdata <- read.csv("T2_GRTS_BioRecGR v2.0.csv")
regions <- read.csv("Rosies_regions.csv")
EMPCore <- read.csv("Clams_EMP_core_sites.csv")

###Laura edit: add the pound symbol before the text separating your sections, this will create separate subsections that you can drop down to using the arrows above the console
#you currently have only untitled sections

#################################################################################################### keep ##############
#################### load data and prep #########################################
#load deltamapr EDSM regions
Delta<-deltamapr::R_EDSM_Subregions_Mahardja_FLOAT

#Filter to subregions of interest and join regions
Deltadrought <- Delta %>%
  filter(SubRegion%in%unique(regions$SubRegion)) %>%
  dplyr::select(SubRegion)%>%left_join(regions)

#filter NAs out of GRTS coordinates
GRTSdata <- filter(GRTSdata, !is.na(long))

#add the subregions to the GRTS dataset
#convert stations file to simple features so we can map stations to subregions
stasSF = GRTSdata %>% st_as_sf(coords = c("long", "lat"), crs = 4326)
stasSF <- st_transform(stasSF, crs = st_crs(Deltadrought))

#Join regions to stations and remove geometry-

###Laura edit: keep either lat and long coordinates or site description so that you aren't dropping samples. See annotations on next chunk of code, below
#I kept the site descriptiona and created a new column called "Location" based on the Location and Geog.Area columns using 'unite'
subs = st_join(stasSF, Deltadrought)%>%filter(!is.na(Region))%>%
  st_drop_geometry() %>%
  #select(c(Year, Month, Clam.Density, Biomass, Grazing.Rate, Recruits, Clam, Region, Location, Geog.Area))%>%
  unite("Location", 9:10, sep="_") 
#it still looks like there are still duplicates, but when we remove them below we'll be sure they're duplicates at the same site and time

#Checking coordinate systems
#st_crs(Deltadrought)
#st_crs(stasSF)

#clean up GRTS data set, remove duplicate rows- 

###Laura edit: you don't want to remove duplicate rows unless you also keep the sampling location in the data frame, because some of these duplicates are samples from different locations within the same region
#removing duplicate rows in many instances leaves you with only one sample per clam per month per region (i.e., no replication, which could affect your statistical analyses below)
#instead, keep lat and long coordinates in line of code above or keep site description, and then remove duplicates, and take average within a region below before running analyses
subssampling <- subs%>%distinct()
unique(subssampling$Month) #some "May" have a space after the word and some don't, which is why you aren't converting all to numbers with one line of code
subssampling$Month[subssampling$Month=="Oct"]<-10
subssampling$Month[subssampling$Month=="May"]<-5
subssampling$Month[subssampling$Month=="May "]<-5
#subssampling$Month[subssampling$Month=="Oct"] <- "October" #change to full month name to convert to numbers
#subssampling$Month <- match(subssampling$Month, month.name) #convert month names to numbers 
subssampling1 <- subssampling %>% #add in seasons
  mutate(Season=case_when(Month%in%3:5 ~ "Spring", # Create seasonal variables
                                                Month%in%6:8 ~ "Summer",
                                                Month%in%9:11 ~ "Fall",
                                                Month%in%c(12, 1, 2) ~ "Winter",
                                                TRUE ~ NA_character_))
subssampling1$Month <- factor(subssampling1$Month)
subssampling1$Survey<-"GRTS" #add this to facilitate combining and summarizing datasets below

#clean up EMP data set
#convert dates to months and seasons
EMPCore1<-EMPCore%>% 
  mutate(Month = month(Date), #create a month and year variable
                                    Year = year(Date), 
                                    Year=if_else(Month==12, Year+1, Year), # Move Decembers to the following year
                                    Season=case_when(Month%in%3:5 ~ "Spring", # Create seasonal variables
                                                     Month%in%6:8 ~ "Summer",
                                                     Month%in%9:11 ~ "Fall",
                                                     Month%in%c(12, 1, 2) ~ "Winter",
                                                     TRUE ~ NA_character_))
#add in regions to EMP data set
stasEMP = EMPCore1 %>% st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
stasEMP <- st_transform(stasEMP, crs = st_crs(Deltadrought))
#st_crs(stasEMP)

#Join regions to stations and remove geometry
subsEMP = st_join(stasEMP, Deltadrought) %>%
  filter(!is.na(Region)) %>%
  st_drop_geometry() %>%
  #select(Year, SpeciesID, abundance, Missing, Month, Season.x, Long_term, Short_term, Region, StationCode) %>%
  distinct()

###Laura edit: if you combine this with GRTS data you will want to keep the location ID instead of lat and long so that you can combine the two data frames

EMPCore2 = left_join(unique(subsEMP), EMPCore1)
  #select(Year, SpeciesID, abundance, Missing, Month, Season, Region, Date, StationCode)

#filter for only PA and CF species
#speciesID PA=6890 CF=6730
clamspecies <- c("6890", "6730")
EMPCore3 <- EMPCore2 %>%
  filter(speciesID == clamspecies)

#rename species to CF and PA
EMPCore3$speciesID[EMPCore3$speciesID=="6890"] <- "PA"
EMPCore3$speciesID[EMPCore3$speciesID=="6730"] <- "CF"

#remove EMP data before 2007
EMPCore4 <- EMPCore3%>% filter(Year > 2006)%>%filter(Month %in% c(5,10))
EMPCore4$Month <- factor(EMPCore4$Month) #change Month column to factor so we can combine w/ GRTS data
EMPCore4$Survey<-"EMP" #add this to facilitate combining and summarizing datasets below

#############GRTS+EMP COMBINED DATA############# keep some? unsure about anovas #######################
#Density
density <- read.csv("short_term_density_ms.csv")
density <- subset(density, select=c(Region,SpeciesID,Density,Latitude,Longitude,Month,Year,Survey))
#run histo's to see if density data are zero inflated, and they are...
hist(log(density$Density[density$SpeciesID=="Corbicula fluminea"]),
     main="log CF Density")
hist(log(density$Density[density$SpeciesID=="Potamocorbula amurensis"]),
     main="log PA Density")

#biomass/grazing rate/density histograms
#data don't look zero inflated anymore....
biomass.gr <- read.csv("short_term_biomass_grazing_revised.csv")
biomass.gr <- subset(biomass.gr, select=c(Region,SpeciesID,Month,Year,Season,Survey,Biomass,Grazing))

hist((biomass.gr$Biomass[biomass.gr$SpeciesID=="Corbicula fluminea"]),
     main="log CF biomass")
hist((biomass.gr$Biomass[biomass.gr$SpeciesID=="Potamocorbula amurensis"]),
     main="log PA biomass")
hist((biomass.gr$Grazing[biomass.gr$SpeciesID=="Potamocorbula amurensis"]),
     main="log PA grazing")
hist((biomass.gr$Grazing[biomass.gr$SpeciesID=="Corbicula fluminea"]),
     main="log CF grazing")
hist((density$Density[density$SpeciesID=="Potamocorbula amurensis"]),
     main="log PA density")
hist((density$Density[density$SpeciesID=="Corbicula fluminea"]),
     main="log CF density")

hist(log(biomass.gr$Biomass[biomass.gr$SpeciesID=="Corbicula fluminea"]),
     main="log CF biomass")
hist(log(biomass.gr$Biomass[biomass.gr$SpeciesID=="Potamocorbula amurensis"]),
     main="log PA biomass")
hist(log(biomass.gr$Grazing[biomass.gr$SpeciesID=="Potamocorbula amurensis"]),
     main="log PA grazing")
hist(log(biomass.gr$Grazing[biomass.gr$SpeciesID=="Corbicula fluminea"]),
     main="log CF grazing")
hist(log(density$Density[density$SpeciesID=="Potamocorbula amurensis"]),
     main="log PA density")
hist(log(density$Density[density$SpeciesID=="Corbicula fluminea"]),
     main="log CF density")
hist(log(density$Density[density$SpeciesID=="Corbicula fluminea"]+1),
     main="log CF density+1")

#running ANOVAs on combined data
#CF Grazing Rate
CF = filter(biomass.gr, SpeciesID == "Corbicula fluminea")
CF.grazing.lm<-lm(log(Grazing+1)~as.factor(Year)+as.factor(Month), 
                  data = CF)
plot(CF.grazing.lm)

CF.grazing.anova<-Anova(CF.grazing.lm, type=2)
CF.grazing.anova
emmeansCF.gr<-emmeans(CF.grazing.lm, specs=pairwise ~Year,adjust="sidak")
print(test(emmeansCF.gr)$contrasts)
hist(CF.grazing.lm)
#CF biomass
CF.biomass.lm<-lm(log(Biomass+1)~as.factor(Year)+as.factor(Month), 
                  data = biomass.gr[biomass.gr$SpeciesID == "Corbicula fluminea",])
plot(CF.biomass.lm)

CF.biomass.anova<-Anova(CF.biomass.lm, type=2)
CF.biomass.anova
emmeansCF.bio<-emmeans(CF.biomass.lm, specs=pairwise ~Year,adjust="sidak")
print(test(emmeansCF.bio)$contrasts)
hist(CF.biomass.lm)
#CF density
CF.density.lm<-lm(log(Density+1)~as.factor(Year)+as.factor(Month), 
                  data = density[density$SpeciesID == "Corbicula fluminea",])
CF.density.anova<-Anova(CF.density.lm, type=2)
CF.density.anova
emmeansCF.den<-emmeans(CF.density.lm, specs=pairwise ~Year,adjust="sidak")
print(test(emmeansCF.den)$contrasts)
hist(CF.density.lm)

#PA Grazing rate
PA.grazing.lm<-lm(log(Grazing+1)~as.factor(Year)+as.factor(Month), 
                  data = biomass.gr[biomass.gr$SpeciesID == "Potamocorbula amurensis",])
PA.grazing.anova<-Anova(CF.grazing.lm, type=2)
PA.grazing.anova
emmeansPA.gr<-emmeans(PA.grazing.lm, specs=pairwise ~Year,adjust="sidak")
print(test(emmeansPA.gr)$contrasts)
hist(PA.grazing.lm)
#PA biomass
PA.biomass.lm<-lm(log(Biomass+1)~as.factor(Year)+as.factor(Month), 
                  data = biomass.gr[biomass.gr$SpeciesID == "Potamocorbula amurensis",])
PA.biomass.anova<-Anova(PA.biomass.lm, type=2)
PA.biomass.anova
emmeansPA.bio<-emmeans(PA.biomass.lm, specs=pairwise ~Year,adjust="sidak")
print(test(emmeansPA.bio)$contrasts)
hist(PA.biomass.lm)
#PA density
PA.density.lm<-lm(log(Density+1)~as.factor(Year)+as.factor(Month), 
                  data = density[density$SpeciesID == "Potamocorbula amurensis",])
PA.density.anova<-Anova(PA.density.lm, type=2)
PA.density.anova
emmeansPA.den<-emmeans(PA.density.lm, specs=pairwise ~Year,adjust="sidak")
print(test(emmeansPA.den)$contrasts)
hist(PA.density.lm)

#################### for Drought synthesis report, not including in manuscript ####
#one grazer at a time. filter to regions where it occurs
CFgr = filter(biomass.gr, SpeciesID == "Corbicula fluminea", !Year %in% c(2013, 2016, 2020))
hist(log(CFgr$Grazing*100+1))
hist(CFgr$Grazing)

ggplot(CFgr, aes(x = as.factor(Year), y = Grazing)) + geom_boxplot() + facet_wrap(~Region)

ggplot(CFgr, aes(x = Year, y = Grazing)) + geom_point()


g2 = lm(log(Grazing*100+1)~as.factor(Year) + Region + Month, 
           data = CFgr)
g2a<-Anova(g2, type=2)
g2a
plot(g2)

CFg = glmmTMB(Grazing*100~ Region + as.factor(Year) + (1|Month), zi = ~Region, 
               family = "nbinom2", data = CFgr)

summary(CFg)
eg1 = emmeans(CFg, specs=pairwise ~Year,adjust="sidak")
plot(eg1)
Anova(CFg)
tukCFg = cld(eg1$emmeans, Letters = letters)
tukCFg$SpeciesID = "Corbicula fluminea"
tukCFg$y = 1800


PAgr = filter(biomass.gr, SpeciesID == "Potamocorbula amurensis", Region %in% 
                  c("Confluence", "Suisun Bay", "Suisun Marsh"), !Year %in% c(2013, 2016, 2020)) 

g3 = lm(Grazing~ as.factor(Year), data = PAgr)

summary(g3)


PAg = glmmTMB(Grazing*100~ Region + as.factor(Year) + (1|Month), zi = ~Region, 
              family = "nbinom2", data = PAgr)

summary(PAg)
PA1 = emmeans(PAg, specs=pairwise ~Year,adjust="sidak")
plot(PA1)
Anova(PAg)
tukPAg = cld(PA1$emmeans, Letters = letters)
tukPAg$SpeciesID = "Potamocorbula amurensis"
tukCF$y = 1800
tukg = bind_rows(tukPAg, tukCFg)

################################################## keep for MS, re-do with count data #########
#Zero inflated models

library(glmmTMB)

library(multcomp)
#convert to counts
CFden = filter(density, SpeciesID == "Corbicula fluminea", !Year %in% c(2013, 2016, 2020)) %>%
  mutate(Density2 = round(Density*0.052))

CFz1 = glmmTMB(Density2~ Region *as.factor(Year) + (1|Month), zi = ~Region, 
               family = "nbinom2", data = CFden)

summary(CFz1)
ecf1 = emmeans(CFz1, specs=pairwise ~Year,adjust="sidak")
plot(ecf1)
ecf2= emmeans(CFz1, specs=pairwise ~Region,adjust="sidak")
plot(ecf2)
ecf3= emmeans(CFz1, specs=pairwise ~Year*Region,adjust="sidak")
plot(ecf3)

Anova(CFz1, type=3)
tukCF = cld(ecf1$emmeans, Letters = letters)
tukCF$SpeciesID = "Corbicula fluminea"
tukCF$y = 1800
# 
# PAden = filter(density, SpeciesID == "Potamocorbula amurensis", Year != 2020) %>%
#   mutate(Density2 = round(Density))
# 
# PAz1 = glmmTMB(Density2~ Region + as.factor(Year) + as.factor(Month), zi = ~., 
#                family = "nbinom2", data = PAden)
# 
# summary(PAz1)
# emmeans(PAz1, specs=pairwise ~Year,adjust="sidak")
#Hm. that is ugly. THis might be happening because there are so few PA in the norh and south central

PAden2 = filter(density, SpeciesID == "Potamocorbula amurensis", Region %in% 
                  c("Confluence", "Suisun Bay", "Suisun Marsh"), !Year %in% c(2013, 2016, 2020)) %>%
  mutate(Density2 = round(Density*0.052))

PAz2 = glmmTMB(Density2~ Region*as.factor(Year) + as.factor(Month), zi = ~., 
               family = "nbinom2", data = PAden2)

summary(PAz2)

#much better!!!
#let's put month as a random effect instead of fixed
PAz3 = glmmTMB(Density2~ Region*as.factor(Year) + (1|Month), zi = ~Region, 
               family = "nbinom2", data = PAden2)

summary(PAz3)
Anova(PAz3, type=3)
emmeans(PAz3, specs=pairwise ~Year,adjust="sidak")
EMMPAz2 = emmeans(PAz3, specs=pairwise ~Year,adjust="sidak", component = "cond")
plot(EMMPAz2)
EMMPAz3 = emmeans(PAz3, specs=pairwise ~Region,adjust="sidak", component = "cond")
plot(EMMPAz3)
EMMPAz4= emmeans(PAz3, specs=pairwise ~Year*Region,adjust="sidak", component = "cond")
plot(EMMPAz4)

tukPA = cld(EMMPAz2$emmeans, Letters = letters)
tukPA$SpeciesID = "Potamocorbula amurensis"
tukPA$y = 10000

tukdens = bind_rows(tukPA, tukCF)
#plot metrics w/ combined data

############################### total grazing rate analysis, keep for MS #########################################################################
library(lme4)
library(lmerTest)
library(multcomp)
library(multcompView)
Grazing = biomass.gr%>%filter(!Year %in% c(2013, 2016, 2020))%>%
  group_by(SpeciesID, Year, Season, Month, Region) %>%
  summarize(grazing=mean(Grazing, na.rm=T))%>%
  ungroup()%>%
  group_by(Year, Season, Month, Region)%>%
  summarize(grazing = sum(grazing, na.rm = T))

hist(log(Grazing$grazing+1)) #looks ok

G1<-lmer(log(grazing+1)~ as.factor(Year)*Region+(1|Month), data = Grazing)
summary(G1)
G1a<-Anova(G1, type=3)
G1a
plot(G1)
emmeansG<-emmeans(G1, specs=pairwise ~Year,adjust="sidak", component = "cond")
print(test(emmeansG)$contrasts)
plot(emmeansG)

################################# Plots, keep for MS #############################################################################################

#total grazing
yrs = read_csv("yearassignments.csv")
Grazing = left_join(Grazing, yrs) %>%
  mutate(Yr_type = factor(Yr_type, levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"))) %>%
  filter(!Year %in% c(2013, 2016, 2020))
#box plots by year

ggplot(Grazing, aes(x = as.factor(Year), y = grazing)) + geom_boxplot()

#average by region first
Grze = group_by(Grazing, Year, Drought, ShortTerm, Month, Season, Region, Yr_type) %>%
  summarize(grazing = mean(grazing, na.rm = T))%>%mutate(Yr_type = factor(Yr_type, levels = c("Critical", "Dry","Below Normal", "Wet"))) %>%
  mutate(Region= factor(Region, levels=c("North", "SouthCentral","Confluence", "Suisun Marsh", "Suisun Bay"),labels=c("North", "South Central","Confluence", "Suisun Marsh", "Suisun Bay")))

#plot combined average grazing rate for MS
pal_yrtype <- c( "Critical" = "#FDE333","Dry"="#53CC67", "Below Normal" = "#009B95","Wet" = "#481F70FF")

grazing_plot<-ggplot() + geom_boxplot(data = Grze, aes(x = Yr_type, y = log(grazing+1), fill = Yr_type), alpha=0.7)+
  xlab("Year type") +
  scale_x_discrete(breaks=c("Critical", "Dry","Below Normal", "Wet"),
                   labels=c("C", "D","BN", "W"))+
  scale_fill_manual(name="Year type", values=pal_yrtype)+theme_bw() +  ylab("log(Grazing Rate [m3/m2/day])") + facet_grid(cols=vars(Region))

ggsave("grazing_plot.tiff", grazing_plot, device = "tiff", height = 5, width = 8)

########### revised biomass and density plots by species ################################################################################### keep #####
#biomass and density
bio = group_by(biomass.gr, Year, SpeciesID, Month, Season, Survey, Panel, Region) %>%
  summarize(Biomass = mean(Biomass, na.rm = T)) %>%
  group_by(Year,  Month, Season, SpeciesID, Region) %>%
  summarize(Biomass = mean(Biomass, na.rm = T)) %>%
  left_join(yrs) %>%
  mutate(Yr_type = factor(Yr_type, levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet")))%>%
  mutate(Region= factor(Region, levels=c("North", "SouthCentral","Confluence", "Suisun Marsh", "Suisun Bay")))

ggplot(bio, aes(x = as.factor(Year), y = Biomass, fill = Yr_type)) + geom_boxplot()+ scale_fill_manual(name="Year type", breaks=c("Critical","Dry", "Below Normal", "Wet"),labels=c("C", "D","BN", "W"))+
  scale_fill_viridis_d(direction = -1)+theme_bw() + ylab("Biomass") + facet_grid(rows=vars(SpeciesID), cols=vars(Region))

#density
dens = group_by(density, Year, SpeciesID, Month, Survey, Region) %>%
  summarize(density = mean(Density, na.rm = T)) %>%
  group_by(Year,  Month, SpeciesID, Region) %>%
  summarize(density = mean(density, na.rm = T)) %>%
  left_join(yrs) %>%
  mutate(Yr_type = factor(Yr_type, levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"))) %>%
  filter(!Year %in% c(2013, 2016, 2020))%>%
  mutate(Region= factor(Region, levels=c("North", "SouthCentral","Confluence", "Suisun Marsh", "Suisun Bay")))

density_plot<-ggplot() + geom_boxplot(data = dens, aes(x = Yr_type, y =density, fill=Yr_type), alpha=0.7)+
  #geom_text(data = tukdens, aes(x = as.factor(Year), y = -110, label = .group))+
  scale_x_discrete(breaks=c("Critical","Dry", "Below Normal", "Wet"),
                   labels=c("C","D", "BN", "W"))+
  xlab("Year type")+
  scale_fill_manual(name="Year type", values=pal_yrtype)+theme_bw() + ylab("Density (clams/m2)") + facet_grid(rows=vars(SpeciesID), cols=vars(Region), scales="free_y")

ggsave("density_plot.tiff", density_plot, device = "tiff", height = 5, width = 8)
##########Gamma Hurdle model################# unsure ####
#DENSITY
#fit a logistic regression to predict probability of non-zero value 
density$non_zero <-ifelse(density$Density > 0.000000, 1, 0) #change raw data to 0/1 
glmden1 <- glm(non_zero~1, data=density,family=binomial(link=logit))
summary(glmden1)
#Gamma GLM with a log link to predict the mean of the non-zero data.
glmden2 <- glm(Density~1, data=subset(density, non_zero==1),family=Gamma(link=log))

#extract coefficients and 95% confidence intervals
(bin_coef <- plogis(coef(glmden1)[[1]])) #output=0.518231
(gamma_coef <- exp(coef(glmden2)[[1]])) #output=1968.151
(plogis(confint(glmden1)))
#    2.5 %    97.5 % 
#0.5032029 0.5332371 
(exp(confint(glmden2)))
#   2.5 %   97.5 % 
#1801.837 2155.588 

#check predictions
(pred <- exp(log(bin_coef)+log(gamma_coef))) #1019.957
mean(density$Density) #1019.957

#run hurdle
hurdle_fn <- function(density,i){
  dat_boot <- density[i, ]
  m1 <- glm(non_zero~1, data=density,
            family=binomial(link=logit))
  m2 <- glm(Density~1, data=subset(density,non_zero==1),
            family=Gamma(link=log))
  bin_coef <- plogis(coef(m1)[[1]])
  gamma_coeg <- exp(coef(m2)[[1]])
  exp(log(bin_coef)+log(gamma_coef))
}
#bootstrap CI calculations
library(boot)
denboot <- boot(density, hurdle_fn, R=10000)
b.ci.den <- boot.ci(denboot,type="bca")
print(b.ci.den)

############ calculate clearance rate for clams for short-term data in Suisun Bay, Suisun Marsh, Confluence at 8 m depth ################
#output as table for Rosie
#group by water year type and region

Clearance<-Grazing%>%mutate(Clearance=grazing/8)%>%group_by(Yr_type, Region)%>%mutate(mean_clearance=mean(Clearance))%>%mutate(max_clearance=max(Clearance))%>%
  dplyr::select(-c(grazing, Index, Drought, ShortTerm, Year, Season, Month, SprNDOI,Clearance))%>%unique()


write_csv(Clearance, "Clam_clearance.csv")
