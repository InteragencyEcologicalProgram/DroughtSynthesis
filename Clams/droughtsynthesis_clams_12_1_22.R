#Drought Synthesis 2021, Clam edition, Long-term analysis
#Department of Water Resources
#Leela Dixit and Laura Twardochleb
#Last updated 10/22/21
#goals: Sampling effort per region, summary metrics for clam density, biomass, and grazing rate

#packages
#install.packages("reshape2")
library(reshape2)
library(readxl)
library(plotly)
#devtools::install_github("sbashevkin/spacetools")
#devtools::install_github("InteragencyEcologicalProgram/deltamapr")
library(spacetools) #use function GGdist to calculate distance from Golden Gate Bridge
library(deltamapr)
lapply(c("deltamapr", "tidyverse", "lubridate", "sf"), require, character.only = TRUE)
library(car)
library(emmeans)
library(multcomp)


############### long-term data initial data manipulation ######################################################################################
#read in data
#abundance data with only 
abundance <- read_excel(("1975-Oct2020 CPUE, 20210511.xlsx")
                        #specify sheet and cell range
                        , range = "75-20 CPUE m2!A8:PD4534"
                        , col_names = T)

#taxonomy only#specify sheet and cell range
taxonomy <- read_excel(("1975-Oct2020 CPUE, 20210511.xlsx")
                       , range = "75-20 CPUE m2!E2:PD8"
                       , col_names = F)
#make species ID into the header, remove species ID row, convert to character type
names(taxonomy) <- as.matrix(taxonomy[7, ])
#taxonomy <- taxonomy[-7, ]
taxonomy[] <- lapply(taxonomy, function(x) type.convert(as.character(x)))

#add column for taxonomy levels
#create column
taxonomy$taxonomy <- c("phylum", "class", "order", "family", "genus", "species", "speciesID")
#move new column to the front
taxa <- taxonomy %>% 
  relocate(taxonomy)

#rotate data frame
taxa1 <- t(taxa)

#new data frame with only clams
taxa2 <- as_tibble(taxa1) #read as a tibble
names(taxa2) <- as.matrix(taxa2[1, ]) #make taxa column the header
taxa2 <- taxa2[-1, ] #remove the taxa row
#filter data frame for clams
clams <- filter(taxa2, phylum=="Mollusca", class=="Bivalvia")
species<-clams$speciesID #these species IDs are clams
#"6890" "6869" "6870" "6895" "6679" "6680" "6690" "6710" "6670" "6915" "6715" "6910" "6910" "6779" "6778" "6860" "6730" "6780" "6782" "6740" "6750" "6770" "6809" "6810" "6825" "6820" "6850" "6830"


#melt the data frame to be able to plot multiple columns
clam_ab <- abundance %>%mutate(across(everything(),as.character))%>% pivot_longer(cols=c(5:420),names_to="speciesID", values_to="abundance")%>%
  dplyr::filter(speciesID%in%species)%>%
  group_by(Year, speciesID)

#new data frame with genus/species in one column, species ID in another
clam2 <- left_join(clam_ab, taxa2, by="speciesID")
#clam2_plot <- ggplot(clam2, aes(Year, mean)) + geom_bar(aes(colour=species), stat="identity")
#ggplotly(clam2_plot)

#combine taxa2, abundance tables with site location tables
#read-in site location table
site_codes<-read_csv("clam_site_codes.csv")
#clam_ab_long<-pivot_longer(clam_ab,cols=4:29,names_to="SpeciesID", values_to="abundance", names_transform = list(SpeciesID=as.character), values_transform = list(abundance=as.character))
clam_sites<-left_join(clam_ab, site_codes, by=c("StationCode"="Site Code"))

########### EMP long-term data, summarize by region ###########################################################################################################################################################################################################################################
#read in region data
rosies_regions<-read_csv("Rosies_regions.csv")

## Load Delta Shapefile from Brian
Delta<-deltamapr::R_EDSM_Subregions_Mahardja_FLOAT

#Filter to subregions of interest and join regions
Deltadrought <- Delta%>%
  filter(SubRegion%in%unique(rosies_regions$SubRegion))%>%
  dplyr::select(SubRegion)%>%left_join(rosies_regions)

#add the subregions to the clam dataset
#convert stations file to simple features so we can map stations to subregions
stasSF = clam_sites %>% st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  #convert to UTMs so it's in the same coordinate reference system as the Delta shapefile
  st_transform(crs = 26910)

#Join regions to stations and remove geometry
subs = st_join(stasSF, Deltadrought) %>%
  filter(!is.na(Region)) %>%
  st_drop_geometry() %>%
  dplyr::select(Region, StationCode)

# join regions to original clam abundance data set 
clams_sites2 = left_join(unique(subs), clam_sites)

############### Summarize long-term sample coverage by season, year, region ###################################################################

#summary of samples/region, season, year: need the sampling date, not just the year
#collapse abundance data to presence/absence of sites: subset dataset to remove abundance column and then take unique rows
clams_sites3<-clams_sites2%>%dplyr::select(-c(abundance, Year,speciesID, Missing, `Period of Record (From)`, `Period of Record (To)`, Status))%>%unique()

#convert dates to months and seasons
clams_sites4<-clams_sites3%>%mutate(Month = month(Date), #create a month and year variable
       Year = year(Date), 
       Year=if_else(Month==12, Year+1, Year), # Move Decembers to the following year
       Season=case_when(Month%in%3:5 ~ "Spring", # Create seasonal variables
                        Month%in%6:8 ~ "Summer",
                        Month%in%9:11 ~ "Fall",
                        Month%in%c(12, 1, 2) ~ "Winter",
                        TRUE ~ NA_character_))

#plot distribution of samples in each region, over each season, year
#summarize count of samples in each region by season by years
site_counts<-clams_sites4%>%group_by(Region, Year, Season)%>%summarize(n=n())
  
#sites_plot_confluence<-site_counts%>%filter(Region=="Confluence")%>%ggplot(aes(x=Year,y=n))+geom_jitter(aes())+
#  facet_wrap(vars(Season))
#sites_plot_confluence

#look at seasonal sample coverage fo EMP clam data
sites_plot<-site_counts%>%ggplot(aes(x=Year,y=Region))+geom_tile(aes(fill=n))+
  scale_fill_gradient(low="white",high="red")+
  geom_text(aes(label=round(n,1)),size=4)+
  ggtitle("EMP Clam Sampling By Season")+
  facet_grid(Season~.)+
  theme_classic()+
  theme(legend.position = "none")
sites_plot
ggsave("clam_seasonal_sampling_coverage.png", sites_plot,  width=14, height=8)

#look at monthly sample coverage for EMP clam data
site_counts2<-clams_sites4%>%group_by(Region, Year, Month)%>%summarize(n=n())
sites_plot2<-site_counts2%>%ggplot(aes(x=Year,y=Region))+geom_tile(aes(fill=n))+
  scale_fill_gradient(low="white",high="red")+
  geom_text(aes(label=round(n,1)),size=4)+
  ggtitle("EMP Clam Sampling By Month")+
  facet_grid(Month~.)+
  theme_classic()+
  theme(legend.position = "none")
sites_plot2
ggsave("clam_monthly_sampling_coverage.png", sites_plot2,  width=14, height=8)

#look at yearly sample coverage for EMP clam data
site_counts3<-clams_sites4%>%group_by(Region, Year)%>%summarize(n=n())
sites_plot3<-site_counts3%>%ggplot(aes(x=Year,y=Region))+geom_tile(aes(fill=n))+
  scale_fill_gradient(low="white",high="red")+
  geom_text(aes(label=round(n,1)),size=4)+
  ggtitle("EMP Clam Sampling By Year")+
  theme_classic()+
  theme(legend.position = "none")
sites_plot3
ggsave("clam_yearly_sampling_coverage.png", sites_plot3,  width=14, height=8)

############### long-term data, summary of clam abundance across delta and suisun marsh by season and year #########################################
#Step 3 - Calculate monthly averages and medians for each Region. Use the regions listed in "Rosies_regions.csv". (and table below) 
#Step 4 - Calculate seasonal averages and medians for each Region by averaging the monthly averages as defined above.  
#Step 5a - Calculate a seasonal average and median for the entire Delta by averaging the regional averages. 
#Step 5b - Calculate regional averages and medians for each year (based on seasonal averages for each region).
#assign seasons
clam_seasonal<-clams_sites2%>%mutate(Month = month(Date), #create a month and year variable
                                    Year = year(Date), 
                                    Year=if_else(Month==12, Year+1, Year), # Move Decembers to the following year
                                    Season=case_when(Month%in%3:5 ~ "Spring", # Create seasonal variables
                                                     Month%in%6:8 ~ "Summer",
                                                     Month%in%9:11 ~ "Fall",
                                                     Month%in%c(12, 1, 2) ~ "Winter",
                                                     TRUE ~ NA_character_))
#calculate averages- filter to two species of interest: 6890 is P. amurensis, 6730 is C. fluminea
#rename clam species
clam_seasonal$speciesID[clam_seasonal$speciesID=="6890"]<-"Potamocorbula amurensis"
clam_seasonal$speciesID[clam_seasonal$speciesID=="6730"]<-"Corbicula fluminea"

species<-c("Potamocorbula amurensis", "Corbicula fluminea")

#summarize sampling coverage for EMP across years: how many sites? 
n_stations_by_region<-clam_seasonal%>%dplyr::select(Year, Region, StationCode, Date)%>%distinct()%>%group_by(Year, Region)%>%summarize(n_samples=n())
coverage<-n_stations_by_region%>%ggplot(aes(x=as.factor(Year),y=Region))+geom_tile(aes(fill=n_samples))+
  scale_fill_gradient(low="white",high="red")+
  #geom_text(aes(label=round(n,1)),size=4)+
  ggtitle("EMP Clam Sampling By Region")+
  theme_classic()+
  theme(legend.position = "none")

monthly_avg<-clam_seasonal%>%filter(speciesID %in%species)%>%group_by(Region, Year, Month, speciesID)%>%
  mutate(monthly_avg_abundance=mean(as.numeric(abundance)))%>%mutate(monthly_median_abundance=median(as.numeric(abundance)))%>%
  distinct(Region,Year, Month,speciesID, .keep_all = TRUE)%>%
  dplyr::select(-c(StationCode, Date, abundance,`Location and channel position`,Latitude, Longitude,`Period of Record (From)`, `Period of Record (To)`, Missing, Status))

seasonal_avg_region<-monthly_avg%>%group_by(Region, Year, Season, speciesID)%>%mutate(seasonal_avg_by_region=mean(monthly_avg_abundance))%>%
  mutate(seasonal_median_by_region=mean(monthly_median_abundance))%>%
  distinct(Region,Year, Season,speciesID, .keep_all = TRUE)%>%dplyr::select(-c(Month,monthly_avg_abundance, monthly_median_abundance))

seasonal_avg_year<-seasonal_avg_region%>%group_by(Year, Season, speciesID)%>%mutate(seasonal_avg_by_year=mean(seasonal_avg_by_region))%>%
  mutate(seasonal_median_by_year=mean(seasonal_median_by_region))%>%distinct(Year, Season,speciesID, .keep_all = TRUE)%>%
  dplyr::select(-c(Region,seasonal_avg_by_region, seasonal_median_by_region))
write_csv(seasonal_avg_year, "seasonal_clam_density_long_term.csv")
  
regional_avg_year<-seasonal_avg_region%>%group_by(Region,Year, speciesID)%>%mutate(regional_avg_by_year=mean(seasonal_avg_by_region))%>%
  mutate(regional_median_by_year=mean(seasonal_median_by_region))%>%distinct(Region, Year, speciesID, .keep_all = TRUE)%>%
  dplyr::select(-c(Season,seasonal_avg_by_region, seasonal_median_by_region))
write_csv(regional_avg_year, "regional_clam_density_long_term.csv")

annual_abundance_index<-regional_avg_year%>%group_by(Year, SpeciesID)%>%mutate(average_abundance=mean(regional_avg_by_year))%>%
  mutate(median_abundance=mean(regional_median_by_year))%>%distinct(Year, SpeciesID, .keep_all = TRUE)%>%
  select(-c(regional_avg_by_year, regional_median_by_year))
write_csv(annual_abundance_index,"annual_clam_density_long_term.csv")
#write separate csv files with summaries for each of the following: regional avg by year, seasonal avg across delta by year, annual index for delta

###################### Explore long-term clam density by region, season, year ##################################################################################################################
hist(annual_abundance_index$average_abundance)
hist(log(annual_abundance_index$average_abundance))


################ Calculate distance to Golden Gate for Potamocorbula for long-term dataset ###########################################################################################################################
#use clam_seasonal, add a column for distance to Golden Gate for each site, then summarize as above for sites with Potamocorbula present
#use site codes data frame to calculate distance to Golden Gate
#use GGDist function to calculate distance (in meters) to Golden Gate- CRS defaults to WGS84
distance<-GGdist(Water_map = spacetools::Delta, Points = site_codes, Latitude_column = Latitude,
                Longitude_column = Longitude, PointID_column = 'Site Code')

#filter for sites with P. amurensis presence and then join the distance data
species2<-c("Potamocorbula amurensis")
clam_seasonal_potamo<-clam_seasonal%>%filter(speciesID %in%species2)
clam_seasonal_potamo2<-merge(clam_seasonal_potamo, distance, by.x="StationCode", by.y = "Site Code")

#weighted distance by density:multiply distance by CPUE of. P. amurensis, add up distances and divide by total cpue for each year
#annual_weighted_distance<-clam_seasonal_potamo2%>%group_by(Year, StationCode)%>%mutate(total_CPUE=sum(as.numeric(abundance)))%>%mutate(distanceXcpue=Distance*total_CPUE)%>%group_by(Year)%>%summarize(mean_weighted_distance=sum(distanceXcpue)/sum(total_CPUE))
#write_csv(annual_weighted_distance, "annual_weighted_potamo_distance.csv")

#plot weighted mean distance by year index
#read in year assignments
yeartypes = read_csv("yearassignments.csv")%>%mutate(Year=as.character(Year))

#potamodist=left_join(annual_weighted_distance, yeartypes, by=c("Year"="Year"))
#potamodist=potamodist%>%mutate(year_factor=if_else(Year_fac>1999, "short_term", "long_term"))

#dist_modeled = lm(Distance~ lag(Index), data = potamodist)
#summary(dist_modeled)
#plot(dist_modeled)

#ggplot(potamodist, aes(x = lag(Index), y = mean_weighted_distance/1000)) + 
  #geom_point() + ylab("Center of Potamocorbula distribution (km from Golden Gate)")+
  #xlab("Previous Year's Sacramento Vally Index") + geom_smooth(method = "lm")+ theme_bw()

#weighted_mean<-ggplot(potamodist, aes(x = lag(Index), y = mean_weighted_distance/1000, fill=year_factor, colour=year_factor)) + 
  #geom_point() + ylab("Center of Potamocorbula distribution (km from Golden Gate)")+
  #xlab("Sacramento Vally Index") + geom_smooth(method = "lm")+ theme_bw()

#calculate mean, max distance by month, season, year
#monthly_distance<-clam_seasonal_potamo2%>%group_by(Year, Season, Month)%>%summarize(monthly_avg_distance=mean(Distance), maximum_distance=max(Distance))

#seasonal_distance<-monthly_distance%>%group_by(Year, Season)%>%summarize(seasonal_avg_distance=mean(monthly_avg_distance), maximum_distance=max(maximum_distance))
#write_csv(seasonal_distance, "seasonal_potamo_distance.csv")

#yearly_distance<-seasonal_distance%>%group_by(Year)%>%summarize(yearly_avg_distance=mean(seasonal_avg_distance), maximum_distance=max(maximum_distance))
#write_csv(yearly_distance, "yearly_potamo_distance.csv")

####### calculate distance by binning stations into 10 km bins and then calculating weighted center of distribution ###########################################
#mean abundance by bins
annual_weighted_distance_binned<-clam_seasonal_potamo2%>%mutate(Year=as.factor(Year))%>%mutate(bins=cut(Distance, breaks=c(40000,50000, 60000, 70000, 80000, 90000, 100000, 110000, 120000)))%>%
  group_by(Year, bins)%>%mutate(total_CPUE=mean(as.numeric(abundance)))%>%mutate(distanceXcpue=Distance*total_CPUE)%>%group_by(Year)%>%summarize(mean_weighted_distance=sum(distanceXcpue)/sum(total_CPUE))

#plot weighted mean distance by year index- mean of abundances by bin
potamodist2=left_join(annual_weighted_distance_binned, yeartypes, by=c("Year"="Year"))
potamodist2=potamodist2%>%mutate(year_factor=if_else(Year>1999, "short_term", "long_term"))

viridis_colours<-c("#481567FF", "#3CBB75FF")
binned_weighted_mean<-ggplot(potamodist2, aes(x = lag(Index), y = mean_weighted_distance/1000, colour=year_factor)) + 
  geom_point() + ylab("Center of Potamocorbula distribution (km from Golden Gate)")+
  scale_colour_manual(values=viridis_colours, name="Sampling period", breaks=c("long_term", "short_term"), labels=c("1987-1999", "2000-2020"))+
  xlab("Previous Year's Sacramento Valley Index") + geom_smooth(method = "lm")+ theme_bw()

#### examine sample coverage by bin 
sample_coverage_bins<-clam_seasonal_potamo2%>%mutate(Year=as.factor(Year))%>%mutate(bins=cut(Distance, breaks=c(40000,50000, 60000, 70000, 80000, 90000, 100000, 110000, 120000)))%>%
  group_by(Year, bins)%>%summarize(n=n())
  
######## model selection for best-fitting model- binned mean data

clam_mod0 = lm(mean_weighted_distance~ Index, data = potamodist2) 
summary(clam_mod0)
plot(clam_mod0)

clam_mod1 = lm(mean_weighted_distance~ lag(Index), data = potamodist2) 
summary(clam_mod1)
plot(clam_mod1)

clam_mod2 = lm(mean_weighted_distance~ lag(Index)+ year_factor, data = potamodist2) 
summary(clam_mod2)
plot(clam_mod2)

clam_mod3 = lm(mean_weighted_distance~ lag(Index)*year_factor, data = potamodist2) 
summary(clam_mod3)
plot(clam_mod3)

clam_mod4=lm(mean_weighted_distance~ year_factor, data = potamodist2)
summary(clam_mod4)
AIC(clam_mod4)


AIC(clam_mod1, clam_mod2, clam_mod3, clam_mod4) #mod 3 is best fit to data

#plot changes in Potamocorbula and Corbicula abundance by region, over time
pal_yrtype2 <- c( "Critical" = "#FDE333", "Dry" = "#53CC67", "Below Normal" = "#009B95","Above Normal" = "#00588B", "Wet" = "#481F70FF")

potamo_plot = clam_seasonal%>%filter(speciesID%in% species)%>%mutate(Year=as.character(Year))%>%left_join(yeartypes)%>%left_join(distance, by=c("StationCode"="Site Code"))%>%mutate(km=Distance/1000)%>%filter(Year>1986)%>%mutate(Region= factor(Region, levels=c("North", "SouthCentral","Confluence", "Suisun Marsh", "Suisun Bay"),labels=c("North", "South Central","Confluence", "Suisun Marsh", "Suisun Bay")))%>%
  ggplot(aes(x = as.factor(Year), y = as.numeric(abundance), fill=Yr_type))+geom_boxplot(alpha=0.7)+ scale_fill_manual(name="Year type",values=pal_yrtype2)+facet_grid(vars(speciesID),vars(Region))+
  scale_x_discrete(breaks=c(1980, 1990, 2000, 2010, 2020))+
  xlab("Year")+ ylab("Density (clams/m2)")+
  theme_bw()

ggsave("potamo_plot.tiff", potamo_plot, device = "tiff", height = 5, width = 8)

potamo_plot_freey = clam_seasonal%>%filter(speciesID%in% species)%>%mutate(Year=as.character(Year))%>%left_join(yeartypes)%>%left_join(distance, by=c("StationCode"="Site Code"))%>%mutate(km=Distance/1000)%>%filter(Year>1986)%>%mutate(Region= factor(Region, levels=c("North", "SouthCentral","Confluence", "Suisun Marsh", "Suisun Bay"),labels=c("North", "South Central","Confluence", "Suisun Marsh", "Suisun Bay")))%>%
  ggplot(aes(x = as.factor(Year), y = as.numeric(abundance), fill=Yr_type))+geom_boxplot(alpha=0.7)+ scale_fill_manual(name="Year type",values=pal_yrtype2)+facet_grid(vars(speciesID),vars(Region), scales="free")+
  scale_x_discrete(breaks=c(1980, 1990, 2000, 2010, 2020))+
  xlab("Year")+ ylab("Density (clams/m2)")+
  theme_bw()

ggsave("potamo_plot_freey.tiff", potamo_plot_freey, device = "tiff", height = 5, width = 8)


################### zero-inflated nb model for Potamocorbula density ##########################################################################################
potamo_monthly_region<-monthly_avg%>%filter(speciesID=="Potamocorbula amurensis")%>%filter(Region%in% c("Confluence", "Suisun Bay"))
hist(log(potamo_monthly_region$monthly_avg_abundance+1)) #still zero-inflated

#Zero inflated models

library(glmmTMB)

library(multcomp)
#convert to counts
PAden = potamo_monthly_region%>%mutate(Density2 = round(monthly_avg_abundance*0.052))

#had to drop South Central region- too many zeros for model to converge
PAdenmod = glmmTMB(Density2~ Region *Year + (1|Month), zi = ~Region, 
               family = "nbinom2", data = PAden)

summary(PAdenmod)
plot(PAden)

#try with seasonally averaged data
potamo_seasonal_region<-seasonal_avg_region%>%filter(speciesID=="Potamocorbula amurensis")%>%filter(Region%in% c("Confluence", "Suisun Bay"))
hist(log(potamo_seasonal_region$seasonal_avg_by_region+1))

#convert to counts
PAden2 = potamo_seasonal_region%>%mutate(Density2 = round(seasonal_avg_by_region*0.052))

#had to drop South Central region- too many zeros for model to converge- this mdoel looks good but the main effects are a little confusing
PAdenmod2 = glmmTMB(Density2~ Region *Year + (1|Season), zi = ~Region, 
                   family = "nbinom2", data = PAden2)

summary(PAdenmod2)

save.image("drought_synthsis.RData")
######## use GAMs to predict max distance to Golden Gate Bridge ###############################################################################################

library(mgcv)

#bind distance and clam abundance data frames
potamo_distance<-  merge(clam_seasonal, distance, by.x="StationCode", by.y = "Site Code")

#filter to just potamocorbula, round abundance
potamo_distance2<-potamo_distance%>%filter(speciesID %in%species2)%>%mutate(CPUE=round(as.numeric(abundance, digits=0)))%>%mutate(Year_fac=as.factor(Year))

#Warning message:
#In newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS, L = G$L,  :Fitting terminated with step failure - check results carefully when grouped by year
GAM = gam(CPUE ~ Year_fac+s(Distance, by=Year_fac, k =6), data = potamo_distance2, family = nb)

#try with just a global smoother for distance- works
GAM2<-gam(CPUE ~ s(Distance, k =6), data = potamo_distance2, family = nb)
#works
datx = filter(potamo_distance2, CPUE != 0)
Newdat = data.frame(Distance = min(datx$Distance):max(datx$Distance))
preds = as.data.frame(predict(GAM2, newdata = Newdat, type = "response")) %>%
  rename(Predictions = `predict(GAM2, newdata = Newdat, type = "response")`)
Newdat = bind_cols(Newdat, preds)
CenterX = data.frame(Max = max(Newdat$Predictions),Distance =
                       Newdat$Distance[which(Newdat$Predictions== max(Newdat$Predictions))])

plot(GAM2)
summary(GAM2)

#works
CenterMod = function(dat) {
  datx = filter(dat, CPUE != 0)
  if(nrow(datx)>0) {
    GAM = gam(CPUE ~ s(Distance, k =8), data = dat, family = nb)
    Newdat = data.frame(Distance = min(datx$Distance):max(datx$Distance))
    preds = as.data.frame(predict(GAM, newdata = Newdat, type = "response")) %>%
      rename(Predictions = `predict(GAM, newdata = Newdat, type = "response")`)
    Newdat = bind_cols(Newdat, preds)
    CenterX = data.frame(Max = max(Newdat$Predictions),Distance =
                           Newdat$Distance[which(Newdat$Predictions== max(Newdat$Predictions))])
    Center =CenterX } else
      Center = data.frame(Max = 0, Distance = NA)
    return(Center)
}

#global function for 2011: highest CPUE is site closest to the Golden Gate Bridge
potamo_distance_2011<-potamo_distance2%>% filter(Year_fac==2011)
GAM = gam(CPUE ~ s(Distance, k =8), data = potamo_distance_2011, family = nb)

#apply center of distribution function
Centers =group_by(potamo_distance2, Year_fac) 
  do(CenterMod(.))

#plot responses
#clamdist with modeled distance
Centers=Centers%>%mutate(Year_fac=as.character(Year_fac))

potamodist=left_join(Centers, yeartypes, by=c("Year_fac"="Year"))
potamodist=potamodist%>%mutate(year_factor=if_else(Year_fac>1999, "short_term", "long_term"))

dist_modeled = lm(Distance~ lag(Index), data = potamodist)
summary(dist_modeled)
plot(dist_modeled)

ggplot(potamodist, aes(x = lag(Index), y = mean_weighted_distance/1000)) + 
  geom_point() + ylab("Center of Potamocorbula distribution (km from Golden Gate)")+
  xlab("Previous Year's Sacramento Vally Index") + geom_smooth(method = "lm")+ theme_bw()

ggplot(potamodist, aes(x = lag(Index), y = Distance/1000, fill=year_factor, colour=year_factor)) + 
  geom_point() + ylab("Center of Potamocorbula distribution (km from Golden Gate)")+
  xlab("Sacramento Vally Index") + geom_smooth(method = "lm")+ theme_bw()
  

write_csv(Centers, "potamo_center.csv")

save.image()


############### long-term grazing and biomass data from USGS (calculated from long-term EMP density data) #####################################################
#label NAs in USGS dataset
#change date format in USGS dataset to YYYY-MM-DD
#change species names in USGS dataset

usgs<-read_csv("Monthly_Bivalve_Metrics_All.csv", na=c(""))
#replace all N/D with zeros
usgs$Biomass[usgs$Biomass=="N/D"|usgs$Biomass=="n/d"]<-0
usgs$Grazing_Rate[usgs$Grazing_Rate=="N/D"|usgs$Grazing_Rate=="n/d"]<-0
usgs$`Clam _Density`[usgs$`Clam _Density`=="N/D"|usgs$`Clam _Density`=="n/d"]<-0
usgs$Filtration_Rate[usgs$Filtration_Rate=="N/D"]<-0
usgs$Average_length[usgs$Average_length=="N/D"]<-0
usgs$Recruits[usgs$Recruits=="N/D"]<-0
usgs$Date<-as.Date(usgs$Date, format= "%m/%d/%Y")
usgs$Clam[usgs$Clam=="PA"]<-"Potamocorbula amurensis"
usgs$Clam[usgs$Clam=="CF"]<-"Corbicula fluminea"

#assign years and seasons
usgs2<-usgs%>%mutate(Month = month(Date), #create a month and year variable
                                     Year = year(Date), 
                                     Year=if_else(Month==12, Year+1, Year), # Move Decembers to the following year
                                     Season=case_when(Month%in%3:5 ~ "Spring", # Create seasonal variables
                                                      Month%in%6:8 ~ "Summer",
                                                      Month%in%9:11 ~ "Fall",
                                                      Month%in%c(12, 1, 2) ~ "Winter",
                                                      TRUE ~ NA_character_))

#assign region to USGS dataset
#convert stations file to simple features so we can map stations to subregions
stasSF2 = usgs2 %>% st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  #convert to UTMs so it's in the same coordinate reference system as the Delta shapefile
  st_transform(crs = 26910)

#Join regions to stations and remove geometry
subs2 = st_join(stasSF2, Deltadrought) %>%
  filter(!is.na(Region)) %>%
  st_drop_geometry() %>%
  dplyr::select(Region, Station)

# join regions to original clam abundance data set, remove NA values in Date
usgs_clams_sites = left_join(unique(subs2), usgs2)

usgs_clams_sites2<-filter(usgs_clams_sites, !is.na(Season))

#summarize biomass and grazing metrics
monthly<-usgs_clams_sites2%>%group_by(Region, Year, Month, Clam)%>%
  mutate(monthly_avg_biomass=mean(as.numeric(Biomass)))%>%mutate(monthly_median_biomass=median(as.numeric(Biomass)))%>%
  mutate(monthly_avg_grazing=mean(as.numeric(Grazing_Rate)))%>%mutate(monthly_median_grazing=median(as.numeric(Grazing_Rate)))%>%
  distinct(Region,Year, Month,Clam, .keep_all = TRUE)%>%
  select(-c(Station, Date, Biomass, Grazing_Rate, `Clam _Density`, Filtration_Rate, Average_length, Recruits, Latitude, Longitude))

seasonal_region<-monthly%>%group_by(Region, Year, Season, Clam)%>%mutate(seasonal_avg_biomass_region=mean(monthly_avg_biomass))%>%
  mutate(seasonal_avg_grazing_region=mean(monthly_avg_grazing))%>%
  distinct(Region,Year, Season,Clam, .keep_all = TRUE)%>%select(-c(Month,monthly_avg_biomass, monthly_median_biomass, monthly_avg_grazing, monthly_median_grazing))

seasonal_year<-seasonal_region%>%group_by(Year, Season, Clam)%>%mutate(seasonal_avg_biomass_year=mean(seasonal_avg_biomass_region))%>%
  mutate(seasonal_avg_grazing_year=mean(seasonal_avg_grazing_region))%>%
  distinct(Year, Season,Clam, .keep_all = TRUE)%>%
  select(-c(Region,seasonal_avg_biomass_region, seasonal_avg_grazing_region))
write_csv(seasonal_year, "seasonal_yearly_clam_biomass_grazing_long_term.csv")

regional_year<-seasonal_region%>%group_by(Region,Year, Clam)%>%mutate(regional_avg_biomass_year=mean(seasonal_avg_biomass_region))%>%
  mutate(regional_avg_grazing_year=mean(seasonal_avg_grazing_region))%>%
  distinct(Region, Year, Clam, .keep_all = TRUE)%>%
  select(-c(Season,seasonal_avg_biomass_region, seasonal_avg_grazing_region))
write_csv(regional_year, "regional_yearly_clam_biomass_grazing_long_term.csv")

annual_index<-regional_year%>%group_by(Year, Clam)%>%mutate(average_biomass=mean(regional_avg_biomass_year))%>%
  mutate(average_grazing=mean(regional_avg_grazing_year))%>%
  distinct(Year, Clam, .keep_all = TRUE)%>%
  select(-c(regional_avg_biomass_year, regional_avg_grazing_year))
write_csv(annual_index,"annual_clam_biomass_grazing_long_term.csv")


############# long-term analysis ANOVA for drought/wet effects ####################################################################################################################################################
#Step 6 - Perform an ANOVA (or similar analysis, as appropriate) with one of the following structures: 
  #Metric ~ Drought/Wet (if using an annual index for the whole Delta) 
  #Metric ~ Drought/Wet + Season 
  #Metric ~ Drought/Wet + Region 


############ what are the 10 core EMP sites? ###################################################################################################################################################################################################################################
EMP_core_sites<-filter(clam_sites, Status=="Active")
unique(EMP_core_sites$StationCode)
write_csv(EMP_core_sites, "Clams_EMP_core_sites.csv")

############### short-term data manipulation ###################################################################################################################################################
#summarize density data coverage for GRTS and EMP core sites
#read-in GRTS data
grts<-read_csv("T2_GRTS_BioRecGR v2.0.csv")
#combine grts and EMP core sites data before summarizing: characterize both datasets by month, season, year

#emp data manipulation- need to correct if going to combine these updated for analyses for 2007 to 2019-> column names are off between the two datasets
emp_core2<-EMP_core_sites%>%dplyr::select(-c(Year,Missing, `Period of Record (From)`, `Period of Record (To)`, Status))%>%
  mutate(Month = month(Date), #create a month and year variable
                                    Year = year(Date), 
                                    Year=if_else(Month==12, Year+1, Year), # Move Decembers to the following year
                                    Season=case_when(Month%in%3:5 ~ "Spring", # Create seasonal variables
                                                     Month%in%6:8 ~ "Summer",
                                                     Month%in%9:11 ~ "Fall",
                                                     Month%in%c(12, 1, 2) ~ "Winter",
                                                     TRUE ~ NA_character_))%>% dplyr::select(-Date)
#filter to two species of interest: 6890 is P. amurensis, 6730 is C. fluminea
emp_core2$speciesID[emp_core2$speciesID=="6890"]<-"Potamocorbula amurensis"
emp_core2$speciesID[emp_core2$speciesID=="6730"]<-"Corbicula fluminea"
emp_core3<-filter(emp_core2, speciesID %in%species)
colnames(emp_core3)<-c("Year", "Month", "StationCode", "SpeciesID", "Density", "Location", "Latitude", "Longitude", "Season")
emp_core3$Survey<-"EMP"
emp_core3$Panel<-"EMP"
emp_core3$Biomass<-NA
emp_core3$Grazing_rate<-NA
emp_core3<-emp_core3%>%dplyr::select(-c(Location, Panel))
emp_core4<-emp_core3%>%dplyr::select(order(colnames(emp_core3)))%>%mutate(across(everything(), as.character))
emp_core4$Year<-as.character(emp_core4$Year)

#grts data manipulation
grts2<-grts%>%unite("Location", 7:8, sep="_")
grts2$Clam[grts2$Clam=="PA"]<-"Potamocorbula amurensis"
grts2$Clam[grts2$Clam=="CF"]<-"Corbicula fluminea"

grts2$Month[grts2$Month=="May"]<-5
grts2$Month[grts2$Month=="Oct"]<-10

grts2<-grts2%>%dplyr::select(c(SiteID, Year, Month, lat, long, 'Clam Density', Biomass, 'Grazing Rate', Clam))

colnames(grts2)<-c("StationCode", "Year", "Month", "Latitude", "Longitude", "Density","Biomass", "Grazing_rate","SpeciesID")
grts2$Survey<-"GRTS"
grts3<-grts2%>%mutate(Year=if_else(Month==12, Year+1, Year), # Move Decembers to the following year
       Season=case_when(Month%in%3:5 ~ "Spring", # Create seasonal variables
                        Month%in%6:8 ~ "Summer",
                        Month%in%9:11 ~ "Fall",
                        Month%in%c(12, 1, 2) ~ "Winter",
                        TRUE ~ NA_character_))
grts4<-grts3%>%dplyr::select(order(colnames(grts3)))%>%mutate(across(everything(), as.character()))

emp_grts<-rbind(emp_core4, grts4)

#drop rows with NAs in lat and long
emp_grts2<-emp_grts%>%filter(!is.na(Latitude))

#summarize count of samples in each region by season by years
#add the subregions to the clam dataset
#convert stations file to simple features so we can map stations to subregions
stasSF2 = emp_grts2 %>% st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  #convert to UTMs so it's in the same coordinate reference system as the Delta shapefile
  st_transform(crs = 26910)

#Join regions to stations and remove geometry
subs2 = st_join(stasSF2, Deltadrought) %>%
  filter(!is.na(Region)) %>%
  st_drop_geometry() %>%
  dplyr::select(Region, StationCode)

# join regions to original clam density data set 
emp_grts3 = left_join(unique(subs2), emp_grts2)%>%filter(Month%in%c(5,10))%>%filter(Year>=2007)
write.csv(emp_grts3, "short_term_density_ms.csv")

site_counts2<-emp_grts2%>%select(-c(SpeciesID, Density))%>%distinct()%>%group_by(Region, Year, Month)%>%summarize(n=n())

#need to change these plots to separate by clam species- also avoid double-counting sites
#plot sample coverage by month, year, region- includes GRTS stations that change each year
emp_grts_plot<-site_counts2%>%ggplot(aes(x=as.factor(Year),y=Region))+geom_tile(aes(fill=n))+
  scale_fill_gradient(low="white",high="red")+
  geom_text(aes(label=round(n,1)),size=4)+
  ggtitle("EMP+GRTS Clam Sampling By Month")+
  facet_grid(Month~.)+
  theme_classic()+
  theme(legend.position = "none")
emp_grts_plot
ggsave("emp_grts_clam_monthly_sampling_coverage.png", emp_grts_plot,  width=14, height=8)

#plot sample coverage by month, year, region- includes only 50 core GRTS stations
emp_grts_plot2<-emp_grts2%>%select(-c(SpeciesID, Density))%>%filter(Panel%in%c("Annual", "EMP"))%>%filter(Month%in%c(5,10))%>%
  filter(Year>=2011)%>%distinct()%>%group_by(Region, Year, Month)%>%summarize(n=n())%>%
  ggplot(aes(x=as.factor(Year),y=Region))+geom_tile(aes(fill=n))+
  scale_fill_gradient(low="white",high="red")+
  geom_text(aes(label=round(n,1)),size=4)+
  ggtitle("EMP+GRTS Clam Sampling By Month")+
  facet_grid(Month~.)+
  theme_classic()+
  theme(legend.position = "none")
emp_grts_plot2
ggsave("emp_grts_clam_monthly_sampling_coverage2.png", emp_grts_plot2,  width=14, height=8)

################## summarize density metrics for short-term analysis ####################################################################################################
monthly_avg_short_term<-emp_grts2%>%filter(Month%in%c(5,10))%>%filter(Year>=2011)%>%group_by(Region, Year, Month, SpeciesID)%>%
  mutate(monthly_avg_density=mean(as.numeric(Density)))%>%mutate(monthly_median_density=median(as.numeric(Density)))%>%
  group_by(Region, Year, SpeciesID)%>%mutate(annual_mean_density=mean(as.numeric(Density)))%>%mutate(annual_median_density=median(as.numeric(Density)))%>%
  distinct(Region,Year, Month,SpeciesID, .keep_all = TRUE)%>%
  select(-c(StationCode, Location,Latitude, Longitude, Survey, Panel))
write_csv(monthly_avg_short_term,"annual_clam_density_short_term.csv")

#plot CF density metrics by year, region, drought status
cf_density_plot<-monthly_avg_short_term%>%filter(SpeciesID=="Corbicula fluminea")%>%
  ggplot(aes(x=as.factor(Year), y=annual_mean_density))+geom_col()+
  ggtitle("Corbicula fluminea Density")+
  facet_grid(Region~.)+
  theme_classic()
cf_density_plot
ggsave("emp_grts_cf_monthly_sampling_coverage.png", cf_density_plot,  width=12, height=14)

#plot PA density metrics by year, region (take average by year), drought status
pa_density_plot<-monthly_avg_short_term%>%filter(SpeciesID=="Potamocorbula amurensis")%>%
  ggplot(aes(x=as.factor(Year), y=annual_mean_density))+geom_col()+
  ggtitle("Potamocorbula amurensis Density")+
  facet_grid(Region~.)+
  theme_classic()
pa_density_plot
ggsave("emp_grts_pa_clam_monthly_sampling_coverage.png", pa_density_plot,  width=12, height=14)

############# summarize coverage and metrics for short-term biomass and grazing ###################################

#change Station names in EMP dataset as follows: C9-L -> C9L
emp_grts3$StationCode<-str_replace_all(emp_grts3$StationCode, "-", "")
emp_grts3$Year<-as.numeric(emp_grts3$Year)
emp_grts3$Month<-as.numeric(emp_grts3$Month)

#join usgs biomass and grazing data (usgs_clams_sites2) with emp_grts2 and subset to years and months matching grts data
short_term_clams<-left_join(emp_grts3, usgs_clams_sites2, by=c("Region"="Region","Year"="Year","Month"="Month","Season"="Season","StationCode"="Station", "SpeciesID"="Clam"))%>%
  filter(Month%in%c(5,10))%>%filter(Year>=2007)

#combine biomass and grazing columns
short_term_clams$Biomass<-NA
short_term_clams$Biomass<-if_else(!is.na(short_term_clams$Biomass.x), short_term_clams$Biomass.x, short_term_clams$Biomass.y)
short_term_clams$Grazing<-NA
short_term_clams$Grazing<-if_else(!is.na(short_term_clams$Grazing_rate), short_term_clams$Grazing_rate, short_term_clams$Grazing_Rate)

short_term_clams2<-short_term_clams%>%dplyr::select(Region, StationCode, SpeciesID, Density, Month, Year, Season, Survey, Biomass, Grazing)

#assign zero for biomass and grazing when density is zero
short_term_clams2$Biomass<-as.numeric(short_term_clams2$Biomass)
short_term_clams2$Density<-as.numeric(short_term_clams2$Density)
short_term_clams2$Grazing<-as.numeric(short_term_clams2$Grazing)
short_term_clams2$Biomass<-if_else(is.na(short_term_clams2$Biomass&short_term_clams2$Density==0), 0, short_term_clams2$Biomass)
short_term_clams2$Grazing<-if_else(is.na(short_term_clams2$Grazing&short_term_clams2$Density==0), 0, short_term_clams2$Grazing)

hist(short_term_clams2$Biomass)
hist(short_term_clams2$Grazing)

write_csv(short_term_clams2, "short_term_biomass_grazing_revised.csv")
#short_term_clams2 = read_csv("data/short_term_biomass_grazing_clams.csv")

#calculate monthly mean biomass and grazing rate
mean_biomass_grazing<-short_term_clams2%>%group_by(Region, Year, Month, SpeciesID)%>%summarize(monthly_avg_biomass=mean(Biomass, na.rm=TRUE), monthly_avg_grazing=mean(Grazing, na.rm=TRUE))
write_csv(mean_biomass_grazing, "short_term_biomass_grazing_metrics.csv")


#####################################33
#short term trends plot
yeartypes = read.csv("data/yearassignments.csv")
short_term = left_join(mean_biomass_grazing, yeartypes) %>%
  mutate(Yr_type = factor(Yr_type, levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet")))
ggplot(short_term, aes(x = as.factor(Year), y = log(monthly_avg_biomass+1), fill = ShortTerm)) + geom_boxplot() +
  facet_grid(~SpeciesID)
ggplot(short_term, aes(x = as.factor(Year), y = monthly_avg_biomass, fill = Yr_type)) + geom_boxplot() +
  facet_grid(~SpeciesID) +
  scale_fill_viridis_d(direction = -1, name = "Year Type")+
  ylab("Average biomass (g/m2)")+
  xlab("Year")+theme_bw()

ggplot(short_term, aes(x = as.factor(Year), y = monthly_avg_grazing, fill = Yr_type)) + geom_boxplot() +
  facet_grid(~SpeciesID) +
  scale_fill_viridis_d(direction = -1, name = "Year Type")+
  ylab("Grazing Rate")+
  xlab("Year")+theme_bw()
