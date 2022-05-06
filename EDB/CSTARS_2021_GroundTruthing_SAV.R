#2021 Emergency Drought Barrier
#2021 CSTARS ground truthing data
#Entire Delta
#Submersed aquatic vegetation point data
#focus on Franks Tract, Big Break, and Clifton Court

#Nick Rasmussen
#nicholas.rasmussen@water.ca.gov

#To do list
#incorporate zeros representing absences into abundance calculations
#do some stats comparing Big Break and Franks Tract
#look at differences in WQ between sites, particularly salinity

# Packages--------
library(tidyverse) #suite of data science tools
library(sf) #tools for making maps
library(deltamapr) #Sam's package with shapefiles for delta waterways
library(plotrix) #standard error function
library(vegan) #PERMANOVA

# Read in the data----------------------------------------------
# Data set is on SharePoint site for the 
# Delta Smelt Resiliency Strategy Aquatic Weed Control Action
# I synced this folder to my OneDrive
#sharepoint_path_read <- normalizePath(
 # file.path(
  #  Sys.getenv("USERPROFILE"),
   # "California Department of Water Resources/DWR - DSRS Aquatic Weed Control Action - MasterDataSet_SAV/Clean&Formatted"
  #)
#) 

#read in field data
#cstars <- read_csv(file = paste0(sharepoint_path_read,"./CSTARS_2021_formatted.csv"))
#glimpse(cstars) #looks good. just need to make lat/long into geometry

#read in file from Github
cstars <- read_csv("https://raw.githubusercontent.com/InteragencyEcologicalProgram/AquaticVegetationPWT/main/MasterDataSet_SAV/Data_Formatted/CSTARS_2021_formatted.csv") 
glimpse(cstars)

#read in df with native/non-native status
origin <- read_csv("https://raw.githubusercontent.com/InteragencyEcologicalProgram/AquaticVegetationPWT/main/MasterDataSet_SAV/Data_Formatted/FranksTractManagement_SpeciesOrigin.csv")

#add a species missing from the origin df
org <- origin %>% 
  add_row(species="Cabomba_caroliniana",native="n")

#read in shape files for Franks Tract, Big Break, and Clifton Court
#files from the CSTARS lab
sf_franks <- read_sf("EDB/Spatial_data_weeds/FranksTractarea_wgs84.shp")
sf_bbreak <- read_sf("EDB/Spatial_data_weeds/BigBreak_wgs84.shp")
sf_ccourt <- read_sf("EDB/Spatial_data_weeds/Cliftoncourtarea_wgs84.shp")

#format data set-----------

#quick look at sample depth 
hist(cstars$depth_to_sav_m)

#website with EPSG codes for CRS
#https://spatialreference.org/

cstars_format <- cstars %>% 
  #specify the crs which is wgs84
  st_as_sf(coords = c(x='longitude_wgs84',y='latitude_wgs84'), 
           crs = 4326
           ,remove=F #retains original columns
           ) %>%   #EPSG code for WGS84
  glimpse()

#First draft of sampling maps-------------

#Note: NAD83 and WGS84 are highly similar and perhaps indistinguishable
#this explains why transformations between them appear to do nothing
#https://www.esri.com/arcgis-blog/products/arcgis-desktop/mapping/wgs84-vs-nad83/

#look at WW_Delta base map CRS
st_crs(WW_Delta)
#CRS = NAD83, which is different than our sample data points; EPSG: 4269
WW_Delta_4326 <- st_transform(WW_Delta, crs = 4326)


#create map showing all Delta SAV data points
(sav_map_all <- ggplot()+
  #plot waterways base layer
  geom_sf(data= WW_Delta_4326, fill= "skyblue3", color= "black") +
  #plot SAV sampling points
  geom_sf(data= cstars_format, fill= "red", color= "black", shape= 22, size= 1.5)+
    coord_sf( 
      xlim =c(-121.870, -121.251),
      ylim = c(38.570, 37.801)
    )+
    theme_bw()

)

#create map showing Franks Tract SAV data points
(sav_map_ft <- ggplot()+
    #plot waterways base layer
    geom_sf(data= WW_Delta_4326, fill= "skyblue3", color= "black") +
    #plot SAV sampling points
    geom_sf(data= cstars_format, fill= "red", color= "black", shape= 22, size= 3.5)+
    #set bounding box for site
    #Box picks up a few unneeded sampling over in Taylor Slough 
    coord_sf( 
      xlim =c(-121.677, -121.576),
      ylim = c(38.07, 38.02)
    )+
    theme_bw()+
    ggtitle("Franks Tract")
)

#create map showing Big Break SAV data points
(sav_map_bb <- ggplot()+
    #plot waterways base layer
    geom_sf(data= WW_Delta_4326, fill= "skyblue3", color= "black") +
    #plot SAV sampling points
    geom_sf(data= cstars_format, fill= "red", color= "black", shape= 22, size= 3.5)+
    #set bounding box for site
    #No stray samples from outside sites captured in this box
    coord_sf( 
      xlim =c(-121.740, -121.685),
      ylim = c(38.031, 38.005)
    )+
    theme_bw()+
    ggtitle("Big Break")
)

#create map showing Clifton Court SAV data points
#no sampling in Clifton Court
#not too surprising because access is restricted
(sav_map_cc <- ggplot()+
    #plot waterways base layer
    geom_sf(data= WW_Delta_4326, fill= "skyblue3", color= "black") +
    #plot SAV sampling points
    geom_sf(data= cstars_format, fill= "red", color= "black", shape= 22, size= 3.5)+
    coord_sf( 
      xlim =c(-121.605, -121.552),
      ylim = c(37.867, 37.818)
    )+
    theme_bw()+
    ggtitle("Clifton Court")
)

#create subsets of data sets by site using site polygons---------

#Look at CRS for CSTARS shape file forS Franks Tract, Big Break, and Clifton Court
st_crs(sf_franks) 
sf_franks_4326 <- st_transform(sf_franks, crs = 4326)

st_crs(sf_bbreak) 
sf_bbreak_4326 <- st_transform(sf_bbreak, crs = 4326)

st_crs(sf_ccourt) 
sf_ccourt_4326 <- st_transform(sf_ccourt, crs = 4326)

#plot the shape files
(map_ft <- ggplot()+
    geom_sf(data= sf_franks_4326, fill= "skyblue3", color= "black") 
)

(map_bb <- ggplot()+
    geom_sf(data= sf_bbreak_4326, fill= "skyblue3", color= "black") 
)

(map_cc <- ggplot()+
    geom_sf(data= sf_ccourt_4326, fill= "skyblue3", color= "black") 
)

# Create a bounding box based on the Franks Tract shapefile 
#will be used to crop base map in plots
bbox_fr_4326 <- st_bbox(sf_franks_4326)

# Create a bounding box based on the Big Break shapefile 
#will be used to crop base map in plots
bbox_bb_4326 <- st_bbox(sf_bbreak_4326)

#Filter CSTARS data set to just those within the Franks Tract polygon
weeds_franks <- cstars_format %>% 
  add_column(site="Franks Tract") %>% 
  st_filter(sf_franks_4326) 
#n=106 (not samples which are fewer)

#Filter CSTARS data set to just those within the Big Break polygon
weeds_bbreak <- cstars_format %>% 
  add_column(site="Big Break") %>% 
  st_filter(sf_bbreak_4326) 
#n=61 rows (not samples which are fewer)

#Filter CSTARS data set to just those within the Clifton Court polygon
weeds_ccourt <- cstars_format %>% 
  add_column(site="Clifton Court") %>% 
  st_filter(sf_ccourt_4326) 
#n=0; no sampling in clifton court

#write a file with Big Break and Franks Tract data
weeds_fb <- bind_rows(weeds_franks,weeds_bbreak)
#write_csv(weeds_fb,"EDB/CSTARS_2021_FT&BB_Formatted.csv")

#Second draft of sampling maps using site polygons----------

#create map showing Franks Tract SAV data points
#this shape file excludes two field points in False River I'd like to include 
(sav_map_ft_only <- ggplot()+
    #plot waterways base layer
    geom_sf(data= WW_Delta_4326, fill= "skyblue3", color= "black") +
    #plot SAV sampling points
    geom_sf(data=weeds_franks, fill= "red", color= "black", shape= 22, size= 3.5)+
    #set bounding box for site
    coord_sf(
      xlim = c(bbox_fr_4326$xmin, bbox_fr_4326$xmax),
      ylim = c(bbox_fr_4326$ymin, bbox_fr_4326$ymax)
    ) + 
    theme_bw()+
    ggtitle("Franks Tract")
)    
#ggsave(file = paste0(sharepoint_path_read,"./FranksTract_CSTARS_SampleSites_2021.png"),type ="cairo-png",width=6, height=4.5,units="in",dpi=300)


#create map showing Big Break SAV data points
(sav_map_bb_only <- ggplot()+
    #plot waterways base layer
    geom_sf(data= WW_Delta_4326, fill= "skyblue3", color= "black") +
    #plot SAV sampling points
    geom_sf(data=weeds_bbreak, fill= "red", color= "black", shape= 22, size= 3.5)+
    #set bounding box for site
    coord_sf(
      xlim = c(bbox_bb_4326$xmin, bbox_bb_4326$xmax),
      ylim = c(bbox_bb_4326$ymin, bbox_bb_4326$ymax)
    ) + 
    theme_bw()+
    ggtitle("Big Break")
)    
#ggsave(file = paste0(sharepoint_path_read,"./BigBreak_CSTARS_SampleSites_2021.png"),type ="cairo-png",width=6, height=4.2,units="in",dpi=300)

          
#look at Franks Tract samples-------

#date range
unique(weeds_franks$date)
#"2021-08-17" "2021-07-20"

#number of samples
ft_count<-weeds_franks %>% 
  st_set_geometry(NULL) %>%  #removes geometry
  distinct(latitude_wgs84,longitude_wgs84,date) %>% 
  summarize(count = n())
#47 samples

#how many open water samples?
ft_wat <- weeds_franks %>% 
  filter(rake_teeth_corr==0 & is.na(species)) %>% 
  st_set_geometry(NULL) %>%  #removes geometry
  distinct(latitude_wgs84,longitude_wgs84,date,time)
#n=5 open water samples

#just look at samples where SAV was present and species have non-zero rake_prop
franks_filter <- weeds_franks %>% 
  filter(rake_teeth_corr>0 & !is.na(species) & (!is.na(rake_prop) & rake_prop!=0) )

#which species
unique(weeds_franks$species)
#9 spp + algae + NA + unidentified

#look at Big Break samples-------

#date range
unique(weeds_bbreak$date)
#"2021-07-22"

#number of samples
bb_count<-weeds_bbreak %>% 
  st_set_geometry(NULL) %>%  #removes geometry
  distinct(latitude_wgs84,longitude_wgs84) %>% 
  summarize(count = n())
#30 samples

#how many open water samples?
bb_wat <- weeds_bbreak %>% 
  filter(rake_teeth_corr==0 & is.na(species)) %>% 
  st_set_geometry(NULL) %>%  #removes geometry
  distinct(latitude_wgs84,longitude_wgs84)
#n=2 open water samples

#just look at samples where SAV was present and species have non-zero rake_prop
bb_filter <- weeds_bbreak %>% 
  filter(rake_teeth_corr>0 & !is.na(species) & (!is.na(rake_prop) & rake_prop!=0) )

#which species
unique(weeds_bbreak$species)
#10 spp + NA + unidentified

#compare the two sites--------------

#combine the two site df's into one df
frbb <- bind_rows(weeds_bbreak,weeds_franks)
#glimpse(frbb)

#add the spp origin info 
fbs1 <- left_join(frbb,org) %>% 
  #add id column 
  #will be used to filter out some rows
  add_column(id = seq(1:167)) 

#show the two duplicated S. pectinata rows
test <- fbs1 %>% 
  filter(site== "Big Break" & species == "Stuckenia_pectinata")
#rows 16 and 17

#drop some duplicate rows
fbs <- fbs1 %>% 
  filter(id!=16 & id!=17)
  
#create version of data set with just Franks Tract to send to SePro
fbs_frank <- fbs %>% 
  filter(site=="Franks Tract")
#write the file
#write_csv(fbs_frank, file = paste0(sharepoint_path_read,"./CSTARS_2021_formatted_FranksTractOnly.csv"))


#look at set of species
taxnat<-fbs %>% 
  #removes geometry
  st_set_geometry(NULL) %>% 
  distinct(across(c('species','native')))
#looks like all species are matched to an origin status as expected

#format df to create bar plot showing % rake cover by spp and site
fb_spp_cov <- fbs %>% 
  #removes geometry
  st_set_geometry(NULL) %>% 
  #add column that calculates absolute rake coverage by spp within sample
  mutate(rake_index = (rake_teeth_corr/100)*(rake_prop/100)) %>% 
  #calculate summary stats by site and species
  group_by(site, species,native) %>% 
  summarize(
    rake_mean = mean(rake_index)
    ,rake_se = std.error(rake_index)
    ,rake_n = n()
    , .groups = 'drop'
    ) %>% 
  #drop unneeded categories
  filter(species!="Algae" & !is.na(species) & species!= "Unidentified"& rake_n>1) %>% 
  mutate(site=as.factor(site)
         ,species=as.factor(species)
         ) %>% 
  glimpse()

#which species 
unique(fb_spp_cov$species)

#plot species mean abundances by site (faceted by site)
(plot_spp_score_avg <-ggplot(fb_spp_cov, aes(x=species, y= rake_mean, fill=native))+
    geom_bar(stat = "identity") + 
    geom_errorbar(aes(ymin=rake_mean-rake_se, ymax=rake_mean+rake_se), width = 0.2) +
    ylab("Mean percent of rake head covered") + xlab("Site") +
    facet_wrap(~site,nrow = 2)
)
#some interesting differences between sites
#BB has S.filiformis and FT doesn't
#FT has lots of Najas and BB has none
#FT has little Myriophyllum while BB has lots
#BB has lots of P. richardsonii while FT has much less

#plot species mean abundances by site (faceted by species)
(plot_spp_score_avg2 <-ggplot(fb_spp_cov, aes(x=site, y= rake_mean, fill=site))+
    geom_bar(stat = "identity") + 
    geom_errorbar(aes(ymin=rake_mean-rake_se, ymax=rake_mean+rake_se), width = 0.2) +
    ylab("Mean percent of rake head covered") + xlab("Site") +
    facet_wrap(~species,nrow = 2)+
    theme(legend.position = "none")
)
#ggsave(file = paste0(sharepoint_path_read,"./FranksTract_BigBreak_SppAbundance_2021.png"),type ="cairo-png",width=8, height=7,units="in",dpi=300)

#format df to create bar plot showing mean % rake cover of non-native spp by site
fb_non <- fbs %>% 
  #removes geometry
  st_set_geometry(NULL) %>% 
  #drop unneeded categories
  filter(species!="Algae" & !is.na(species) & species!= "Unidentified") %>% 
  #add column that calculates absolute rake coverage by spp within sample
  mutate(rake_index = (rake_teeth_corr/100)*(rake_prop/100)) %>% 
  #sum coverage within samples by origin group (native vs non-native)
  group_by(site,latitude_wgs84,longitude_wgs84,native) %>% 
  summarize(rake_org_sum=sum(rake_index)) %>% 
  #calculate summary stats by site and origin
  group_by(site, native) %>% 
  summarize(
    org_mean = mean(rake_org_sum)
    ,org_se = std.error(rake_org_sum)
    , .groups = 'drop'
  ) %>% 
  glimpse()

#plot origin type mean abundances by site
(plot_org_avg <-ggplot(fb_non, aes(x=site, y= org_mean, fill=native))+
    geom_bar(position=position_dodge(0.95),stat = "identity") + 
    geom_errorbar(aes(ymin=org_mean-org_se, ymax=org_mean+org_se),position=position_dodge(0.95), width = 0.2) +
    ylab("Mean percent of rake head covered") + xlab("Site") 
)

#format df to make plots of total rake coverage
fb_cov <- fbs %>% 
  #removes geometry
  st_set_geometry(NULL) %>%  
  #reduce data set to just total rake coverage, ignoring spp comp level 
  distinct(site,latitude_wgs84,longitude_wgs84,rake_teeth_corr)

#plot histogram of sample volume by site
(sav_hist <-ggplot(fb_cov
                   , aes(rake_teeth_corr))+
    geom_histogram() + 
    facet_wrap(site~.)+
    ylab("Number of samples") + xlab("Percent of rake head covered")  
  )
#nearly all samples are 100% coverage

#calculate mean rake coverage by site
fb_avg <- fb_cov %>% 
  group_by(site) %>% 
  summarize(
    rake_mean = mean(rake_teeth_corr)
    ,rake_se = std.error(rake_teeth_corr)
    , .groups = 'drop')

#plot mean and standard error for sample volume by site
(plot_site_avg <-ggplot(fb_avg, aes(x=site, y=rake_mean))+ 
    geom_bar(stat = "identity") + 
    geom_errorbar(aes(ymin=rake_mean-rake_se, ymax=rake_mean+rake_se), width = 0.2) +
    ylab("Mean percent of rake head covered") + xlab("Site")
    +
  theme_bw()
)
#ggsave(file = paste0(sharepoint_path_read,"./FranksTract_BigBreak_TotalVegVolume_2021.png"),type ="cairo-png",width=2.5, height=3,units="in",dpi=300)


#redo sampling maps with points indicating size of the sav sample---------

#format df to make maps of total rake coverage
#just need to add geometry back into df
fb_cov_g <- fb_cov %>% 
  #specify the crs which is wgs84
  st_as_sf(coords = c(x='longitude_wgs84',y='latitude_wgs84'), 
           crs = 4326
           ,remove=F #retains original columns
  ) %>%   #EPSG code for WGS84
  glimpse()

#create map showing Franks Tract SAV data points
(sav_map_ft_only2 <- ggplot()+
    #plot waterways base layer
    geom_sf(data= WW_Delta_4326, fill= "skyblue3", color= "black") +
    #plot SAV sampling points
    geom_sf(data=fb_cov_g, fill= "dark green", color= "black", shape= 21
            #use volume of sample for size of points on map
            ,aes(size=rake_teeth_corr)
    )+
    #set bounding box for site
    coord_sf(
      xlim = c(bbox_fr_4326$xmin, bbox_fr_4326$xmax),
      ylim = c(bbox_fr_4326$ymin, bbox_fr_4326$ymax)
    ) + 
    theme_bw()+
    ggtitle("Franks Tract")
)        

#create map showing Big Break SAV data points

(sav_map_bb_only2 <- ggplot()+
    #plot waterways base layer
    geom_sf(data= WW_Delta_4326, fill= "skyblue3", color= "black") +
    #plot SAV sampling points
    geom_sf(data=fb_cov_g, fill= "dark green", color= "black", shape= 21
            #use volume of sample for size of points on map
            ,aes(size=rake_teeth_corr)
    )+
    #set bounding box for site
    coord_sf(
      xlim = c(bbox_bb_4326$xmin, bbox_bb_4326$xmax),
      ylim = c(bbox_bb_4326$ymin, bbox_bb_4326$ymax)
    ) + 
    theme_bw()+
    ggtitle("Big Break")
)        

#try PERMANOVA------------------


#format data set as matrix
#might need to remove rare spp
sav_prep <- fbs %>% 
  filter(
    #remove rake samples with no SAV; can't have these in analysis
    rake_teeth_corr > 0 & 
    #remove unIDed taxa
    species!="Unidentified") %>% 
  #removes geometry
  st_set_geometry(NULL) %>% 
  #reduce to just needed columns
  select(site,latitude_wgs84,longitude_wgs84,species,rake_prop) %>% 
  pivot_wider(id_cols=c(site,latitude_wgs84,longitude_wgs84)
              , names_from = species
              , values_from = rake_prop) %>% 
  #replace na with zero 
  replace(is.na(.), 0) %>% 
  glimpse()

#look at abundances of species and should probably cut some of the less common ones




