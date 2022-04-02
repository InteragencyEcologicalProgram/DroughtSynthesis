#Emergency Drought Barrier
#SePro SAV surveys
#conduct PERMANOVA analysis on SAV community
#look for differences among years

#Notes
#use DWR WY types or a simplification of them to categorize years in analyses
#should I also try SIMPER?
#for repeated measured PEMANOVA, stations aren't in same locations across all years
#but they are fairly consistent among most recent years
#so could used repeated measures analysis just for recent years
#need to look up which years have same stations

#load packages
library(tidyverse) #data science tools
library(lubridate) #formatting date-time
library(vegan) #PERMANOVA
library(RVAideMemoire) #pairwise PERMANOVA

#read in data - veg, wq, herbicide use

#sepro sav data
sav <- read_csv("https://raw.githubusercontent.com/InteragencyEcologicalProgram/AquaticVegetationPWT/main/MasterDataSet_SAV/Data_Formatted/FranksTractManagement_2014-2021_formatted.csv")

#PERMANOVA assumes roughly balances design
#if looking at effect of year, then make sure years have similar sample sizes
sav_n <- sav %>% 
  distinct(station,date) %>% 
  group_by(date) %>% 
  summarize(n = n())
#all years have 100 samples except for two 
#2016 = 45, 2015 = 200
#should randomly subset 2015 to 100 samples
#could exclude 2016 or just let the design be a bit unbalanced

#create random subset of 100 rake samples from the 200 collected in 2015

#start by grabbing the 2015 sampling data
sav15 <- sav %>% 
  #filter data set to just 2015 samples
  filter(date=="2015-10-13") 

#choose which stations to keep using random numbers
sav15r <- sav15 %>% 
  #condense to just the list of unique stations
  distinct(station) %>% 
  #add column of randomly generated numbers
  mutate(random = sample(200, size = nrow(.), replace = F)) %>% 
  #just keep rows assigned to 1-100
  filter(random >0 & random < 101)

#subset the 2015 data based on random numbers
sav15sb <- left_join(sav15r,sav15) %>% 
  select(-random)

#remove the original full set of 2015 data from main data set
sav_no15 <- sav %>% 
  filter(date!="2015-10-13")

#add the subsetted 2015 data to main data set
sav_sub <- bind_rows(sav_no15,sav15sb)

#make sure there are now 100 instead of 200 samples for 2015
sav_n2 <- sav_sub %>% 
  distinct(station,date) %>% 
  group_by(date) %>% 
  summarize(n = n())
#looks like it worked

#look at distribution of scores by species using histograms
ggplot(sav_sub, aes(x = rake_coverage_ordinal) )+
         geom_histogram()+
         facet_wrap(.~species)

#look at rare taxa and consider removing them
#Note that these numbers will likely be affected by 2015 subsampling
sav_spp_sum <- sav_sub %>% 
  #drop the visual samples and any abundances of zero
  filter(survey_method=="rake_weighted" & rake_coverage_ordinal!=0) %>% 
  group_by(species) %>% 
  summarize(freq = n()) %>% 
  arrange(freq)
#let's drop the spp with fewer than ten detections
unique(sav_spp_sum$species)
#five rare taxa removed in code below

#format data set as matrix
sav_wide <- sav_sub %>% 
  #add year column
  mutate(year = as.factor(year(date))) %>% 
  filter(
    #remove the small number of visual only samples
    survey_method!="visual"
    #remove rake samples with no SAV; can't have these in analysis
    & sav_incidence==1
    #try dropping 2016 because only 45 instead of 100 samples
    & year != "2016"
    ) %>%
  #remove rare taxa (fewer than 10 detections ever)
  filter(species!="Heteranthera_dubia" & species!="Nitella_sp" & species!="Potamogeton_nodosus"      
         & species!="Potamogeton_pusillus"  & species!= "Potamogeton_zosteriformis") %>% 
  #filter(species=="Potamogeton_richardsonii" | species== "Ceratophyllum_demersum" | species== "Egeria_densa" | species== "Najas_guadalupensis") %>% 
  #convert long to wide
  pivot_wider(id_cols=c(station,year,date)
              , names_from = species
              , values_from = rake_coverage_ordinal) 

#create df with just the environmental predictors
sav_env<- sav_wide %>% 
  select(year) %>% 
  glimpse()

#create df with just subset of columns that contain species abundance data
sav_data<-sav_wide %>% 
  select(Egeria_densa:Myriophyllum_spicatum) %>% 
  glimpse()

#square root transform the abundance data
#is there a more appropriate transformation to use for ordinal data?
#also does the function do this automatically? check documentation
sav_data_sqrt<-sqrt(sav_data)
#glimpse(sav_data_sqrt)
#range(sav_data_sqrt) #0.000000 2.236068
#makes sense because raw scores are 0 to 5

#make a distance matrix based on bray-curtis
sav_dist <- vegdist(sav_data_sqrt, method="bray")

#PERMANOVA using adonis function----------------

#decide if bray is the best method; I know I've also used horn
permanova_sav<-adonis2(sav_data_sqrt ~ year, data=sav_env
                       #set number of permutations
                       ,permutations=999
                       #each marginal term analyzed in a model with all other variables
                       #,by = "margin"
                       ,method="bray"
                       )
permanova_sav 
#year is significant p < 0.001

#pairwise PERMANOVA 
#determine which years are different from one another in centroids
pairwise_comp <- pairwise.perm.manova(resp = sav_dist, fact = sav_env$year
                                      #test chosen arbitrarily for now
                                      #, test="Wilks"
                                      #standard number of permutations
                                      ,nperm=999
                                      #show progress bar for computations
                                      #,progress = T #didn't seem to work
                                      #use Benjamini-Hochberg adjustment for multiple comparisons
                                      ,p.method = "BH"
                                      #show table of F values
                                      #,F = T #didn't seem to work
                                      #show tables of R2 values
                                      #,R2=T #didn't seem to work
                                      )
pairwise_comp
#indicates that all years are different from all other years
#both pseudoreplication and lack of homogeneity of disperions are likley an issue 


#perform analysis of multivariate homogeneity of group dispersions
#to determine if the multivariate scatter is similar among years
#this is an assumption of PERMANOVA
#if not similar among years, this can results in misleading significant p-values

dispersion <- betadisper(sav_dist, group=sav_env$year)
permutest(dispersion) 
#p<0.001, so we failed the test (ie, dispersions are not homogeneous)
#so we don't know if centroids or dispersions caused the significant year effect
#this is true regardless of whether we subset the 2015 data or exclude the 2016 data

#plot of dispersion
plot(dispersion, hull=F, ellipse=T, label = F) ##sd ellipse

TukeyHSD(dispersion)
#six pairs of years have significantly different dispersions

#NMDS------------------

#sav.mds <- metaMDS(sav_data_sqrt,dist="horn",k=3, try = 99, trymax = 150,autotransform=F
                   #,plot = T
#                   )
#no convergence after 100 attempts

#see if using the autotransform instead of square root transform helps
#check to be sure there are no samples without any spp and no NAs
#maybe drop another rare species (Myriophyllum)
sav.mds2 <- metaMDS(sav_data,dist="horn",k=4, try = 99, trymax = 150,autotransform=T,noshare = 0.1)
#this finally worked and took 112 runs and gave a stress score of 0.10
#A good rule of thumb: stress < 0.05 provides an excellent representation in reduced dimensions,
#< 0.1 is great, < 0.2 is good/ok, and stress < 0.3 provides a poor representation.

#shepard plot
stressplot(sav.mds2)
#Large scatter around the line suggests that original dissimilarities are 
#not well preserved in the reduced number of dimensions

#goodness of fit
ngof<-goodness(sav.mds2)
plot(sav.mds2)
points(sav.mds2, display="species", cex=ngof*100) #this didn't work
#large circles are bad; not sure how large is bad, but this looks OK to me

#slightly better plot
#consider using continuous predictor like salinity in plot to show where spp land
ordiplot(sav.mds2,type="n")
ordiellipse(sav.mds2,groups=sav_env$year,draw="polygon",col="grey90",label=F)
orditorp(sav.mds2,display="species",col="red",air=0.01)
#orditorp(sav.mds2,display="sites",cex=1.25,air=0.01)

#Start graph process
data.scores <- as.data.frame(scores(sav.nmds)) #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores

###
data.scores$Year<-edsm.sm.env.data$Year
data.scores$Month<-edsm.sm.env.data$Month
str(data.scores)

species.scores <- as.data.frame(scores(sav.nmds, "species")) #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores) # create a column of species, from the rownames of species.scores
head(species.scores)


#FOR VECTORS
vf<-envfit(sav.nmds, edsm.sm.fish.data_sqrt, perm=999)
vf

#So the r2 data is used to scale the values in columns NMDS1 and NMDS2. The final plot is produced with:
spp.scrs <- as.data.frame(scores(vf, display = "vectors"))
spp.scrs <- cbind(spp.scrs, Species = rownames(spp.scrs))
spp.scrs <- spp.scrs[order(spp.scrs$Species),]
spp.scrs

#Let's remove the species that don't matter, check the vf object for info
#Only ones with p-value <0.01 for now
spp.scrs.shiny<-spp.scrs[c("AMS","BKS","MSS","NAN","SPLT","STB","TFS","TSM","TSS","WAG"),]
spp.scrs.shiny

spp.scrs.shiny$NMDS1<-spp.scrs.shiny$NMDS1*(max(data.scores$NMDS1))
spp.scrs.shiny$NMDS2<-spp.scrs.shiny$NMDS2*(max(data.scores$NMDS2))

#Set palette
cbbPalette <- c("#e6194B", "#4363d8", "#ffe119")

ggMDS_year<- ggplot() + geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,fill=Year),size=8,colour="black",shape=21,alpha=0.4) +
  theme_bw() + geom_segment(data = spp.scrs.shiny, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),arrow = arrow(length = unit(0.25, "cm")), colour = "black") +
  scale_fill_manual(values=cbbPalette) + 
  geom_text(data = spp.scrs.shiny, aes(x = NMDS1, y = NMDS2, label = Species),size=5,nudge_x = .001,nudge_y = -.005) +
  theme(axis.text.x = element_text(size=19, color="black"),axis.text.y = element_text(size=19, color="black")) +
  theme(legend.background=element_rect(colour="black"),legend.position = c(0.85,0.75),legend.title=element_text(size=20),legend.text=element_text(size=20))+
  theme(axis.text.x = element_text(size=21, color="black"),axis.text.y = element_text(size=20, color="black"),axis.title.x = element_text(size = 27),axis.title.y = element_text(size = 27)) +
  annotate("text", size=5, x = -1.1, y = 2, label = c(paste("2D Stress:",round(sav.nmds$stress,2),sep=" ")))
ggMDS_year

















