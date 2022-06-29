#Jellyfish analyses
#Rosemary Hartman
#6/28/2022

library(tidyverse)
library(readxl)
library(lubridate)
library(glmmTMB)
library(DHARMa)
library(multcomp)
library(lme4)
library(emmeans)
library(mgcv)
library(effects)

library(spacetools)
library(sf)
library(stars)
library(lmerTest)
library(MuMIn)
library(broom)


#load the R data file with all the integrated data in it

load("jellyfish.RData")

#some exploritory plots
ggplot(filter(AlljelliesMean, Year == 2017), aes(x = Month, y = meanJellies)) +
  geom_col()+ facet_grid(Region~Year)

ggplot(AlljelliesMean, aes(x = Year, y = meanJellies, fill = ShortTerm)) +
  geom_col()+ facet_wrap(~Region)



ggplot(AlljelliesMean, aes(x = Year, y = meanJellies, fill = Yr_type)) +
  geom_col()+ facet_wrap(~Region)

#I'm going to alter Dave's drougth color palette just slightly so the wet years aren't so dark
pal_yrtype <- c( "Critical" = "#FDE333", "Dry" = "#53CC67", "Below Normal" = "#009B95","Above Normal" = "#00588B", "Wet" = "#481F70FF") 
ggplot(AlljelliesMean, aes(x = Year, y = meanJellies)) +
  geom_col(aes(fill = Yr_type), position = "dodge")+
  scale_fill_manual(values = pal_yrtype)+
  # geom_errorbar(aes(ymin = meanJellies-sdJellies, ymax = meanJellies + sdJellies))+
  ylab("Mean monthly Maeotias CPUE") + theme_bw()+
  facet_wrap(~Region)

ggplot(AlljelliesTot, aes(x = Temp_surf, y = TotJellies, 
                          color = Yr_type)) +
  geom_point()+ facet_wrap(~Region)

ggplot(AlljelliesTot, aes(x = Year, y = TotJellies, 
                          color = Yr_type)) +
  geom_point()+ facet_wrap(~Region)


ggplot(filter(AlljelliesTot, Region != "North", Region != "SouthCentral"), aes(x = Month, y = TotJellies)) +
  geom_point()+ facet_grid(Year~Region)+ 
  scale_y_log10()


ggplot(filter(AlljelliesTot,Source == "STN"), aes(x = Month, y = TotJellies)) +
  geom_point()+ facet_wrap(~Region)+ 
  scale_y_log10()


ggplot(AlljelliesTot, aes(x = Drought, y = TotJellies)) +
  geom_point()+ facet_grid(Season~Region)

Alltotsub = filter(AlljelliesTot, Month %in% c(6,7,8,9,10), Region %in% c( "Suisun Bay","Confluence","Suisun Marsh")) %>%
  mutate(Yearf = as.factor(Year))

ggplot(Alltotsub, aes(x = Drought, y = log(TotJellies+1))) +
  geom_boxplot()+ facet_wrap(~Region)


Allmeansub = filter(AlljelliesMean, Month %in% c(6,7,8,9,10)) #, Region %in% c( "Suisun Bay","Confluence","Suisun Marsh"))

  
ggplot(Allmeansub, aes(x = Drought, y = log(meanJellies+1), fill = Drought)) +
  #  scale_fill_manual(guide = NULL, values = pal_drought)+
  geom_boxplot()+ facet_wrap(~Region) + theme_bw()+
  scale_x_discrete(labels = c("Drought", "Neutral", "Wet"))+
  ylab("Mean monthly jellyfish CPUE (log-transformed)")

##############
#Box plot of year type by region (this is the one we like)
ggplot(Allmeansub, aes(x = Yr_type, y= log(meanJellies+1), fill = Yr_type)) +
  scale_fill_manual(guide = NULL, values = pal_yrtype)+
  geom_boxplot( alpha = 0.8)+ facet_wrap(~Region) + theme_bw()+
  scale_x_discrete(labels = c("Critical", "Dry", "Below\nNormal", "Wet"))+
  ylab("Mean summer jellyfish CPUE (log-transformed)")
#ggsave("plots/Jelliesboxplot.tiff", device = "tiff", width = 6, height = 5)

###############

ggplot(Allmeansub, aes(x =Index, y = log(meanJellies+1))) +
  geom_smooth(method = "lm")+
  geom_point()+ facet_wrap(~Region) + theme_bw()+
  # scale_x_discrete(labels = c("Drought", "Neutral", "Wet"))+
  ylab("Mean monthly jellyfish CPUE (log-transformed)")


#limit it to just the summer, just regions where jellies are normally caught
Allmeansub2 = filter(AlljelliesMean, Month %in% c(6,7,8,9,10), Region %in% c( "Suisun Bay","Confluence","Suisun Marsh"))

###############################################################################
#What kind of analysis can we do on this?

hist(AlljelliesMean$meanJellies)
hist(Allmeansub2$meanJellies)
hist(log(Allmeansub2$meanJellies+1))
#slightly zero-inflated

#hmmm... or I could do it right and use count and the offset instead of the mean, then it's sure to be zero-inflated
AlljelliesMean2 = mutate(AlljelliesMean, rJellies = round(meanJellies), Yearf = as.factor(Year)) %>%
  filter( Month %in% c(6,7,8,9,10), Region %in% c( "Suisun Bay","Confluence","Suisun Marsh"))
#


#############Try it with monthly means
jelz3b = glmmTMB(rJellies~ Yr_type*Region + (1|Month) + (1|Yearf),  family = "nbinom2", 
                 data = AlljelliesMean2)
summary(jelz3b)
ecf1 = emmeans(jelz3b, specs=pairwise ~Yr_type*Region,adjust="sidak")
plot(ecf1)
z3resb = simulateResiduals(jelz3b)
plot(z3resb)
#Not too bad, all things considered
#small issue with KS test, but close to being OK.
testZeroInflation(jelz3b)
#we're good there

testOutliers(jelz3b, type = 'bootstrap')
#good there
testDispersion(jelz3b)
#good there too 

#######################################3
#THIS IS THE MODEL FOR THE PAPER
#try one more thing
jelz3c = lmer(log(meanJellies+1)~ Yr_type*Region + (1|Month) + (1|Yearf), 
              data = AlljelliesMean2)

summary(jelz3c)
res2 = simulateResiduals(jelz3c)
plot(res2)
#That looks perfect why be overly complicated?
write.csv(summary(jelz3c)$coefficients, "JellyfishCPUEmodel.csv")

#now let's do some post hoc comparisons
library(effects)
library(emmeans)
pcs = emmeans(jelz3c, pairwise ~ "Yr_type*Region")
plot(pcs)
pcs

plot(allEffects(jelz3c))

plot(emmeans(jelz3c,pairwise ~ Yr_type|Region), comparison = T)
plot(emmeans(jelz3c,pairwise ~ Region|Yr_type))
#Huh. This seems to say there is no difference between water year types within a region

#test models one region at a time, just to make sure
susuin = filter(AlljelliesMean2, Region == "Suisun Bay")
suslm = lmer(log(meanJellies+1)~ Yr_type + (1|Month) + (1|Yearf), 
             data = susuin)
summary(suslm)

susMarsh = filter(AlljelliesMean2, Region == "Suisun Marsh")
susMlm = lmer(log(meanJellies+1)~ Yr_type + (1|Month) + (1|Yearf), 
              data = susMarsh)
summary(susMlm)

conf = filter(AlljelliesMean2, Region == "Confluence")
conflm = lmer(log(meanJellies+1)~ Yr_type + (1|Month) + (1|Yearf), 
              data = conf)
foo = summary(conflm)
emmeans(conflm, pairwise ~ Yr_type, adjust = "tukey")
#when modeled seperately, there is maybe significant effect in the conflusence, but not other regions

##############################################Salinity plots
ggplot(AlljelliesMean2, aes(x = Sal_mean, y = rJellies)) + geom_point(aes(color = Yr_type))+
  facet_wrap(~Region)


ggplot(filter(AlljelliesTot, Month %in% c(6,7,8,9,10), Year > 1999), aes(x = Sal_surf, y = log(TotJellies+1))) + 
  geom_point(aes(color = Yr_type))+
  facet_wrap(~Region, scales = "free") + 
  geom_smooth()+
  ylab("Ln Jellyfish per m3")+
  xlab("Salinity (PSU)")

ggplot(filter(Alljellies2, Month %in% c(6,7,8,9,10), Year > 1999), aes(x = Sal_surf, y = log(CPUE+1))) + 
  geom_point(aes(color = Region))

library(mgcv)
#library(brms)
sal = gamm(TotJellies ~ s(Sal_surf) + Yearf, data = Alltotsub, family = nb)
summary(sal)
plot(sal$gam)

#Bleh. I liked the idea from Arthur of just calculating the quantiles and seing what slainities had 50% of th ecatch

#I think I first need to weight the salinity by number caught, same as the distribution

ggplot(filter(Alltotsub, TotJellies != 0), aes(x = Sal_surf)) + geom_histogram()

summary(filter(Alltotsub, TotJellies != 0)$Sal_surf)

library(Hmisc)

msal = wtd.mean(Alltotsub$Sal_surf, Alltotsub$TotJellies)
sdsal = sqrt(wtd.var(Alltotsub$Sal_surf, Alltotsub$TotJellies))
minsal = msal-sdsal
maxsal = msal+sdsal

#OK, bin by salinity and see which bins have th ehighest percentage of catch
weighted = mutate(Alltotsub, weightedSal = Sal_surf*TotJellies) %>%
  filter(!is.na(Sal_surf)) %>%
  mutate(Salbin = round(Sal_surf),
         Salbin2 = factor(Salbin, levels = sort(unique(Salbin))))

ggplot(weighted, aes(x= Salbin, y = TotJellies/15)) + geom_col(aes(fill = Salbin))+
  scale_color_viridis_b(guide = NULL)+
  xlab("Salinity (PSU)")+ ylab("Average annual Maeotias CPUE")+
  theme_bw()

Saliniteis = group_by(weighted, Salbin2) %>%
  dplyr::summarize(percent = sum(TotJellies)/sum(weighted$TotJellies))

#most catch is between 4.5 and 7.5. Rerun model to see if water year type is still significant.
sweetspot = filter(AlljelliesMean2, Sal_mean >minsal, Sal_mean <maxsal)

ggplot(sweetspot, aes(x = Yr_type, y = log(meanJellies+1))) + geom_boxplot()


#plot of catch between 4.5 and 7.5
ggplot(sweetspot, aes(x = Yr_type, y = log(meanJellies+1), fill = Yr_type)) + geom_boxplot(alpha = 0.8)  +
  # facet_wrap(~Region)+
  scale_fill_manual(values = pal_yrtype2) +
  ylab("log-trasnformed Maeotias CPUE") + xlab("Water Year Type")+
  theme_bw()
ggsave("Maeotias4_7ppt.tiff", device = "tiff", height = 6, width = 6)


jelz3ss = lmer(log(meanJellies+1)~ Yr_type*Region + (1|Month) + (1|Yearf), 
               data = sweetspot)

summary(jelz3ss)
res2 = simulateResiduals(jelz3ss)
plot(res2)
#No! It's really all about salinity
#but wet years do seem to be higher. Maybe if I remove region?

jelz3ss2 = lmer(log(meanJellies+1)~ Yr_type + (1|Month) + (1|Yearf), 
                data = sweetspot)

summary(jelz3ss2)
#Nope. Odd.

emmeans(jelz3ss2, pairwise ~ Yr_type)
####################################################################################
#center of distribution from the Golden Gate

stations = read_csv("data/IEPstationsw_Regions.csv")

#I need suisun station gps
Suisun = read_csv("data/StationsLookUp.csv") %>%
  rename(StationID = StationCode, Latitude = y_WGS84, Longitude = x_WGS84) %>%
  mutate(Source = "Suisun", Region = "Suisun Marsh") %>%
  dplyr::select(Source, Region, StationID, Latitude, Longitude)

stations = bind_rows(Suisun, stations) %>%
  dplyr::select(-SubRegion)

AlljelliesTot2 = left_join(AlljelliesTot, stations) %>%
  filter(!is.na(Latitude)) 

Alljelliessta = group_by(AlljelliesTot2, StationID, Latitude, Longitude, Source) %>%
  summarize(n = n()) %>%
  mutate(Station = paste(Source, StationID))

AlljelliesTotsf = left_join(AlljelliesTot, stations) %>%
  filter(!is.na(Latitude)) %>%
  st_as_sf(coords = c( "Longitude","Latitude"), crs = 4326)

distance<-GGdist(Water_map = spacetools::Delta, Points = Alljelliessta, Latitude_column = Latitude,
                 Longitude_column = Longitude, PointID_column = Station)

#Do I want to calculate the average distance for all sites where Meaotias was caught? 
#OR do I weight the stations by the number caught?
#I want to weight it.
distancex = left_join(distance, Alljelliessta) %>%
  dplyr::select(-Station)

Alljel = left_join(AlljelliesTot2, distancex) 
Alljelsum = mutate(Alljel, weightedD = Distance*TotJellies) %>%
  group_by(Month, Year, Yr_type, Index) %>%
  summarize(Meandist = sum(weightedD)/(sum(TotJellies, na.rm = T)), jellies = sum(TotJellies, na.rm = t)) %>%
  droplevels()

#get dayflow outflow
#(Grab Dayflow from the flowplots.R file)
load("Dayflow.RData")
DFmonth = mutate(DF, Month = month(Date), Year = year(Date)) %>%
  group_by(Month, Year) %>%
  summarize(Outlfow = mean(OUT))

#Take out all samples with no jellies, or less than 20 total jellies
Alljelsum = left_join(Alljelsum, DFmonth) %>%
  filter(!is.nan(Meandist), jellies >20)

save(Alljel, Alljelsum, distancex, file = "data/JellieswDistance.Rdata")
load("data/JellieswDistance.Rdata")
Mypal = c(brewer.pal(12, "Set3"), brewer.pal(8, "Dark2"))

#Facet by month
ggplot(Alljelsum, aes(x = Meandist, y = Outlfow)) + 
  geom_point(aes(color = as.factor(Year))) + geom_smooth(method = "lm")+
  facet_wrap(~Month)

################
#linear model 

#convert cubic feet to cubic meters
Alljelsum = mutate(Alljelsum, Yearr = as.factor(Year),
                   OutflowM = Outlfow*0.0283168,
                   DistK = Meandist/1000)
jl1 = lmer(DistK~ OutflowM + (1|Yearr), data = Alljelsum)
summary(jl1)
plot(jl1)
res = simulateResiduals(jl1)
plot(res)
jl1s = summary(jl1)
R2 = r.squaredGLMM(jl1)

write.csv(jl1s$coefficients, "outputs/jellyfishdistance.csv")

#format an equation to print on the graph
EQ = paste("y = ", format(unname(coef(jl1s)[1]), digits = 2), " + ",
           b = format(unname(coef(jl1s)[2]), digits = 2), "x,", " R2 = ",
           r2 = format(R2[1], digits = 3), sep = "")
EQ

pal_yrtype2 <- c( "Critical" = "#FDE333", "Dry" = "#53CC67", "Below Normal" = "#009B95","Wet" = "#481F70FF") 

#Plot of outflow versus center of distribution for paper
ggplot(droplevels(Alljelsum), aes(x = DistK, y = OutflowM)) + 
  geom_point(aes(color = Yr_type)) + geom_smooth(method = "lm") + 
  scale_color_manual(values = pal_yrtype2, name = "Water Year\nType")+
  xlab("Center of Maeotias distribution \n(Km from Golden Gate)")+
  ylab("Monthly Mean Delta Outflow (m3/sec)") +
  annotate("text", x = 70, y = 320, label = EQ)+
  theme_bw()
ggsave("plots/Jelliesdistance.tiff", device = "tiff", height = 5, width = 6)

######################################################################
#I think I need to weight distance differently
library(mgcv)
ggplot(Alljel, aes(x = Distance, y = log(TotJellies+1))) + geom_point() + geom_smooth()

#What if I did a GAM for each month and calculated the distance at the maximum catch?
test = filter(Alljel, Year == 2019, Month == 8) %>%
  mutate(rJel = round(TotJellies))
GAMtest = gamm(rJel ~ s(Distance, k =4), random = list(StationID=~1), data = test, family = nb)
GAMtest
vis.gam(GAMtest)

#Huh, not sure how to calculate distance to maximum with the stations as a random. 

GAMtest2 = gam(rJel ~ s(Distance, k =6),  data = test, family = nb)
summary(GAMtest2)
plot(GAMtest2)
gam.check(GAMtest2)

#Now use predict to find the maximum
Newdat = data.frame(Distance = 50000:140000)
preds = as.data.frame(predict(GAMtest2, newdata = Newdat, type = "response")) %>%
  rename(Predictions = `predict(GAMtest2, newdata = Newdat, type = "response")`)
Newdat = bind_cols(Newdat, preds)
max(Newdat$Predictions)
Newdat$Distance[which(Newdat$Predictions== max(Newdat$Predictions))]

#OK, now try applying this to all months I'll make a function

CenterMod = function(dat) {
datx = filter(dat, rJel != 0)
if(nrow(datx)>0) {
  GAM = gam(rJel ~ s(Distance, k =6),  data = dat, family = nb)
  
  Newdat = data.frame(Distance = min(datx$Distance):max(datx$Distance))
  preds = as.data.frame(predict(GAM, newdata = Newdat, type = "response")) %>%
    rename(Predictions = `predict(GAM, newdata = Newdat, type = "response")`)
  Newdat = bind_cols(Newdat, preds)
  CenterX = data.frame(Max = max(Newdat$Predictions),Distance =  Newdat$Distance[which(Newdat$Predictions== max(Newdat$Predictions))])
  Center =CenterX } else
    Center = data.frame(Max = 0, Distance = NA)
  
  return(Center)
}

#make sure the function works
CenterMod(test)

#filter to just the summer
Alljelx = mutate(Alljel, rJel = round(TotJellies)) %>%
  filter(Month %in% c(5,6,7,8,9,10,11))

#apply center of distribution function to each month
Centers = group_by(Alljelx, Year, Month) %>%
  do(CenterMod(.)) 

#months with very low catch got weird
centers = left_join(Centers, DFmonth) %>%
 # filter(Distance < 140000, Max > 20) %>%
  left_join(Yeartypes)
ggplot(centers, aes(x = Distance, y = Outlfow)) + 
  geom_point(aes(color = Yr_type)) + geom_smooth(method = "lm") +
  coord_cartesian(ylim = c(2500, 16000))

#Huh. I'm not sure whether this is better or not. It shows the same general trends though. 
######################################################################

#OK, now try one more method
# install.packages("remotes")
#remotes::install_github("pbs-assess/sdmTMB", dependencies = TRUE)
#library(sdmTMB)
#library(INLA)

#We start by creating a mesh object that contains matrices to apply the SPDE approach.
#mesh = make_mesh(Alljelx, xy_cols = c("Longitude", "Latitude"), cutoff = .001)
#BLEHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH
####################################################
#how unbalanced are my samples?

ggplot(Alljelx, aes(x = Distance)) +geom_histogram()+
  facet_grid(Month~Year)

#Actually pretty well balanced bewteen years, just not between months

ggplot(Alljelx, aes(x = Distance, y = log(rJel))) +geom_point()+
  facet_grid(Month~Year)

#Try a single gam with month and yearas blocking terms like Sam suggested
Alljelx = mutate(Alljelx, Yearf = as.factor(Year), Yearm = as.factor(paste(Yearf, Month)))

GAMtest3 = gam(rJel ~ s(Distance, k =6, by = Yearm), data = Alljelx, family = nb)
GAMtest3

#Bleh. THis is taking forever and probably not working. I could do it by year, but not month and year. 
#I'm tempted to stick with the individual GAMs I had before. However, given the general
#good coverage for my dataset (versus the clams one) I don't know that I even have to worry about this too much.
#I want it to be as similar as possible, but I think I'm OK.