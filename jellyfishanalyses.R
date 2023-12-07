#Jellyfish analyses
#Rosemary Hartman
#last updated 9/05/2023

library(tidyverse)
library(lubridate)
library(glmmTMB)
library(DHARMa)
library(multcomp)
library(lme4)
library(emmeans)

library(effects)
library(lmerTest)
library(colorspace)
library(MuMIn)
library(broom)
library(visreg)
library(car)
library(ggbeeswarm)

#load the R data file with all the integrated data in it

load("jellyfish.RData")
yrs = read_csv("data/yearassignments.csv")
#I'm going to alter Dave's drougth color palette just slightly so the wet years aren't so dark

pal_yrtype <- c( "Critical" = "#FDE333", "Dry" = "#53CC67", "Below Normal" = "#009B95","Above Normal" = "#00588B", "Wet" = "#481F70FF") 
pal_yrtypecols = darken(pal_yrtype, amount = .3)
#steve wants catch by survey and month

monthly = group_by(AlljelliesTot, Month, Source) %>%
  summarize(Jellies = mean(TotJellies))
ylab3 = expression(paste(italic("M. marginata"), " CPUE (log-transformed)"))
ylabMaeotias <- expression(paste("Mean Jun-Oct ", italic(" M. marginata"), " CPUE (log-transformed)"))

AlljelliesTot = filter(AlljelliesTot, Year <2021) %>%
  mutate(Region = factor(Region, levels = c("North", "SouthCentral", "Confluence", "Suisun Marsh", "Suisun Bay")))

Alltotsub = filter(AlljelliesTot, Month %in% c(6,7,8,9,10), Region %in% c( "Suisun Bay","Confluence","Suisun Marsh")) %>%
  mutate(Yearf = as.factor(Year))



Allmeansub = filter(AlljelliesMean, Month %in% c(6,7,8,9,10)) %>% #, Region %in% c( "Suisun Bay","Confluence","Suisun Marsh"))
  mutate(Region = factor(Region, levels = c("North", "SouthCentral", "Confluence", "Suisun Marsh", "Suisun Bay")))

#limit it to just the summer, just regions where jellies are normally caught
Allmeansub2 = filter(AlljelliesMean, Month %in% c(6,7,8,9,10), Region %in% c( "Suisun Bay","Confluence","Suisun Marsh"))

AlljelliesMean2 = mutate(AlljelliesMean, rJellies = round(meanJellies), 
                         Yearf = as.factor(Year)) %>%
  filter( Month %in% c(6,7,8,9,10), Region %in% c( "Suisun Bay","Confluence","Suisun Marsh"))


#some exploritory plots
ggplot(AlljelliesMean, aes(x = Year, y = meanJellies, fill = ShortTerm)) +
  geom_col()+ facet_wrap(~Region)


ggplot(AlljelliesMean, aes(x = Year, y = meanJellies, fill = Yr_type)) +
  geom_col()+ facet_wrap(~Region)+
  theme_bw()+
  ylab("Mean Jellyfish CPUE")


ggplot(monthly, aes(x = Month, y = Jellies, fill = Source))+ geom_col(position = "dodge")

ggplot(AlljelliesMean, aes(x = Year, y = meanJellies)) +
  geom_col(aes(fill = Yr_type), color = "black", position = "dodge")+
  scale_fill_manual(values = pal_yrtype)+
  #geom_errorbar(aes(ymin = meanJellies-sdJellies, ymax = meanJellies + sdJellies, group = Region))+
  ylab("Mean monthly Maeotias CPUE") + theme_bw()+
  facet_wrap(~Region)

AlljelliesMean3 = group_by(AlljelliesMean, Year, Region, Yr_type) %>%
  summarize(CPUE = mean(meanJellies), sdJellies = sd(meanJellies), seJellies = sdJellies/n())
ggplot(AlljelliesMean3, aes(x = Year, y = CPUE)) +
  geom_col(aes(fill = Yr_type), color = "black", position = "dodge")+
  scale_fill_manual(values = pal_yrtype)+
  geom_errorbar(aes(ymin = CPUE-seJellies, ymax = CPUE+ seJellies, group = Region))+
  ylab("Mean monthly Maeotias CPUE") + theme_bw()+
  facet_wrap(~Region)

#############this was figure 3, then we cut it. But maybe I want it in supplemental...
ggplot(AlljelliesTot, aes(x = as.factor(Year), y = log(TotJellies+1))) +
  geom_boxplot(aes(fill = Yr_type, color = Yr_type))+
  scale_fill_manual(values = pal_yrtype, name = "Year Type")+
  scale_color_manual(values = pal_yrtypecols, name = "Year Type")+
  # geom_errorbar(aes(ymin = meanJellies-sdJellies, ymax = meanJellies + sdJellies))+
  ylab(ylab3) + theme_bw()+ xlab(NULL)+
  scale_x_discrete(breaks = c(2010, 2015, 2020))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position = "bottom")+
  facet_wrap(~Region, nrow = 1)

ggsave("plots/jelliesbyyear.tiff", device = "tiff", width = 8, height = 6)


ggplot(AlljelliesTot, aes(x = as.factor(Year), y = log(TotJellies+1))) +
  geom_boxplot(aes(fill = Yr_type, color = Yr_type))+
  scale_fill_manual(values = pal_yrtype, name = "Year Type")+
  scale_color_manual(values = pal_yrtypecols, name = "Year Type")+
  # geom_errorbar(aes(ymin = meanJellies-sdJellies, ymax = meanJellies + sdJellies))+
  ylab(ylab3) + theme_bw()+ xlab(NULL)+
  scale_x_discrete(breaks = c(2010, 2015, 2020))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position = "bottom")+
  facet_wrap(~Region, nrow = 1)


#is abundance related to temperature?
ggplot(AlljelliesTot, aes(x = Temp_surf, y = TotJellies, 
                          color = Yr_type)) +
  geom_point()+ facet_wrap(~Region)

#plot by drought designation, except we don't have any wet periods
ggplot(Alltotsub, aes(x = Drought, y = log(TotJellies+1))) +
  geom_boxplot()+ facet_wrap(~Region)
  
ggplot(Allmeansub, aes(x = Drought, y = log(meanJellies+1), fill = Drought)) +
  #  scale_fill_manual(guide = NULL, values = pal_drought)+
  geom_boxplot()+ facet_wrap(~Region) + theme_bw()+
  scale_x_discrete(labels = c("Drought", "Neutral", "Wet"))+
  ylab("Mean monthly jellyfish CPUE (log-transformed)")

##############
#Box plot of year type by region (this is the one we like)
#THis is figure 4 (now figure 3)
ggplot(Allmeansub, aes(x = Yr_type, y= log(meanJellies+1), fill = Yr_type)) +
  scale_fill_manual(guide = NULL, values = pal_yrtype)+
  geom_boxplot( alpha = 0.8)+ facet_wrap(~Region, nrow = 1) + theme_bw()+
  scale_x_discrete(labels = c("Critical", "Dry", "Below\nNormal", "Wet"))+
    ylab(ylabMaeotias)+xlab(NULL)
ggsave("plots/Jelliesboxplot.tiff", device = "tiff", width = 7, height = 5)

###############
#water year index versus jellyfish catch
ggplot(Allmeansub, aes(x =Index, y = log(meanJellies+1))) +
  geom_smooth(method = "lm")+
  geom_point()+ facet_wrap(~Region) + theme_bw()+
  # scale_x_discrete(labels = c("Drought", "Neutral", "Wet"))+
  ylab("Mean monthly jellyfish CPUE (log-transformed)")



###############################################################################
#What kind of analysis can we do on this?

hist(AlljelliesMean$meanJellies)
hist(Allmeansub2$meanJellies)
hist(log(Allmeansub2$meanJellies+1))
#slightly zero-inflated

#hmmm... or I could do it right and use count and the offset instead of the mean, then it's sure to be zero-inflated

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
Anova(jelz3c)



#now let's do some post hoc comparisons
pcs = emmeans(jelz3c, pairwise ~ "Yr_type|Region")
plot(pcs, comparisons = T)
pcs

pc = pcs$contrasts

plot(allEffects(jelz3c))
visreg(jelz3c, xvar = "Yr_type", by = "Region")

test = visreg(jelz3c, xvar = "Yr_type", by = "Region")
plot(test, gg = TRUE) + ggtitle("Mytitle")

effs = allEffects(jelz3c)
foo = predictorEffect("Yr_type", jelz3c)
foo2 = predictorEffect("Yr_type", jelz3c, residuals = TRUE)

#automatic plots
plot(foo, lines = list(multiline = TRUE), confint=list(style="auto"))
plot(foo2, lattice = list(key.args = list(columns = 3)))

plot(emmeans(jelz3c,pairwise ~ Yr_type|Region), comparison = T)
plot(emmeans(jelz3c,pairwise ~ Region|Yr_type))
#Huh. This seems to say there is no difference between water year types within a region

#pull out the useful bits to plot myself
effDF = bind_cols(foo2$fit, foo2$x, foo2$lower, foo2$upper) %>%
  rename(Prediction = ...1, Lower = ...4, Upper = ...5) %>%
  ungroup() %>%
  mutate(Yr_type = factor(Yr_type), Yr_type2 = as.numeric(Yr_type))


Resid = bind_cols(AlljelliesMean2, foo2$residuals) %>%
  rename(Residuals = ...15)  %>%
  ungroup() %>%
  mutate(Yr_type = factor(Yr_type))

effDF2 = left_join(effDF, Resid) %>%
  mutate(Residual2 = Residuals + Prediction, Yr_type2 = as.numeric(Yr_type))

#I was going to include a partial residual plot, but decided to cut it. 
ggplot(effDF, aes(x = Yr_type2, y = Prediction))+ geom_point()+ geom_line()+
  geom_errorbar(aes(ymin = Lower, ymax = Upper, group = Region))+
  facet_wrap(~Region)+
  geom_jitter(data = effDF2, aes(y = Residual2),  
             color = "blue", alpha = 0.3, width = 0.2)+
  theme_bw()+ xlab(NULL)+
  scale_x_continuous(breaks = c(1,2,3,4), labels =  c("Critical", "Dry", "Below\nNormal", "Wet"))+
  ylab("Model predicted Jun-Oct Meaotias CPUE \n(individuals per 10000m3)")

ggsave("plots/JellyResiduals.tiff", width = 7, height =4, device = "tiff")

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

#I think I first need to weight the salinity by number caught, same as the distribution

ggplot(filter(Alltotsub, TotJellies != 0), aes(x = Sal_surf)) + geom_histogram()
ggplot(filter(Alltotsub, TotJellies != 0), aes(x = Sal_surf)) + geom_boxplot(fill = "lightblue")+
  theme_bw()+ xlab("Salinity where jellyfish were caught")


ggplot(filter(Alltotsub, TotJellies != 0), aes(y = Sal_surf, x = 1, color = Yr_type)) + 
  geom_quasirandom()+
  scale_color_manual(values = pal_yrtype)+
  ylab("Salinity where jellyfish were caught") + xlab(NULL)+
  theme_bw()+theme(axis.text.x = element_blank())

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
  theme_bw()+
  facet_wrap(~Source)

Saliniteis = group_by(weighted, Salbin2) %>%
  dplyr::summarize(percent = sum(TotJellies)/sum(weighted$TotJellies))

#most catch is between 4.5 and 7.5. Rerun model to see if water year type is still significant.
sweetspot = filter(AlljelliesMean2, Sal_mean >minsal, Sal_mean <maxsal)

ggplot(sweetspot, aes(x = Yr_type, y = log(meanJellies+1))) + geom_boxplot()


#plot of catch between 4.5 and 7.5
ggplot(sweetspot, aes(x = Yr_type, y = log(meanJellies+1), fill = Yr_type)) + geom_boxplot(alpha = 0.8)  +
  # facet_wrap(~Region)+
  scale_fill_manual(values = pal_yrtype) +
  ylab(ylabMaeotias) + xlab("Water Year Type")+
  theme_bw()+ theme(legend.position = "none")
ggsave("plots/Maeotias4_7ppt.tiff", device = "tiff", height = 4, width = 4)


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
# 
# jellysta = dplyr::select(Alljellies2, Station, StationID, Source, Latitude, Longitude) %>%
#   filter(!is.na(Latitude), Source != "20mm") %>%
#   distinct()
# 
# distance<-GGdist(Water_map = spacetools::Delta, Points = jellysta, Latitude_column = Latitude,
#                  Longitude_column = Longitude, PointID_column = StationID) 
# distance =  distinct(distance)
# 
# #Do I want to calculate the average distance for all sites where Meaotias was caught? 
# #OR do I weight the stations by the number caught?
# #I want to weight it.
# distancex = left_join(distance, jellysta) %>%
#   dplyr::select(-Station) 
# 
# Alljel = left_join(AlljelliesTot, distancex) 
# Alljelsum = mutate(Alljel, weightedD = Distance*TotJellies) %>%
#   filter(!is.na(Distance)) %>%
#   group_by(Month, Year, Yr_type, Index) %>%
#   summarize(Meandist = sum(weightedD)/(sum(TotJellies, na.rm = T)), jellies = sum(TotJellies, na.rm = t)) %>%
#   droplevels()
# 
# save(Alljel, Alljelsum, file = "data/JellydatawDistance.Rdata")
#get dayflow outflow
#(Grab Dayflow from the flowplots.R file)
load("data/Dayflow.RData")
DFmonth = mutate(DF, Month = month(Date), Year = year(Date)) %>%
  group_by(Month, Year) %>%
  dplyr::summarize(Outlfow = mean(OUT))
 load("data/JellydatawDistance.Rdata")# 
 #Take out all samples with no jellies, or less than 20 total jellies
 Alljelsum = left_join(Alljelsum, DFmonth) %>%
   filter(!is.nan(Meandist), jellies >20)
# 
# save(Alljel, Alljelsum, distancex, file = "data/JellieswDistance.Rdata")

 Mypal = c(brewer.pal(12, "Set3"), brewer.pal(8, "Dark2"))
# 
# #Facet by month
 ggplot(Alljelsum, aes(x = Meandist, y = Outlfow)) + 
   geom_point(aes(color = as.factor(Year))) + geom_smooth(method = "lm")+
   facet_wrap(~Month)
# 
# ################
# #linear model 
# 
# #convert cubic feet to cubic meters
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
# 
write.csv(jl1s$coefficients, "outputs/jellyfishdistance.csv")
# 


# #format an equation to print on the graph
EQ = paste("y = ", format(unname(coef(jl1s)[1]), digits = 3), " + ",
            b = format(unname(coef(jl1s)[2]), digits = 2), "x,", " R2 = ",
            r2 = format(R2[1], digits = 3), sep = "")
 EQ

 # 
 pal_yrtype2 <- c( "Critical" = "#FDE333", "Dry" = "#53CC67", "Below Normal" = "#009B95","Wet" = "#481F70FF") 
# 
# #Plot of outflow versus center of distribution for paper
 ylabMaeotias2 =  expression(paste("Center of ", italic("M. marginata"), " distribution (Km from Golden Gate)"))
 ggplot(droplevels(Alljelsum), aes(y = DistK, x = OutflowM)) + 
   geom_point(aes(color = Yr_type)) + geom_smooth(method = "lm") + 
   scale_color_manual(values = pal_yrtype2, name = "Water Year\nType")+
   ylab(ylabMaeotias2)+
   xlab("Monthly Mean Delta Outflow (m3/sec)") +
   annotate("text", y = 75, x = 200, label = EQ)+
   theme_bw()
 ggsave("plots/Jelliesdistance.tiff", device = "tiff", height = 5, width = 6)
########################################
#try log-transforming
jl2 = lmer(log(DistK)~ OutflowM + (1|Yearr), data = Alljelsum)
summary(jl2)
plot(jl2)
res = simulateResiduals(jl2)
plot(res)
jl2s = summary(jl2)
R22 = r.squaredGLMM(jl2)



# #format an equation to print on the graph
EQ2 = paste("y = ", format(unname(coef(jl2s)[1]), digits = 3), " + ",
           b = format(unname(coef(jl2s)[2]), digits = 2), "x,", " R2 = ",
           r2 = format(R22[1], digits = 3), sep = "")
EQ2
# 
# #Plot of outflow versus center of distribution for paper
ggplot(droplevels(Alljelsum), aes(y = log(DistK), x = OutflowM)) + 
  geom_point(aes(color = Yr_type)) + geom_smooth(method = "lm") + 
  scale_color_manual(values = pal_yrtype2, name = "Water Year\nType")+
  ylab("log Center of Maeotias distribution \n(Km from Golden Gate)")+
  xlab("Monthly Mean Delta Outflow (m3/sec)") +
 annotate("text", y = 4.1, x = 5, label = EQ2)+
  theme_bw()
ggsave("plots/Jelliesdistance.tiff", device = "tiff", height = 5, width = 6)

######################################################################3
#Back of the envelope feeding rate analyssis

Means = AlljelliesMean2 %>%
  filter(Season == "Summer") %>%
  group_by(Yr_type, Region) %>%
  summarise(Mean = mean(meanJellies)/10000, FR = Mean*100, FRlow = Mean*20, FRhigh = Mean*1000)

#How many copepods are around?
library(zooper)
library(readxl)
biomass = read_excel("data/Biomass conversions_CEB Updated.xlsx")
zoops = Zoopsynther(Data_type = "Community",
                    Sources = c("EMP", "FMWT", "STN"), Size_class = c("Meso", "Micro"))

#Get rid of undersampled groups, just summer, 2007-2021
zoops2 = filter(zoops, !Undersampled, Year > 2006) %>%
  mutate(Month = month(Date))  %>%
  filter(Month %in% c(6,7,8,9,10), Class == "Copepoda") %>%
  mutate(Taxname = str_remove(Taxname, "_UnID")) %>%
  left_join(biomass) %>%
  mutate(BPUE = CPUE*CarbonWeight_ug)

#add regional assignments
zoopstas = read.csv("data/IEPstationsw_Regions.csv") %>%
  mutate(Station = as.character(Station))

#just the three regions with lots of jellyfish. Just the copepods
zoops3 = left_join(zoops2, zoopstas) %>%
  filter(Region %in% c( "Confluence",   "Suisun Bay", "Suisun Marsh", "SouthCentral")) %>%
  group_by(SampleID, Region, Month, Year) %>%
  summarise(Copepods = sum(CPUE, na.rm = T), BPUE = sum(BPUE, na.rm =T)) %>%
  left_join(yrs) %>%
  group_by(Region, Yr_type) %>%
  summarise(CopepodsM = mean(Copepods), sdCops = sd(Copepods)/sqrt(n()), BPUE = mean(BPUE, na.rm =T)) %>% 
  mutate(Taxa = "Maeotias",Yr_type = factor(Yr_type, levels = c("Critical", "Dry","Below Normal", "Wet"),
                                            labels = c("Critical", "Dry", "Below\nNormal", "Wet")))
  
#Calculate percentage of copepods eaten over the course of the summer (assuming no copepod reproduction)
Means2 = left_join(Means, zoops3) %>%
  mutate(CopepodsPerSummer = FR*90, PercentEaten = CopepodsPerSummer/CopepodsM,
         CopepodsPerSummerlow = FRlow*90, PercentEatenlow = CopepodsPerSummerlow/CopepodsM,
         CopepodsPerSummerhigh = FRhigh*90, PercentEatenhigh = CopepodsPerSummerhigh/CopepodsM,
         Yr_type = factor(Yr_type, levels = c("Critical", "Dry", "Below Normal", "Wet")))

write.csv(Means2, "JellyGrazing.csv")

ggplot(Means2)+ geom_col(aes(x = Region, y = CopepodsM), fill = "grey")+ 
  geom_col(aes(x = Region, y = CopepodsPerSummer), fill = "darkblue")+
  geom_errorbar(aes(x = Region, ymin = CopepodsM-sdCops, ymax = CopepodsM+sdCops))+
  geom_errorbar(aes(x = Region, ymin = CopepodsPerSummerlow, ymax = CopepodsPerSummerhigh, group = Yr_type), width = 0.5, color = "blue")+
  facet_wrap(~Yr_type)+ theme_bw() + ylab("Mean Calanoid copepod CPUE")

#bleh
Means3 = pivot_longer(Means2, cols=c(CopepodsPerSummer, CopepodsM), names_to = "Metric", values_to = "Copepods")


ggplot(Means3)+ geom_col(aes(x = Region, y = Copepods, fill = Metric), position = "dodge")+ 
#  geom_errorbar(data = filter(Means3, Metric == "CopepodsM"), 
 #               aes(x = Region, ymin = Copepods-sdCops, ymax = Copepods+sdCops, group = Yr_type), 
  #              position = position_nudge(x = -0.25), width = 0.3)+
  geom_errorbar(data = filter(Means3, Metric == "CopepodsPerSummer"), 
                aes(x = Region, ymin = CopepodsPerSummerlow, ymax = CopepodsPerSummerhigh, group = Yr_type), 
                width = 0.2, color = "blue", position = position_nudge(x = 0.25))+
  scale_fill_manual(values = c("darkred", "lightblue"), 
                    labels = c("Mean Copepopd CPUE", "Potential jellyfish \n consumption"))+
  facet_wrap(~Yr_type)+ theme_bw() + ylab("Mean Calanoid copepod CPUE")


#Wim says to use clearence rates instead of prey number per day
#Moller et al 2007 said that for Aurelia feeding on Acarita tonsa, clearence rate was F = 0.0073D^2.1 in liters/day at 15C
#it also found temperature effected things at a rate of F=1.17*exp(.18T)
#Wintzer et al 2011 found Meaotias between 2-45 mm in bell diameter. But most of the trawl
#surveys are probably getting 30-40 mm guys.

#Divide by 1000 to convert to cubic meters
Clear = 0.0073*40^2.1/1000
clear2 = 1.17*exp(.18*20)/1000

#Hansen has the clearence rate at 0.093*D^2 (artemia larvae), 0.002025*D^1.88 (copepedites)
clear3 = 0.002025*40^1.88/1000

#Oelsen et al 1995 said 580 mL/hr per individual (30mm) at 21C
clear4 = 580/1000*24/1000
#Morand found Eurhamphaea (ctinophore 40mm diametr) clearance from 12-36 L per day on copepods
clear5 = 20/1000

#Chrysaora (scyphozoa) 40mm size had a clearence rate of 240 L/day
clear6 = 240/1000

#riisgard found a 43 mm aurelia (scyphozoa)cleared adult copepods at a rate of 0.6 l/hr, so 14.4 L/day, higher for nauplii
clear7 = 14.4/1000



MeansCL = AlljelliesMean2 %>%
  filter(Season == "Summer") %>%
  group_by(Yr_type, Region) %>%
  summarise(Mean = mean(meanJellies)/10000, clearance = Mean*clear2, percent = clearance*100)

#jellies clearence

write.csv(MeansCL, "MeansCL.csv", row.names = F)

#copepod mortality between 0.05 and 0.46, kimmerer et al 2018

#That wasn't very impressive. Well, what about the densities durign a bloom?

Maxes = arrange(Alljellies2b, -CPUE)[1:10,]

Maxes = mutate(Maxes, clearance = CPUE/10000*clear2)

Maxes2 = group_by(Alljellies2b, Region, Yr_type) %>%
  summarize(Max = max(CPUE, na.rm = T), Maxclearance = Max/10000*clear2, Mean = mean(CPUE, na.rm = T), Meanclearance = Mean/10000*clear2)

write.csv(Maxes2, "Clearence.csv")

######################################################################

#combine with clam clearence and graph

clear = read_csv("Clearence_wclams.csv") %>%
  mutate(Yr_type = factor(Yr_type, levels = c("Critical", "Dry","Below Normal", "Wet"),
                          labels = c("Critical", "Dry", "Below\nNormal", "Wet")),
         Region =  factor(Region, levels = c("North", "SouthCentral", "Confluence", "Suisun Marsh", "Suisun Bay")))

ggplot(clear, aes(x = Yr_type,fill = Taxa)) + 
  facet_wrap(~Region, nrow = 1) +
  geom_col(position = "dodge", aes( y = MeanClearence))+ 
  geom_col(position = "dodge", aes( y = MaxClearence), alpha = 0.5)+ 
  theme_bw()+
  scale_fill_brewer(palette = "Dark2", name = "Taxon")+ylab("Clearance Rate (/day)")+
  xlab("Water Year Type")
ggsave("plots/grazingclamjellies.tiff", device = "tiff", width = 8, height = 5, units = "in")

#######################################################
#combine with clorophyll data for ammount of chlorophyll
#the critters could be eating over the course of the summer

chloro = read_csv("data/Combined Average Chloro for Jellies.csv")
#group by water year type and average

chloro2 = left_join(chloro, yrs) %>%
  group_by(Region, Yr_type) %>%
  dplyr::summarize(chl = mean(avg_chloro, na.rm =T)) %>%
  mutate(Taxa = "clams",
         Yr_type = factor(Yr_type, levels = c("Critical", "Dry","Below Normal", "Wet"),
         labels = c("Critical", "Dry", "Below\nNormal", "Wet")))

#Calculate potential annual loss of chlorophyll, in ug. Then convert to mgC by muliplying by 32 (from Lucas et al 2006) and dividing by 1000. 
#did I do this right? Do I have to do something to reflect the fact that chlorophyll is in L and clams are in square meters? probably not because the clearence rate is a proportion.
clear2 = left_join(clear, chloro2) %>%
  mutate(maxchloro = chl*MaxClearence, meanchloro = chl*MeanClearence,
         maxchloroY = chl*MaxClearence*365, meanchloroY = chl*MeanClearence*365,
         maxC = maxchloro*32/1000, meanC = meanchloro*32/1000)

#now zooplankton loss, in # of copepods

clear3 = left_join(clear2, zoops3) %>%
  mutate(maxzoo = CopepodsM*MaxClearence, meanzoo = CopepodsM*MeanClearence,
         maxzooB = BPUE*MaxClearence/1000, meanzooB = BPUE*MeanClearence/1000)

#I need to convert to biomass of phyto and zooplankton,
#but I'll start with this.
ggplot(dplyr::filter(clear3, Taxa == "clams"), aes(x = Yr_type))+
  facet_wrap(~Region, nrow = 1) +
  geom_col(position = "dodge", aes( y = meanchloro))+ 
  geom_col(position = "dodge", aes( y = maxchloro), alpha = 0.5)+ 
  theme_bw()+
  scale_fill_brewer(palette = "Dark2", name = "Taxon")+
  ylab("Micrograms chlorophyll per Day per square meter")+
  xlab("Water Year Type")

#but I'll start with this.
ggplot(dplyr::filter(clear3, Taxa == "clams"), aes(x = Yr_type))+
  facet_wrap(~Region, nrow = 1) +
  geom_col(position = "dodge", aes( y = meanchloro))+ 
  geom_col(position = "dodge", aes( y = maxchloro), alpha = 0.5)+ 
  theme_bw()+
  scale_fill_brewer(palette = "Dark2", name = "Taxon")+
  ylab("MgC per m3/day")+
  xlab("Water Year Type")

ggplot(dplyr::filter(clear3, Taxa == "Maeotias"), aes(x = Yr_type))+
  facet_wrap(~Region, nrow = 1) +
  geom_col(position = "dodge", aes( y = meanzoo))+ 
  geom_col(position = "dodge", aes( y = maxzoo), alpha = 0.5)+ 
  theme_bw()+
  scale_fill_brewer(palette = "Dark2", name = "Taxon")+
  ylab("Copepods consumed per cubic meter per Day")+
  xlab("Water Year Type")

ggplot(dplyr::filter(clear3, Taxa == "Maeotias"), aes(x = Yr_type))+
  facet_wrap(~Region, nrow = 1) +
  geom_col(position = "dodge", aes( y = meanzooB))+ 
  geom_col(position = "dodge", aes( y = maxzooB), alpha = 0.5)+ 
  theme_bw()+
  scale_fill_brewer(palette = "Dark2", name = "Taxon")+
   scale_fill_brewer(palette = "Dark2", name = "Taxon")+
  ylab("Biomass of Copepods (mg) consumed per cubic meter per day")+
  xlab("Water Year Type")


#see if I can combine the carbon consumption for both taxa
ann = data.frame(Yr_type = "Critical", Clearance = .25, Letter = "B", Region = "North") %>%
  mutate(Region = factor(Region, levels = c("North", "SouthCentral", "Confluence", "Suisun Marsh", "Suisun Bay"),
                         labels =  c("North", "SouthCentral", "Confluence", "Suisun Marsh", "Suisun Bay")))

labelsX = c("Mean Clams", "Max Clams", 
          expression(paste("Mean", italic(" M. marginata"))),
          expression(paste("Max", italic(" M. marginata"))))

clear4 = pivot_longer(clear3, cols = c(meanC, maxC, meanzooB, maxzooB), names_to = "parameter",
                      values_to = "CarbonConsumed") %>%
  mutate(param = str_sub(parameter, 1, 3),
         parameter = factor(parameter, levels = c("meanC", "maxC", "meanzooB", "maxzooB"),
                            labels = c("Mean Clams", "Max Clams", 
                                       "Mean M. marginata",
                                      "Max M. marginata")),
                
         Region = factor(Region, levels = c("North", "SouthCentral", "Confluence", "Suisun Marsh", "Suisun Bay"),
                         labels =  c("North", "SouthCentral", "Confluence", "Suisun Marsh", "Suisun Bay")),
         Taxa = factor(Taxa, levels = c("clams", "Maeotias"), labels = c("Clams", "M. marginata")))

ggplot(clear4, aes(x = Yr_type, y = CarbonConsumed, fill = parameter, alpha = parameter))+
  geom_col(position = "dodge")+
  facet_wrap(~Region, nrow =1)+
  theme_bw()+
  scale_alpha_manual(values = c(1, .3, 1, .4), name = NULL, labels = labelsX)+
  scale_fill_manual(values = c("springgreen4", "springgreen4", "darkorange2", "darkorange2"), 
                    name = NULL, labels = labelsX)+
ylab("Estimated Grazing Impact \n(mgC/m3/day)")+
  xlab("Water Year Type")+
  theme(legend.position = "bottom",legend.text.align = 0)+
  geom_text(data = ann, aes(label = Letter, x = Yr_type, y = Clearance,), inherit.aes = FALSE,
            size = 12)

ggsave("plots/grazingclamjellies2.tiff", device = "tiff", width = 8, height = 5, units = "in")


#make the clearence rate graph match
clear4.5 = pivot_longer(clear3, cols = c(MeanClearence, MaxClearence), names_to = "parameter",
                      values_to = "Clearance") %>%
  mutate(param = paste(str_sub(parameter, 1, 3), Taxa),
         Region = factor(Region, levels = c("North", "SouthCentral", "Confluence", "Suisun Marsh", "Suisun Bay"),
                         labels =  c("North", "SouthCentral", "Confluence", "Suisun Marsh", "Suisun Bay")),

         parameter2 = factor(param, levels = c("Mea clams", "Max clams", "Mea Maeotias", "Max Maeotias"),
                   labels = c("Mean Clams", "Max Clams", 
                              "Mean M. marginata",
                              "Max M. marginata")))

ann2 = data.frame(Yr_type = "Critical", Clearance = .9, Letter = "A", Region = "North") %>%
  mutate(Region = factor(Region, levels = c("North", "SouthCentral", "Confluence", "Suisun Marsh", "Suisun Bay"),
                  labels =  c("North", "SouthCentral", "Confluence", "Suisun Marsh", "Suisun Bay")))

ggplot(clear4.5, aes(x = Yr_type, y = Clearance, fill = parameter2, alpha = parameter2))+
  geom_col(position = "dodge")+
  facet_wrap(~Region, nrow =1)+
  theme_bw()+
  scale_alpha_manual(values = c(1, .3, 1, .4), name = NULL, labels = labelsX)+
  scale_fill_manual(values = c("springgreen4", "springgreen4", "darkorange2", "darkorange2"), 
                    name = NULL, labels = labelsX)+
  ylab("Water Column Turnover Rate \n(proportion of water column per day)")+
  xlab("Water Year Type")+
  theme(legend.position = "none")+
  geom_text(data = ann2, aes(label = Letter, x = Yr_type, y = Clearance,), inherit.aes = FALSE,
            size = 12)
ggsave("plots/grazingclamjellies.tiff", device = "tiff", width = 8, height = 4, units = "in")

#####################################################

#how unbalanced are my samples?


ggplot(AlljelliesTot, aes(x = Distance)) +geom_histogram()+
  facet_grid(Month~Year)

#Actually pretty well balanced bewteen years, just not between months
