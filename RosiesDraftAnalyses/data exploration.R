#Look at some of the big-picture differences between wet and dry years
library(tidyverse)
library(readxl)
library(viridis)
library(DroughtData)

#Boz's integrated data set
Integrated_data_set = lt_seasonal

#Fish indecies
Fish <- read_excel("data/Integrated data set.xlsx", na = "NA") %>%
  mutate(Season = factor(Season, levels = c("Winter", "Spring", "Summer", "Fall")),
         SmeltIndex = as.numeric(SmeltIndex)) %>%
  dplyr::select(Year, Season, Sbindex, SmeltIndex, LongfinIndex, AmShadIndex) %>%
  rename(YearAdj = Year)

yrs = read_csv("data/yearassignments.csv"
)

#grab zooplankton data from Arthur
zoopsBPUE_seasonal = read_csv("data/zoop_drought_lt_bpue_szn.csv") %>%
  rename(YearAdj = water_year, ZoopBPUE = BPUE_ug)
zoopsBPUE_regional = read_csv("data/zoop_drought_lt_bpue_reg.csv") %>%
  rename(YearAdj = water_year)
Integrated_data = left_join(Integrated_data_set, zoopsBPUE_seasonal) %>%
  left_join(Fish)

#Chlorophyll data from KEith
Chl = read_csv("data/chla_data_stats_LT2.csv")
#ok, beautiful! do some averaging
Chl2 = group_by(Chl, Region, Season, month, ds_year) %>%
  summarize(Chla = mean(chlaAvg)) %>%
  group_by( Region, Season, ds_year) %>%
  summarize(Chla = mean(Chla)) %>%
  group_by(Season, ds_year) %>%
  summarize(Chla = mean(Chla), logChl = log(Chla)) %>%
  rename(YearAdj = ds_year)

Chl2reg = group_by(Chl, Region, Season, month, ds_year) %>%
  summarize(Chla = mean(chlaAvg)) %>%
  group_by( Region, Season, ds_year) %>%
  summarize(Chla = mean(Chla), logChl = log(Chla)) %>%
  rename(Year = ds_year) %>%
  left_join(yrs)

Integrated = left_join(Integrated_data, Chl2)

  
#log-transform zooplankton and fish
Int = mutate(Integrated, logDS = log(SmeltIndex +1), logShad = log(AmShadIndex +1), 
             logSB = log(Sbindex+1), logLFS = log(LongfinIndex+1), logzoopB = log(ZoopBPUE)) 

#transition the data set from wide to long. 
IntLong = pivot_longer(Int, cols = `Outflow`:logzoopB, 
                       names_to = "Metric", values_to = "Value")

#look at it without the "not drought or wet" years
ggplot(filter(IntLong, Drought != "N"), aes(x = Drought, y = Value)) + geom_boxplot() +
  facet_grid(Metric~Season, scales = "free_y")

#Huh. Chlorophyll goes up
Chla = filter(IntLong, Metric == "logChl", Drought != "N")
ggplot(Chla, aes(x = Drought, y = Value)) + geom_boxplot()+ facet_wrap(~Season)
ggplot(filter(Chla, Season == "Fall"), aes(x = Drought, y = Value)) + geom_boxplot()
ggplot(filter(Chl2reg, Drought != "N"), aes(x = Drought, y = logChl)) + geom_boxplot()+ facet_wrap(Region~Season)

#quick model of chlorophyll
m = glm(logChl~ Drought*Season+Region +Season, data = Chl2reg)
summary(m)
emmeans(m, pairwise ~ Drought|Season, adjust = "sidak")
#wow, there are really no broad-scale patterns with Chlorophyll

zoo = filter(IntLong, Metric == "logzoopB")
ggplot(zoo, aes(x = Drought, y = Value)) + geom_boxplot()+ facet_wrap(~Season)
ggplot(filter(zoo, Season == "Fall"), aes(x = Drought, y = Value)) + geom_boxplot()

Temp = filter(IntLong, Metric == "Temperature", Drought != "N")
ggplot(Temp, aes(x = Drought, y = Value)) + geom_boxplot()+ facet_wrap(~Season, scales = "free_y")


#Let's make a rough "Drought impact" index. I"m just making this up tho.

DroughtImpact = group_by(IntLong, Season, Metric, Drought) %>%
  summarize(Mean = mean(Value, na.rm = T)) %>% 
  pivot_wider(names_from = Drought, values_from = Mean) %>%
  mutate(Index = (D-W)/mean(c(D,N, W), na.rm = T), IndexB = D/W)

#

#Now let's try scaling all the variables first and then creating a drought index
#also limit it to just the FMWT index for fish
#and get rid of winter zoops cause i don't have a lot of those
DrIm2 = Int %>%
  mutate(SmeltIndex = case_when(
    Season == "Fall" ~ SmeltIndex,
  ),
  logzoopB = case_when(
    Season %in% c("Fall", "Spring", "Summer") ~ logzoopB
  ),
  Turbidity = Secchi *-1) %>%
  mutate(across(`Outflow`:Turbidity, scale)) %>%
  pivot_longer(cols = `Outflow`:Turbidity, names_to = "Metric", values_to = "Value") %>%
  group_by(Season, Metric, Drought) %>%
  summarize(Mean = mean(Value, na.rm = T)) %>% 
  pivot_wider(names_from = Drought, values_from = Mean) %>%
  mutate(Index = (D-W), IndexB = D/W)

ggplot(DroughtImpact, aes(x = Season, y = Index)) + facet_wrap(~Metric)+
  geom_col()

ggplot(DrIm2, aes(x = Season, y = Index)) + facet_wrap(~Metric)+
  geom_col()

#the fish have such a huge difference it's hard to see everything else.
ggplot(DrIm2, 
       aes(x = Metric, y = Index, fill = Index)) + facet_wrap(~Season)+
  geom_col()+
  scale_fill_gradient2(low = "red", high = "blue", mid = "grey") + theme_bw()


#Some sort of threshold?
#Some sort of good/bad indicator. 
Cats = read_excel("data/Integrated data set.xlsx", sheet = "Categories")
DrIm2b = left_join(DrIm2, Cats) %>%
  mutate(Index2 = abs(Index), colr = case_when(
    Index <0 ~ "red",
    Index >0 ~ "blue"
  )) %>%
  filter(!is.na(colr), Metric %in% c("DeltaExport", "Delta Outflow", "DissAmmonia", "DissNitrateNitrite",
                                     "logChla", "logzoopB", "Salinity", "Sbindex",  "Turbidity",
                                     "SmeltIndex", "Temperature"))

DrIm2b = mutate(DrIm2b, 
                Metric = factor(Metric,  levels =  c("DeltaExport", "Delta Outflow",
                                                     "Turbidity","Salinity",  "Temperature",
                                                     "DissAmmonia", "DissNitrateNitrite",
                  "logChla", "logzoopB", "Sbindex", "SmeltIndex"), 
                                        labels = 
                                          c("Exports", "Outflow","Turbidity", 
                                            "Salinity", "Temperature",
                                            "Ammonium", "Nitrate",
                                            "Chla", "Zoops", "Stripers", 
                                            "Smelt")))

ggplot(DrIm2b, aes(x=Category, group = Metric)) +
  geom_col(aes(fill = colr, y = Index2, alpha = Index2), 
           position =position_dodge2(width = 1, preserve = "single"))+
  geom_text(aes(label = Metric, y = Index2), position = position_dodge(.9))+
  scale_fill_manual(values = c("blue", "red"), labels = c("increase", "decrease"),
                    name = "Direction of \n Drought Impact")+
  coord_polar() + theme_bw()+
  scale_alpha(guide = NULL)+
  scale_y_continuous( name = NULL) + facet_wrap(~Season)



ggplot(DrIm2b, aes(x=Category, group = Metric)) +
  geom_col(aes(fill = colr, y = Index, alpha = Index2), 
           position =position_dodge2(width = 1, preserve = "single"))+
  #geom_text(aes(label = Metric, y = Index2), position = position_dodge(.9))+
  scale_fill_manual(values = c("blue", "red"), labels = c("increase", "decrease"),
                    name = "Direction of \n Drought Impact")+
  geom_text(aes(x = Category, label = Metric, y = 0, group = Metric), hjust = 0, angle = 90,
            position = position_dodge2(width = 1, preserve = "single"))+
   theme_bw()+
  scale_y_continuous( name = NULL) + facet_grid(.~Season, space = "free")


#Color code by good versus bad
ggplot(DrIm2b, aes(x=Category, group = Metric)) +
  geom_col(aes(fill = GoodBad, y = Index, alpha = Index2), 
           position =position_dodge2(width = 1, preserve = "single"))+
  #geom_text(aes(label = Metric, y = Index2), position = position_dodge(.9))+
  scale_fill_manual(values = c("red", "blue"), labels = c("Stressor", "Good Thing"),
                    name = "Direction of \n Drought Impact")+
  geom_text(aes(x = Category, label = Metric, y = 0, group = Metric), hjust = 0, angle = 90,
            position = position_dodge2(width = 1, preserve = "single"))+
  theme_bw()+
  scale_y_continuous( name = NULL) + facet_grid(.~Season, space = "free")


#create an annual index
AnnIm = group_by(DrIm2b, GoodBad, Metric, Category) %>%
  summarize(Index = mean(Index), IndexB = mean(IndexB), Index2 = mean(Index2)) %>%
  mutate(colr = case_when(
    GoodBad == "Bad Things" & Index >0 ~ "red",
    GoodBad == "Bad Things" & Index <0 ~ "blue",
    GoodBad == "Good Things" & Index >0 ~ "blue",
    GoodBad == "Good Things" & Index <0 ~ "red",
  ))

#Color code by good versus bad
ggplot(AnnIm, aes(x=Metric, group = Metric)) +
  geom_col(aes(fill = colr, y = Index, alpha = Index2), 
           position =position_dodge2(width = 1, preserve = "single"))+
  scale_fill_manual(values = c("blue", "red"), guide = NULL)+
  geom_text(aes(x = Metric, label = Metric, y = 0, group = Metric), hjust = 0, angle = 90,
            position = position_dodge2(width = 1, preserve = "single"))+
  theme_bw()+
  scale_y_continuous( name = "Drought Impact Level") + facet_grid(.~GoodBad, scales = "free_x") +
  scale_x_discrete(name = NULL) + theme(axis.text.x = element_blank()) +
  scale_alpha(guide = NULL)

#try a different way

#Color code by good versus bad
ggplot(AnnIm, aes(x=Metric, group = Metric)) +
  geom_col(aes(fill = GoodBad, y = Index, alpha = Index2), 
           position =position_dodge2(width = 1, preserve = "single"))+
  scale_fill_manual(values = c("red", "blue"))+
  geom_text(aes(x = Metric, label = Metric, y = 0, group = Metric), hjust = 0, angle = 90,
            position = position_dodge2(width = 1, preserve = "single"))+
  theme_bw()+
  scale_y_continuous( name = "Drought Impact Level") + 
  scale_x_discrete(name = NULL) + theme(axis.text.x = element_blank()) +
  scale_alpha(guide = NULL)


#Now without the good/bad colors, 'cause no one likes that
ggplot(AnnIm, aes(x=Metric, group = Metric)) +
  geom_col(aes(y = Index, alpha = Index2, fill = Category), color = "darkgrey",
           position =position_dodge2(width = 1, preserve = "single"))+
  geom_text(aes(x = Metric, label = Metric, y = 0, group = Metric), hjust = 0, angle = 90,
            position = position_dodge2(width = 1, preserve = "single"))+
  theme_bw()+
  scale_fill_brewer(palette = "Set1", labels = c("Fish", "Hydrology", "Lower Trophic", "Water Quality"))+
  scale_y_continuous( name = "Drought Impact Level") + 
  scale_x_discrete(name = NULL) + theme(axis.text.x = element_blank()) +
  scale_alpha(guide = NULL)

#see if the ratio of the raw data works better than the difference inthe scaled data
AnnIm2 = group_by(DroughtImpact,Metric) %>%
  summarize(Index = mean(Index, na.rm = T), IndexB = mean(IndexB, na.rm = T)) %>%
  mutate(colr = case_when(
    IndexB < 1 ~ "red",
    IndexB >1 ~ "blue"
  ))



ggplot(AnnIm2, aes(x=Metric)) +
  geom_col(aes(y = IndexB, fill = colr))+
  geom_text(aes(x = Metric, label = Metric, y = 0, group = Metric), hjust = 0, angle = 90,
            position = position_dodge2(width = 1, preserve = "single"))+
  theme_bw()+
  scale_y_continuous( name = "Drought Impact Level") + 
  scale_x_discrete(name = NULL) + theme(axis.text.x = element_blank())+
  scale_fill_manual(values = c("lightblue", "red"),labels = c("increase", "decrease"))


#############################################################
#I'm going to try a new index where I run a linear model on each metric and use the 
#R2 from the model as my index.

Rsquar = function(data, Value, vars) {
  rs = data.frame(Metrics = vars, Rs = rep(NA, length(vars)), 
                  Est = rep(NA, length(vars)),
                  Ps = rep(NA, length(vars)))
  for(i in 1:length(vars)){
    m1 = lm(unlist(data[,vars[i]])~ data$Drought)
    rs[i,2]= summary(m1)$adj.r.squared
    rs[i,3]= -summary(m1)$coefficients[3,1]
    rs[i,4]= summary(m1)$coefficients[3,4]
    
  }
  return(rs)
  
}

AnnIm3 =  Int %>%
  mutate(SmeltIndex = case_when(
    Season == "Fall" ~ SmeltIndex,),
  logzoopB = case_when(
    Season %in% c("Fall", "Spring", "Summer") ~ logzoopB ),
  TempSummer = case_when(
    Season %in% c("Summer") ~ Temperature
  ),
  Turbidity = Secchi * -1) %>%
  mutate(across(`Outflow`:Turbidity, scale)) 

#Hmmm, no effect of temperature, but maybe it gets swamped by seasonal effects
m = glm(Temperature~ Drought+Season, data = AnnIm3)
m2 = glm(Temperature~ Drought, data = AnnIm3)
summary(m)
library(car)
Anova(m)
library(visreg)
visreg(m)
visreg(m, xvar = "Drought", by = "Season")
visreg(m, xvar = "Season", by = "Drought")


#can I pull out the Rsquraed for just the drought effect?
library(rsq)
rsq.partial(m, adj = T)
rsq.partial(m, m2, adj = TRUE)

test = Rsquar(AnnIm3, vars = names(AnnIm3)[6:25])
AnnIm4 = mutate(test, colr = case_when(
  Ps > 0.05 ~ "grey",
  Ps < 0.05 & Est >0 ~ "blue",
  Ps < 0.05 & Est < 0 ~ "red"
),
sig = case_when(
  Ps > 0.05 ~ "grey",
  Ps < 0.05 ~ "blue"
)
  ) %>%
  filter(!Metrics %in% c("logzooC", "SpCndSurface", "TotPhos", "Secchi", "X2", "Sbindex", "ZoopBPUE", 
                         "SmeltIndex", "LongfinIndex", "AmShadIndex", "Chla"))


AnnIm4 = mutate(AnnIm4, Metrics = factor(Metrics, levels =  c("Export", "Outflow",
                                     "Turbidity","Salinity",  "Temperature", "TempSummer",
                                     "logzoopB", "logSB", "logDS", "logLFS", "logShad", "logChl"), 
                labels = 
                  c("Exports",  "Outflow","Turbidity", 
                    "Salinity", "Temperature", "Summer Temperature",
                    
                    "Zooplankton", "Age-Zero Striped Bass", 
                    "Delta Smelt", "Longfin Smelt", "American Shad", "Chlorophyll")))


#plot of R2
ggplot(filter(AnnIm4, !Metrics %in% c("Temperature", "Zoops")), aes(x=Metrics)) +
  geom_col(aes(y = Rs, fill = colr))+
  geom_text(aes(x = Metrics, label =  Metrics), y = 0, hjust = 0, angle = 90,
            position = position_dodge2(width = 1, preserve = "single"))+
  theme_bw()+
  scale_fill_manual(values = c("lightblue", "grey", "red"), 
                    labels = c("increase", "non-significant", "decrease"),
                    name = "Direction of impact")+
  scale_y_continuous( name = "Drought Impact Level (R2)") + 
  scale_x_discrete(name = NULL) + theme(axis.text.x = element_blank())


#plot of coefficients
ggplot(filter(AnnIm4, Metrics != "Temperature", Metrics != "Zoops"), aes(x=Metrics)) +
  geom_col(aes(y = Est, fill = colr))+
  geom_text(aes(x = Metrics, label =  Metrics), y = 0, hjust = 0, angle = 90,
            position = position_dodge2(width = 1, preserve = "single"))+
  theme_bw()+
  scale_fill_manual(values = c("lightblue", "grey", "red"), 
                    labels = c("increase", "non-significant", "decrease"),
                    name = "Direction of impact")+
  scale_y_continuous( name = "Drought Impact Level (Coeficient)") + 
  scale_x_discrete(name = NULL) + theme(axis.text.x = element_blank(), legend.position = c(0.85,0.85))

#let's try varying alpha by R2
ggplot(filter(AnnIm4, Metrics != "Temperature", Metrics != "Zoops"), aes(x=Metrics)) +
  geom_col(aes(y = Est, fill = sig, alpha = Rs))+
  geom_text(aes(x = Metrics, label =  Metrics), y = 0, hjust = 0, angle = 90,
            position = position_dodge2(width = 1, preserve = "single"))+
  theme_bw()+
  scale_fill_manual(values = c("blue", "black"), 
                    labels = c("p<0.05", "non-significant"),
                    name = "Significance")+
  scale_alpha(range = c(0.3, 1), name = "R-squared")+
  scale_y_continuous( name = "Drought Impact Level (Coeficient)") + 
  scale_x_discrete(name = NULL) + 
  theme(axis.text.x = element_blank(), 
        legend.position = c(0.85,0.8))


########################################################
#compare zooplankton data from Status and Trends to what Arthur put together

SNTzoop = read.csv("data/StatusandTrendsZoopBPUE.csv")

#totals by season and year
SNTzoop2 = group_by(SNTzoop, quarter, qyear) %>%
  summarize(Zoop_BPUE_mg2 = sum(bpue_mg), logzoopB2 = log(Zoop_BPUE_mg2)) %>%
  mutate(Season = factor(quarter, levels = c("Q1", "Q2", "Q3", "Q4"),
         labels = c("Winter", "Spring", "Summer", "Fall"))) %>%
  rename(Year = qyear)

#make some quick plots of fall abundance
ggplot(filter(Integrated_data_set, Season == "Fall"), aes(x = Index, y = szn_CPUE))+
  geom_point()
ggplot(filter(Int, Season == "Fall"), aes(x = Index, y = logzoopB))+
  geom_point()

#subset fall zooplankton and plot by region
SNTfall = SNTzoop%>%
  mutate(Season = factor(quarter, levels = c("Q1", "Q2", "Q3", "Q4"),
                         labels = c("Winter", "Spring", "Summer", "Fall"))) %>%
  mutate(Region = factor(region, levels = c("spl", "ss", "dt"), 
                         labels =c("San Pablo", "Suisun", "Delta"))) %>%
  rename(Year = qyear) %>%
  filter(Season == "Fall") %>%
  merge(yrs)

#plot zoops by region
ggplot(filter(SNTfall, Drought != "N", !is.na(region)), 
       aes(x= Drought, y = log(bpue), fill = Drought)) + geom_boxplot()+
  facet_grid(~Region)

ggplot(filter(SNTfall, Drought != "N", !is.na(region)), 
       aes(x= Drought, y = log(cpue+1))) + geom_boxplot()+
  facet_grid(~region)

#make a bar plot instead
STNmeans = group_by(SNTfall, Drought, Region) %>%
  summarize(bpuem = mean(bpue, na.rm = T), sdbpue = sd(bpue, na.rm = T), 
            se = sdbpue/4)


ggplot(filter(STNmeans, Drought != "N", !is.na(Region), Region != "San Pablo"), 
       aes(x= Drought, y = bpuem, fill = Drought)) + geom_col()+ 
  #geom_errorbar(aes(ymin = bpuem - se, ymax = bpuem + se, group = Drought))+
  facet_grid(~Region) + theme_bw() + 
  scale_x_discrete(labels = c("Multi-Year \nDrought",  "Multi-Year \nWet"))+
  ylab("Biomass of Zooplankton per Meter Squared")


#now upload the SNTs chlorophyll data and make a quick graph

SNTchl = read.csv("data/WQtimeseries.csv")


#subset fall zooplankton and plot by region
SNTfallchl = SNTchl%>%
  mutate(Season = factor(quarter, levels = c("Q1", "Q2", "Q3", "Q4"),
                         labels = c("Winter", "Spring", "Summer", "Fall"))) %>%
  mutate(Region = factor(region, levels = c("spl", "ss", "dt"), 
                         labels =c("San Pablo", "Suisun", "Delta"))) %>%
  rename(Year = qyear) %>%
  filter(Season == "Fall", AnalyteName == "chla") %>%
  merge(yrs)

#plot chl by region
ggplot(filter(SNTfallchl, Drought != "N", !is.na(region)), 
       aes(x= Drought, y = Result, fill = Drought)) + geom_boxplot()+
  facet_grid(~Region)


#make a bar plot instead
STNmeanschl = group_by(SNTfallchl, Drought, Region) %>%
  summarize(chlm = mean(Result, na.rm = T), sdbpue = sd(Result, na.rm = T), 
            se = sdbpue/4)

ggplot(filter(STNmeanschl, Drought != "N", Region %in% c("Suisun", "Delta")), 
       aes(x= Drought, y = chlm, fill = Drought)) + geom_col()+
  facet_grid(~Region)+theme_bw() + 
  scale_x_discrete(labels = c("Multi-Year \nDrought",  "Multi-Year \nWet"))+
  ylab("Chlorophyll ug/L")



zoopsfall = filter(IntLong, Metric == "logzoopB", Season == "Fall")
zoops = filter(IntLong, Metric == "logzoopB")
ggplot(zoopsfall, aes(x = Drought, y = Value)) + geom_boxplot()
ggplot(zoops, aes(x = Drought, y = Value)) + geom_boxplot()+ facet_wrap(~Season)

zootest = select(Integrated_data_set, Year, Season, Drought, Zoop_CPUE, Zoop_BPUE_mg) %>%
  filter(Season == "Fall")
zootest2 = merge(zootest, zoops2)

#Wow. Zooplankton is waaaay less abundant in the LSZ, but not other place in the Delta. 
#But maybe that's because I had mysids in the Status and Trends dataset. 

zootest3 = merge(zoops, SNTzoop2)
ggplot(zootest3, aes(x = Value, y = logzoopB2, color = Season)) + geom_point()

ggplot(zootest3, aes(x= Drought, y = logzoopB2)) + facet_wrap(~Season) + geom_boxplot()
#so that's everywhere

#now look at just suisun
Suisun = filter(SNTzoop, region == "ss", qyear >1974) %>%
  group_by(quarter, qyear) %>%
  summarize(Zoop_BPUE_mg2 = sum(bpue_mg), logzoopB2 = log(Zoop_BPUE_mg2)) %>%
  mutate(Season = factor(quarter, levels = c("Q1", "Q2", "Q3", "Q4"),
                         labels = c("Winter", "Spring", "Summer", "Fall"))) %>%
  rename(Year = qyear)%>%
  left_join(zoops) 


ggplot(filter(Suisun, Drought != "N"), aes(x= Drought, y = logzoopB2)) + facet_wrap(~Season) + geom_boxplot()
#So zooplankton are lower in summer and fall in suisun, but just summer and fall.

dist = read_excel("data/distribution_matrix.xlsx", na = "NA")
dist_long = pivot_longer(dist, cols = 3:ncol(dist), names_to = "taxa", 
                         values_to= "distance") %>%
rename(Year = water_year)%>%
  left_join(yrs)


DistDI = group_by(dist_long, Season, taxa, Drought) %>%
  summarize(distance = mean(distance, na.rm = T)) %>% 
  pivot_wider(names_from = Drought, values_from = distance) %>%
  mutate(Index = (D-W)/mean(c(D,N, W), na.rm = T)) %>%
  mutate(colr = case_when(
    Index >0 ~ "red",
   Index <0 ~ "blue",
    Index >0 ~ "blue",
    Index <0 ~ "red",
  ))



ggplot(DistDI, aes(x=taxa)) +
  geom_col(aes(fill = colr, y = Index, alpha = Index), 
           position =position_dodge2(width = 1, preserve = "single"))+
  #geom_text(aes(label = Metric, y = Index2), position = position_dodge(.9))+
  scale_fill_manual(values = c("blue", "red"), labels = c("Westward", "Eastward"),
                    name = "Change in center \nof distribution")+
  geom_text(aes(x = taxa, y = 0, label = taxa), hjust = 0, angle = 90,
            position = position_dodge2(width = 1, preserve = "single"))+
  theme_bw()+ facet_grid(.~Season, space = "free") + scale_alpha(guide = NULL) +
  ylab("Drought shift (Km)")+ theme(axis.text.x = element_blank())

#####################################################################
#Look at zoops annually by region
yrs2 = select(yrs, Year, Drought, Index, Yr_type) %>%
  distinct()
zoopReg = zoopsBPUE_regional %>%
  rename(Year = water_year) %>%
  left_join( yrs2) %>%
  filter(Drought %in% c("D", "W"), Region != "North")
ggplot(zoopReg, aes(x = Drought, y = log(BPUE_ug), fill = Drought)) + geom_boxplot() + facet_grid(.~Region) + 
  theme_bw() + scale_x_discrete(labels = c("Multi-Year \nDrought", "Multi-Year \nWet"))+
  ylab("Log Zooplankton Biomass Per Unit Volume")

z1 = lm(log(BPUE_ug)~ Drought*Region, data = zoopReg)
summary(z1)
library(emmeans)
emmeans(z1, pairwise ~ Drought:Region)

zoops = zoopsBPUE_seasonal %>%
  left_join( yrs2) %>%
  filter(Drought %in% c("D", "W"), Season != "Winter")
ggplot(zoops, aes(x = Drought, y = log(szn_BPUE), fill = Drought)) + geom_boxplot() + 
  facet_grid(.~Season) + 
  theme_bw() + scale_x_discrete(labels = c("Multi-Year \nDrought", "Multi-Year \nWet"))+
  ylab("Log Zooplankton Biomass Per Unit Volume")

#Now look at it by taxa, season, region, etc.
load("data/Taxon_drought.RData")
taxa_AR = rename(taxa_annual_reg, Year = water_year) %>%
  left_join(yrs2) %>%
  filter(Drought %in% c("W", "D"), Region != "North") 
  
ggplot(taxa_AR, aes(x = Drought, y = log(BPUE_ug + 1), fill = Taxlifestage)) +
  geom_boxplot()+ facet_grid(Taxlifestage~Region, scales = "free_y")
