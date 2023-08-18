#Look at some of the big-picture differences between wet and dry years
library(tidyverse)
library(readxl)
library(viridis)
library(DroughtData)
library(lubridate)
library(emmeans)

#Boz's integrated data set

#load("C:/Users/rhartman/OneDrive - California Department of Water Resources/Drought/WQ-LT-Publication/outputs/NutrientsChlorophyll.RData")
Nuts = readRDS("C:/Users/rhartman/OneDrive - California Department of Water Resources/Drought/WQ-LT-Publication/data/lt_avg_nutr.rds")
Chl = readRDS("C:/Users/rhartman/OneDrive - California Department of Water Resources/Drought/WQ-LT-Publication/data/lt_avg_chla.rds")
WQ = readRDS("C:/Users/rhartman/OneDrive - California Department of Water Resources/Drought/WQ-LT-Publication/data/lt_avg_wq_meas.rds")

wqnuts = left_join(Nuts, Chl) %>%
  left_join(WQ) %>%
  mutate(LogChl = log(Chlorophyll))

#Fish indecies
Fish <- read_excel("data/Integrated data set.xlsx", na = "NA") %>%
  mutate(Season = factor(Season, levels = c("Winter", "Spring", "Summer", "Fall")),
         SmeltIndex = as.numeric(SmeltIndex)) %>%
  dplyr::select(Year, Season, Sbindex, SmeltIndex, LongfinIndex, AmShadIndex, TFSindex) %>%
  rename(YearAdj = Year) %>%
  mutate(logDS = log(SmeltIndex +1), logShad = log(AmShadIndex +1), 
logSB = log(Sbindex+1), logLFS = log(LongfinIndex+1), logTFS = log(TFSindex))


#stick with FMWT indicies
Fishfall = filter(Fish, Season == "Fall") %>%
  pivot_longer(cols = c(Sbindex, SmeltIndex, LongfinIndex, AmShadIndex, logDS, logShad, logSB, logLFS, TFSindex, logTFS), 
               names_to = "Metric", values_to = "Value")

#quick graph of FMWT DS index for another reason...
dsfall = filter(Fishfall, Metric == "SmeltIndex")
ggplot(dsfall, aes(x = YearAdj, y = Value)) + geom_col()+
  geom_text(data = filter(dsfall, YearAdj >2011), aes(y = Value +10, label = Value), vjust = 0)+
  ylab("FMWT Delta Smelt Index")+ xlab("Year")+
  annotate("text", x = 1974, y = 1, label = "No Index", angle = 90, hjust =0)+
  annotate("text", x = 1979, y = 1, label = "No Index", angle = 90, hjust =0)+
  theme_bw()


yrs = read_csv("data/yearassignments.csv") 


#grab zooplankton data from Arthur
zoopsBPUE_seasonal = read_csv("data/zoop_drought_lt_bpue_szn.csv") %>%
  rename(YearAdj = water_year, ZoopBPUE = s_BPUE)
zoopsBPUE_regional = read_csv("data/zoop_drought_lt_bpue_reg.csv") %>%
  rename(YearAdj = water_year,  ZoopBPUE = r_BPUE) %>%
  mutate(Value = log(ZoopBPUE), Metric = paste(Region, "logZoopBPUE", sep = "_"))
  
#Chlorophyll data
Chl2 = select(wqnuts, Season, Region, YearAdj, Chlorophyll, LogChl) %>%
  group_by(YearAdj, Region) %>%
  summarize(Chl = mean(Chlorophyll, na.rm = T), logChl = log(Chl)) %>%
  mutate(Metric = paste(Region, "logChl", sep = "_"), Value = logChl)


Nuts = group_by(wqnuts, YearAdj) %>%
  summarize(Nitrate = mean(DissNitrateNitrite, na.rm = T), logNat = log(Nitrate),
            Ammonia = mean(DissAmmonia, na.rm = T), logAm = log(Ammonia), Phos = mean(DissOrthophos, na.rm =t),
            logPhos = log(Phos)) %>%
            pivot_longer(cols = c(Nitrate, logNat, Ammonia, logAm, Phos, logPhos), names_to = "Metric", values_to = "Value")


#I need updatd secchi, temperature data from Bos. 
#bring in other hydrology
hyro = pivot_longer(lt_avg_hydro, cols = c(Outflow, Export, X2), names_to = "Metric", values_to = "Value") %>%
  group_by(YearAdj,Metric) %>%
          summarize (Value = mean(Value))



#water quality is pretty consistant, both seasonally and regionally. Let's just do the annual mean
WQreg = pivot_longer(wqnuts, cols = c(Temperature, Salinity, Secchi),
                     names_to = "Metric", values_to = "Value") %>%
  group_by(YearAdj, Metric) %>%
  summarize(Value = mean(Value, na.rm = T))


load("data/ResidenceTime.RData")
RTlong = ungroup(DFRTann) %>%
  pivot_longer(cols = c(SACRT, SJRT), names_to = "Metric", values_to = "Value") %>%
  rename(YearAdj = WY) %>%
  dplyr::select(YearAdj, Metric, Value)

#Now add salmon
salmon = read_excel("data/Grandtab WR FR SR CRR.xlsx", sheet = "salmonforR")
#we want to associate CRR with migration year, not return year
salmon2 = salmon %>% #mutate(salmon, Year = Year-2) %>% 
  pivot_longer(cols = c(`WR CRR`, `SR SR CRR`, `CV SR CRR`,`CV FR CRR`,       
                        `SR FR CRR`, `Hatchery FR CRR`, `Wild SR FR CRR`), names_to = "Metric", values_to = "Value")

ggplot(filter(salmon2, !is.na(Migration)), aes(x = Year, y = Value, fill = Migration))+ geom_col()+
  facet_wrap(~Metric) + scale_fill_manual(values = c("#FDE333","#53CC67","#00588B"), guide = NULL)+
theme_bw() 
ggplot(filter(salmon2, !is.na(Migration)), aes(x = Migration, y = Value, fill = Migration))+ geom_boxplot()+
  facet_wrap(~Metric)+ scale_fill_manual(values = c("#FDE333","#53CC67","#00588B"), guide = NULL)+
  theme_bw()

#Can I make an "all salmon" metric?
yrs = read_csv("data/yearassignments.csv")
salsum = group_by(salmon2, Year, Migration) %>%
  filter(!Metric %in% c("Hatchery FR CRR", "SR FR CRR")) %>%
  summarise(CRR = mean(Value, na.rm = T)) %>%
  filter(!is.na(CRR)) %>%
  mutate(Metric = "Salmon CRR", Value = CRR, YearAdj = Year)

ggplot(salsum, aes(x = Migration, y = CRR, fill = Migration))+ geom_boxplot()+
  scale_fill_manual(values = c("#FDE333","#53CC67","#00588B"), guide = NULL)+
  theme_bw()

salsum = select(salsum, YearAdj,Metric, Value)


#Bind them together
#Combine
IntLong = bind_rows(Fishfall, Chl2, zoopsBPUE_regional, 
                    RTlong, hyro, Nuts, WQreg, salsum) %>%
  dplyr::select(YearAdj, Metric, Value) %>%
  rename(Year = YearAdj) %>%
  filter(!is.na(Metric))


Int2 = left_join(IntLong, yrs) %>%
  mutate(Yr_type = factor(Yr_type, levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet")),
         MetricL = factor(Metric, levels =  c("Outflow", "Export", "X2","SACRT", "SJRT", "Salinity",  "Secchi","Temperature" ,
                                              "logNat", "logAm", "logPhos",
                                              "Confluence_logChl", 
                                            "North_logChl", "SouthCentral_logChl", "Suisun Bay_logChl",
                                           "Confluence_logZoopBPUE","SouthCentral_logZoopBPUE","Suisun Bay_logZoopBPUE", 
                                           "Suisun Marsh_logZoopBPUE",              
                                 "logSB", "logDS", "logLFS", "logShad",  "logTFS","Salmon CRR"), 
                                labels = c("Outflow", "Exports", "X2", "Sacramento Res. Time", 
                                           "San Joaquin Res. Time",
                                           "Salinity",  "Secchi Depth",
                                           "Temperature" , "Nitrate + Nitrite", "Ammonium", "Orthophosphate",
                                           "Chl. Confluence", 
                                           "Chl. North", "Chl. SouthCentral", 
                                           "Chl. Suisun Bay",
                                           "Zoops Confluence","Zoops SouthCentral",
                                           "Zoops Suisun Bay", 
                                           "Zoops Suisun Marsh",              
                                           "Striped Bass", "Delta Smelt", 
                                           "Longfin Smelt", 
                                           "American Shad", "Threadfin Shad", "Salmon CRR")))

Int3a = filter(Int2, !is.na(MetricL))
##############################################################################################################

#save teh data so I can just make the graphs quickly
#save(Int3a, Int2, file = "data/Integrateddata.Rdata")
#save(Int3a, Int2, file = "data/Integrateddata2023.Rdata")
#load("data/Integrateddata2023.Rdata")

#nick asked for a subset of the data
Drought4Nick = filter(Int2, Metric %in% c("logNat", "Nitrate", "Ammonia", "logAm", "Phos", "logPhos", "Temperature", 
                                          "Secchi", "Salinity"))

save(Int2, Drought4Nick, file = "data/DroughtNick.Rdata")

#look at it without the "not drought or wet" years
ggplot(filter(Int3a, Drought != "N"), aes(x = Drought, y = Value)) + geom_boxplot() +
  facet_wrap(MetricL~., scales = "free_y")

ggplot(Int3a, aes(x = Drought, y = Value, fill = Drought)) + geom_boxplot() +
  facet_wrap(MetricL~., scales = "free_y")+ drt_color_pal_drought()+
  theme_bw()+ theme(legend.position = "none")

ggsave("plots/AllParams.tiff", device = "tiff", width = 9, height = 8, units = "in")

ggplot(Int3a, aes(x = Yr_type, y = Value, fill = Yr_type), alpha = 0.3) + geom_boxplot() +
  facet_wrap(MetricL~., scales = "free_y") + drt_color_pal_yrtype()+
  theme_bw()

ggplot(Int3a, aes(x = as.factor(DroughtYear), y = Value, fill = as.factor(DroughtYear))) + geom_boxplot() +
  facet_wrap(MetricL~., scales = "free_y")

################################################################################
#average zooplankton biomass in Suisun for wet versus dry years
zoops = filter(Int3a, Metric == "Suisun Bay_logZoopBPUE") %>%
  group_by(Yr_type) %>%
  summarize(mean = mean(Value), mean2 = exp(mean))


###################################################################################
#Fish were the only interesting ones in the "drought year" plot

FishDrought = filter(Int3a, Metric %in% c("logSB", "logDS", "logShad", "logLFS", "Salmon CRR", "logTFS")) %>%
  mutate(DroughtYear = factor(DroughtYear, levels = c("0", "1", "2", "3+"), ordered = F),
         DY = as.numeric(DroughtYear))

save(FishDrought, file = "FishDrough.RData")

ggplot(FishDrought, aes(x = as.factor(DroughtYear), y = Value, fill = as.factor(DroughtYear))) + geom_boxplot() +
  facet_wrap(MetricL~., scales = "free_y")+ theme_bw()+
  scale_fill_brewer(palette = "Dark2", name = "Year of Drought")

#quick stats on this 
SB = filter(FishDrought, Metric == "logSB")
FD1 = lm(Value ~ DY + Year, data = SB)
summary(FD1)
emmeans(FD1, pairwise ~ DroughtYear)
plot(FD1)
#Need the year term to account for change over time. BUt it works!
LFS = filter(FishDrought, Metric == "logLFS")
LFS1 = lm(Value ~ DroughtYear + Year, data = LFS)
summary(LFS1)
plot(LFS1)
emmeans(LFS1, pairwise ~ DroughtYear)
#longfin smelt highly significant. But it's all wet versus dry, not between multiple dry years

DS = filter(FishDrought, Metric == "logDS")
DS1 = lm(Value ~ DroughtYear + Year, data = DS)
summary(DS1)
plot(DS1)
#no effect of multiple drought years on Delta Smelt


shad = filter(FishDrought, Metric == "logShad")
shad1 = lm(Value ~ DroughtYear + Year, data = shad)
summary(shad1)
emmeans(shad1, pairwise ~ DroughtYear)
plot(shad1)
#All the effect is dry versus wet years, not multiple years of drought

CRRs = filter(FishDrought, Metric == "Salmon CRR")
ggplot(CRRs, aes(x=Year, y = Value))+ geom_point()+ geom_smooth()

#there isn't really a chance over time, so I won'd include yera
CRRs1 = lm(Value ~ DroughtYear, data = CRRs)
summary(CRRs1)
plot(CRRs1)
emmeans(CRRs1, pairwise ~ DroughtYear)
#The difference really shows up in 3 or more years of drought.
#not too bad!
save(FishDrought, file ="FishDrough.RData")
#####################################################################################
# rework it so residence time is a column

Int3 = left_join(ungroup(Int3a), dplyr::select(rename(ungroup(DFRTann), Year = WY), Year, SACRT, SJRT))

ggplot(Int3, aes(x = SACRT, y = Value)) +
  geom_point(aes(color = Yr_type))+
  drt_color_pal_yrtype(aes_type = "color")+
  geom_smooth()+
  facet_wrap(~MetricL, scales = "free_y")+
  theme_bw()



# Now do outflow

Int4 = rename(lt_avg_hydro, Year = YearAdj) %>%
   dplyr::select( Year, Outflow) %>%
  group_by(Year) %>%
  summarize(Outflow = mean(Outflow, na.rm = T)) %>%
  right_join(ungroup(Int2)) %>%
  filter(Metric != "Outflow")

ggplot(Int4, aes(x = log(Outflow), y = Value)) +
  geom_point(aes(color = Yr_type))+
  drt_color_pal_yrtype(aes_type = "color")+
  geom_smooth()+
  facet_wrap(~MetricL, scales = "free_y")+
  theme_bw()


##############################################################################################
#Let's make a rough "Drought impact" index. I"m just making this up tho.
library(effsize)

DroughtImpact = group_by(Int3a, Metric, Drought) %>%
  summarize(Mean = mean(Value, na.rm = T)) %>% 
  pivot_wider(names_from = Drought, values_from = Mean) %>%
  mutate(Index = (D-W)/mean(c(D,N, W), na.rm = T), IndexB = D/W)

#Let's try it again and use Cohen's F
DroughtImpact2 = filter(Int3a, Drought != "N", !Metric %in% c("logDS", "logShad", "logSB", "logLFS", "logTFS")) %>%
  mutate(Drought = as.factor(Drought)) %>%
  group_by(Metric) %>%
  summarize(Cohen = cohens_f(lm(Value ~ Drought))$Cohens_f, 
            #magnitude = cohen.d(Value ~ Drought, na.rm = T)$magnitude,
            Pval = summary(lm(Value ~ Drought))$coefficients[2,4],
            estimate = summary(lm(Value ~ Drought))$coefficients[2,1],
            Sig = case_when(Pval < 0.05 & Pval > 0.01 ~ "*",
                            Pval < 0.01 & Pval > 0.001 ~ "**",
                            Pval < 0.001 ~ "***",
                            Pval > 0.05 ~ "(NS)"),
            Cohen = Cohen*sign(estimate)*-1)

library(effectsize)

DSdat = filter(Int3a, Metric =="logDS") %>%
  mutate(Drought = as.factor(Drought)) %>%
  group_by(Metric) %>%
  arrange(Year) %>%
  mutate(lagValue = lag(Value)) %>%
  filter(Drought !="N")

test =  cohens_f(lm(Value ~ Drought+ Year + lagValue, data = DSdat))

#do the fish seperately (per Matt Nobriga)
DroughtImpact2f = filter(Int3a, Metric %in% c("logDS", "logShad", "logSB", "logLFS", "logTFS")) %>%
  mutate(Drought = as.factor(Drought)) %>%
  group_by(Metric) %>%
  arrange(Year) %>%
  mutate(lagValue = lag(Value)) %>%
  filter(Drought !="N") %>%
  summarize(Cohen = cohens_f(lm(Value ~ Drought+ Year + lagValue))$Cohens_f_partial[1], 
           
            Pval = summary(lm(Value ~ Drought+ Year + lagValue))$coefficients[2,4],
            estimate = summary(lm(Value ~ Drought+ Year + lagValue))$coefficients[2,1],
            Sig = case_when(Pval < 0.05 & Pval > 0.01 ~ "*",
                            Pval < 0.01 & Pval > 0.001 ~ "**",
                            Pval < 0.001 ~ "***",
                            Pval > 0.05 ~ "(NS)"),
            Cohen = Cohen*sign(estimate)*-1)

DroughtImpact2 = bind_rows(DroughtImpact2, DroughtImpact2f)

#BAR PLOTS/Arrow plows
DroughtImpact2a = mutate(DroughtImpact2, 
                         Metric = factor(Metric, levels =  c("Outflow", "Export", "X2","SACRT", "SJRT", "Salinity",  "Secchi","Temperature" ,
                                                                "logNat", "logAm", "logPhos",
                                                                  "Confluence_logChl", 
                                                                    "North_logChl", "SouthCentral_logChl", "Suisun Bay_logChl",
                                                                  "Confluence_logZoopBPUE","SouthCentral_logZoopBPUE","Suisun Bay_logZoopBPUE", 
                                                                                                       "Suisun Marsh_logZoopBPUE",              
                                                                                                       "logSB", "logDS", "logLFS", "logShad", "logTFS", "Salmon CRR"), 
                                                                                   labels = c("Outflow", "Exports", "X2", "Sacramento Res. Time", 
                                                                                              "San Joaquin Res. Time",
                                                                                              "Salinity",  "Secchi Depth",
                                                                                              "Temperature" , "Nitrate + Nitrite", "Ammonium", "Orthophosphate",
                                                                                              "Chl. Confluence", 
                                                                                              "Chl. North", "Chl. SouthCentral", 
                                                                                              "Chl. Suisun Bay",
                                                                                              "Zoops Confluence","Zoops SouthCentral",
                                                                                              "Zoops Suisun Bay", 
                                                                                              "Zoops Suisun Marsh",              
                                                                                              "Striped Bass", "Delta Smelt", 
                                                                                              "Longfin Smelt", 
                                                                                              "American Shad", "Threadfin Shad", "Salmon CRR"))) %>%
  filter(!is.na(Metric))

ggplot(DroughtImpact2a, aes(x = Metric, y = Cohen, fill = Sig)) + geom_col() +
  ylab("Drought Effect Size")+
  theme_bw()+
  scale_fill_viridis_d(option = "C", direction = 1, alpha = 0.6)+
  geom_text(aes(label = paste(Metric, Sig), y = Cohen + 0.1), angle = 90, hjust = 0)+
  coord_cartesian(ylim = c(-2.8, 5))+ xlab(NULL)+
  scale_x_discrete(label = NULL)

DroughtImpact2a = mutate(DroughtImpact2a, yval = case_when(Cohen< 0 ~ 0.1,
                                                           TRUE ~ -Cohen + 0.1))
write.csv(DroughtImpact2a, "DroughtImpact.csv", row.names = F)
save(DroughtImpact2a, Int3a,  file = "outputs/DroughtImpac.RData")
#load("outputs/DroughtImpac.RData")

##Arrow plot with all things, even the non-significant ones
ggplot(DroughtImpact2a, aes(x = Metric, y = 0)) + 
  geom_segment(aes(xend = Metric, yend = Cohen, color = Sig), 
               arrow = arrow(length = unit(0.2, "inches")),
               size = 2) +
  ylab("Drought Effect Size (Cohen's F)")+ xlab(NULL)+
 # scale_color_viridis_d(option = "C", direction = -1, name = "Magnatude of\nEffect")+
  theme_bw()+
  scale_color_viridis_d(option = "C", direction  = -1, labels = c("(NS) Non-Significant", "* P<0.05", "** P<0.01", "*** P<0.001"), 
                     name = "Significance")+
  geom_text(aes(label = paste(Metric, Sig), y = -yval+0.2), angle = 90, hjust = 0)+
  coord_cartesian(ylim = c(-1.5, 2.5))+
  theme(axis.text.x = element_blank(), legend.position = "right", legend.direction = "vertical")

ggsave("plots/EffectArrows2023.tiff", device = "tiff", width = 9, height = 7, units = "in")

######################################################################################
#now try and do the origional graph but highlight which are significant
impacts =rename(DroughtImpact2a, MetricL = Metric) %>%
  arrange(MetricL) %>%
  mutate(Y = c(75000, 8000, 90, 60, 120, 5, 90, 17.5, -0.7, -1.9, -2.1, 2, 2.2, 3.1, 2.4, 9.5, 10.5, 10, 10.5, 9, 7, 10, 8.5, 9, 4)) %>%
  filter(Sig != "(NS)")

ggplot(Int3a, aes(x = Drought, y = Value, fill = Drought)) + geom_boxplot() +
  geom_text(data = impacts, x = 1, aes(label = Sig, y = Y), inherit.aes = F, size = 10, color = "red")+
  facet_wrap(MetricL~., scales = "free_y")+ drt_color_pal_drought()+
  theme_bw()+ theme(legend.position = "none")


ggsave("plots/AllParamssig2023.tiff", device = "tiff", width = 9, height = 8, units = "in")

#mapps for zooplankton and chlorophyll

library(sf)
library(deltamapr)
load("DroughtRegions.RData")
#I need to check whether the units are C/L or C/m3
zoops = filter(DroughtImpact2a, Metric %in% c("Zoops Confluence", 
                                              "Zoops Suisun Bay", 
                                              "Zoops Suisun Marsh",
                                              "Zoops SouthCentral" ))%>%
  mutate(Region = case_when(Metric == "Zoops SouthCentral" ~ "SouthCentral",
                            Metric == "Zoops Confluence" ~ "Confluence",
                            Metric == "Zoops Suisun Marsh" ~ "Suisun Marsh",
                            Metric == "Zoops Suisun Bay" ~ "Suisun Bay"))

zoopRegions = left_join(Regions, zoops) %>%
  st_transform(crs = 4269) %>%
  mutate(Metric = "Zooplankton")

ggplot()+ 
  geom_sf(data = zoopRegions, aes(fill = estimate)) +
  geom_sf(data = WW_Delta, alpha = 0.5)

ggplot()+ 
  geom_sf(data = filter(zoopRegions, Region != "North"), aes(fill = Cohen)) +
  geom_sf(data = WW_Delta, alpha = 0.2) + theme_bw()+
  scale_fill_continuous(name = "Drought \nImpact")+
  ggtitle("Zooplankton Biomass")+
  coord_sf(xlim = c(-122.2, -121.2), ylim = c(37.8, 38.4))


############################################
#chlorophyll map

Chl = filter(DroughtImpact2a, Metric %in% c("Chl. Suisun Bay", 
                                              "Chl. SouthCentral", "Chl. North",
                                              "Chl. Confluence")) %>%
  mutate(Region = case_when(Metric == "Chl. SouthCentral" ~ "SouthCentral",
                            Metric == "Chl. Confluence" ~ "Confluence",
                            Metric == "Chl. North" ~ "North",
                            Metric == "Chl. Suisun Bay" ~ "Suisun Bay"))

chRegions = left_join(Regions, Chl) %>%
  st_transform(crs = 4269) %>%
  mutate(Metric = "Chlorophyll")

ggplot()+ 
  geom_sf(data = chRegions, aes(fill = estimate)) +
  geom_sf(data = WW_Delta, alpha = 0.5)

ggplot()+ 
  geom_sf(data = filter(chRegions, Region != "Suisun Marsh"), aes(fill = Cohen)) +
  geom_sf(data = WW_Delta, alpha = 0.2) + theme_bw()+
  scale_fill_continuous(name = "Drought \nImpact")+
  ggtitle("Chlorophyll")+
  coord_sf(xlim = c(-122.2, -121.2), ylim = c(37.8, 38.4))


test = bind_rows(chRegions, zoopRegions)

library(sfheaders)
library(ggsn)
ggplot()+ 
  geom_sf(data = test, aes(fill = Cohen)) +
  geom_sf(data = WW_Delta, alpha = 0.2) + theme_bw()+
  scale_fill_viridis(name = "Drought \nImpact", na.value = "grey90")+
  facet_wrap(~Metric, nrow = 2)+
  coord_sf(xlim = c(-122.2, -121.2), ylim = c(37.8, 38.45))+
  north(x.min = -122.1, x.max = -121.2, y.min = 37.8, y.max = 38.4, scale = .15)+
  scalebar(data = test, st.dist = .03,
           transform = T, dist = 10, dist_unit = "km", location = "bottomleft",
           facet.var = "Metric", facet.lev = "Chlorophyll", st.bottom = FALSE)+xlab(NULL)+ ylab(NULL)

ggsave("CHlZoopsmap.tiff", device = "tiff", height = 8, width = 6, units = "in")
#################################################################

# #now try combining not-drought and wet years
# DroughtImpact2b = mutate(Int2, Drought2 = case_when(Drought == "D" ~ "D",
#                                                    TRUE ~ "W"),
#                         Drought2 = as.factor(Drought2)) %>%
#   group_by(Metric) %>%
#   summarize(Cohen = cohen.d(Value ~ Drought2, na.rm = T)$estimate, 
#             magnitude = cohen.d(Value ~ Drought2, na.rm = T)$magnitude)
# 
# ggplot(DroughtImpact2b, aes(x = Metric, y = 0)) + 
#   geom_segment(aes(xend = Metric, yend = -Cohen, color = Cohen), 
#                arrow = arrow(length = unit(0.3, "inches")),
#                size = 2) +
#   ylab("Drought Effect Size (Cohen's D)")+ xlab(NULL)+
#   scale_color_viridis(option = "E", guide = NULL)+
#   theme_bw()+
#   geom_text(aes(label = Metric, y = -Cohen), angle = 90, hjust = 0)+
#   coord_cartesian(ylim = c(-2.8, 5))+
#   theme(axis.text.x = element_blank())


#what about the slope of the residence time line?
Int3 = filter(Int3, !is.na(Value), !is.nan(Value))
Resmetric = filter(Int3, !is.na(Value), !is.nan(Value), Metric != "SACRT") %>%
  group_by(MetricL) %>%
  summarize(intercept = coef(lm(Value ~ SACRT))[1],
            grad = coef(lm(Value ~ SACRT))[2],
            r2 = summary(lm(Value ~ SACRT))$r.squared,
            P =  summary(lm(Value ~ SACRT))$coefficients[2,4],
            Y = max(Value)) 

#############################################################################
#linear models of stuff versus residence time
ggplot(filter(Int3, Metric != "SACRT"), aes(x = SACRT, y = Value)) +
  geom_point(aes(color = Yr_type))+
  drt_color_pal_yrtype(aes_type = "color")+
  geom_smooth(method = "lm")+
  geom_text(data = Resmetric, aes(x = 50, y = Y, 
                                  label = paste("y = x", round(grad, 3),
                                                "+", round(intercept, 3), "\n R2 =", round(r2, 4),
                                                " P = ", round(P, 4), sep = "")),
            size = 3, nudge_y = -1)+
 xlab("Sacramento Residence Time (days)")+
  facet_wrap(~MetricL, scales = "free_y")+
  theme_bw()


#now versus net delta outflow

Outmetric = Int4 %>%
  group_by(MetricL) %>%
  summarize(intercept = coef(lm(Value ~ log(Outflow)))[1],
            grad = coef(lm(Value ~ log(Outflow)))[2],
            r2 = summary(lm(Value ~ log(Outflow)))$r.squared,
            P =  summary(lm(Value ~ log(Outflow)))$coefficients[2,4],
            Y = max(Value, na.rm = T)) 



ggplot(filter(Int4, !is.na(MetricL)), aes(x = log(Outflow), y = Value)) +
  geom_point(aes(color = Yr_type))+
  drt_color_pal_yrtype(aes_type = "color")+
  geom_smooth(method = lm)+
  geom_text(data = Outmetric, aes(x = 9, y = Y, 
                                  label = paste("y = x", round(grad, 3),
                                                "+", round(intercept, 3), "\n R2 =", round(r2, 4),
                                                " P = ", round(P, 4), sep = "")),
            size = 3, nudge_y = -1)+
  facet_wrap(~MetricL, scales = "free_y")+
  theme_bw()



#Verses sac valley index


Indexes = Int3a %>%
  group_by(MetricL) %>%
  summarize(intercept = coef(lm(Value ~ Index))[1],
            grad = coef(lm(Value ~ Index))[2],
            r2 = summary(lm(Value ~ Index))$r.squared,
            P =  summary(lm(Value ~ Index))$coefficients[2,4],
            Y = max(Value, na.rm = T),
            sig = case_when(P < 0.05 ~ T,
                            P > 0.05 ~ F)) 


ggplot(filter(Int3a, !is.na(MetricL)), aes(x = Index, y = Value)) +
  geom_point(aes(color = Yr_type))+
  drt_color_pal_yrtype(aes_type = "color")+
  geom_smooth(method = lm, color = "grey 50", alpha = 0.2, linetype =2)+
  geom_abline(data = Indexes, aes(intercept = intercept, slope = grad, alpha = sig), size = 1)+
  scale_alpha_manual(values = c(0.1, 1), guide = NULL)+
  # geom_text(data = Indexes, aes(x = 9, y = Y, 
  #                                 label = paste(#"y = x", round(grad, 3),
  #                                               #"+", round(intercept, 3), "\n 
  #                                               "R2 =", round(r2, 2),
  #                                               " P = ", round(P, 4), sep = "")),
  #           size = 3, nudge_y = -1)+
  facet_wrap(~MetricL, scales = "free_y")+
  theme_bw() + xlab("Sac Valley Index")

#Having the formulas on the plots is a mess. Maybe just the R2 and p-value?

ggsave("plots/WYindexRegressions.tiff", device = "tiff", width = 10, height = 10, units = "in")

write.csv(Indexes, "IndexRegressions.csv", row.names = FALSE)

#######################################################################################
#look at delta outflow (controlled by management) versus unimpaired flow

library(patchwork)
library(mgcv)
out = filter(Int3a, MetricL == "Outflow")
outgm = gam(Value ~ s(Index), data = out) 
summary(outgm)
plot(outgm)
outGAM = ggplot(filter(Int3a, MetricL == "Outflow"), aes(x = Index, y = Value)) +
  geom_point(aes(color = Yr_type))+
  drt_color_pal_yrtype(aes_type = "color")+
  geom_smooth()+
  annotate("text", x = 4, y = 75000, label = "R2 = 0.975", hjust = 0)+
  annotate("text", x = 4, y = 83000, label = "B", hjust = 0, size =10)+
  theme_bw() + xlab("Sac Valley Index") + ylab("Annual Mean Delta Outflow (CFS)")

outLM = ggplot(filter(Int3a, MetricL == "Outflow"), aes(x = Index, y = Value)) +
  geom_point(aes(color = Yr_type))+
  drt_color_pal_yrtype(aes_type = "color")+
  geom_smooth(method = "lm")+
  annotate("text", x = 4, y = 75000, label = "R2 = 0.925", hjust = 0)+
  annotate("text", x = 4, y = 83000, label = "A", hjust = 0, size =10)+
  theme_bw() + xlab("Sac Valley Index") + ylab("Annual Mean Delta Outflow (CFS)")+
  theme(legend.position = "none")

outLM + outGAM 

ggsave("plots/WYvsOUT.tiff", device = "tiff", width = 10, height = 5, units = "in")



#grab unimpaired monthly Sac flow form cdec
library(cder)
SMU = cdec_query("SMU", 65, start.date = as.Date("1970-01-01"), end.date = as.Date("2021-12-31"))
FPT = cdec_query("FTP", 66, "E", start.date = as.Date("1970-01-01"), end.date = as.Date("2021-12-31"))
DTO = cdec_query("DTO", 23, "D", start.date = as.Date("1970-01-01"), end.date = as.Date("2021-12-31"))


DTOm =  mutate(DTO, Month = month(DateTime), Year = year(DateTime)) %>%
  group_by(Month, Year) %>%
  summarise(outFlow = mean(Value, na.rm = T))

SMU = mutate(SMU, Month = month(DateTime), Year = year(DateTime)) %>%
  mutate(flowUnimparied = Value*43559.9/2.628e+6) %>%
select(Month, Year, flowUnimparied)
  

FPT = mutate(FPT, Month = month(DateTime), Year = year(DateTime)) %>%
  select(Month, Year, Value)%>%
  mutate(flowSac = Value*43559.9/2.628e+6)

DF2 = mutate(DF, Month = month(Date), Year = year(Date)) %>%
  filter(OUT >0) %>%
  group_by(Month, Year) %>%
  summarize(OUT = mean(OUT,na.rm = T))

flows = left_join(SMU, FPT) %>%
  left_join(DTOm) %>%
  left_join(DF2)

ggplot(flows, aes(x = log(flowUnimparied), y = log(OUT))) + geom_point(aes(color = Year))+geom_smooth()+
  ylab("Delta Outflow (log-transformed)") + xlab("Sac River Unimpared flow (log-transformed)") + theme_bw()+
  scale_color_viridis_b(option = "A") + facet_wrap(~Month, scales = "free")
#monthly time step does not look as good

ggplot(flows, aes(x = log(outFlow), y = log(OUT))) + geom_point(aes(color = Year))+geom_smooth()+
  ylab("Sac River flow (log-transformed)") + xlab("Sac River Unimpared flow (log-transformed)") + theme_bw()+
  scale_color_viridis_b(option = "A")

##############################################################
#zooplankton map with slope of outflow relatinoship

zoopsOut = filter(Outmetric, MetricL %in% c("Zooplankton\nSouthCentral (ugC/L)", 
                                              "Zooplankton\nSuisun Marsh  (ugC/L)", "Zooplankton\nConfluence (ugC/L)",
                                              "Zooplankton\nSuisun Bay  (ugC/L)")) %>%
  mutate(Region = case_when(MetricL == "Zooplankton\nSouthCentral (ugC/L)" ~ "SouthCentral",
                            MetricL == "Zooplankton\nConfluence (ugC/L)" ~ "Confluence",
                            MetricL == "Zooplankton\nSuisun Marsh  (ugC/L)" ~ "Suisun Marsh",
                            MetricL == "Zooplankton\nSuisun Bay  (ugC/L)" ~ "Suisun Bay"))

zoopRegionsOut = left_join(Regions, zoopsOut) %>%
  mutate(Metrictype = "Zooplankton")

ggplot()+ 
  geom_sf(data = filter(zoopRegionsOut, Region != "North"), aes(fill = grad)) +
  geom_sf(data = WW_Delta, alpha = 0.5) + theme_bw()+
  scale_x_continuous(limits = c(-122.2, -121.2)) +
  scale_y_continuous( limits = c(37.65, 38.5))+
  scale_fill_gradient2(name = "Slope of Outflow \n Relationship", low = "skyblue", high = "darkred", mid = "white")+
  ggtitle("Zooplankton")

ggplot()+ 
  geom_sf(data = filter(zoopRegions, Region != "North"), aes(fill = Cohen*-1)) +
  geom_sf(data = WW_Delta, alpha = 0.2) + theme_bw()+
  scale_x_continuous(limits = c(-122.2, -121.2)) +
  scale_y_continuous( limits = c(37.65, 38.5))+
  scale_fill_continuous(name = "Drought Effect \nSize (Cohen's D)")




ChlOut = filter(Outmetric, MetricL %in% c("Chlorophyll\nConfluence (log(ug/L))", 
                                          "Chlorophyll\nNorth (log(ug/L))", "Chlorophyll\nSouthCentral (log(ug/L))",
       "Chlorophyll\n Suisun Bay (log(ug/L))")) %>%
 mutate(Region = case_when(MetricL == "Chlorophyll\nSouthCentral (log(ug/L))" ~ "SouthCentral",
           MetricL == "Chlorophyll\nConfluence (log(ug/L))" ~ "Confluence",
            MetricL == "Chlorophyll\nNorth (log(ug/L))" ~ "North",
             MetricL == "Chlorophyll\n Suisun Bay (log(ug/L))" ~ "Suisun Bay"))


chls = filter(DroughtImpact2a, Metric %in% c("Confluence Chla", 
                                             "South Delta Chla", "North Chla",
                                             "Suisun Bay Chla")) %>%
  mutate(Region = case_when(Metric == "South Delta Chla" ~ "SouthCentral",
                            Metric == "Confluence Chla" ~ "Confluence",
                            Metric == "North Chla" ~ "North",
                            Metric == "Suisun Bay Chla" ~ "Suisun Bay"))

chlRegionsOut = left_join(Regions, ChlOut)%>%
  mutate(Metrictype = "Chla")
chlRegions = left_join(Regions, chls)%>%
  mutate(Metrictype = "Chla")

ggplot()+ 
  geom_sf(data = filter(chlRegionsOut, Region != "Suisun Marsh"), aes(fill = grad)) +
  geom_sf(data = WW_Delta, alpha = 0.5) + theme_bw()+
  scale_x_continuous(limits = c(-122.2, -121.2)) +
  scale_y_continuous( limits = c(37.65, 38.5))+
  scale_fill_gradient2(name = "Slope of Outflow \n Relationship", low = "skyblue", high = "darkred", mid = "white")+
  
  ggtitle("Chlorophyll")

ggplot()+ 
  geom_sf(data = filter(chlRegions, Region != "Suisun Marsh"), aes(fill = Cohen*-1)) +
  geom_sf(data = WW_Delta, alpha = 0.2) + theme_bw()+
  scale_x_continuous(limits = c(-122.2, -121.2)) +
  scale_y_continuous( limits = c(37.65, 38.5))+
  scale_fill_gradient2(name = "Drought Effect \nSize (Cohen's D)", low = "darkblue", high = "darkorange", mid = "white")
  

#zoops and chlorophyll facetted

mapszoopch = bind_rows(chlRegions, zoopRegions)
Outzoopchl = bind_rows(chlRegionsOut, zoopRegionsOut)

ggplot(data = mapszoopch)+ 
  geom_sf(aes(fill = Cohen*-1)) +
  geom_sf(data = WW_Delta, alpha = 0.2) + theme_bw()+
  scale_x_continuous(limits = c(-122.2, -121.2)) +
  scale_y_continuous( limits = c(37.65, 38.5))+
  scale_fill_continuous(name = "Drought Effect \nSize (Cohen's D)", na.value = "white") +
  facet_wrap(~Metrictype)



ggplot(data = Outzoopchl)+ 
  geom_sf(aes(fill = grad)) +
  geom_sf(data = WW_Delta, alpha = 0.2) + theme_bw()+
  scale_x_continuous(limits = c(-122.2, -121.2)) +
  scale_y_continuous( limits = c(37.65, 38.5))+
  scale_fill_continuous(name = "Slope of Outflow\nRelationship", na.value = "white") +
  facet_wrap(~Metrictype, scales = "free")
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


##############################################################################

#####################################################
#NMDS!!!

library(vegan)
Intmat = pivot_wider(Int4, id_cols = c(Year, Outflow, Yr_type, Drought), names_from = "Metric", values_from = "Value")
Intmat = Intmat[which(!is.na(rowSums(Intmat[,5:23]))),]



Envmat = select(Intmat, Year, Outflow, Yr_type, Drought) %>%
  mutate(Drought = factor(Drought))


Intmat2 = Intmat[,5:23]

maxes = group_by(Int4, Metric) %>%
  summarise(Max = max(Value, na.rm = T))
  
Intmat3 = select(Intmat2, !`logChla North`)

Intmat4 = left_join(Int4, maxes) %>%
  mutate(Percent = Value/Max) %>%
  pivot_wider(id_cols = c(Year, Outflow, Yr_type, Drought), names_from = "Metric", values_from = "Percent") %>%
  select(!"logChla North")
Intmat4 = Intmat4[which(!is.na(rowSums(Intmat4[,5:22]))),]
Intmat4a = Intmat4[,5:22]

DroughtNMDS = metaMDS(Intmat3)
source("plotNMDS.R")
PlotNMDS(DroughtNMDS, group = "Drought", data = Envmat)
PlotNMDS(DroughtNMDS, group = "Yr_type", data = Envmat)

DroughtNMDS2 = metaMDS(Intmat4a)
PlotNMDS(DroughtNMDS2, group = "Drought", data = Envmat)
PlotNMDS(DroughtNMDS2, group = "Yr_type", data = Envmat)
PlotNMDS2(DroughtNMDS2, group = "Yr_type", lines = "Outflow", data = Envmat)

#################################################################
#South Central chlorophyll versus San Juaquin RT

SCC = filter(Int3, Metric == "logChla SouthCentral")

ggplot(SCC, aes(x = SJRT, y = Value)) + geom_point()+
  geom_smooth()

Chl = mutate(Chl, mYear = ds_year + (1-month)/12)
Chlorophyll = left_join(Chl, select(DFRTall, -Yr_type))
ggplot(filter(Chlorophyll,  Region =="South-Central Delta"), aes(x = SJRT, y = chlaAvg_log10))+
  geom_point(aes(color = Yr_type))+ geom_smooth()+
  drt_color_pal_yrtype(aes_type = "color")+
  theme_bw()+
  ylab("South-Central Delta chlorophyll \n (ug/L, log-transformed)")+
  xlab("San Joaquin Residence time (days)")

ggplot(filter(Chlorophyll,  Region =="South-Central Delta"), aes(y = SJRT, x = chlaAvg_log10))+
  geom_point(aes(color = Yr_type))+ geom_smooth()+
  drt_color_pal_yrtype(aes_type = "color")+
  theme_bw()+
  xlab("South-Central Delta log CHL")+
  ylab("San Joaquin Residence time (days)")

