#Look at some of the big-picture differences between wet and dry years
library(tidyverse)
library(readxl)
library(viridis)
library(DroughtData)
library(lubridate)

#Boz's integrated data set

load("C:/Users/rhartman/OneDrive - California Department of Water Resources/Drought/WQ-LT-Publication/NutrientsChlorophyll.RData")

#Fish indecies
Fish <- read_excel("data/Integrated data set.xlsx", na = "NA") %>%
  mutate(Season = factor(Season, levels = c("Winter", "Spring", "Summer", "Fall")),
         SmeltIndex = as.numeric(SmeltIndex)) %>%
  dplyr::select(Year, Season, Sbindex, SmeltIndex, LongfinIndex, AmShadIndex) %>%
  rename(YearAdj = Year) %>%
  mutate(logDS = log(SmeltIndex +1), logShad = log(AmShadIndex +1), 
logSB = log(Sbindex+1), logLFS = log(LongfinIndex+1))

#stick with FMWT indicies
Fishfall = filter(Fish, Season == "Fall") %>%
  pivot_longer(cols = c(Sbindex, SmeltIndex, LongfinIndex, AmShadIndex, logDS, logShad, logSB, logLFS), 
               names_to = "Metric", values_to = "Value")

yrs = read_csv("data/yearassignments.csv") 

#grab zooplankton data from Arthur
zoopsBPUE_seasonal = read_csv("data/zoop_drought_lt_bpue_szn.csv") %>%
  rename(YearAdj = water_year, ZoopBPUE = s_BPUE)
zoopsBPUE_regional = read_csv("data/zoop_drought_lt_bpue_reg.csv") %>%
  rename(YearAdj = water_year,  ZoopBPUE = r_BPUE) %>%
  mutate(Value = log(ZoopBPUE), Metric = paste(Region, "logZoopBPUE", sep = "_"))
  
#Chlorophyll data
Chl2 = select(NutsCLa, Season, Region, YearAdj, Chlorophyll, LogChl) %>%
  group_by(YearAdj, Region) %>%
  summarize(Chl = mean(Chlorophyll, na.rm = T), logChl = log(Chl)) %>%
  mutate(Metric = paste(Region, "logChl", sep = "_"), Value = logChl)


Nuts = group_by(NutsCLa, YearAdj) %>%
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
WQreg = pivot_longer(lt_avg_wq, cols = c(Temperature, Salinity, Secchi),
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

salsum = group_by(salmon2, Year, Migration) %>%
  summarise(CRR = mean(Value, na.rm = T)) %>%
  filter(!is.na(CRR))

ggplot(salsum, aes(x = Migration, y = CRR, fill = Migration))+ geom_boxplot()+
  scale_fill_manual(values = c("#FDE333","#53CC67","#00588B"), guide = NULL)+
  theme_bw()


#Bind them together
#Combine
IntLong = bind_rows(Fishfall, Chl2, zoopsBPUE_regional, hyro, Nuts, WQreg, salsum) %>%
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
                                 "Sbindex", "SmeltIndex", "LongfinIndex", "AmShadIndex"), 
                                labels = c("Outflow (CFS)", "SWP + CVP \nExports (CFS)", "X2", "Sacramento\nResidence Time (days)", "San Joaquin\nResidence Time, (days)",
                                           "Salinity (PSU)",  "Secchi Depth \n(cm)",
                                           "Temperature (C)" , "logNitrate", "logAmmonia", "logPhosohorus",
                                           "Chlorophyll\nConfluence (log(ug/L))", 
                                           "Chlorophyll\nNorth (log(ug/L))", "Chlorophyll\nSouthCentral (log(ug/L))", 
                                           "Chlorophyll\n Suisun Bay (log(ug/L))",
                                           "Zooplankton\nConfluence (ugC/L)","Zooplankton\nSouthCentral (ugC/L)",
                                           "Zooplankton\nSuisun Bay  (ugC/L)", 
                                           "Zooplankton\nSuisun Marsh  (ugC/L)",              
                                           "Striped Bass \nlog FMWT Index", "Delta Smelt\nlog FMWT Index", 
                                           "Longfin  \nlog FMWT Index", 
                                           "Am Shad  \nlog FMWT Index")))

Int3a = filter(Int2, !is.na(MetricL))


#look at it without the "not drought or wet" years
ggplot(filter(Int3a, Drought != "N"), aes(x = Drought, y = Value)) + geom_boxplot() +
  facet_wrap(MetricL~., scales = "free_y")

ggplot(Int3a, aes(x = Drought, y = Value, fill = Drought)) + geom_boxplot() +
  facet_wrap(MetricL~., scales = "free_y")+ drt_color_pal_drought()

ggplot(Int3a, aes(x = Yr_type, y = Value, fill = Yr_type), alpha = 0.3) + geom_boxplot() +
  facet_wrap(MetricL~., scales = "free_y") + drt_color_pal_yrtype()+
  theme_bw()

ggplot(Int3a, aes(x = as.factor(DroughtYear), y = Value, fill = as.factor(DroughtYear))) + geom_boxplot() +
  facet_wrap(MetricL~., scales = "free_y")


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

#Let's try it again and use Cohen's D
DroughtImpact2 = filter(Int3a, Drought != "N") %>%
  mutate(Drought = as.factor(Drought)) %>%
  group_by(Metric) %>%
  summarize(Cohen = cohen.d(Value ~ Drought, na.rm = T)$estimate, 
            magnitude = cohen.d(Value ~ Drought, na.rm = T)$magnitude,
            Pval = summary(lm(Value ~ Drought))$coefficients[2,4],
            estimate = summary(lm(Value ~ Drought))$coefficients[2,1],
            Sig = case_when(Pval < 0.05 & Pval > 0.01 ~ "*",
                            Pval < 0.01 & Pval > 0.001 ~ "**",
                            Pval < 0.001 ~ "***",
                            Pval > 0.05 ~ "(NS)"))

#BAR PLOTS/Arrow plows
DroughtImpact2a = mutate(DroughtImpact2, Metric = factor(Metric, levels =  c("Outflow", "Export", "X2","SACRT", "SJRT", "Salinity",  "Secchi","Temperature" ,
                                                                                                       "logNat", "logAm", "logPhos",
                                                                                                       "Confluence_logChl", 
                                                                                                       "North_logChl", "SouthCentral_logChl", "Suisun Bay_logChl",
                                                                                                       "Confluence_logZoopBPUE","SouthCentral_logZoopBPUE","Suisun Bay_logZoopBPUE", 
                                                                                                       "Suisun Marsh_logZoopBPUE",              
                                                                                                       "Sbindex", "SmeltIndex", "LongfinIndex", "AmShadIndex"), 
                                                                                   labels = c("Outflow (CFS)", "SWP + CVP \nExports (CFS)", "X2", "Sacramento\nResidence Time (days)", "San Joaquin\nResidence Time, (days)",
                                                                                              "Salinity (PSU)",  "Secchi Depth \n(cm)",
                                                                                              "Temperature (C)" , "logNitrate", "logAmmonia", "logPhosohorus",
                                                                                              "Chlorophyll\nConfluence (log(ug/L))", 
                                                                                              "Chlorophyll\nNorth (log(ug/L))", "Chlorophyll\nSouthCentral (log(ug/L))", 
                                                                                              "Chlorophyll\n Suisun Bay (log(ug/L))",
                                                                                              "Zooplankton\nConfluence (ugC/L)","Zooplankton\nSouthCentral (ugC/L)",
                                                                                              "Zooplankton\nSuisun Bay  (ugC/L)", 
                                                                                              "Zooplankton\nSuisun Marsh  (ugC/L)",              
                                                                                              "Striped Bass \nlog FMWT Index", "Delta Smelt\nlog FMWT Index", 
                                                                                              "Longfin  \nlog FMWT Index", 
                                                                                              "Am Shad  \nlog FMWT Index"))) %>%
  filter(!is.na(Metric))

ggplot(DroughtImpact2a, aes(x = Metric, y = Cohen, fill = magnitude)) + geom_col() +
  ylab("Drought Effect Size")+
  theme_bw()+
  scale_fill_viridis_d(option = "C", direction = 1, alpha = 0.6)+
  geom_text(aes(label = paste(Metric, Sig), y = Cohen + 0.1), angle = 90, hjust = 0)+
  coord_cartesian(ylim = c(-2.8, 5))+ xlab(NULL)+
  scale_x_discrete(label = NULL)

DroughtImpact2a = mutate(DroughtImpact2a, yval = case_when(Cohen< 0 ~ 0.1,
                                                           TRUE ~ -Cohen + 0.1))
#Arrow plot with all things, even the non-significant ones
ggplot(DroughtImpact2a, aes(x = Metric, y = 0)) + 
  geom_segment(aes(xend = Metric, yend = Cohen, color = magnitude, alpha = Sig), 
               arrow = arrow(length = unit(0.2, "inches")),
               size = 2) +
  ylab("Drought Effect Size (Cohen's D)")+ xlab(NULL)+
  scale_color_viridis_d(option = "C", direction = -1)+
  theme_bw()+
  scale_alpha_manual(values = c(0.5, 0.6, 0.8, 1), 
                     labels = c("(NS) Non-Significant", "* P<0.05", "** P<0.01", "*** P<0.001"), 
                     name = "Significance")+
  geom_text(aes(label = paste(Metric, Sig), y = -yval+0.2), angle = 90, hjust = 0)+
  coord_cartesian(ylim = c(-2.8, 5))+
  theme(axis.text.x = element_blank())



#mapps for zooplankton and chlorophyll

library(sf)
library(deltamapr)
load("DroughtRegions.RData")

zoops = filter(DroughtImpact2a, Metric %in% c("Zooplankton\nSouthCentral (ugC/L)", 
                                              "Zooplankton\nConfluence (ugC/L)", "Zooplankton\nSuisun Bay  (ugC/L)",
                                              "Zooplankton\nSuisun Marsh  (ugC/L)")) %>%
  mutate(Region = case_when(Metric == "Zooplankton\nSouthCentral (ugC/L)" ~ "SouthCentral",
                            Metric == "Zooplankton\nConfluence (ugC/L)" ~ "Confluence",
                            Metric == "Zooplankton\nSuisun Marsh  (ugC/L)" ~ "Suisun Marsh",
                            Metric == "Zooplankton\nSuisun Bay  (ugC/L)" ~ "Suisun Bay"))

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

Chl = filter(DroughtImpact2a, Metric %in% c("Chlorophyll\n Suisun Bay (log(ug/L))", 
                                              "Chlorophyll\nSouthCentral (log(ug/L))", "Chlorophyll\nConfluence (log(ug/L))",
                                              "Chlorophyll\nNorth (log(ug/L))")) %>%
  mutate(Region = case_when(Metric == "Chlorophyll\nSouthCentral (log(ug/L))" ~ "SouthCentral",
                            Metric == "Chlorophyll\nConfluence (log(ug/L))" ~ "Confluence",
                            Metric == "Chlorophyll\nNorth (log(ug/L))" ~ "North",
                            Metric == "Chlorophyll\n Suisun Bay (log(ug/L))" ~ "Suisun Bay"))

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


ggplot()+ 
  geom_sf(data = test, aes(fill = Cohen)) +
  geom_sf(data = WW_Delta, alpha = 0.2) + theme_bw()+
  scale_fill_viridis(name = "Drought \nImpact")+
  facet_wrap(~Metric)+
  coord_sf(xlim = c(-122.2, -121.2), ylim = c(37.8, 38.45))
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
            Y = max(Value, na.rm = T)) 


ggplot(filter(Int3a, !is.na(MetricL)), aes(x = Index, y = Value)) +
  geom_point(aes(color = Yr_type))+
  drt_color_pal_yrtype(aes_type = "color")+
  geom_smooth(method = lm)+
  geom_text(data = Indexes, aes(x = 9, y = Y, 
                                  label = paste("y = x", round(grad, 3),
                                                "+", round(intercept, 3), "\n R2 =", round(r2, 4),
                                                " P = ", round(P, 4), sep = "")),
            size = 3, nudge_y = -1)+
  facet_wrap(~MetricL, scales = "free_y")+
  theme_bw() + xlab("Sac Valley Index")



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

