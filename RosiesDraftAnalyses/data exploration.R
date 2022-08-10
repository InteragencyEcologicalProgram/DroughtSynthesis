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

yrs = read_csv("data/yearassignments.csv") 

#grab zooplankton data from Arthur
zoopsBPUE_seasonal = read_csv("data/zoop_drought_lt_bpue_szn.csv") %>%
  rename(YearAdj = water_year, ZoopBPUE = s_BPUE)
zoopsBPUE_regional = read_csv("data/zoop_drought_lt_bpue_reg.csv") %>%
  rename(YearAdj = water_year,  ZoopBPUE = r_BPUE)
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
  left_join(yrs) %>%
  mutate(Yr_type = factor(Yr_type, levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet")))

#plot of chlorophyll by water year type
ggplot(Chl2reg, aes(x = Yr_type, y = logChl, fill = Yr_type))+
  geom_boxplot(alpha = 0.7)+
  facet_grid(Season~Region)+
  drt_color_pal_yrtype()+
  ylab("Chlorophyl ug/L (log-transformed)")+
  xlab("Year Type")+
  theme_bw()+
  scale_x_discrete(labels = c("C", "D", "B", "A", "W"))

#plot of raw chlrophyll data by day of year
Chl = mutate(Chl, Yday = yday(Date), Yr_type = factor(ds_year_type, 
                                                      levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet")))

ggplot(Chl, aes(x = Yday, y = chlaAvg_log10, color = Yr_type))+
  geom_point(alpha = 0.3, shape = "circle open" )+
  geom_smooth()+
  drt_color_pal_yrtype(aes_type = "color")+
  facet_wrap(~Region)+
  theme_bw()

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

ggplot(IntLong, aes(x = Drought, y = Value)) + geom_boxplot() +
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

###################################################################################################
#i'm gonna want regions in stead of seasons for some metrics, probalby more interesting

#summarize chlorophyll by region, fix region names, and make a regional metric 
Chl2regB = group_by(Chl2reg, Year, Region) %>%
  summarize(Chla = mean(Chla, na.rm = T), logChla = mean(logChl, na.rm = T)) %>%
  rename(YearAdj = Year) %>%
  mutate(Region = case_when(Region == "South-Central Delta" ~ "SouthCentral",
                            Region == "North Delta" ~ "North",
                            TRUE ~ Region),
         Metric = paste("logChla", Region)) %>%
  rename(Value = logChla)

#reigonal metric label
Zoops = mutate(zoopsBPUE_regional, Metric = paste("ZoopBPUE", Region)) %>%
  rename(Value = ZoopBPUE)

#bring in other hydrology
hyro = pivot_longer(lt_seasonal, cols = c(Outflow, Export, X2), names_to = "Metric", values_to = "Value") %>%
  group_by(YearAdj,Metric) %>%
          summarize (Value = mean(Value))

#water quality is pretty consistant, both seasonally and regionally. Let's just do the annual mean
WQreg = pivot_longer(lt_regional, cols = c(Temperature, Salinity, Secchi),
                     names_to = "Metric", values_to = "Value") %>%
  group_by(YearAdj, Metric) %>%
  summarize(Value = mean(Value, na.rm = T))

#Just to FMWT for fish
Fish2 = filter(Fish, Season == "Fall") %>%
  pivot_longer(cols = c(Sbindex, SmeltIndex, LongfinIndex, AmShadIndex), names_to = "Metric",
               values_to = "Value") %>%
  mutate(Value = log(Value+1))

load("ResidenceTime.RData")
RTlong = ungroup(DFRTann) %>%
  pivot_longer(cols = c(SACRT, SJRT), names_to = "Metric", values_to = "Value") %>%
  rename(YearAdj = WY) %>%
  dplyr::select(YearAdj, Metric, Value)


#Bind them together
integratd_data2 = bind_rows(Chl2regB, Zoops, WQreg, Fish2, RTlong, hyro) %>%
  dplyr::select(YearAdj, Metric, Value) %>%
  rename(Year = YearAdj)

Int2 = left_join(integratd_data2, yrs) %>%
  mutate(Yr_type = factor(Yr_type, levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet")),
         MetricL = factor(Metric, levels =  c("Outflow", "Export", "X2","SACRT", "SJRT", "Salinity",  "Secchi","Temperature" , "logChla Confluence", 
                                            "logChla North", "logChla SouthCentral", "logChla Suisun Bay",
                                           "ZoopBPUE Confluence","ZoopBPUE SouthCentral","ZoopBPUE Suisun Bay", 
                                           "ZoopBPUE Suisun Marsh",              
                                 "Sbindex", "SmeltIndex", "LongfinIndex", "AmShadIndex"), 
                                labels = c("Outflow (CFS)", "SWP + CVP \nExports (CFS)", "X2", "Sacramento\nResidence Time (days)", "San Joaquin\nResidence Time, (days)",
                                           "Salinity (PSU)",  "Secchi Depth \n(cm)",
                                           "Temperature (C)" , "Chlorophyll\nConfluence (log(ug/L))", 
                                           "Chlorophyll\nNorth (log(ug/L))", "Chlorophyll\nSouthCentral (log(ug/L))", 
                                           "Chlorophyll\n Suisun Bay (log(ug/L))",
                                           "Zooplankton\nConfluence (ugC/L)","Zooplankton\nSouthCentral (ugC/L)",
                                           "Zooplankton\nSuisun Bay  (ugC/L)", 
                                           "Zooplankton\nSuisun Marsh  (ugC/L)",              
                                           "Striped Bass \nlog FMWT Index", "Delta Smelt\nlog FMWT Index", 
                                           "Longfin  \nlog FMWT Index", 
                                           "Am Shad  \nlog FMWT Index")))


#look at it without the "not drought or wet" years
ggplot(filter(Int2, Drought != "N"), aes(x = Drought, y = Value)) + geom_boxplot() +
  facet_wrap(MetricL~., scales = "free_y")

ggplot(Int2, aes(x = Drought, y = Value, fill = Drought)) + geom_boxplot() +
  facet_wrap(MetricL~., scales = "free_y")+ drt_color_pal_drought()

ggplot(Int2, aes(x = Yr_type, y = Value, fill = Yr_type), alpha = 0.3) + geom_boxplot() +
  facet_wrap(MetricL~., scales = "free_y") + drt_color_pal_yrtype()+
  theme_bw()

# rework it so residence time is a column

Int3 = left_join(ungroup(Int2), dplyr::select(rename(ungroup(DFRTann), Year = WY), Year, SACRT, SJRT))

ggplot(Int3, aes(x = SACRT, y = Value)) +
  geom_point(aes(color = Yr_type))+
  drt_color_pal_yrtype(aes_type = "color")+
  geom_smooth()+
  facet_wrap(~MetricL, scales = "free_y")+
  theme_bw()


# Now do outflow

Int4 = rename(lt_seasonal, Year = YearAdj) %>%
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

DroughtImpact = group_by(IntLong, Season, Metric, Drought) %>%
  summarize(Mean = mean(Value, na.rm = T)) %>% 
  pivot_wider(names_from = Drought, values_from = Mean) %>%
  mutate(Index = (D-W)/mean(c(D,N, W), na.rm = T), IndexB = D/W)

#Let's try it again and use Cohen's D
DroughtImpact2 = filter(Int2, Drought != "N") %>%
  mutate(Drought = as.factor(Drought)) %>%
  group_by(Metric) %>%
  summarize(Cohen = cohen.d(Value ~ Drought, na.rm = T)$estimate, 
            magnitude = cohen.d(Value ~ Drought, na.rm = T)$magnitude)

#now try combining not-drought and wet years
DroughtImpact2b = mutate(Int2, Drought2 = case_when(Drought == "D" ~ "D",
                                                   TRUE ~ "W"),
                        Drought2 = as.factor(Drought2)) %>%
  group_by(Metric) %>%
  summarize(Cohen = cohen.d(Value ~ Drought2, na.rm = T)$estimate, 
            magnitude = cohen.d(Value ~ Drought2, na.rm = T)$magnitude)


#what about the slope of the residence time line?
Int3 = filter(Int3, !is.na(Value), !is.nan(Value))
Resmetric = filter(Int3, !is.na(Value), !is.nan(Value), Metric != "SACRT") %>%
  group_by(MetricL) %>%
  summarize(intercept = coef(lm(Value ~ SACRT))[1],
            grad = coef(lm(Value ~ SACRT))[2],
            r2 = summary(lm(Value ~ SACRT))$r.squared,
            P =  summary(lm(Value ~ SACRT))$coefficients[2,4],
            Y = max(Value)) 

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
            Y = max(Value)) 


ggplot(Int4, aes(x = log(Outflow), y = Value)) +
  geom_point(aes(color = Yr_type))+
  drt_color_pal_yrtype(aes_type = "color")+
  geom_smooth(method = lm)+
  geom_text(data = Resmetric, aes(x = 9, y = Y, 
                                  label = paste("y = x", round(grad, 3),
                                                "+", round(intercept, 3), "\n R2 =", round(r2, 4),
                                                " P = ", round(P, 4), sep = "")),
            size = 3, nudge_y = -1)+
  facet_wrap(~MetricL, scales = "free_y")+
  theme_bw()



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

