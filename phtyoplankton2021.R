#2021 EMP community composition data

library(tidyverse)
library(readxl)
library(lubridate)
library(deltamapr)
library(sf)

EMP2021 = read_excel("data/HABs/2021_EMP_Taxonomy.xlsx")

#now strata for SFHA sesonal report
#attach regional assignments
regs = read.csv("AllIEP_wRegions.csv") %>%
  st_as_sf(coords = c("Longitude","Latitude"), crs = 4326)
reg3 = st_transform(R_EDSM_Strata_1718P1, crs = 4326)
allreg = st_join(regs, reg3) %>%
  st_drop_geometry()

EMP2021 = left_join(EMP2021, allreg)

#plot it at the genus level
EMP_wzeros = pivot_wider(EMP2021, id_cols = c(`Full Code`, StationCode, SampleDate, Region, SubRegion), names_from = Genus,
                         values_from = `Organisms per mL`, values_fill = 0, values_fn = sum) %>%
  pivot_longer(cols = "Cocconeis":last_col(), names_to = "Genus", values_to = "CountperML") %>%
  mutate(Month = month(SampleDate))

ggplot(EMP_wzeros, aes(x = StationCode, y = CountperML, fill = Genus))+ geom_col()+facet_wrap(~Month)

ggplot(EMP_wzeros, aes(x = SubRegion, y = CountperML, fill = Genus))+ geom_col()+facet_wrap(~Month)

#now the algal type level
EMP_wzeros2 = pivot_wider(EMP2021, id_cols = c(`Full Code`, StationCode, SampleDate), names_from = `Algal Type`,
                         values_from = `Organisms per mL`, values_fill = 0, values_fn = sum) %>%
  pivot_longer(cols = "Pennate Diatom":last_col(), names_to = "AlgalType", values_to = "CountperML") %>%
  mutate(Month = month(SampleDate))

ggplot(EMP_wzeros2, aes(x = StationCode, y = CountperML, fill = AlgalType))+ geom_col()+facet_wrap(~Month)

#everything but cyanobacteria

ggplot(filter(EMP_wzeros2, AlgalType != "Cyanobacterium"), aes(x = StationCode, y = CountperML, fill = AlgalType))+ geom_col()+facet_wrap(~Month)

#now just the cyanobacteria

cyanos = filter(EMP2021, `Algal Type`== "Cyanobacterium") %>%
  pivot_wider(id_cols = c(`Full Code`, StationCode, SampleDate), names_from = Genus,
                         values_from = `Organisms per mL`, values_fill = 0, values_fn = sum) %>%
  pivot_longer(cols = "Eucapsis":last_col(), names_to = "Genus", values_to = "CountperML") %>%
  mutate(Month = month(SampleDate))

ggplot(cyanos, aes(x = StationCode, y = CountperML, fill = Genus))+ geom_col()+facet_wrap(~Month)


#subset just the harmful taxa

HABs = filter(cyanos, Genus %in% c("Aphanizomenon", "Dolichospermum", "Microcystis", "Oscillatoria")) %>%
  left_join(allreg)

ggplot(HABs, aes(x = Month, y = CountperML, fill = Genus))+ geom_col()+facet_wrap(~StationCode)
#Means by region

HABsMean = group_by(HABs, Month, Region, Genus) %>%
  summarize(CountpermL = mean(CountperML))

ggplot(HABsMean, aes(x = Month, y = CountpermL, fill = Genus))+ geom_col()+facet_wrap(~Region)


###################################################################################################
#Now let's plot individuals per ml

EMP_wzerosX = mutate(EMP2021, CellsPerMl = `Organisms per mL`*`Number of cells per unit`) %>%
  pivot_wider(id_cols = c(`Full Code`, StationCode, SampleDate), names_from = Genus,
                         values_from = CellsPerMl, values_fill = 0, values_fn = sum) %>%
  pivot_longer(cols = "Cocconeis":last_col(), names_to = "Genus", values_to = "CellsperML") %>%
  mutate(Month = month(SampleDate))


ggplot(EMP_wzerosX, aes(x = StationCode, y = CellsperML, fill = Genus))+ geom_col()+facet_wrap(~Month)

#now the algal type level
EMP_wzeros2X = mutate(EMP2021, CellsPerMl = `Organisms per mL`*`Number of cells per unit`) %>%
  pivot_wider(id_cols = c(`Full Code`, StationCode, SampleDate), names_from = `Algal Type`,
              values_from = `Organisms per mL`, values_fill = 0, values_fn = sum) %>%
  pivot_longer(cols = "Pennate Diatom":last_col(), names_to = "AlgalType", values_to = "CellsperML") %>%
  mutate(Month = month(SampleDate))

ggplot(EMP_wzeros2X, aes(x = StationCode, y = CellsperML, fill = AlgalType))+ geom_col()+facet_wrap(~Month)

#everything but cyanobacteria

ggplot(filter(EMP_wzeros2X, AlgalType != "Cyanobacterium"), 
       aes(x = StationCode, y = CellsperML, fill = AlgalType))+ geom_col()+facet_wrap(~Month)

#now just the cyanobacteria

cyanosX = filter(EMP2021, `Algal Type`== "Cyanobacterium") %>%
  mutate(CellsPerMl = `Organisms per mL`*`Number of cells per unit`) %>%
  pivot_wider(id_cols = c(`Full Code`, StationCode, SampleDate), names_from = Genus,
              values_from = `CellsPerMl`, values_fill = 0, values_fn = sum) %>%
  pivot_longer(cols = "Eucapsis":last_col(), names_to = "Genus", values_to = "CellsperML") %>%
  mutate(Month = month(SampleDate))

ggplot(cyanosX, aes(x = StationCode, y = CellsperML, fill = Genus))+ geom_col()+facet_wrap(~Month)


###################################################################################################
#Now let's plot biovolume per ml

EMP_wzerosB = mutate(EMP2021, Biovol = `Organisms per mL`*`Number of cells per unit`*`Biovolume 1`) %>%
  pivot_wider(id_cols = c(`Full Code`, StationCode, SampleDate, Region, Stratum), names_from = Genus,
              values_from = Biovol, values_fill = 0, values_fn = sum) %>%
  pivot_longer(cols = "Cocconeis":last_col(), names_to = "Genus", values_to = "Biovol") %>%
  mutate(Month = month(SampleDate))


ggplot(EMP_wzerosB, aes(x = Region, y = Biovol, fill = Genus))+ geom_col()+facet_wrap(~Month)

#now the algal type level
EMP_wzeros2B = mutate(EMP2021, Biovol = `Organisms per mL`*`Number of cells per unit`*`Biovolume 1`) %>%
  pivot_wider(id_cols = c(`Full Code`, StationCode, SampleDate, Region, Stratum), names_from = `Algal Type`,
              values_from = Biovol, values_fill = 0, values_fn = sum) %>%
  pivot_longer(cols = "Pennate Diatom":last_col(), names_to = "AlgalType", values_to = "Biovol") %>%
  mutate(Month = month(SampleDate))

ggplot(EMP_wzeros2B, aes(x = Stratum, y = Biovol, fill = AlgalType))+ geom_col()+facet_wrap(~Month)


ggplot(EMP_wzeros2B, aes(x = Month, y = Biovol, fill = AlgalType))+ 
  geom_col(position = "fill") +scale_fill_brewer(palette = "Set2")+
  ylab("Percent composition (biovolume)")


#everything but cyanobacteria

ggplot(filter(EMP_wzeros2B, AlgalType != "Cyanobacterium"), 
       aes(x = StationCode, y = Biovol, fill = AlgalType))+ geom_col()+facet_wrap(~Month)

#now just the cyanobacteria

cyanosB = filter(EMP2021, `Algal Type`== "Cyanobacterium") %>%
  mutate(Biovol = `Organisms per mL`*`Number of cells per unit`*`Biovolume 1`) %>%
  pivot_wider(id_cols = c(`Full Code`, StationCode, SampleDate), names_from = Genus,
              values_from = Biovol, values_fill = 0, values_fn = sum) %>%
  pivot_longer(cols = "Eucapsis":last_col(), names_to = "Genus", values_to = "Biovol") %>%
  mutate(Month = month(SampleDate))

ggplot(cyanosB, aes(x = StationCode, y = Biovol, fill = Genus))+ geom_col()+facet_wrap(~Month)
ggplot(cyanosB, aes(x = Month, y = Biovol, fill = Genus))+ geom_col(position = "fill")+
  ylab("Percent composition (biovolume, cyanobacteria only)")



#############################################################################
#now stuff for the SFHR
#Nick plotted total biovolume and biovolume of diatoms
#Convert biovolume from cubic microns to cubic mm

EMP_wzeros2B = mutate(EMP_wzeros2B, Biovolmm = Biovol/1000000000)

Phytot = group_by(EMP_wzeros2B, Stratum, Month, StationCode, Region) %>%
  summarize(TotalBiovol = sum(Biovolmm)) %>%
  mutate(Tax = "All Phytoplankton")

Diatom = filter(EMP_wzeros2B, AlgalType == "Pennate Diatom" | AlgalType == "Centric Diatom") %>%
  group_by(Stratum, Month, StationCode, Region) %>%
  summarize(TotalBiovol = sum(Biovolmm)) %>%
  mutate(Tax = "Diatoms")

Phy = bind_rows(Phytot, Diatom) %>%
  mutate(Monthf = factor(Month, levels = c(6,7,8,9), labels = c("Jun", "Jul", "Aug", "Sep"))) %>%
  filter(Stratum == "Lower Sacramento", Month != 6)
ggplot(Phy, aes(x = Monthf, y = TotalBiovol)) + geom_boxplot(fill = "darkolivegreen") + facet_grid(Stratum ~ Tax)+
  ylab("Biovolume (mm3/mL)") + xlab("Month") + coord_cartesian(ylim = c(0, 0.06))

summary(Phytot$TotalBiovol)
sd(Phytot$TotalBiovol)

summary(Diatom$TotalBiovol)
sd(Diatom$TotalBiovol)

sum(Diatom$TotalBiovol)/sum(Phytot$TotalBiovol)
##########################################################################
#look at prior years' data

EMPphyto = read_csv("data/HABs/1975-2020_phyto.csv")
#look for regions and attach
regs = read.csv("IEP_StationswRegions_19OCT2021.csv")

EMPphyto = left_join(EMPphyto, dplyr::select(regs, StationCode, SubRegion, Region))

EMPphyto2011_2020 = mutate(EMPphyto, Year = year(SampleDate), Month = month(SampleDate)) %>%
  filter(Year >2010) %>%
mutate(Cells = case_when(
  is.na(Cells_per_Unit) ~ Organisms_per_mL,
  TRUE ~ Organisms_per_mL*Cells_per_Unit)) %>%
  pivot_wider(id_cols = c(StationCode, SampleDate, Month, Year, Region, SubRegion), names_from = Genus,
              values_from = Cells, values_fill = 0, values_fn = sum) %>%
  pivot_longer(cols = "Achnanthes":last_col(), names_to = "Genus", values_to = "Cells")




#read in cyanobacteria types
types = read_excel("data/HABs/Phyto Classification.xlsx")
types = group_by(types, Genus, `Algal Type`) %>%
  summarize(N = n()) %>%
  mutate(AlgalType = case_when(
    `Algal Type` == "Centric diatom" ~ "Centric Diatom",
    `Algal Type` == "Unknown Genus" ~ "Unknown",
    `Algal Type` %in% c("Coccolithophore", "Eustigmatophyte", "Haptophyte", "Raphidophyte",
                        "Silico-flagellate", "Synurophyte", "Xanthophyte", "Kathablepharid") ~ 
      "Other",
    TRUE ~ `Algal Type`
  ) )

EMPphytoX = mutate(EMPphyto, Year = year(SampleDate), Month = month(SampleDate)) %>%
  filter(Year >2010) %>%
  mutate(Cells = case_when(
    is.na(Cells_per_Unit) ~ Organisms_per_mL,
    TRUE ~ Organisms_per_mL*Cells_per_Unit),
    AlgalType = NULL) %>%
  left_join(types)


EMPphyto2011_20202 =  pivot_wider(EMPphytoX, id_cols = c(StationCode, SampleDate, Month, Year, Region, SubRegion), names_from = AlgalType,
                                  values_from = Cells, values_fill = 0, values_fn = sum) %>%
  pivot_longer(cols = "Pennate Diatom":last_col(), names_to = "AlgalType", values_to = "Cells")



ggplot(EMPphyto2011_20202, aes(x = Region, y = Cells, fill = AlgalType))+ 
  geom_col()+facet_wrap(~Month)

test = filter(EMPphytoX, is.na(AlgalType))
unique(test$Genus)
#everything but cyanobacteria

ggplot(filter(EMPphyto2011_20202, AlgalType != "Cyanobacterium" & AlgalType != "Soft Body"), 
       aes(x = StationCode, y = Cells, fill = AlgalType))+ geom_col()+facet_wrap(~Month) +
  coord_cartesian(ylim = c(0, 500000))


ggplot(filter(EMPphyto2011_20202, AlgalType != "Cyanobacterium" & AlgalType != "Soft Body"), 
       aes(x = as.factor(Month), y = log(Cells+1), fill = AlgalType))+ 
  geom_boxplot() + facet_wrap(~AlgalType)

#now just the cyanobacteria

cyanosX = filter(EMPphytoX, AlgalType== "Cyanobacterium") %>%
  pivot_wider(id_cols = c(StationCode, SampleDate), names_from = Genus,
              values_from = Cells, values_fill = 0, values_fn = sum) %>%
  pivot_longer(cols = "Aphanizomenon":last_col(), names_to = "Genus", values_to = "CellsperML") %>%
  mutate(Month = month(SampleDate), Year = year(SampleDate))

ggplot(cyanosX, aes(x = StationCode, y = CellsperML, fill = Genus))+ 
  geom_col()+facet_grid(Year~Month) 
ggplot(filter(cyanosX, Genus != "Eucapsis"), aes(x = StationCode, y = CellsperML, fill = Genus))+ 
  geom_col()+facet_grid(Year~Month) 

#########################################################################
EMPphyto2011_2020 = mutate(EMPphyto, Year = year(SampleDate), Month = month(SampleDate)) %>%
  filter(Year >2010) %>%
  pivot_wider(id_cols = c(StationCode, SampleDate, Month, Year), names_from = Genus,
              values_from = Organisms_per_mL, values_fill = 0, values_fn = sum) %>%
  pivot_longer(cols = "Achnanthes":last_col(), names_to = "Genus", values_to = "Cells")


#read in cyanobacteria types
types = read_excel("data/HABs/Phyto Classification.xlsx")
types = group_by(types, Genus, `Algal Type`) %>%
  summarize(N = n()) %>%
  mutate(AlgalType = case_when(
    `Algal Type` == "Centric diatom" ~ "Centric Diatom",
    `Algal Type` == "Unknown Genus" ~ "Unknown",
    `Algal Type` %in% c("Coccolithophore", "Eustigmatophyte", "Haptophyte", "Raphidophyte",
                        "Silico-flagellate", "Synurophyte", "Xanthophyte", "Kathablepharid") ~ 
      "Other",
    TRUE ~ `Algal Type`
  ) )

EMPphytoX = mutate(EMPphyto, Year = year(SampleDate), Month = month(SampleDate)) %>%
  filter(Year >2010) %>%
  mutate(Cells = Organisms_per_mL,
    AlgalType = NULL) %>%
  left_join(types)

test = dplyr::select(EMPphytoX, -N, -AlgalType)


EMPphyto2011_20202 =  pivot_wider(EMPphytoX, id_cols = c(StationCode, SampleDate, Month, Year), names_from = AlgalType,
                                  values_from = Cells, values_fill = 0, values_fn = sum) %>%
  pivot_longer(cols = "Pennate Diatom":last_col(), names_to = "AlgalType", values_to = "Cells")



ggplot(EMPphyto2011_20202, aes(x = StationCode, y = Cells, fill = AlgalType))+ 
  geom_col()+facet_wrap(~Month)

ggplot(EMPphyto2011_20202, aes(x = Month, y = Cells, fill = AlgalType))+ 
  geom_col(position = "fill")

test = filter(EMPphytoX, is.na(AlgalType))
unique(test$Genus)
#everything but cyanobacteria

ggplot(filter(EMPphyto2011_20202, AlgalType != "Cyanobacterium" & AlgalType != "Soft Body"), 
       aes(x = StationCode, y = Cells, fill = AlgalType))+ geom_col()+facet_wrap(~Month) +
  coord_cartesian(ylim = c(0, 500000))


ggplot(filter(EMPphyto2011_20202, AlgalType != "Cyanobacterium" & AlgalType != "Soft Body"), 
       aes(x = as.factor(Month), y = log(Cells+1), fill = AlgalType))+ 
  geom_boxplot() + facet_wrap(~AlgalType)

#now just the cyanobacteria

cyanosX = filter(EMPphytoX, AlgalType== "Cyanobacterium") %>%
  pivot_wider(id_cols = c(StationCode, SampleDate), names_from = Genus,
              values_from = Cells, values_fill = 0, values_fn = sum) %>%
  pivot_longer(cols = "Aphanizomenon":last_col(), names_to = "Genus", values_to = "CellsperML") %>%
  mutate(Month = month(SampleDate), Year = year(SampleDate))

ggplot(cyanosX, aes(x = StationCode, y = log(CellsperML+1), fill = Genus))+ 
  geom_col()+facet_grid(Year~Month) 
ggplot(filter(cyanosX, Genus != "Eucapsis"), aes(x = StationCode, y = CellsperML, fill = Genus))+ 
  geom_col()+facet_grid(Year~Month) 

##############################################################################################
#try and combine 2021 with older data

OlderEMP = read_csv("EMP_1975_2020_phyto.csv") %>%Regions<-read_csv("RosiesDraftAnalyses/Rosies_regions2.csv")

## Load Delta Shapefile from Brian
Delta<-deltamapr::R_EDSM_Subregions_Mahardja_FLOAT%>%
  filter(SubRegion%in%unique(Regions$SubRegion))%>%  #Filter to regions of interest
  dplyr::select(SubRegion)

Regs = unique(Regions[,c(1,5)])
Delta = merge(Delta, Regs) %>%
  st_transform(crs = 4326)



Habs2 =   st_join(HABssf, Delta) %>%
  st_drop_geometry() %>%
  filter(!is.na(Region), !is.na(Microcystis)) %>% 
  mutate(Yearf = as.factor(Year),
         Month2 = factor(Month, levels = c(6,7,8,9,10),
                         labels = c("Jun", "Jul", "Aug", "Sep", "Oct")))    



SFH2a = mutate(Habs2, HABord = case_when(
  Microcystis == 1 ~ "absent",
  Microcystis %in% c(2,3) ~ "Low",
  Microcystis %in% c(4,5) ~ "High")) %>%
  mutate(HABord = factor(HABord, levels = c("absent", "Low", "High"), ordered = T)) %>%
  filter(Year >2013)

#now an orgered logistic regression
library(MASS)
library(car)
ord2 = polr(HABord ~Yearf, data = SFH2a, Hess = T)
summary(ord2)
Anova(ord2)
pairs = emmeans(ord1, pairwise ~ Yearf)$contrasts
write.csv(pairs, "visualdata_alldelta.csv")
pr <- profile(ord1)
confint(pr)
plot(pr)
pairs(pr)

(ctable <- coef(summary(ord1)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(ord1))
exp(cbind(OR = coef(ord1), ci))

emp = filter(HABs, Source == "EMP", !is.na(Microcystis), Year == 2015)
table(emp$Month)
  mutate(SampleTime = as_datetime(SampleTime))
names(OlderEMP) 
names(EMP2021)
EMP2021 = rename(EMP2021, Organisms_per_mL = `Organisms per mL`, Group = `Colony/Filament/Individual Group Code`,
                 Cells_per_Unit = `Number of cells per unit`)
 EMP2021 =  mutate(EMP2021, SampleTime = as_datetime(SampleTime), Year = year(SampleDate), Month = month(SampleDate))
EMPall = bind_rows(OlderEMP, EMP2021) %>%
  dplyr::select(-c(Cells, `Full Code`, `GALD 1`, `GALD 2`, `GALD 3`), -starts_with("Biovolume"))
################################################################################################3
##################################################################################################
#Start running from here!!
#write.csv(EMPall, "EMP_phyto_data.csv", row.names = F)

types = read_excel("data/HABs/Phyto Classification.xlsx")
types = group_by(types, Genus, `Algal Type`) %>%
  summarize(N = n()) %>%
  mutate(AlgalType = case_when(
    `Algal Type` == "Centric diatom" ~ "Centric Diatom",
    `Algal Type` == "Unknown Genus" ~ "Unknown",
    `Algal Type` %in% c("Coccolithophore", "Eustigmatophyte", "Haptophyte", "Raphidophyte",
                        "Silico-flagellate", "Synurophyte", "Xanthophyte", "Kathablepharid") ~ 
      "Other",
    TRUE ~ `Algal Type`
  ) )


#now strata for SFHA sesonal report
#attach regional assignments
regs = read.csv("AllIEP_wRegions.csv") %>%
  st_as_sf(coords = c("Longitude","Latitude"), crs = 4326)
reg3 = st_transform(R_EDSM_Strata_1718P1, crs = 4326)
allreg = st_join(regs, reg3) %>%
  st_drop_geometry()


EMPall = read.csv("EMP_phyto_data.csv") %>%
  select(-X, -SubRegion, -Stratum, -Stratum2, -Region, -nudge) %>%
  left_join(allreg, by = "StationCode")

#EMPall = left_join(EMPall, allreg)

EMPallsum = group_by(EMPall, StationCode, Region, SampleDate, Month, Year, Algal.Type) %>%
  summarize(OrgperML = sum(Organisms_per_mL, na.rm = T)) %>%
  #rename(Algal.Type = `Algal Type`) %>%
  group_by(Region, Month, Year, Algal.Type) %>%
  summarize(OrgperML = mean(OrgperML)) %>%
  filter(Year > 2013) %>%
  mutate(Algal.Type = case_when(
    Algal.Type %in% c("Ciliate", "Dinoflagelate", "Euglenoid", "Haptophyte", "Xanthophyte") ~ "Other",
    TRUE ~ Algal.Type
  ))

#ggplot(filter(EMPallsum, Year == 2020), aes(x = Month, y = OrgperML, fill = Genus)) + geom_col()+
#  facet_grid(Algal.Type~Year)

ggplot(filter(EMPallsum, !is.na(Region), Algal.Type != "Cyanobacterium", Year > 2018), aes(x = Month, y = OrgperML, fill = Algal.Type)) + geom_col()+
  facet_grid(Region~Year)



EMPallcy = EMPall %>%
  #rename(Algal.Type = `Algal Type`) %>%
  group_by( StationCode, Stratum, SampleDate, Month, Year, Algal.Type, Genus) %>%
  summarize(OrgperML = sum(Organisms_per_mL, na.rm = T)) %>%
  filter(Algal.Type == "Cyanobacterium") %>%
  group_by(Stratum, Month, Year, Genus) %>%
  summarize(OrgperML = mean(OrgperML)) %>%
  filter(Year > 2013, Stratum %in% c("Lower Sacramento", "Lower San Joaquin", "Southern Delta"),
         Month %in% c(6,7,8,9))

#Summer HABS near the barrier

ggplot(EMPallcy,  aes(x = Month, y = OrgperML, fill = Genus)) + geom_col()+
  facet_grid(Stratum~Year)

ggplot(filter(EMPallcy, Year == 2021),  aes(x = Month, y = OrgperML, fill = Genus)) + geom_col()

test2020 = filter(EMPall, Year == 2020)

#harmful critters only
EMPHAB = filter(EMPallcy,  Genus %in% c("Aphanizomenon", "Anabaena", "Dolichospermum", 
                                        "Microcystis", "Oscillatoria", "Cylindrospermopsis",  "Anabaenopsis",
                                        "Planktothrix")) %>%
  mutate(Genus = case_when(Genus == "Anabaena" ~"Dolichospermum",
                           TRUE ~ Genus))


ggplot(EMPHAB,  aes(x = Month, y = OrgperML, fill = Genus)) + geom_col()+
  facet_grid(Stratum~Year) +theme_bw() +theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Dark2")+
  ylab("Organisms per mL")

ggplot(filter(EMPHAB, Year == 2021),  aes(x = Month, y = OrgperML, fill = Genus)) + geom_col(position = "dodge")+
  theme_bw() +theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Dark2")+
  ylab("Organisms per mL")

EMPBAB2021 = filter(EMPHAB, Year == 2021) %>%
  pivot_wider(id_cols = c(Year, Month), names_from = Genus, values_from = OrgperML, values_fill = 0, values_fn = mean) %>%
  pivot_longer(cols = c(Aphanizomenon, Microcystis, Dolichospermum, Oscillatoria), values_to = "OrgperML", names_to = "Genus")#%>%
 # mutate(Genus = factor(Genus, levels = c("Oscillatoria", "Aphanizomenon", "Dolichospermum", "Microcystis")))

ggplot(EMPBAB2021,  aes(x = Month, y = OrgperML, fill = Genus)) + geom_col( position = "dodge")+
  theme_bw() +theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Dark2")+
  ylab("Organisms per mL") +
  scale_x_continuous(breaks = c(6,7,8,9),labels = c("June", "July", "August", "September"))

library(ggridges)
ggplot(EMPBAB2021, aes(x = Month, y = Genus, height = OrgperML/1000, fill = Genus)) + 
  geom_ridgeline()+
  theme_bw() +theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Dark2")+
  ylab("Organisms per mL")



EMPallNoc = group_by(EMPall, StationCode, Stratum, SampleDate, Month, Year, Algal.Type) %>%
  summarize(OrgperML = sum(Organisms_per_mL, na.rm = T)) %>%
  filter(Algal.Type != "Cyanobacterium") %>%
  group_by(Stratum, Month, Year, Algal.Type) %>%
  summarize(OrgperML = mean(OrgperML)) %>%
  filter(Year > 2013, Stratum %in% c("Lower Sacramento", "Lower San Joaquin", "Southern Delta"),
         Month %in% c(6,7,8,9))

ggplot(EMPallNoc,  aes(x = Month, y = OrgperML, fill = Algal.Type)) + geom_col()+
  facet_grid(Stratum~Year)


#all genuses
EMPallgenus = group_by(EMPall, StationCode, Region, SampleDate, Month, Year, Algal.Type, Genus) %>%
  summarize(OrgperML = sum(Organisms_per_mL, na.rm = T)) %>%
  group_by(Region, Month, Year, Algal.Type, Genus) %>%
  summarize(OrgperML = mean(OrgperML)) %>%
  filter(Year > 2013, Region == "SouthCentral")%>%
  mutate(Algal.Type = case_when(
    Algal.Type %in% c("Ciliate", "Dinoflagellate", "Euglenoid", "Haptophyte", "Xanthophyte") ~ "Other",
    TRUE ~ Algal.Type
  ))


ggplot(EMPallgenus, aes(x = Month, y = OrgperML, fill = Genus)) + geom_col()+
  facet_grid(Algal.Type~Year, scales = "free_y")+
  scale_fill_discrete(guide = NULL)


########################
#just franks tract


FRK = group_by(EMPall, StationCode, Stratum, SampleDate, Month, Year, Algal.Type) %>%
  summarize(OrgperML = sum(Organisms_per_mL, na.rm = T)) %>%
  filter(StationCode == "D19", Year > 2013,
         Month %in% c(6,7,8,9))

ggplot(FRK, aes(x = Month, y = OrgperML, fill = Algal.Type)) + geom_col()+
  facet_grid(.~Year, scales = "free_y")

ggplot(filter(FRK, Algal.Type != "Cyanobacterium"), aes(x = Month, y = OrgperML, fill = Algal.Type)) + geom_col()+
  facet_grid(.~Year, scales = "free_y")


FRKc = group_by(EMPall, StationCode, Stratum, SampleDate, Month, Year, Algal.Type, Genus) %>%
  
  summarize(OrgperML = sum(Organisms_per_mL, na.rm = T)) %>%
  filter(Algal.Type == "Cyanobacterium") %>%
  filter(StationCode == "D19", Year > 2013,
         Month %in% c(6,7,8,9)) %>%
  mutate(Genus = case_when(
    Genus == "Chroococcus" ~ "Eucapsis",
    TRUE ~ Genus
  ))

ggplot(FRKc, aes(x = Month, y = OrgperML, fill = Genus)) + geom_col()+
  facet_grid(.~Year, scales = "free_y") + scale_fill_brewer(palette = "Set3")


ggplot(filter(FRKc, Genus != "Eucapsis"), aes(x = Month, y = OrgperML, fill = Genus)) + geom_col()+
  facet_grid(.~Year, scales = "free_y") + scale_fill_brewer(palette = "Set3")

sort(unique(EMPallcy$Genus))

###################################################
#Do we see more harmful algae in barrier years?
EMPallzeros = pivot_wider(EMPall, id_cols = c(SampleDate, SampleTime, StationCode, Year, Month, Region, Stratum),
                          names_from = Genus, values_from = Organisms_per_mL, values_fn = sum, values_fill = 0) %>%
  pivot_longer(cols = "Achnanthes":last_col(), names_to = "Genus", values_to = "Organisms_per_mL") %>%
  left_join(types) %>%
  rename(Algal.Type = `Algal Type`)

EMPcy = group_by(EMPallzeros, StationCode, Stratum, SampleDate, Month, Year, Algal.Type, Genus) %>%
  summarize(OrgperML = sum(Organisms_per_mL, na.rm = T)) %>%
  filter(Algal.Type == "Cyanobacterium") %>%
  filter(Year > 2013, Stratum %in% c("Lower Sacramento", "Lower San Joaquin", "Southern Delta"),
         Month %in% c(6,7,8,9))

#Summer HABS near the barrier


#harmful critters only
EMPHAB2 = filter(EMPcy,  Genus %in% c("Aphanizomenon", "Anabaena", "Dolichospermum", 
                                      "Cylindrospermopsis",  "Anabaenopsis",
                                      "Microcystis", "Oscillatoria", "Planktothrix"))  %>% 
  mutate(Genus = case_when(Genus == "Anabaena" ~"Dolichospermum",
     TRUE ~ Genus))
  
HABcol = data.frame(Color = brewer.pal(7, "Dark2"),
                    Genus = sort(unique(EMPHAB2$Genus)))


hist(EMPHAB2$OrgperML)

#mean per species of harmful cristters by year
EMPHAB3s = group_by(EMPHAB2,  Stratum, SampleDate, Month, Year, Genus) %>%
  summarize(OrgperML = mean(OrgperML))

ggplot(EMPHAB3s, aes(x = Month, y = OrgperML, fill = Genus)) + geom_col()+ 
  facet_grid(Stratum~Year) +
  ylab("Organisms per mL of harmful algae") +
  xlab("Month")+
  theme_bw() + theme(legend.position = "bottom")+
  scale_fill_brewer(palette = "Dark2")


#sum of harmful critters per sample
EMPHAB3 = group_by(EMPHAB2,  StationCode, Stratum, SampleDate, Month, Year) %>%
  summarize(OrgperML = sum(OrgperML))

hist(EMPHAB3$OrgperML)
hist(log(EMPHAB3$OrgperML+1))
#Of couse. Now we're zero inflated

library(glmmTMB)
library(performance)
library(DHARMa)
EMPHAB3 = mutate(EMPHAB3, lnOrg = log(OrgperML + 1), rOrg = round(OrgperML),
                 yearf = factor(Year, levels = c(2014,2015,2016,2017,2018,2019,2020,2021)))

c1 = lmer(lnOrg ~ yearf+Stratum + (1|StationCode), data = EMPHAB3)
summary(c1)
emmeans(c1, pairwise~ yearf)
visreg(c1)


c2 = glmmTMB(rOrg ~ yearf+Stratum + (1|StationCode), zi = ~., family = "nbinom2", data = EMPHAB3)
c2b = glmmTMB(rOrg ~ yearf*Stratum + (1|StationCode), zi = ~., family = "nbinom2", data = EMPHAB3)


foo = as.data.frame(summary(c2)$coefficients$cond) %>%
  mutate(type = "cond")
foo2 = as.data.frame(summary(c2)$coefficients$cond) %>%
  mutate(type = "zi")
foo3 = bind_rows(foo, foo2)
write.csv(foo3, "foo3.csv")
visreg(c2)
Anova(c2, component = "cond")
Anova(c2, component = "zi")

pairsY = emmeans(c2, "yearf", component = "cond")
pairsYz = emmeans(c2, "yearf", component = "zi")
foo = as.data.frame(pairs(pairsYz)) %>%
  mutate(type = "zi")
foo2 = as.data.frame(pairs(pairsY)) %>%
  mutate(type = "count")
foo3 = bind_rows(foo, foo2)

pairsS = emmeans(c2, "Stratum", component = "cond")
pairsSz = emmeans(c2, "Stratum", component = "zi")


foo4 = as.data.frame(pairs(pairsS)) %>%
  mutate(type = "count") %>%
  bind_rows(as.data.frame(pairs(pairsSz)) %>%
              mutate(type = "zi")) %>%
  bind_rows(foo3)


simresc2 = simulateResiduals(c2)
plot(simresc2)
performance(c2)


EMPmean = group_by(EMPHAB3, Year, Stratum) %>%
  summarize(Mean = mean(OrgperML, na.rm = T), SD = sd(OrgperML), SE = SD/n(), N = n())
ggplot(EMPHAB3, aes(x = Stratum, y = lnOrg)) + geom_boxplot()+ facet_wrap(~Year)
ggplot(EMPmean, aes(x = Stratum, y = Mean)) + geom_col(fill = "green")+ facet_wrap(~Year) +
  geom_errorbar(aes(ymin = Mean-SE, ymax = Mean +SE))+
  geom_text(aes(label = paste("N =", N), y =2500)) + ylab("Organisms per mL of harmful algae") +
  xlab("Region")+
  scale_x_discrete(label = c("Sacramento", "San Joaquin", "South Delta")) + theme_bw()# +
  #scale_fill_brewer(palette = "Dark2", guide = NULL)


#quick permanova
library(vegan)
HABmat = pivot_wider(EMPHAB2, id_cols = c(StationCode, Stratum, SampleDate, Month, Year), 
                     names_from = "Genus", values_from = "OrgperML", values_fill = 0) %>%
  mutate(yearf = as.factor(Year), total = sum(Aphanizomenon, Anabaena, Dolichospermum, 
                                      Cylindrospermopsis,  Anabaenopsis,
                                      Microcystis, Oscillatoria, Planktothrix)) %>%
  ungroup() %>%
  filter(total != 0)

HABmatX = as.matrix(dplyr::select(HABmat, Anabaena:Oscillatoria))

perm <- how(nperm = 999)
setBlocks(perm) <- with(HABmat, Stratum)

ad1=adonis(HABmatX ~ yearf + Stratum, data = HABmat, permutations = perm)
HABnmds = metaMDS(HABmatX, trymax = 500)
plot(HABnmds)

with(HABmat, ordiellipse(HABnmds, groups = Stratum))
with(HABmat, ordiellipse(HABnmds, groups = yearf))

##################################################
#what's with anabaena versus dolichospermum?

AD = filter(EMPall, Genus %in% c("Anabaena", 
                                "Dolichospermum"))
AD2 = as.data.frame(table(AD$Year, AD$Genus))

ggplot(AD2, aes(x = Var1, y = Freq, fill = Var2)) + geom_col()+ xlab("Year")
