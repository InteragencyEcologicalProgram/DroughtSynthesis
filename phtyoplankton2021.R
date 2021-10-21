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

EMPphyto = left_join(EMPphyto, select(regs, StationCode, SubRegion, Region))

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

test = select(EMPphytoX, -N, -AlgalType)


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

