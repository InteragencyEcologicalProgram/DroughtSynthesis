#Phytoplankton 
#COmpare microcystis in grab samples versus visual index

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



WQ = read_csv("data/SACSJ_delta_water_quality_1975_2020.csv")
WQ = mutate(WQ, Date = mdy(Date), Month = month(Date), Year = year(Date)) %>%
  rename(StationCode = Station)


#join the data together
Phyto = left_join(EMPallzeros, WQ)
test = filter(Phyto, is.na(WTSurface))

MicroSummer = filter(Phyto, Genus == "Microcystis", Month %in% c(5,6,7,8,9,10), Year >2010) %>%
  mutate(MicroPA = case_when(
    Organisms_per_mL == 0 ~ "Absent",
    Organisms_per_mL >0 ~ "Present"
  ),
  Microcystis = as.factor(Microcystis))

ggplot(MicroSummer, aes(x = MicroPA, y = WTSurface)) + geom_boxplot()+
  facet_wrap(~Microcystis)

ggplot(MicroSummer, aes(x = MicroPA, y = WindVelocity)) + geom_boxplot()+
  facet_wrap(~Microcystis)

ggplot(MicroSummer, aes(x = WindVelocity, fill = MicroPA)) + geom_histogram(position = "dodge")+
  facet_wrap(~Microcystis)

ggplot(MicroSummer, aes(fill = MicroPA, x = WTSurface)) + geom_histogram(position = "dodge")+
  facet_wrap(~Microcystis)

##########
#just data with Mc present
Microsum2 = filter(MicroSummer, Microcystis %in% c(2,2.5, 3, 3.5, 4))

ggplot(Microsum2, aes(x = WTSurface, fill = MicroPA)) + geom_histogram(position = "dodge")+
  facet_wrap(~Year)

ggplot(Microsum2, aes(x = WindVelocity, fill = MicroPA)) + geom_histogram(position = "dodge")+
  facet_wrap(~Year)
