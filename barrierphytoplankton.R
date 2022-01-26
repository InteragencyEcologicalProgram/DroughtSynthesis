#PHytoplanktno data


library(tidyverse)
library(readxl)
library(lubridate)
library(deltamapr)
library(sf)
library(vegan)
library(glmmTMB)
library(glmmTMB)
library(performance)
library(DHARMa)
library(lme4)
library(lmerTest)
library(emmeans)
library(visreg)
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

#read in the phytoplankton data I organized earlier
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

#look at it without cyanobacteria
ggplot(filter(EMPallsum, !is.na(Region), Algal.Type != "Cyanobacterium", Year > 2018), aes(x = Month, y = OrgperML, fill = Algal.Type)) + geom_col()+
  facet_grid(Region~Year)


#select the cyanobacteria near the barrier, summer only
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

#addin zeros so we can calculate averages properly
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


#non-cyanobacteria near the barrier
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

#add in zeros
EMPallzeros = pivot_wider(EMPall, id_cols = c(SampleDate, SampleTime, StationCode, Year, Month, Region, Stratum),
                          names_from = Genus, values_from = Organisms_per_mL, values_fn = sum, values_fill = 0) %>%
  pivot_longer(cols = "Achnanthes":last_col(), names_to = "Genus", values_to = "Organisms_per_mL") %>%
  left_join(types) %>%
  rename(Algal.Type = `Algal Type`)

#subset cyanobateria
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
#Of couse. Now we're zero inflated. I'll try a linear model, but we will probably need t
#move to a zero-inflated negative binomial

#round the "org per mL' to turn it into count data. Sorta cheating but
#it's probably OK since the volume was the same for each sample
EMPHAB3 = mutate(EMPHAB3, lnOrg = log(OrgperML + 1), rOrg = round(OrgperML),
                 yearf = factor(Year, levels = c(2014,2015,2016,2017,2018,2019,2020,2021)))

#let's try a regular linear model, just to see what hapens
c1 = lmer(lnOrg ~ yearf+Stratum + (1|StationCode), data = EMPHAB3)
summary(c1)
emmeans(c1, pairwise~ yearf)
visreg(c1)

#now the zero-inflation
c2 = glmmTMB(rOrg ~ yearf+Stratum + (1|StationCode), zi = ~., family = "nbinom2", data = EMPHAB3)
c2b = glmmTMB(rOrg ~ yearf*Stratum + (1|StationCode), zi = ~., family = "nbinom2", data = EMPHAB3)

#organize the output into a nice looking table
foo = as.data.frame(summary(c2)$coefficients$cond) %>%
  mutate(type = "cond")
foo2 = as.data.frame(summary(c2)$coefficients$cond) %>%
  mutate(type = "zi")
foo3 = bind_rows(foo, foo2)
write.csv(foo3, "foo3.csv")
visreg(c2)
Anova(c2, component = "cond")
Anova(c2, component = "zi")

#look at the pairwise comparisons
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

#check residual plots
simresc2 = simulateResiduals(c2)
plot(simresc2)
performance(c2)


#I don't remember why I did this
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

#pivot from long to wide
HABmat = pivot_wider(EMPHAB2, id_cols = c(StationCode, Stratum, SampleDate, Month, Year), 
                     names_from = "Genus", values_from = "OrgperML", values_fill = 0, values_fn = sum) %>%
  mutate(yearf = as.factor(Year), total = sum(Aphanizomenon, Dolichospermum, 
                                              Cylindrospermopsis,  Anabaenopsis,
                                              Microcystis, Oscillatoria, Planktothrix)) %>%
  ungroup() %>%
  filter(total != 0)

#subset just the rows with species counts
HABmatX = as.matrix(dplyr::select(HABmat, Dolichospermum:Planktothrix))

#subset number of permutations and constrain to strata
perm <- how(nperm = 999)
setBlocks(perm) <- with(HABmat, Stratum)

#PERMANOVA
ad1=adonis(HABmatX ~ yearf + Stratum, data = HABmat, permutations = perm)

#try an NMDS
HABnmds = metaMDS(HABmatX, trymax = 500)
plot(HABnmds)

with(HABmat, ordiellipse(HABnmds, groups = Stratum))
with(HABmat, ordiellipse(HABnmds, groups = yearf))
