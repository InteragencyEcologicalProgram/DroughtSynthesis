#Long term clam graphs
library(car)
library(tidyverse)
library(emmeans)
library(multcomp)
pal_drought <- c( "D" = "#FDE333", "N" = "#53CC67","W" = "#00588B") 

clams_annual = read_csv("data/annual_clam_density_long_term.csv")
clams_annualg = read_csv("data/annual_clam_biomass_grazing_long_term.csv") %>%
  rename(SpeciesID = Clam)
clamdist = read_csv("data/yearly_potamo_distance.csv")
yeartypes = read_csv("data/yearassignments.csv")

clams = left_join(clams_annual, clams_annualg) %>%
  left_join(yeartypes)
clamdist = left_join(clamdist, yeartypes)

ggplot(clams, aes(x = Drought, y = average_abundance)) + geom_boxplot() +
  facet_wrap(~SpeciesID)

ggplot(clams, aes(x = Drought, y = median_abundance)) + geom_boxplot() +
  facet_wrap(~SpeciesID)


ggplot(clams, aes(x = Drought, y = average_biomass)) + geom_boxplot() +
  facet_wrap(~SpeciesID)
ggplot(clams, aes(x = Drought, y = average_grazing, fill = Drought)) + 
  geom_boxplot() +
  scale_fill_manual(values = pal_drought)+
  facet_wrap(~SpeciesID)+
  scale_x_discrete(labels = c("Drought", "Neutral", "Wet Period"))+
  ylab("grazing rate (m3/m2/day)")+ theme_bw()


ggplot(clams, aes(x = Drought, y = average_abundance, fill = Drought)) + 
  geom_boxplot() +
  scale_fill_viridis_d(direction = -1, guide = NULL)+
  facet_wrap(~SpeciesID)+
  scale_x_discrete(labels = c("Drought", "Neutral", "Wet Period"))+
  ylab("Abundance (N/m2)")+ theme_bw()

ggplot(clamdist, aes(x = Drought, y = yearly_avg_distance/1000, fill = Drought)) + 
  geom_boxplot() +
  scale_fill_viridis_d(direction = -1, guide = NULL)+
  scale_x_discrete(labels = c("Drought", "Neutral", "Wet Period"))+
  ylab("Center of Potamocorbula Distribution (km from GG)")+ theme_bw()+
  xlab("")

######################
#model stuff
hist(clamdist$yearly_avg_distance, breaks = 20)
hist(clams$average_abundance)
summary(clams$average_abundance)
hist(log(clams$average_abundance+1))
hist(log(clams$average_abundance+1))
hist(log(clams$average_biomass+1))
#wow! average biomass is great!!!

CFlt = filter(clams, SpeciesID == "Corbicula fluminea")
d1 = lm(log(average_biomass+1)~ Drought, data = CFlt)
summary(d1)
#plot(d1)
Anova(d1, type = 2)
cf = emmeans(d1, pairwise ~ Drought)
tukcf = cld(cf$emmeans, Letters = letters)
tukcf$SpeciesID = "Corbicula fluminea"

PAlt = filter(clams, SpeciesID == "Potamocorbula amurensis", Year >1986)
d2 = lm(log(average_biomass+1)~ Drought, data = PAlt)
summary(d2)
#plot(d2)
Anova(d2, type = 2)
pa = emmeans(d2, pairwise~Drought)
tukpa = cld(pa$emmeans, Letters = c("b","c", "d"))
tukpa$SpeciesID = "Potamocorbula amurensis"

tukan = bind_rows(tukcf, tukpa)

ggplot() + 
  geom_boxplot(data = clams, aes(x = Drought, y = average_biomass, fill = Drought)) +
  scale_fill_manual(values = pal_drought, guide = NULL)+
  geom_text(data = tukan, aes(x = Drought, y = 25, label = .group))+
  facet_wrap(~SpeciesID)+
  scale_x_discrete(labels = c("Drought", "Neutral", "Wet Period"))+
  ylab("Biomass (g/m2)")+ theme_bw()


##################################
#abundance wasn't quite so good, but we'll give it a try

a1 = lm(log(average_abundance+1)~ Drought, data = CFlt)
summary(a1)
plot(a1)
Anova(a1, type = 2)
cfa = emmeans(a1, pairwise ~ Drought)
tukcfa = cld(cfa$emmeans, Letters = letters)
tukcfa$SpeciesID = "Corbicula fluminea"

a2 = lm(log(average_abundance+1)~ Drought, data = PAlt)
summary(a2)
plot(a2)

Anova(a2, type = 2)
paa = emmeans(a2, pairwise~Drought)
tukpaa = cld(paa$emmeans, Letters = c("b","c", "d"))
tukpaa$SpeciesID = "Potamocorbula amurensis"

tukana = bind_rows(tukcfa, tukpaa)

ggplot() + 
  geom_boxplot(data = clams, aes(x = Drought, y = average_abundance, fill = Drought)) +
  scale_fill_viridis_d(direction = -1, guide = NULL)+
  geom_text(data = tukana, aes(x = Drought, y = 3100, label = .group))+
  facet_wrap(~SpeciesID)+
  scale_x_discrete(labels = c("Drought", "Neutral", "Wet Period"))+
  ylab("Density (clams/m2)")+ theme_bw()



###################################
#include regions
clamsr = read_csv("data/regional_yearly_clam_biomass_grazing_long_term.csv") %>%
  rename(SpeciesID = Clam) %>%
  left_join(yeartypes)
hist(log(clamsr$regional_avg_biomass_year+1))
#not as nice


CFltr = filter(clamsr, SpeciesID == "Corbicula fluminea")
d1r = lm(log(regional_avg_biomass_year+1)~ Drought*Region, data = CFltr)
summary(d1r)
plot(d1r)
Anova(d1r, type = 2)
emmeans(d1r, pairwise ~ Drought*Region)
plot(emmeans(d1r, pairwise ~ Drought*Region))

PAltr = filter(clamsr, SpeciesID == "Potamocorbula amurensis")
d2r = lm(log(regional_avg_biomass_year+1)~ Drought*Region, data = PAltr)
summary(d2r)
plot(d2r)
Anova(d2r, type = 2)
#huh. something is fishy with this data set, I think we need to replafe the zeros, I'll do it later.

####################################3
#distance was also ugly, but...

dist1 = lm(yearly_avg_distance~ Drought, data = clamdist)
summary(dist1)
plot(dist1)
Anova(dist1, type = 2)
emmeans(dist1, pairwise ~ Drought)
plot(emmeans(d1r, pairwise ~ Drought))

#not significant.... odd...


dist2 = lm(yearly_avg_distance~ Index, data = clamdist)
summary(dist2)
plot(dist2)

ggplot(clamdist, aes(x = Index, y = yearly_avg_distance)) + geom_point()

#maybe the average idstance is more responsive to the previous year's water year index

clamdist = mutate(clamdist, lagIndex = lag(Index))

dist3 = lm(yearly_avg_distance~ lagIndex, data = clamdist)
summary(dist3)
plot(dist3)

ggplot(clamdist, aes(x = lagIndex, y = yearly_avg_distance/1000)) + 
  geom_point() + ylab("Center of Potamocorbula distribution (km from Golden Gate)")+
  xlab("Previous Year's Sacramento Vally Index") + geom_smooth(method = "lm")+ theme_bw()

###################################
#long-term grazing

clamsgr = group_by(clams, Year, Index, Yr_type, Drought) %>%
  summarize(totgraz = sum(average_grazing, na.rm = T))

hist(log(clamsgr$totgraz+1))

graz = lm(log(totgraz+1) ~ Drought, data = clamsgr)
summary(graz)
plot(graz)


graz2 = lm(log(totgraz+1) ~ Index, data = clamsgr)
summary(graz2)
plot(graz2)


ggplot(clamsgr, aes(x = Drought, y = log(totgraz+1), fill = Drought)) + geom_boxplot()+
  scale_fill_viridis_d(direction = -1) + theme_bw() + ylab("Combined Grazing rate")

ggplot(clamsgr, aes(x = Index, y = log(totgraz+1))) + geom_point()

clamsgr$lagi =dplyr::lag(clamsgr$Index)

ggplot(clamsgr, aes(x = lagi, y = log(totgraz+1), fill = Drought)) + geom_point()

#huh. Nothing, But we're missing some data for Corbicula in the early 2000s. 
####################################
#each clam individuall
gr = lm(log(average_grazing+1)~ Drought, data = CFlt)

summary(gr)
#plot(gr)
Anova(gr, type = 2)
cfg = emmeans(gr, pairwise ~ Drought)
plot(emmeans(gr, pairwise ~ Drought))
tukcfg = cld(cfg$emmeans, Letters = letters)
tukcfg$SpeciesID = "Corbicula fluminea"



gr2 = lm(log(average_grazing+1)~ Drought, data = PAlt)
summary(gr2)
plot(gr2)
Anova(gr2, type = 2)
pag = emmeans(gr2, pairwise ~ Drought)
tukpag = cld(pag$emmeans, Letters = c("b", "d", "e"))
tukpag$SpeciesID = "Potamocorbula amurensis"

tukg = bind_rows(tukcfg, tukpag)

ggplot() + 
  geom_boxplot(data = clams, aes(x = Drought, y = average_grazing, fill = Drought)) +
  scale_fill_manual(values = pal_drought, guide = NULL)+
  geom_text(data = tukg, aes(x = Drought, y = 3, label = .group))+
  facet_wrap(~SpeciesID)+
  scale_x_discrete(labels = c("Drought", "Neutral", "Wet Period"))+
  ylab("Grazing Rate (m3/m2/day)")+ theme_bw()
