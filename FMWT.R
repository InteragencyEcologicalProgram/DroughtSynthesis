#quick look at FMWT

library(tidyverse)
library(MASS)
library(emmeans)
library(visreg)
library(lme4)
library(lmerTest)

FMWT = read.csv("data/FMWTindices.csv") %>%
  mutate(YearX = Year)

#year types
#but these are based on water year, whereas the FMWT is probably most senstative
#to the previous water year
yeartypes = read.csv("data/yearassignments.csv") %>%
  mutate(YearX  = Year-1, Year = NULL)


FMWT = left_join(FMWT, yeartypes) %>%
  filter(Year > 1969)

#make a quick box plot
ggplot(FMWT, aes(x = Drought, y = Longfin.Smelt)) + geom_boxplot() + scale_y_log10()

#what kind of distribution are we looking at?
hist(FMWT$Longfin.Smelt)
hist(log(FMWT$Longfin.Smelt))

#now some models. First just log-transformed linear models.
l1 = lm(log(Longfin.Smelt)~Drought, data = FMWT)
summary(l1)
plot(l1)
emmeans(l1, pairwise ~ Drought)
l2 = lm(log(Longfin.Smelt)~Index, data = FMWT)
summary(l2)
plot(l2)
visreg(l2)


#ok, now how about a poisson?


p1 = glm(Longfin.Smelt ~ Drought, data = FMWT, family = "poisson")
summary(p1)
plot(p1)
#that's a big bucket of NOPE


#ok, now how about a negative binomial?

nb1 = glm.nb(Longfin.Smelt ~ Drought, data = FMWT, link = "log")
summary(nb1)
plot(nb1)
#similar to log-transformed version, but diagnostic plots don't look as good.

nb2 = glm.nb(Longfin.Smelt ~ Index, data = FMWT)
summary(nb2)

nb3 = glmer.nb(Longfin.Smelt ~ Index + (1|Year), data = FMWT)
summary(nb3)
#not sure that's right

nb4 = glm.nb(Longfin.Smelt ~ Index + Year, data = FMWT)
summary(nb4)
plot(nb4)
visreg(nb4)
#yuck. q-q plot looks aweful.
#but the year effect looks pretty good. 


#######################################################################
#just some basic summary plots of indecies by season and year for the short-term analyses
yeartypes = read.csv("data/yearassignments.csv")

Integrated_data_set <- read_excel("data/Integrated data set.xlsx", na = "NA")
recent = filter(Integrated_data_set, Year > 2010) %>%
  dplyr::select(-Index)%>%
  mutate(SmeltIndex = as.numeric(SmeltIndex),
         Season = factor(Season, levels = c("Winter", "Spring", "Summer", "Fall"), 
                         labels = c("Winter - SKT","Spring - 20mm", "Summer - STN", "Fall - FMWT"))) %>%
  left_join(yeartypes) %>%
  mutate(Yr_type = factor(Yr_type, levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet")))

recent2 = pivot_longer(recent, cols = c(Sbindex, AmShadIndex, SmeltIndex, LongfinIndex),
                       names_to = "Species", values_to = "IndexX") %>%
  mutate(Species = factor(Species, levels = c("Sbindex", "AmShadIndex", 
                                              "SmeltIndex", "LongfinIndex"),
                          labels = c("Age-0 Striped Bass", "American Shad",
                                     "Delta Smelt", "Longfin Smelt")))

ggplot(recent2, aes(x = Year, y = IndexX)) + geom_col(aes(fill = ShortTerm))+
  facet_grid(Species~Season, scales = "free_y")

ggplot(recent2, aes(x = Year, y = IndexX)) + geom_col(aes(fill = Yr_type))+
  scale_fill_viridis_d( direction = -1)+
  facet_grid(Season~Species, scales = "free_y") +
  theme_bw()

ggplot(recent2, aes(x = Year, y = IndexX)) + geom_col(aes(fill = ShortTerm))+
  facet_wrap(~Species+Season, scales = "free_y")+ theme_bw()+
  scale_fill_viridis_d(name = "Year Type", direction = -1)


ggplot(recent2, aes(x = Year, y = IndexX)) + geom_col(aes(fill = Yr_type))+
  facet_wrap(~Species+Season, scales = "free_y")+ theme_bw()+
  scale_fill_viridis_d(name = "Year Type",  direction = -1)

ggplot(recent, aes(x = Year, y = SmeltIndex)) + geom_col(aes(fill = ShortTerm))+
  facet_wrap(~Season, scales = "free_y")

ggplot(recent, aes(x = Year, y = Sbindex)) + geom_col(aes(fill = ShortTerm))+
  facet_wrap(~Season, scales = "free_y")+ ylab("Striped Bass Index")

ggplot(recent, aes(x = Year, y = AmShadIndex)) + geom_col(aes(fill = ShortTerm))+
  facet_wrap(~Season, scales = "free_y")+ ylab("American Shad Index")+ theme_bw()

ggplot(recent, aes(x = Year, y = LongfinIndex)) + geom_col(aes(fill = ShortTerm))+
  facet_wrap(~Season, scales = "free_y")+ ylab("Longfin Smelt Index")
