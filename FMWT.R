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
yeartypes = read_excel("data/Integrated data set.xlsx", sheet = "yearassignments") %>%
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