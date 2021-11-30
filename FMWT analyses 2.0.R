# Threadfin Shad, American Shad, Delta Smelt, Longfin Smelt, Striped Bass (Age0)

# packages to load -----

library(tidyverse)
library(MASS)
library(emmeans)
library(visreg)
library(lme4)
library(lmerTest)
library(readxl)

# data & set-up -----

fmwt <- read.csv("data/FMWTindices.csv")

# Year types are based on water year, whereas the FMWT is probably most sensitive
# to the previous water year (so FMWT for Calendar Year 2020 is most sensitive to
# water year 2020)

yeartypes <- read_excel("data/Integrated data set.xlsx", sheet = "yearassignments")

fmwt <- left_join(fmwt, yeartypes) %>%
  filter(Year > 1969)

# analyses by species ----

## Longfin Smelt ----

### summary stats & box plot ----
fmwt %>% 
  group_by(Drought) %>% 
  summarise(mean = mean(Longfin.Smelt, na.rm = T), n = n())

hist(fmwt$Longfin.Smelt)
hist(log(fmwt$Longfin.Smelt),
     main = "Longfin Smelt",
     xlab = "Longfin Smelt Index, ln(x)")

LFSplot <- ggplot(filter(fmwt, Drought != "N"), aes(x = Drought, y = log(Longfin.Smelt), fill = Drought)) 
LFSplot + geom_boxplot() + labs(title = "Longfin Smelt", x = "Water Year Type", y = "Index, ln(x)")

ggplot(fmwt, aes(x = Year, y = Longfin.Smelt)) +
  geom_col(aes(fill = Drought, y = max(fmwt$Longfin.Smelt, na.rm = T)), alpha = 0.5)+
  geom_point()

### linear models, log-transformed -----

lfs1 <- lm(log(Longfin.Smelt) ~ Drought, data = fmwt)
summary(lfs1)
plot(lfs1) # looks reasonable

# pairwise comparison for estimated marginal means in the lm
emmeans(lfs1, pairwise ~ Drought) 
visreg(lfs1) # visualize regression model...

# try the index

lfs2 <- lm(log(Longfin.Smelt) ~ Index, data = fmwt)
summary(lfs2)
plot(lfs2)
visreg(lfs2)

### LFS: convincing drought effect ----

### other models -----
# just to demo what untransformed data look like...
lfs3 <- lm(Longfin.Smelt ~ Drought, data = fmwt)
summary(lfs3)
plot(lfs3) # yikes
visreg(lfs3)

#### glm, Poisson ----
p1 <- glm(Longfin.Smelt ~ Drought, data = fmwt, family = "poisson")
summary(p1)
plot(p1)
# as Rosie says...that's a big bucket of NOPE

#### nb glm, year-type ----
nb1 <- glm.nb(Longfin.Smelt ~ Drought, data = fmwt, link = "log")
summary(nb1)
plot(nb1)
# similar to log-transformed version, but diagnostic plots don't look as good
# are observations 11 & 13 (years 1980 & 1982) outliers for other spp too?

#### nb glm, Sac Index ----
nb2 <- glm.nb(Longfin.Smelt ~ Index, data = fmwt)
summary(nb2)
plot(nb2)

#### nb glmm, Sac Index & intercept  ----
nb3 <- glmer.nb(Longfin.Smelt ~ Index + (1|Year), data = fmwt)
summary(nb3)
# not sure that's right

#### nb glm, Sac Index & year ----
nb4 <- glm.nb(Longfin.Smelt ~ Index + Year, data = fmwt)
summary(nb4)
plot(nb4) # not so bad
visreg(nb4)

## Threadfin Shad ----
### summary stats & box plot ----
fmwt %>% 
  group_by(Drought) %>% 
  summarise(mean = mean(Threadfin.Shad, na.rm = T), n = n())

hist(fmwt$Threadfin.Shad)
hist(log(fmwt$Threadfin.Shad),
     main = "Threadfin Shad",
     xlab = "Threadfin Shad Index, ln(x)")
# log transformation helps but results are still skewed

TFSplot <- ggplot(fmwt, aes(x = Drought, y = log(Threadfin.Shad), fill = Drought)) 
TFSplot + geom_boxplot() + labs(title = "Threadfin Shad", x = "Water Year Type", y = "Index, ln(x)")

### linear model, log-transformed -----

tfs1 <- lm(log(Threadfin.Shad) ~ Drought, data = fmwt)

summary(tfs1)
plot(tfs1) # could look worse

# pairwise comparison for estimated marginal means in the lm
emmeans(tfs1, pairwise ~ Drought) 
visreg(tfs1) 

tfs2 <- lm(log(Threadfin.Shad) ~ Drought + Year, data = fmwt)
summary(tfs2)
visreg(tfs2, type = "contrast")

### TFS: potential drought effect ----
# regardless, other factors are almost certainly playing a role

## American Shad ----
### summary stats & box plot ----
fmwt %>% 
  group_by(Drought) %>% 
  summarise(mean = mean(American.Shad, na.rm = T), n = n())

hist(fmwt$American.Shad)
hist(log(fmwt$American.Shad),
     main = "American Shad",
     xlab = "American Shad Index, ln(x)")

AMSplot <- ggplot(fmwt, aes(x = Drought, y = log(American.Shad), fill = Drought)) 
AMSplot + geom_boxplot() + labs(title = "American Shad", x = "Water Year Type", y = "Index (log10(x))")

### linear model, log-transformed -----

ams1 <- lm(log(American.Shad) ~ Drought, data = fmwt)
summary(ams1)
plot(ams1) # looks reasonable

# pairwise comparison for estimated marginal means in the lm
emmeans(ams1, pairwise ~ Drought) 
visreg(ams1)

### AMS: convincing evid of drought effect ----
# no difference btwn normal and wet years, but the other pairwise
# comparisons are all convincing (D vs N, D vs W)

## Delta Smelt ----
### summary stats & box plot ----
fmwt %>% 
  group_by(Drought) %>% 
  summarise(mean = mean(Delta.Smelt, na.rm = T), n = n())

hist(fmwt$Delta.Smelt)
hist(log(fmwt$Delta.Smelt + 1),
     main = "Delta Smelt",
     xlab = "Delta Smelt Index, ln(x+1)")

DSplot <- ggplot(fmwt, aes(x = Drought, y = log(Delta.Smelt+1), fill = Drought)) 
DSplot + geom_boxplot() + labs(title = "Delta Smelt", x = "Water Year Type", y = "Index (log10(x+1))")

### linear models, log-transformed -----

ds1 <- lm(log(Delta.Smelt + 1) ~ Drought, data = fmwt)
summary(ds1)
plot(ds1) # could be worse

# pairwise comparison for estimated marginal means in the lm
emmeans(ds1, pairwise ~ Drought) 
visreg(ds1) # no clear drought effect here

# is there a Year effect?
ds2 <- lm(log(Delta.Smelt + 1) ~ Drought + Year, data = fmwt)
summary(ds2)
plot(ds2) # sketchy-acceptable
visreg(ds2) 
visreg(ds2, type = "contrast") # doesn't show much more

### DS: no drought- but strong year-effect ----

## Striped Bass Age 0 ----
### summary stats & box plot ----
fmwt %>% 
  group_by(Drought) %>% 
  summarise(mean = mean(Striped.Bass.Age0, na.rm = T), n = n())

hist(fmwt$Striped.Bass.Age0)
hist(log(fmwt$Striped.Bass.Age0),
     main = "Striped Bass Age 0",
     xlab = "Striped Bass Index, ln(n)")

SB0plot <- ggplot(fmwt, aes(x = Drought, y = log(Striped.Bass.Age0), fill = Drought)) 
SB0plot + geom_boxplot() + labs(title = "Striped Bass, Age 0", x = "Water Year Type", y = "Index (log10(x))")

### linear models, log-transformed -----

sb01 <- lm(log(Striped.Bass.Age0) ~ Drought, data = fmwt)
summary(sb01)
plot(sb01) # looks reasonable

# pairwise comparison for estimated marginal means in the lm
emmeans(sb01, pairwise ~ Drought) 
visreg(sb01) 

### SB0: convincing evidence of drought effect ----