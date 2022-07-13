#Hab analysis - regional and water year comparisons

library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(sf)
library(deltamapr)
library(visreg)
library(MASS)
library(car)
library(lubridate)



#import data with all the visual index data
load("data/data package/HABs.RData")

#import shapefile with regions
regions = st_read("data/HABregions.shp")


#check a few plots for outliers
ggplot(HABs, aes(x = Temperature)) + geom_histogram()
summary(HABs$Temperature)
filter(HABs, Temperature <5)
#missing 120 rows, and some of those are 0s, definitely wrong.
HABs = filter(HABs, Temperature >5)

ggplot(HABs, aes(x = Secchi)) + geom_histogram()
summary(HABs$Secchi)
test =filter(HABs, Secchi <10)

group_by(HABs, Source) %>%
  summarize(secm = min(Secchi, na.rm = T), secM = max(Secchi, na.rm = T))

#Ugh, definitely some more rows where Secchi is in meters, not centemeters. But its not consistent!

HABs = mutate(HABs, Secchi = case_when(Secchi <5 ~Secchi *100,
                                       TRUE ~ Secchi))
summary(HABs$Secchi)
summary(HABs$Temperature)

#convert HAB data to a spatial object and plot it
HABssf = filter(HABs, !is.na(Longitude), !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(4326))

############################################################################
###################################################################
#Now let's do the entire year, by regions
# (but just the regions we're interested in)

Habs2 =   st_join(HABssf, regions) %>%
  st_drop_geometry() %>%
  filter(!is.na(Stratum), !is.na(Microcystis)) %>% 
  mutate(Year = year(Date), Yearf = as.factor(Year),
         Month2 = factor(Month, levels = c(6,7,8,9,10),
                         labels = c("Jun", "Jul", "Aug", "Sep", "Oct")))    



####################################################################################
#Models for HAB weed report

#This is the data for table 2-2
effort = group_by(Habs2, Year, Stratum2) %>%
  summarize(N = n()) %>%
  pivot_wider(id_cols = Stratum2, names_from = Year, values_from = N)

write.csv(effort, "outputs/visualindexeffort.csv")



##############################################################
#ordered logistic regression
HABs3 = Habs2

Habs2 = mutate(Habs2, HABord = case_when(
  Microcystis == 1 ~ "Absent",
  Microcystis %in% c(2,3) ~ "Low",
  Microcystis %in% c(4,5) ~ "High")) %>%
  mutate(HABord = factor(HABord, levels = c("Absent", "Low", "High"), ordered = T)) %>%
  filter(Year >2013) %>%
  droplevels()


#now an orgered logistic regression
library(multcomp)
ord2 = polr(HABord ~Yearf + Stratum2, data = Habs2, Hess = T)
summary(ord2)
Anova(ord2)
pairs = emmeans(ord2, pairwise ~ Yearf)
cont = pairs$contrasts
plot(emmeans(ord2, pairwise ~ Yearf), comparisons = TRUE)
tukcfg = cld(emmeans(ord2, pairwise ~ Yearf), Letters = letters) %>%
  mutate(Year = as.numeric(as.character(Yearf)), 
         Letter = str_trim(.group)) 

tukcfg2 = cld(emmeans(ord2, pairwise ~ Stratum2), Letters = letters) %>%
  mutate( 
    Letter = str_trim(.group)) 

#this is table 2-11
Tuekyresults = bind_rows(tukcfg, tukcfg2)
write.csv(Tuekyresults, "outputs/Pairwise_visualdata_July.csv")

write.csv(pairs, "visualdata_alldelta_July.csv")
pr <- profile(ord2)
confint(pr)
plot(pr)
pairs(pr)

#This is figure 2-27 
#Plot across the whole Delta, just summer/fall
ggplot(HABs3, aes(x = Year, fill = as.factor(Microcystis))) +
  geom_bar(position = "fill", color = "grey")+ 
  scale_fill_manual(values = c("white", "tan2", "yellow", "red", "darkred"), 
                    labels = c("absent", "low", "medium", "high", "very high"),
                    name = "Microcystis")+ ylab("Relative Frequency") +
  geom_text(data = tukcfg, aes(x = Year, y = 0.7, label = Letter), inherit.aes = F)

#ggsave("YearHAB.tiff", device = "tiff", width = 6, height = 5)


#Plot for paper with just three categories
#
ggplot(Habs2, aes(x = Year, fill = HABord)) +
  geom_bar(position = "fill", color = "grey")+ 
  scale_fill_manual(values = c("white", "orange", "red"), 
                    labels = c("absent", "low", "high"),
                    name = "Microcystis")+ ylab("Relative Frequency") +
  geom_text(data = tukcfg, aes(x = Year, y = 0.7, label = Letter), inherit.aes = F)
ggsave("plots/YearHAB_3cat.tiff", device = "tiff", width = 6, height = 5)


(ctable <- coef(summary(ord2)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
#This is table 2-10
(ctable <- cbind(ctable, "p value" = p))
write.csv(ctable, "outputs/Visualindexmodel_july.csv")

(ci <- confint(ord2))
exp(cbind(OR = coef(ord2), ci))
###################################################


#Now we will do a seperate logistic regression for each region
HabMod = nest_by(Habs2, Stratum2) %>%
  mutate(mod = list(polr(HABord ~Yearf, data = data, Hess = T)),
         pairs = list(emmeans(mod, pairwise ~ Yearf)),
         CLD = list(cld(pairs, Letters = letters)))

#pairwise comparisons
RegTuk = summarize(HabMod, broom::tidy(CLD))%>%
  mutate(Year = as.numeric(as.character(Yearf)), 
         Letter = str_trim(.group)) %>%
  rename(emmean = estimate, std.erroremm = std.error)

regMod = summarize(HabMod, broom::tidy(mod)) %>%
  mutate(Yearf = str_sub(term, start = 6, end = 9))

#table of coefficients
ctable <- summarize(HabMod, ctab = coef(summary(mod)),
                    p = pnorm(abs(ctab[, "t value"]), lower.tail = FALSE) * 2)


#Table for appendix A
regMod2 = left_join(regMod, RegTuk) %>%
  bind_cols(ctable)
#write.csv(regMod2, "outputs/regionalresults.csv")


######################################################################################
#plots by year and region

#By Region, just summer/fall
#This is plot 2-28
ggplot(Habs2, aes(x = Year, fill = as.factor(Microcystis))) +
  geom_bar(position = "fill", color = "grey")+ facet_wrap(~Stratum2, nrow = 4)+
  scale_fill_manual(values = c("white", "tan2", "yellow", "red", "darkred"), 
                    labels = c("absent", "low", "medium", "high", "very high"),
                    name = "Microcystis")+ ylab("Relative Frequency") +
  geom_text(data = RegTuk, aes(x = Year, y = 0.9, label = Letter), size = 4, inherit.aes = FALSE)+
  theme_bw()+ theme(legend.position = "top", legend.key = element_rect(color = "black"))

#ggsave("RegionalHAB.tiff", device = "tiff", width = 6, height = 7)

#now with just three categories
ggplot(Habs2, aes(x = Year, fill = HABord)) +
  geom_bar(position = "fill", color = "grey")+ facet_wrap(~Stratum2, nrow = 4)+
  scale_fill_manual(values = c("white", "orange",  "red"), 
                    labels = c("absent", "low", "high"),
                    name = "Microcystis")+ ylab("Relative Frequency") +
  geom_text(data = RegTuk, aes(x = Year, y = 0.9, label = Letter), size = 4, inherit.aes = FALSE)+
  theme_bw()+ theme(legend.position = "top", legend.key = element_rect(color = "black"))
