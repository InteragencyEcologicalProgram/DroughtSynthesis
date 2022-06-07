#Final HABs graphs and analyses
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(sf)
library(deltamapr)
library(brms)
library(DHARMa)
library(visreg)
library(MASS)
library(car)
library(DroughtData)
library(lubridate)

library(here)

i_am("HABanalysis.R")

#import data with all the visual index data
load("data/data package/HABs.RData")

#import shapefile with regions
regions = st_read("data/HABregions.shp")

#convert HAB data to a spatial object and plot it
HABssf = filter(HABs, !is.na(Longitude), !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(4326))

ggplot() + geom_sf(data = WW_Delta) + geom_sf(data = HABssf)+
  geom_sf_label(data = HABssf, aes(label = Station), 
                position = "jitter", label.size = 0.05)

############################################################################
###################################################################
#Now let's do the entire year, by regions
sfhaball2 = st_crop(HABssf, regions)%>%
  st_join(regions)%>%
  st_drop_geometry() %>%
  filter(!is.na(Stratum), !is.na(Microcystis)) %>%
  #        Stratum %in% c("Suisun Marsh", "Suisun Bay", "Lower Sacramento", "Cache Slough/Liberty Island", "Sac Deep Water Shipping Channel")) %>%
  mutate(Yearf = as.factor(Year), Yearm = Year + (Month-1)/12, Mic = factor(Microcystis, levels = c(1,2,3,4,5), labels = c(
    "absent", "low", "med", "high", "v.high")))  

#set up a color pallette for later use
mypal = regions$colors

# Map of regions for report ####################
# This was the basis for figure 1-2, but then Ted tweaked it in Adobe Illustrator
ggplot() + geom_sf(data = regions, aes(fill = Stratum2), alpha = 0.7)+ 
  geom_sf(data = WW_Delta, fill = "lightblue")+ # + geom_sf(data = HABssf1)+
scale_fill_manual(values = regions$colors, guide = NULL)+
  geom_sf_label(data = regions,aes(label = Stratum2))+
  coord_sf(xlim = c(-121.3, -121.9), ylim = c(37.7, 38.6))+
  theme_bw()+
  xlab(NULL) + ylab(NULL)

ggsave("HABregionsmap.tiff", device = "tiff", width = 5, height = 7)
#


#do the whole delta, but broken up into subregions
sfhaball = filter(HABs, !is.na(Microcystis), !is.na(Longitude), !is.na(Latitude)) %>%
  mutate(Year = year(Date)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(4326)) %>%
st_crop(regions)%>%
  st_join(regions)


#Now make sure we only have data with microcystis observations that are in the region of interest
SFHall =   sfhaball %>%
    filter(!is.na(Stratum), !is.na(Microcystis)) %>%
  mutate(Yearf = as.factor(Year), Yearm = Year + (Month-1)/12, 
         Mic = factor(Microcystis, levels = c(1,2,3,4,5), labels = c(
    "absent", "low", "med", "high", "v.high")))   


###############################
#plot of just 2021, all months
#THIS IS THE PLOT FOR THE REPORT
SFH2021 = filter(SFHall, Year == 2021)

#This is figure 2-11 in the report
ggplot(filter(SFHall, Year == 2021), aes(x = Stratum2, fill = Mic))+geom_bar(position = "fill", color = "grey")+
  facet_wrap(~Month, nrow = 3)+
  scale_fill_manual(values = c("white", "tan2", "yellow", "red", "darkred"), name = "Microcystis") + 
  theme_bw()+
  theme(legend.position = "top")+
  ylab(NULL) + xlab(NULL)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

ggsave("HABs2021.tiff", device = "tiff", width = 6, height = 5)


#Now summarize by month, year, and stratum and add zeros so we can plot it
SFHall2 = group_by(SFHall, Yearm, Yearf, Year, Month, Mic, Stratum) %>%
  summarize(n = n()) %>%
  mutate(Yearm2 = as.factor(Yearm)) %>%
  ungroup()

SFHallzeros =   pivot_wider(SFHall2, id_cols = c(Yearm, Yearf, Year, Month, Stratum), 
                            names_from = Mic, values_from = n, values_fill = 0) %>%
  pivot_longer(cols = c(absent, low, med, high, v.high), values_to = "n", names_to = "Mic") %>%
  mutate(Mic = factor(Mic, levels = c(
    "absent", "low", "med", "high", "v.high")))



SFH =   sfhab %>%
  st_drop_geometry() %>%
  mutate(Yearf = as.factor(Year),
         Month2 = factor(Month, levels = c(6,7,8,9,10),
                         labels = c("Jun", "Jul", "Aug", "Sep", "Oct")))   



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
  Microcystis == 1 ~ "absent",
  Microcystis %in% c(2,3) ~ "Low",
  Microcystis %in% c(4,5) ~ "High")) %>%
  mutate(HABord = factor(HABord, levels = c("absent", "Low", "High"), ordered = T)) %>%
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
write.csv(Tuekyresults, "outputs/Pairwise_visualdata.csv")

#write.csv(pairs, "visualdata_alldelta.csv")
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

ggsave("YearHAB.tiff", device = "tiff", width = 6, height = 5)


(ctable <- coef(summary(ord2)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
#This is table 2-10
(ctable <- cbind(ctable, "p value" = p))
write.csv(ctable, "outputs/Visualindexmodel.csv")

(ci <- confint(ord2))
exp(cbind(OR = coef(ord2), ci))

#By Region, just summer/fall
# ggplot(SFH2a, aes(x = Year, fill = as.factor(Microcystis))) +
#   geom_bar(position = "fill", color = "grey")+ facet_wrap(~Stratum2)+
#   scale_fill_manual(values = c("white", "tan2", "yellow", "red", "darkred"), 
#                     labels = c("absent", "low", "medium", "high", "very high"),
#                     name = "Microcystis")+ ylab("Relative Frequency") 

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
write.csv(regMod2, "outputs/regionalresults.csv")

#By Region, just summer/fall
#This is plot 2-28
ggplot(Habs2, aes(x = Year, fill = as.factor(Microcystis))) +
  geom_bar(position = "fill", color = "grey")+ facet_wrap(~Stratum2, nrow = 4)+
  scale_fill_manual(values = c("white", "tan2", "yellow", "red", "darkred"), 
                    labels = c("absent", "low", "medium", "high", "very high"),
                    name = "Microcystis")+ ylab("Relative Frequency") +
  geom_text(data = RegTuk, aes(x = Year, y = 0.9, label = Letter), size = 4, inherit.aes = FALSE)+
  theme_bw()+ theme(legend.position = "top", legend.key = element_rect(color = "black"))

ggsave("RegionalHAB.tiff", device = "tiff", width = 6, height = 7)

##############################################################################
#Now the flow analysis

library(RColorBrewer)
library(smonitr)

#set up a color pallete for later
pal = c(brewer.pal(8, "Set2"), brewer.pal(8, "Dark2"))

#Use the integrated flow and temperature data from the drougth data package
flowX = raw_hydro_1975_2021 %>%
  rename(Year = YearAdj)
Temps = raw_wq_1975_2021 %>%
  rename(Year = YearAdj)

#Get dayflow data 
Dayflow = get_odp_data(pkg_id = "dayflow", fnames = "Dayflow Results")


DF1997_2020 =  Dayflow$`Dayflow Results 1997 - 2020` %>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  dplyr::select(Date, OUT, EXPORTS, SJR)


DF2021 =  Dayflow$`Dayflow Results 2021` %>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  dplyr::select(Date, OUT, EXPORTS, SJR) 


#now I can put them all together!
DF = bind_rows(DF1997_2020, DF2021)


#check and see if outflow and exports are correlated
ggplot(filter(flowX, Outflow >0, Year > 2007), 
       aes(y = Outflow, x = Export, color = as.factor(Year))) + geom_point()+
  scale_y_log10()+ ylab("Daily Average Delta Outflow (CFS)")+
  xlab("Daily Average SWP+CVP Exports (CFS)")+
  scale_color_manual(values = pal, name = NULL)+
  theme_bw()

#now merge these data frames so we have flow and HAB and temperature dat atogether!
names(DF)
names(flowX)
names(Temps)
names(Habs2)
Habs2 = mutate(Habs2, Date = as.Date(Date))
test = left_join(Habs2, dplyr::select(flowX, -Year, -Season)) %>%
  filter(Temperature >0, Temperature <30) %>%
  left_join(DF)



#Note: All of these models take a long time to run. 

#Do a bayesian mixed model of HABs in the summer in the south and central delta regions

SoDelta = dplyr::filter(test, Stratum2 %in% c("Lower SJ", "Lower Sac", "South Delta", "Franks", "OMR"))


#Scale and center the variables and get ride of values where we have NAs
SoDelta = mutate(SoDelta, day = yday(Date), Outscale = scale(OUT),
                 Exscale = scale(EXPORTS), SJRs = scale(SJR), Tempscale = scale(Temperature), Secchs = scale(Secchi)) %>%
  filter(!is.na(Outscale), !is.na(Tempscale), !is.na(SJRs), !is.na(Exscale), !is.na(Secchs))

#now let's look at all possible combinations of temperature, outflow, exports, and secchi depth.
#San Joaquin flow is too highly correlated with Outflow to use.

M5.3 = brm(HABord ~ Tempscale + Outscale + Exscale + (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
           iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
           control = list(max_treedepth = 15),
           chains = 2, cores=4, threads = threading(2))

M5.41 = brm(HABord ~ Tempscale + Outscale + Secchs+ (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
                      iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
                       control = list(max_treedepth = 15),
                       chains = 2, cores=4, threads = threading(2))

M5.5 = brm(HABord ~ Tempscale + Outscale + Exscale+ Secchs + (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
           iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
           control = list(max_treedepth = 15),
           chains = 2, cores=4, threads = threading(2))


# M5.6 = brm(HABord ~ Tempscale +  Exscale+ SJRs + (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
#            iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
#            control = list(max_treedepth = 15),
#            chains = 2, cores=4, threads = threading(2))

M5.61 = brm(HABord ~ Tempscale +  Exscale+ Secchs + (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
           iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
           control = list(max_treedepth = 15),
           chains = 2, cores=4, threads = threading(2))

# M5.7 = brm(HABord ~   Exscale+ SJRs + (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
#            iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
#            control = list(max_treedepth = 15),
#            chains = 2, cores=4, threads = threading(2))

M5.71 = brm(HABord ~   Exscale+ Secchs + (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
           iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
           control = list(max_treedepth = 15),
           chains = 2, cores=4, threads = threading(2))


M5.8 = brm(HABord ~ Tempscale + (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
           iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
           control = list(max_treedepth = 15),
           chains = 2, cores=4, threads = threading(2))



# M5.9 = brm(HABord ~  SJRs + (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
#            iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
#            control = list(max_treedepth = 15),
#            chains = 2, cores=4, threads = threading(2))

M5.91 = brm(HABord ~  Secchs + (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
                      iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
                       control = list(max_treedepth = 15),
                       chains = 2, cores=4, threads = threading(2))
           


# M5.10 = brm(HABord ~  SJRs + Tempscale+ (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
#            iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
#            control = list(max_treedepth = 15),
#            chains = 2, cores=4, threads = threading(2))

M5.101 = brm(HABord ~  Secchs + Tempscale+ (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
            iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
            control = list(max_treedepth = 15),
            chains = 2, cores=4, threads = threading(2))

M5.11 = brm(HABord ~  Outscale+ (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
            iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
            control = list(max_treedepth = 15),
            chains = 2, cores=4, threads = threading(2))

M5.12 = brm(HABord ~  Exscale+ (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
            iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
            control = list(max_treedepth = 15),
            chains = 2, cores=4, threads = threading(2))

M5.13 = brm(HABord ~  Tempscale + Outscale+ (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
            iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
            control = list(max_treedepth = 15),
            chains = 2, cores=4, threads = threading(2))

M5.14 = brm(HABord ~  Secchs + Outscale+ (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
            iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
            control = list(max_treedepth = 15),
            chains = 2, cores=4, threads = threading(2))

M5.15 = brm(HABord ~  Exscale + Outscale+ (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
            iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
            control = list(max_treedepth = 15),
            chains = 2, cores=4, threads = threading(2))

M5.16 = brm(HABord ~  Exscale + Outscale+ Secchs + (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
            iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
            control = list(max_treedepth = 15),
            chains = 2, cores=4, threads = threading(2))

#Save your work!
save.image()

#add infermation criteria to each model
M5.41 = add_criterion(M5.41, "loo")
M5.41 = add_criterion(M5.41, "waic")
M5.3 = add_criterion(M5.3, "loo")
M5.3 = add_criterion(M5.3, "waic")
M5.5 = add_criterion(M5.5, "loo")
M5.5 = add_criterion(M5.5, "waic")
M5.61 = add_criterion(M5.61, "loo")
M5.61 = add_criterion(M5.61, "waic")
M5.71 = add_criterion(M5.71, "loo")
M5.71 = add_criterion(M5.71, "waic")
M5.8 = add_criterion(M5.8, "loo")
M5.8 = add_criterion(M5.8, "waic")
M5.91 = add_criterion(M5.91, "loo")
M5.91 = add_criterion(M5.91, "waic")
M5.101 = add_criterion(M5.101, "loo")
M5.101 = add_criterion(M5.101, "waic")
M5.11 = add_criterion(M5.11, "loo")
M5.11 = add_criterion(M5.11, "waic")
M5.12 = add_criterion(M5.12, "waic")
M5.13 = add_criterion(M5.13, "waic")
M5.14 = add_criterion(M5.14, "waic")
M5.15= add_criterion(M5.15, "waic")
M5.16= add_criterion(M5.16, "waic")


#Compare WAIC scores and LOO scores
test = loo_compare(M5.41, M5.3,  M5.5, M5.61, M5.71, M5.8, M5.91, M5.101, M5.11, criterion = "loo")
test = loo_compare(M5.41, M5.3, M5.5, M5.61,  M5.71, M5.8, M5.91, M5.101, M5.11, 
                   M5.12, M5.13, M5.15, M5.14, M5.16, criterion = "waic")
write.csv(test, "outputs/WAICscores.csv")

# So, the best model was M5.61, but M5.5 was close behind
#this checks our assumptions and plots the conditional effects
pp_check(M5.5)
cex5.5 = conditional_effects(M5.5, categorical = TRUE)
cex5.5

pp_check(M5.61)
cex5.61 = conditional_effects(M5.61, categorical = TRUE)
cex5.61

#save all our work
save(M5.41, M5.3, M5.5, M5.61,M5.71, M5.8, M5.91, M5.101, M5.11, M5.12, M5.13, M5.14, M5.15, M5.16, file = "MCmodels30mar2022")

#double check we don't have weird correlations
ggplot(SoDelta, aes(x = day, y = Export, color = Yearf)) + geom_point()
ggplot(SoDelta, aes(x = day, y = Outflow, color = Yearf)) + geom_point()
ggplot(SoDelta, aes(x =Export, y = Outflow, color = Yearf)) + geom_point()
ggplot(SoDelta, aes(x =Outflow, y = Export, color = Yearf)) + geom_point()
ggplot(SoDelta, aes(x =Outflow, y = OUT, color = Yearf)) + geom_point()
ggplot(SoDelta, aes(x =Export, y = EXPORTS, color = Yearf)) + geom_point()
ggplot(SoDelta, aes(x =EXPORTS, y = SJR, color = Yearf)) + geom_point()
ggplot(SoDelta, aes(x =OUT, y = SJR, color = Yearf)) + geom_point()+
coord_cartesian(xlim = c(0, 20000))
load("MCmodels30mar2022")

ggplot(SoDelta, aes(x = day, y = Temperature, color = Yearf)) + geom_point()+
  ylab("Water Temperature, degrees C") + xlab("Day of the Year")
ggplot(SoDelta, aes(x = Temperature, y = Outflow, color = Yearf)) + geom_point()
ggplot(SoDelta, aes(x = Temperature, y = Export, color = Yearf)) + geom_point()

# I want prettier plots of the conditional effects
# These are plots 2-31, 2-32, and 2-33
#I should write a function for thsi. 
#plot temperature effect
  temp = cex5.61$`Tempscale:cats__`
  lm = lm(Temperature ~Tempscale, data = SoDelta)
  foo = as.data.frame(summary(lm)$coefficients)
  
  newdata = data.frame(Tempscale = c(-2,0,2))
  newdata = mutate(temp, Temperature = Tempscale*foo$Estimate[2] + foo$Estimate[1])
  
  
  ggplot(filter(newdata, cats__ != "absent"), aes(x = Temperature, y = estimate__)) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = cats__), alpha = 0.3)+
    geom_line(aes(color = cats__))+
    # scale_fill_manual(values = c("blue", "orange", "red"), 
    #                   labels = c("Absent", "Low", "High"), name = "Microcystis")+
    # scale_color_manual(values = c("blue", "orange", "red"), 
    #                    labels = c("Absent", "Low", "High"), name = "Microcystis")+
    scale_fill_manual(values = c("orange", "red"), 
                      labels = c("Low", "High"), name = "Microcystis")+
    scale_color_manual(values = c("orange", "red"), 
                       labels = c("Low", "High"), name = "Microcystis")+
    
    xlab("Temperature C")+
    ylab("Probability")+
    geom_vline(xintercept = mean(filter(SoDeltasum, Yearf == '2021', Month2 == "Jul")$Temperature),
             linetype = 2)+
    annotate("text", x = 22.8, y = 0.5, angle = 90, label = "Mean Jul 2021")+
    theme_bw()
  
  ggsave("plots/MicTemp2.tiff", device = "tiff", width = 6, height = 4, units = "in") 
  
   ggsave("plots/MicTemp.tiff", device = "tiff", width = 6, height = 4, units = "in") 
   
 
  ex = cex5.61$`Exscale:cats__`
  lmE = lm(Export ~Exscale, data = SoDelta)
  fooE = as.data.frame(summary(lmE)$coefficients)
  newdataE = mutate(ex, Exports = Exscale*fooE$Estimate[2] + fooE$Estimate[1])
  
  
  ggplot(filter(newdataE, cats__ != "absent"), aes(x = Exports, y = estimate__)) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = cats__), alpha = 0.3)+
    geom_line(aes(color = cats__))+
    # scale_fill_manual(values = c("blue", "orange", "red"), 
    #                   labels = c("Absent", "Low", "High"), name = "Microcystis")+
    # scale_color_manual(values = c("blue", "orange", "red"), 
    #                    labels = c("Absent", "Low", "High"), name = "Microcystis")+
    scale_fill_manual(values = c("orange", "red"), 
                      labels = c("Low", "High"), name = "Microcystis")+
    scale_color_manual(values = c("orange", "red"), 
                       labels = c("Low", "High"), name = "Microcystis")+
    xlab("Project Exports (cfs)")+
    ylab("Probability")+
    geom_vline(xintercept = 1500, linetype = 2)+
    annotate("text", x = 1300, y = 0.4, label = "TUCP Export Limit", angle = 90)+
    theme_bw()
  ggsave("plots/MicExports2.tiff", device = "tiff", width = 6, height = 4, units = "in")


  turb = cex5.61$`Secchs:cats__`
  lmS = lm(Secchi ~Secchs, data = SoDelta)
  fooS = as.data.frame(summary(lmS)$coefficients)
  newdataS = mutate(turb, Secchi = Secchs*fooS$Estimate[2] + fooS$Estimate[1])
  
  
  ggplot(filter(newdataS, cats__ != "absent"), aes(x = Secchi, y = estimate__)) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = cats__), alpha = 0.3)+
    geom_line(aes(color = cats__))+
    # scale_fill_manual(values = c("blue", "orange", "red"), 
    #                   labels = c("Absent", "Low", "High"), name = "Microcystis")+
    # scale_color_manual(values = c("blue", "orange", "red"), 
    #                    labels = c("Absent", "Low", "High"), name = "Microcystis")+
    scale_fill_manual(values = c("orange", "red"), 
                      labels = c("Low", "High"), name = "Microcystis")+
    scale_color_manual(values = c("orange", "red"), 
                       labels = c("Low", "High"), name = "Microcystis")+
    xlab("Secchi Depth (cm)")+
    ylab("Probability")+
    geom_vline(xintercept = mean(filter(SoDeltasum, Yearf == '2021', Month2 == "Jul")$Secchi),
               linetype = 2)+
    annotate("text", x = 70, y = 0.5, angle = 90, label = "Mean Jul 2021")+
    
    theme_bw()
  ggsave("plots/MicSecchi2.tiff", device = "tiff", width = 6, height = 4, units = "in")
  

  #######################################################################
  #Now I want to use our model to say how big an effect the TUCP had. BUt that's hard
  #because we don't have a great "no TUCP" export scenario

#First lets calculate the mean exports, outflow, temperature, and secchi that
  #were actually observed.
SoDeltasum = group_by(SoDelta, Year, Yearf, Month2) %>%
  summarize(Exscale = mean(Exscale), Outscale = mean(Outscale), Export = mean(Export), 
            Outflow = mean(Outflow), Tempscale = mean(Tempscale), 
            Secchs = mean(Secchs),
            Secchi = mean(Secchi),
            Temperature = mean(Temperature), day = median(day)) %>%
  filter(Yearf %in% c("2021", "2020")) %>%
  droplevels()

save(SoDelta, SoDeltasum, file = "SoDelta.RData")




#Exports of 1500 CFS = -1.18 exscale
summary(lmE)
(1500-5808)/3644
(2500-5808)/3644
summary(lmO)
(3000-7460)/6893
(4000-7460)/6893

#OK! So, if we hold the Exports constant at 1500, what's the difference between 1500 and 2500 CFS exports?


#create new dataframes where outflow, temperatures, and secchi depth are what was actually observed and only exports changes

newdata2e = data.frame(Exscale = rep(-1.18, 5), Outscale = rep(-.647, 5), 
                      Tempscale = filter(SoDeltasum, Yearf == '2021')$Tempscale,
                      Secchs = filter(SoDeltasum, Yearf == '2021')$Secchs,
                      day = c(165,190, 224, 252), Yearf = "2021", Scenario = "1500 CFS")

newdata3e = data.frame(Exscale = rep(-.908, 4), Outscale = rep(-.647, 4), 
                      Tempscale = filter(SoDeltasum, Yearf == '2021')$Tempscale,
                      Secchs = filter(SoDeltasum, Yearf == '2021')$Secchs,
                      day = c(165,190, 224, 252), Yearf = "2021", Scenario = "2500 CFS")

newdata4e = data.frame(Exscale = rep(-.4995, 4), Outscale = rep(-.647, 4), 
                       Tempscale = filter(SoDeltasum, Yearf == '2021')$Tempscale,
                       Secchs = filter(SoDeltasum, Yearf == '2021')$Secchs,
                       day = c(165,190, 224, 252), Yearf = "2021", Scenario = "4000 CFS")
allnewe = bind_rows(newdata2e, newdata3e, newdata4e )

library(tidybayes)

Predictionse = add_epred_draws(allnewe, M5.61)%>%
  median_qi(.epred)

#This is Figure 2-34 in the report
ggplot(filter(Predictionse),  aes(x = as.factor(day), y = .epred, fill = Scenario)) + 
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = .lower, ymax = .upper, group = Scenario), position = "dodge")+
  facet_wrap(~.category)+
  scale_fill_manual(values = c("grey", "darkgreen", "lightblue"), name = "Export Scenario")+
  scale_x_discrete(labels = c("June", "July", "August", "Sept"), name = "Month")+
  theme_bw()+ylab("Probability")

diffe = group_by(Predictionse, HABs, day) %>%
  summarize(Difference = Probability[1]-Probability[2])


