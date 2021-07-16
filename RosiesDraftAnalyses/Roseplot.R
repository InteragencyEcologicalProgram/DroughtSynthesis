#test out a rose plot

library(tidyverse)
library(readxl)

testdat = read_excel("testpolarchart.xlsx")
testdat$Metric = factor(testdat$Metric, levels = unique(testdat$Metric),
                        labels = c("smelt population","salmon","silversides","salinity","temperature","outflow" , "weeds",       
                                   "microcystis","phytoplankton", "contaminants", "smelt life history"))

ggplot(testdat, aes(x=Category, group = Metric)) +
    geom_hline(yintercept = seq(0, 5, by = 1),
             color = "grey", size = 1) +
  geom_col(aes(fill = Uncertainty, y = length), position =position_dodge2(width = 1, preserve = "single"))+
  geom_vline(xintercept = seq(.5, 16.5, by = 1),
             color = "grey", size = 1) +
  scale_fill_manual(values = c("red", "darkcyan", "orange", "grey"), labels = c("High", "Low", "Med", "????"))+
  geom_text(aes(label = Metric, y = length), position = position_dodge(.9))+
  
 # annotate("text", x = rep("water", 5), y = c(.9, 1.9, 2.9, 3.9, 4.9), 
#           label = c("no impact", "minor impact", "major impact", "management trigger", "ecosystem shift"), size = 3)+
  coord_polar() + theme_bw()+
scale_y_continuous( name = NULL)


test2 = read.csv("RosiesDraftAnalyses/testdata.csv")
test2 = filter(test2, !is.na(Year))
test2w = group_by(test2, Drought) %>%
  summarize(mpred = mean(Predators, na.rm = T), sdpred = sd(Predators, na.rm = T),
            mzoops = mean(zoopBPUE, na.rm = T), sdzoops = sd(zoopBPUE),
            mchla = mean(chla, na.rm = T), sdcla = sd(chla), mtemp = mean(temp))

test2$Drought = factor(test2$Drought, levels = c("D", "N", "W"), labels = c("Multi-Year \n Drought", "Neither", "Multi-Year \n Wet"))
test3 = filter(test2, Drought !=  "Neither")
ggplot(test2w, aes(x = Drought, y = mpred))+
  geom_col() + geom_errorbar(aes(ymin = mpred -sdpred, ymax = mpred+sdpred))

ggplot(test2, aes(x = Drought, y = Predators, fill = Drought)) + geom_boxplot()
ggplot(test2, aes(x = Drought, y = zoopBPUE)) + geom_boxplot()
ggplot(test2, aes(x = Drought, y = FMWTIndex)) + geom_boxplot()
ggplot(test2, aes(x = Drought, y = chla)) + geom_boxplot()

ggplot(test3, aes(x = Drought, y = Predators, fill = Drought)) + geom_boxplot()+
  xlab(NULL)+ylab("Striped Bass Index (FMWT)") + theme_bw()
ggplot(test3, aes(x = Drought, y = zoopBPUE, fill = Drought)) + geom_boxplot()+
  ylab("Mean Zooplankton Biomass per cubic meter")+ theme_bw()
ggplot(test3, aes(x = Drought, y = FMWTIndex, fill = Drought)) + geom_boxplot()+
  ylab("Delta Smelt Index (FMWT)")+ theme_bw()
ggplot(test3, aes(x = Drought, y = chla, fill = Drought)) + geom_boxplot()+
  ylab("Mean Chlorophyll-a in ug/L")+ theme_bw()

ggplot(test3, aes(x = Drought, y = temp, fill = Drought)) + geom_boxplot()+
  ylab("Water Temperature (C)")+ theme_bw()
ggplot(test3, aes(x = Drought, y = secchi, fill = Drought)) + geom_boxplot()+
  ylab("secchi depth (cm)")+ theme_bw()


#Test some other things
Nutrients <- read_csv("~/Drought/FLOATDrought/Analyses/Nutrients.csv")
yeartypes <- read_csv("~/Drought/FLOATDrought/yeartypes.csv")
Nuts = left_join(Nutrients, yeartypes)
Nuts2 = group_by(Nuts, Year, Drought, Index) %>%
  summarize(chla = mean(Chla, na.rm = T))

ggplot(Nuts2, aes(x = Year, y = chla, color = Drought)) + geom_line()
ggplot(Nuts2, aes(x = Index, y = chla, color = Drought)) + geom_line()
ggplot(Nuts2, aes(x = Drought, y = chla, fill = Drought)) + geom_boxplot()
ggplot(filter(Nuts, Drought != "N", !is.na(Drought)), 
       aes(x = Drought, y = log(Chla), fill = Drought)) + geom_boxplot() + facet_wrap(~Season)
