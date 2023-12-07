#Flow pots

library(tidyverse)
library(lubridate)
library(readxl)
library(visreg)

library(cder)
library(DroughtData)

#dayflow data
load("data/Dayflow.RData")
yrs = read_csv("data/yearassignments.csv")

#Delta outflow, Sacramento flow, SJR flow, etc, from 2022
DTO = cdec_query("DTO",23, start.date = as.Date("2021-10-1"), end.date= as.Date("2023-7-1"))
SAC = cdec_query("FPT", 20,"D", start.date = as.Date("2021-10-1"), 
                 end.date= as.Date("2023-7-1"))
SJR = cdec_query("VNS", 41,"D", start.date = as.Date("2021-10-1"), 
                 end.date= as.Date("2023-7-1"))
Exporst =  cdec_query(c("TRP","HRO"), 70,"D", start.date = as.Date("2021-10-1"), 
                      end.date= as.Date("2023-7-1"))
flow2022 = data.frame(Date = SAC$DateTime, OUT = DTO$Value, SJR = SJR$Value,
                      SAC = SAC$Value, CVP = filter(Exporst, StationID == "HRO")$Value,
                      SWP = filter(Exporst, StationID == "TRP")$Value)

DF2 = bind_rows(DF, flow2022) %>%
  mutate(Yearx = year(Date), 
         Month = month(Date),
         Day = yday(Date),
         wyDay = case_when(Day >= 274 ~ Day -274,
                           Day < 274 ~ Day +91),
         Year = case_when(Month %in% c(10,11,12) ~ Yearx+1,
                          TRUE ~ Yearx),
         Pumps = CVP+SWP) %>%
  left_join(yrs)


DF3 = pivot_longer(DF2, cols = c(SAC, OUT, EXPORTS, SJR, GCD, CVP, SWP, MISDV, Pumps),
                   names_to = "Station", values_to = "Flow") %>%
  filter(Flow >0, !Station %in% c("EXPORTS", "GCD", "MISDV", "CVP", "SWP")) %>%
  mutate(Station = factor(Station, levels = c("OUT", "Pumps", "SAC", "SJR"),
                          labels = c("Delta Outflow", "SWP+CVP Exports",
                                     "Sacramento at Freeport", "San Joaquin at Vernalis")),
         Whitepaper = factor(Whitepaper, levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet", "2020", "2021", "2022")))


ggplot(DF3, aes(x = Date, y = Flow, color = Station)) + geom_line()
ggplot(filter(DF3, Station == "SWP+CVP Exports", Year >2000, Year <2023),  aes(x = Day, y = Flow, color = Yr_type)) + 
  geom_point()+ geom_smooth()

ggplot(filter(DF3, Station == "Delta Outflow", Year >2000, Year <2023),  aes(x = Day, y = Flow, color = Yr_type)) + 
  geom_point()+ geom_smooth()
########################################################################

#Plot of change in exports over time for SBDS paper
DFyear = group_by(DF3, Yr_type, Year, Index, Station) %>%
  summarize(AnnualFlow = mean(Flow, na.rm =T))


ggplot(DFyear, aes(x = Year, y = AnnualFlow, color = Station))+ geom_line()+
  facet_wrap(~Station, scales = "free_y")+ theme_bw()

#list of droughts
droughts = data.frame(droughtstart = rep(c(1976, 1987, 2007, 2012, 2020),4),
                      Station = c(rep("Delta Outflow",5), rep("SWP+CVP Exports",5),
                                  rep("Sacramento at Freeport",5), rep("San Joaquin at Vernalis",5)),
                      droughtend = rep(c(1977, 1994, 2009, 2016, 2022),4),
                      ymax = c(rep(90000,5), rep(10000,5), rep(50000, 5), rep(20000, 5)))

ggplot(DFyear, aes(x = Year, y = AnnualFlow, color = Station))+ geom_line(linewidth = 1)+
  facet_wrap(~Station, scales = "free_y")+ 
  scale_color_brewer(palette = "Dark2", guide = NULL)+
  geom_rect(data = droughts, aes(xmin = droughtstart, xmax = droughtend+1, ymin = 0, ymax = ymax), inherit.aes = F,
            alpha = 0.3)+
    theme_bw()+
  ylab("Average Annual Flow (CFS)")+
  xlab("Water Year")

#we decided to just do outflow and exports
ggplot(filter(DFyear, Station %in% c("Delta Outflow", "SWP+CVP Exports")),
              aes(x = Year, y = AnnualFlow, color = Station))+ geom_line(linewidth = 1)+
  facet_wrap(~Station, scales = "free_y", nrow =2)+ 
  scale_color_brewer(palette = "Dark2", guide = NULL)+
  geom_rect(data = filter(droughts, Station %in% c("Delta Outflow", "SWP+CVP Exports")),
            aes(xmin = droughtstart-.5, xmax = droughtend+.5, ymin = 0, ymax = ymax), inherit.aes = F,
            alpha = 0.3)+
  theme_bw()+
  ylab("Average Annual Flow (CFS)")+
  xlab("Water Year")

ggsave("plots/OutflowExports.tiff", device = "tiff", width = 8, height =6)


#########################################################
library(zoo)
DF_WYT = group_by(DF3, Yr_type, wyDay, Station) %>%
  summarize(Flow = mean(Flow, na.rm = T))

test = DF_WYT %>%
  group_by(Yr_type, Station) %>%
  mutate(roll = rollmean(Flow, k = 7, fill = NA)) %>%
  ungroup()


ggplot(DF_WYT, aes(x = wyDay, y = Flow, color = Yr_type)) + geom_line()+
  facet_wrap(~Station, scales = "free_y")


ggplot(test, aes(x = wyDay, y = roll, color = Yr_type)) + geom_line()+
  facet_wrap(~Station, scales = "free_y")

ggplot(DF3, aes(x = wyDay, y = Flow, color = Yr_type, fill = Yr_type))+
  geom_smooth()+ facet_wrap(~Station, scales = "free_y")

Flow2122 = filter(DF3, Year %in% c(2020, 2021, 2022))

pal_yrt <- c( "Critical" = "#FDE333", "Dry" = "#53CC67", "Below Normal" = "#009B95", 
"Above Normal" = "#00588B","Wet" = "#481F70FF", "2020" = "black", "2021" = "grey40", "2022" = "grey80") 

#######################################################
#plot for white paper
ggplot(filter(DF3, !Year  %in% c(2020, 2021, 2022)), mapping = aes(x = wyDay, y = Flow, color = Whitepaper, fill = Whitepaper))+
  geom_smooth(size = 0.5, method = "gam", formula = y ~ s(x, bs = "cc"))+ 
  facet_wrap(~Station, scales = "free_y")+
  #drt_color_pal_yrtype(aes_type = "color", 
   #                                scale_title =  "Long-Term\nAverage")+
  #drt_color_pal_yrtype(aes_type = "fill", scale_title =  "Long-Term\nAverage")+
  geom_line(data = Flow2122,
              mapping = aes(x = wyDay, y = Flow, color = Whitepaper),
            inherit.aes = FALSE, size = 1)+
  theme_bw()+ 
  scale_fill_manual(values = pal_yrt, name = NULL)+
  scale_color_manual(values = pal_yrt, name = NULL)+
  ylab("Flow (CFS)")+
  scale_x_continuous(breaks = c(0, 90, 180, 270), 
                     labels = c("Oct", "Jan", "Apr", "Aug"))+
  theme(legend.position = "bottom")

ggsave("plots/flowplots.tiff", device = "tiff", width = 9, height = 7, units = "in")

################################################################
#Box plot of % diverted

DFsum = pivot_wider(DF3, names_from = Station, values_from = Flow) %>%
  mutate(PercetDiverted = `SWP+CVP Exports`/(`Sacramento at Freeport`+ `San Joaquin at Vernalis`),
         Season = case_when(Month %in% c(3,4,5) ~ "Spring",
                            Month %in% c(6,7,8) ~ "Summer",
                            Month %in% c(9,10,11) ~ "Fall",
                            Month %in% c(12,1,2) ~ "Winter"),
         Season = factor(Season, levels = c("Winter", "Spring", "Summer", "Fall")))

DFsum2 = group_by(DFsum, Season, Year, Month, Yr_type, Whitepaper) %>%
  summarize(PercentDiverted = mean(PercetDiverted)) %>%
  filter(!is.na(Whitepaper))

group_by(DFsum2, Whitepaper) %>%
  summarize(PercentDiverted = mean(PercentDiverted, na.rm =T))


ggplot(DFsum2, aes(x = Whitepaper, y = PercentDiverted))+ geom_boxplot(aes(fill = Yr_type)) +
  facet_wrap(~Season)+ drt_color_pal_yrtype()+ theme_bw()+ ylab("Export to Inflow Ratio")

ggplot(DFsum2, aes(x = Whitepaper, y = PercentDiverted))+ geom_boxplot(aes(fill = Yr_type)) +
   drt_color_pal_yrtype()+ theme_bw()+ ylab("Export to Inflow Ratio")+
  scale_x_discrete(labels = c("Critical", "Dry", "Below\nNormal", "Above\nNormal", "Wet", "2020", "2021", "2022"))+
  xlab(NULL)+ theme(legend.position = "none")

ggsave("plots/whitepaper/FlowRatio.tiff", device = "tiff", width = 8, height = 6)

DFsum3 = group_by(DFsum, Year, Yr_type) %>%
  summarize(PercentDiverted = mean(PercetDiverted, na.rm =T)) 

ggplot(DFsum3, aes(x = Year, y = PercentDiverted))+ geom_line() +
  geom_rect(data = filter(droughts, Station =="Delta Outflow"),
            aes(xmin = droughtstart, xmax = droughtend+1, ymin = 0, ymax = 1), 
            inherit.aes = F,alpha = 0.3)+
  theme_bw()+
  ylab("Annual Percent of inflow diverted")+
  xlab("Water Year")


#############################
