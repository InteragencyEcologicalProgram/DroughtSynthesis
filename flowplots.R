#Flow pots

library(tidyverse)
library(lubridate)
library(readxl)
library(visreg)

library(cder)
library(DroughtData)

#dayflow data
load("data/dayflow.RData")
yrs = read_csv("data/yearassignments.csv")

#Delta outflow, Sacramento flow, SJR flow, etc, from 2022
DTO = cdec_query("DTO",23, start.date = as.Date("2021-10-1"), end.date= as.Date("2022-10-1"))
SAC = cdec_query("FPT", 20,"D", start.date = as.Date("2021-10-1"), 
                 end.date= as.Date("2022-10-1"))
SJR = cdec_query("VNS", 41,"D", start.date = as.Date("2021-10-1"), 
                 end.date= as.Date("2022-10-1"))
Exporst =  cdec_query(c("TRP","HRO"), 70,"D", start.date = as.Date("2021-10-1"), 
                      end.date= as.Date("2022-10-1"))
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
