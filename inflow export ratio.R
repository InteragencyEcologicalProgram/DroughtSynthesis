#steve wants to know about inflow/export ratios

library(DroughtData)
library(tidyverse)
library(lubridate)

yrs = read_csv("data/yearassignments.csv") %>%
  rename(YearAdj = Year)
hydro = raw_hydro_1975_2022
names(hydro)

hydro = mutate(hydro, ExIn = Export/InflowTotal, Month = month(Date))
ggplot(hydro, aes(x = Date, y = ExIn))+ geom_point()+ geom_line()

hydro = left_join(hydro, yrs) 

hydrosum = group_by(hydro, YearAdj, Season, Whitepaper, Month, Yr_type) %>%
  summarise(ExIn = mean(ExIn, na.rm = T)) %>%
  mutate(Whitepaper = factor(Whitepaper, levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet", "2020", "2021", "2022"),
                        labels = c("Critical", "Dry", "Below \nNormal", "Above \nNormal", "Wet", "2020", "2021", "2022")))
ggplot(hydrosum, aes(x = Whitepaper, y = ExIn))+ geom_boxplot(aes(fill = Yr_type)) +
  facet_wrap(~Season)+ drt_color_pal_yrtype()+ theme_bw()+ ylab("Export to Inflow Ratio")

ggplot(hydrosum, aes(x = Whitepaper, y = ExIn))+ geom_boxplot(aes(fill = Yr_type)) +
 drt_color_pal_yrtype()+ theme_bw()+ ylab("Export to Inflow Ratio")+ xlab(NULL)+
  theme(legend.position = "none")

ggsave("plots/whitepaper/FlowRatio.tiff", device = "tiff", width = 8, height = 6)
