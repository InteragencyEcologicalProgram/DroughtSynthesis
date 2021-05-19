#Quick plot of the major droughts and stuff

library(waterYearType)
library(tidyverse)

indecies = water_year_indices
i1820 = data.frame(WY = c(2018,2019,2020, 2018,2019,2020), Index = c(7.14, 10.34, 6.0, 3.03, 4.94, 2.1), 
                   Yr_type = c("Below Normal", "Wet", "Dry", "Below Normal", "Wet", 
                              "Critical"), location = c(rep("Sacramento Valley", 3), rep("San Joaquin Valley", 3)))
indecies = bind_rows(indecies, i1820)

ggplot(indecies, aes(x = WY, y = Index, fill = Yr_type))+
  scale_fill_manual(values = c("chartreuse3", "darkorange", "firebrick", "firebrick1", "dodgerblue"))+
  geom_bar(stat = "identity")+
  facet_grid(location~., scales = "free_y")+
  coord_cartesian(xlim = c(1960, 2020))+
  theme(legend.position = "bottom")

#now how about X2?

