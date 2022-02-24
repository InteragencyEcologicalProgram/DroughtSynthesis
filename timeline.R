#random salvage data
#also a timeline

library(tidyverse)
library(lubridate)
library(vistime)

##################################################3
timeline = read_excel("data/drought timeline.xlsx")
timeline = filter(timeline, Category != "NA")


gg_vistime(timeline, col.event = "ShortName", 
           col.start = "Start", col.end = "End", col.group = "Category", col.color = "color", linewidth = 15)

gg_vistime(timeline, col.event = "ShortName", 
           col.start = "Start", col.end = "End", col.group = "Cat2", col.color = "color", linewidth = 15)
