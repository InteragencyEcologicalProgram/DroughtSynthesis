#water age modeling

library(tidyverse)
library(readxl)
library(lubridate)

waterage = read_excel("data/age_2015_2016_2021.xlsx")
ggplot(waterage, aes(x = Exports, y = Outflow, size = `Mean age`)) + geom_point()
ggplot(waterage, aes(x = Exports, y = `Mean age`, color = as.factor(Year))) + geom_point()
ggplot(waterage, aes(x = Outflow,  y = `Mean age`, color = as.factor(Year), shape = as.factor(Month))) + geom_point()

ggplot(waterage, aes(x = log(Outflow)+Exports,  y = `Mean age`, color = as.factor(Year), shape = as.factor(Month))) + geom_point()
