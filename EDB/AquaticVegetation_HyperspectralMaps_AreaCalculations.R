#Drought Barrier
#Hyperspectral imagery coverage data
#Franks Tract and Clifton Court

#to do list
#make polished versions of bar graphs
#add indicator for missing data years?
#could color by water year type

#load packages
library(tidyverse) #variety of data science tools
library(PerformanceAnalytics) #plotting correlations

#set working directory
setwd("C:/Users/nrasmuss/OneDrive - California Department of Water Resources/Drought/DroughtBarrier")

#read in data
cc <- read_csv("./Regional_Area_Estimates/CliftonCourt_wgs84_area_ha.csv")%>% 
  add_column("site" = as.factor("Clifton Court")) 

ft <- read_csv("./Regional_Area_Estimates/FranksTract_wgs84_area_ha.csv")%>% 
  add_column("site" = as.factor("Franks Tract"))

bb <- read_csv("./Regional_Area_Estimates/BigBreak_wgs84_area_ha.csv")%>% 
  add_column("site" = as.factor("Big Break")) 

#combine data into one data set
all <- bind_rows(ft,cc,bb)%>% 
  rename(year_month = Year) %>% 
  mutate(
    #create year column from year-month
    year=as.integer(str_sub(year_month,1,4))
    #create month column from year-month
    ,month=as.integer(str_sub(year_month,5,6))
    #sum all acreage categories to get total area
    ,total = rowSums(across(soil:shadow))
    #create column that sums the two FAV species
    ,fav = hyacinth + primrose
    #calculate proportion of area that is SAV
    ,sav_prop = sav/total
    #calculate proportion of area that is FAV
    ,fav_prop = fav/total
         )  %>% 
  #reduce to just needed columns and reorder them
  select(
    year
    ,month
    ,site
    ,sav
    ,fav
    ,sav_prop
    ,fav_prop
  ) %>% 
  glimpse()

#look at number of years for each imaging month
season <- all %>% 
  distinct(year,month) %>% 
  group_by(month) %>% 
  summarize(count = n())
  
  

#total veg: convert wide to long
veg_tot <- all %>% 
  select(year:fav) %>% 
  pivot_longer(c(sav:fav), names_to = "type_total", values_to = "area_ha") %>% 
  mutate(across(c("type_total"), as.factor))  

#proportion veg: convert wide to long
veg_prop <- all %>% 
  select(year:site,sav_prop:fav_prop)%>% 
  pivot_longer(c(sav_prop:fav_prop), names_to = "type_prop", values_to = "area_prop") %>% 
  mutate(across(c("type_prop"), as.factor)) 

#total area: stacked bar plots
ggplot(veg_tot, aes(x=year, y=area_ha,  fill = type_total))+
    geom_bar(position = "stack", stat = "identity") + 
    ylab("Vegetation Coverage (ha)") + xlab("Year") + 
   scale_fill_discrete(labels=c("FAV","SAV")) +
    facet_grid(site~.)

#proportion area: stacked bar plots
ggplot(veg_prop, aes(x=year, y=area_prop,  fill = type_prop))+
    geom_bar(position = "stack", stat = "identity") + 
    ylab("Vegetation Coverage Proportion") + xlab("Year") + 
    scale_fill_discrete(labels=c("FAV","SAV")) +
    facet_grid(site~.)

#make correlation plots------------

#Clifton Court: correlations among all land surface types

#need to drop the years with missing data first
ccz <- cc %>% 
  filter(sav!=0)

chart.Correlation(ccz[2:10])
#significant correlations, highest to lowest
#water vs SAV (-1.00)
#primrose vs shadow
#npv vs soil
#npv vs sav
#npv vs water
#primrose vs riparian
#riparian vs emergent
#fav vs soil

#Franks Tract: correlations among all land surface types
chart.Correlation(ft[2:10])
#significant correlations, highest to lowest
#water vs SAV (-0.99)
#riparian vs emergent
#pennywort vs primrose
#primrose vs water
#sav vs primrose
#soil vs pennywort
#emergent vs hyacinth
#riparian vs hyacinth

#Big Bend: correlations among all land surface types
chart.Correlation(bb[2:10])

#make comparisons among sites within land types

#format data sets
veg_corr <- all %>% 
  select(year:site,sav_prop:fav_prop) %>% 
  pivot_wider(id_cols = c(year,month), names_from = site, values_from = c(sav_prop,fav_prop)) %>%
  #rename columns
  rename(
    sav_prop_ft = "sav_prop_Franks Tract" 
    ,sav_prop_bb = "sav_prop_Big Break"
    ,sav_prop_cc = "sav_prop_Clifton Court"
  ) %>% 
  glimpse()


#plot correlations
#chart.Correlation(bothw[c(2,3,6,7)])
#sav  corr is 0.74

#make slightly nicer plot focused on comparing SAV between sites
ggplot(veg_corr, aes(x=sav_prop_ft, y= sav_prop_bb))+
    geom_point() +
    geom_smooth(method = 'lm', se=F)+
    geom_text(aes(label=year)
              , vjust = -0.9
              )+
    xlab("Franks Tract SAV")+
    ylab("Big Break SAV ") 

ggplot(veg_corr, aes(x=sav_prop_ft, y= sav_prop_cc))+
  geom_point() +
  geom_smooth(method = 'lm', se=F)+
  geom_text(aes(label=year)
            , vjust = -0.9
  )+
  xlab("Franks Tract SAV")+
  ylab("Clifton Court SAV ") 


#next make this same plot but with % coverage instead of hectares
#the 1:1 line represents how the correlation would look if proportion of SAV
#coverage were the same at both sites across years
ggplot(veg_corr, aes(x=sav_prop_ft, y= sav_prop_bb,color=month))+
  geom_point() +
  geom_smooth(method = 'lm', se=T)+
  geom_abline(intercept = 0, slope=1,linetype="dashed")+
  geom_text(aes(label=year)
            , vjust = -0.9
  )+
  xlab("Franks Tract SAV")+
  ylab("Big Break SAV ") 

ggplot(veg_corr, aes(x=sav_prop_ft, y= sav_prop_cc,color=month))+
  geom_point() +
  geom_smooth(method = 'lm', se=T)+
  geom_abline(intercept = 0, slope=1,linetype="dashed")+
  geom_text(aes(label=year)
            , vjust = -0.9
  )+
  xlab("Franks Tract SAV")+
  ylab("Clifton Court SAV ") 


#anomaly plots---------

#add the necessary columns
botha <- bothw %>% 
  mutate(sav_ft_an = sav_ft-mean(sav_ft)) %>% 
  mutate(sav_cc_an = sav_cc-mean(sav_cc,na.rm=T)) %>% 
  glimpse()

#franks tract
(plot_ft_an <-ggplot(botha, aes(x=Year, y= sav_ft_an))+
    geom_bar(stat="identity")+
    ylab("Vegetation Coverage (hectares)") + xlab("Year") +
    ggtitle("Franks Tract")
    )
#could use a different color for drought barrier years


