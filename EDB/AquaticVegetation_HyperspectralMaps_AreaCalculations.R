#Drought Barrier
#Hyperspectral imagery coverage data
#Franks Tract, Big Break, and Clifton Court

#to do list
#correlate areas with water quality and with herbicide quantities
#make polished versions of bar graphs
#add indicator for missing data years?
#could color by water year type

#load packages
library(tidyverse) #variety of data science tools
library(lubridate) #format date/time
library(PerformanceAnalytics) #plotting correlations
library(DEGreport) #adds corr and p to plots
library(ggcorrplot) #plotting correlation matrix

#correlations to run
#areas of SAV vs fluridone quantity and fluridone acres treated
#areas of SAV vs each water quality parameter
#how much can we combine these into one correlation matrix?


#read in data ------------------

#area estimates for the three regions

#2004-2008, 2014-2015, 2019-2020
cc <- read_csv("EDB/weeds_regional_area_estimates/CliftonCourt_wgs84_area_ha.csv")%>% 
  add_column("site" = as.factor("Clifton Court")) 

#2004-2008, 2014-2020
ft <- read_csv("EDB/weeds_regional_area_estimates/FranksTract_wgs84_area_ha.csv")%>% 
  add_column("site" = as.factor("Franks Tract"))

#2004-2008, 2014-2020
bb <- read_csv("EDB/weeds_regional_area_estimates/BigBreak_wgs84_area_ha.csv")%>% 
  add_column("site" = as.factor("Big Break")) 

#read in sonde data (2015-2021)
wq <- read_csv("EDB/frk_sonde_data_summary.csv")

#read in discrete water quality data 
#EMP stations of interest are D19 (Franks Tract) and C9 (Clifton Court)
#Bay Study station of interest is 853 (near Big Break)
dwq <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.731.3&entityid=6c5f35b1d316e39c8de0bfadfb3c9692")

#read in fluridone application data (2013-2021)
herb <- read_csv("EDB/franks_tract_fluridone_applications_2006-2021.csv")

#combine veg area estimates data into one data set-----------

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
#add symbol for missing data
#use better colors
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

#format discrete water quality data-----------

dwq_format <- dwq %>% 
  #keep just the stations of interest
  filter(Station == "EMP C9" | Station == "EMP D19" | Station == "Baystudy 853") %>% 
  filter(Date > "2003-12-31") %>% 
  mutate(month = month(Date)
         ,year = year(Date)) %>%
  glimpse()
#need to look close at which surveys/stations have which water quality parameters
#perhaps ideally we would pick the three months that are immediately prior
#to the remote sensing, which varies among years

#figure out which WQ parameters are available for each of the three stations
#dwq_sum <- dwq_format %>% 
 # group_by(Station) %>% 
  #summarise_all(funs(sum(!is.na(.))))
dwq_sum <- dwq_format %>% 
  group_by(Station) %>% 
summarise(across(where(is.numeric),
                 mean,
                 na.rm = T))

#filter out just the needed WQ parameters
dwq_par <- dwq_format %>%
  select(Station, Date, year, month, Tide, Temperature, Conductivity, Salinity, Secchi, DissolvedOxygen, pH)
#no secchi for C9
#no DO or pH for 853

#look at number of measurements for each parameter by station and year
dwq_sum2 <- dwq_par %>% 
  group_by(Station,year) %>% 
  summarize_all(funs(sum(!is.na(.))))
#C9 only includes 2016-2020
#other two sites include 2004-2020

#work on selecting the correct months of WQ data for calculating means to compare with veg data
#start by creating a df with year and month for veg imaging
vtime <- all %>% 
  #just unique combos of year and month
  distinct(year,month) 
#come back to this later

#create data frame to match WQ stations and sites
stm <- as.data.frame(
  cbind(
  Station = c("Baystudy 853", "EMP C9", "EMP D19")
  , site = c("Big Break","Clifton Court","Franks Tract")
)
)

#for now, just use WQ from months March to Oct
wq_avg <-dwq_par %>% 
  filter(month > 2 & month < 11) %>%
  select(-month) %>% 
  group_by(Station,year) %>% 
  summarise(across(where(is.numeric),
                   mean,
                   na.rm = T)) 

#add region info to WQ data set
wr <- left_join(wq_avg,stm)

#then add veg data
wrv <-left_join(all,wr)

#format fluridone application data------------

herb_format <- herb %>% 
  mutate(
    #calculate lbs of fluridone used per site and year
    quantity_lbs = area_acres * depth_ft * (rate_ppb/367.73331896144)
  ) %>% 
  #sum acres and lbs of fluridone by year
  group_by(year) %>% 
  summarise(area_acres_tot = sum(area_acres)
            ,quantity_lbs_tot = sum(quantity_lbs)) %>% 
  mutate(
    #calculate rate in ppb across all three Franks Tract sites
    #mean depth was alway 7.9 ft
    fl_rate_ppb = (quantity_lbs_tot / (area_acres_tot * 7.9))*367.73331896144
    #convert acres to hectares
    ,fl_area_ha = area_acres_tot * 0.404686
    #convert lbs to kg
    ,fl_quantity_kg = quantity_lbs_tot * 0.4535924 
    #make year an integer
    ,year = as.integer(year)
  ) %>% 
  select(-c(area_acres_tot,quantity_lbs_tot)) %>% 
  glimpse()

#format sonde water quality data-----------------

#need to convert long to wide for correlation matrix
wq_format <- wq %>% 
  select(-value_se) %>% 
  pivot_wider(
    id_cols=c(year)
    ,names_from = parameter
    , values_from= value_mean
  ) %>% 
  mutate(year = as.integer(year)) %>% 
  glimpse()


#make correlation plots for veg area types------------

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

#correlations between SAV acreage and fluridone applications in Franks Tract----------

#join veg acreages and fluridone data sets by year
frankf <- left_join(all,herb_format) %>% 
  filter(site=="Franks Tract") %>% 
  glimpse()

#plot correlation between SAV acreage and fluridone quantities 
#can only compare 2014-2020 (and eventually 2021)
#could request older treatment data from DBW
#DBW website doesn't have reports for years I need (2004-2008)
ggplot(frankf, aes(sav,fl_quantity_kg))+ 
  geom_smooth(method = "lm")  + 
  geom_point() +
  geom_text(aes(label=year)
            , vjust = -0.9
  )+
  geom_cor(method = "pearson")+
  xlab("Area of SAV (ha)")+
  ylab("Quantity of fluridone applied (kg))")
#correlation not significant

#plot correlation between SAV acreage and treatment acreage 
#can only compare 2014-2020 (and eventually 2021)
#could request older treatment data from DBW
#DBW website doesn't have reports for years I need (2004-2008)
ggplot(frankf, aes(sav,fl_area_ha))+ 
  geom_smooth(method = "lm")  + 
  geom_point() +
  geom_text(aes(label=year)
            , vjust = -0.9
  )+
  geom_cor(method = "pearson")+
  xlab("Area of SAV (ha)")+
  ylab("Area treated with fluridone (ha))")
#set y-axis range so it doesn't go below zero
#significant correlation
#hard to know if incrase is due to lack of treatment or DBW giving up because of veg intensity

#correlations among water quality parameters and SAV acreage----------------
#consider using EMP discrete data instead of sonde data
#which would be less data per year but would include more years of veg data
#sonde data starts 2015 while EMP discrete likely goes back much farther

#reduce acreage data set to just year and sav acreage
frankfs <- frankf %>% 
  select(year,sav) 

#join sav acreage and wq data
frankw <- left_join(frankfs,wq_format) %>% 
  #drop years with no WQ data
  filter(year>2014)

#create correlation matrix
corr_matrix <- round(cor(frankw[2:8]),2)

#grid of correlations
ggcorrplot(corr_matrix, method ="square", type="lower", p.mat = corrp_matrix, lab=T)
#some WQ parameters are strongly correlated but not with SAV

# Computing correlation matrix with p-values
corrp_matrix <- cor_pmat(frankw[2:8])

# Visualizing the correlation matrix
ggcorrplot(corr_matrix, method ="square", type="lower")













