#Emergency Drought Barrier
#FRK sonde water quality data
#calculate annual means and standard errors
#use to determine if aquatic weed species abundance correlate with water quality
#based on SePro vegetation surveys conducted annually in October

#packages
library(tidyverse) #suite of data science tools
library(lubridate) #formatting dates
library(ggcorrplot) #plotting correlation matrix
library(DEGreport) #adds corr and p to plots
library(DroughtData) #load Dave's drought synthesis WQ data package for Delta Outflow data
library(readxl) #read excel file
library(janitor) #clean up column names

#format Delta outflow data------------

#view(raw_hydro_1975_2021)
#glimpse(raw_hydro_1975_2021)

dout <- raw_hydro_1975_2021 %>% 
  #add month column
  mutate(month = as.integer(month(Date))) %>%  
  #just use March through Oct
  filter((month > 2 & month < 11) & YearAdj > 2003) %>%
  #calculate annual means
  group_by(YearAdj) %>% 
  summarize(out_mean = mean(Outflow)) %>% 
  rename(year = YearAdj)


#Read in discrete WQ data----------------

#read in discrete water quality data (note this is a large file)
#EMP stations of interest are D19 (Franks Tract) and C9 (Clifton Court)
#Bay Study station of interest is 853 (near Big Break)
#Note that it doesn't yet include the 2021 data I need
dwq <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.731.3&entityid=6c5f35b1d316e39c8de0bfadfb3c9692")

#for 2021 Big Break data use Blind Point sonde because don't have Bay Study 853 data
#should just request the data from Bay Study
#got Blind Point data from WDL 
#https://wdl.water.ca.gov/WaterDataLibrary/StationDetails.aspx?Station=B9502900&source=map

#unique(bp21_temp$quality_code) #"1: Good data"      "151: Data Missing"

bp21_temp <- read_csv("EDB/BlindPoint_B9502900_Water_Temperature_Daily_Mean.csv",skip=8) %>% 
  clean_names() %>% 
  mutate(date = mdy_hms(date_time)) %>% 
  filter(quality_code =="1: Good data") %>% 
  rename(temp = water_temperature_daily_mean_degc) %>% 
  select(-c(date_time,quality_code)) %>% 
  glimpse()


bp21_ec <- read_csv("EDB/BlindPoint_B9502900_Conductivity_Daily_Mean.csv",skip=8) %>% 
  clean_names() %>% 
  mutate(date = mdy_hms(date_time)) %>% 
  filter(quality_code =="1: Good data") %>% 
  rename(ec = conductivity_daily_mean_misc) %>% 
  select(-c(date_time,quality_code)) %>% 
  glimpse()

bp21 <- full_join(bp21_temp,bp21_ec) %>% 
  filter(date > "2021-02-28" & date < "2021-11-01")  %>%
  summarise(across(where(is.numeric),
                   mean,
                   na.rm = T))  


#Dave data package doesn't include all the WQ data I need for my stations (as of 4/7/22)
#view(raw_wq_1975_2021)
#glimpse(raw_wq_1975_2021)

#wq <- raw_wq_1975_2021 %>% 
  #just keep the two most relevant stations and months
  #data package doesn't include Bay Study so no Bay Study 853 station
  #also doesn't have all the months of data I need so use the EDI package + EMP data request for 2021
 # filter((Station == "D19" | Station == "C9") & (Month > 2 & Month < 11))

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
  select(Station, Date, year, month, Temperature, Conductivity, Salinity, Secchi, DissolvedOxygen, pH)
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
#vtime <- alln %>% 
  #just unique combos of year and month
 # distinct(year,month) 
#come back to this later

#for now, just use WQ from months March to Oct
wq_msub <-dwq_par %>% 
  filter(month > 2 & month < 11) %>% 
  select(-month)

#requested 2021 data for C9 and D19 from EMP 
dwq21 <- read_excel("EDB/2021 DEMP Sonde Data_C9_D19.xlsx") %>% 
  #clean up column names
  clean_names() %>% 
  #split date and time because formatting as date time was causing problems for some reason
  separate("sample_date", into=c("date","time"),sep=" ",remove=T)%>% 
  #format date
  mutate(Date = mdy(date)) %>% 
  #look at column types
  glimpse()

# Making data frame with existing station strings and their replacement
stn <- data.frame(target = c("C9 - West Canal @ Clifton Court Intake","D19 - Frank's Tract near Russo's Landing"),
                 replacement = c("EMP C9","EMP D19"))

# Making the named replacement vector from tr
replacements <- c(stn$replacement)
names(replacements) <- c(stn$target)

#format 2021 WQ data
dwq21_format <- dwq21 %>% 
  mutate(
    #simplify station names
    Station = str_replace_all(station_name,pattern = replacements)
    #make secchi depth column numeric even though there are NAs for C9
    ,secchi = as.numeric(secchi_depth_centimeters_secchi_depth_1)
    #add a year column
    ,year = year(Date)
    ) %>%
  #filter to just the months I need
  #March to Oct
  filter(Date > "2021-02-28" & Date < "2021-11-01") %>% 
  #simply parameter names and make them match Sam's EDI data set
  rename(Temperature = water_temperature_c_epa_170_1_field_1
         , Conductivity = specific_conductance_u_s_cm_25_c_epa_120_1_field_1
         , Secchi = secchi                                  
         , DissolvedOxygen = dissolved_oxygen_mg_l_astm_method_d888_09_c_field_1  
         , pH = p_h_p_h_units_epa_150_1_field_1                                          
  ) %>% 
  #keep just the needed columns
  select(Station
         , Date
         , year
         , Temperature
         , Conductivity
         , Secchi
         , DissolvedOxygen
         , pH) %>% 
  glimpse()
  
  
#combine 2021 data with rest of years
wq_all <- bind_rows(wq_msub,dwq21_format) %>% 
  glimpse()

#calculate means for each parameter and year
wq_avg <- wq_all %>% 
  group_by(Station,year) %>% 
  summarise(across(where(is.numeric),
                   mean,
                   na.rm = T),.groups = 'drop') %>% 
  glimpse()

#adding a row for 2021 for Big Break
#don't have BS 853 data for 2021 yet
#also add temp and EC data from Blind Point sonde station (see bp21)

wq_avg2 <- wq_avg %>% 
  add_row(Station = "Baystudy 853"
          ,year = as.numeric("2021")
          ,Temperature = as.numeric("19.33185")
          ,Conductivity= as.numeric("2168.672")
          ,Salinity=as.numeric("")
          ,Secchi=as.numeric("")
          ,DissolvedOxygen =as.numeric("")
          ,pH  =as.numeric(""))

#combine WQ with outflow
#this will match WQ data set by year which will add this deltawide measure to all stations
#but that's fine given how I will use the data
wqout <- full_join(wq_avg2,dout) %>% 
  arrange(Station, year)

#write summary stats file 
#write_csv(wqout,"EDB/discrete_wq&outflow_data_summary.csv")


#Franks Tract sonde: data formatting----------------------

#read and combine all FRK sonde WQ data files
#only covers 2015 to present
wq_list <- dir(path = "EDB/FRK_data" ,pattern = "\\.csv", full.names = T, recursive=T)
frk <- map_dfr(wq_list, ~read_csv(.x)) %>% 
  glimpse()

#look at date ranges
#includes data from station installation to Nov 2021
range(frk$time) #"2015-07-01 00:00:00 UTC" "2021-11-20 23:45:00 UTC"
#Note that annual means for 2015 and (temporarily) 2021 will be based on fewer data 
#so they will be skewed
#could drop 2015 or just use July 1- October 1 data for all years
#that would be the mean of the four months of data right before the veg surveys
#which probably works fine

#look at data quality categories
unique(frk$qaqc_flag_id)
#"G" "M" "X" "A" "U"
#A = added filler data
#G = Good
#M = Missing
#U = Unchecked
#I don't know what X means

#how many measurements of each type?
qual_ct<-frk %>% 
  group_by(parameter,qaqc_flag_id) %>% 
  summarize(count = n())  
#vast majority are G so I think we can just use those for our purposes

#filter out bad data
frk_clean <- frk %>% 
    mutate(
    #create month column
    month = as.integer(month(time))
    #create a year column
    ,year = as.integer(year(time))
    #make parameter a factor
    ,parameter = as.factor(parameter)
         ) %>% 
  #only keep the data categorized as good and data during months July to Sept
  filter(qaqc_flag_id=="G" & (month > 6 & month < 10)) %>% 
  #only keep needed columns
  select(parameter,year,month,time,value,unit) %>% 
  glimpse()

#plot time series of remaining data by parameter
#this takes a while to run because of the data set size
#ggplot(frk_clean, aes(x=time, y =value))+
 # geom_point()+
  #geom_line()+
  #facet_wrap(~parameter, scales = "free")

#Franks Tract sonde: look at correlations among the six parameters---------------

#start with frk df
#remove the bad data
#convert long to wide (a column for each parameter)
#make corr matrix
#plot corr matrix

#calculate annual means and standard errors

#standard error function
se <- function(x) sd(x)/sqrt(length(x))

frk_sum <- frk_clean %>% 
  group_by(parameter,year) %>% 
  summarize(value_mean = mean(value)
            ,value_se = se(value)
            )

#plot the summary data
ggplot(frk_sum, aes(x=year, y =value_mean))+
  geom_errorbar(aes(ymin=value_mean-value_se, ymax=value_mean+value_se), width = 0.2) +
  geom_point()+
  geom_line()+
  facet_wrap(~parameter, scales = "free")

#write summary stats file 
#write_csv(frk_sum,"EDB/frk_sonde_data_summary.csv")

