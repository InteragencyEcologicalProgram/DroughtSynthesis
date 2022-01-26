#Special Studies Drought Synthesis
#Drought Indices

#required packages
library(tidyverse)
library(lubridate) #working with date-time
library(PerformanceAnalytics) #plotting correlations
library(RCurl) #read csv files from GitHub
library(waterYearType) #DWR water years
library(zoo) #used to assign dates to wate years rather than calendar years

#to do
#could calculate mean values for the two relevant divisions
#or could just use Sac drainage because it contributes more water to the system

#read in the data----------
#used read.table because it worked better than tidyverse options for dealing with variable 
#numbers of spaces between columns

#palmer drought severity index (05)
#note that these links will need to be updated over time because the date changes as files are updated
#https://www.ncei.noaa.gov/pub/data/cirs/climdiv/
pdsi <- read.table(file="https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-pdsidv-v1.0.0-20220108"
                   ,header=F
                   )

#palmer hydrological drought index (06)
phdi <- read.table(file="https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-phdidv-v1.0.0-20220108"
                   ,header=F
                   )

#palmer "Z" index (07)
zndx <- read.table(file="https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-zndxdv-v1.0.0-20220108"
                   ,header=F
                   )

#modified palmer drought severity index (08)
pmdi <- read.table(file="https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-pmdidv-v1.0.0-20220108"
                   ,header=F
                   )

#import dayflow data from the IEP Status and Trends GitHub repo
dayflow<- read_csv("https://raw.githubusercontent.com/InteragencyEcologicalProgram/Status-and-Trends/master/data/dayflow_all.csv")

#format data------------

#combine data sets for all indices
indices <- bind_rows(pdsi,phdi,pmdi,zndx)

#steps to format and filter data set
indices_cleaner <-indices %>% 
  #rename headers
  rename(division_index_year = V1
         ,jan = V2
         ,feb = V3
         ,mar = V4 
         ,apr = V5
         ,may = V6
         ,jun = V7
         ,jul = V8
         ,aug = V9 
         ,sep = V10
         ,oct = V11
         ,nov = V12
         ,dec = V13
         ) %>% 
  #split up info in first column based on character position
  separate(division_index_year,c("division","index","year"),sep=c(3,5)) %>%
  #filter to just the two divisions needed
  #Sacramento drainage is 402 and San Joaquin drainage is 405
  filter(division == "402" | division =="405") %>% 
  #change df from wide to long
  pivot_longer(cols=jan:dec, names_to = "month", values_to = "value") %>% 
  #create a month-year column and format as date type
  mutate(date1 = paste(month, year),
         date = my(date1)
  ) %>% 
  #subset and reorder columns
  select("division", "index", "date", "value" ) %>% 
  #filter out missing data (-99.90 or -99.99)
  filter(value > -99) %>% 
  glimpse()
 
#make a wide version for correlation plots
indices_w <- indices_cleaner %>% 
  pivot_wider(names_from=index
              ,names_prefix = "index_"
              ,values_from=value) 

#make an even wider version for correlation plots
indices_ww <- indices_w %>% 
  pivot_wider(names_from = division
              ,names_prefix = "div_"
              ,values_from = index_05:index_07)

#summarize dayflow by month to match frequency of palmer indices
dayflow_m<-dayflow %>%
  mutate(year = year(Date),
         month = month(Date)) %>% 
  unite(year_month,year:month,sep="-",remove=T) %>% 
  group_by(year_month) %>%
  summarize(OUT_mean = mean(OUT))

#plots-------------

#make faceted plot showing each index for each division
(plot_di <-ggplot(indices_cleaner, aes(x=date, y= value))+
   geom_bar(stat = "identity") + 
   ylab("Index") + xlab("Date") + 
   facet_wrap(index~division,nrow = 4)
)
#should replace numbers with division and index names
#also add horizontal lines showing categories of wet/dry

#plot just PHDI for sacramento drainage
sach <- indices_cleaner %>% 
  filter(division == "402" & index == "06") %>% 
  mutate (category_coarse = cut (value,
                         breaks = c(-Inf, -3, -1, 1, 3, Inf),
                         labels = c("very dry", "dry", "normal", "wet", "very wet" ),
                         right = TRUE)
  )
#write these data to share
#write_csv(sach, "PHDI_Sacramento.csv")

#combine monthly dayflow and sac PHDI
#first format sac PHDI date column
sach_m<-sach %>%
  mutate(year = year(date),
         month = month(date)) %>% 
  unite(year_month,year:month,sep="-",remove=T) 

#add a column to sach_m df that categorizes month by water year (Oct 1-Sept 30)
sach_m$WY <- as.integer(as.yearmon(sach_m$date) - 9/12 + 1)

#add DWR water year categories to df
wyear <- water_year_indices %>% 
  filter(location=="Sacramento Valley" & WY > 1905) %>% 
  select("WY","Index","Yr_type")

#combine palmer and DWR water year dfs
sac_wy <- left_join(wyear,sach_m)

#calculate standard units of palmer indices
#(monthly value-long term mean)/ standard deviation of mean
#focus just on Sac river division (402) and PHDI
#water year (Oct 1-Sept 30) is more useful than calendar year
sach_std <- sac_wy %>% 
    mutate(
      #calculates long term mean
    phdi_ltmean = mean(value)
    #calculates long term standard deviation
    ,phdi_sd =sd(value)
    #calculates standard units 
    ,phdi_std = (value-phdi_ltmean)/phdi_sd
    ) 

#create boxplot of Sac PHDI categorized by WY type
ggplot(sach_std, aes(x=Yr_type, y=value))+
  geom_boxplot()

#create boxplot of Sac PHDI in standardized units categorized by WY type
ggplot(sach_std, aes(x=Yr_type, y=phdi_std))+
  geom_boxplot()


#combine palmer and dayflow data sets
comb <- left_join(dayflow_m,sach_m)
plot(comb$OUT_mean~comb$value)
cor.test(comb$OUT_mean,comb$value) #cor=0.5253882

#color palette for categories
pal <- c(
  "very dry" = "red",
  "dry" = "orange", 
  "normal" = "green", 
  "wet" = "blue" ,
  "very wet" = "violet"
)

(plot_sach <-ggplot(sach, aes(x=date, y= value, fill = category_coarse))+
    geom_bar(stat = "identity") + 
    ylab("Value") + xlab("Date") +
    ggtitle("Sacramento Drainage PHDI")+
    scale_fill_manual(
      values = pal,
      limits = names(pal))
)
#index generally ranges from -6 to +6, with negative values denoting dry spells, 
#and positive values indicating wet spells
#There are a few values in the magnitude of +7 or -7.
#values 0 to -0.5 = normal
#-0.5 to -1.0 = incipient drought
#-1.0 to -2.0 = mild drought
#-2.0 to -3.0 = moderate drought
#-3.0 to -4.0 = severe drought
#greater than -4.0 = extreme drought
#Similar adjectives are attached to positive values of wet spells
#may I should combine the following:
#normal + incipient (0 to 1.0)
#mild + moderate (1.1 to 3.0)
#severe + extreme (3.1 and up)

#plot correlations among indices within districts

#Sacramento
sac_within <- indices_w %>% 
  filter(division == 402) 

#plot
chart.Correlation(sac_within[3:6])

#correlations
cor(sac_within[3:6])

#San Joaquin
sj_within <- indices_w %>% 
  filter(division == 405) 

#plot
chart.Correlation(sj_within[3:6])

#correlations
cor(sj_within[3:6])

#plot correlations of same index between districts

#plots
chart.Correlation(indices_ww[2:9])

#index 05
cor(indices_ww[2:3])  #0.7790742

#index 06
cor(indices_ww[4:5])  #0.8211686

#index 07
cor(indices_ww[8:9]) #0.8188015

#index 08
cor(indices_ww[6:7]) #0.8319748

  
  
  
  
  
  
