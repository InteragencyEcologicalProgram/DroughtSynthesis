#do a 20mm index for the other fish. 

# For consistency among years, surveys 1 through 9 are considered for the purpose of index calculation.
# The index is calculated using the data from only 4 of these surveys: the two before and the two after the
# point where the average length of delta smelt (less than 60mm in length) equals 20mm. From this subset
# of surveys, the delta smelt catch-per-unit-effort (CPUE) is calculated for each “core” station, which
# includes 41 stations that have been consistently sampled throughout the history of the survey. To each
# station’s CPUE, 1 is added, and then a log10 transformation is performed.

library(tidyverse)

catch20 = read_csv("data/20-mm_Catch table_EDl.csv", guess_max = 10000)

catch20 = rename(catch20, Survey = qry_AMC_EDI_01.Survey.Survey, Station = qry_AMC_EDI_01.Station.Station)

catch20$UseforIndex <- FALSE
catch20$UseforIndex[catch20$Year == 1995 & catch20$Survey %in% 2:5] <- TRUE
catch20$UseforIndex[catch20$Year == 1996 & catch20$Survey %in% 4:7] <- TRUE
catch20$UseforIndex[catch20$Year == 1997 & catch20$Survey %in% 4:7] <- TRUE
catch20$UseforIndex[catch20$Year == 1998 & catch20$Survey %in% 3:6] <- TRUE
catch20$UseforIndex[catch20$Year == 1999 & catch20$Survey %in% 3:6] <- TRUE
catch20$UseforIndex[catch20$Year == 2000 & catch20$Survey %in% 5:8] <- TRUE
catch20$UseforIndex[catch20$Year == 2001 & catch20$Survey %in% 4:7] <- TRUE
catch20$UseforIndex[catch20$Year == 2002 & catch20$Survey %in% 4:7] <- TRUE
catch20$UseforIndex[catch20$Year == 2003 & catch20$Survey %in% 4:7] <- TRUE
catch20$UseforIndex[catch20$Year == 2004 & catch20$Survey %in% 3:6] <- TRUE
catch20$UseforIndex[catch20$Year == 2005 & catch20$Survey %in% 5:8] <- TRUE
catch20$UseforIndex[catch20$Year == 2006 & catch20$Survey %in% 4:7] <- TRUE
catch20$UseforIndex[catch20$Year == 2007 & catch20$Survey %in% 4:7] <- TRUE
catch20$UseforIndex[catch20$Year == 2008 & catch20$Survey %in% 4:7] <- TRUE
catch20$UseforIndex[catch20$Year == 2009 & catch20$Survey %in% 4:7] <- TRUE
catch20$UseforIndex[catch20$Year == 2010 & catch20$Survey %in% 4:7] <- TRUE
catch20$UseforIndex[catch20$Year == 2011 & catch20$Survey %in% 5:8] <- TRUE
catch20$UseforIndex[catch20$Year == 2012 & catch20$Survey %in% 5:8] <- TRUE
catch20$UseforIndex[catch20$Year == 2013 & catch20$Survey %in% 3:6] <- TRUE
catch20$UseforIndex[catch20$Year == 2014 & catch20$Survey %in% 3:6] <- TRUE
catch20$UseforIndex[catch20$Year == 2015 & catch20$Survey %in% 3:6] <- TRUE
catch20$UseforIndex[catch20$Year == 2016 & catch20$Survey %in% 2:5] <- TRUE
catch20$UseforIndex[catch20$Year == 2017 & catch20$Survey %in% 3:6] <- TRUE
catch20$UseforIndex[catch20$Year == 2018 & catch20$Survey %in% c(1,2,3,9)] <- TRUE
catch20$UseforIndex[catch20$Year == 2019 & catch20$Survey %in% 3:6] <- TRUE
## Arbitrary for 2020?:
catch20$UseforIndex[catch20$Year == 2020 & catch20$Survey %in% 3:6] <- TRUE
## Arbitrary for 2020?:
catch20$UseforIndex[catch20$Year == 2021 & catch20$Survey %in% 3:6] <- TRUE

#Function for calculating index for the 20mm survey

annualindex20 = function(data, Fish) {
  
  #Select the column with the catch for your fish of interest
  Fish1 = select(data, Fish)
  names(Fish1) = "Fish"
  
  #put in zeros for NAs and calcualte CPUE
  data = cbind(data, Fish1) %>%
    mutate(Fish = case_when(
      is.na(Fish) ~ 0,
      TRUE ~ Fish
    ))
  
  #total CPUE per station
  data2 = group_by(data,Station, Survey, Year, UseforIndex) %>%
    summarize(catch = sum(Fish), Volumet = sum(Volume), CPUE = catch/Volumet*10000 )

  
  station<-read_excel("data/20_IndexStations.xlsx")#csv file of station information
 station$index = TRUE
  
  #join stations to data and log-transformCPUE
 data3 = left_join(data2, station) %>%
   filter(index, UseforIndex) %>%
   mutate(logCPUE = log(CPUE+1, base = 10))
 
 Surveyindex = group_by(data3, Year, Survey) %>%
   summarize(SIndex = 10^(mean(logCPUE, na.rm = T))-1)
 
  #annual index is mean of the first two survey indecies, divided by 1000
  annualindex = group_by(Surveyindex, Year) %>%
    summarize(Index = sum(SIndex))
  
}

Ashad = annualindex20(catch20, "American Shad") %>%
  rename(ASHAD = Index)

Dsmelt = annualindex20(catch20, "Delta Smelt") %>%
  rename(Dsmelt = Index)


Lsmelt = annualindex20(catch20, "Longfin Smelt") %>%
  rename(Lsmelt = Index)


SBass = annualindex20(catch20, "Striped Bass") %>%
  rename(Sbass = Index)


twentymil = left_join(Ashad, Dsmelt) %>%
  left_join(Lsmelt) %>%
  left_join(SBass)
write.csv(twentymil, "Twentymilindex.csv")
