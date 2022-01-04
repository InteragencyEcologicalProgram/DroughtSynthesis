#SKT index

library(tidyverse)
library(lubridate)

SKT = read_csv("data/2021_SKT_EDI_update.csv")
SKTstations = read_csv("data/SKTstations.csv")

# To calculate the index, stations are grouped into 3 spatial regions and a mean catch per 10,000 cubic
# meters of water (i.e., CPUE) is calculated. The regional means are then summed to create an index for
# each survey, and survey indices are summed to calculate the SKT index. The SKT index allows for
# comparison across years to reflect trends in the population, but it is not an abundance estimate of the
# overall population.

str(SKT)

SKTindex = function(data, Fish){
  #highilght the fish of interest
  Fish1 = select(data, Fish)
  names(Fish1) = "Fish"
  data = cbind(data, Fish1) %>%
    mutate(Fish = case_when(
      is.na(Fish) ~ 0,
      TRUE ~ Fish
    ))
  
  #attach region designations and get rid of non-index stations
  #also filter to Surveys 1-4
  SKTstations = read_csv("data/SKTstations.csv")
  data = left_join(data, SKTstations) %>%
    filter(!is.na(Region), SurveyNumber %in% c(1:4))

  #calculate CPUE and regional means
  data2 = mutate(data, CPUE = Fish/Volume*10000) %>%
    group_by(Year, SurveyNumber, Region) %>%
    summarize(CPUE = mean(CPUE, na.rm = T))
  
  #survey index
  SurveyIndex = group_by(data2, Year, SurveyNumber) %>%
    summarize(SI = sum(CPUE, na.rm = T))
  
  #annual index
  SKTInd = group_by(SurveyIndex, Year) %>%
    summarize(Index = sum(SI, na.rm = T))
  
  return(SKTInd)
}

SKTDS = SKTindex(SKT, "Delta_Smelt") %>%
  rename(DSmelt = Index)

SKTSB = SKTindex(SKT, "striped_bass_age_0") %>%
  rename(Sbass0 = Index)

SKTSB1 = SKTindex(SKT, "striped_bass_age_1") %>%
  rename(Sbass1 = Index)

SKTLS = SKTindex(SKT, "longfin_smelt") %>%
  rename(lsmelt = Index)

SKTAS = SKTindex(SKT, "American_shad") %>%
  rename(shad = Index)

SKTindeces = left_join(SKTDS, SKTSB) %>%
  left_join(SKTSB1) %>%
  left_join(SKTLS) %>%
  left_join(SKTAS)
