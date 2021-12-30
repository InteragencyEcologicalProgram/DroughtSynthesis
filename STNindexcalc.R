#Function to calculate the STN Delta Smelt index (or similar index for other fish)

#This assumes you have the STation lookup table from the STN database and teh
#"catch per station" csv on the FTP site
#https://filelib.wildlife.ca.gov/Public/TownetFallMidwaterTrawl/TNS%20MS%20Access%20Data/TNS%20data/CatchPerStation1959-2021.csv

annualindex<-function(data, #catch per staion dataset
                      Fish){ #Name of fish to calculate an index for, as a character
  
  #Select the column with the catch for your fish of interest
  Fish1 = select(data, Fish)
  names(Fish1) = "Fish"
  data = cbind(data, Fish1)
  
  station<-read_excel("data/luStation.xlsx")#csv file of station information
  station<-mutate(station,StationCode=StationCodeSTN)
  
  #join stations to data and calculate weighted catch
  data = mutate(data, StationCode = as.character(`Station Code`))
  data = left_join(data, station) %>%
    mutate(weightedcatch = WeightingFactor*Fish) %>%
    filter(Index ==1)
  
  #Just surveys 1 and 2 for Delta Smelt, 1-4 for the others, since 5-6 didn't start until 2002
  if(Fish == "Delta Smelt" | Fish == "Longfin Smelt"){
    data = filter(data, Survey %in% c(1,2) )
  } else data = filter(data, Survey %in% 1:4 )
  
  #total weighted catch per survey
  data2 = group_by(data, Year, Survey) %>%
    summarize(surveyIndex = sum(weightedcatch))
  
  #annual index is mean of the first two survey indecies, divided by 1000
  annualindex = group_by(data2, Year) %>%
    summarize(Index = round(mean(surveyIndex)/1000, 1))
}

catch = read_csv("data/CatchPerStation1959-2021.csv") 

AMShad = annualindex(catch, "American Shad") %>%
  rename(Amshad = Index)

#Try it for DeltaSmelt

Dsmelt = annualindex(catch, "Delta Smelt") %>%
  rename(deltasmelt = Index)
#YESSS


#Longfin smelt
Lsmelt = annualindex(catch, "Longfin Smelt") %>%
  rename(Longfin = Index)


STNindecies = left_join(AMShad, Dsmelt) %>%
  left_join(Lsmelt)

write.csv(STNindecies, "STNindecies.csv")


#Should i just do the first two surveys or all surveys?

catch2 = group_by(catch, Survey, Year) %>%
  summarize(DSM = sum(`Delta Smelt`), AMS = sum(`American Shad`), LFS = sum(`Longfin Smelt`)) %>%
  group_by(Survey) %>%
  summarize(DSM2 = mean(DSM), AMS2 = mean(AMS), LFS2 = mean(LFS))



catch3 = group_by(filter(catch, Year > 1995), Survey, Year) %>%
  summarize(DSM = sum(`Delta Smelt`), AMS = sum(`American Shad`), LFS = sum(`Longfin Smelt`)) %>%
  group_by(Survey) %>%
  summarize(DSM2 = mean(DSM), AMS2 = mean(AMS), LFS2 = mean(LFS))


ggplot(catch2, aes(x=Survey, y = AMS2)) + geom_bar(stat = "identity") + ylab("American Shad")
ggplot(catch2, aes(x=Survey, y = LFS2)) + geom_bar(stat = "identity") + ylab("Longfin Smelt")
ggplot(catch2, aes(x=Survey, y = DSM2)) + geom_bar(stat = "identity") + ylab("Delta Smelt")

ggplot(catch3, aes(x=Survey, y = AMS2)) + geom_bar(stat = "identity") + ylab("American Shad")
ggplot(catch3, aes(x=Survey, y = LFS2)) + geom_bar(stat = "identity") + ylab("Longfin Smelt")
ggplot(catch3, aes(x=Survey, y = DSM2)) + geom_bar(stat = "identity") + ylab("Delta Smelt")
