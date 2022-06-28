#How do I calculate center of distribution?

library(tidyverse)

clamtest = read_csv("clamexample.csv")


#I could just do the average distance of stations where clams were caught,
#but that doesn't take into account how many clams were caught.
center1 = clamtest %>%
  filter(CPUE != 0) %>%
  group_by(Year) %>%
  summarize(Distance = mean(Distance))

#I can do the average distance for each clam, but that doesn't take into account
#differences in sampling between years (like different station locations)
center2 = clamtest %>%
  mutate(WeightedCPUE = Distance*CPUE) %>%
  group_by(Year) %>%
  summarize(Distance = sum(WeightedCPUE)/sum(CPUE))

#I might have to only include stations that were sampled consistantly between
#years, but it would be nice to have something like a "random effect" of station
#to account for differential sampling.
