#Look at some of the big-picture differences between wet and dry years
library(tidyverse)
library(readxl)
library(viridis)

#check out the integrated data set so far
Integrated_data_set <- read_excel("data/Integrated data set.xlsx", na = "NA") %>%
  mutate(Season = factor(Season, levels = c("Winter", "Spring", "Summer", "Fall")))

yrs = dplyr::select(Integrated_data_set, Year, Season, Drought, Index, Yr_type)

#log-transform zooplankton and clorophyll
Int = mutate(Integrated_data_set, logChla = log(Chla), logzoopB = log(Zoop_BPUE_mg), 
             logzooC = log(Zoop_CPUE)) %>%
  dplyr::select(-Chla, -Zoop_BPUE_mg, -Zoop_CPUE) 


#transition the data set from wide to long. 
IntLong = pivot_longer(Int, cols = `Delta Outflow`:logzooC, 
                       names_to = "Metric", values_to = "Value")

#look at it without the "not drought or wet" years
ggplot(filter(IntLong, Drought != "N"), aes(x = Drought, y = Value)) + geom_boxplot() +
  facet_grid(Metric~Season, scales = "free_y")

#Huh. Chlorophyll goes up
Chla = filter(IntLong, Metric == "logChla", Drought != "N")
ggplot(Chla, aes(x = Drought, y = Value)) + geom_boxplot()+ facet_wrap(~Season)


#Let's make a rough "Drought impact" index. I"m just making this up tho.

DroughtImpact = group_by(IntLong, Season, Metric, Drought) %>%
  summarize(Mean = mean(Value, na.rm = T)) %>% 
  pivot_wider(names_from = Drought, values_from = Mean) %>%
  mutate(Index = (D-W)/mean(c(D,N, W), na.rm = T))

  

#Now let's try scaling all the variables first and then creating a drought index
DrIm2 = Int %>%
  mutate(across(X2:logzooC, scale)) %>%
  pivot_longer(cols = X2:logzooC, names_to = "Metric", values_to = "Value") %>%
  group_by(Season, Metric, Drought) %>%
  summarize(Mean = mean(Value, na.rm = T)) %>% 
  pivot_wider(names_from = Drought, values_from = Mean) %>%
  mutate(Index = (D-W))

ggplot(DroughtImpact, aes(x = Season, y = Index)) + facet_wrap(~Metric)+
  geom_col()

ggplot(DrIm2, aes(x = Season, y = Index)) + facet_wrap(~Metric)+
  geom_col()

#the fish have such a huge difference it's hard to see everything else.
ggplot(DrIm2, 
       aes(x = Metric, y = Index, fill = Index)) + facet_wrap(~Season)+
  geom_col()+
  scale_fill_gradient2(low = "red", high = "blue", mid = "grey") + theme_bw()

#Some sort of threshold?
#Some sort of good/bad indicator. 
Cats = read_excel("data/Integrated data set.xlsx", sheet = "Categories")
DrIm2b = left_join(DrIm2, Cats) %>%
  mutate(Index2 = abs(Index), colr = case_when(
    Index <0 ~ "red",
    Index >0 ~ "blue"
  )) %>%
  filter(!is.na(colr), Metric %in% c("DeltaExport", "DissAmmonia", "DissNitrateNitrite",
                                     "logChla", "logzoopB", "Salinity", "Sbindex", "Secchi",
                                     "SmeltIndex", "Temperature", "X2"))

DrIm2b = mutate(DrIm2b, Metric = factor(Metric, levels =  c("DeltaExport", "DissAmmonia", "DissNitrateNitrite",
                                             "logChla", "logzoopB", "Salinity", "Sbindex", "Secchi",
                                             "SmeltIndex", "Temperature", "X2"), labels = 
                                          c("Exports", "Ammonium", "Nitrate",
                                            "Chla", "Zoops", "Salinity", "Stripers", "Secchi",
                                            "Smelt", "Temperature", "X2")))

ggplot(DrIm2b, aes(x=Category, group = Metric)) +
  geom_col(aes(fill = colr, y = Index2, alpha = Index2), 
           position =position_dodge2(width = 1, preserve = "single"))+
  geom_text(aes(label = Metric, y = Index2), position = position_dodge(.9))+
  scale_fill_manual(values = c("blue", "red"), labels = c("increase", "decrease"),
                    name = "Direction of \n Drought Impact")+
  coord_polar() + theme_bw()+
  scale_alpha(guide = NULL)+
  scale_y_continuous( name = NULL) + facet_wrap(~Season)


ggplot(DrIm2b, aes(x=Category, group = Metric)) +
  geom_col(aes(fill = colr, y = Index, alpha = Index2), 
           position =position_dodge2(width = 1, preserve = "single"))+
  #geom_text(aes(label = Metric, y = Index2), position = position_dodge(.9))+
  scale_fill_manual(values = c("blue", "red"), labels = c("increase", "decrease"),
                    name = "Direction of \n Drought Impact")+
  geom_text(aes(x = Category, label = Metric, y = 0, group = Metric), hjust = 0, angle = 90,
            position = position_dodge2(width = 1, preserve = "single"))+
   theme_bw()+
  scale_y_continuous( name = NULL) + facet_grid(.~Season, space = "free")


#Color code by good versus bad
ggplot(DrIm2b, aes(x=Category, group = Metric)) +
  geom_col(aes(fill = GoodBad, y = Index, alpha = Index2), 
           position =position_dodge2(width = 1, preserve = "single"))+
  #geom_text(aes(label = Metric, y = Index2), position = position_dodge(.9))+
  scale_fill_manual(values = c("red", "blue"), labels = c("Stressor", "Good Thing"),
                    name = "Direction of \n Drought Impact")+
  geom_text(aes(x = Category, label = Metric, y = 0, group = Metric), hjust = 0, angle = 90,
            position = position_dodge2(width = 1, preserve = "single"))+
  theme_bw()+
  scale_y_continuous( name = NULL) + facet_grid(.~Season, space = "free")


#create an annual index
AnnIm = group_by(DrIm2b, GoodBad, Metric, Category) %>%
  summarize(Index = mean(Index), Index2 = mean(Index2)) %>%
  mutate(colr = case_when(
    GoodBad == "Bad Things" & Index >0 ~ "red",
    GoodBad == "Bad Things" & Index <0 ~ "blue",
    GoodBad == "Good Things" & Index >0 ~ "blue",
    GoodBad == "Good Things" & Index <0 ~ "red",
  ))

#Color code by good versus bad
ggplot(AnnIm, aes(x=Metric, group = Metric)) +
  geom_col(aes(fill = colr, y = Index, alpha = Index2), 
           position =position_dodge2(width = 1, preserve = "single"))+
  scale_fill_manual(values = c("blue", "red"), guide = NULL)+
  geom_text(aes(x = Metric, label = Metric, y = 0, group = Metric), hjust = 0, angle = 90,
            position = position_dodge2(width = 1, preserve = "single"))+
  theme_bw()+
  scale_y_continuous( name = "Drought Impact Level") + facet_grid(.~GoodBad, scales = "free_x") +
  scale_x_discrete(name = NULL) + theme(axis.text.x = element_blank()) +
  scale_alpha(guide = NULL)

#try a different way

#Color code by good versus bad
ggplot(AnnIm, aes(x=Metric, group = Metric)) +
  geom_col(aes(fill = GoodBad, y = Index, alpha = Index2), 
           position =position_dodge2(width = 1, preserve = "single"))+
  scale_fill_manual(values = c("red", "blue"))+
  geom_text(aes(x = Metric, label = Metric, y = 0, group = Metric), hjust = 0, angle = 90,
            position = position_dodge2(width = 1, preserve = "single"))+
  theme_bw()+
  scale_y_continuous( name = "Drought Impact Level") + 
  scale_x_discrete(name = NULL) + theme(axis.text.x = element_blank()) +
  scale_alpha(guide = NULL)


########################################################
#compare zooplankton data from Status and Trends to what Arthur put together

SNTzoop = read.csv("data/StatusandTrendsZoopBPUE.csv")

#totals by season and year
SNTzoop2 = group_by(SNTzoop, quarter, qyear) %>%
  summarize(Zoop_BPUE_mg2 = sum(bpue_mg), logzoopB2 = log(Zoop_BPUE_mg2)) %>%
  mutate(Season = factor(quarter, levels = c("Q1", "Q2", "Q3", "Q4"),
         labels = c("Winter", "Spring", "Summer", "Fall"))) %>%
  rename(Year = qyear)

ggplot(filter(Integrated_data_set, Season == "Fall"), aes(x = Index, y = Zoop_CPUE))+
  geom_point()
ggplot(filter(Int, Season == "Fall"), aes(x = Index, y = logzoopB))+
  geom_point()



zoopsfall = filter(IntLong, Metric == "logzoopB", Season == "Fall")
zoops = filter(IntLong, Metric == "logzoopB")
ggplot(zoopsfall, aes(x = Drought, y = Value)) + geom_boxplot()
ggplot(zoops, aes(x = Drought, y = Value)) + geom_boxplot()+ facet_wrap(~Season)

zootest = select(Integrated_data_set, Year, Season, Drought, Zoop_CPUE, Zoop_BPUE_mg) %>%
  filter(Season == "Fall")
zootest2 = merge(zootest, zoops2)

#Wow. Zooplankton is waaaay less abundant in the LSZ, but not other place in the Delta. 
#But maybe that's because I had mysids in the Status and Trends dataset. 

zootest3 = merge(zoops, SNTzoop2)
ggplot(zootest3, aes(x = Value, y = logzoopB2, color = Season)) + geom_point()

ggplot(zootest3, aes(x= Drought, y = logzoopB2)) + facet_wrap(~Season) + geom_boxplot()
#so that's everywhere

#now look at just suisun
Suisun = filter(SNTzoop, region == "ss", qyear >1974) %>%
  group_by(quarter, qyear) %>%
  summarize(Zoop_BPUE_mg2 = sum(bpue_mg), logzoopB2 = log(Zoop_BPUE_mg2)) %>%
  mutate(Season = factor(quarter, levels = c("Q1", "Q2", "Q3", "Q4"),
                         labels = c("Winter", "Spring", "Summer", "Fall"))) %>%
  rename(Year = qyear)%>%
  left_join(zoops) 


ggplot(filter(Suisun, Drought != "N"), aes(x= Drought, y = logzoopB2)) + facet_wrap(~Season) + geom_boxplot()
#So zooplankton are lower in summer and fall in suisun, but just summer and fall.

dist = read_excel("data/distribution_matrix.xlsx", na = "NA")
dist_long = pivot_longer(dist, cols = 3:ncol(dist), names_to = "taxa", 
                         values_to= "distance") %>%
rename(Year = water_year)%>%
  left_join(yrs)


DistDI = group_by(dist_long, Season, taxa, Drought) %>%
  summarize(distance = mean(distance, na.rm = T)) %>% 
  pivot_wider(names_from = Drought, values_from = distance) %>%
  mutate(Index = (D-W)/mean(c(D,N, W), na.rm = T)) %>%
  mutate(colr = case_when(
    Index >0 ~ "red",
   Index <0 ~ "blue",
    Index >0 ~ "blue",
    Index <0 ~ "red",
  ))



ggplot(DistDI, aes(x=taxa)) +
  geom_col(aes(fill = colr, y = Index, alpha = Index), 
           position =position_dodge2(width = 1, preserve = "single"))+
  #geom_text(aes(label = Metric, y = Index2), position = position_dodge(.9))+
  scale_fill_manual(values = c("blue", "red"), labels = c("Westward", "Eastward"),
                    name = "Change in center \nof distribution")+
  geom_text(aes(x = taxa, y = 0, label = taxa), hjust = 0, angle = 90,
            position = position_dodge2(width = 1, preserve = "single"))+
  theme_bw()+ facet_grid(.~Season, space = "free") + scale_alpha(guide = NULL) +
  ylab("Drought shift (Km)")+ theme(axis.text.x = element_blank())


