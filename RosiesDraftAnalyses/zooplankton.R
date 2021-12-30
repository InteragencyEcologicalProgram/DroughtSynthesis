#zooplankton

library(tidyverse)
library(readxl)

zoops = read_csv("data/zoop_drought_lt_REGbpue.csv")
yeartypes = read_excel("data/Integrated data set.xlsx", sheet = "yearassignments")
zoops = rename(zoops, Year = water_year) %>%
  left_join(yeartypes)

ggplot(dplyr::filter(zoops, Region != "North", Drought != "N"), aes(x = Drought, y = BPUE_ug, fill = Drought)) +geom_boxplot()+
  facet_wrap(~Region)+ scale_x_discrete(labels = c("multi-year \nDrought", "multi-year \nwet")) +
  ylab("Mean Zooplankton Biomass")


#Look at just the spring zooplankton for the TUCP analysis
zoopsSp = read_csv("data/zoop_drought_lt_seasonal.csv")
zoopsSp = rename(zoopsSp, Year = water_year) %>%
  left_join(yeartypes) %>%
  filter(Season == "Spring")


ggplot(zoopsSp, aes(x = Index, y = log(szn_BPUE), color = Year)) + geom_point()

Zoop <- read_csv("data/taxa_szn_matrix.csv")
Zoop = rename(Zoop, Year = water_year) %>%
  left_join(yeartypes)

Zooplong = pivot_longer(Zoop, `All taxa`:`Neomysis kadiakensis Adult`, names_to = "Taxa",
                        values_to= "BPUE")
Zoopsp = filter(Zooplong, Season == "Spring", !is.na(SprNDOI), Year <2020)

mysidsSum = filter(Zoopsp, Taxa %in% c("Hyperacanthomysis longirostris Adult", 
                                        "Neomysis mercedis Adult", "Neomysis kadiakensis Adult")) %>%
  group_by(Year, SprNDOI) %>%
  summarize(BPUE = sum(BPUE, na.rm = T)) %>%
  mutate(Taxa = "Mysids")

Zoopspx = bind_rows(Zoopsp, mysidsSum) %>%
  filter(!Taxa %in% c("Hyperacanthomysis longirostris Adult", 
                     "Neomysis mercedis Adult", "Neomysis kadiakensis Adult"))

ggplot(filter(Zoopspx, Taxa != "All taxa"), aes(x = SprNDOI, y = log(BPUE+1))) + geom_point() + geom_smooth(method = "lm")+
  facet_wrap(~Taxa, scales = "free_y") + xlab("Spring Net Delta Outflow Index")+
  ylab("log-transformed spring zooplankton BPUE (ug/mL)")

ggplot(Zoopsp, aes(x = Index, y = log(BPUE+1))) + geom_point() + geom_smooth(method = "lm")+
  facet_wrap(~Taxa, scales = "free_y")

ggplot(Zooplong, aes(x = SprNDOI, y = log(BPUE+1))) + geom_point() + geom_smooth(method = "lm")+
  facet_wrap(~Taxa, scales = "free_y")

ggplot(filter(Zoopsp, Taxa != "All taxa", Year >1999), aes(x = SprNDOI, y = log(BPUE+1))) + geom_point() + geom_smooth(method = "lm")+
  facet_wrap(~Taxa, scales = "free_y") + xlab("Spring Net Delta Outflow Index")+
  ylab("log-transformed spring zooplankton BPUE (ug/mL)")

LM = function(dat) {summary(lm(data = dat, log(BPUE+1)~ SprNDOI))$coefficients[2,4]}
Zoopspx$SprNDOI2 = Zoopspx$SprNDOI/10000
test = filter(Zoopspx, Year > 1999) %>%
  group_by(Taxa) %>%
   do(slope =summary(lm(data = ., log(BPUE+1)~ SprNDOI2))$coefficients[2,1],
      intercept = summary(lm(data = ., log(BPUE+1)~ SprNDOI2))$coefficients[1,1],
      p =  summary(lm(data = ., log(BPUE+1)~ SprNDOI2))$coefficients[2,4]) %>%
  ungroup() 

foo = data.frame(Taxa = test$Taxa, slope = round(unlist(test$slope), digits = 3), 
                 intercept = round(unlist(test$intercept), digits = 2),
                 p = round(unlist(test$p), digits = 3))

Zoopsp2 = left_join(Zoopspx, foo) %>%
  mutate(sig = case_when(
    p > 0.05 ~ "NS",
    TRUE ~ "SIG"
  ))

ggplot(filter(Zoopsp2, Taxa != "All taxa", Year >1999), aes(x = SprNDOI2, y = log(BPUE+1))) + 
  geom_point() + geom_smooth(method = "lm", aes(color = sig, alpha = sig))+
  scale_alpha_manual(values = c(0, .5), guide = NULL)+ 
  scale_color_manual(values = c("grey", "blue"), guide = NULL)+
  facet_wrap(~Taxa, scales = "free_y") + xlab("Mean March-May Delta Outflow Index (10,000 cfs)")+
  ylab("log-transformed March-May BPUE (ug/mL)")+
  geom_text(data = filter(foo, Taxa != "All taxa"), aes(x = 5, y = 0, label = paste(slope, "X + ", intercept, " p=", p,  sep = "")))

mysids = filter(Zoopsp2, Taxa %in% c("Hyperacanthomysis longirostris Adult", 
                                     "Neomysis mercedis Adult", "Neomysis kadiakensis Adult"))

mysids = mutate(mysids, regime = case_when(
  Year < 1987 ~ "preclam",
  Year >=  1987 & Year<2000 ~ "PrePOD",
  Year >= 2000 ~ "POD and after"
))

mysidsSum = filter(Zoopsp2, Taxa %in% c("Hyperacanthomysis longirostris Adult", 
                                     "Neomysis mercedis Adult", "Neomysis kadiakensis Adult"),
                   Year >2000) %>%
  group_by(Year, SprNDOI2) %>%
  summarize(mysidsBPUE = sum(BPUE))



mysids = mutate(mysids, regime = case_when(
  Year < 1987 ~ "preclam",
  Year >=  1987 & Year<2000 ~ "PrePOD",
  Year >= 2000 ~ "POD and after"
))


library(RColorBrewer)
ggplot(mysids, aes(x = SprNDOI2, y = log(BPUE+1), color = regime)) + 
  geom_point() + geom_smooth(method = "lm", aes(color = regime))+
  scale_color_manual(values = brewer.pal(3, "Dark2"), 
                     labels = c("POD: 2000-2020", "Pre-clam: 1975-1986", "Post-Clam: 1987-2019"))+
  facet_wrap(~Taxa, scales = "free_y") + xlab("Mean March-May Delta Outflow Index (10,000 cfs)")+
  ylab("log-transformed March-May BPUE (ug/mL)")+theme_bw()+
  theme(legend.position = "bottom")


ggplot(mysids, aes(x = SprNDOI2, y = log(BPUE+1), color = Taxa)) + 
  geom_point() + geom_smooth(method = "lm")+
  scale_color_manual(values = brewer.pal(3, "Dark2"))+
  facet_wrap(~regime, scales = "free_y") + xlab("Mean March-May Delta Outflow Index (10,000 cfs)")+
  ylab("log-transformed March-May BPUE (ug/mL)")+theme_bw()+
  theme(legend.position = "bottom")

ggplot(filter(Zoopsp2, Year >1999, Taxa %in% c("Hyperacanthomysis longirostris Adult", 
                                                "Neomysis mercedis Adult", "Neomysis kadiakensis Adult")), 
       aes(x = SprNDOI2, y = log(BPUE+1))) + 
  geom_point() + geom_smooth(method = "lm", aes(color = sig, alpha = sig))+
  scale_alpha_manual(values = c(0, .5), guide = NULL)+ 
  scale_color_manual(values = c("grey", "blue"), guide = NULL)+
  facet_wrap(~Taxa) + xlab("Mean March-May Delta Outflow Index (10,000 cfs)")+
  ylab("log-transformed March-May BPUE (ug/mL")+
  geom_text(data = filter(foo, Taxa %in% c("Hyperacanthomysis longirostris Adult", 
                                            "Neomysis mercedis Adult", "Neomysis kadiakensis Adult")),
            aes(x = 5, y = 0, label = paste(slope, "X + ", intercept, " p=", p,  sep = "")))


###############################################################################
#do it again with CPUE


ZoopC <- read_csv("SeasonalCPUE_bytaxa.csv")
ZoopC = rename(ZoopC, Year = water_year, Taxa = Taxlifestage) %>%
  left_join(yeartypes)

ZoopspC = filter(ZoopC, Season == "Spring", !is.na(SprNDOI), Year <2020)

ggplot(ZoopspC, aes(x = SprNDOI, y = log(szn_CPUE+1))) + geom_point() + geom_smooth(method = "lm")+
  facet_wrap(~Taxa, scales = "free_y") + xlab("Spring Net Delta Outflow Index")+
  ylab("log-transformed spring zooplankton CPUE")

ggplot(filter(ZoopspC,  Year >1999), aes(x = SprNDOI, y = log(szn_CPUE+1))) + geom_point() + geom_smooth(method = "lm")+
  facet_wrap(~Taxa, scales = "free_y") + xlab("Spring Net Delta Outflow Index")+
  ylab("log-transformed spring zooplankton BPUE (ug/mL)")

LMc = function(dat) {summary(lm(data = dat, log(szn_CPUE+1)~ SprNDOI))$coefficients[2,4]}
ZoopspC$SprNDOI2 = ZoopspC$SprNDOI/10000
testC = filter(ZoopspC, Year > 1999) %>%
  group_by(Taxa) %>%
  do(slope =summary(lm(data = ., log(szn_CPUE+1)~ SprNDOI2))$coefficients[2,1],
     intercept = summary(lm(data = ., log(szn_CPUE+1)~ SprNDOI2))$coefficients[1,1],
     p =  summary(lm(data = ., log(szn_CPUE+1)~ SprNDOI2))$coefficients[2,4]) %>%
  ungroup() 

fooC = data.frame(Taxa = testC$Taxa, slope = round(unlist(testC$slope), digits = 3), 
                 intercept = round(unlist(testC$intercept), digits = 2),
                 p = round(unlist(testC$p), digits = 3))

Zoopsp2C = left_join(ZoopspC, fooC) %>%
  mutate(sig = case_when(
    p > 0.05 ~ "NS",
    TRUE ~ "SIG"
  ))

ggplot(filter(Zoopsp2C, Year >1999), aes(x = SprNDOI2, y = log(szn_CPUE+1))) + 
  geom_point() + geom_smooth(method = "lm", aes(color = sig, alpha = sig))+
  scale_alpha_manual(values = c(0, .5), guide = NULL)+ 
  scale_color_manual(values = c("grey", "blue"), guide = NULL)+
  facet_wrap(~Taxa, scales = "free_y") + xlab("Mean March-May Delta Outflow Index (10,000 cfs)")+
  ylab("log-transformed March-May CPUE")+
  geom_text(data = fooC, aes(x = 5, y = 0, label = paste(slope, "X + ", intercept, " p=", p,  sep = "")))

ggplot(filter(Zoopsp2C, Year >1999, Taxa %in% c("Hyperacanthomysis longirostris Adult", 
                                                "Neomysis mercedis Adult", "Neomysis kadiakensis Adult")), 
       aes(x = SprNDOI2, y = log(szn_CPUE+1))) + 
  geom_point() + geom_smooth(method = "lm", aes(color = sig, alpha = sig))+
  scale_alpha_manual(values = c(0, .5), guide = NULL)+ 
  scale_color_manual(values = c("grey", "blue"), guide = NULL)+
  facet_wrap(~Taxa) + xlab("Mean March-May Delta Outflow Index (10,000 cfs)")+
  ylab("log-transformed March-May CPUE")+
  geom_text(data = filter(fooC, Taxa %in% c("Hyperacanthomysis longirostris Adult", 
                                              "Neomysis mercedis Adult", "Neomysis kadiakensis Adult")),
                          aes(x = 5, y = 0, label = paste(slope, "X + ", intercept, " p=", p,  sep = "")))


mysidsC = filter(Zoopsp2C, Taxa %in% c("Hyperacanthomysis longirostris Adult", 
                                     "Neomysis mercedis Adult", "Neomysis kadiakensis Adult"))

mysidsC = mutate(mysidsC, regime = case_when(
  Year < 1987 ~ "preclam",
  Year >=  1987 & Year<2000 ~ "PrePOD",
  Year >= 2000 ~ "POD and after"
))

library(RColorBrewer)
ggplot(mysidsC, aes(x = SprNDOI2, y = log(szn_CPUE+1), color = regime)) + 
  geom_point() + geom_smooth(method = "lm", aes(color = regime))+
  scale_color_manual(values = brewer.pal(3, "Dark2"), 
                     labels = c("POD: 2000-2020", "Pre-clam: 1975-1986", "Post-Clam: 1987-2019"))+
  facet_wrap(~Taxa, scales = "free_y") + xlab("Mean March-May Delta Outflow Index (10,000 cfs)")+
  ylab("log-transformed March-May BPUE (ug/mL)")+theme_bw()+
  theme(legend.position = "bottom")


ggplot(filter(mysidsC, Year >1999), aes(x = SprNDOI2, y = log(szn_CPUE+1), color = Taxa)) + 
  geom_point() + geom_smooth(method = "lm")+
  scale_color_manual(values = brewer.pal(3, "Dark2"))+
 xlab("Mean March-May Delta Outflow Index (10,000 cfs)")+
  ylab("log-transformed March-May CPUE")+theme_bw()+
  theme(legend.position = "bottom")
