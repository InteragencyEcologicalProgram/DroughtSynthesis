#various flow metrics

library(cder)
library(lubridate)
library(tidyverse)
library(car)
library(MASS)
library(brms)
library(cmdstanr)
library(emmeans)
library(multcomp)
#apparently exports are HRO and TRP sensor 70

#https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=HRO

#https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=TRP

#we also want DTO for Delta Outflow - sensor 23

Outflow = cdec_query("DTO", 23, "D", as.Date("2014-01-01"), as.Date("2020-09-30"))
names(Outflow)
#better source for 2021 outflow
DTO = read_csv("data/NDOI_WY2021.csv")
DTO = mutate(DTO, DateTime = mdy(Date), SensorType = "OUTFLOW", SensorNumber = 23, StationID = "DTO") %>%
  rename(Value = NDOIcfs)


Ex1 = cdec_query("HRO", 70, "D", as.Date("2014-01-01"), as.Date("2021-10-31"))

Ex2 = cdec_query("TRP", 70, "D", as.Date("2014-01-01"), as.Date("2021-10-31"))

flow = bind_rows(Outflow, Ex1, Ex2, DTO)

ggplot(flow, aes(x = DateTime, y = Value, color = StationID))+geom_line()+
  facet_wrap(~StationID, scales = "free_y", nrow = 3)

#Calculate total exports

flow2 = group_by(flow, DateTime, SensorNumber, SensorType) %>%
  summarize(Value = sum(Value)) %>%
  mutate(Year = year(DateTime), Month = month(DateTime))

ggplot(flow2, aes(x = DateTime, y = Value, color = SensorType))+geom_line()+
  facet_wrap(~SensorType, scales = "free_y", nrow = 3)

#Average by month
flowmonth = group_by(flow2, Month, Year, SensorType) %>%
  summarize(Value = mean(Value, na.rm=T)) %>%
  mutate(Yearmonth = Year + (Month-1)/12)

ggplot(flowmonth, aes(x = Yearmonth, y = Value, color = SensorType))+geom_line()+
  facet_wrap(~SensorType, scales = "free_y", nrow = 3)

load("barrierhabs.RData")

#We may also want to do absent/low/high
BH2 = mutate(BH2, HABord = case_when(
  Microcystis == 1 ~ "Absent",
  Microcystis %in% c(2,3) ~ "Low",
  Microcystis %in% c(4,5) ~ "High"
),
HABord = factor(HABord, levels = c("Absent", "Low", "High"), ordered = T)) %>%
  filter(Year>2013) %>%
  droplevels()


#summarize by month
BHmonth = group_by(BH2, Month, Year) %>%
  summarise(Absent = length(HABord[which(HABord == "Absent")]),
          Low = length(HABord[which(HABord == "Low")]),
         High = length(HABord[which(HABord == "High")]),
         Total = length(HABord)) %>%
  mutate(Yearmonth = Year + (Month-1)/12,
         Absentp = Absent/Total,
         Highp = High/Total,
         Lowp = Low/Total,
         Present = Low+High,
         Presentp = 1-Absentp)

flowm2 = pivot_wider(flowmonth, id_cols = c(Month, Year), names_from = SensorType, values_from = Value)

Habflow = left_join(BHmonth, flowm2)


ggplot(Habflow, aes(x=Absentp, y = log(OUTFLOW))) +geom_point()+geom_smooth()

ggplot(Habflow, aes(x=Lowp, y = log(OUTFLOW))) +geom_point()+geom_smooth()
ggplot(Habflow, aes(x=Highp, y = log(OUTFLOW))) +geom_point()+geom_smooth()
ggplot(Habflow, aes(y=Presentp, x = log(OUTFLOW))) +
  geom_point(aes( color = as.factor(Year)))+geom_smooth(method = "lm") +
  ylab("Percent of observations with Microcystis present")+
  xlab("Log-transformed Delta Outflow (CFS)") + theme_bw()+
  scale_color_discrete(name = NULL)+
  geom_vline(xintercept = log(3000), linetype = 2, color = "blue")+
  geom_vline(xintercept = log(4000), color = "red")+
  annotate("text", x = 7.95, y = 0, label = "TUCP", angle = 90, color = "blue")+
  annotate("text", x = 8.25, y = 0, label = "D-1641", angle = 90, color = "red")

b1 = glmer(cbind(Present,Total)~ log(OUTFLOW) + (1|Year), data = Habflow,
           family = "binomial")
summary(b1)

newdata = filter(Habflow, Year == 2021, Month %in% c(6,7,8))

ggplot(filter(Habflow, Year == 2021, Month %in% c(6,7,8)), 
              aes(x=Month, y = OUTFLOW)) + geom_line() + 
  coord_cartesian(ylim= c(2000, 4000))

foo = predict(b1, newdata = newdata)

newdata2 = newdata       
newdata2$OUTFLOW = 4000
foo2 = predict(b1, newdata = newdata2)
foo
foo2

library(rsq)
rsq(b1)

#############################################################################
#look at all months, not just May-October

str(HABs)
HABs = filter(HABs, !is.na(Month), !is.na(Microcystis))
ggplot(HABs, aes(x = as.factor(Month), fill = as.factor(Microcystis))) + 
  geom_bar(position = "fill")+
  scale_fill_manual(values = c("white", "tan2", "yellow", "red", "darkred"), 
                    labels = c("absent", "low", "medium", "high", "very high"),
                    name = "Microcystis")+ ylab("Relative Frequency")+
  xlab("Month of year (2013-2021)")

############################################################################
#OK, I think I want flow on a finer scale, plus temperature. Can I do that?
#Nutrients would also be nice.

library(DroughtData)
flowX = raw_hydro_1975_2021 %>%
  rename(Year = YearAdj)
Temps = raw_wq_1975_2021 %>%
  rename(Year = YearAdj)

names(flowX)
names(Temps)
names(HWR)
HWR = mutate(HWR, Date = as.Date(Date))
test = left_join(HWR, dplyr::select(flowX, -Year, -Season)) %>%
  filter(Temperature >0)


#OK! MOdel HABs versus Outflow and temperature
M1 = polr(HABord ~ Temperature + Outflow + Stratum2, data = test, Hess = F)
summary(M1)

Anova(M1)
pr <- profile(M1)
confint(pr)
plot(pr)

pairs(pr)

(ctable <- coef(summary(M1)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(ord1))
exp(cbind(OR = coef(ord1), ci))
#OK, I'm not sure what's going on here, but we need random effects of station and year, so switching to brms

test2 = dplyr::filter(test, Temperature < 30)

#start with this. I may also want month or day of year in here.
#Exports will be another interesting explanitory variable. I also need
#to figure out if I"m using the right distribution
M2 = brm(HABord ~ Temperature + Outflow + Stratum2 + (1|Year), data = test2, family = cumulative,
         chains = 2, iter = 2000,   backend = "cmdstanr", normalize = FALSE)
summary(M2)
#why is this SO SLOW!?

M3.1 = brm(HABord ~ Temperature + Outflow + Stratum2 + (1|Year), data = test2, family = cumulative,
         iter = 4000,   backend = "cmdstanr", normalize = FALSE, 
         control = list(adapt_delta = 0.95, max_treedepth = 15),
         chains = 2, cores=4, threads = threading(2))
summary(M3.1)



## Extract marginal effects
M3.1_conditions <- make_conditions(M3.1, "Temperature")
M3.1_effects <- conditional_effects(M3.1, "Stratum2", condition= M3.1_conditions, categorical= TRUE)
M3.1_effects$`Stratum2:cats__`

temp = conditional_effects(M3.1, "Temperature", categorical= TRUE)
conditional_effects(M3.1, "Stratum2",categorical= TRUE)


M4 = brm(HABord ~ Temperature + Export + Stratum2 + (1|Year), data = test2, family = cumulative,
          iter = 4000,   backend = "cmdstanr", normalize = FALSE, 
         control = list(adapt_delta = 0.99, max_treedepth = 20),
         chains = 2, cores=4, threads = threading(2))

summary(M4)
#Wow. Three terms and a random effect and it looks like its going to take days to run.
#####################

#let's simplify

SoDelta = filter(test2, Stratum2 == "South Delta")


M5.1 = brm(HABord ~ Temperature + Export +  Yearf, data = SoDelta, family = cumulative,
         iter = 4000,   backend = "cmdstanr", normalize = FALSE, 
         control = list(max_treedepth = 15),
         chains = 2, cores=4, threads = threading(2))

summary(M5.1)
#It went fast once. Now it's not. I'm really sad :(

M5 = polr(HABord ~ Temperature + Export + Stratum2, data = test2)

summary(M5)

(ctable <- coef(summary(M5)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(ctable, "p value" = p))

library(AER)
coeftest(M5)

ggplot(SoDelta, aes(x = HABord, y = Export)) + geom_boxplot()+
  facet_wrap(~Yearf)

#OK, there were no "absent" values in 2016 in the south delta 


M5.2 = polr(HABord ~ Temperature + Export, data = filter(SoDelta, Year == 2021))
summary(M5.2)
plot(profile(M5.2))

#OK, this is super, duper not working. WHat can I do here?

M5.1 = brm(HABord ~ Temperature + Outflow + Export +  Yearf, data = SoDelta, family = acat,
           iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
           control = list(max_treedepth = 15),
           chains = 2, cores=4, threads = threading(2))
#Why did this one run so fast? 

summary(M5.1)
conditional_effects(M5.1, categorical = TRUE)
max_mc1_effects <- conditional_effects(M5.1, "Temperature", categorical= TRUE)$Temperature
ggplot(max_mc1_effects) + geom_smooth(aes(x = Temperature, y = estimate__, color = HABord)) +
  geom_ribbon(aes(x = Temperature, ymin = lower__, ymax = upper__))
  

#hmmm.... this one is definitley telling me that outflow is a better predictor than exports.
#Oh, but I wanted to scale this. And maybe log-transform exports
SoDelta = mutate(SoDelta, day = yday(Date), Outscale = scale(Outflow), Exscale = scale(Export), Tempscale = scale(Temperature))

M5.2 = brm(HABord ~ Tempscale + Outscale + Exscale + (1|Yearf/day), data = SoDelta, family = acat,
           iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
           control = list(max_treedepth = 15),
           chains = 2, cores=4, threads = threading(2))
#Why did this one run so fast? 

summary(M5.2)
conditional_effects(M5.2, categorical = TRUE)
#hmmm.... this one is definitley telling me that outflow is a better predictor than exports.
#what the actual fuck. Now it's super fast. 
#was it the 'acat' instead of cumulative?
M5.3 = brm(HABord ~ Tempscale + Outscale + Exscale + Yearf + (1|day), data = SoDelta, family = cumulative,
           iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
           control = list(max_treedepth = 15),
           chains = 2, cores=4, threads = threading(2))
# Nope. Not that. 
summary(M5.3)
conditional_effects(M5.3, categorical = TRUE)
#definitley a lower outflow effect and a bigger export effect when you take day of year into account
#save(M5.3, M5.2, M5.1, file = "HABsbrimsresults.RData")
######################################################
#Let's try weekly averages
#take out the "far west" region
library(lme4)
library(lmerTest)

testweek = mutate(test2, Week = week(Date)) %>%
  filter(Stratum2 != "Far West") %>%
  group_by(Week, Month, Year, Stratum2) %>%
  summarise(Absent = length(HABord[which(HABord == "Absent")]),
            Low = length(HABord[which(HABord == "Low")]),
            High = length(HABord[which(HABord == "High")]),
            Total = length(HABord),
            Export = mean(Export, na.rm = T), Outflow = mean(Outflow, na.rm = T), 
            Temperature = mean(Temperature, na.rm = T)) %>%
  mutate(Yearmonth = Year + (Month-1)/12,
         Absentp = Absent/Total,
         Highp = High/Total,
         Lowp = Low/Total,
         Present = Low+High,
         Presentp = 1-Absentp)%>%
  ungroup()%>%
  mutate( Outscaled = scale(log(Outflow)), Expxcale = scale(Export), Tempscale = scale(Temperature))

bleh = glmer(cbind(Present,Total)~ Outscaled + Expxcale+ Tempscale + Week + (1|Stratum2)+ (1|Year), data = testweek,
             family = "binomial", na.action = "na.fail")
summary(bleh)
visreg(bleh, type = "conditional", scale = "response")

library(MuMIn)
dredge(bleh)
#OK, AIC says all of them except outflow.


#Is it the same for all regions?

bleh2 =  glmer(cbind(Present,Total)~ Outscaled*Stratum2 + Expxcale*Stratum2+ Tempscale + Week + (1|Year), data = testweek,
               family = "binomial", na.action = "na.fail")
summary(bleh2)
dredge(bleh2)

#Best model
best = glmer(cbind(Present,Total)~ Expxcale*Stratum2+ Tempscale + Week + (1|Year), data = testweek,
             family = "binomial", na.action = "na.fail")
summary(best)
visreg(best, xvar = "Tempscale")
visreg(best, xvar = "Week")
visreg(best, xvar = "Expxcale", by = "Stratum2")

#just hte south delta, to see if it matches the BRMS results

Soweek = filter(testweek, Stratum2 == "South Delta")


blehSouth =  glmer(cbind(Present,Total)~ Outscaled + Expxcale+ Tempscale + Week + (1|Year), data = Soweek,
               family = "binomial", na.action = "na.fail")
summary(blehSouth)
dredge(blehSouth)

#I am somewhat confused. And it's friday.

#############################################

load("data/Dayflow.RData")

DF = mutate(DF, Year = year(Date), Month = month(Date), Projects = CVP + SWP, EO = Projects/OUT, ominuse = Projects - OUT)
DFsummer = filter(DF, Month %in% c(6,7,8,9), OUT >0)

ggplot(DFsummer, aes(y = OUT, x = Projects)) + geom_point()+ geom_smooth()+
  geom_abline(slope = 1, intercept = 0, size = 2, color = "red")

ggplot(DFsummer, aes(x = ominuse)) + geom_histogram()       
