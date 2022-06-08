#quick plots of flow

# The 'smonitr" package is one some DWR folks put together for organizing some of the IEP data
#remotes::install_github("InteragencyEcologicalProgram/smonitr")
library(smonitr)
library(lubridate)
library(tidyverse)
library(zoo)

#Get the Dayflow data from the CNRA portal
Dayflow = get_odp_data(pkg_id = "dayflow", fnames = "Dayflow Results")


#I suck at dealing with lists, so I broke it up into component data frames and then put them together
#there is probably a better way of doing this. 
DF1997_2020 =  Dayflow$`Dayflow Results 1997 - 2020` %>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  dplyr::select(Date, OUT, EXPORTS, SJR)


DF2021 =  Dayflow$`Dayflow Results 2021` %>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  dplyr::select(Date, OUT, EXPORTS, SJR) 


#now I can put them all together!
DF = bind_rows(DF1997_2020, DF2021)

#Calculate the seven-day rolling average
DFlong = pivot_longer(DF, cols = c(OUT, EXPORTS, SJR), names_to = "Metric", values_to = "CFS") %>%
  filter(CFS >0) %>%
  group_by(Metric) %>%
  mutate(Sevenday = rollmean(CFS, k = 7, fill = NA), DOY = yday(Date))

#Mean daily flow metric over the past twenty two years
DFmean = group_by(DFlong, Metric, DOY) %>%
  summarize(Sevenday = exp(mean(log(Sevenday), na.rm = T)), CFS = mean(CFS, na.rm = T)) %>%
  mutate(Metric2 = "Historic Mean")

DFlong2 = mutate(DFlong, Metric2 = "2021") %>%
  filter(Date > ymd("2021-05-01")) %>%
  bind_rows(DFmean)

#some quick plots
ggplot(filter(DFlong, Date > ymd("2021-05-01"), CFS >0), aes(x = Date, y = CFS, color = Metric))+
         geom_line()

ggplot(filter(DFlong, Date > ymd("2021-05-01"), CFS >0), aes(x = Date, y = Sevenday, color = Metric))+
  geom_line()

#this is FIgure 2-6 in the report
ggplot(filter(DFlong2, DOY >120, DOY <270) )+
  geom_line(aes(x = DOY, y = Sevenday, color = Metric, linetype = Metric2), size = 1)+
   theme_bw()+
  scale_x_continuous(breaks = c(121, 152, 182, 213, 244, 274), labels = c("May", "Jun", "Jul", "Aug", "Sep", "Oct"))+
  scale_linetype(name = NULL)+
  scale_color_brewer(palette = "Set2", name = NULL)+
  xlab("Date")+ ylab("Flow (CFS - seven day average)")

#Facet by metric to be easier to show in presentation
ann_text <- data.frame(Metric = c("EXPORTS", "OUT", "OUT","EXPORTS", "OUT", "OUT"), 
                       Line = c("TUCP", "D1641", "TUCP", "TUCP", "D1641", "TUCP"),
                       Sevenday = c(1500, 4000, 3000, 1500, 4000, 3000),
                       DOY = c(121, 152, 152, 213, 213, 213))

ggplot(filter(DFlong2, DOY >120, DOY <270) )+
  geom_line(aes(x = DOY, y = Sevenday,  linetype = Metric2), size = 1)+
  theme_bw()+
  geom_line(data = ann_text,mapping = aes(x = DOY, y = Sevenday, color = Line), size = 1)+
  scale_x_continuous(breaks = c(121, 152, 182, 213, 244, 274), labels = c("May", "Jun", "Jul", "Aug", "Sep", "Oct"))+
  scale_linetype(name = NULL)+
  scale_color_brewer(palette = "Set2", name = NULL)+
  facet_wrap(~Metric, nrow = 3, scales = "free_y")+
  xlab("Date")+ ylab("Flow (CFS - seven day average)")



DFmonth = mutate(DFlong2, Month = month(Date)) %>%
  group_by(Metric, Metric2, Month) %>%
  summarize(Min = min(CFS, na.rm = T), Max = max(CFS, na.rm = T), mean = mean(CFS, na.rm = T))

DFmonth2 = pivot_wider(filter(DFmonth, Metric2 == "2021"), id_cols = Month, names_from = Metric, values_from = c(Min, Max, mean))      

#this is table 2-6 in the report
write.csv(DFmonth2, file = "Flowsummary.csv", row.names = F)       


#######################################################
#flow by year

DFlong =  mutate(DFlong, Year = year(Date),
                Metric = factor(Metric, levels = c("OUT", "SJR", "EXPORTS"), labels = c("Outflow", "San Joaquin Flow", "Project Exports"))) 


#This is figure 2-19 in the report
ggplot(filter(DFlong, DOY >120, DOY <270, Year > 2014) )+
  geom_line(aes(x = DOY, y = Sevenday, color = as.factor(Year)), size = 1)+
  theme_bw()+
  scale_x_continuous(breaks = c(121, 152, 182, 213, 244, 274), labels = c("May", "Jun", "Jul", "Aug", "Sep", "Oct"))+
  scale_color_brewer(palette = "Set2", name = NULL)+
  facet_wrap(~Metric, nrow = 3, scales = "free_y")+
  xlab("Day of Year")+ ylab("Flow (CFS - seven day average)")

#average summer outflow by water year type
yeartypes = read.csv("data/yearassignments.csv")

DFlongb = left_join(DFlong, yeartypes) %>%
  filter(DOY >120, DOY <270, Metric == "Outflow") %>%
  group_by(DOY, Yr_type, Year) %>%
  summarize(OUTm = mean(CFS, na.rm = T))

DFlongb = group_by(DFlongb, Yr_type, Year) %>%
  summarize(OUTmm = mean(OUTm, na.rm = T))

############################################################
#Correlations
ggplot(DF, aes(x = EXPORTS, y = OUT)) + geom_point() + geom_smooth()
ggplot(DF, aes(x = OUT, y = EXPORTS)) + geom_point() + geom_smooth()

ggplot(filter(DF, yday(Date) >120 & yday(Date) <270, OUT >0), aes(x = EXPORTS, y = OUT)) + geom_point() + geom_smooth()
ggplot(filter(DF, yday(Date) >120 & yday(Date) <270, OUT >0), aes(x = SJR, y = OUT)) + geom_point() + geom_smooth()
ggplot(filter(DF, yday(Date) >120 & yday(Date) <270, OUT >0), aes(x = EXPORTS, y = SJR)) + geom_point() + geom_smooth()
