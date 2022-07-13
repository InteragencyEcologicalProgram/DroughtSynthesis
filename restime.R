#let's calculate residence time!!!!!!!!!!!!!!!!!!!!!!!!

library(smonitr)
library(tidyverse)
library(lubridate)
library(readxl)

#Get the Dayflow data from the CNRA portal
Dayflow = get_odp_data(pkg_id = "dayflow", fnames = "Dayflow Results")


#I suck at dealing with lists, so I broke it up into component data frames and then put them together
#there is probably a better way of doing this. 
DF1970_83 =  Dayflow$`Dayflow Results 1970 - 1983` %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  dplyr::select(Date, OUT, EXPORT, SJR, GCD, SAC, CVP, SWP, MISDV) %>%
  rename(EXPORTS = EXPORT)

DF1984_96 = Dayflow$`Dayflow Results 1984 - 1996` %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  dplyr::select(Date, OUT, EXPORT, SJR, GCD, SAC, CVP, SWP, MISDV) %>%
  rename(EXPORTS = EXPORT)

DF1997_2020 =  Dayflow$`Dayflow Results 1997 - 2020` %>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  dplyr::select(Date, OUT, EXPORTS, SJR, GCD, SAC, CVP, SWP, MISDV)


DF2021 =  Dayflow$`Dayflow Results 2021` %>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  dplyr::select(Date, OUT, EXPORTS, SJR, SAC, CVP, SWP, MISDV) 


#now I can put them all together!
DF = bind_rows(DF1970_83, DF1984_96, DF1997_2020, DF2021)

#Residence time calculations from Hammock et al 2015

#Sacramento River Residence time
#Intercetp - 4.210
#Sac river flow - -1.012 (log-10 transformed)\
#Ag diversions (GCD) - 0.005748
#Pumping - 0.000271
#Ag x Flow interaction - -0.001951

# Variable	Parameter estimate	
# Intercept	2.9227944	
# Flow (SJR)	-0.6820884	
# Ag div (GCD)	-0.0044666	
# Pump	-0.0050784	
# Pump × Flow	0.0019711	
# Ag div × Flow	0.0022309	

#This was validated on monthly averages (the 15th-14th), so that's what I'll use here too
#Now convert everything to metric!!!!!!!

DFmonth = mutate(DF, Month = month(Date), Year = year(Date), 
                 Month2 = case_when(mday(Date) >14 ~ (Month + 1),
                                    TRUE ~ Month),
                 Year = case_when(Month2 == 13 ~ Year +1,
                                  TRUE ~Year),
                 Month2 = case_when(Month2 == 13 ~ 1,
                                    TRUE ~ Month2),
                 WY = case_when(Month2 %in% c(10,11,12) ~ Year +1, Month2 %in% c(1:9)~ Year),
                 LogSAC = log(SAC/35.315, base = 10),
                 LogSJR = log(SJR/35.315, base = 10),
                 Pumps = CVP + SWP)%>%
  group_by(Month2, Year, WY) %>%
  summarize(SACl = mean(LogSAC), SAC = log(mean(SAC), base = 10), SJR = mean(LogSJR), Pump = mean(Pumps/35.315), Ag = mean(GCD/35.315))



DFRes = mutate(DFmonth, SJRres = 2.92279 + SJR*-0.68208 + Ag*-0.0044666+ Pump*-0.0050784 + Pump*SJR*0.0019711+ Ag*SJR*0.0022308,
               SACres = 4.121+ SAC*-1.012 + Ag*0.005748+Pump*0.000271+ Ag*SAC*-0.001951,
               Ymonth = WY + (1/12)*Month2)  

ggplot(DFRes, aes(x = Ymonth, y = 10^SACres)) + geom_point()
ggplot(DFRes, aes(x = Ymonth, y = 10^SJRres)) + geom_point()



#set the coeffiencts for an SJR model
lmSJR = lm(SJRres ~ SJR + Ag + Pump + Pump*SJR + Ag*SJR, data = DFRes)


SJRres2 = predict(lmSJR, newdata = DFmonth)
DFres2 = bind_cols(SJRres2 =SJRres2,  DFmonth) %>%
  mutate(Ymonth = WY + (1/12)*Month2)

ggplot(DFres2, aes(x = Ymonth, y = SJRres2)) + geom_point()+
  xlab("Year")+ ylab("San Joaquin Residence TIme")


#Now for the sacramento

lmSC = lm(SACres ~ SAC + Ag + Pump + Ag*SAC, data = DFRes)
coeffsSC = lmSC$coef

#lmSC$coefficients = c(4.210, -1.012, 0.005748,0.000271, -0.001951)

SACres2 = predict(lmSC, newdata = DFmonth)
DFres2b = bind_cols(SACres2 =SACres2,  DFres2) %>%
  mutate(Ymonth = WY + (1/12)*Month2)

ggplot(DFres2, aes(x = Ymonth, y = SACres2)) + geom_point()+
  xlab("Year")+ ylab("Sacramento Residence TIme")

################################################################
#Check out Bruce's data

Bruce = read_csv("data/ResTimeInputs.csv") %>%
  rename(Year = water_year_sam)
Yeartypes = read_csv("data/yearassignments.csv")

Bruce = left_join(Bruce, Yeartypes)

ggplot(Bruce, aes(x = Drought, y = sac_pred_full)) + geom_boxplot()
ggplot(Bruce, aes(x = Drought, y = sjr_pred_full)) + geom_boxplot()
ggplot(Bruce, aes(x = ag_div, y = GCD, color = Year)) + geom_point()
ggplot(Bruce, aes(x = Year, y = GCD, color = month_fifteen)) + geom_point()
ggplot(Bruce, aes(x = Year, y = ag_div, color = month_fifteen)) + geom_point()


##############################################################################
#OK, Apparently i'm going to have to re-do his model fitting excercise with the new modeled outputs from Shey
Shey = read_excel("data/")
