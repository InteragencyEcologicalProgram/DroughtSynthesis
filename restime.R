#let's calculate residence time!!!!!!!!!!!!!!!!!!!!!!!!

library(smonitr)
library(tidyverse)
library(lubridate)
library(readxl)
library(visreg)
library(MuMIn)

#Get the Dayflow data from the CNRA portal
# Dayflow = get_odp_data(pkg_id = "dayflow", fnames = "Dayflow Results")
# 
# 
# #I suck at dealing with lists, so I broke it up into component data frames and then put them together
# #there is probably a better way of doing this. 
# DF1970_83 =  Dayflow$`Dayflow Results 1970 - 1983` %>%
#   mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
#   dplyr::select(Date, OUT, EXPORT, SJR, GCD, SAC, CVP, SWP, MISDV) %>%
#   rename(EXPORTS = EXPORT)
# 
# DF1984_96 = Dayflow$`Dayflow Results 1984 - 1996` %>%
#   mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
#   dplyr::select(Date, OUT, EXPORT, SJR, GCD, SAC, CVP, SWP, MISDV) %>%
#   rename(EXPORTS = EXPORT)
# 
# DF1997_2020 =  Dayflow$`Dayflow Results 1997 - 2020` %>%
#   mutate( Date = as.Date(Date, format = "%m/%d/%Y")) %>%
#   dplyr::select(Date, OUT, EXPORTS, SJR, GCD, SAC, CVP, SWP, MISDV)
# 
# 
# DF2021 =  Dayflow$`Dayflow Results 2021` %>%
#   mutate( Date = as.Date(Date, format = "%m/%d/%Y")) %>%
#   dplyr::select(Date, OUT, EXPORTS, SJR, SAC, CVP, SWP, MISDV) 
# 
# 
# #now I can put them all together!
# DF = bind_rows(DF1970_83, DF1984_96, DF1997_2020, DF2021)
#save(DF, file = "data/dayflow.RData")
load("data/dayflow.RData")

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
Shey = read_excel("data/Ag_diversions_fromShey.xlsx") %>%
  rename(Yearmonth = concantonate_2) %>%
  mutate(Month = month(Yearmonth))  %>%
  dplyr::select(Yearmonth, Month, mod, water_year_sam)

Bruce2 = read_excel("data/rt_df_merged.xlsx", sheet = "rt_df_merged")
str(Bruce2)
BruceDSMRT = filter(Bruce2, !is.na(mean_sac_rt), pumping == "yes") %>%
  mutate(Month = as.numeric(factor(month_fifteen, levels = c("January","February",  "March","April", "May", 
                                                  "June", "July","August","September","October", "November","December")))) %>%
  left_join(Shey) %>%
  dplyr::select(Month, water_year_sam, ag_div_m, mod, SJR_m, SAC_m, CVP_m, SWP_m, mean_sac_rt, mean_sjr_rt) %>%
  mutate(mod_m = mod/35.315, LogSac = log(SAC_m, base = 10), LogSJR = log(SJR_m),
         logRT_sac = log(mean_sac_rt, base = 10), 
         logRT_sjr = log(mean_sjr_rt, base = 10),
         pump = SWP_m + CVP_m)

lm_sac = glm(logRT_sac ~LogSac + mod_m + pump, data = BruceDSMRT)
summary(lm_sac)

lm_sac2 = glm(logRT_sac ~LogSac + ag_div_m + pump, data = BruceDSMRT)
summary(lm_sac2)
plot(lm_sac)

anova(lm_sac, lm_sac2)

visreg(lm_sac)

#Use the same method of checking all possible models that Bruce did
global_sac = glm(logRT_sac ~LogSac + mod_m + pump + pump*LogSac + mod_m*LogSac,
                 data = BruceDSMRT, na.action = na.fail)
sac1 = dredge(global_sac)
#Should I use the top model? Or average the top two? Or top three?
topmod = get.models(sac1, subset = delta <3)
lm_sac_best = model.avg(topmod)
summary(lm_sac_best)

lm_sac_best2 = glm(logRT_sac ~LogSac*pump, data = BruceDSMRT)
summary(lm_sac_best2)

global_sac2 = glm(logRT_sac ~LogSac + ag_div_m + pump + pump*LogSac + ag_div_m*LogSac,
                 data = BruceDSMRT, na.action = na.fail)

dredge(global_sac2)
bruce_sac_best = glm(logRT_sac ~LogSac +  pump*LogSac + ag_div_m*LogSac,
                     data = BruceDSMRT, na.action = na.fail)

#Huh. 

#Now predict based on both my model and Bruce's model, I guess
BruceDSMRT = mutate(BruceDSMRT, preds_rosie1 = predict(lm_sac_best), preds_rosie2 = predict(lm_sac_best2),
                    preds_Bruce = predict(bruce_sac_best))

long_rt = pivot_longer(BruceDSMRT, cols = c(logRT_sac, preds_rosie1, preds_rosie2, preds_Bruce),
                       names_to = "Model", values_to = "LogRT"
                       ) %>%
  mutate(yearmonth = water_year_sam + (Month-1)/12)
ggplot(long_rt, aes(x=Month, y = LogRT, color = Model))+ geom_point()+ geom_line()+
  facet_wrap(~water_year_sam)

########################################################
#OK, as far as the data we have so far, My model and Bruce's model are almost identical (for Sac)
#let's look at historic data
Bruce_historic = filter(Bruce2,  pumping == "yes", water_year_sam > 1979) %>%
  mutate(Month = as.numeric(factor(month_fifteen, levels = c("January","February",  "March","April", "May", 
                                                             "June", "July","August","September","October", "November","December")))) %>%
  left_join(Shey) %>%
  dplyr::select(Month, water_year_sam, ag_div_m, mod, SJR_m, SAC_m, CVP_m, SWP_m, mean_sac_rt, mean_sjr_rt) %>%
  mutate(mod_m = mod/35.315, LogSac = log(SAC_m, base = 10), LogSJR = log(SJR_m), logRT_sac = log(mean_sac_rt, base = 10),
         logRT_sjr = log(mean_sjr_rt, base = 10),
         pump = SWP_m + CVP_m)

Bruce_historic = mutate(Bruce_historic, preds_rosie1 =predict(lm_sac_best, newdata = Bruce_historic), preds_rosie2 = predict(lm_sac_best2, newdata = Bruce_historic),
                    preds_Bruce = predict(bruce_sac_best, newdata = Bruce_historic))

long_rt_his1 = pivot_longer(Bruce_historic, cols = c(logRT_sac, preds_rosie1, preds_rosie2, preds_Bruce),
                       names_to = "Model", values_to = "LogRT"
) %>%
  mutate(yearmonth = water_year_sam + (Month-1)/12)
ggplot(long_rt_his1, aes(x=yearmonth, y = LogRT, color = Model))+ geom_point()+ geom_line()

#Now back-transform and re-plot residence time
long_rt_his1 = mutate(long_rt_his1, ResTime = LogRT^10)
ggplot(long_rt_his1, aes(x=yearmonth, y = ResTime, color = Model))+ geom_point()+ geom_line()

#now the san joaquin
#############################################################

#Use the same method of checking all possible models that Bruce did
global_sjr = glm(logRT_sjr ~LogSJR + mod_m + pump + pump*LogSJR + mod_m*LogSJR,
                 data = BruceDSMRT, na.action = na.fail)
sj1 = dredge(global_sjr)
#Should I use the top model? Or average the top two? Or top three?

lm_sj_best = get.models(sj1, subset = delta <1)[[1]]
summary(lm_sj_best)
#why doesn't this work with "predict?"
lm_sj_best = glm(logRT_sjr ~LogSJR + mod_m + pump + pump*LogSJR + mod_m*LogSJR,
                 data = BruceDSMRT, na.action = na.fail)
summary(lm_sj_best)


global_sj2 = glm(logRT_sjr ~LogSJR + ag_div_m + pump + pump*LogSJR + ag_div_m*LogSJR,
                  data = BruceDSMRT, na.action = na.fail)
sj2 = dredge(global_sj2)

bruce_sj_best = get.models(sj2, subset = delta <1)[[1]]
summary(bruce_sj_best)
bruce_sj_best = glm(logRT_sjr ~LogSJR + ag_div_m + pump + pump*LogSJR + ag_div_m*LogSJR,
                 data = BruceDSMRT, na.action = na.fail)


#Huh. 

#Now predict based on both my model and Bruce's model, I guess
BruceDSMRT = mutate(BruceDSMRT, preds_rosie1 = predict(lm_sj_best),
                    preds_Bruce = predict(bruce_sj_best))

long_rt = pivot_longer(BruceDSMRT, cols = c(logRT_sjr, preds_rosie1, preds_Bruce),
                       names_to = "Model", values_to = "LogRT"
) %>%
  mutate(yearmonth = water_year_sam + (Month-1)/12)
ggplot(long_rt, aes(x=Month, y = LogRT, color = Model))+ geom_point()+ geom_line()+
  facet_wrap(~water_year_sam)
#the fit is not as good on the San Joaquin side. but my model and Bruce's model fit similarly 


########################################################
#OK, as far as the data we have so far, My model and Bruce's model are almost identical (for Sac)
#let's look at historic data

Bruce_historic2 = mutate(Bruce_historic, preds_rosie1 =predict(lm_sj_best, newdata = Bruce_historic), 
                        preds_Bruce = predict(bruce_sj_best, newdata = Bruce_historic))

long_rt_his = pivot_longer(Bruce_historic2, cols = c(logRT_sjr, preds_rosie1, preds_Bruce),
                           names_to = "Model", values_to = "LogRTSJ"
) %>%
  mutate(yearmonth = water_year_sam + (Month-1)/12)
ggplot(long_rt_his, aes(x=yearmonth, y = LogRTSJ, color = Model))+ geom_point()+ geom_line()

#Now back-transform and re-plot residence time
long_rt_his = mutate(long_rt_his, ResTime = LogRTSJ^10)
ggplot(long_rt_his, aes(x=yearmonth, y = ResTime, color = Model))+ geom_point()+ geom_line()

#now average residence time for AUg, sept, oct

all_hist = long_rt_his1 %>%
  mutate(River = "Sac") %>%
  bind_rows(mutate(long_rt_his, River = "SJR")) %>%
  filter(Model != "preds_rosie2", Model != "preds_Bruce") %>%
  mutate(Model = case_when(Model %in% c("logRT_sac", "logRT_sjr") ~ "DSM2",
                           TRUE ~ Model))

ggplot(all_hist, aes(x=yearmonth, y = ResTime, color = Model))+ geom_point()+ geom_line()+
  facet_wrap(~River, nrow = 2, scales = "free_y")


#Just summer-fall
sumfall = filter(all_hist, Month %in% c(8,9,10)) %>%
  group_by(water_year_sam, River, Model) %>%
  summarize(aveRT = mean(ResTime)) 


ggplot(sumfall, aes(x=water_year_sam, y = aveRT, color = Model))+ geom_point()+ geom_line()+
  facet_wrap(~River, nrow = 2)+
  geom_rect(data = yeartypes, aes(fill = Yr_type, x = Year))

#2014 and 2015 on the SJR side aren't lining up.

test2014 = filter(all_hist, water_year_sam %in% c(2014, 2015))
ggplot(all_hist, aes(x=yearmonth, y = LogSJR))+geom_point() + geom_line()
ggplot(all_hist, aes(x=yearmonth, y = SJR_m))+geom_point() + geom_line()

ggplot(all_hist, aes(x=yearmonth, y = LogSac))+geom_point() + geom_line()
ggplot(all_hist, aes(x=yearmonth, y = mod_m))+geom_point() + geom_line()
ggplot(all_hist, aes(x=ag_div_m, y = mod_m))+geom_point() + geom_smooth(method = "lm")

###################################################################
#Let's bring in the final few years of data!

Allags = read_csv("data/DCD_div_monthly_WY_1970_2021.csv")
names(Allags)
Allags2 = mutate(Allags, datetime = mdy(datetime),Year = year(datetime), Month2 = month(datetime), 
                WY = case_when(Month2 %in% c(10,11,12) ~ Year +1, 
                               TRUE ~ Year),
                mod_m = Div_cfs/35.315) 

DFAll = left_join(DFmonth, Allags2)%>%
  rename(LogSac = SACl, LogSJR = SJR, pump = Pump)


preds =data.frame(logSJrt = predict(lm_sj_best, newdata = DFAll), logSACrt = predict(lm_sac_best2, newdata = DFAll))

DFRTall = bind_cols(DFAll, preds) %>%
  mutate(SACRT = 10^logSACrt, 
                 SJRT = 10^logSJrt, mYear = Year + (1-Month2)/12)
ggplot(DFRTall)  + 
  geom_col(aes(x = mYear, y = 250, fill = Drought), alpha = 0.3)+
  geom_line( aes(x = mYear, y = SJRT))+
  drt_color_pal_drought()+
  xlab("Date") +ylab("San Joaquin Residnece Time (Days)")

ggplot(DFRTann)  + 
  geom_col(aes(x = WY, y = 130, fill = Drought), alpha = 0.3)+
  geom_line( aes(x = WY, y = SJRT))+
  drt_color_pal_drought()+
  xlab("Date") +ylab("San Joaquin Residnece Time (Days)")+
  theme_bw()

ggplot(DFRTall) + 
  geom_col(aes(x = mYear, y = 120, fill = Drought), alpha = 0.3)+
  geom_line( aes(x = mYear, y = SACRT))+
  drt_color_pal_drought()+
  xlab("date")+ylab("Sacramento Residence Time (Days)")+
  theme_bw()


ggplot(DFRTann) + 
  geom_col(aes(x = WY, y = 75, fill = Drought), alpha = 0.3)+
  geom_line( aes(x = WY, y = SACRT))+
  drt_color_pal_drought()+
  xlab("date")+ylab("Annual Average \nSacramento Residence Time (Days)")+
  theme_bw()

#OK, what's the deal with different results than the other data? But I like these better, so I won't worry about it
pal_yrtype <- c( "C" = "#FDE333", "D" = "#53CC67", "BN" = "#009B95","AN" = "#00588B", "W" = "#481F70FF") 

#now add wate ryear types and make some summary plots
yeartypes = rename(Yeartypes, WY = Year)
DFRTall = left_join(DFRTall, yeartypes) %>%
  mutate(Yr_type = factor(Yr_type, levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"),
                          labels = c("C", "D", "BN", "AN", "W"), ordered = TRUE),
         Month = factor(Month2, levels = c(10,11,12,1,2,3,4,5,6,7,8,9), labels = c("Oct", "Nov", "Dec",
                                                                                  "Jane", "Feb", "Mar",
                                                                                  "Apr", "May", "June",
                                                                                  "Jul", "Aug", "Sep"))) %>%
  filter(!is.na(Yr_type))

#Get a duck for mom
library(rphylopic)
duck = image_data("3ceaa22b-8879-4545-9e32-425010f33cd4", size = 256)[[1]]

#box plot by water year type
ggplot(DFRTall, aes(x = Yr_type, y = SACRT, fill = Yr_type)) + geom_boxplot(alpha = 0.8)+
  facet_wrap(~Month) +
  xlab("Water Year Type") + ylab("Sacramento Residence Time (Days)") + 
  theme_bw()+
  #add_phylopic(duck)+
  scale_fill_manual(values = pal_yrtype, labels = c("Critical", "Dry", "Below N.", "Above N.", "Wet"))

ggplot(DFRTall, aes(x = Yr_type, y = SJRT, fill = Yr_type)) + geom_boxplot()+
  facet_wrap(~Month) +
  xlab("Water Year Type") + ylab("San Joaquin Residence Time (Days)")+ 
  theme_bw()+
  scale_fill_manual(values = pal_yrtype, labels = c("Critical", "Dry", "Below N.", "Above N.", "Wet"))

#############################################

#now look at drought-neutral wet

names(DFRTall)

ggplot(DFRTall, aes(x = Drought, y = SJRT, fill = Drought)) + geom_boxplot()+
  facet_wrap(~Month) +
  xlab("Water Year Type") + ylab("San Joaquin Residence Time (Days)")+ 
  theme_bw()

#Annual averages

DFRTann = group_by(DFRTall, WY, Yr_type, Drought) %>%
  summarize(SACRT = mean(SACRT), SJRT = mean(SJRT))


ggplot(DFRTann, aes(x = Drought, y = SJRT, fill = Drought)) + geom_boxplot()+
  xlab("Water Year Type") + ylab("San Joaquin Residence Time (Days)")+ 
  theme_bw()


ggplot(DFRTann, aes(x = Drought, y = SACRT, fill = Drought)) + geom_boxplot()+
  xlab("Water Year Type") + ylab("Sacramento Residence Time (Days)")+ 
  theme_bw()
save(DFRTall, DFRTann, file = "ResidenceTime.RData")


