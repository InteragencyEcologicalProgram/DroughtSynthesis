library(dataRetrieval)
library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(patchwork)
library(knitr)
library(car)
library(multcomp)
library(emmeans)
library(stringr)
library(ggpubr)

save_dir<-file.path("C:/Users/estumpne/Documents/R/DroughtData-master_0_4/DroughtData-master/output")

#load lt_seasonal.rda and raw_hydro_1975_2021.rda

#pull USGS Q - daily values at SJJ, RVB, TMS, DCH, CCH, CCH41

#RIO

siteNumbers <- c("11455420")

startDate <- "2010-10-01"

endDate <- "2021-09-30"

parameterCd <- c("00060", "00065", "72137", "00095")

dvRIO <- readNWISdv(siteNumbers, parameterCd, startDate, endDate)

dvRIO <- rename(dvRIO, "Q_RIO" = "X_72137_00003")

dvRIO <- subset(dvRIO, select = c(Date, Q_RIO))

#SJJ

siteNumbers <- c("11337190")

startDate <- "2010-10-01"

endDate <- "2021-09-30"

parameterCd <- c("00060", "00065", "72137", "00095")

dvSJJ <- readNWISdv(siteNumbers, parameterCd, startDate, endDate)

dvSJJ <- rename(dvSJJ, "Q_SJJ" = "X_72137_00003")

dvSJJ <- subset(dvSJJ, select = c(Date, Q_SJJ))

#TMS

siteNumbers <- c("11337080")

startDate <- "2010-10-01"

endDate <- "2021-09-30"

parameterCd <- c("00060", "00065", "72137", "00095")

dvTMS <- readNWISdv(siteNumbers, parameterCd, startDate, endDate)

#DCH

siteNumbers <- c("11313433")

startDate <- "2010-10-01"

endDate <- "2021-09-30"

parameterCd <- c("00060", "00065", "72137", "00095")

dvTMS <- readNWISdv(siteNumbers, parameterCd, startDate, endDate)

#CCH (aka RYI) - old Cache station

siteNumbers <- c("11455350")

startDate <- "2010-10-01"

endDate <- ""

parameterCd <- c("00060", "00065", "72137", "00095")

dvCCH <- readNWISdv(siteNumbers, parameterCd, startDate, endDate)

#CCH41 (aka RYF) - new Cache station

siteNumbers <- c("11455385")

startDate <- "2019-04-27"

endDate <- "2021-09-30"

parameterCd <- c("00060", "00065", "72137", "00095")

dvCCH41 <- readNWISdv(siteNumbers, parameterCd, startDate, endDate)


#stack Cache stations

dvCCH_41 <- rbind(cbind(dvCCH), cbind(dvCCH41))

CCH_plot <- ggplot()+ geom_line(data = dvCCH_41, aes(x=Date, y=X_72137_00003), color = "red")

CCH_plot

dvCCH_41 <- rename(dvCCH_41, "Q_CCH" = "X_72137_00003")

dvCCH_41 <- subset(dvCCH_41, select = c(Date, Q_CCH))

#bring in Dayflow Outflow

DTO <- filter(raw_hydro_1975_2021, Date >= "2010-12-01")

DTO_plot <- ggplot()+ geom_line(data = DTO, aes(x=Date, y=Outflow), color = "red")

DTO_plot

#TODO combine the RIO, SJJ, DCH, TMS records to produce combo USGS flow, for now using pull from Aquarius (internal NWIS)

#add USGS combined Q (from Aquarius because not 'approved' and available thru dataRetrieval) to DTO data frame

#DTO$Date_p <- as.POSIXct(DTO$Date, format = "%Y-%m-%d", tz="America/Pacific")

#bring in USGS Q and compare to Dayflow outflow

combo <- read.csv("combo.csv", skip=14, stringsAsFactors = FALSE)

#combo$Date <- as.POSIXct(combo$Timestamp..UTC.08.00., format = "%Y-%m-%d")

combo$Date <- as.Date(combo$Timestamp..UTC.08.00., format = "%Y-%m-%d")

combo_st <- filter(combo, Date >= "2010-12-01")

combo_st1 <- rename(combo_st, "Q_USGS_tf" = "Value")

combo_st2 <- subset(combo_st1, select = c(Date, Q_USGS_tf))

DTO_comp <- left_join(DTO, combo_st2, by="Date")

#add Drought column to DTO_comp

drought <- subset(lt_seasonal, select = c("YearAdj", "Season", "Drought", "YearType"))

DTO_comp <- left_join(DTO_comp, drought, by=c("YearAdj", "Season"))

#add RIO, SJJ, and CCH to DTO compare df

DTO_comp <- left_join(DTO_comp, dvRIO, by="Date")

DTO_comp <- left_join(DTO_comp, dvSJJ, by="Date")

DTO_comp <- left_join(DTO_comp, dvCCH_41, by="Date")

# plot high vs low dayflow outflow + USGS Q relationship

# all flow

DF_lm <- lm(Outflow~Q_USGS_tf, DTO_comp)

DF_compare <- ggplot(data = DTO_comp, aes(x=Outflow, y=Q_USGS_tf)) + geom_smooth(method = "lm")+ geom_point() + stat_regline_equation(label.x=0, label.y=300000) + stat_cor(aes(label=..rr.label..), label.x=100, label.y=250000)+ ylab("Dayflow Outflow") +xlab("USGS Outflow")+ geom_rect(aes(xmin = 0 - 0.5, xmax = 10000 + 0.5, ymin = -10000 - 0.5, ymax = 22000 + 0.5),fill = "transparent", color = "purple", size = 1.5)+theme_bw()

DF_USGS_comp<- DF_compare+annotate("text", x = 0, y = 350000, size = 10, label = "A")

DF_USGS_comp

ggsave(plot=DF_USGS_comp, filename=file.path(save_dir, "Fig_38A.png"), device="png", height=12, width=15, units="in")

# flow < 10K cfs

DF_low <- filter(DTO_comp, Outflow < 10000)

DF_lm_low <- lm(Outflow~Q_USGS_tf, DF_low)

DF_low_compare <- ggplot(data = DF_low, aes(x=Outflow, y=Q_USGS_tf)) + geom_smooth(method = "lm")+ geom_point() + stat_regline_equation(label.x=100, label.y=-5000) + stat_cor(aes(label=..rr.label..), label.x=100, label.y=-8000)+ ylab("Dayflow Outflow") +xlab("USGS Outflow")+ geom_rect(aes(xmin = 0 - 0.5, xmax = 10000 + 0.5, ymin = -10000 - 0.5, ymax = 22000 + 0.5),fill = "transparent", color = "purple", size = 1.5)+theme_bw()


DF_USGS_comp_low <- DF_low_compare+annotate("text", x = 500, y = 20000, size = 10, label = "B")

DF_USGS_comp_low

ggsave(plot=DF_USGS_comp_low, filename=file.path(save_dir, "Fig_38B.png"), device="png", height=12, width=15, units="in")

#percent Delta Outflow values <10K cfs

quantile(DTO_comp$Outflow, probs = 0.5902)

#regression at low flows

#Outflow <50K R2=0.79, <25K R2 = 0.46, <15K R2 =0.25, <10K R2 = 0.07

# Calculate RMSE

sqrt(mean(DTO_lm_low$residuals^2))

DTO_lm <- lm(Outflow~Q_USGS_tf, DTO_comp)

# Calculate RMSE

sqrt(mean(DTO_lm$residuals^2))

summary(DTO_lm_low)
summary(DTO_lm)

#take seasonal mean of dayflow and USGS stations - necessary for Cache Slough comparison

DTO_comp_seasonal <- aggregate(Q_USGS_tf ~ Season + YearAdj + Drought + YearType, data = DTO_comp, mean)

DTO_comp_seasonal_RIO <- aggregate(Q_RIO ~ Season + YearAdj + Drought + YearType, data = DTO_comp, mean)

DTO_comp_seasonal_CCH <- aggregate(Q_CCH ~ Season + YearAdj + Drought + YearType, data = DTO_comp, mean)

DTO_comp_seasonal_SJJ <- aggregate(Q_SJJ ~ Season + YearAdj + Drought + YearType, data = DTO_comp, mean)

DTO_comp_seasonal_out <- aggregate(Outflow ~ Season + YearAdj + Drought + YearType, data = DTO_comp, mean)

DTO_comp_Q <- left_join(DTO_comp_seasonal, DTO_comp_seasonal_RIO, by=c("YearAdj", "Season"))

DTO_comp_Q <- left_join(DTO_comp_Q, DTO_comp_seasonal_out, by=c("YearAdj", "Season"))

DTO_comp_Q <- left_join(DTO_comp_Q, DTO_comp_seasonal_CCH, by=c("YearAdj", "Season"))

DTO_comp_Q <- left_join(DTO_comp_Q, DTO_comp_seasonal_SJJ, by=c("YearAdj", "Season"))

head(DTO_comp_Q)

DTO_comp_Q <- subset(DTO_comp_Q, select=c(Season, YearAdj, Q_USGS_tf, Drought, Q_CCH, Q_RIO, Q_SJJ, YearType, Outflow))

head(DTO_comp_Q)

DTO_comp_seasonal <- DTO_comp_Q




