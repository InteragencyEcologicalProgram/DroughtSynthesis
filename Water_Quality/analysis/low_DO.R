library(dataRetrieval)
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

save_dir<-file.path("C:/Users/estumpne/Documents/R/DroughtData-master_0_4/DroughtData-master/output")

#data retrieval pull of Decker station through NWIS we services

siteNumbers <- c("11455478", "11455485")

startDate <- "2014-10-01"

endDate <- "2021-12-01"

parameterCd <- c("00300")

#00010 Temp
#00060 Discharge
#00095 SpC
#00400 pH
#00480 Sal_ppt
#00300 DO_mgL
#00301 DO_perc
#32316 fCHL
#32295 fDOM
#63680 Turb
#72137 Discharge_tf
#72254 velocity
#90860 Sal_psu
#99133 NO3

#pull data - timestamp defaults to UTC in but data is collected in PST

uvDEC_TOL <- readNWISuv(siteNumbers,parameterCd,startDate,endDate)

DEC_TOL_DO <- select(uvDEC_TOL, "dateTime", "X_00300_00000") 

DEC_TOL_DO <- filter(DEC_TOL_DO, dateTime >="2014-10-01")

DEC_TOL_DO <- filter(DEC_TOL_DO, dateTime <="2021-12-01")

DEC_TOL_DO <- rename(DEC_TOL_DO, "DO_mgL" = "X_00300_00000")


#apply tidal filter - run Tgodin filter function

#add new tidally filtered DO column

#DEC_TOL_DO$DO_mgL_tf <- Tgodinfn(DEC_TOL_DO$dateTime, DEC_TOL_DO$DO_mgL)

#compare instantaneous vs tf

# <- ggplot()+ geom_line(data = DEC_TOL_DO, aes(x=dateTime, y=DO_mgL), color = "red") + geom_line(data = DEC_TOL_DO, aes(x=dateTime, y=DO_mgL_tf), color = "blue")

#DEC_DO_tf_plot

#read in Aquarius DO

#DO_pre <- read.csv("C:/Users/estumpne/Documents/R/Conf_bloom/DEC_TOL_121721_v2.csv") 

#DO <- subset(DEC_TOL_alldata2, select = c("Date_PST", "DO_mgL"))

#str(DO$Date_PST)

#DO$timestamp <- as.POSIXct(DO$Date_PST, format = "%Y-%m-%d %H:%M:%S")

#downstep to daily mean

dailyDO_DEC <- DEC_TOL_DO %>%mutate(date = as.Date(dateTime)) %>%group_by(date) %>%summarize(mean_DO = mean(DO_mgL, na.rm=TRUE), mean_DO = mean(DO_mgL, na.rm = TRUE))

#plot daily mean

DO_plot_DEC_day <- ggplot()+ geom_line(data = dailyDO_DEC, aes(x=date, y=mean_DO), color = "red")+ geom_line(data = dailyDO_DEC , aes(x=date, y=mean_DO), color = "blue")

DO_plot_DEC_day

#downstep to monthly mean

#monthlyDO_DEC <- dailyDO_DEC %>%
#mutate(date = month(Date_PST)) %>% group_by(month=floor_date(date, "month")) %>% summarise(month = mean(month), mean_DO = mean(mean_DO, na.rm = TRUE))

#plot monthly mean

#DO_plot_DEC_month <- ggplot()+ geom_line(data = monthlyDO_DEC, aes(x=month, y=mean_DO), color = "red")#+ geom_line(data = CFL_test, aes(x=Date_PST, y=NO3.y), color = "blue")

#DO_plot_DEC_month

#add wateryear to daily and monthly data frames

wtr_yr <- function(dateTime, start_month=10) {
  # Convert dates into POSIXlt
  dates.posix = as.POSIXlt(dateTime)
  # Year offset
  offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
  # Water year
  adj.year = dates.posix$year + 1900 + offset
  # Return the water year
  adj.year
}

DEC_DO_day_WY <- dailyDO_DEC %>% 
  mutate(wtr_yr = wtr_yr(date))

#DEC_DO_month_WY <- monthlyDO_DEC %>% mutate(wtr_yr = wtr_yr(month))

DEC_DO_day_WY$day <- day(DEC_DO_day_WY$date)

DEC_final <- DEC_DO_day_WY %>%
  group_by(wtr_yr) %>% 
  mutate(wtr_day = (as.integer(difftime(date,ymd(paste0(wtr_yr - 1 ,'-09-30')), units = "days"))))

DEC_final$station <- c("DEC")

DEC_final <- mutate(DEC_final, wtr_yr=as.character(wtr_yr))

DEC_WY_plot <- ggplot() + geom_line(data = DEC_final, aes(x = wtr_day, y= mean_DO, color = wtr_yr), size= 1, alpha = 0.6) + labs(col = "Water Year", y = "mean DO (mg/)", x = "WY Day") + scale_color_brewer(palette="BuGn", na.translate = F) + theme_bw() + theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold")) +theme(legend.text = element_text(size = 20)) + theme(legend.title = element_text(size = 20)) 

DEC_WY_plot

ggsave(plot=DEC_WY_plot, filename=file.path(save_dir, "DEC_WY_plot.png"), device="png", height=12, width=15, units="in")

#add TOE N (11455139) and TOE S (11455139) DO 

#data retrieval pull through NWIS web services - all data not approved, skip to AQ 

siteNumbers <- c("11455139", "11455140")

startDate <- ""

endDate <- ""

parameterCd <- c("00300")

uvTOE <- readNWISuv(siteNumbers,parameterCd,startDate,endDate)

TOE_DO <- read.csv("C:/Users/estumpne/Documents/R/Conf_bloom/Dissolved_oxygen.mg_l.BGC_PROJECT@11455140.EntireRecord.csv", skip=14, stringsAsFactors = FALSE) 

TOES_DO <- read.csv("C:/Users/estumpne/Documents/R/Conf_bloom/Dissolved_oxygen.mg_l.BGC_PROJECT@11455139.EntireRecord.csv", skip=14, stringsAsFactors = FALSE) 

TOE_DO <- rbind(TOE_DO, TOES_DO)

TOE_DO <- rename(TOE_DO, dateTime = Timestamp..UTC.08.00., DO_mgL = Value )

TOE_DO$dateTime <- as.POSIXct(TOE_DO$dateTime, format = "%Y-%m-%d %H:%M:%S")

TOE_DO <- subset(TOE_DO, select = c(dateTime, DO_mgL)) 

TOE_DO_plot <- ggplot()+ geom_line(data = TOE_DO, aes(x=dateTime, y=DO_mgL), color = "red")

TOE_DO_plot

#apply tidal filter - run Tgodin filter function

#add new tidally filtered DO column

#TOE_DO$DO_mgL_tf <- Tgodinfn(TOE_DO$dateTime, TOE_DO$DO_mgL)

#compare instantaneous vs tf

#TOE_DO_tf_plot <- ggplot()+ geom_line(data = TOE_DO, aes(x=dateTime, y=DO_mgL), color = "red") + geom_line(data = TOE_DO, aes(x=dateTime, y=DO_mgL_tf), color = "blue")

#TOE_DO_tf_plot

#downstep to daily mean

dailyDO_TOE <- TOE_DO %>%mutate(date = as.Date(dateTime)) %>%group_by(date) %>%summarize(mean_DO = mean(DO_mgL, na.rm=TRUE))

#plot daily mean

DO_plot_TOE_day <- ggplot()+ geom_line(data = dailyDO_TOE, aes(x=date, y=mean_DO), color = "red")

DO_plot_TOE_day

#downstep to monthly mean

#monthlyDO_TOE <- dailyDO_TOE %>%
  #mutate(date = month(Date_PST)) %>%group_by(month=floor_date(date, "month")) %>%summarise(month = mean(month), mean_DO = mean(mean_DO, na.rm = TRUE))

#plot monthly mean

#DO_plot_TOE_month <- ggplot()+ geom_line(data = monthlyDO_TOE, aes(x=month, y=mean_DO), color = "red")+ geom_line(data = dailyDO_TOE, aes(x=date, y=mean_DO), color = "blue")

#DO_plot_TOE_month

#add wateryear to daily and monthly data frames

TOE_DO_day_WY <- dailyDO_TOE %>% mutate(wtr_yr = wtr_yr(date))

#TOE_DO_month_WY <- monthlyDO_TOE %>% mutate(wtr_yr = wtr_yr(month))

TOE_DO_day_WY$day <- day(TOE_DO_day_WY$date)

TOE_final <- TOE_DO_day_WY %>% group_by(wtr_yr) %>% mutate(wtr_day = (as.integer(difftime(date,ymd(paste0(wtr_yr - 1 ,'-09-30')), units = "days"))))

TOE_final$station <- c("TOE")

#all water years

TOE_final <- mutate(TOE_final, wtr_yr=as.character(wtr_yr))

#bind rows to insert NA in TOE_final data frame for plotting - otherwise line is generated that represents missing data  for 2017-12-22 and 2019-02-18 and 2019-08-13

TOE_final_bind1 <- data.frame(date = as.Date(17157, origin = "1970-01-01"), mean_DO = NA, wtr_yr = as.character(2017), day = as.integer(22), wtr_day = as.integer(83), station = "TOE") 

TOE_final_bind2 <- data.frame(date = as.Date(17945, origin = "1970-01-01"), mean_DO = NA, wtr_yr = as.character(2019), day = as.integer(18), wtr_day = as.integer(143), station = "TOE")

TOE_final_bind3 <- data.frame(date = as.Date(18121, origin = "1970-01-01"), mean_DO = NA, wtr_yr = as.character(2019), day = as.integer(13), wtr_day = as.integer(317), station = "TOE")

TOE_final_bind <- rbind(TOE_final_bind1, TOE_final_bind2, TOE_final_bind3)

TOE_final<- bind_rows(list(TOE_final, TOE_final_bind))

DO_plot_TOE_day <- ggplot()+ geom_line(data = TOE_final, aes(x=date, y=mean_DO), color = "red")

DO_plot_TOE_day

TOE_WY_plot <- ggplot() + geom_line(data=TOE_final, aes(x=wtr_day, y=mean_DO, color=wtr_yr), size=1, alpha=0.6) + labs(col = "Water Year", y="mean DO (mg/L)", x="WY Day", fill = "Water Year")+scale_color_brewer(palette="YlOrRd", na.translate = F ) + theme_bw() + theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold")) +theme(legend.text = element_text(size = 20)) + theme(legend.title = element_text(size = 20)) 

TOE_WY_plot

ggsave(plot=TOE_WY_plot, filename=file.path(save_dir, "TOE_WY_plot.png"), device="png", height=12, width=15, units="in")

#add TOE and DEC to same data frame

TOE_DEC_final <- rbind(TOE_final, DEC_final)

#TOE_DEC_final <- rbind(cbind(TOE_final), cbind(DEC_final)) 

#plot time series of two stations

#TOE_DEC_final <- -c(TOE_DEC_final$mean_DO_tf)

TOE_DEC_DO_ts <- ggplot() + geom_line(data=TOE_DEC_final, aes(x=date, y=mean_DO, color=station), size=1, alpha=0.6) + labs(y="mean DO (mg/L)", x="date") +scale_color_brewer(palette="Dark2") + theme_bw() + theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold")) +theme(legend.text = element_text(size = 20)) + theme(legend.title = element_text(size = 20))

TOE_DEC_DO_ts

ggsave(plot=TOE_DEC_DO_ts, filename=file.path(save_dir, "TOE_DEC_DO_ts.png"), device="png", height=12, width=15, units="in")


TOE_DEC_DO_box <- ggplot() + geom_boxplot(data=TOE_DEC_final, aes(x=station, y=mean_DO, color=station))+scale_color_brewer(palette="Dark2")+theme_bw()+ theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold")) +theme(legend.text = element_text(size = 20)) + theme(legend.title = element_text(size = 20)) %>% summarise(TOE_DEC_final, )

TOE_DEC_DO_box

hydro_st <- filter(raw_hydro_1975_2021, Date >= "2010-12-01")

drought_st<- subset(lt_seasonal, select = c("YearAdj", "Season", "Drought", "YearType")) %>% filter(lt_seasonal, YearAdj >= 2011)

str(lt_seasonal)

for_DO_drought <- left_join(drought, for_DO, by= c("YearAdj", "Season"))

DEC_TOE_season <- left_join(for_DO, TOE_DEC_final, by = c("YearAdj", "Season"))



DO_seasonal <- left_join(DTO_comp, drought, by=c("YearAdj", "Season"))

DTO_comp <- left_join(DTO_comp, drought, by=c("YearAdj", "Season"))

#create water day column notes

library(lubridate)

datetime <- seq(as.Date("1950/1/1"), as.Date("2099/12/31"), by = "day")
year <- year(datetime)
month <- month(datetime)
day <- day(datetime)
julian_day <- yday(datetime)

datetime_dataframe <- data.frame(datetime, year, month, day, julian_day)


wtr_yr <- function(dates, start_month=10) {
  # Convert dates into POSIXlt
  dates.posix = as.POSIXlt(dates)
  # Year offset
  offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
  # Water year
  adj.year = dates.posix$year + 1900 + offset
  # Return the water year
  adj.year
}

datetime_dataframe$wtr_yr <-wtr_yr(datetime_dataframe$datetime)

new_df <- datetime_dataframe %>%
  group_by(wtr_yr) %>% 
  mutate(wtr_day = (as.integer(difftime(datetime,ymd(paste0(wtr_yr - 1 ,'-09-30')), units = "days"))))





