########Striped Bass Index Calculations

##############all years


#setwd("C:/Users/tmalinich/Malinich_Data/STN/Indices/2021")
library(plyr)
library(ggplot2)
library(psych)
library(dplyr)
library(lubridate)
library(ggthemes)
library(extrafontdb)
library(splitstackshape)#expanding rows for calculating mean FL

library(scales)
library(svglite)
library(cowplot)
library(ggfortify)
library(readxl)

########################upload all files

source("RosiesDraftAnalyses/Indice_Sourcecode.R")

toweffort<-read_excel("data/TowEffort.xlsx", guess_max = 100000)#xlsx file of tow effort
fishsample<-read_excel("data/Sample.xlsx", guess_max = 100000)#csv file of Sample
fishlength<-read_excel("data/Length_STN.xlsx", guess_max = 100000)#csv file of length and length frequency
fishcatch<-read_excel("data/Catch_STN.xlsx")#csv file of catch
station<-read_excel("data/luStation.xlsx")#csv file of station information
#sb0STN= read.csv("C:/Users/tmalinich/Malinich_Data/STN/Indices/2021/sb0_STN_2021.csv", header=T)

############################join tables

towsample<-left_join(toweffort,fishsample,by="SampleRowID")
towsamplecatch<-left_join(towsample,fishcatch,by="TowRowID")
#towsamplecatchlength<-left_join(towsamplecatch,fishlength,by="CatchRowID")
FishLength<-fishlength%>%
  select(LengthRowID,CatchRowID,ForkLength,LengthFrequency)
#fishlength_nonqc<-fishlength_nonqc%>%
#  select(LengthRowID,CatchRowID,ForkLength,LengthFrequency)
#if(
#  max(fishlength_nonqc$CatchRowID)>max(FishLength$CatchRowID)){
#       fishlength=rbind(FishLength,fishlength_nonqc)}
       

#####################select only bass
SB0<-towsamplecatch[towsamplecatch$OrganismCode==0,]
#View(SB0)

#########################mean forklength by survey and survey index


y=1960#year to examine

MLS<-MeanLengthSurvey(SB0,fishlength,y)

########################Calculate mid-survey dates

MD<-middate(fishsample,y)


#################calculate growthrate and the date the index is set

grwthrate(MLS,MD)#Use functions above for middate and mean length at each survey


MLS################calculate the annual index if possible

annualSBindex(MLS)

#####
annualindex_adj=c()


yrs<-(1959:1963)
yrs<-c(yrs,1965)
yrs<-c(yrs,1967:1994)
yrs<-c(yrs,2000:2021)

for(i in yrs){
  year=i
  MLS=MeanLengthSurvey(SB0,fishlength,year)
  MD<-middate(fishsample,year)
  grwthrate(MLS,MD)
  
  annualindex_adj=c(annualindex_adj,round(annualSBindex(MLS),1))
  
  print(i)
  
}

adj_indexSB<-cbind(yrs,annualindex_adj)


######################################################################
#Try it for American Shad
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

#######################################################graphing for index
sb0STN<-left_join(sb0STN,adj_index,by=c("Year"="yrs"),copy=TRUE)

#same for STN SB0
#create bar plot
sb0STNplot= ggplot(sb0STN, aes(x=Year, y=Index)) + theme_few() +
  geom_bar(stat="identity", width = 0.8, color="Black", fill="Grey") + 
  geom_line(aes(x=Year,y=annualindex_adj),color="BLUE")+
  geom_point(aes(x=Year,y=annualindex_adj),color="BLUE")+
  annotate("text",label="No Index",x=c(1966,1983,1995,2002),y=13,size=2.5,colour="red",angle=90)+
  theme(legend.position="none") +
  scale_x_continuous(breaks=c(seq(1959,2018,4),2021)) +
  scale_y_continuous(limits=c(0,120),breaks=seq(0,120,20),expand=c(0,0))

#modify theme components
sb0STNplot <-
  sb0STNplot + theme(
    panel.border = element_rect(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.title.x = element_text(size = 12, color = "Black"),
    axis.title.y = element_text(size = 12, color = "Black"),
    axis.text.x = element_text(size = 10, color = "Black",angle=45,vjust=.7),
    axis.text.y = element_text(size = 10, color = "Black")
  )
sb0STNplot
#output, save as png
#ggsave("sb0STNplot_2021_ad.png", plot= sb0STNplot, scale = 1, width = 6.5, height = 3, units = "in", device = "png", path = "C:/Users/tmalinich/Malinich_Data/STN/Indices/2021", dpi = 300)
ggsave("sb0STNplot_2021_ad.png", plot= sb0STNplot, scale = 1, width = 6.5, height = 8, units = "in", device = "png", path = "C:/Users/tmalinich/Malinich_Data/STN/Indices/2021", dpi = 300)

#subset of data, last 10 years
subsetsb0STN= subset(sb0STN, Year >=2011)

#smaller subplot of last 5 years
sb0STNplotlast5= ggplot(subsetsb0STN, aes(x=Year, y=Index, label=Index)) + theme_few() +
  geom_bar(stat="identity", width = 0.6, color="Black", fill="Black") + 
  theme(legend.position="none") +
  scale_x_continuous(breaks=seq(2010,2021,1)) +
  scale_y_continuous(limits=c(0,4.5), breaks=seq(0,4.5,1),expand=c(0,0)) +
  geom_text(size=3.5, vjust=-0.5)
sb0STNplotlast5<-sb0STNplotlast5 + theme(panel.border=element_rect(color="black"), axis.ticks=element_line(color="black"), axis.title.x=element_text(size=12, color="Black"), axis.title.y=element_text(size=12, color="Black"), axis.text.x=element_text(size=10, color = "Black"), axis.text.y=element_text(size=10, color="Black"))
sb0STNplotlast5
#output, save as png
ggsave("sb0STNplotinlay_2021.png", plot= sb0STNplotlast5, scale = 1, width = 5, height = 2, units = "in", device = "png", path = "C:/Users/tmalinich/Malinich_Data/STN/Indices/2021", dpi = 300)

####################################SB0 size histograms
####################################
####################################
####################################
####################################

join1<-left_join(fishlength,fishcatch,by="CatchRowID")

join1v1<-join1%>%
  select(CatchRowID,TowRowID,ForkLength,LengthFrequency,OrganismCode,Catch)

join2<-left_join(join1v1,toweffort, by="TowRowID")

join2v1<-join2%>%
  select(CatchRowID,TowRowID,SampleRowID,ForkLength,LengthFrequency,OrganismCode,TowNumber,Catch)

join3<-left_join(join2v1,fishsample,by="SampleRowID")

join3v1<-join3%>%
  select(OrganismCode,SampleDate,Survey,TowNumber,LengthFrequency,ForkLength,Catch,StationCode)

join3v2<-left_join(join3v1,station,by=c("StationCode"="StationCodeSTN"))
join3v2<-join3v2%>%
  select(OrganismCode,SampleDate,Survey,TowNumber,LengthFrequency,ForkLength,Catch,StationCode,Index)

SBlengths<-join3v2[join3v2$OrganismCode==0,]
SBlengths$SampleDate<-mdy_hms(SBlengths$SampleDate)
SBlengths<-SBlengths%>%
  mutate(year=year(SampleDate))
SBlengths2021<-SBlengths[SBlengths$year==2021,]
SBlengths2021<-SBlengths2021%>%
  na.omit(SBlengths2021)

SBlengths2021<-expandRows(SBlengths2021,"LengthFrequency")#drops rows where length is recorded as zero, use adjusted length frequency in the future


lengthframe=data.frame(Survey=SBlengths2021$Survey,FL=SBlengths2021$ForkLength,Index=SBlengths2021$Index)


#describeBy(SBlengths[SBlengths$Year==2019,9],SBlengths[SBlengths$Year==2019,2])



survey1 = ggplot(lengthframe, aes(x = FL,fill=as.factor(Index))) +
  geom_histogram(
    data = subset(lengthframe, Survey == 1),
    # = as.factor(lengthframe$Index),
    bins = 80
  ) + geom_vline(
    data = lengthframe,
    aes(xintercept = 38.1),
    linetype = "dotted",
    size = 1,
    colour = "red"
  ) + geom_vline(
    data = lengthframe,
    aes(xintercept = 13.5),
    linetype = "dashed",
    size = 1,
    colour = "black"
  ) +
  scale_x_continuous(name = "Fork Length (mm)",
                     limits = c(5, 70),
                     expand = c(0, 3)) + scale_y_continuous(name = "Age-0 Striped Bass Count",
                                                            limits = c(0, 20),
                                                            expand = c(0, 0)) +
  scale_fill_manual(name="Station Type" ,values=c("grey32","grey60"),label=c("Non-Index","Index"))+
  theme_bw() + ggtitle("Survey 1") + 
  theme(
    panel.border = element_rect(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.title.x = element_text(size = 12, color = "Black"),
    axis.title.y = element_text(size = 12, color = "Black"),
    axis.text.x = element_text(size = 10, color = "Black"),
    axis.text.y = element_text(size = 10, color = "Black")
  )#+
survey1


survey2 = ggplot(lengthframe, aes(x = FL, fill = as.factor(Index))) +
  geom_histogram(
    data = subset(lengthframe, Survey == 2),
   # fill = "grey",
    bins = 80
  ) + geom_vline(
    data = lengthframe,
    aes(xintercept = 38.1),
    linetype = "dotted",
    size = 1,
    colour = "red"
  ) + geom_vline(
    data = lengthframe,
    aes(xintercept = 31.6),
    linetype = "dashed",
    size = 1,
    colour = "black"
  ) +
  scale_x_continuous(name = "Fork Length (mm)",
                     limits = c(5, 70),
                     expand = c(0, 3)) + scale_y_continuous(name = "Age-0 Striped Bass Count",
                                                            limits = c(0, 20),
                                                            expand = c(0, 0)) +
  scale_fill_manual(name="Station Type" ,values=c("grey32","grey60"),label=c("Non-Index","Index"))+
  theme_bw() + ggtitle("Survey 2") + 
  theme(
    panel.border = element_rect(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.title.x = element_text(size = 12, color = "Black"),
    axis.title.y = element_text(size = 12, color = "Black"),
    axis.text.x = element_text(size = 10, color = "Black"),
    axis.text.y = element_text(size = 10, color = "Black")
  )
  #+
survey2



survey3 = ggplot(lengthframe, aes(x = FL,fill = as.factor(Index))) +
  geom_histogram(
    data = subset(lengthframe, Survey == 3),
   # fill = "grey",
    bins = 40
  ) + geom_vline(
    data = lengthframe,
    aes(xintercept = 38.1),
    linetype = "dotted",
    size = 1,
    colour = "red"
  ) + geom_vline(
    data = lengthframe,
    aes(xintercept = 39.3),
    linetype = "dashed",
    size = 1,
    colour = "black"
  ) +
  scale_x_continuous(name = "Fork Length (mm)",
                     limits = c(5, 70),
                     expand = c(0, 3)) + scale_y_continuous(name = "Age-0 Striped Bass Count",
                                                            limits = c(0, 20),
                                                            expand = c(0, 0)) +
  scale_fill_manual(name="Station Type" ,values=c("grey32","grey60"),label=c("Non-Index","Index"))+
  theme_bw() + ggtitle("Survey 3") + 
  theme(
    panel.border = element_rect(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.title.x = element_text(size = 12, color = "Black"),
    axis.title.y = element_text(size = 12, color = "Black"),
    axis.text.x = element_text(size = 10, color = "Black"),
    axis.text.y = element_text(size = 10, color = "Black")
  )

  #+
survey3
#survey4=ggplot(lengthframe,aes(x=FL))+
#  geom_histogram(data=subset(lengthframe,Survey==4),fill="grey",bins=40)+geom_vline(data=lengthframe,aes(xintercept=38.1),linetype="dashed",size=1,colour="red")+geom_vline(data=lengthframe,aes(xintercept=23.8),linetype="dashed",size=1,colour="black")+
#  scale_x_continuous(name="Fork Length (mm)",limits=c(5,80),expand = c(0, 3))+scale_y_continuous(name="Age-0 Striped Bass Count",limits=c(0,40),expand = c(0, 0))+
#  theme_bw()+ggtitle("Survey 4")+theme(title =element_text(size=25, face='bold'),axis.title.x.bottom=element_text(size=25),axis.title.y=element_text(size=25),axis.text.x=element_text(size=22),axis.text.y=element_text(size=22))#+
#survey4
#survey5=ggplot(lengthframe,aes(x=FL))+
#  geom_histogram(data=subset(lengthframe,Survey==5),fill="grey",bins=40)+geom_vline(data=lengthframe,aes(xintercept=38.1),linetype="dashed",size=1,colour="red")+geom_vline(data=lengthframe,aes(xintercept=55),linetype="dashed",size=1,colour="black")+
#  scale_x_continuous(name="Fork Length (mm)",limits=c(5,80),expand = c(0, 3))+scale_y_continuous(name="Age-0 Striped Bass Count",limits=c(0,40),expand = c(0, 0))+
#  theme_bw()+ggtitle("Survey 5")+theme(title =element_text(size=25, face='bold'),axis.title.x.bottom=element_text(size=25),axis.title.y=element_text(size=25),axis.text.x=element_text(size=22),axis.text.y=element_text(size=22))#+
#survey5

ggsave("SBsurvey1.png", plot= survey1, scale = 1, width = 5, height = 3, units = "in", device = "png", path = "C:/Users/tmalinich/Malinich_Data/STN/Indices/2021", dpi = 300)

ggsave("SBsurvey2.png", plot= survey2, scale = 1, width = 5, height = 3, units = "in", device = "png", path = "C:/Users/tmalinich/Malinich_Data/STN/Indices/2021", dpi = 300)

ggsave("SBsurvey3.png", plot= survey3, scale = 1, width = 5, height = 3, units = "in", device = "png", path = "C:/Users/tmalinich/Malinich_Data/STN/Indices/2021", dpi = 300)





h=ggplot(lengthframe,aes(FL,colour=as.factor(Survey)))+geom_density(size=1.5)+theme_bw()+scale_colour_discrete(name="Surveys")+ 
  theme(legend.justification=c(1.3,1.1), legend.position=c(1,1))
h






