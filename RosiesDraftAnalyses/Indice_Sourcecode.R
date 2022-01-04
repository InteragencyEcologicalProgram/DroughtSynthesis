###########Striped Bass Index 
#source code for functions


#Survey Mean Length
MeanLengthSurvey<-function(SB0Data,FishLength,yearexamine){
  
  #bring in station information
  station<-read_excel("data/luStation.xlsx")#csv file of station information
  station<-mutate(station,StationCode=StationCodeSTN)
  
  
  lengthtable<-left_join(SB0Data,FishLength,by="CatchRowID")
  lengthtable<-left_join(lengthtable,station,by="StationCode")
  
  library(lubridate)#package for working with dates/times
  #lengthtable$SampleDate<-mdy_hms(lengthtable$SampleDate)
  lengthtable<-mutate(lengthtable,year=year(lengthtable$SampleDate))
  
  #################simplify table
  lengthtable2<-lengthtable%>%
    select(SampleDate,StationCode,Survey,Index,OrganismCode,Catch,year,ForkLength,LengthFrequency,WeightingFactor)
  
  lengthtable3<-lengthtable2[lengthtable2$year==yearexamine,]
  lengthtable3<-na.omit(lengthtable3)

  lengthtable4<-lengthtable3%>%
    mutate(Frequency=ForkLength*LengthFrequency)
  lengthtable5<-lengthtable4[lengthtable4$Index==1,]
  
  lengthtable5<-lengthtable5%>%
    group_by(Survey,StationCode)%>%
    mutate(total_Sbcatch=sum(LengthFrequency))
  
  lengthtable5<-lengthtable5[lengthtable5$ForkLength>0,]
  
  lengthtable5<-lengthtable5%>%
    group_by(Survey,StationCode)%>%
    mutate(propCatch=LengthFrequency/sum(LengthFrequency))
  
  lengthtable5<-lengthtable5%>%
    group_by(Survey,StationCode)%>%
    mutate(adj_LF=total_Sbcatch*propCatch)
  
  lengthtable5<-lengthtable5%>%
    mutate(Frequency=ForkLength*adj_LF)
  
  bySurvey<-lengthtable5%>%
    group_by(Survey)%>%
    summarize(SumLF=sum(Frequency,na.rm=TRUE),SumF=sum(adj_LF,na.rm=TRUE))
  bySurvey<-bySurvey%>%
    mutate(meanFL=SumLF/SumF)
  
  SI<-lengthtable5%>%
    group_by(Survey,StationCode,WeightingFactor)%>%
    summarize(Sum_n=sum(adj_LF,na.rm=TRUE))
  SI<-SI%>%
    mutate(weightedCatch=WeightingFactor*Sum_n)
  surveyIndex<-SI%>%
    group_by(Survey)%>%
    summarize(surveyIndex=sum(weightedCatch,na.rm=TRUE)/1000)
  Surveyresults<-left_join(bySurvey,surveyIndex,by="Survey")
  
 Surveyresults
}

###########determine the mid-date of each survey

middate<-function(samplefile,yearexamine){
  library(lubridate)#package for working with dates/times
  #samplefile$SampleDate<-mdy_hms(samplefile$SampleDate)
  samplefile<-mutate(samplefile,year=year(samplefile$SampleDate))
  yearsample<-samplefile[samplefile$year==yearexamine,]

  station<-read_excel("data/luStation.xlsx")#csv file of station information
  station<-mutate(station,StationCode=StationCodeSTN)
  
  yearsample<-left_join(yearsample,station,by="StationCode")
  yearsample<-yearsample[yearsample$Index==1,]
  
  bySurvey<-yearsample%>%
    group_by(Survey)%>%
    summarize(MinDate=min(SampleDate,na.rm=FALSE),MaxDate=max(SampleDate,na.rm=FALSE))
 
  bySurvey<-bySurvey%>%
    mutate(length=interval(MinDate,MaxDate))
  
  bySurvey<-bySurvey%>%
    mutate(length=int_length(length))
  
  bySurvey<-bySurvey%>%
    mutate(half_length=length/2)
  
  bySurvey<-bySurvey%>%
    mutate(middate=round_date(MinDate+half_length,unit="day"))
  bySurvey
}

###################growth rate calculation
grwthrate<-function(MLS,MD){
  library(lubridate)#package for working with dates/times
  MD<-as.data.frame(MD)
  #MD$middate<-ymd(MD$middate)
  
  FL_index<-MLS[MLS$meanFL>=38.1,]
  sindex<-L_index$Survey[1]
  
  if(sindex!=1){FL_S2=sindex}else{FL_S2=1}
  FL_S1<-FL_S2-1
  
  FL2<-MLS[MLS$Survey==FL_S2,4]#extract meanFL
  FL1<-MLS[MLS$Survey==FL_S1,4]#extract mean FL
  
  MD2<-MD[MD$Survey==FL_S2,6]
  MD1<-MD[MD$Survey==FL_S1,6]
  
  time<-int_length(interval((MD1),(MD2)))/(60*60*24)
  
  growth_rate<-as.numeric((FL2-FL1)/time)
  dateset<-round_date(MD1+round(as.numeric((38.1-FL1)/growth_rate),0),unit="day")
  
    list(growth_rate=growth_rate,Date_set=dateset)
}


#################################################################

annualSBindex<-function(MLS){
  
  FL_index<-MLS[MLS$meanFL>=38.1,]
  
  ifelse(is.na(FL_index$Survey[1]),FL_S2<-max(MLS$Survey),ifelse(FL_index$Survey[1]!=1,FL_S2<-FL_index$Survey[1],FL_S2<-1))
  
  FL_S1<-FL_S2-1
  
  FL2<-as.numeric(MLS[MLS$Survey==FL_S2,4])#extract meanFL
  FL1<-as.numeric(MLS[MLS$Survey==FL_S1,4])#extract mean FL
  
  SI2<-log(as.numeric(MLS[MLS$Survey==FL_S2,5]))#extract meanFL
  SI1<-log(as.numeric(MLS[MLS$Survey==FL_S1,5]))#extract meanFL
  
  annualindex <- exp(SI1 + ((SI2 - SI1) / (FL2 - FL1)) * (38.1 - FL1))
  ifelse(is.numeric(annualindex),annualindex,NA)
  annualindex
}



annualindex<-function(data, Fish){
  
  Fish1 = select(data, Fish)
  names(Fish1) = "Fish"
  data = cbind(data, Fish1)
  station<-read_excel("data/luStation.xlsx")#csv file of station information
  station<-mutate(station,StationCode=StationCodeSTN)
  data = mutate(data, StationCode = as.character(`Station Code`))
  data = left_join(data, station) %>%
    mutate(weightedcatch = WeightingFactor*Fish) %>%
    filter(Index ==1, Survey %in% c(1,2))
  
  data2 = group_by(data, Year, Survey) %>%
    summarize(surveyIndex = sum(weightedcatch))
  
 

  
annualindex = group_by(data2, Year) %>%
  summarize(Index = round(mean(surveyIndex)/1000, 1))
  #Filter to the first two surveys  
}


