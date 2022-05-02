#Drought Barrier
#Hyperspectral imagery coverage data
#Franks Tract, Big Break, and Clifton Court

#to do list
#also bring in clifton court herbicide applications
#make polished versions of bar graphs
#add indicator for missing data years?
#could color by water year type

#load packages
library(tidyverse) #variety of data science tools
library(lubridate) #format date/time
library(janitor) #clean up column names
library(PerformanceAnalytics) #plotting correlations
library(DEGreport) #adds corr and p to plots
library(ggcorrplot) #plotting correlation matrix
library(ggpubr) #combining plots into panel
library(ggpmisc) #add equations and R^2 to plots


#correlations to run
#areas of SAV vs fluridone quantity and fluridone acres treated
#areas of SAV vs each water quality parameter


#read in data ------------------

#area estimates for the three regions

#2004-2008, 2014-2015, 2019-2020
cc <- read_csv("EDB/weeds_regional_area_estimates/CliftonCourt_wgs84_area_ha.csv")%>% 
  add_column("site" = as.factor("Clifton Court")) 

#2004-2008, 2014-2020
ft <- read_csv("EDB/weeds_regional_area_estimates/FranksTract_wgs84_area_ha.csv")%>% 
  add_column("site" = as.factor("Franks Tract"))

#2004-2008, 2014-2020
bb <- read_csv("EDB/weeds_regional_area_estimates/BigBreak_wgs84_area_ha.csv")%>% 
  add_column("site" = as.factor("Big Break")) 

#2021 data for all sites
new <- read_csv("EDB/weeds_regional_area_estimates/RegionalAreaEstimates_2021_Provisional.csv") 

#read in time series of area data for common area of the delta from SMR repo
smr <- read_csv("https://raw.githubusercontent.com/InteragencyEcologicalProgram/Status-and-Trends/master/data/AquaticVegCoverage_2004-2020_CSTARS_report.csv")

#estimated waterway area for each of the three sites, legal delta, and common area of delta
#use these to calculate proportion of area covered by SAV and FAV
ww <- read_csv("EDB/weeds_regional_area_estimates/waterway_area_ha.csv")

#read in sonde data (2015-2021)
wqf <- read_csv("EDB/frk_sonde_data_summary.csv")

#read in discrete wq data and delta outflow (2004-2021)
wqd <- read_csv("EDB/discrete_wq&outflow_data_summary.csv")

#read in fluridone application data (2006-2021)
herb <- read_csv("EDB/franks_tract_fluridone_applications_2006-2021_summary.csv")

#combine veg area estimates data into one data set (up to 2020)-----------

all <- bind_rows(ft,cc,bb)%>% 
  rename(year_month = Year) %>% 
  mutate(
    #create year column from year-month
    year=as.integer(str_sub(year_month,1,4))
    #create month column from year-month
    ,month=as.integer(str_sub(year_month,5,6))
    #sum all acreage categories to get total area
    ,total = rowSums(across(soil:shadow))
    #create column that sums the two FAV species
    ,fav = hyacinth + primrose
    #calculate proportion of area that is SAV
    ,sav_prop = sav/total
    #calculate proportion of area that is FAV
    ,fav_prop = fav/total
  )  %>% 
  #reduce to just needed columns and reorder them
  select(
    year
    ,month
    ,site
    ,sav
    ,fav
    ,sav_prop
    ,fav_prop
  ) %>% 
  glimpse()

#format the 2021 data to combine with rest of data series
new_formatted <- new %>%
  #only keep the column for area estimated in ha
  select(year:site,hectares) %>% 
  #for now drop, the whole delta and delta common area data
  filter(site!="Legal Delta" & site!="Delta Common Area") %>% 
  #convert to wide form
  pivot_wider(id_cols=c(year, month, site),names_from = type,values_from = hectares) %>% 
  #clean up column names
  clean_names() %>% 
  mutate(
    #make year an integer
    year=as.integer(year)
    #make month an integer
    ,month=as.integer(month)
    #sum all acreage categories to get total area
    ,total = rowSums(across(arundo:w_primrose))
    #create column that sums the two FAV species
    ,fav = w_hyacinth + w_primrose
    #calculate proportion of area that is SAV
    ,sav_prop = sav/total
    #calculate proportion of area that is FAV
    ,fav_prop = fav/total
  )   %>% 
  #reduce to just needed columns and reorder them
  select(
    year
    ,month
    ,site
    ,sav
    ,fav
    ,sav_prop
    ,fav_prop
  ) %>% 
  glimpse()

#combine 2021 data with rest of time series
alln <- bind_rows(all,new_formatted)

#add another way of calculating proportion of area as SAV and FAV
#existing columns sum area of all classes for denominator
#also try using standard waterway area, mostly derived from DBW data
allnw <- left_join(alln,ww) %>% 
  #create SAV and FAV proportions using waterway area
  mutate(
    sav_prop_w = sav/waterways_ha
    ,fav_prop_w = fav/waterways_ha
    )

#quick plots of correlation between two proportion types
ggplot(allnw,aes(sav_prop,sav_prop_w))+
  geom_abline(intercept = 0, slope=1,linetype="dashed")+
  geom_smooth(method = "lm")  + 
  geom_point() +
  geom_cor(method = "pearson")
  
ggplot(allnw,aes(fav_prop,fav_prop_w))+
  geom_abline(intercept = 0, slope=1,linetype="dashed")+
  geom_smooth(method = "lm")  + 
  geom_point() +
  geom_cor(method = "pearson")

#look at number of years for each imaging month
season <- alln %>% 
  distinct(year,month) %>% 
  group_by(month) %>% 
  summarize(count = n())

#total veg: convert wide to long
veg_tot <- alln %>% 
  select(year:fav) %>% 
  pivot_longer(c(sav:fav), names_to = "type_total", values_to = "area_ha") %>% 
  mutate(across(c("type_total"), as.factor))  

#proportion veg: convert wide to long
veg_prop <- alln %>% 
  select(year:site,sav_prop:fav_prop)%>% 
  pivot_longer(c(sav_prop:fav_prop), names_to = "type_prop", values_to = "area_prop") %>% 
  mutate(across(c("type_prop"), as.factor)) 

#focal sites: stacked bar plots------------------------------
#add symbol for missing data
#use better colors
(foc_ha <- ggplot(veg_tot, aes(x=year, y=area_ha,  fill = type_total))+
  geom_bar(position = "stack", stat = "identity", colour="grey25") + 
  ylab("Vegetation Coverage (ha)") + xlab("Year") + 
  #scale_fill_discrete(labels=c("FAV","SAV")) +
  scale_fill_manual(name= NULL
                    ,labels=c("Floating","Submersed")
                    ,values=c("#88BA33","#556B2F")
                    ,guide=guide_legend(keyheight=0.5)
  )  +
  #customizes names in legend key, specifies the custom color palette, and sets height of elements in legend
  theme(
    legend.box.spacing=unit(0, units="cm"), 
    legend.margin=margin(t=0,r=0,b=2,l=0, unit="pt")) +
  theme_bw()+
  facet_grid(site~.)
)
#ggsave(plot=foc_ha, "EDB/Hyperspectral_Veg_Area_TimeSeries_FocalSiteArea.png",type ="cairo-png",width=8, scale=0.8, height=7,units="in",dpi=300)


#proportion area: stacked bar plots
ggplot(veg_prop, aes(x=year, y=area_prop,  fill = type_prop))+
  geom_bar(position = "stack", stat = "identity") + 
  ylab("Vegetation Coverage Proportion") + xlab("Year") + 
  scale_fill_discrete(labels=c("FAV","SAV")) +
  facet_grid(site~.)

#summary stats--------------

#2004-2006: moderately low SAV
#2007-2008: DBW does intensive fluridone treatments
#2009-2013: no veg imagery
#2014: fairly low SAV
#2105 sudden increase in SAV
#2015-2020: sustained high SAV

erg <- alln %>% 
  #just FT 
  filter(site=="Franks Tract") 

bbeff <- alln %>% 
  filter(site=="Big Break")

cceff <- alln %>% 
  filter(site=="Clifton Court")
range(cceff$fav_prop)


#barplot of common area of delta through time--------------------

#format time series that is just missing 2021
veg_cm <- smr %>% 
  #drop 2021 because it's just NAs
  filter(year!="2021") %>% 
  #just keep the needed columns
  select(year, sav_prop,wh_prop,wp_prop) %>% 
  rename(sav = sav_prop) %>% 
  #the original fav_tot_prop column includes pennywort
  #lets make a new one that is just water hyacinth and water primrose
  mutate(fav = wh_prop + wp_prop) %>% 
  #drop unneeded columns
  select(-c(wh_prop,wp_prop)) %>% 
  #convert to long format
  pivot_longer(cols = sav:fav, names_to = "type", values_to = "prop") 
  
#get the 2021 data for the common delta area
veg_cm_new <- new %>%
  #only keep the column for area estimated in ha
  select(year:site,hectares) %>% 
  #for now drop, the whole delta and delta common area data
  filter(site=="Delta Common Area") %>%  
  #convert to wide form
  pivot_wider(id_cols=c(year, month, site),names_from = type,values_from = hectares) %>% 
  #clean up column names
  clean_names() %>% 
  mutate(
    #make year an integer
    year=as.integer(year)
    #make month an integer
    ,month=as.integer(month)
    #sum all acreage categories to get total area
    ,total = rowSums(across(arundo:w_primrose))
    #create column that sums the two FAV species
    ,fav = w_hyacinth + w_primrose
    #calculate proportion of area that is SAV
    ,sav_prop = sav/total
    #calculate proportion of area that is FAV
    ,fav_prop = fav/total
  )   %>% 
  #reduce to just needed columns and reorder them
  select(
    year
    ,month
    ,site
    ,sav
    ,fav
    ,sav_prop
    ,fav_prop
  ) %>% 
  glimpse()

#add another way of calculating proportion of area as SAV and FAV
#existing columns sum area of all classes for denominator
#also try using standard waterway area, mostly derived from DBW data
allnw2 <- left_join(veg_cm_new,ww) %>% 
  #create SAV and FAV proportions using waterway area
  #use this one
  mutate(
    sav_prop = sav/waterways_ha
    ,fav_prop = fav/waterways_ha
  ) %>% 
  #just keep needed columns
  select(year,sav_prop,fav_prop) %>% 
  rename(sav = sav_prop
         ,fav = fav_prop) %>% 
  #convert to long format
  pivot_longer(cols = sav:fav, names_to = "type", values_to = "prop")

#combine 2021 data with rest of time series
alln2 <- bind_rows(veg_cm,allnw2)

#make time series barplot
(veg_perc <- ggplot(data=alln2,aes(x=year, y=prop, fill=type)) + 
    #specifies the independent and dependent variables as well as groupings
    geom_bar(position="stack", stat="identity", colour="grey25")+  
    #specifies that this is a stacked bar plot
    ylab("Water area occupied") + xlab("Year")+
    #x- and y-axis labels
    scale_fill_manual(name= NULL
                      ,labels=c("Floating","Submerged")
                      ,values=c("#88BA33","#556B2F")
                      ,guide=guide_legend(keyheight=0.5)
    )  +
    #customizes names in legend key, specifies the custom color palette, and sets height of elements in legend
    theme(
      legend.box.spacing=unit(0, units="cm"), 
      legend.margin=margin(t=0,r=0,b=2,l=0, unit="pt")) +
    theme_bw()
)
#ggsave(plot=veg_perc, "EDB/Hyperspectral_Veg_Area_TimeSeries_DeltaCommonArea.png",type ="cairo-png",width=8, scale=0.9, height=4.5,units="in",dpi=300)


#Compare veg areas to discrete WQ and delta outflow--------------------------  
#NOTE: cut the discrete WQ data formatting and moved to different file
#Need to add the formatted version back to this file for subsequent analysis

#create data frame to match WQ stations and sites
stm <- as.data.frame(
  cbind(
    Station = c("Baystudy 853", "EMP C9", "EMP D19")
    , site = c("Big Break","Clifton Court","Franks Tract")
  )
)

#add region info to WQ data set
wr <- full_join(wqd,stm) %>% 
  arrange(site,year)

#then add veg data
wrv <-left_join(alln,wr) %>% 
  arrange(site,year)
#delta outflow data missing for Big Break but shouldn't be
#fix this in the AquaticVeg_EnvDrivers.R code

#format sonde water quality data-----------------
#only 2015-present, so not as useful for looking at imagery data set which starts in 2004
#use discrete WQ data instead

#need to convert long to wide for correlation matrix
wq_format <- wqf %>% 
  select(-value_se) %>% 
  pivot_wider(
    id_cols=c(year)
    ,names_from = parameter
    , values_from= value_mean
  ) %>% 
  mutate(year = as.integer(year)) %>% 
  glimpse()


#make correlation plots for veg area types------------
#NOTE: the within site correlations don't yet include 2021

#Clifton Court: correlations among all land surface types

#need to drop the years with missing data first
ccz <- cc %>% 
  filter(sav!=0)

chart.Correlation(ccz[2:10])
#significant correlations, highest to lowest
#water vs SAV (-1.00)
#primrose vs shadow
#npv vs soil
#npv vs sav
#npv vs water
#primrose vs riparian
#riparian vs emergent
#fav vs soil

#Franks Tract: correlations among all land surface types
chart.Correlation(ft[2:10])
#significant correlations, highest to lowest
#water vs SAV (-0.99)
#riparian vs emergent
#pennywort vs primrose
#primrose vs water
#sav vs primrose
#soil vs pennywort
#emergent vs hyacinth
#riparian vs hyacinth

#Big Bend: correlations among all land surface types
chart.Correlation(bb[2:10])

#make comparisons among sites within land types-----------

#format data sets
veg_corr <- alln %>% 
  select(year:site,sav_prop:fav_prop) %>% 
  pivot_wider(id_cols = c(year,month), names_from = site, values_from = c(sav_prop,fav_prop)) %>%
  #rename columns
  rename(
    sav_prop_ft = "sav_prop_Franks Tract" 
    ,sav_prop_bb = "sav_prop_Big Break"
    ,sav_prop_cc = "sav_prop_Clifton Court"
    ,fav_prop_ft = "fav_prop_Franks Tract" 
    ,fav_prop_bb = "fav_prop_Big Break"
    ,fav_prop_cc = "fav_prop_Clifton Court"
  ) %>% 
  glimpse()


#plot correlations
#chart.Correlation(bothw[c(2,3,6,7)])
#sav  corr is 0.74

#make slightly nicer plot focused on comparing SAV between sites
ggplot(veg_corr, aes(x=sav_prop_ft, y= sav_prop_bb))+
    geom_point() +
    geom_smooth(method = 'lm', se=F)+
    geom_text(aes(label=year)
              , vjust = -0.9
              )+
    xlab("Franks Tract SAV")+
    ylab("Big Break SAV ") 

ggplot(veg_corr, aes(x=sav_prop_ft, y= sav_prop_cc))+
  geom_point() +
  geom_smooth(method = 'lm', se=F)+
  geom_text(aes(label=year)
            , vjust = -0.9
  )+
  xlab("Franks Tract SAV")+
  ylab("Clifton Court SAV ") 


#SAV: Franks Tract vs Big Break
#next make this same plot but with % coverage instead of hectares
#the 1:1 line represents how the correlation would look if proportion of SAV
#coverage were the same at both sites across years
fb<-ggplot(veg_corr, aes(x=sav_prop_ft, y= sav_prop_bb))+
  geom_point() +
  geom_smooth(method = 'lm', se=T)+
  geom_abline(intercept = 0, slope=1,linetype="dashed")+
  geom_text(aes(label=year)
            , vjust = -0.9
  )+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +        
  xlab("Franks Tract SAV")+
  ylab("Big Break SAV ") +
  coord_cartesian(xlim = c(0,0.7), ylim = c(0, 0.7))

fb_mod <- cor.test(veg_corr$sav_prop_ft,veg_corr$sav_prop_bb)
#correlation is significant 
#p-value = 0.009311; cor=0.6881768 

#SAV: Franks Tract vs Clifton Court

fc<-ggplot(veg_corr, aes(x=sav_prop_ft, y= sav_prop_cc))+
  geom_point() +
  geom_smooth(method = 'lm', se=T)+
  geom_abline(intercept = 0, slope=1,linetype="dashed")+
  geom_text(aes(label=year)
            , vjust = -0.9
  )+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + 
  xlab("Franks Tract SAV")+
  ylab("Clifton Court SAV ")+
  coord_cartesian(xlim = c(0,0.7), ylim = c(0, 0.7))

fc_mod <- cor.test(veg_corr$sav_prop_ft,veg_corr$sav_prop_cc)
#correlation is significant 
#p-value = 0.01538; cor=0.7353165

#FAV: Franks Tract vs Big Break
#next make this same plot but with % coverage instead of hectares
#the 1:1 line represents how the correlation would look if proportion of SAV
#coverage were the same at both sites across years
fbf<-ggplot(veg_corr, aes(x=fav_prop_ft, y= fav_prop_bb))+
  geom_point() +
  geom_smooth(method = 'lm', se=T)+
  geom_abline(intercept = 0, slope=1,linetype="dashed")+
  geom_text(aes(label=year)
            , vjust = -0.9
  )+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +        
  xlab("Franks Tract FAV")+
  ylab("Big Break FAV ")
  #coord_cartesian(xlim = c(0,0.04), ylim = c(0, 0.125))

fbf_mod <- cor.test(veg_corr$fav_prop_ft,veg_corr$fav_prop_bb)
#correlation is significant 
#p-value = 4.738e-07; cor=0.9531752  

#FAV: Franks Tract vs Clifton Court

fcf<-ggplot(veg_corr, aes(x=fav_prop_ft, y= fav_prop_cc))+
  geom_point() +
  geom_smooth(method = 'lm', se=T)+
  geom_abline(intercept = 0, slope=1,linetype="dashed")+
  geom_text(aes(label=year)
            , vjust = -0.9
  )+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + 
  xlab("Franks Tract FAV")+
  ylab("Clifton Court FAV ")
  #coord_cartesian(xlim = c(0,0.04), ylim = c(0, 0.10))

fcf_mod <- cor.test(veg_corr$fav_prop_ft,veg_corr$fav_prop_cc)
#correlation is significant 
#p-value = 0.04372; cor=0.6457414

sfigure <- ggarrange(fbf,fcf,
                    labels = c("A", "B"),
                    ncol = 2, nrow = 1)
#ggsave(plot=sfigure, "EDB/Hyperspectral_SAV_AreaCorrPanel_FT_CC_BB.png",type ="cairo-png",width=8, height=4.5,units="in",dpi=300)

ffigure <- ggarrange(fbf,fcf,
                     labels = c("A", "B"),
                     ncol = 2, nrow = 1)
#ggsave(plot=ffigure, "EDB/Hyperspectral_FAV_AreaCorrPanel_FT_CC_BB.png",type ="cairo-png",width=8, height=4.5,units="in",dpi=300)

#correlations between SAV acreage and fluridone applications in Franks Tract----------

#join veg acreages and fluridone data sets by year
frankf <- left_join(alln,herb) %>% 
  filter(site=="Franks Tract") %>% 
  glimpse()

#plot correlation between SAV acreage and fluridone quantities 
#can only compare 2014-2020 (and eventually 2021)
#could request older treatment data from DBW
#DBW website doesn't have reports for years I need (2004-2008)
ggplot(frankf, aes(sav,fl_quantity_kg))+ 
  geom_smooth(method = "lm")  + 
  geom_point() +
  geom_text(aes(label=year)
            , vjust = -0.9
  )+
  geom_cor(method = "pearson")+
  xlab("Area of SAV (ha)")+
  ylab("Quantity of fluridone applied (kg))")
#correlation not significant

#plot correlation between SAV acreage and treatment acreage 
(flur<-ggplot(frankf, aes(fl_area_ha,sav))+ 
  geom_smooth(method = "lm")  + 
  geom_point() +
  geom_text(aes(label=year)
            , vjust = -0.9
  )+
  #geom_cor(method = "pearson")+
    xlab("Area treated with fluridone (ha)")+
    ylab("Area of SAV (ha)")+
    ylim(-250,1600)+
  theme_bw()
)
#ggsave(plot=flur, "EDB/Hyperspectral_SAV_Area_v_Fluridone.png",type ="cairo-png",width=8, scale=0.9, height=4.5,units="in",dpi=300)
#set y-axis range so it doesn't go below zero
#significant correlation
#hard to know if incrase is due to lack of treatment or DBW giving up because of veg intensity

#plot correlation between SAV acreage and treatment acreage
#2008 removed to see if relationship still significant

frankf8 <- frankf %>% 
  filter(year!=2008)

ggplot(frankf8, aes(fl_area_ha,sav))+ 
  geom_smooth(method = "lm")  + 
  geom_point() +
  geom_text(aes(label=year)
            , vjust = -0.9
  )+
  geom_cor(method = "pearson")+
  xlab("Area treated with fluridone (ha)")+
  ylab("Area of SAV (ha)")

#correlations among sonde water quality parameters and SAV acreage----------------
#sonde data starts 2015 while EMP discrete likely goes back much farther
#use discrete data instead (see below)

#reduce acreage data set to just year and sav acreage
frankfs <- frankf %>% 
  select(year,sav) 

#join sav acreage and wq data
frankw <- left_join(frankfs,wq_format) %>% 
  #drop years with no WQ data
  filter(year>2014)

#create correlation matrix
corr_matrix <- round(cor(frankw[2:8]),2)

# Computing correlation matrix with p-values
corrp_matrix <- cor_pmat(frankw[2:8])

#grid of correlations
ggcorrplot(corr_matrix, method ="square", type="lower", p.mat = corrp_matrix, lab=T)
#some WQ parameters are strongly correlated but not with SAV


# Visualizing the correlation matrix
ggcorrplot(corr_matrix, method ="square", type="lower")


#correlations among discrete water quality parameters and SAV acreage by site----------------

#clifton court only has discrete WQ data for 2016-2010
#and from that period only has imagery for 2019-2020
#so can't do a meaningful analysis

#probably need to do each of the three regions separately

#start by removing unneeded columns
wrvs <- wrv %>% 
  select(year,site:fav,Temperature:out_mean)

dfta <- wrvs %>% 
  filter(site == "Franks Tract") 
  
#add herbicide data
dft <- left_join(dfta,herb) %>% 
  #but only keep area treated
  select(-c(fl_rate_ppb,fl_quantity_kg)) %>% 
  glimpse()
#note we are missing treatment data for 2004-2005
  

dbb <- wrvs %>% 
  filter(site == "Big Break")

#create correlation matrices
#needed use="pairwise.complete.obs" because of NAs
f_corr_matrix <- round(cor(dft[3:12],use="pairwise.complete.obs"),3)
b_corr_matrix <- round(cor(dbb[c(3:8,11)],use="pairwise.complete.obs"),3)

# Computing correlation matrix with p-values
f_corrp_matrix <- round(cor_pmat(dft[3:12],use="pairwise.complete.obs"),3)
b_corrp_matrix <- round(cor_pmat(dbb[c(3:8,11)],use="pairwise.complete.obs"),3)

#grid of correlations
ggcorrplot(f_corr_matrix, method ="square", type="lower", p.mat = f_corrp_matrix, lab=T)
ggcorrplot(b_corr_matrix, method ="square", type="lower", p.mat = b_corrp_matrix, lab=T)
#few WQ parameters are strongly correlated to SAV area and none have sign. p-values
#BB SAV and salinity have corr = -0.52

#SAV: Extract the corr and pval from the comparisons between sav and wq parameters
fout <- as.data.frame(cbind("f_corr" = f_corr_matrix[,1],"f_pval" = f_corrp_matrix[,1]))
fout$parameter <- row.names(fout)

bout <- as.data.frame(cbind("b_corr" = b_corr_matrix[,1],"b_pval" = b_corrp_matrix[,1]))
bout$parameter <- row.names(bout)

#combine output df by parameter
aout <- full_join(fout,bout)

#clean up output df
vstat <- aout %>% 
  #no need to have corr of SAV with itself
  #also conductivity and salinity are perfectly correlated with each other
  filter(parameter!="sav" & parameter!="Salinity") %>% 
  select(parameter,f_corr:f_pval,b_corr:b_pval)
#write_csv(vstat,"EDB/sav_area_corr_discrete_wq.csv")

#FAV: Extract the corr and pval from the comparisons between fav and wq parameters
fout2 <- as.data.frame(cbind("f_corr" = f_corr_matrix[,2],"f_pval" = f_corrp_matrix[,2]))
fout2$parameter <- row.names(fout2)

bout2 <- as.data.frame(cbind("b_corr" = b_corr_matrix[,2],"b_pval" = b_corrp_matrix[,2]))
bout2$parameter <- row.names(bout2)

#combine output df by parameter
aout2 <- full_join(fout2,bout2)

#clean up output df
vstat2 <- aout2 %>% 
  #no need to have corr of SAV with itself
  #also conductivity and salinity are perfectly correlated with each other
  filter(parameter!="fav" & parameter!="Salinity") %>% 
  select(parameter,f_corr:f_pval,b_corr:b_pval)
#write_csv(vstat2,"EDB/fav_area_corr_discrete_wq.csv")

#plot correlation between SAV acreage and EC at Big Break
#note that EC and delta outflow are correlated so stick with outflow plot for report
(ec<-ggplot(dbb, aes(Conductivity,sav))+ 
    geom_smooth(method = "lm")  + 
    geom_point() +
    geom_text(aes(label=year)
              , vjust = -0.9
    )+
    #geom_cor(method = "pearson")+
    xlab("Annual mean conductivity (uS per cm)")+
    ylab("Area of SAV (ha)")+
    theme_bw()
)

#plot correlation between SAV acreage and Delta outflow at Big Break
(oflw<-ggplot(dbb, aes(out_mean,sav))+ 
    geom_smooth(method = "lm")  + 
    geom_point() +
    geom_text(aes(label=year)
              , vjust = -0.9
    )+
    #geom_cor(method = "pearson")+
    xlab("Annual mean delta outflow (cubic ft per sec)")+
    ylab("Area of SAV (ha)")+
    theme_bw()
)
#ggsave(plot=oflw, "EDB/Hyperspectral_SAV_Area_v_Outflow.png",type ="cairo-png",width=8, scale=0.9, height=4.5,units="in",dpi=300)

#plot correlation between FAV acreage and temperature at Big Break
(wtemp<-ggplot(dbb, aes(Temperature,fav))+ 
    geom_smooth(method = "lm")  + 
    geom_point() +
    geom_text(aes(label=year)
              , vjust = -0.9
    )+
    #geom_cor(method = "pearson")+
    xlab("Annual mean water temperature (C)")+
    ylab("Area of FAV (ha)")+
    theme_bw()
)
#ggsave(plot=wtemp, "EDB/Hyperspectral_FAV_Area_v_Temp.png",type ="cairo-png",width=8, scale=0.9, height=4.5,units="in",dpi=300)

#try to build some multiple regression models--------------------
#might not work because of small sample size

#Franks Tract
#originally considered the following predictors
#temperature, EC, secchi, outflow, herbicides
#but EC is significantly correlated with temp and outflow
#so just use temp, secchi, ouflow, herbicides
#note that secchi is tricky because remote sensing's ability to detect SAV is affected 
#by turbidity in addition to veg both affecting and being affected by turbidity
#did also look at correlations between veg and DO and pH but these are responses to veg
#rather than predictors

#SAV
ft_sav_mod <- lm(sav ~ fl_area_ha + out_mean + Temperature + Secchi + fav, data = dft)
summary(ft_sav_mod)
anova(ft_sav_mod)
#only herbicide is significant
#same result we got with the correlations

#FAV
#didn't include EC because correlated with temp and flow
#didn't include Secchi because shouldn't affect FAV much
#didn't include pH and DO because those are responses
#didn't include herbicides because I don't have data handy
ft_fav_mod <- lm(fav ~ out_mean + Temperature, data = dft)
summary(ft_fav_mod)
anova(ft_fav_mod)
#neither is significant

#Big Break

#SAV
#didn't include DO or pH because responses and also not available from Bay Study station
#didn't include EC because correlated with outflow
#don't have herbicide data handy
bb_sav_mod <- lm(sav ~ out_mean + Temperature + Secchi + fav, data = dbb)
summary(bb_sav_mod)
anova(bb_sav_mod)
#only outflow significant

#SAV
#didn't include DO or pH because responses and also not available from Bay Study station
#didn't include EC because correlated with outflow
#didn't include Secchi because shouldn't affect FAV
#don't have herbicide data handy
bb_fav_mod <- lm(fav ~ out_mean + Temperature + Secchi, data = dbb)
summary(bb_fav_mod)
anova(bb_fav_mod)
#only temperature significant

#correlations among discrete water quality parameters and SAV acreage across site----------------
#trying this because too little replication within individual sites
#mostly data from Big Break and Franks Tract

#start by removing unneeded columns
wrvs2 <- wrv %>% 
  select(year,site,sav_prop,Temperature:pH) %>% 
  #drop clifton court because only two years of paired data
  #filter(site!="Clifton Court") %>% 
  glimpse()


#plot correlation between SAV acreage and conductance
ggplot(wrvs2, aes(sav_prop,Conductivity))+ 
  geom_smooth(method = "lm")  + 
  geom_point() +
  geom_text(aes(label=year,col=site)
            , vjust = -0.9
  )+
  geom_cor(method = "pearson")+
  xlab("Proportion Area of SAV")+
  ylab("Conductance")
#marginally sign. corr p=0.08

#plot correlation between SAV acreage and temperature
ggplot(wrvs2, aes(sav_prop,Temperature))+ 
  geom_smooth(method = "lm")  + 
  geom_point() +
  geom_text(aes(label=year,col=site)
            , vjust = -0.9
  )+
  geom_cor(method = "pearson")+
  xlab("Proportion Area of SAV")+
  ylab("Temperature")
#no sign. corr

#plot correlation between SAV acreage and secchi
ggplot(wrvs2, aes(sav_prop,Secchi))+ 
  geom_smooth(method = "lm")  + 
  geom_point() +
  geom_text(aes(label=year,col=site)
            , vjust = -0.9
  )+
  geom_cor(method = "pearson")+
  xlab("Proportion Area of SAV")+
  ylab("Secchi")
#significant corr; p=0.03
#is this because sav gets more light when the water is clearer (higher secchi) or
#because it's easier to image sav when water is clear or both?






