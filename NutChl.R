#Cclculate and graph potential chlorophyll and N:P ratios
#Origional code by Mine Berg mbearg@esassoc.com
#Updated by Rosemary Hartman


library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)

#Hi Rosemary, below is the code for NP ratio calculations and potential Chl a. 
#These calculations were used to add the columns to the DWR_NutChl.csv spreadsheet that I emailed you previously. 
#Just FYI, to go from mg to ug, values are multiplied by 1000, and to go from ug to umol, values are divided by 14 for N and by 31 for P. 
#Contracting into one step, to go from mg/L NH4 to umol/L NH4 multiply by 71.43 and to go from mg/L orthophosphate to umol/L orthophosphate, 
#multiply by 32.26. Best, Mine
hab_nutr_chla_mvi <- read_csv("data/hab_nutr_chla_mvi.csv")

#Use the dataset that Dave made for us, change some names
nc<- hab_nutr_chla_mvi %>%
  rename(Ammonium_mgL = DissAmmonia,
         Nitrate_mgL = DissNitrateNitrite,
         Orthophosphate_mgL = DissOrthophos,
         Chla_ugL = Chlorophyll)

#Calculate molar nitrogen and phosphorus  
nc = mutate(nc, NH4_umolL= Ammonium_mgL*71.43,
            NO3_umolL= Nitrate_mgL*71.43,
            PO4_umolL= Orthophosphate_mgL*32.26,
            DIN_umolL= NH4_umolL + NO3_umolL,
            #Nitrogen to phosphorus ratio
            NPratio<- DIN_umolL / PO4_umolL,
            #Potential chlorophyll (Based on nitrogen)
            PotChla_ugL= (DIN_umolL + Chla_ugL),
            Year = year(Date),
            month = month(Date))

#To calculate residual chlorophyll, residual nitrogen concentration was converted to chlorophyll 
#using the ratio 1 micromole N: 1 microgram chlorophyll-a (Cloern and Jassby 2012; Gowen et al. 1992). 
#Residual nitrogen was calculated by summing all the dissolved inorganic nitrogen species (nitrate + nitrite + ammonium) in units of molar mass N. 
#Potential chlorophyll-a was compared with measured chlorophyll-a for each region of the Delta for the summers of 2014–2020, and for summer 2021.

#Pivot longer for graphing
lo = pivot_longer(nc, cols = c(Chla_ugL, DIN_umolL), names_to = "Analyte", values_to = "mgL")

#mean value per month and region
lomean = group_by(lo, Region, Year, month, Analyte) %>%
  summarize(mgLm = mean(mgL, na.rm = T))

#Just the summer months
cs<-subset(lomean, month>3 & month<10)
cs$Analyte<-factor(cs$Analyte, levels=c("DIN_umolL","Chla_ugL"))
cs$Region<-factor(cs$Region, levels=c("Cache/Liberty","Upper Sac","Lower Sac","East Delta","Lower SJ","Franks","OMR","South Delta"))

#Plot potentail chlorophyll and measuredchlorophyll by month and region
c<-ggplot(cs, aes(fill=Analyte, y=mgLm, x= month))+
  geom_bar(position="stack", stat="identity")+
  facet_grid(Region~Year)+
  scale_fill_brewer(palette="Set2", name="Chlorophyll", labels=c("Potential","Measured"))+
  scale_y_continuous("Chlorophyll (μg/L)")+scale_x_continuous("Month", breaks=c(4,5,6,7,8,9))
c

#Apply a theme
t<-theme(strip.text.x = element_text(size = 14, colour = "black"),strip.text.y = element_text(size = 14, colour = "black"))+
  theme(axis.title.x=element_text(size=16),axis.title.y=element_text(size=16))+
  theme(axis.text.x  = element_text(size=14), axis.text.y  = element_text(size=14))

#Update and save plot
c1<-c+theme_bw()+t
c1
ggsave("ChlRegions.png")

#Repeat for just the southern Delta
cf<-cs[cs$Region == 'Lower SJ' | cs$Region == 'Franks' | cs$Region == 'OMR' ,]
d<-ggplot(cf, aes(fill=Analyte, y=Mean, x=month))+
  geom_bar(position="stack", stat="identity")+
  facet_grid(Region~Year)+
  scale_fill_brewer(palette="Set2", name="Chlorophyll", labels=c("Potential","Measured"))+
  scale_y_continuous("Chlorophyll (μg/L)")+
  scale_x_continuous("Month", breaks=c(4,5,6,7,8,9))

d1<-d+theme_bw()+t
d1
ggsave("ChlFranks.png")

###############################################################
#N:P ratio


#calculate mean, min, max and standard deviation by month and year and region
Ratiossum = group_by(nc, Year, month, Region) %>%
  summarize(NPratioM = mean(NPratio, na.rm = T), sdNP = sd(NPratio, na.rm = T), minNP = min(NPratio), maxNP = max(NPratio))

ggplot(filter(Ratiossum, month %in% c(6,7,8,9)), aes(x = month, y = NPratioM)) + geom_col()+ facet_grid(Region~Year)+
  geom_hline(yintercept = 16, color = "red")+
  geom_errorbar(aes(ymin = NPratioM-sdNP, ymax = NPratioM + sdNP))

load("Regions.RData")
#This is Figure A-3 in Appendix A. 
ggplot(Ratiossum, aes(x = month, y = NPratioM, fill = Region)) + geom_col()+ 
  facet_grid(Region~Year, scales = "free_y")+
  geom_hline(yintercept = 16, color = "red")+
  geom_errorbar(aes(ymin = minNP, ymax = maxNP))+
  scale_fill_manual(values = reg3$colors, guide = NULL)+
  scale_x_continuous(breaks = c(2,4,6,8,10))+
  xlab("Month of Year")+ ylab("N:P Ratio")+
  theme_bw()
