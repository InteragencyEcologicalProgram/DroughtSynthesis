require(dplyr)
require(DroughtData)
require(ggplot2)
require(patchwork)
require(knitr)
require(car)
require(multcomp)
require(multcompView)
require(emmeans)
require(stringr)
require(readr)

#prepare model plotter and tukey plotter functions
model_plotter<-function(model, data){
  data<-data%>%
    mutate(Residuals=resid(model),
           Fitted=predict(model))
  
  p_hist<-ggplot(data, aes(x=Residuals))+
    geom_histogram()+
    xlab("Residuals (cm)")+
    theme_bw()
  
  p_res_fit<-ggplot(data, aes(x=Residuals, y=Fitted))+
    geom_point()+
    ylab("Predicted secchi (cm)")+
    xlab("Residuals (cm)")+
    theme_bw()
  
  p_obs_fit<-ggplot(data, aes(x=Secchi, y=Fitted))+
    geom_point()+
    geom_abline(slope=1, intercept=0, color="red")+
    ylab("Predicted secchi (cm)")+
    xlab("Observed secchi (cm)")+
    theme_bw()
  
  out<-(p_hist+plot_layout(ncol=1))+(p_res_fit+p_obs_fit+plot_layout(ncol=2))+plot_layout(nrow=2, widths=c(1, 0.5, 0.5))
  
  return(out)
}

tukey_plotter<-function(model, data, data_type, model_type){
  
  tuk<-emmeans(model, list(data=data_type, model=model_type))
  
  tuk_data<-as_tibble(cld(tuk$data, sort=FALSE, Letters = letters))%>%
    mutate(.group=str_remove_all(.group, fixed(" ")))%>%
    left_join(data%>%
                group_by(across(all_of(data_type)))%>%
                summarise(max_sec=max(Secchi), .groups="drop"),
              by=data_type)
  
  tuk_model<-as_tibble(cld(tuk$model, sort=FALSE, Letters = letters))%>%
    mutate(.group=str_remove_all(.group, fixed(" ")))%>%
    left_join(data%>%
                group_by(across(all_of(model_type)))%>%
                summarise(max_sec=max(Secchi), .groups="drop"),
              by=model_type)
  
  p_data<-ggplot(tuk_data, aes(x=.data[[data_type]], y=emmean, ymin=lower.CL, ymax=upper.CL, label=.group))+
    geom_boxplot(data=data, aes(x=.data[[data_type]], y=Secchi), inherit.aes = FALSE)+
    geom_pointrange(color="red", position=position_nudge(x=0.1), size = 0.3)+
    geom_text(aes(y=max_sec+(max(data$Secchi)-min(data$Secchi))/20))+
    ylab("Secchi Depth (cm)")+
    theme_bw()
  
  p_model<-ggplot(tuk_model, aes(x=.data[[model_type]], y=emmean, ymin=lower.CL, ymax=upper.CL, label=.group))+
    geom_boxplot(data=data, aes(x=.data[[model_type]], y=Secchi), inherit.aes = FALSE)+
    geom_pointrange(color="red", position=position_nudge(x=0.1), size = 0.3)+
    geom_text(aes(y=max_sec+(max(data$Secchi)-min(data$Secchi))/20), angle=if_else(model_type=="Year_fac", 90, 0), hjust=if_else(model_type=="Year_fac", "left", NA_character_), vjust=0.25)+
    ylab("Secchi Depth (cm)")+
    theme_bw()+
  
  {if(model_type=="Year_fac"){
    list(geom_tile(data=data, 
                   aes(x=Year_fac, y=min(Secchi)-(max(Secchi)-min(Secchi))/20, 
                       fill=Drought, height=(max(Secchi)-min(Secchi))/20), 
                   inherit.aes = FALSE),
         xlab("Year"),
         theme(axis.text.x=element_text(angle=90, vjust=0.5)),
         scale_y_continuous(expand = expansion(mult=c(0,0.1))),
         drt_color_pal_drought())
  }}
  
  out<-p_data/p_model+plot_annotation(tag_levels="A")
  
  if(model_type=="Year_fac"){
    out<-out+plot_layout(heights = c(0.8, 1))
  }
  
  return(out)
}

tukey_log_plotter<-function(model, data, data_type, model_type){
  
  tuk<-emmeans(model, list(data=data_type, model=model_type))
  
  tuk_data<-as_tibble(cld(tuk$data, sort=FALSE, Letters = letters))%>%
    mutate(.group=str_remove_all(.group, fixed(" ")))%>%
    left_join(data%>%
                group_by(across(all_of(data_type)))%>%
                summarise(max_sec=max(log_Secchi), .groups="drop"),
              by=data_type)
  
  tuk_model<-as_tibble(cld(tuk$model, sort=FALSE, Letters = letters))%>%
    mutate(.group=str_remove_all(.group, fixed(" ")))%>%
    left_join(data%>%
                group_by(across(all_of(model_type)))%>%
                summarise(max_sec=max(log_Secchi), .groups="drop"),
              by=model_type)
  
  p_data<-ggplot(tuk_data, aes(x=.data[[data_type]], y=emmean, ymin=lower.CL, ymax=upper.CL, label=.group))+
    geom_boxplot(data=data, aes(x=.data[[data_type]], y=log_Secchi), inherit.aes = FALSE)+
    geom_pointrange(color="red", position=position_nudge(x=0.1), size = 0.3)+
    geom_text(aes(y=max_sec+(max(data$log_Secchi)-min(data$log_Secchi))/20))+
    ylab("log(Secchi Depth [cm])")+
    theme_bw()
  
  p_model<-ggplot(tuk_model, aes(x=.data[[model_type]], y=emmean, ymin=lower.CL, ymax=upper.CL, label=.group))+
    geom_boxplot(data=data, aes(x=.data[[model_type]], y=log_Secchi), inherit.aes = FALSE)+
    geom_pointrange(color="red", position=position_nudge(x=0.1), size = 0.3)+
    geom_text(aes(y=max_sec+(max(data$log_Secchi)-min(data$log_Secchi))/20), angle=if_else(model_type=="Year_fac", 90, 0), hjust=if_else(model_type=="Year_fac", "left", NA_character_), vjust=0.25)+
    ylab("log(Secchi Depth [cm])")+
    theme_bw()+
    {if(model_type=="Year_fac"){
      list(geom_tile(data=data, 
                     aes(x=Year_fac, y=min(log_Secchi)-(max(log_Secchi)-min(log_Secchi))/20, 
                         fill=Drought, height=(max(log_Secchi)-min(log_Secchi))/20), 
                     inherit.aes = FALSE),
           xlab("Year"),
           theme(axis.text.x=element_text(angle=90, vjust=0.5)),
           scale_y_continuous(expand = expansion(mult=c(0,0.1))),
           drt_color_pal_drought())
    }}
  
  out<-p_data/p_model+plot_annotation(tag_levels="A")
  
  if(model_type=="Year_fac"){
    out<-out+plot_layout(heights = c(0.8, 1))
  }
  
  return(out)
}


#SEASONAL

#Seasonal secchi plots

#load secchi seasonal data-updated data table to be secchi specific
secchidata_seasonal<-lt_seasonal%>%
  filter(!is.na(Secchi))%>%
  mutate(Season=factor(Season, levels=c("Winter", "Spring", "Summer", "Fall")),
         Year_fac=factor(YearAdj),
         Drought=factor(Drought, levels=c("D", "N", "W")))

#plot secchi seasonal data by year-removed free scale to more easily view differences
ggplot(secchidata_seasonal, aes(x=YearAdj, y=Secchi, fill=Drought))+
  geom_point(shape=21, color="black")+
  facet_wrap(~Season)+
  drt_color_pal_drought()+
  ylab("Secchi (cm)")+
  theme_bw()

#plot secchi seasonal data by drought index-removed free scale to more easily view differences
ggplot(secchidata_seasonal, aes(x=Drought, y=Secchi, fill=Drought))+
  geom_boxplot()+
  facet_wrap(~Season)+
  drt_color_pal_drought()+
  ylab("Secchi (cm)")+
  theme_bw()

#Seasonal Secchi Analyses

#Drought
m_sec_seas_d<-aov(Secchi ~ Drought + Season, data=secchidata_seasonal)

#check assumptions seasonal secchi drought
model_plotter(m_sec_seas_d, secchidata_seasonal)
#`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

#histogram somewhat non-normal, try log transformation of secchi
logsecchidata_seasonal<-lt_seasonal%>%
  filter(!is.na(Secchi))%>%
  mutate(log_Secchi=log(Secchi), Season=factor(Season, levels=c("Winter", "Spring", "Summer", "Fall")),
         Year_fac=factor(YearAdj),
         Drought=factor(Drought, levels=c("D", "N", "W")))

m_sec_seas_d_log<-aov(log_Secchi ~ Drought + Season, data=logsecchidata_seasonal)

model_plotter(m_sec_seas_d_log, secchidata_seasonal)
#log def seems to be the better way to go

#check results of log transformed seasonal secchi drought
m_sec_seas_d_log_Anova<-Anova(m_sec_seas_d_log, type=2)
m_sec_seas_d_log_Anova

#Anova Table (Type II tests)
#
#Response: log(Secchi)
#Sum Sq  Df F value   Pr(>F)    
#Drought    4.2925   2  31.171 3.61e-12 ***
#  Season     3.9887   3  19.310 9.57e-11 ***
#  Residuals 11.0857 161                     
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#post-hoc test seasonal log secchi drought
p_m_sec_seas_d_log<-tukey_log_plotter(m_sec_seas_d_log, logsecchidata_seasonal, "Season", "Drought")
ggsave(
  plot = p_m_sec_seas_d_log,
  filename = "Water_Quality/LogSec_season_drought_model.png",
  height = 7,
  width = 6,
  units = "in"
)

p_m_sec_seas_d_log


#Year

m_sec_seas_y<-aov(Secchi ~ Year_fac + Season, data=secchidata_seasonal)

#check assumptions seasonal secchi year
model_plotter(m_sec_seas_y, secchidata_seasonal)
#`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
#histogram and residuals look good, don't transform

#check results seasonal secchi year
m_sec_seas_y_Anova<-Anova(m_sec_seas_y, type=2)
m_sec_seas_y_Anova

#Anova Table (Type II tests) seasonal secchi year
#
#Response: Secchi
#Sum Sq  Df F value    Pr(>F)    
#Year_fac   60145  46  12.497 < 2.2e-16 ***
#  Season     17364   3  55.320 < 2.2e-16 ***
#  Residuals  12241 117                      
#---
#  Signif. codes:  
#  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#post-hoc test seasonal secchi year
p_m_sec_seas_y<-tukey_plotter(m_sec_seas_y, secchidata_seasonal, "Season", "Year_fac")
ggsave(
  plot = p_m_sec_seas_y, 
  filename = "Water_Quality/Sec_season_year_model.png", 
  height = 9, 
  width = 8, 
  units = "in"
)

p_m_sec_seas_y

#save seasonal secchi anova outputs
anovas<-bind_rows(
  
  mutate(as_tibble(m_sec_seas_d_log_Anova, rownames = "Parameter"), model="Seasonal_Drought"),
  mutate(as_tibble(m_sec_seas_y_Anova, rownames = "Parameter"), model="Seasonal_Year")
)%>%
  mutate(`Pr(>F)`=if_else(`Pr(>F)`<0.001, "< 0.001", as.character(round(`Pr(>F)`, 4))))%>%
  write_csv("Water_Quality/Sec_seas_anovas.csv")

#REGIONAL

#regional secchi plots

#load regional secchi data
secchidata_regional<-lt_regional%>%
  filter(!is.na(Secchi))%>%
  mutate(Region=factor(Region, levels=c("Suisun Marsh", "Suisun Bay", "Confluence", "SouthCentral", "North")),
         Year_fac=factor(YearAdj),
         Drought=factor(Drought, levels=c("D", "N", "W")))



#plot regional secchi data by year
ggplot(secchidata_regional, aes(x=YearAdj, y=Secchi, fill=Drought))+
  geom_point(shape=21, color="black")+
  facet_wrap(~Region)+
  drt_color_pal_drought()+
  ylab("Secchi (cm)")+
  theme(axis.text.x=element_text(angle = 90))

#plot regional secchi data by drought index
ggplot(secchidata_regional, aes(x=Drought, y=Secchi, fill=Drought))+
  geom_boxplot()+
  facet_wrap(~Region)+
  drt_color_pal_drought()+
  ylab("Secchi (cm)")+
  theme_bw()

#Regional Secchi Analyses

#Drought
m_sec_reg_d<-aov(Secchi ~ Drought + Region, data=secchidata_regional)

#check assumptions regional secchi drought
model_plotter(m_sec_reg_d, secchidata_regional)
#`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

#check results regional secchi drought
m_reg_d_Anova<-Anova(m_sec_reg_d, type=2)
m_reg_d_Anova

#Anova Table (Type II tests) regional secchi drought

#Response: Secchi
#Sum Sq  Df F value    Pr(>F)    
#Drought    22413   2  20.743 5.879e-09 ***
#  Region    111047   4  51.387 < 2.2e-16 ***
#  Residuals 115074 213                      
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#try a log transformation
m_reg_d_log<-aov(log(Secchi) ~ Drought + Region, data=secchidata_regional)
model_plotter(m_reg_d_log, secchidata_regional)
#better, use log secchi

#prepare regional secchi data with log transformation
logsecchidata_regional<-lt_regional%>%
  filter(!is.na(Secchi))%>%
  mutate(log_Secchi=log(Secchi), Region=factor(Region, levels=c("Suisun Marsh", "Suisun Bay", "Confluence", "SouthCentral", "North")),
         Year_fac=factor(YearAdj),
         Drought=factor(Drought, levels=c("D", "N", "W")))

m_sec_reg_d_log<-aov(log_Secchi ~ Drought + Region, data=logsecchidata_regional)

#check results regional log secchi drought
m_sec_reg_d_log_Anova<-Anova(m_sec_reg_d_log, type=2)
m_sec_reg_d_log_Anova

#Anova Table (Type II tests) regional log secchi drought

#Response: log_Secchi
#Sum Sq  Df F value    Pr(>F)    
#Drought    4.6819   2  33.584 2.101e-13 ***
#  Region    26.8466   4  96.287 < 2.2e-16 ***
#  Residuals 14.8470 213                      
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#post-hoc test regional secchi drought
p_m_sec_reg_d_log<-tukey_log_plotter(m_sec_reg_d_log, logsecchidata_regional, "Region", "Drought")
ggsave(
  plot = p_m_sec_reg_d_log,
  filename = "Water_Quality/LogSec_region_drought_model.png",
  height = 7, 
  width = 6, 
  units = "in"
)

p_m_sec_reg_d_log


#Year

m_sec_reg_y<-aov(Secchi ~ Year_fac + Region, data=secchidata_regional)

#check assumptions regional secchi year
model_plotter(m_sec_reg_y, secchidata_regional)
#`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

#try log of secchi in regional
m_sec_reg_y_log<-aov(log(Secchi) ~ Year_fac + Region, data=secchidata_regional)

model_plotter(m_sec_reg_y_log, secchidata_regional)
#better, use log of secchi in regional year, can use already loaded log transformed 
#regional data

m_sec_reg_y_log<-aov(log_Secchi ~ Year_fac + Region, data=logsecchidata_regional)

#check results regional log secchi year
m_sec_reg_y_log_Anova<-Anova(m_sec_reg_y_log, type=2)
m_sec_reg_y_log_Anova

#Anova Table (Type II tests)
#
#Response: log_Secchi
#Sum Sq  Df  F value    Pr(>F)    
#Year_fac  13.6973  46   8.6292 < 2.2e-16 ***
#  Region    29.5008   4 213.7305 < 2.2e-16 ***
#  Residuals  5.8317 169                       
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


#post hoc test regional log secchi year
p_m_sec_reg_y_log<-tukey_log_plotter(m_sec_reg_y_log, logsecchidata_regional, "Region", "Year_fac")
ggsave(
  plot = p_m_sec_reg_y_log,
  filename = "Water_Quality/LogSec_region_year_model.png",
  height = 9,
  width = 8,
  units = "in"
)

p_m_sec_reg_y_log


#save regional secchi anova outputs
anovas<-bind_rows(
  mutate(as_tibble(m_sec_reg_d_log_Anova, rownames = "Parameter"), model="Regional_Drought"),
  mutate(as_tibble(m_sec_reg_y_log_Anova, rownames = "Parameter"), model="Regional_Year")
)%>%
  mutate(`Pr(>F)`=if_else(`Pr(>F)`<0.001, "< 0.001", as.character(round(`Pr(>F)`, 4))))%>%
  write_csv("Water_Quality/Sec_reg_anovas.csv")


#2020/2021 COMPARISON PLOTS

#Compare 2021 secchi to prior years
raw_data<-DroughtData::raw_wq_1975_2021%>%
  filter(!is.na(Secchi))%>%
  left_join(secchidata_regional%>%
              distinct(YearAdj, SVIndex, YearType, Drought),
            by="YearAdj")%>%
  mutate(across(c(Drought, YearType), list(`20_21`=~case_when(YearAdj==2021 ~ "2021", 
                                                              YearAdj==2020 ~ "2020", 
                                                              TRUE ~ as.character(.x)))),
         across(c(YearType, YearType_20_21), ~factor(.x, levels=c("2020", "2021", "Critical", "Dry", "Below Normal", "Above Normal", "Wet"))),
         Region=factor(Region, levels=c("Suisun Marsh", "Suisun Bay", "Confluence", "SouthCentral", "North")),
         Season=factor(Season, levels=c("Winter", "Spring", "Summer", "Fall")))

#How does 2021 compare to Drought, Normal, and Wet periods?
p_2021_d<-ggplot(raw_data, aes(x=Drought_20_21, y=Secchi, fill=Drought))+
  geom_boxplot()+
  drt_color_pal_drought()+
  xlab("Drought")+
  ylab("Secchi Depth (cm)")+
  theme_bw()

ggsave(
  plot = p_2021_d,
  filename = "Water_Quality/Sec_drought_20_21.png",
  height = 4,
  width = 5,
  units = "in"
)

p_2021_d

#Does that change regionally or seasonally?
p_2021_d_rs<-ggplot(raw_data, aes(x=Drought_20_21, y=Secchi, fill=Drought))+
  geom_boxplot()+
  drt_color_pal_drought()+
  facet_grid(Season~Region, scales = "free_y")+
  xlab("Drought")+
  ylab("Secchi (cm)")+
  theme_bw()+
  theme(axis.text.x=element_text(angle = 45, hjust=1))
ggsave(plot=p_2021_d_rs, filename=file.path(save_dir, "Sec_drought_rs_20_21.png"), device="png", height=8, width=10, units="in")
p_2021_d_rs

#How does 2021 compare to each water year type?
p_2021_yt<-ggplot(raw_data, aes(x=YearType_20_21, y=Secchi, fill=YearType))+
  geom_boxplot()+
  drt_color_pal_yrtype()+
  xlab("Year type")+
  ylab("Secchi (cm)")+
  theme_bw()+
  theme(axis.text.x=element_text(angle = 45, hjust=1))
ggsave(plot=p_2021_yt, filename=file.path(save_dir, "Sec_yeartype_20_21.png"), device="png", height=4, width=6, units="in")
p_2021_yt

#Does that change regionally or seasonally?
p_2021_yt_rs<-ggplot(raw_data, aes(x=YearType_20_21, y=Secchi, fill=YearType))+
  geom_boxplot()+
  drt_color_pal_yrtype()+
  facet_grid(Season~Region, scales = "free_y")+
  xlab("Year type")+
  ylab("Secchi (cm)")+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45, hjust=1))
ggsave(plot=p_2021_yt_rs, filename=file.path(save_dir, "Sec_yeartype_rs_20_21.png"), device="png", height=8, width=12, units="in")
p_2021_yt_rs
