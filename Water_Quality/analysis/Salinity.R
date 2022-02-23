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
    xlab("Residuals (PSU)")+
    theme_bw()
  
  p_res_fit<-ggplot(data, aes(x=Residuals, y=Fitted))+
    geom_point()+
    ylab("Predicted salinity (PSU)")+
    xlab("Residuals (PSU)")+
    theme_bw()
  
  p_obs_fit<-ggplot(data, aes(x=Salinity, y=Fitted))+
    geom_point()+
    geom_abline(slope=1, intercept=0, color="red")+
    ylab("Predicted salinity (PSU)")+
    xlab("Observed salinity (PSU)")+
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
                summarise(max_sal=max(Salinity), .groups="drop"),
              by=data_type)
  
  tuk_model<-as_tibble(cld(tuk$model, sort=FALSE, Letters = letters))%>%
    mutate(.group=str_remove_all(.group, fixed(" ")))%>%
    left_join(data%>%
                group_by(across(all_of(model_type)))%>%
                summarise(max_sal=max(Salinity), .groups="drop"),
              by=model_type)
  
  p_data<-ggplot(tuk_data, aes(x=.data[[data_type]], y=emmean, ymin=lower.CL, ymax=upper.CL, label=.group))+
    geom_boxplot(data=data, aes(x=.data[[data_type]], y=Salinity), inherit.aes = FALSE)+
    geom_pointrange(color="red", position=position_nudge(x=0.1), size = 0.3)+
    geom_text(aes(y=max_sal+(max(data$Salinity)-min(data$Salinity))/20))+
    ylab("Salinity (PSU)")+
    theme_bw()
  
  p_model<-ggplot(tuk_model, aes(x=.data[[model_type]], y=emmean, ymin=lower.CL, ymax=upper.CL, label=.group))+
    geom_boxplot(data=data, aes(x=.data[[model_type]], y=Salinity), inherit.aes = FALSE)+
    geom_pointrange(color="red", position=position_nudge(x=0.1), size = 0.3)+
    geom_text(aes(y=max_sal+(max(data$Salinity)-min(data$Salinity))/20), angle=if_else(model_type=="Year_fac", 90, 0), hjust=if_else(model_type=="Year_fac", "left", NA_character_), vjust=0.25)+
    ylab("Salinity (PSU)")+
    theme_bw()+
  
  {if(model_type=="Year_fac"){
    list(geom_tile(data=data, 
                   aes(x=Year_fac, y=min(Salinity)-(max(Salinity)-min(Salinity))/20, 
                       fill=Drought, height=(max(Salinity)-min(Salinity))/20), 
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

#seasonal salinity plots

#load seasonal salinity data
salinitydata_seasonal<-lt_seasonal%>%
  filter(!is.na(Salinity))%>%
  mutate(Season=factor(Season, levels=c("Winter", "Spring", "Summer", "Fall")),
         Year_fac=factor(YearAdj),
         Drought=factor(Drought, levels=c("D", "N", "W")))

#plot seasonal salnity data by year
ggplot(salinitydata_seasonal, aes(x=YearAdj, y=Salinity, fill=Drought))+
  geom_point(shape=21, color="black")+
  facet_wrap(~Season)+
  drt_color_pal_drought()+
  ylab("Salinity (PSU)")+
  theme_bw()

#plot seasonal salinity data by drought index
ggplot(salinitydata_seasonal, aes(x=Drought, y=Salinity, fill=Drought))+
  geom_boxplot()+
  facet_wrap(~Season)+
  drt_color_pal_drought()+
  ylab("Salinity (PSU)")+
  theme_bw()

#Seasonal Salinity Analyses

#Drought
m_sal_seas_d<-aov(Salinity ~ Drought + Season, data=salinitydata_seasonal)

#check assumptions seasonal salinity drought
model_plotter(m_sal_seas_d, salinitydata_seasonal)
#`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

#check results seasonal salinity drought
m_sal_seas_d_Anova<-Anova(m_sal_seas_d, type=2)
m_sal_seas_d_Anova

#Anova Table (Type II tests) seasonal salinity drought
#
#Response: Salinity
#Sum Sq  Df F value    Pr(>F)    
#Drought   149.776   2 104.269 < 2.2e-16 ***
#  Season     81.545   3  37.846 < 2.2e-16 ***
#  Residuals 122.816 171                      
#---
#  Signif. codes:  
#  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#post hoc test seasonal salinity drought
p_m_sal_seas_d<-tukey_plotter(m_sal_seas_d, salinitydata_seasonal, "Season", "Drought")
ggsave(
  plot = p_m_sal_seas_d,
  filename = "Water_Quality/Sal_season_drought_model.png",
  height = 7,
  width = 6,
  units = "in"
)

p_m_sal_seas_d


#Year

m_sal_seas_y<-aov(Salinity ~ Year_fac + Season, data=salinitydata_seasonal)

#check assumptions seasonal salinity year
model_plotter(m_sal_seas_y, salinitydata_seasonal)
#`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

#check results seasonal salinity year
m_sal_seas_y_Anova<-Anova(m_sal_seas_y, type=2)
m_sal_seas_y_Anova

#Anova Table (Type II tests) seasonal salinity year
#
#Response: Salinity
#Sum Sq  Df F value    Pr(>F)    
#Year_fac  215.143  46  10.339 < 2.2e-16 ***
#  Season     77.429   3  57.057 < 2.2e-16 ***
#  Residuals  57.448 127                      
#---
#  Signif. codes:
#  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#post-hoc test seasonal salinity year
p_m_sal_seas_y<-tukey_plotter(m_sal_seas_y, salinitydata_seasonal, "Season", "Year_fac")
ggsave(
  plot = p_m_sal_seas_y,
  filename = "Water_Quality/Sal_season_year_model.png",
  height = 10,
  width = 9, 
  units = "in"
)

p_m_sal_seas_y


#save seasonal salinity anova outputs
anovas<-bind_rows(
  
  mutate(as_tibble(m_sal_seas_d_Anova, rownames = "Parameter"), model="Seasonal_Drought"),
  mutate(as_tibble(m_sal_seas_y_Anova, rownames = "Parameter"), model="Seasonal_Year")
)%>%
  mutate(`Pr(>F)`=if_else(`Pr(>F)`<0.001, "< 0.001", as.character(round(`Pr(>F)`, 4))))%>%
  write_csv("Water_Quality/Sal_seas_anovas.csv")

#REGIONAL

#regional salinity plots

#load regional salinity data
salinitydata_regional<-lt_regional%>%
  filter(!is.na(Salinity))%>%
  mutate(Region=factor(Region, levels=c("Suisun Marsh", "Suisun Bay", "Confluence", "SouthCentral", "North")),
         Year_fac=factor(YearAdj),
         Drought=factor(Drought, levels=c("D", "N", "W")))

#plot regional salinity data by year
ggplot(salinitydata_regional, aes(x=YearAdj, y=Salinity, fill=Drought))+
  geom_point(shape=21, color="black")+
  facet_wrap(~Region, scale="free_y")+
  drt_color_pal_drought()+
  ylab("Salinity (PSU)")+
  theme(axis.text.x = element_text(angle = 90))
 
#would be good to place S Central and North plots on a different scale

#plot regional salinity data by drought index
ggplot(salinitydata_regional, aes(x=Drought, y=Salinity, fill=Drought))+
  geom_boxplot()+
  facet_wrap(~Region, scale="free_y")+
  drt_color_pal_drought()+
  ylab("Salinity (PSU)")+
  theme_bw()
#again probably be good to change scale of S Central and North plots 

#Regional Salinity Analyses

#Drought
m_sal_reg_d<-aov(Salinity ~ Drought + Region, data=salinitydata_regional)

#check assumptions regional salinity drought
model_plotter(m_sal_reg_d, salinitydata_regional)
#`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
#stick with non transformed salinity

#check results regional salinity drought
m_sal_reg_d_Anova<-Anova(m_sal_reg_d, type=2)
m_sal_reg_d_Anova

#Anova Table (Type II tests) regional salinity drought
#
#Response: Salinity
#Sum Sq  Df F value    Pr(>F)    
#Drought    176.89   2  50.688 < 2.2e-16 ***
#  Region    1663.16   4 238.294 < 2.2e-16 ***
#  Residuals  380.38 218                      
#---
#  Signif. codes:  
#  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#post-hoc test regional salinity drought
p_m_sal_reg_d<-tukey_plotter(m_sal_reg_d, salinitydata_regional, "Region", "Drought")
ggsave(
  plot = p_m_sal_reg_d,
  filename = "Water_Quality/Sal_region_drought_model.png",
  height = 7,
  width = 6,
  units = "in"
)

p_m_sal_reg_d

#Year

m_sal_reg_y<-aov(Salinity ~ Year_fac + Region, data=salinitydata_regional)

#check assumptions regional salinity year
model_plotter(m_sal_reg_y, salinitydata_regional)
#`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
#stick with non transformed salinity, although there is clearly a pattern in residuals

#check results regional salinity year
m_sal_reg_y_Anova<-Anova(m_sal_reg_y, type=2)
m_sal_reg_y_Anova

#Anova Table (Type II tests) regional salinity year

#Response: Salinity
#Sum Sq  Df  F value    Pr(>F)    
#Year_fac   249.43  46   3.0649 6.868e-08 ***
#  Region    1658.59   4 234.3702 < 2.2e-16 ***
#  Residuals  307.84 174                       
#---
#  Signif. codes:  
#  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#post hoc test regional salinity year
p_m_sal_reg_y<-tukey_plotter(m_sal_reg_y, salinitydata_regional, "Region", "Year_fac")
ggsave(
  plot = p_m_sal_reg_y,
  filename = "Water_Quality/Sal_region_year_model.png",
  height = 10,
  width = 9,
  units = "in"
)

p_m_sal_reg_y

#save regional salinity anova outputs
anovas<-bind_rows(
  mutate(as_tibble(m_sal_reg_d_Anova, rownames = "Parameter"), model="Regional_Drought"),
  mutate(as_tibble(m_sal_reg_y_Anova, rownames = "Parameter"), model="Regional_Year")
)%>%
  mutate(`Pr(>F)`=if_else(`Pr(>F)`<0.001, "< 0.001", as.character(round(`Pr(>F)`, 4))))%>%
  write_csv("Water_Quality/Sal_reg_anovas.csv")


# Compare 2021 salinity to prior years

raw_data<-DroughtData::raw_wq_1975_2021%>%
  filter(!is.na(Salinity))%>%
  left_join(salinitydata_regional%>%
              distinct(YearAdj, SVIndex, YearType, Drought),
            by="YearAdj")%>%
  mutate(across(c(Drought, YearType), list(`20_21`=~case_when(YearAdj==2021 ~ "2021", 
                                                              YearAdj==2020 ~ "2020", 
                                                              TRUE ~ as.character(.x)))),
         across(c(YearType, YearType_20_21), ~factor(.x, levels=c("2020", "2021", "Critical", "Dry", "Below Normal", "Above Normal", "Wet"))),
         Region=factor(Region, levels=c("Suisun Marsh", "Suisun Bay", "Confluence", "SouthCentral", "North")),
         Season=factor(Season, levels=c("Winter", "Spring", "Summer", "Fall")))

#How does 2021 salinity compare to Drought, Normal, and Wet periods?
p_2021_d<-ggplot(raw_data, aes(x=Drought_20_21, y=Salinity, fill=Drought))+
  geom_boxplot()+
  drt_color_pal_drought()+
  xlab("Drought")+
  ylab("Salinity (PSU)")+
  theme_bw()

ggsave(
  plot = p_2021_d,
  filename = "Water_Quality/Sal_drought_20_21.png",
  height = 4,
  width = 5,
  units = "in"
)

p_2021_d

#Does that change regionally or seasonally?
p_2021_d_rs<-ggplot(raw_data, aes(x=Drought_20_21, y=Salinity, fill=Drought))+
  geom_boxplot()+
  drt_color_pal_drought()+
  facet_grid(Season~Region, scales = "free_y")+
  xlab("Drought")+
  ylab("Salinity (PSU)")+
  theme_bw()+
  theme(axis.text.x=element_text(angle = 45, hjust=1))
ggsave(plot=p_2021_d_rs, filename=file.path(save_dir, "Sal_drought_rs_20_21.png"), device="png", height=8, width=10, units="in")
p_2021_d_rs

#How does 2021 compare to each water year type?
p_2021_yt<-ggplot(raw_data, aes(x=YearType_20_21, y=Salinity, fill=YearType))+
  geom_boxplot()+
  drt_color_pal_yrtype()+
  xlab("Year type")+
  ylab("Salinity (PSU)")+
  theme_bw()+
  theme(axis.text.x=element_text(angle = 45, hjust=1))
ggsave(plot=p_2021_yt, filename=file.path(save_dir, "Sal_yeartype_20_21.png"), device="png", height=4, width=6, units="in")
p_2021_yt

#Does that change regionally or seasonally?
p_2021_yt_rs<-ggplot(raw_data, aes(x=YearType_20_21, y=Salinity, fill=YearType))+
  geom_boxplot()+
  drt_color_pal_yrtype()+
  facet_grid(Season~Region, scales = "free_y")+
  xlab("Year type")+
  ylab("Salinity (PSU)")+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45, hjust=1))
ggsave(plot=p_2021_yt_rs, filename=file.path(save_dir, "Sal_yeartype_rs_20_21.png"), device="png", height=8, width=12, units="in")
p_2021_yt_rs
