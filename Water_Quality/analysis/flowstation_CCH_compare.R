
#short term analysis (2010 - 2021) to compare dayflow outflow and Cache at Ryer station
#must run compare_USGS_Q_to_dayflow script to generate DTO_comp_seasonal data frame

library(dplyr)
library(DroughtData)
library(ggplot2)
library(patchwork)
library(knitr)
library(car)
library(multcomp)
library(emmeans)
library(stringr)
library(readr)

save_dir<-file.path("C:/Users/estumpne/Documents/R/DroughtData-master_0_4/DroughtData-master/output")

#load data - notice CCH identifier gets changed to RYI for plots

seasonal_Q<-DTO_comp_seasonal%>%filter(!is.na(Q_CCH))%>%mutate(Season=factor(Season, levels=c("Winter", "Spring", "Summer", "Fall")),YearAdj=factor(YearAdj),Drought=factor(Drought, levels=c("W", "N", "D")))

seasonal_Q$log_offset_CCH <- log(seasonal_Q$Q_CCH + abs(min(seasonal_Q$Q_CCH))+1)

#load Sam's function for year + season analysis



model_plotter_CCH<-function(model, data){
  data<-data%>%
    mutate(Residuals=resid(model),
           Fitted=predict(model))

  p_hist<-ggplot(data, aes(x=Residuals))+
    geom_histogram()+
    xlab("CCH Q (cfs)")+
    theme_bw()

  p_res_fit<-ggplot(data, aes(x=Residuals, y=Fitted))+
    geom_point()+
    ylab("Predicted CCH Q (cfs)")+
    xlab("Residuals CCH Q (cfs)")+
    theme_bw()

  p_obs_fit<-ggplot(data, aes(x=log_offset_CCH, y=Fitted))+
    geom_point()+
    geom_abline(slope=1, intercept=0, color="red")+
    ylab("Predicted CCH Q (cfs)")+
    xlab("Residuals CCH Q (cfs)")+
    theme_bw()

  out<-(p_hist+plot_layout(ncol=1))+(p_res_fit+p_obs_fit+plot_layout(ncol=2))+plot_layout(nrow=2, widths=c(1, 0.5, 0.5))

  return(out)
}

#load Sam's tukey function for Q

tukey_plotter_CCH_yr<-function(model, data, data_type, model_type){

  tuk<-emmeans(model, list(data=data_type, model=model_type))

  tuk_data<-as_tibble(cld(tuk$data, sort=FALSE, Letters = letters))%>%
    mutate(.group=str_remove_all(.group, fixed(" ")))%>%
    left_join(data%>%
                group_by(across(all_of(data_type)))%>%
                summarise(max_Q=max(log_offset_CCH), .groups="drop"),
              by=data_type)

  tuk_model<-as_tibble(cld(tuk$model, sort=FALSE, Letters = letters))%>%
    mutate(.group=str_remove_all(.group, fixed(" ")))%>%
    left_join(data%>%
                group_by(across(all_of(model_type)))%>%
                summarise(max_Q=max(log_offset_CCH), .groups="drop"),
              by=model_type)

  p_data<-ggplot(tuk_data, aes(x=.data[[data_type]], y=emmean, ymin=lower.CL, ymax=upper.CL, label=.group))+
    geom_boxplot(data=data, aes(x=.data[[data_type]], y=log_offset_CCH), inherit.aes = FALSE)+
    geom_pointrange(color="red", position=position_nudge(x=0.1))+
    geom_text(aes(y=max_Q+(max(data$log_offset_CCH)-min(data$log_offset_CCH))/20), size=6)+
    ylab("log(RYI_Q)")+
    theme_bw(base_size=16)

  p_model<-ggplot(tuk_model, aes(x=.data[[model_type]], y=emmean, ymin=lower.CL, ymax=upper.CL, label=.group))+
    geom_boxplot(data=data, aes(x=.data[[model_type]], y=log_offset_CCH), inherit.aes = FALSE)+
    geom_pointrange(color="red", position=position_nudge(x=0.1))+
    geom_text(aes(y=max_Q+(max(data$log_offset_CCH)-min(data$log_offset_CCH))/20), angle=if_else(model_type=="YearAdj", 90, 0), hjust=if_else(model_type=="YearAdj", "left", NA_character_), vjust=0.25, size=6)+
    ylab("log(RYI_Q)")+
    theme_bw(base_size=16)+
    {if(model_type=="YearAdj"){
      list(geom_tile(data=data,
                     aes(x=YearAdj, y=min(log_offset_CCH)-(max(log_offset_CCH)-min(log_offset_CCH))/20,
                         fill=Drought, height=(max(log_offset_CCH)-min(log_offset_CCH))/20),
                     inherit.aes = FALSE),
           xlab("Year"),
           theme(axis.text.x=element_text(angle=90, vjust=0.5)),
           scale_y_continuous(expand = expansion(mult=c(0,0.1))),
           drt_color_pal_drought())
    }}

  out<-p_data/p_model+plot_annotation(tag_levels="A")

  if(model_type=="YearAdj"){
    out<-out+plot_layout(heights = c(0.8, 1))
  }

  return(out)
}

#Parameter: USGS CCH_Q
#ANOVA:  Metric ~ Drought/Wet + Season

#Data – seasonal averages from 2011-2021

#ANOVA:  Metric ~ factor(Year) + Season

#Data – seasonal averages from 2011-2021

#Plot seasonal Q by year

ggplot(seasonal_Q, aes(x=YearAdj, y=log_offset_CCH, fill=Drought))+
  geom_point(shape=21, color="black")+
  facet_wrap(~Season, scales="free")+
  drt_color_pal_drought()+
  ylab("Q")+
  theme_bw()

#Plot seasonal Outflow by Drought Index

ggplot(seasonal_Q, aes(x=Season, y=log_offset_CCH, fill=Drought))+
  geom_boxplot()+
  facet_wrap(~Season, scales="free")+
  drt_color_pal_drought()+
  ylab("log(CCH Q + offset)")+
  theme_bw()

# ANOVA for Q by YearAdj + Season

m_CCH_year <- aov(log_offset_CCH ~ factor(YearAdj) + Season, data = seasonal_Q)

#Estimated effects may be unbalanced with factor(YearAdj)*Season
m_CCH_year

m_CCH_year_Anova <- Anova(m_CCH_year, type=2)

m_CCH_year_Anova

# check residuals

#hist(resid(m_out_year))

#fitted values of the model

data.fit = fitted(m_CCH_year)

#residuals of the model

data.res = resid(m_CCH_year)

#look at residuals

hist(data.res)
hist(data.fit)

data.stdres = rstandard(m_CCH_year)

#make qq plot and add line. first obtain normal prob plot for standardized resids

data.stdres = rstandard(m_CCH_year)

qqnorm(data.stdres)

qqline(data.stdres)

#check assumptions using Sam's function

#run model plotter on line 28 again and then return to line 167

model_plotter_out(m_CCH_year, seasonal_Q)

#anova table

summary(m_CCH_year)

#post - hoc using Sam's tukey function

CCH_year_tukey <-tukey_plotter_CCH_yr(m_CCH_year, seasonal_Q, "Season", "YearAdj")

CCH_year_tukey

ggsave(plot=CCH_year_tukey, filename=file.path(save_dir, "log_RYI_season_year_model.png"), device="png", height=12, width=15, units="in")

# ANOVA for X2 by Drought + Season

m_CCH_dr <- aov(log_offset_CCH ~ Drought + Season, data = seasonal_Q)

summary(m_CCH_dr)

m_CCH_dr_Anova <- Anova(m_CCH_dr, type = 2)

# check residuals

#hist(resid(ex_year))

#fitted values of the model

data.fit = fitted(m_CCH_dr)

#residuals of the model

data.res = resid(m_CCH_dr)

#look at residuals

hist(data.res)
hist(data.fit)

data.stdres = rstandard(m_CCH_dr)

#make qq plot and add line. first obtain normal prob plot for standardized resids

data.stdres = rstandard(m_CCH_dr)

qqnorm(data.stdres)

qqline(data.stdres)

#check assumptions using Sam's function

model_plotter_CCH(m_CCH_dr, seasonal_Q)

#anova table

summary(m_CCH_dr)

#post - hoc using Sam's tukey function

CCH_drought_tukey <-tukey_plotter_CCH_yr(m_CCH_dr, seasonal_Q, "Season", "Drought")

CCH_drought_tukey

ggsave(plot=CCH_drought_tukey, filename=file.path(save_dir, "log_RYI_season_drought_model.png"), device="png", height=12, width=15, units="in")

#save all Anova output
anovas<-bind_rows(
  mutate(as_tibble(m_CCH_year_Anova, rownames = "Parameter"), model="Year_Season"),
  mutate(as_tibble(m_CCH_dr_Anova, rownames = "Parameter"), model="Season_Drought")
)%>%
  mutate(`Pr(>F)`=if_else(`Pr(>F)`<0.001, "< 0.001", as.character(round(`Pr(>F)`, 4))))%>%
  write_csv(file.path(save_dir, "RYI_Q_anovas.csv"))

#save all Anova output
anovas<-bind_rows(
  mutate(as_tibble(m_CCH_year_Anova, rownames = "Parameter"), model="Year_Season"),
  mutate(as_tibble(m_CCH_dr_Anova, rownames = "Parameter"), model="Season_Drought")
)

anovas$Metric <- c('RYI_Q')

anovas <- anovas %>% relocate(Metric, .before = Parameter)%>%
  mutate(`Pr(>F)`=if_else(`Pr(>F)`<0.001, "< 0.001", as.character(round(`Pr(>F)`, 4))))%>%
  write_csv(file.path(save_dir, "RYI_Q_anovas.csv"))

#compare 2021 to prior years
#load data
#raw_out <- raw_hydro_1975_2021

#raw_out$log_Outflow = log(raw_out$Outflow)

#adding drought_20_21 and yeartype_20_21 columns

CCH<-seasonal_Q%>%
  filter(!is.na(log_offset_CCH))%>%
  #left_join(lt_regional%>%distinct(YearAdj, SVIndex, YearType, Drought),by="YearAdj")%>%
  mutate(across(c(Drought, YearType), list(`20_21`=~case_when(YearAdj==2021 ~ "2021",YearAdj==2020 ~ "2020",TRUE ~ as.character(.x)))),across(c(YearType, YearType_20_21), ~factor(.x, levels=c("2020", "2021", "Critical", "Dry", "Below Normal", "Above Normal", "Wet"))),Season=factor(Season, levels=c("Winter", "Spring", "Summer", "Fall")))

# graph how 2021 compares to Drought, Normal, and Wet periods?

CCH_Q<-ggplot(CCH, aes(x=Drought_20_21, y=log_offset_CCH, fill=Drought))+geom_boxplot()+drt_color_pal_drought()+xlab("Drought")+ylab("log_offset_CCH")+theme_bw()

CCH_Q

ggsave(plot=CCH_Q, filename=file.path(save_dir, "log_CCH_drought_20_21.png"), device="png", height=4, width=5, units="in")




