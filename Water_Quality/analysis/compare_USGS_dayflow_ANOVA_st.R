#install D.Bos' drought synthesis package
#must run compare_USGS_Q_to_dayflow script to generate DTO_comp_seasonal data frame
#short term analysis (2010 - 2021) to compare dayflow outflow and USGS outflow
#USGS outflow begins on line 20, Dayflow outflow begins on line 265

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

#load data

seasonal_Q<-DTO_comp_seasonal%>%filter(!is.na(Q_USGS_tf))%>%mutate(Season=factor(Season, levels=c("Winter", "Spring", "Summer", "Fall")),YearAdj=factor(YearAdj),Drought=factor(Drought, levels=c("W", "N", "D")))

seasonal_Q$log_Q_USGS_tf = log(seasonal_Q$Q_USGS_tf)


#load Sam's function for year + season analysis

model_plotter_Q<-function(model, data){
  data<-data%>%
    mutate(Residuals=resid(model),
           Fitted=predict(model))

  p_hist<-ggplot(data, aes(x=Residuals))+
    geom_histogram()+
    xlab("USGS Q (cfs)")+
    theme_bw()

  p_res_fit<-ggplot(data, aes(x=Residuals, y=Fitted))+
    geom_point()+
    ylab("Predicted USGS Q (cfs)")+
    xlab("Residuals USGS Q (cfs)")+
    theme_bw()

  p_obs_fit<-ggplot(data, aes(x=log_Q_USGS_tf, y=Fitted))+
    geom_point()+
    geom_abline(slope=1, intercept=0, color="red")+
    ylab("Predicted USGS Q (cfs)")+
    xlab("Residuals USGS Q (cfs)")+
    theme_bw()

  out<-(p_hist+plot_layout(ncol=1))+(p_res_fit+p_obs_fit+plot_layout(ncol=2))+plot_layout(nrow=2, widths=c(1, 0.5, 0.5))

  return(out)
}

#load Sam's tukey function for Q


tukey_plotter_Q_yr<-function(model, data, data_type, model_type){

  tuk<-emmeans(model, list(data=data_type, model=model_type))

  tuk_data<-as_tibble(cld(tuk$data, sort=FALSE, Letters = letters))%>%
    mutate(.group=str_remove_all(.group, fixed(" ")))%>%
    left_join(data%>%
                group_by(across(all_of(data_type)))%>%
                summarise(max_Q=max(log_Q_USGS_tf), .groups="drop"),
              by=data_type)

  tuk_model<-as_tibble(cld(tuk$model, sort=FALSE, Letters = letters))%>%
    mutate(.group=str_remove_all(.group, fixed(" ")))%>%
    left_join(data%>%
                group_by(across(all_of(model_type)))%>%
                summarise(max_Q=max(log_Q_USGS_tf), .groups="drop"),
              by=model_type)

  p_data<-ggplot(tuk_data, aes(x=.data[[data_type]], y=emmean, ymin=lower.CL, ymax=upper.CL, label=.group))+
    geom_boxplot(data=data, aes(x=.data[[data_type]], y=log_Q_USGS_tf), inherit.aes = FALSE)+
    geom_pointrange(color="red", position=position_nudge(x=0.1))+
    geom_text(aes(y=max_Q+(max(data$log_Q_USGS_tf)-min(data$log_Q_USGS_tf))/20), size=6)+
    ylab("log(USGS_Q)")+
    theme_bw(base_size=16)

  p_model<-ggplot(tuk_model, aes(x=.data[[model_type]], y=emmean, ymin=lower.CL, ymax=upper.CL, label=.group))+
    geom_boxplot(data=data, aes(x=.data[[model_type]], y=log_Q_USGS_tf), inherit.aes = FALSE)+
    geom_pointrange(color="red", position=position_nudge(x=0.1))+
    geom_text(aes(y=max_Q+(max(data$log_Q_USGS_tf)-min(data$log_Q_USGS_tf))/20), angle=if_else(model_type=="YearAdj", 90, 0), hjust=if_else(model_type=="YearAdj", "left", NA_character_), vjust=0.25, size=6)+
    ylab("log(USGS_Q)")+
    theme_bw(base_size=16)+
    {if(model_type=="YearAdj"){
      list(geom_tile(data=data,
                     aes(x=YearAdj, y=min(log_Q_USGS_tf)-(max(log_Q_USGS_tf)-min(log_Q_USGS_tf))/20,
                         fill=Drought, height=(max(log_Q_USGS_tf)-min(log_Q_USGS_tf))/20),
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

#Parameter: USGS DTO (RVB+SJJ+DCH+TMS)
#ANOVA:  Metric ~ Drought/Wet + Season

#Data – seasonal averages from 2011-2021

#ANOVA:  Metric ~ factor(Year) + Season

#Data – seasonal averages from 2011-2021

#Plot seasonal Q by year

ggplot(seasonal_Q, aes(x=YearAdj, y=Q_USGS_tf, fill=Drought))+
  geom_point(shape=21, color="black")+
  facet_wrap(~Season, scales="free")+
  drt_color_pal_drought()+
  ylab("Q")+
  theme_bw()

#Plot seasonal Outflow by Drought Index

ggplot(seasonal_Q, aes(x=Season, y=Q_USGS_tf, fill=Drought))+
  geom_boxplot()+
  facet_wrap(~Season, scales="free")+
  drt_color_pal_drought()+
  ylab("Q")+
  theme_bw()

# ANOVA for Q by YearAdj + Season

m_Q_year <- aov(log_Q_USGS_tf ~ factor(YearAdj) + Season, data = seasonal_Q)

#Estimated effects may be unbalanced with factor(YearAdj)*Season
m_Q_year

m_Q_year_Anova <- Anova(m_Q_year, type=2)

m_Q_year_Anova

# check residuals

#hist(resid(m_out_year))

#fitted values of the model

data.fit = fitted(m_Q_year)

#residuals of the model

data.res = resid(m_Q_year)

#look at residuals

hist(data.res)
hist(data.fit)

data.stdres = rstandard(m_Q_year)

#make qq plot and add line. first obtain normal prob plot for standardized resids

data.stdres = rstandard(m_Q_year)

qqnorm(data.stdres)

qqline(data.stdres)

#check assumptions using Sam's function

#run model plotter on line 28 again and then return to line 167

model_plotter_out(m_Q_year, seasonal_Q)

#anova table

summary(m_Q_year)

#post - hoc using Sam's tukey function

Q_year_tukey <-tukey_plotter_Q_yr(m_Q_year, seasonal_Q, "Season", "YearAdj")

Q_year_tukey

ggsave(plot=Q_year_tukey, filename=file.path(save_dir, "Fig_39.png"), device="png", height=12, width=15, units="in")

m_out_dr <- aov(log_Q_USGS_tf ~ Drought + Season, data = seasonal_Q)

summary(m_out_dr)

m_out_dr_Anova <- Anova(m_out_dr, type = 2)
m_out_dr_Anova

# check residuals

#hist(resid(ex_year))

#fitted values of the model

data.fit = fitted(m_out_dr)

#residuals of the model

data.res = resid(m_out_dr)

#look at residuals

hist(data.res)
hist(data.fit)

data.stdres = rstandard(m_out_dr)

#make qq plot and add line. first obtain normal prob plot for standardized resids

data.stdres = rstandard(m_out_dr)

qqnorm(data.stdres)

qqline(data.stdres)

#check assumptions using Sam's function
#dev.off()
model_plotter_out(m_out_dr, seasonal_Q)

#Error in .Call.graphics(C_palette2, .Call(C_palette2, NULL)) :
  #invalid graphics state - RUN dev.off() then rerun line

#anova table

summary(m_out_dr)

#post - hoc using Sam's tukey function

out_drought_tukey <-tukey_plotter_Q_yr(m_out_dr, seasonal_Q, "Season", "Drought")

out_drought_tukey

ggsave(plot=out_drought_tukey, filename=file.path(save_dir, "Q_season_drought_model.png"), device="png", height=12, width=15, units="in")

#save all Anova output
anovas<-bind_rows(
  mutate(as_tibble(m_out_year_Anova, rownames = "Parameter"), model="Year_Season"),
  mutate(as_tibble(m_out_dr_Anova, rownames = "Parameter"), model="Season_Drought")
)%>%
  mutate(`Pr(>F)`=if_else(`Pr(>F)`<0.001, "< 0.001", as.character(round(`Pr(>F)`, 4))))%>%
  write_csv(file.path(save_dir, "USGS_Q_anovas.csv"))

#save all Anova output
anovas<-bind_rows(
  mutate(as_tibble(m_out_year_Anova, rownames = "Parameter"), model="Year_Season"),
  mutate(as_tibble(m_out_dr_Anova, rownames = "Parameter"), model="Season_Drought")
)

anovas$Metric <- c('log(USGS_Q)')

anovas <- anovas %>% relocate(Metric, .before = Parameter)%>%
  mutate(`Pr(>F)`=if_else(`Pr(>F)`<0.001, "< 0.001", as.character(round(`Pr(>F)`, 4))))%>%
  write_csv(file.path(save_dir, "USGS_Q_anovas.csv"))

#repeat for dayflow outflow short term

#load data

seasonal_Q<-DTO_comp_seasonal%>%filter(!is.na(Outflow))%>%mutate(Season=factor(Season, levels=c("Winter", "Spring", "Summer", "Fall")),YearAdj=factor(YearAdj),Drought=factor(Drought, levels=c("W", "N", "D")))

seasonal_Q$log_Outflow = log(seasonal_Q$Outflow)


#load Sam's function for year + season analysis

model_plotter_out<-function(model, data){
  data<-data%>%
    mutate(Residuals=resid(model),
           Fitted=predict(model))

  p_hist<-ggplot(data, aes(x=Residuals))+
    geom_histogram()+
    xlab("Outflow (cfs)")+
    theme_bw()

  p_res_fit<-ggplot(data, aes(x=Residuals, y=Fitted))+
    geom_point()+
    ylab("Predicted Outflow (cfs)")+
    xlab("Outflow (cfs)")+
    theme_bw()

  p_obs_fit<-ggplot(data, aes(x=log_Outflow, y=Fitted))+
    geom_point()+
    geom_abline(slope=1, intercept=0, color="red")+
    ylab("Predicted Outflow (cfs)")+
    xlab("Residuals Outflow (cfs)")+
    theme_bw()

  out<-(p_hist+plot_layout(ncol=1))+(p_res_fit+p_obs_fit+plot_layout(ncol=2))+plot_layout(nrow=2, widths=c(1, 0.5, 0.5))

  return(out)
}

#load Sam's tukey function for Q


tukey_plotter_out_yr<-function(model, data, data_type, model_type){

  tuk<-emmeans(model, list(data=data_type, model=model_type))

  tuk_data<-as_tibble(cld(tuk$data, sort=FALSE, Letters = letters))%>%
    mutate(.group=str_remove_all(.group, fixed(" ")))%>%
    left_join(data%>%
                group_by(across(all_of(data_type)))%>%
                summarise(max_Q=max(log_Outflow), .groups="drop"),
              by=data_type)

  tuk_model<-as_tibble(cld(tuk$model, sort=FALSE, Letters = letters))%>%
    mutate(.group=str_remove_all(.group, fixed(" ")))%>%
    left_join(data%>%
                group_by(across(all_of(model_type)))%>%
                summarise(max_Q=max(log_Outflow), .groups="drop"),
              by=model_type)

  p_data<-ggplot(tuk_data, aes(x=.data[[data_type]], y=emmean, ymin=lower.CL, ymax=upper.CL, label=.group))+
    geom_boxplot(data=data, aes(x=.data[[data_type]], y=log_Outflow), inherit.aes = FALSE)+
    geom_pointrange(color="red", position=position_nudge(x=0.1))+
    geom_text(aes(y=max_Q+(max(data$log_Outflow)-min(data$log_Outflow))/20), size=6)+
    ylab("log(USGS_Q)")+
    theme_bw(base_size=16)

  p_model<-ggplot(tuk_model, aes(x=.data[[model_type]], y=emmean, ymin=lower.CL, ymax=upper.CL, label=.group))+
    geom_boxplot(data=data, aes(x=.data[[model_type]], y=log_Outflow), inherit.aes = FALSE)+
    geom_pointrange(color="red", position=position_nudge(x=0.1))+
    geom_text(aes(y=max_Q+(max(data$log_Outflow)-min(data$log_Outflow))/20), angle=if_else(model_type=="YearAdj", 90, 0), hjust=if_else(model_type=="YearAdj", "left", NA_character_), vjust=0.25, size=6)+
    ylab("log(USGS_Q)")+
    theme_bw(base_size=16)+
    {if(model_type=="YearAdj"){
      list(geom_tile(data=data,
                     aes(x=YearAdj, y=min(log_Outflow)-(max(log_Outflow)-min(log_Outflow))/20,
                         fill=Drought, height=(max(log_Outflow)-min(log_Outflow))/20),
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

#Parameter: USGS DTO (RVB+SJJ+DCH+TMS)
#ANOVA:  Metric ~ Drought/Wet + Season

#Data – seasonal averages from 2011-2021

#ANOVA:  Metric ~ factor(Year) + Season

#Data – seasonal averages from 2011-2021

#Plot seasonal Q by year

ggplot(seasonal_Q, aes(x=YearAdj, y=log_Outflow, fill=Drought))+
  geom_point(shape=21, color="black")+
  facet_wrap(~Season, scales="free")+
  drt_color_pal_drought()+
  ylab("Q")+
  theme_bw()

#Plot seasonal Outflow by Drought Index

ggplot(seasonal_Q, aes(x=Season, y=log_Outflow, fill=Drought))+
  geom_boxplot()+
  facet_wrap(~Season, scales="free")+
  drt_color_pal_drought()+
  ylab("Q")+
  theme_bw()

# ANOVA for Q by YearAdj + Season

m_Q_year <- aov(log_Outflow ~ factor(YearAdj) + Season, data = seasonal_Q)

#Estimated effects may be unbalanced with factor(YearAdj)*Season
m_Q_year

m_Q_year_Anova <- Anova(m_Q_year, type=2)

m_Q_year_Anova

# check residuals

#hist(resid(m_out_year))

#fitted values of the model

data.fit = fitted(m_Q_year)

#residuals of the model

data.res = resid(m_Q_year)

#look at residuals

hist(data.res)
hist(data.fit)

data.stdres = rstandard(m_Q_year)

#make qq plot and add line. first obtain normal prob plot for standardized resids

data.stdres = rstandard(m_Q_year)

qqnorm(data.stdres)

qqline(data.stdres)

#check assumptions using Sam's function

model_plotter_out(m_Q_year, seasonal_Q)

#anova table

summary(m_Q_year)

#post - hoc using Sam's tukey function

Q_year_tukey <-tukey_plotter_Q_yr(m_Q_year, seasonal_Q, "Season", "YearAdj")

Q_year_tukey

ggsave(plot=Q_year_tukey, filename=file.path(save_dir, "Outflow_st_tukey.png"), device="png", height=12, width=15, units="in")

m_out_dr <- aov(log_Outflow ~ Drought + Season, data = seasonal_Q)

summary(m_out_dr)

m_out_dr_Anova <- Anova(m_out_dr, type = 2)
m_out_dr_Anova

# check residuals

#hist(resid(ex_year))

#fitted values of the model

data.fit = fitted(m_out_dr)

#residuals of the model

data.res = resid(m_out_dr)

#look at residuals

hist(data.res)
hist(data.fit)

data.stdres = rstandard(m_out_dr)

#make qq plot and add line. first obtain normal prob plot for standardized resids

data.stdres = rstandard(m_out_dr)

qqnorm(data.stdres)

qqline(data.stdres)

#check assumptions using Sam's function
#dev.off()
model_plotter_out(m_out_dr, seasonal_Q)

#Error in .Call.graphics(C_palette2, .Call(C_palette2, NULL)) :
#invalid graphics state - RUN dev.off() then rerun line

#anova table

summary(m_out_dr)

#post - hoc using Sam's tukey function

out_drought_tukey <-tukey_plotter_out_yr(m_out_dr, seasonal_Q, "Season", "Drought")

out_drought_tukey

ggsave(plot=out_drought_tukey, filename=file.path(save_dir, "out_st_season_drought_model.png"), device="png", height=12, width=15, units="in")

#save all Anova output
anovas<-bind_rows(
  mutate(as_tibble(m_out_year_Anova, rownames = "Parameter"), model="Year_Season"),
  mutate(as_tibble(m_out_dr_Anova, rownames = "Parameter"), model="Season_Drought")
)%>%
  mutate(`Pr(>F)`=if_else(`Pr(>F)`<0.001, "< 0.001", as.character(round(`Pr(>F)`, 4))))%>%
  write_csv(file.path(save_dir, "Outflow_st_anovas.csv"))

#save all Anova output
anovas<-bind_rows(
  mutate(as_tibble(m_out_year_Anova, rownames = "Parameter"), model="Year_Season"),
  mutate(as_tibble(m_out_dr_Anova, rownames = "Parameter"), model="Season_Drought")
)

anovas$Metric <- c('log(Outflow)')

anovas <- anovas %>% relocate(Metric, .before = Parameter)%>%
  mutate(`Pr(>F)`=if_else(`Pr(>F)`<0.001, "< 0.001", as.character(round(`Pr(>F)`, 4))))%>%
  write_csv(file.path(save_dir, "Outflow_st_anovas.csv"))





