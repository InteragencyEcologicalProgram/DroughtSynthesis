#install D.Bos' drought synthesis package

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
library(scales)

# install.packages("devtools")
# devtools::install_github("mountaindboz/DroughtData")

# Define file path on the Drought Synthesis SharePoint for where to store figures and other output
drt_syn_abs_sp_path <- function(fp_rel = NULL) {
  fp_drt_syn <- "California Department of Water Resources/Drought Synthesis - Documents"
  
  if (is.null(fp_rel)) {
    fp_abs <- normalizePath(file.path(Sys.getenv('USERPROFILE'), fp_drt_syn))
  } else {
    fp_abs <- normalizePath(file.path(Sys.getenv('USERPROFILE'), fp_drt_syn, fp_rel))
  }
  
  return(fp_abs)
}

save_dir <- drt_syn_abs_sp_path("WQ_Team/Report_2022-02_Figures")

#load data

seasonal_out<-lt_seasonal%>%filter(!is.na(Outflow))%>%mutate(Season=factor(Season, levels=c("Winter", "Spring", "Summer", "Fall")),YearAdj=factor(YearAdj),Drought=factor(Drought, levels=c("D", "N", "W")))

seasonal_out$log_Outflow = log(seasonal_out$Outflow)

#load Sam's function for year + season analysis

model_plotter_out<-function(model, data){
  data<-data%>%
    mutate(Residuals=resid(model),
           Fitted=predict(model))

  p_hist<-ggplot(data, aes(x=Residuals))+
    geom_histogram()+
    xlab("log_Outflow (cfs)")+
    theme_bw()

  p_res_fit<-ggplot(data, aes(x=Residuals, y=Fitted))+
    geom_point()+
    ylab("Predicted log_Outflow (cfs)")+
    xlab("Residuals log_Outflow (cfs)")+
    theme_bw()

  p_obs_fit<-ggplot(data, aes(x=log_Outflow, y=Fitted))+
    geom_point()+
    geom_abline(slope=1, intercept=0, color="red")+
    ylab("Predicted log_Outflow (cfs)")+
    xlab("Observed log_Outflow (cfs)")+
    theme_bw()

  out<-(p_hist+plot_layout(ncol=1))+(p_res_fit+p_obs_fit+plot_layout(ncol=2))+plot_layout(nrow=2, widths=c(1, 0.5, 0.5))

  return(out)
}

#load Sam's tukey function for Outflow

tukey_plotter_out_yr<-function(model, data, data_type, model_type){

  tuk<-emmeans(model, list(data=data_type, model=model_type))

  tuk_data<-as_tibble(cld(tuk$data, sort=FALSE, Letters = letters))%>%
    mutate(.group=str_remove_all(.group, fixed(" ")))%>%
    left_join(data%>%
                group_by(across(all_of(data_type)))%>%
                summarise(max_out=max(log_Outflow), .groups="drop"),
              by=data_type)

  tuk_model<-as_tibble(cld(tuk$model, sort=FALSE, Letters = letters))%>%
    mutate(.group=str_remove_all(.group, fixed(" ")))%>%
    left_join(data%>%
                group_by(across(all_of(model_type)))%>%
                summarise(max_out=max(log_Outflow), .groups="drop"),
              by=model_type)

  p_data<-ggplot(tuk_data, aes(x=.data[[data_type]], y=emmean, ymin=lower.CL, ymax=upper.CL, label=.group))+
    geom_boxplot(data=data, aes(x=.data[[data_type]], y=log_Outflow), inherit.aes = FALSE)+
    geom_pointrange(color="red", position=position_nudge(x=0.1), size = 0.3)+
    geom_text(aes(y=max_out+(max(data$log_Outflow)-min(data$log_Outflow))/20))+
    ylab("log(Outflow [cfs])")+
    theme_bw()

  p_model<-ggplot(tuk_model, aes(x=.data[[model_type]], y=emmean, ymin=lower.CL, ymax=upper.CL, label=.group))+
    geom_boxplot(data=data, aes(x=.data[[model_type]], y=log_Outflow), inherit.aes = FALSE)+
    geom_pointrange(color="red", position=position_nudge(x=0.1), size = 0.3)+
    geom_text(aes(y=max_out+(max(data$log_Outflow)-min(data$log_Outflow))/20), angle=if_else(model_type=="YearAdj", 90, 0), hjust=if_else(model_type=="YearAdj", "left", NA_character_), vjust=0.25)+
    ylab("log(Outflow [cfs])")+
    theme_bw()+
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

#Parameter: Delta Outflow
#ANOVA:  Metric ~ Drought/Wet + Season

#Data – seasonal averages from 1975-2021

#ANOVA:  Metric ~ factor(Year) + Season

#Data – seasonal averages from 1975-2021

#Plot seasonal Outflow by year

ggplot(seasonal_out, aes(x=YearAdj, y=Outflow, fill=Drought))+
  geom_point(shape=21, color="black")+
  facet_wrap(~Season, scales="free")+
  drt_color_pal_drought()+
  ylab("Outflow (cfs)")+
  theme_bw()

#Plot seasonal Outflow by Drought Index

out_season_plot <- ggplot(seasonal_out, aes(x=Season, y=Outflow, fill=Drought))+
  geom_boxplot()+
  facet_wrap(~Season, scales="free")+
  drt_color_pal_drought()+
  ylab("Outflow (cfs)")+
  theme_bw()

out_season_plot

ggsave(plot=out_season_plot, filename=file.path(save_dir, "out_season_plot.png"), device="png", height=12, width=15, units="in")
# ANOVA for X2 by YearAdj + Season

m_out_year <- aov(log_Outflow ~ factor(YearAdj) + Season, data = seasonal_out)

#Estimated effects may be unbalanced with factor(YearAdj)*Season
m_out_year

m_out_year_Anova <- Anova(m_out_year, type=2)

m_out_year_Anova

# check residuals

#hist(resid(m_out_year))

#fitted values of the model

data.fit = fitted(m_out_year)

#residuals of the model

data.res = resid(m_out_year)

#look at residuals

hist(data.res)
hist(data.fit)

data.stdres = rstandard(m_out_year)

#make qq plot and add line. first obtain normal prob plot for standardized resids

data.stdres = rstandard(m_out_year)

qqnorm(data.stdres)

qqline(data.stdres)

#check assumptions using Sam's function

model_plotter_out(m_out_year, seasonal_out)

#anova table

summary(m_out_year)

#post - hoc using Sam's tukey function

out_year_tukey <-tukey_plotter_out_yr(m_out_year, seasonal_out, "Season", "YearAdj")

out_year_tukey

ggsave(
  plot = out_year_tukey,
  filename = file.path(save_dir, "LogOut_season_year_model.png"),
  height = 10,
  width = 9,
  units = "in"
)

# ANOVA for outflow by Drought + Season

m_out_dr <- aov(log_Outflow ~ Drought + Season, data = seasonal_out)

summary(m_out_dr)

m_out_dr_Anova <- Anova(m_out_dr, type = 2)

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
model_plotter_out(m_out_dr, seasonal_out)

#Error in .Call.graphics(C_palette2, .Call(C_palette2, NULL)) :
  #invalid graphics state - RUN dev.off() then rerun line

#anova table

summary(m_out_dr)

#post - hoc using Sam's tukey function

out_drought_tukey <-tukey_plotter_out_yr(m_out_dr, seasonal_out, "Season", "Drought")

out_drought_tukey

ggsave(
  plot = out_drought_tukey,
  filename = file.path(save_dir, "LogOut_season_drought_model.png"),
  height = 7,
  width = 6,
  units = "in"
)

#save all Anova output

anovas<-bind_rows(
  mutate(as_tibble(m_out_year_Anova, rownames = "Parameter"), model="Year_Season"),
  mutate(as_tibble(m_out_dr_Anova, rownames = "Parameter"), model="Season_Drought")
)%>%
  mutate(`Pr(>F)`=if_else(`Pr(>F)`<0.001, "< 0.001", as.character(round(`Pr(>F)`, 4))))%>%
  write_csv(file.path(save_dir, "Outflow_anovas.csv"))

#save all Anova output
anovas<-bind_rows(
  mutate(as_tibble(m_out_year_Anova, rownames = "Parameter"), model="Year_Season"),
  mutate(as_tibble(m_out_dr_Anova, rownames = "Parameter"), model="Season_Drought")
)

anovas$Metric <- c('log(Outflow)')

anovas <- anovas %>% relocate(Metric, .before = Parameter)%>%
  mutate(`Pr(>F)`=if_else(`Pr(>F)`<0.001, "< 0.001", as.character(round(`Pr(>F)`, 4))))%>%
  write_csv(file.path(save_dir, "Outflow_anovas.csv"))

#compare 2021 to prior years
#load data
#adding drought_20_21 and yeartype_20_21 columns
raw_out <- raw_hydro_1975_2021

raw_out_2<-raw_out%>%
  filter(!is.na(Outflow))%>%
  left_join(lt_seasonal%>%distinct(YearAdj, SVIndex, YearType, Drought),by="YearAdj")%>%
  mutate(across(c(Drought, YearType), list(`20_21`=~case_when(YearAdj==2021 ~ "2021",YearAdj==2020 ~ "2020",TRUE ~ as.character(.x)))),across(c(YearType, YearType_20_21), ~factor(.x, levels=c("2020", "2021", "Critical", "Dry", "Below Normal", "Above Normal", "Wet"))),Season=factor(Season, levels=c("Winter", "Spring", "Summer", "Fall")))

# graph how 2021 compares to Drought, Normal, and Wet periods?

out_2021_d <- ggplot(raw_out_2, aes(x = Drought_20_21, y = Outflow, fill = Drought)) +
  geom_boxplot() +
  drt_color_pal_drought() +
  xlab("Drought") +
  ylab("Outflow (cfs)") +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides = "l") +
  theme_bw() +
  theme(panel.grid.minor = element_blank())

out_2021_d

ggsave(
  plot = out_2021_d,
  filename = file.path(save_dir, "Out_drought_20_21.png"),
  height = 4,
  width = 5,
  units = "in"
)

#Does the comparison of 2020 & 2021 to other water year types change seasonally?

out_2021_d_s <- ggplot(raw_out_2, aes(x = Drought_20_21, y = Outflow, fill = Drought)) +
  geom_boxplot() +
  drt_color_pal_drought() +
  facet_grid(~Season, scales = "free_y") +
  xlab("Drought") +
  ylab("Outflow (cfs)") +
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks(sides = "l") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )

out_2021_d_s

ggsave(
  plot = out_2021_d_s,
  filename = file.path(save_dir, "Out_drought_20_21_seas.png"),
  height = 5.5,
  width = 6.5,
  units = "in"
)

