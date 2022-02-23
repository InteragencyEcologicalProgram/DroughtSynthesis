#install drought synthesis package

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

seasonal_ex<-lt_seasonal%>%filter(!is.na(Export))%>%mutate(Season=factor(Season, levels=c("Winter", "Spring", "Summer", "Fall")), YearAdj=factor(YearAdj),Drought=factor(Drought, levels=c("D", "N", "W")))

#load Sam's function for year + season analysis

model_plotter_export<-function(model, data){
  data<-data%>%
    mutate(Residuals=resid(model),
           Fitted=predict(model))

  p_hist<-ggplot(data, aes(x=Residuals))+
    geom_histogram()+
    xlab("Residuals ")+
    theme_bw()

  p_res_fit<-ggplot(data, aes(x=Residuals, y=Fitted))+
    geom_point()+
    ylab("Predicted Export ")+
    xlab("Residuals ")+
    theme_bw()

  p_obs_fit<-ggplot(data, aes(x=Export, y=Fitted))+
    geom_point()+
    geom_abline(slope=1, intercept=0, color="red")+
    ylab("Predicted Export ")+
    xlab("Observed Export ")+
    theme_bw()

  out<-(p_hist+plot_layout(ncol=1))+(p_res_fit+p_obs_fit+plot_layout(ncol=2))+plot_layout(nrow=2, widths=c(1, 0.5, 0.5))

  return(out)
}

#load Sam's tukey function for Export


tukey_plotter_ex_yr<-function(model, data, data_type, model_type){

  tuk<-emmeans(model, list(data=data_type, model=model_type))

  tuk_data<-as_tibble(cld(tuk$data, sort=FALSE, Letters = letters))%>%
    mutate(.group=str_remove_all(.group, fixed(" ")))%>%
    left_join(data%>%
                group_by(across(all_of(data_type)))%>%
                summarise(max_export=max(Export), .groups="drop"),
              by=data_type)

  tuk_model<-as_tibble(cld(tuk$model, sort=FALSE, Letters = letters))%>%
    mutate(.group=str_remove_all(.group, fixed(" ")))%>%
    left_join(data%>%
                group_by(across(all_of(model_type)))%>%
                summarise(max_export=max(Export), .groups="drop"),
              by=model_type)

  p_data<-ggplot(tuk_data, aes(x=.data[[data_type]], y=emmean, ymin=lower.CL, ymax=upper.CL, label=.group))+
    geom_boxplot(data=data, aes(x=.data[[data_type]], y=Export), inherit.aes = FALSE)+
    geom_pointrange(color="red", position=position_nudge(x=0.1), size = 0.3)+
    geom_text(aes(y=max_export+(max(data$Export)-min(data$Export))/20))+
    ylab("Export (cfs)")+
    theme_bw()

  p_model<-ggplot(tuk_model, aes(x=.data[[model_type]], y=emmean, ymin=lower.CL, ymax=upper.CL, label=.group))+
    geom_boxplot(data=data, aes(x=.data[[model_type]], y=Export), inherit.aes = FALSE)+
    geom_pointrange(color="red", position=position_nudge(x=0.1), size = 0.3)+
    geom_text(aes(y=max_export+(max(data$Export)-min(data$Export))/20), angle=if_else(model_type=="YearAdj", 90, 0), hjust=if_else(model_type=="YearAdj", "left", NA_character_), vjust=0.25)+
    ylab("Export (cfs)")+
    theme_bw()+
    {if(model_type=="YearAdj"){
      list(geom_tile(data=data,
                     aes(x=YearAdj, y=min(Export)-(max(Export)-min(Export))/20,
                         fill=Drought, height=(max(Export)-min(Export))/20),
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

#Parameters: Delta Exports

#ANOVA:  Metric ~ factor(Year) + Season

#Data – seasonal averages from 1975-2021

#Plot seasonal Export by year

ggplot(seasonal_ex, aes(x=YearAdj, y=Export, fill=Drought))+
  geom_point(shape=21, color="black")+
  facet_wrap(~Season, scales="free")+
  drt_color_pal_drought()+
  ylab("Export")+
  theme_bw()

#Plot seasonal Export by Drought Index

ggplot(seasonal_ex, aes(x=Drought, y=Export, fill=Drought))+
  geom_boxplot()+
  facet_wrap(~Season, scales="free")+
  drt_color_pal_drought()+
  ylab("Export")+
  theme_bw()

# ANOVA for Export by Year + Season

m_ex_year <- aov(Export ~ factor(YearAdj) + Season, data = seasonal_ex)

m_ex_year_Anova <- Anova(m_ex_year, type=2)

m_ex_year_Anova

# check residuals

#hist(resid(m_ex_year))

#fitted values of the model

data.fit = fitted(m_ex_year)

#residuals of the model

data.res = resid(m_ex_year)

#look at residuals

hist(data.res)
hist(data.fit)

data.stdres = rstandard(m_ex_year)

#make qq plot and add line. first obtain normal prob plot for standardized resids

data.stdres = rstandard(m_ex_year)

qqnorm(data.stdres)

qqline(data.stdres)

#check assumptions using Sam's function

model_plotter_export(m_ex_year, seasonal_ex)

#anova table

summary(m_ex_year)

#post - hoc using Sam's tukey function

ex_year_tukey <-tukey_plotter_ex_yr(m_ex_year, seasonal_ex, "Season", "YearAdj")

ex_year_tukey

ggsave(
  plot = ex_year_tukey,
  filename = file.path(save_dir, "Exp_season_year_model.png"),
  height = 10,
  width = 9,
  units = "in"
)

# ANOVA for Export by Drought + Season

#ANOVA:  Metric ~ Drought/Wet + Season

#Data – seasonal averages from 1975-2021

#functions specific for export - drought analysis

#load Sam's function for year + season analysis

#load Sam's tukey function for Export

m_ex_dr <- aov(Export ~ Drought + Season, data = seasonal_ex)

m_ex_dr_Anova <- Anova(m_ex_dr, type=2)

m_ex_dr_Anova

# check residuals

#hist(resid(ex_year))

#fitted values of the model

data.fit = fitted(m_ex_dr)

#residuals of the model

data.res = resid(m_ex_dr)

#look at residuals

hist(data.res)
hist(data.fit)

data.stdres = rstandard(m_ex_dr)

#make qq plot and add line. first obtain normal prob plot for standardized resids

data.stdres = rstandard(m_ex_dr)

qqnorm(data.stdres)

qqline(data.stdres)

#check assumptions using Sam's function

model_plotter_export(m_ex_dr, seasonal_ex)

#anova table

summary(m_ex_dr)

#post - hoc using Sam's tukey function

ex_drought_tukey <-tukey_plotter_ex_yr(m_ex_dr, seasonal_ex, "Season", "Drought")

ex_drought_tukey

ggsave(
  plot = ex_drought_tukey,
  filename = file.path(save_dir, "Exp_season_drought_model.png"),
  height = 7,
  width = 6,
  units = "in"
)

#save all Anova output
anovas<-bind_rows(
  mutate(as_tibble(m_ex_year_Anova, rownames = "Parameter"), model="Year_Season"),
  mutate(as_tibble(m_ex_dr_Anova, rownames = "Parameter"), model="Season_Drought")
)

anovas$Metric <- c('Export')

anovas <- anovas %>% relocate(Metric, .before = Parameter)%>%
  mutate(`Pr(>F)`=if_else(`Pr(>F)`<0.001, "< 0.001", as.character(round(`Pr(>F)`, 4))))%>%
  write_csv(file.path(save_dir, "Export_anovas.csv"))

#compare 2021 export to prior years

#load data

raw_ex <- raw_hydro_1975_2021 %>% filter(!is.na(Export)) %>% left_join(lt_seasonal %>% distinct(YearAdj, SVIndex, YearType, Drought),by="YearAdj") %>% mutate(across(c(Drought, YearType), list(`20_21`=~case_when(YearAdj==2021 ~ "2021",YearAdj==2020 ~ "2020",TRUE ~ as.character(.x)))),across(c(YearType, YearType_20_21), ~factor(.x, levels=c("2020", "2021", "Critical", "Dry", "Below Normal", "Above Normal", "Wet"))),Season=factor(Season, levels=c("Winter", "Spring", "Summer", "Fall")))

# graph how 2021 export compares to Drought, Normal, and Wet periods?

ex_2021_d <- ggplot(raw_ex, aes(x = Drought_20_21, y = Export, fill = Drought)) +
  geom_boxplot() +
  drt_color_pal_drought() +
  xlab("Drought") +
  ylab("Export (cfs)") +
  theme_bw()

ex_2021_d

ggsave(
  plot = ex_2021_d,
  filename = file.path(save_dir, "Exp_drought_20_21.png"),
  height = 4,
  width = 5,
  units = "in"
)


#Does the export comparison of 2020 & 2021 to other water year types change seasonally?

ex_2021_d_s<-ggplot(raw_ex, aes(x=Drought_20_21, y=Export, fill=Drought))+geom_boxplot()+drt_color_pal_drought()+facet_grid(~Season, scales = "free_y")+xlab("Drought")+ylab("Export")+theme_bw(base_size = 16)+theme(axis.text.x=element_text(angle = 45, hjust=1))

ex_2021_d_s

ggsave(plot=ex_2021_d_s, filename=file.path(save_dir, "ex_drought_s_20_21.png"), device="png", height=8, width=10, units="in")


