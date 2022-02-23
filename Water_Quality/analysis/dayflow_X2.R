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

seasonal_X2<-lt_seasonal%>%filter(!is.na(X2))%>%mutate(Season=factor(Season, levels=c("Winter", "Spring", "Summer", "Fall")),YearAdj=factor(YearAdj),Drought=factor(Drought, levels=c("D", "N", "W")))

#seasonal_X2 <- slice(seasonal_X2, 1:(n() - 3))

#load Sam's function for year + season analysis

model_plotter_X2<-function(model, data){
  data<-data%>%
    mutate(Residuals=resid(model),
           Fitted=predict(model))

  p_hist<-ggplot(data, aes(x=Residuals))+
    geom_histogram()+
    xlab("Residuals (km)")+
    theme_bw()

  p_res_fit<-ggplot(data, aes(x=Residuals, y=Fitted))+
    geom_point()+
    ylab("Predicted X2 (km)")+
    xlab("Residuals (km)")+
    theme_bw()

  p_obs_fit<-ggplot(data, aes(x=X2, y=Fitted))+
    geom_point()+
    geom_abline(slope=1, intercept=0, color="red")+
    ylab("Predicted X2 (km)")+
    xlab("Observed X2 (km)")+
    theme_bw()

  out<-(p_hist+plot_layout(ncol=1))+(p_res_fit+p_obs_fit+plot_layout(ncol=2))+plot_layout(nrow=2, widths=c(1, 0.5, 0.5))

  return(out)
}

#load Sam's tukey function for X2

tukey_plotter_X2<-function(model, data, data_type, model_type){

  tuk<-emmeans(model, list(data=data_type, model=model_type))

  tuk_data<-as_tibble(cld(tuk$data, sort=FALSE, Letters = letters))%>%
    mutate(.group=str_remove_all(.group, fixed(" ")))%>%
    left_join(data%>%
                group_by(across(all_of(data_type)))%>%
                summarise(max_x2=max(X2), .groups="drop"),
              by=data_type)

  tuk_model<-as_tibble(cld(tuk$model, sort=FALSE, Letters = letters))%>%
    mutate(.group=str_remove_all(.group, fixed(" ")))%>%
    left_join(data%>%
                group_by(across(all_of(model_type)))%>%
                summarise(max_x2=max(X2), .groups="drop"),
              by=model_type)

  p_data<-ggplot(tuk_data, aes(x=.data[[data_type]], y=emmean, ymin=lower.CL, ymax=upper.CL, label=.group))+
    geom_boxplot(data=data, aes(x=.data[[data_type]], y=X2), inherit.aes = FALSE)+
    geom_pointrange(color="red", position=position_nudge(x=0.1), size = 0.3)+
    geom_text(aes(y=max_x2+(max(data$X2)-min(data$X2))/20))+
    ylab("X2 (km)")+
    theme_bw()

  p_model<-ggplot(tuk_model, aes(x=.data[[model_type]], y=emmean, ymin=lower.CL, ymax=upper.CL, label=.group))+
    geom_boxplot(data=data, aes(x=.data[[model_type]], y=X2), inherit.aes = FALSE)+
    geom_pointrange(color="red", position=position_nudge(x=0.1), size = 0.3)+
    geom_text(aes(y=max_x2+(max(data$X2)-min(data$X2))/20), angle=if_else(model_type=="YearAdj", 90, 0), hjust=if_else(model_type=="YearAdj", "left", NA_character_), vjust=0.25)+
    ylab("X2 (km)")+
    theme_bw()+
    {if(model_type=="YearAdj"){
      list(geom_tile(data=data,
                     aes(x=YearAdj, y=min(X2)-(max(X2)-min(X2))/20,
                         fill=Drought, height=(max(X2)-min(X2))/20),
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
#Parameters: Delta Outflow, Delta X2s, X2

#ANOVA:  Metric ~ Drought/Wet + Season

#Data – seasonal averages from 1975-2021

#ANOVA:  Metric ~ factor(Year) + Season

#Data – seasonal averages from 1975-2021

#Plot seasonal X2 by year

ggplot(seasonal_X2, aes(x=YearAdj, y=X2, fill=Drought))+
  geom_point(shape=21, color="black")+
  facet_wrap(~Season, scales="free")+
  drt_color_pal_drought()+
  ylab("X2")+
  theme_bw()

#Plot seasonal X2 by Drought Index

ggplot(seasonal_X2, aes(x=Season, y=X2, fill=Drought))+
  geom_boxplot()+
  facet_wrap(~Season, scales="free")+
  drt_color_pal_drought()+
  ylab("X2")+
  theme_bw()

# ANOVA for X2 by Year + Season

m_X2_year <- aov(X2 ~ factor(YearAdj) + Season, data = seasonal_X2)

m_X2_year_Anova <- Anova(m_X2_year, type=2)

m_X2_year_Anova

# check residuals

#hist(resid(ex_year))

#fitted values of the model

data.fit = fitted(m_X2_year)

#residuals of the model

data.res = resid(m_X2_year)

#look at residuals

hist(data.res)
hist(data.fit)

data.stdres = rstandard(m_X2_year)

#make qq plot and add line. first obtain normal prob plot for standardized resids

data.stdres = rstandard(m_X2_year)

qqnorm(data.stdres)

qqline(data.stdres)

#check assumptions using Sam's function

#run model plotter on line 28 again and then return to line 167

model_plotter_X2(m_X2_year, seasonal_X2)

#anova table

summary(m_X2_year)

#post - hoc using Sam's tukey function

X2_year_tukey <-tukey_plotter_X2(m_X2_year, seasonal_X2, "Season", "YearAdj")

X2_year_tukey

ggsave(
  plot = X2_year_tukey,
  filename = file.path(save_dir, "X2_season_year_model.png"),
  height = 10,
  width = 9,
  units = "in"
)

# ANOVA for X2 by Drought + Season

m_X2_dr <- aov(X2 ~ Drought + Season, data = seasonal_X2)

m_X2_dr_Anova <- Anova(m_X2_dr, type=2)

m_X2_dr_Anova

# check residuals

#hist(resid(ex_year))

#fitted values of the model

data.fit = fitted(m_X2_dr)

#residuals of the model

data.res = resid(m_X2_dr)

#look at residuals

hist(data.res)
hist(data.fit)

data.stdres = rstandard(m_X2_dr)

#make qq plot and add line. first obtain normal prob plot for standardized resids

data.stdres = rstandard(m_X2_dr)

qqnorm(data.stdres)

qqline(data.stdres)

#check assumptions using Sam's function

model_plotter_X2(m_X2_dr, seasonal_X2)

#anova table

summary(m_X2_dr)

#post - hoc using Sam's tukey function

X2_drought_tukey <-tukey_plotter_X2(m_X2_dr, seasonal_X2, "Season", "Drought")

X2_drought_tukey

ggsave(
  plot = X2_drought_tukey,
  filename = file.path(save_dir, "X2_season_drought_model.png"),
  height = 7,
  width = 6,
  units = "in"
)

#save all Anova output
anovas<-bind_rows(
  mutate(as_tibble(m_X2_year_Anova, rownames = "Parameter"), model="Year_Season"),
  mutate(as_tibble(m_X2_dr_Anova, rownames = "Parameter"), model="Season_Drought")
)

anovas$Metric <- c('X2')

anovas <- anovas %>% relocate(Metric, .before = Parameter)%>%
  mutate(`Pr(>F)`=if_else(`Pr(>F)`<0.001, "< 0.001", as.character(round(`Pr(>F)`, 4))))%>%
  write_csv(file.path(save_dir, "X2_anovas.csv"))

#compare 2021 X2 to prior years

#load data

raw_X2 <- raw_hydro_1975_2021 %>% filter(!is.na(X2)) %>% left_join(lt_seasonal %>% distinct(YearAdj, SVIndex, YearType, Drought),by="YearAdj") %>% mutate(across(c(Drought, YearType), list(`20_21`=~case_when(YearAdj==2021 ~ "2021",YearAdj==2020 ~ "2020",TRUE ~ as.character(.x)))),across(c(YearType, YearType_20_21), ~factor(.x, levels=c("2020", "2021", "Critical", "Dry", "Below Normal", "Above Normal", "Wet"))),Season=factor(Season, levels=c("Winter", "Spring", "Summer", "Fall")))

# graph how 2021 X2 compares to Drought, Normal, and Wet periods?

X2_2021_d <- ggplot(raw_X2, aes(x = Drought_20_21, y = X2, fill = Drought)) +
  geom_boxplot() +
  drt_color_pal_drought() +
  xlab("Drought") +
  ylab("X2 (km)") +
  theme_bw()

X2_2021_d

ggsave(
  plot = X2_2021_d,
  filename = file.path(save_dir, "X2_drought_20_21.png"),
  height = 4,
  width = 5,
  units = "in"
)

#Does the X2 comparison of 2020 & 2021 to other water year types change seasonally?

X2_2021_d_s <- ggplot(raw_X2, aes(x = Drought_20_21, y = X2, fill = Drought)) +
  geom_boxplot() +
  drt_color_pal_drought() +
  facet_grid(~Season, scales = "free_y") +
  xlab("Drought") +
  ylab("X2 (km)") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

X2_2021_d_s

ggsave(
  plot = X2_2021_d_s,
  filename = file.path(save_dir, "X2_drought_20_21_seas.png"),
  height = 5.5,
  width = 6.5,
  units = "in"
)

