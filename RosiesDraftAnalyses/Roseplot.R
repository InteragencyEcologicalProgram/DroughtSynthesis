#test out a rose plot

library(tidyverse)
library(readxl)
library(viridis)

# testdat = read_excel("testpolarchart.xlsx")
# testdat$Metric = factor(testdat$Metric, levels = unique(testdat$Metric),
#                         labels = c("smelt population","salmon","silversides","salinity","temperature","outflow" , "weeds",       
#                                    "microcystis","phytoplankton", "contaminants", "smelt life history"))
# 
# ggplot(testdat, aes(x=Category, group = Metric)) +
#     geom_hline(yintercept = seq(0, 5, by = 1),
#              color = "grey", size = 1) +
#   geom_col(aes(fill = Uncertainty, y = length), position =position_dodge2(width = 1, preserve = "single"))+
#   geom_vline(xintercept = seq(.5, 16.5, by = 1),
#              color = "grey", size = 1) +
#   scale_fill_manual(values = c("red", "darkcyan", "orange", "grey"), labels = c("High", "Low", "Med", "????"))+
#   geom_text(aes(label = Metric, y = length), position = position_dodge(.9))+
#   
#  # annotate("text", x = rep("water", 5), y = c(.9, 1.9, 2.9, 3.9, 4.9), 
# #           label = c("no impact", "minor impact", "major impact", "management trigger", "ecosystem shift"), size = 3)+
#   coord_polar() + theme_bw()+
# scale_y_continuous( name = NULL)
# 
# 
# test2 = read.csv("RosiesDraftAnalyses/testdata.csv")
# test2 = filter(test2, !is.na(Year))
# test2w = group_by(test2, Drought) %>%
#   summarize(mpred = mean(Predators, na.rm = T), sdpred = sd(Predators, na.rm = T),
#             mzoops = mean(zoopBPUE, na.rm = T), sdzoops = sd(zoopBPUE),
#             mchla = mean(chla, na.rm = T), sdcla = sd(chla), mtemp = mean(temp))
# 
# test2$Drought = factor(test2$Drought, levels = c("D", "N", "W"), labels = c("Multi-Year \n Drought", "Neither", "Multi-Year \n Wet"))
# test3 = filter(test2, Drought !=  "Neither")
# ggplot(test2w, aes(x = Drought, y = mpred))+
#   geom_col() + geom_errorbar(aes(ymin = mpred -sdpred, ymax = mpred+sdpred))
# 
# ggplot(test2, aes(x = Drought, y = Predators, fill = Drought)) + geom_boxplot()
# ggplot(test2, aes(x = Drought, y = zoopBPUE)) + geom_boxplot()
# ggplot(test2, aes(x = Drought, y = FMWTIndex)) + geom_boxplot()
# ggplot(test2, aes(x = Drought, y = chla)) + geom_boxplot()
# 
# ggplot(test3, aes(x = Drought, y = Predators, fill = Drought)) + geom_boxplot()+
#   xlab(NULL)+ylab("Striped Bass Index (FMWT)") + theme_bw()
# ggplot(test3, aes(x = Drought, y = zoopBPUE, fill = Drought)) + geom_boxplot()+
#   ylab("Mean Zooplankton Biomass per cubic meter")+ theme_bw()
# ggplot(test3, aes(x = Drought, y = FMWTIndex, fill = Drought)) + geom_boxplot()+
#   ylab("Delta Smelt Index (FMWT)")+ theme_bw()
# ggplot(test3, aes(x = Drought, y = chla, fill = Drought)) + geom_boxplot()+
#   ylab("Mean Chlorophyll-a in ug/L")+ theme_bw()
# 
# ggplot(test3, aes(x = Drought, y = temp, fill = Drought)) + geom_boxplot()+
#   ylab("Water Temperature (C)")+ theme_bw()
# ggplot(test3, aes(x = Drought, y = secchi, fill = Drought)) + geom_boxplot()+
#   ylab("secchi depth (cm)")+ theme_bw()
# 
# 
# #Test some other things
# Nutrients <- read_csv("~/Drought/FLOATDrought/Analyses/Nutrients.csv")
# yeartypes <- read_csv("~/Drought/FLOATDrought/yeartypes.csv")
# Nuts = left_join(Nutrients, yeartypes)
# Nuts2 = group_by(Nuts, Year, Drought, Index) %>%
#   summarize(chla = mean(Chla, na.rm = T))
# 
# ggplot(Nuts2, aes(x = Year, y = chla, color = Drought)) + geom_line()
# ggplot(Nuts2, aes(x = Index, y = chla, color = Drought)) + geom_line()
# ggplot(Nuts2, aes(x = Drought, y = chla, fill = Drought)) + geom_boxplot()
# ggplot(filter(Nuts, Drought != "N", !is.na(Drought)), 
#        aes(x = Drought, y = log(Chla), fill = Drought)) + geom_boxplot() + facet_wrap(~Season)


######################################################
#This is a mess and I don't know what I'm doing but here goes
masterplot = read_excel("data/master plot.xlsx", sheet = "organized for data vis")
masterplot = masterplot %>%
  filter(Comparison != "Critical: Other year types") %>%
  mutate(Strength2 = case_when(
    direction == "Decrease" ~ Strength*-1,
    TRUE ~ Strength
  ), Group = factor(Group, levels = unique(Group))) %>%
  arrange(Group, Strength2) %>% 
  mutate(Metric = factor(Metric, levels = unique(Metric)))

data = masterplot

nObsType <- nlevels(as.factor(masterplot$Metric))
data$ID <- rep( seq(1, nrow(data)/nObsType) , each=nObsType)

# Get the name and the y position of each label
label_data <- data %>%
  mutate(ID = 1:nrow(data),
         Y = case_when(direction == "Decrease" ~0,
                       TRUE ~ Strength +1),
           Metric = case_when(
             Group == "Space1" ~ "",
             Group == "Space2"~ "",
             Group == "Space3"~ "",
             TRUE~ as.character(Metric)
           ))

number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$ID-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)

label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)
empty_bar = 2
# prepare a data frame for base lines
base_data <- data %>% 
  mutate(ID = 1:nrow(data)) %>%
  group_by(Group) %>% 
  summarize(start=min(ID), end=max(ID) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1

grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# Make the plot
p <- ggplot(data) +      
  
  # Add the stacked bar
  geom_bar(aes(x=Metric, y=Strength2,  alpha = Rsquared, fill = Strength2),  stat="identity") +
scale_fill_viridis()+
 # ylim(-150,max(label_data$ID, na.rm=T)) +
  theme_minimal() +
  theme(
    axis.line.x = element_line(size = 2),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  
  geom_hline(yintercept = 0)+
  coord_polar() +
  
  # Add labels on top of each bar
  geom_text(data=label_data, aes(x=ID, y=Y, label=Metric, hjust=hjust), 
            color="black", fontface="bold",alpha=0.6, size=5, angle= label_data$angle, inherit.aes = FALSE ) +
  annotate("text", x = 0, y =-10, label = " ")

p


p2 <- ggplot(data) +      
  
  # Add the stacked bar
  geom_bar(aes(x=Metric, y=Coefficient,  alpha = Rsquared, fill = Coefficient),  stat="identity") +
  scale_fill_viridis()+
  # ylim(-150,max(label_data$ID, na.rm=T)) +
  theme_minimal() +
  theme(
    axis.line.x = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    #plot.margin = unit(rep(-1,4), "cm") 
  ) +
  geom_hline(yintercept = 0)+
  coord_polar() +
  
  # Add labels on top of each bar
  geom_text(data=label_data, aes(x=ID, y=Coefficient, label=Metric, hjust=hjust), 
            color="black", fontface="bold",alpha=0.6, size=5, angle= label_data$angle, inherit.aes = FALSE ) +
  annotate("text", x = 0, y =-2, label = " ")

p2


p3 <- ggplot(filter(data, Metric != "Lower Trophic", Metric != "Water Quality")) +      
  
  # Add the stacked bar
  geom_bar(aes(x=Metric, y=Coefficient,  fill = Coefficient),  stat="identity") +
  scale_fill_viridis()+
  # ylim(-150,max(label_data$ID, na.rm=T)) +
  theme_minimal() +
  theme(
    legend.position =  NULL,
    axis.line.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank()
   # plot.margin = unit(rep(-1,4), "cm") 
  ) +
  
  # Add labels on top of each bar
  geom_text(data=label_data, aes(x=ID, y=0, label=Metric), hjust = 0,
            color="black", fontface="bold",alpha=0.6, size=4, angle= 90, inherit.aes = FALSE )

p3


# Make the plot
p4 <- ggplot(data) +      
  
  # Add the stacked bar
  geom_bar(aes(x=Metric, y=Strength,   fill = direction),  stat="identity") +
  scale_fill_manual(values = c("skyblue","darkorange"), 
                    labels = c("Decrease \nwith Drought", "Increase \nwith Drought"),name = NULL)+
  # ylim(-150,max(label_data$ID, na.rm=T)) +

  
  geom_hline(yintercept = 0)+
  coord_polar() +
  
  # Add labels on top of each bar
  geom_text(data=label_data, aes(x=ID, y=2, label=Metric, hjust=hjust), 
            color="black", fontface="bold",alpha=0.6, size=5, angle= label_data$angle, inherit.aes = FALSE ) +
  annotate("text", x = 0, y =-2, label = " ")+
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom"#,
   # plot.margin = unit(rep(-.5,4), "cm") 
  ) 
p4



# Make the plot
data3 = masterplot %>%
  filter(Comparison != "Critical: Other year types", Group != "Space3") %>%
  mutate(Strength2 = case_when(
    direction == "Decrease" ~ Strength*-1,
    TRUE ~ Strength
  ), Group = factor(direction)) %>%
  arrange(Strength2) %>% 
  mutate(Metric = factor(Metric, levels = unique(Metric)))

data3$ID <- 1:nrow(data3)

# Get the name and the y position of each label
label_data3 <- data3  %>%
  mutate(Metric2 = case_when(Metric == "Lower Trophic"~ "",
                         Metric == "Water Quality" ~ "" ,
                         TRUE ~ as.character(Metric)))

number_of_bar <- nrow(label_data3)
angle <- 90 - 360 * (label_data3$ID-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data3$hjust <- ifelse( angle < -90, 1, 0)
label_data3$angle <- ifelse(angle < -90, angle+180, angle)

#
##################
#this is the good version
#
p6 <- ggplot(data3) +      
  
  # Add the stacked bar
  geom_bar(aes(x=Metric, y=Strength,   fill = direction),  stat="identity") +
  scale_fill_manual(values = c("skyblue","darkorange"), 
                    labels = c("Decrease \nwith Drought", "Increase \nwith Drought"),name = NULL)+
  # ylim(-150,max(label_data$ID, na.rm=T)) +
  
  
  geom_hline(yintercept = 0)+
  coord_polar() +
  
  # Add labels on top of each bar
  geom_text(data=label_data3, aes(x=ID, y=1, label=Metric2, hjust=hjust), 
            color="black", fontface="bold",alpha=0.6, size=5, angle= label_data3$angle, inherit.aes = FALSE ) +
  annotate("text", x = 0, y =-2, label = " ")+
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom"#,
    # plot.margin = unit(rep(-.5,4), "cm") 
  ) 
p6



###############################################################
#Now the version just for 2021
master2021 = read_excel("data/master plot.xlsx", sheet = "thisyear")
data2021 = master2021 %>%
  mutate(Strength2 = case_when(
    direction == "Decrease" ~ Strength*-1,
    TRUE ~ Strength
  ), Group = factor(direction)) %>%
  mutate(Metric = factor(Metric, levels = levels(data3$Metric)))

data2021a = left_join(data2021, dplyr::select(data3, Metric, ID))

# Get the name and the y position of each label
label_data2021 <- data2021a  %>%
  mutate(Metric2 = case_when(Metric == "Lower Trophic"~ "",
                             Metric == "Water Quality" ~ "" ,
                             TRUE ~ as.character(Metric)),
         Metric2 = factor(Metric2, levels = c(levels(Metric), "")))

number_of_bar <- nrow(label_data2021)

angle <- 90 - 360 * (label_data2021$ID-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data2021$hjust <- ifelse( angle < -90, 1, 0)
label_data2021$angle <- ifelse(angle < -90, angle+180, angle)


p2021 <- ggplot(data2021) +      
  
  # Add the stacked bar
  geom_bar(aes(x=Metric, y=Strength,   fill = direction),  stat="identity") +
  scale_fill_manual(values = c( "palegreen3","#FDE333", "grey"),
  #scale_fill_manual(values = c("skyblue","darkorange", "grey"),
                   labels = c("Lower than \npast droughts", 
  "Higher than \npast droughts", "Not yet \nevaluated"),name = NULL)+
  # ylim(-150,max(label_data$ID, na.rm=T)) +
  
  
  geom_hline(yintercept = 0)+
  coord_polar() +
  
  # Add labels on top of each bar
  geom_text(data=label_data2021, aes(x=ID, y=.2, label=Metric2, hjust=hjust), 
            color="black", fontface="bold",alpha=0.6, size=5, angle= label_data2021$angle, inherit.aes = FALSE ) +
  annotate("text", x = 0, y =-2, label = " ")+
  scale_y_continuous(limits = c(-2, 5))+
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom"#,
    # plot.margin = unit(rep(-.5,4), "cm") 
  ) 
p2021
