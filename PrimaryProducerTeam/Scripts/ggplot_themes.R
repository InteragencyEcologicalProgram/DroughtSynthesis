library(ggplot2)
library(lemon)
library(ggsci)
library(cowplot)

#### GGPLOT THEMES ############################
theme_ppt <- theme(panel.grid = element_blank(),
                   plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
                   text = element_text(size= 16),
                   plot.background = element_rect(fill = "transparent", color= "transparent"), # bg of the plot
                   panel.background = element_rect(fill= "transparent", color= "transparent"),
                   panel.border= element_rect(fill= NA, color= NA, linetype= "solid", size= 1),
                   #panel.ontop = TRUE,
                   axis.line = element_line(color= "black", size= 0.25),
                   axis.text = element_text(colour="black", size= 14),
                   axis.title.x = element_text(vjust= -0.75),
                   axis.title.y = element_text(vjust= 1.5),
                   legend.background = element_rect(size= 0.25, color="black", fill= "transparent"),
                   legend.key = element_blank(),
                   strip.background=element_rect(fill="transparent", color="transparent"),
                   strip.text = element_text(size= 20)
)
#axis.text.x = element_text(angle= 45, hjust= 1))


theme_doc <- theme(panel.grid = element_blank(),
                   plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
                   text = element_text(size= 12),
                   plot.background = element_rect(fill = "transparent", color= "transparent"), # bg of the plot
                   panel.background = element_rect(fill= "transparent", color= "transparent"),
                   panel.border= element_rect(fill= NA, color= NA, linetype= "solid", size= 1),
                   #panel.ontop = TRUE,
                   axis.line = element_line(color= "black", size= 0.25),
                   axis.text = element_text(colour="black", size= 12),
                   axis.title.x = element_text(vjust= -0.75),
                   axis.title.y = element_text(vjust= 1.5),
                   legend.background = element_rect(size= 0.25, color="black", fill= "transparent"),
                   legend.key = element_blank(),
                   strip.background=element_rect(fill="transparent", color="transparent"),
                   strip.text = element_text(size= 14)
)
#axis.text.x = element_text(angle= 45, hjust= 1))

theme_map <- theme(panel.grid = element_line(color= "grey92", size= 1),
                   plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
                   text = element_text(size= 12),
                   plot.background = element_rect(fill = "transparent", color= "transparent"), # bg of the plot
                   panel.background = element_rect(fill= "transparent", color= NA),
                   panel.border= element_rect(fill= NA, color= "gray60", linetype= "solid", size= 2),
                   panel.ontop = FALSE,
                   #axis.line = element_line(color= "black", size= 0.25),
                   axis.line = element_blank(),
                   axis.text = element_text(colour="black", size= 10),
                   axis.title.x = element_text(vjust= -0.75),
                   axis.title.y = element_text(vjust= 1.5),
                   legend.background = element_rect(size= 0.25, color="black", fill= "transparent"),
                   legend.key = element_blank(),
                   strip.background=element_rect(fill="transparent", color="transparent"),
                   strip.text = element_text(size= 14)
                   )
