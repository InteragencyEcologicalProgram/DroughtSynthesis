#EMP's phyto data (after Dave cleaned it up)

library(tidyverse)

library(tibble)
library(lubridate)
library(readxl)
library(sf)
library(here)


phyto_edb = read_csv("AllEMPphyto.csv")
tax = group_by(phyto_edb, Genus, AlgalType) %>%
  summarize(n = n())
#plot it at the genus level
EMP_wzeros = pivot_wider(phyto_edb, id_cols = c(Station, Date, Stratum, Stratum2, Year), names_from = Genus,
                         values_from = `OrganismsPerMl`, values_fill = 0, values_fn = sum) %>%
  pivot_longer(cols = "Cocconeis":last_col(), names_to = "Genus", values_to = "CountperML") %>%
  mutate(Month = month(Date)) %>%
  left_join(tax) %>%
  mutate(AlgalType = case_when(
    `AlgalType` == "Centric diatom" ~ "Centric Diatom",
    `AlgalType` == "Unknown Genus" ~ "Unknown",
    `AlgalType` %in% c("Coccolithophore", "Eustigmatophyte", "Haptophyte", "Raphidophyte",
                       "Silico-flagellate", "Synurophyte", "Xanthophyte", "Kathablepharid") ~ 
      "Other",
    TRUE ~ `AlgalType`
  ) )

ggplot(EMP_wzeros, aes(x = Station, y = CountperML, fill = Genus))+ geom_col()+facet_wrap(~Month)

ggplot(EMP_wzeros, aes(x = Stratum, y = CountperML, fill = AlgalType))+ geom_col()+facet_grid(Year~Month)

###################################################
#Harmful species


#harmful critters only
#Anabaena was rennamed Dolichospermum at some point, so rename those
EMPHAB = filter(EMP_wzeros,  Genus %in% c("Aphanizomenon", "Anabaena", "Dolichospermum", 
                                          "Microcystis", "Oscillatoria", "Cylindrospermopsis",  "Anabaenopsis",
                                          "Planktothrix"), !is.na(Stratum)) %>%
  mutate(Genus = case_when(Genus == "Anabaena" ~"Dolichospermum",
                           TRUE ~ Genus))

#Average nuber of each taxa by region, month, and year
EMPHABave = group_by(EMPHAB, Stratum2, Month, Year, Genus) %>%
  summarize(CountperML = mean(CountperML))


ggplot(EMPHABave, aes(x = Stratum2, y = CountperML, fill = Genus))+ 
  geom_col()+facet_wrap(~Year) + 
  ylab("Organisms per mL") + theme_bw()+ theme(legend.position = "bottom")+
  theme(axis.text.x = element_text(angle = 90))#+
# scale_x_continuous(breaks = c(2,6,10), labels = c("Feb", "Jun", "Oct"))

#Microcystis isn't well sampled, so maybe leave it out?
ggplot(filter(EMPHABave, Genus != "Microcystis"), 
       aes(x = Month, y = CountperML, fill = Genus))+ 
  geom_col()+facet_grid(Year~Stratum2, scales = "free_y")

#one more try

ggplot(EMPHABave, aes(x = Year, y = CountperML, fill = Genus))+ 
  geom_col(position = "fill")+facet_wrap(~Stratum2) + 
  ylab("Organisms per mL") + theme_bw()+ theme(legend.position = "bottom")+
  scale_x_continuous(breaks = c(2,6,10), labels = c("Feb", "Jun", "Oct"))

#I think this is the one I want to use
ggplot(EMPHABave, aes(x = Year, y = CountperML, fill = Genus))+ 
  geom_col()+facet_grid(Genus~Stratum2, scales = "free_y") + 
  ylab("Organisms per mL") + theme_bw()+ scale_fill_discrete(guide = NULL)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
ggsave("plots/HABspecies.tiff", device = "tiff", width = 7, height = 8)
