library(tidyverse)
library(lubridate)
library(sf)
library(readxl)
library(DroughtData)

Toxins = read_csv("data/HABs/ToxinDataDWR2.csv") %>%
  mutate(Date = mdy(Date))
Toxins2 = pivot_wider(Toxins, id_cols = c(Date, Station), names_from = Analyte, 
                      values_from = Result, values_fill = 0) 

ggplot(Toxins2, aes(x = Date, y = MC, color = Station)) + geom_line(size = 1) + geom_point()+
  scale_color_brewer(palette = "Set1")+ ylab("Microcystins (ug/L)") + theme_bw()


hab_samples <- read_excel("data/HABs/DWR_DFD_Cyanotoxin_results_2021 - JG.xlsx")
unique(hab_samples$`PTOX Species`)
habs = mutate(hab_samples, Species = case_when(
  `PTOX Species` %in% c("Aphanizomenon cf. flos-aquae/klebahnii", 
                        "Aphanizomenon flos-aquae/klebahnii", "cf. Aphanizomenon") ~ "Aphanizomenon",
  `PTOX Species` %in% c("Oscillatoria cf. perornata", "Oscillatorialean filament(s)") ~ "Oscillatoria",
  `PTOX Species` %in% c("cf. Phormidium", "Phormidium/Microcoleus") ~ "Phormidium",
  `PTOX Species` == "cf. Planktothrix" ~ "Planktothrix",
  TRUE ~ `PTOX Species`
), Concentration = parse_number(`units/ml`))
unique(habs$Species)

habs2 = pivot_wider(habs, id_cols = c(Station, Date), names_from = Species, 
                       values_from = Concentration, values_fill = 0, values_fn = sum) %>%
  dplyr::select(!`none observed`) %>%
  pivot_longer(cols = "Aphanizomenon":"cf. Chrysosporum", names_to = "Species") %>%
  filter(!is.na(value))

ggplot(habs2, aes(x = as.factor(Date), y = value, fill = Species)) + geom_bar(stat = "identity")

habs2021 = filter(habs2, year(Date)==2021, Station %in% c("Banks PP", "Clifton Court Forebay"),
                  value !=0) %>%
  mutate(Day = date(Date))

ggplot(habs2021, aes(x = as.factor(Date), y = value, fill = Species)) + geom_col() +
  facet_wrap(~Station)+ scale_fill_manual(breaks = HABcol$Genus, values = HABcol$Color) + theme_bw()+
  theme(axis.text.x = element_text(vjust = 1, hjust = 1, angle = 90))+
  ylab("units present") +xlab(NULL)
  
HABcol = data.frame(Color = brewer.pal(7, "Dark2"),
                    Genus = sort(unique(EMPHAB2$Genus)))


hab_samples = mutate(hab_samples, Result = case_when(
  `Result (ng/mL)` %in% c("ND", "ND*") ~"0",
  TRUE ~ `Result (ng/mL)`
), Result = as.numeric(Result))

toxin3 = group_by(hab_samples, Station, Analyte, Date) %>%
  summarize(result = mean(Result, na.rm = T)) %>%
  mutate(result = case_when(
    is.nan(result) ~ 0,
    TRUE ~ result
  ))

ggplot(toxin3, aes(x = Date, y = result)) + geom_line()

##############################################################
#look at data from CEDEN
ceden = read_csv("data/HABs/ceden_data_20220208093111.xls.csv")
str(ceden)

cdensf = st_as_sf(ceden, coords = c("TargetLongitude","TargetLatitude"), crs = st_crs(4326))

regions = R_EDSM_Strata_1718P1%>%
  st_transform(crs = st_crs(4326))

cdensf = st_join(cdensf, regions)  
cdensfin = st_intersection(cdensf, regions)  

ggplot() + geom_sf(data = WW_Delta) + geom_sf(data = regions)+
  geom_sf(data = cdensfin)
#UGH not helpful What am I doing wrong?

#just filter by counties instead.

cedensub = filter(ceden, county %in% c("Contra Costa", "San Joaquin", "Solano", "Sacramento"))