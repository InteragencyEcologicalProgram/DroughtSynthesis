#another look at EMP's phytoplankton
#some of this borrowed from Dave


# Load packages
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)
library(tibble)
library(lubridate)
library(readxl)
library(sf)
library(here)


# 1. Import Data ----------------------------------------------------------

# Create a vector of all file paths within the data-raw/Phyto_data folder
fp_phyto_data <- dir("data/Phyto_data/", recursive = TRUE, full.names = TRUE)

# Import earlier phytoplankton data
df_phyto_early <-
  read_csv(
    str_subset(fp_phyto_data, "EMP_phyto_data.csv$"),
    col_types = "-DTc-cccd--d-----------"
  )

# Import phytoplankton data collected from Dec 2020 - Oct 2021
lst_phyto_recent <- map(str_subset(fp_phyto_data, "202[01]\\.xlsx$"), read_excel)

# Import phytoplankton classification table (copied from the DroughtSynthesis repository)
df_phyto_taxonomy <- read_excel(str_subset(fp_phyto_data, "Phyto Classification.xlsx$"))

# Import EMP station coordinates from EDI
df_coord_emp <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.458.4&entityid=827aa171ecae79731cc50ae0e590e5af")


# 2. Clean and Combine Data -----------------------------------------------

# Earlier data:
df_phyto_early_c <- df_phyto_early %>%
  # filter to years 2014-2020, don't include Dec 2020 through 2021 since there is some overlap
  filter(SampleDate >= "2014-01-01" & SampleDate < "2020-12-01") %>%
  # Rename some variables
  rename(
    Date = SampleDate,
    Station = StationCode,
    OrganismsPerMl = Organisms_per_mL
  )

# Recent data (Dec 2020 - Oct 2021):
# Create a vector of variable names to keep in each list element
vec_vars_keep <-
  c(
    "SampleDate",
    "SampleTime",
    "StationCode",
    "Taxon",
    "Genus",
    "Species",
    "Factor",
    "Count",
    "Unit Abundance (# of Natural Units)"
  )

df_phyto_recent <- lst_phyto_recent %>%
  # Select and rename variables in each element
  map(
    ~ dplyr::select(.x, any_of(vec_vars_keep)) %>%
      rename(Count = contains("Abundance"))
  ) %>%
  # Combine data now that all variable names and types are consistent
  bind_rows() %>%
  # Remove rows with all NA's
  filter(!if_all(everything(), is.na)) %>%
  # Convert SampleDate to date and calculate Organisms/mL
  mutate(
    Date = date(SampleDate),
    OrganismsPerMl = Factor * Count
  ) %>%
  # Rename and remove some variables
  rename(Station = StationCode) %>%
  dplyr::select(-c(SampleDate, Factor))

# Combine recent data to earlier data
df_phyto_all <- bind_rows(df_phyto_early_c, df_phyto_recent)


# 3. Clean All Data -------------------------------------------------------

library(deltamapr)

# # Assign EDB regions to EMP stations
# df_region_emp <- df_coord_emp %>%
#   select(Station, Latitude, Longitude) %>%
#   filter(!if_any(c(Latitude, Longitude), is.na)) %>%
#   # Convert to sf object
#   st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
#   st_join(st_make_valid(EDBdata:::sf_edb_reg), join = st_intersects) %>%
#   # Drop sf geometry column since it's no longer needed
#   st_drop_geometry() %>%
#   # Assign "Outside" to stations without region assignments
#   replace_na(list(Region = "Outside"))

reg3 = st_transform(R_EDSM_Strata_1718P1, crs = 4326)

df_region_emp <- df_coord_emp %>%
     dplyr::select(Station, Latitude, Longitude) %>%
     filter(!if_any(c(Latitude, Longitude), is.na)) %>%
     # Convert to sf object
     st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
     st_join(reg3, join = st_intersects) %>%
     # Drop sf geometry column since it's no longer needed
     st_drop_geometry() 



# Three genera need to be added to the taxonomy table: Mayamaea, Acanthoceras, and Lindavia
# I looked up these three genera in AlgaeBase: https://www.algaebase.org/
# Also adding a placeholder for unknown Genera
df_add_genera <-
  tribble(
    ~Genus, ~AlgalType,
    "Mayamaea", "Pennate Diatom",
    "Acanthoceras", "Centric Diatom",
    "Lindavia", "Centric Diatom",
    "Unknown", "Unknown"
  )

# Prepare phytoplankton classification table to be joined to data
df_phyto_taxonomy_c1 <- df_phyto_taxonomy %>%
  rename(AlgalType = `Algal Type`) %>%
  # We are not including Kingdom, Phylum, and Class in this table for now
  # since they are not up to date with recent changes in the higher taxonomy
  # of phytoplankton
  distinct(Genus, AlgalType) %>%
  filter(
    !(Genus == "Leptocylindrus" & AlgalType =="Centric diatom"),
    Genus != "Unknown"
  ) %>%
  # Add additional genera to the classification table
  bind_rows(df_add_genera)

# Finish cleaning up the phytoplankton data
phyto_edb <- df_phyto_all %>%
  mutate(
    # Create DateTime variable in PST
    DateTime = ymd_hm(
      paste0(Date, " ", hour(SampleTime), ":", minute(SampleTime)),
      tz = "Etc/GMT+8"
    ),
    # Fix a few erroneous Station names:
    # Stations C3A-Hood and C3A-HOOD represent station C3A,
    # Stations NZ328 and NZ542 are most likely typos and most likely represent NZ325 and NZS42
    Station = case_when(
      Station == "C3A-HOOD" ~ "C3A",
      Station == "C3A-Hood" ~ "C3A",
      Station == "NZ328" ~ "NZ325",
      Station == "NZ542" ~ "NZS42",
      TRUE ~ Station
    ),
    # Add variable for year
    Year = year(Date)
  ) %>%
  # Add EDB regions to all phytoplankton data
  left_join(df_region_emp, by = "Station") %>%
  # Remove data for stations outside of the EDB regions keeping NA values in
  # Region to catch errors during testing
  # Also remove the EZ stations since they are not fixed
  filter(
    !str_detect(Station, "^EZ")
  ) %>%
  # Add taxonomic information to all phytoplankton data
  left_join(df_phyto_taxonomy_c1, by = "Genus") %>%
  # Reorder columns
  dplyr::select(
    Station,
    Stratum,
    Year,
    Date,
    DateTime,
    Taxon,
    Genus,
    Species,
    AlgalType,
    Count,
    OrganismsPerMl
  )

write.csv(phyto_edb, file = "AllEMPphyto.csv")


#######################################################

#plot it at the genus level
EMP_wzeros = pivot_wider(phyto_edb, id_cols = c(Station, Date, Stratum, Year), names_from = Genus,
                         values_from = `OrganismsPerMl`, values_fill = 0, values_fn = sum) %>%
  pivot_longer(cols = "Cocconeis":last_col(), names_to = "Genus", values_to = "CountperML") %>%
  mutate(Month = month(Date)) %>%
  left_join(df_phyto_taxonomy_c1) %>%
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


######################################################
#no cyanobacteria
EMP_nocy = filter(EMP_wzeros, AlgalType != "Cyanobacterium")
ggplot(EMP_nocy, aes(x = Stratum, y = CountperML, fill = AlgalType))+ 
  geom_col()+facet_grid(Year~Month)

ggplot(filter(EMP_nocy, Year == 2016), aes(x = Month, y = CountperML, fill = AlgalType))+ 
  geom_col()+facet_wrap(~Stratum)

ggplot(filter(EMP_wzeros, Year == 2016), aes(x = Month, y = CountperML, fill = AlgalType))+ 
  geom_col()+facet_wrap(~Stratum)

###################################################
#Harmful species


#harmful critters only
EMPHAB = filter(EMP_wzeros,  Genus %in% c("Aphanizomenon", "Anabaena", "Dolichospermum", 
                                        "Microcystis", "Oscillatoria", "Cylindrospermopsis",  "Anabaenopsis",
                                        "Planktothrix"), !is.na(Stratum)) %>%
  mutate(Genus = case_when(Genus == "Anabaena" ~"Dolichospermum",
                           TRUE ~ Genus)) 

EMPHABave = group_by(EMPHAB, Stratum, Month, Year, Genus) %>%
  summarize(CountperML = mean(CountperML))

ggplot(EMPHABave, aes(x = Month, y = CountperML, fill = Genus))+ 
  geom_col()+facet_grid(Year~Stratum) + scale_y_log10()+
  ylab("Organisms per mL") + theme_bw()+ theme(legend.position = "bottom")+
  scale_x_continuous(breaks = c(0,4,8,12))

ggplot(filter(EMPHABave, Genus != "Microcystis"), aes(x = Month, y = CountperML, fill = Genus))+ 
  geom_col()+facet_grid(Year~Stratum, scales = "free_y")

