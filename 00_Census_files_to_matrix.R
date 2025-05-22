#################### Processing data from census files #########################
# We need obtain the data in unmarked format

# Libraries
library(tidyverse)
library(here)
library(readxl)

rm(list = ls())
source("functions.R")

# Directories ------------------------------------------------------------------
loc.data <- paste0(getwd(), "/DATA/")
loc.census <- paste0(getwd(), "/DATA/BBDD_ALEX_Ocells_Aiguamolls/")
loc.out <- paste0(getwd(), "/OUTPUT/")

# Distance data frame  ---------------------------------------------------------
fls <- list.files(loc.census)[3:7]

full_data <- data.frame()
for (i in fls){
  fl <- read_csv(paste0(loc.census, i), col_names = TRUE)
  
  fl <- fl %>% 
    mutate(
      species_corr = str_remove_all(species, "[0-9[:punct:]]") %>% str_squish()
    ) %>% 
    check_birdlife(species_corr) %>% 
    mutate(
      birdlife_name = if_else(
        str_detect(species_corr, " sp$") | str_count(species_corr, " ") > 1, 
        NA, 
        birdlife_name
      )
    )
  
  full_data <- rbind(full_data, fl)
}

rm(fl, fls, i)

# Preparing data for Distance algorithm
full = data.frame(
  species = ifelse(is.na(full_data$birdnet_name), full_data$species_corr, full_data$birdnet_name),
  Region.Label = case_when(
    full_data$trans_official_name == "01. Vilasacra_Fortia" ~ "Vila_Sacra",
    full_data$trans_official_name == "02. Gallinera" ~ "Gallinera",
    full_data$trans_official_name == "03. Fluvia" ~ "Fluvia",
    full_data$trans_official_name == "04. Cortalet_Mata" ~ "Cortalet",
    full_data$trans_official_name == "05. Muga" ~ "Muga"
  ),
  Sample.Label = case_when(
    full_data$sector == "s1" ~ "S01",
    full_data$sector == "s2" ~ "S02",
    full_data$sector == "s3" ~ "S03",
    full_data$sector == "s4" ~ "S04",
    full_data$sector == "s5" ~ "S05",
    full_data$sector == "s6" ~ "S06",
  ),
  Area = 240000,
  Effort = 600,
  distbegin = case_when(
    full_data$banda == "0-25 m" ~ 0,
    full_data$banda == "25-100 m" ~ 25,
    full_data$banda == ">100 m" ~ 100
  ),
  distend = case_when(
    full_data$banda == "0-25 m" ~ 25,
    full_data$banda == "25-100 m" ~ 100,
    full_data$banda == ">100 m" ~ 250
  ),
  size = full_data$total,
  data = full_data$calendar_date,
  month = full_data$month,
  ornitoleg = full_data$ornitoleg,
  horari_inici = full_data$horari_inici,
  horari_final = full_data$horari_final
  ) %>%
  mutate(
    id = paste0(Region.Label, "_", Sample.Label),
    object = c(1:nrow(full_data))
  )

rm(full_data)

saveRDS(full, file = paste0(loc.out, "full_wider_db.rds"))
