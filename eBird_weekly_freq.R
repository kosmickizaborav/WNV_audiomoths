library(fields)
library(mapSpain)
library(sf)
library(terra)
library(ebirdst)
library(tidyverse)

# setwd("~/Documents/E4WARNING/From_suitability_to_abundances/WNV_audiomoths")

rm(list = ls())

# Directories ------------------------------------------------------------------
loc.data <- paste0(getwd(), "/DATA/")
loc.out <- paste0(getwd(), "/OUTPUT/")

# Download ebird data ----------------------------------------------------------
audios <- readRDS(file = paste0(loc.out, "audios_raw_data.txt"))
# # IMPORTANT!!! In order to obtain a list of species names, run in bash the following: 
# python3 species.py --lat 42.225039 --lon 3.092257 --week -1 --o /home/catuxa/Documents/E4WARNING/From_suitability_to_abundances/species.txt
species_names <- read_tsv("species_cortalet.txt", col_names = FALSE) %>%
  separate(X1, c("scientific_name", "common_name"), "_")
audios <- merge(audios, species_names, by = "common_name", all.x = TRUE)

location <- data.frame(lon = 3.092257, lat = 42.225039) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

taxonomy_list <- read.csv(url("https://www.birds.cornell.edu/clementschecklist/wp-content/uploads/2024/10/eBird_taxonomy_v2024.csv")) %>%
  janitor::clean_names()

audios$freq <- NA
for (i in 1:nrow(audios)){
  
  if (i %% 100 == 0){
    cat("-------------------- Row", i, "-----------------------\n")
  }
  
  audio_row <- audios[i, ]
  species <- audio_row$scientific_name
  wk <- week(audio_row$date)
  
  code_species <- taxonomy_list$species_code[taxonomy_list$sci_name == species]
  cat(i, "-----", code_species, "-----\n")
  
  if (length(code_species) == 0 || is.na(code_species)) {
    cat(species, "not available \n")
    audios[i, "freq"] <- NA
    next
  }
  
  # Wrap risky part in tryCatch
  tryCatch({
    # Check data availability
    ebirdst_download_status(code_species,
                            download_abundance = TRUE, 
                            pattern = "median_3km",
                            dry_run = TRUE)
    
    # Load raster
    raster_temp <- load_raster(code_species, 
                               product = "abundance", 
                               metric = "median",
                               period = "weekly", 
                               resolution = "3km")
    
    raster_temp <- raster_temp[[wk]]
    location <- location %>% st_transform(st_crs(raster_temp))
    F_jk <- terra::extract(raster_temp, location)[1,2]
    
    audios[i, "freq"] <- F_jk
    
  }, error = function(e) {
    message("❌ Error for row ", i, " (", species, "): ", conditionMessage(e))
    audios[i, "freq"] <- NA
  })
}


