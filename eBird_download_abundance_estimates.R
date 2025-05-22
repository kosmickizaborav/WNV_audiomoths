#################### Extracting eBird abundace estimates #######################
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

# Obtaining a vector with our study area: Sant Pere Pescador and Castelló d'Empuries municipalities --
# region boundary
region_boundary <- esp_get_munic(region = "Catalunya") %>% 
  filter(ine.prov.name == "Girona")

# Obtaining extracting points --------------------------------------------------
# Read the transect data
location <- readRDS(paste0(loc.out, "transects.rds")) %>%
  st_transform(4236)

# # Extract data from points
# location <- data.frame( # Sant Pere Pescador and Castelló d'Empuries points
#   lon = c(3.097687, 3.096502, 3.122178), 
#   lat = c(42.236601, 42.216132, 42.235563)
# )
# location <- st_as_sf(location, coords = c("lon", "lat"), crs = 4236, remove = FALSE)

taxonomy_list <- read.csv(url("https://www.birds.cornell.edu/clementschecklist/wp-content/uploads/2024/10/eBird_taxonomy_v2024.csv")) %>%
  janitor::clean_names()

# Abundance maps available:
av <- ebirdst_runs

# Calculating abundance estimates for each species -----------------------------
ebird_abundance_estimates <- function(species_list, location, region_boundary, plotting = FALSE){
  ebird_data <- data.frame()
  
  i = 0
  for (species in species_list) {
    i = i + 1
    print(i)
    code_species <- taxonomy_list$species_code[taxonomy_list$sci_name == species]
    cat(species, "----", code_species, "\n")
    
    # Si no hay código de especie, asignar NA
    if (length(code_species) == 0) {
      ebird <- location %>%
        mutate(
          total = NA,
          scientific_name = species,
          code_name = NA
        ) %>% 
        st_drop_geometry()
    } else {
      # Usar tryCatch para evitar que errores detengan el bucle
      ebird <- tryCatch({
        cat("Download raster", "\n")
        ebirdst_download_status(code_species, pattern = "seasonal_mean_3km", 
                                download_all = FALSE)
        
        # raster_temp <- load_raster(code_species, product = "abundance", metric = "mean",
        #                            period = "full-year", resolution = "3km")
        raster_temp <- load_raster(code_species, product = "abundance", metric = "mean",
                                   period = "seasonal", resolution = "3km")
        raster_temp <- raster::subset(raster_temp, c("breeding", "prebreeding_migration", "postbreeding_migration"))
        raster_temp <- mean(raster_temp, na.rm = TRUE)
        
        raster_temp <- crop(raster_temp, st_transform(region_boundary, st_crs(raster_temp))) %>% 
          mask(st_transform(region_boundary, st_crs(raster_temp)))
        
        if (plotting== TRUE) {
          plot(st_geometry(location %>% st_transform(st_crs(raster_temp))))
          plot(raster_temp[[1]], axes = FALSE, main = species, , add = TRUE)
          # plot(st_geometry(st_transform(region_boundary, st_crs(raster_temp))), add = TRUE)
          plot(st_geometry(location %>% st_transform(st_crs(raster_temp))), add = TRUE)
        }
        
        ebird_extracted <- terra::extract(raster_temp, location %>% st_transform(st_crs(raster_temp))) %>%
          # mutate(total = rowSums(.[,-1], na.rm = TRUE)) %>% 
          mutate(total = .[,-1]) %>%
          group_by(ID) %>%
          summarise(total = mean(total, na.rm = TRUE)) 
        
        ebird_result <- cbind(location, ebird_extracted[,-1]) %>%
          mutate(
            scientific_name = species,
            code_name = code_species
          ) %>%
          st_drop_geometry()
        
      }, error = function(e) {
        message("Error con especie ", species, ": ", e$message)
        location %>%
          mutate(
            total = NA,
            scientific_name = species,
            code_name = code_species
          ) %>%
          st_drop_geometry()
      })
    }
    
    ebird_data <- rbind(ebird_data, ebird)
  }
  
  return(ebird_data)
}

# Apply the function
audios <- readRDS(file = paste0(loc.out, "audios_raw_data.txt"))

# # IMPORTANT!!! In order to obtain a list of species names, run in bash the following: 
# python3 species.py --lat 42.225039 --lon 3.092257 --week -1 --o /home/catuxa/Documents/E4WARNING/From_suitability_to_abundances/species.txt
species_names <- read_tsv("species_cortalet.txt", col_names = FALSE) %>%
  separate(X1, c("scientific_name", "common_name"), "_")
audios <- merge(audios, species_names, by = "common_name", all.x = TRUE)

species_list <- unique(audios$scientific_name)
species_list <- species_list[-178] # It's a NA
sum(is.na(species_list))
# species_list = c("Prunella collaris", "Turdus merula")

ebird_abundances <- ebird_abundance_estimates(species_list, location, region_boundary, 
                                              plotting = FALSE)

saveRDS(ebird_abundances, file = paste0(loc.out, "ebird_abundances_breedingsround.rds"))

# How many species with total = NA
ebird_abundances %>% filter(is.na(total)) %>% dplyr::select(scientific_name) %>% distinct()

# Refilling the data (some maps are called "year-around") ----------------------
ebird_abundance_estimates_seasonal <- function(species_list, location, region_boundary, plotting = FALSE){
  ebird_data <- data.frame()
  
  i = 0
  for (species in species_list) {
    i = i + 1
    print(i)
    code_species <- taxonomy_list$species_code[taxonomy_list$sci_name == species]
    cat(species, "----", code_species, "\n")
    
    # Si no hay código de especie, asignar NA
    if (length(code_species) == 0) {
      ebird <- location %>%
        mutate(
          total = NA,
          scientific_name = species,
          code_name = NA
        ) %>% 
        st_drop_geometry()
    } else {
      # Usar tryCatch para evitar que errores detengan el bucle
      ebird <- tryCatch({
        cat("Download raster", "\n")
        ebirdst_download_status(code_species, pattern = "abundance_seasonal_mean_3km", 
                                download_all = FALSE)
        
        raster_temp <- load_raster(code_species, product = "abundance", metric = "mean",
                                   period = "seasonal", resolution = "3km")
        
        raster_temp <- crop(raster_temp, st_transform(region_boundary, st_crs(raster_temp))) %>% 
          mask(st_transform(region_boundary, st_crs(raster_temp)))
        
        if (plotting== TRUE) {
          plot(st_geometry(location %>% st_transform(st_crs(raster_temp))))
          plot(raster_temp[[1]], axes = FALSE, main = species, , add = TRUE)
          # plot(st_geometry(st_transform(region_boundary, st_crs(raster_temp))), add = TRUE)
          plot(st_geometry(location %>% st_transform(st_crs(raster_temp))), add = TRUE)
        }
        
        ebird_extracted <- terra::extract(raster_temp, location %>% st_transform(st_crs(raster_temp))) %>%
          # mutate(total = rowSums(.[,-1], na.rm = TRUE)) %>% 
          mutate(total = .[,-1]) %>%
          group_by(ID) %>%
          summarise(total = mean(total, na.rm = TRUE)) 
        
        ebird_result <- cbind(location, ebird_extracted[,-1]) %>%
          mutate(
            scientific_name = species,
            code_name = code_species
          ) %>%
          st_drop_geometry()
        
      }, error = function(e) {
        message("Error con especie ", species, ": ", e$message)
        location %>%
          mutate(
            total = NA,
            scientific_name = species,
            code_name = code_species
          ) %>%
          st_drop_geometry()
      })
    }
    
    ebird_data <- rbind(ebird_data, ebird)
  }
  
  return(ebird_data)
}
species_list <- ebird_abundances %>% 
  filter(is.na(total)) %>%
  pull(scientific_name) %>%
  unique()

ebird_abundances_seasonal <- ebird_abundance_estimates_seasonal(species_list, location, region_boundary, 
                                              plotting = FALSE)

ebird_abundances <- rbind(ebird_abundances %>% drop_na(total), ebird_abundances_seasonal)

# How many species with total = NA
ebird_abundances %>% filter(is.na(total)) %>% dplyr::select(scientific_name) %>% distinct()

ebird_abundances <- ebird_abundances %>%
  rename("ebird_abund" = "total")

saveRDS(ebird_abundances, file = paste0(loc.out, "ebird_abundances.rds"))
