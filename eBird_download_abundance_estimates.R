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
  filter(ine.prov.name == "Girona" & name %in% c("Castelló d'Empúries", "Sant Pere Pescador"))

# Obtaining extracting points --------------------------------------------------
# Extract data from points
location <- data.frame( # Sant Pere Pescador and Castelló d'Empuries points
  lon = c(3.097687, 3.096502, 3.122178), 
  lat = c(42.236601, 42.216132, 42.235563)
)
location <- st_as_sf(location, coords = c("lon", "lat"), crs = 4236, remove = FALSE)

taxonomy_list <- read.csv(url("https://www.birds.cornell.edu/clementschecklist/wp-content/uploads/2024/10/eBird_taxonomy_v2024.csv")) %>%
  janitor::clean_names()

# Abundance maps available:
av <- ebirdst_runs

# Calculating abundance estimates for each species -----------------------------
ebird_abundance_estimates <- function(species_list, location, region_boundary, plotting = FALSE){
  # Empty dataframe
  ebird_data <- data.frame()
  
  for(species in species_list){
    # Selecting species, check the ebird code.
    # get_species() is not reliable, check eBird Taxonomy v2024 list
    
    code_species = taxonomy_list$species_code[taxonomy_list$sci_name == species]
    cat(species, "----", code_species, "\n")
    
    if (is.na(code_species) == TRUE){
      ebird_row <- data.frame(
        scientific_name = species,
        code_name = code_species,
        ebird_abundance = NA
      )
    } else {
      # Checking the raster stored folder 
      # ebirdst_data_dir()
      
      # Download data
      cat("Download raster", "\n")
      ebirdst_download_status(code_species, patter = "abundance_seasonal_mean_3km")
      
      # Load relative abundance raster stack 
      raster_temp <- load_raster(code_species, product = "abundance", metric = "mean",
                                 period = "seasonal", resolution = "3km")
      # plot(raster_temp, axes = FALSE)
      
      # Cropping and masking
      raster_temp <- crop(raster_temp, st_transform(region_boundary, st_crs(raster_temp))) |> 
        mask(st_transform(region_boundary, st_crs(raster_temp)))
      
      if (plotting == TRUE){
        # Map the cropped data
        plot(raster_temp[[1]], axes = FALSE, main = species)
        plot(st_geometry(st_transform(region_boundary, st_crs(raster_temp))), add = TRUE)
        plot(st_geometry(location %>% st_transform(st_crs(raster_temp))), add = TRUE)
      }
      
      # Extract the data
      # We downloaded the data from seasonal abundance raster, which it is a raster stack 
      # including 4 periods: breeding, nonbreeding, prebreeding_migration, postbreeding_migration
      # We have to sum these periods to obtain a year-arounf value by location, then, sum the locations
      ebird <- terra::extract(raster_temp, location %>% st_transform(st_crs(raster_temp)))[-1] %>%
        rowSums(., na.rm = TRUE) %>% sum(na.rm = TRUE)
      
      ebird_row <- data.frame(
        scientific_name = species,
        code_name = code_species,
        ebird_abundance = ebird
      ) 
    }
    ebird_data <- rbind(ebird_data, ebird_row)
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
# species_list = "Limosa lapponica"

ebird_abundances <- ebird_abundance_estimates(species_list, location, region_boundary, 
                                              plotting = TRUE)

