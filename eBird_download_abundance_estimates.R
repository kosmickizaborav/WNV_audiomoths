library(fields)
library(mapSpain)
library(sf)
library(terra)
library(ebirdst)
library(tidyverse)

setwd("~/Documents/E4WARNING/From_suitability_to_abundances/WNV_audiomoths")

# Obtaining a vector with our study area: Sant Pere Pescador and Castelló d'Empuries municipalities --
# region boundary
region_boundary <- esp_get_munic(region = "Catalunya") %>% 
  filter(ine.prov.name == "Girona" & name %in% c("Castelló d'Empúries", "Sant Pere Pescador"))

# project boundary to match raster data
region_boundary_proj <- st_transform(region_boundary, st_crs(raster_temp))

# Obtaining extracting points --------------------------------------------------
# Extract data from points
location <- data.frame( # Sant Pere Pescador and Castelló d'Empuries points
  lon = c(3.097687, 3.096502), 
  lat = c(42.236601, 42.216132)
)
location <- st_as_sf(location, coords = c("lon", "lat"), crs = 4236, remove = FALSE)

# Calculating abundance estimates for each species -----------------------------
ebird_abundance_estimates <- function(species_list, location, region_boundary_proj, plotting = FALSE){
  # Empty dataframe
  ebird_data <- data.frame()
  
  for(species in species_list){
    # Selecting species, check the ebird code
    code_species = get_species(species)
    cat(species, "----", code_species, "\n")
    
    # Checking the raster stored folder 
    # ebirdst_data_dir()
    
    # Download data
    cat("Download raster", "\n")
    ebirdst_download_status(code_species, patter = "abundance_seasonal_mean_3km", force = TRUE)
    
    # Load relative abundance raster stack 
    raster_temp <- load_raster(code_species, product = "abundance", metric = "mean",
                               period = "seasonal", resolution = "3km")
    # plot(raster_temp, axes = FALSE)
    
    # Cropping and masking
    raster_temp <- crop(raster_temp, region_boundary_proj) |> 
      mask(region_boundary_proj)
    
    if (plotting == TRUE){
      # Map the cropped data
      plot(raster_temp[[1]], axes = FALSE)
      plot(st_geometry(region_boundary_proj), add = TRUE)
      plot(st_geometry(location %>% st_transform(st_crs(raster_temp))), add = TRUE)
    }
  
    # Extract the data
    # We downloaded the data from seasonal abundance raster, which it is a raster stack 
    # including 4 periods: breeding, nonbreeding, prebreeding_migration, postbreeding_migration
    # We have to sum these periods to obtain a year-arounf value by location, then, sum the locations
    ebird <- terra::extract(raster_temp, location %>% st_transform(st_crs(raster_temp)))[-1] %>%
      rowSums(.) %>% sum()
    
    ebird_row <- data.frame(
      scientific_name = species,
      code_name = code_species,
      ebird_abundance = ebird
    ) 
    
    ebird_data <- rbind(ebird_data, ebird_row)
  }
}



