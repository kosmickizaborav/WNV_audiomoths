#' ---
#' title: "01_download_drive_data"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#' **SECTION 1 - Downloading census files from Google drive** 
#' download census data from Alex. All data is located in the folder
#' "BBDD_ALEX_Ocells_Aiguamolls", organized by year and transect.
#' kept the same structure when downloading. 
#' 
#' **SECTION 2 - Standardizing census data for analysis**
#' reformatting alex tables to be easier for analysis. 
#' 
#' **SECTION 2 - Downloading audiomoth fieldsheet**
#' downloading the excel where we enter the basic audiomoth field information


# packages and directories ------------------------------------------------

library(tidyverse)
library(here)
library(readxl)
library(googledrive)

# login to google account
drive_auth("ninaupupina@gmail.com")


# folder name where the data will be downloaded
census_dir <- here("Data", "Census")
birdnet_dir <- here("Data", "Birdnet")

if(!dir.exists(census_dir) | !dir.exists(birdnet_dir)){
  
  if(!dir.exists(here("Data"))) { here("Data") |> dir.create() }
  
  census_dir |> dir.create()
  birdnet_dir |> dir.create()
  
}

# 1 - Downloading census files from Google drive-------------------------------

# folder name on the google drive where Alex uploads the data
drive_dir <- "BBDD_ALEX_Ocells_Aiguamolls"
# years of the study to download (subfolders in the main drive folder)
years <- c("2024")

drive_get(drive_dir) |> 
  # list folders in the directory
  drive_ls(type = "folder") |> 
  as_tibble() |> 
  # selecting only the years of interest (for now only 2024)
  filter(name %in% years) |> 
  # split by year
  group_split(name) |> 
  map(~{
    
    year <- unique(.x$name)
      
    year_dir <- here(census_dir, year)
    
    # create the folder for each year
    if(!dir.exists(year_dir)) { year_dir |> dir.create() }
    
    # list all the transect folders
    .x |> 
      # list only folders in this directory
      drive_ls(type = "folder") |> 
      as_tibble() |> 
      mutate(year = year)
    
  }) |> 
  bind_rows() |> 
  # split by transect folder
  group_split(name) |> 
  map(~{
    
    transect <- unique(.x$name)
    year <- unique(.x$year)
    
    # create a folder for each transect
    trans_dir <- here(census_dir, year, transect)
    
    if(!dir.exists(trans_dir)) { trans_dir |> dir.create() }
    
    # list the files within each transect folder and download the data
    .x |> 
      # list all the excels present in the transect folder
      drive_ls(type = "xlsx") |> 
      as_tibble() |> 
      mutate(year = year) |> 
      bind_rows() |> 
      # split by the excell file
      group_split(name) |> 
      map(~{
        
        f_name <- .x$name |> 
          str_remove(".xlsx") |> 
          janitor::make_clean_names() |> 
          # correcting some typos
          str_replace_all(c(
            "cens0" = "cens_0", 
            "sacara" = "sacra", 
            "vila_sacra" = "vilasacra"
            )
          )
        
        drive_download(
          .x, 
          path = here(trans_dir, str_c(.x$year, "_", f_name, ".xlsx")), 
          overwrite = T
        )
        
      })
    
  }) 




# 2 - Downloading audiomoth field-sheet ------------------------------------


# download the file where we keep audiomoth deployment info
drive_find(
  "Birds_Audio", 
  shared_drive = "Mosquito Alert Drive", 
  type = "folder"
  ) |>
  drive_ls() |> 
  filter(name == "AudioMoth") |> 
  drive_download(
    path = here(birdnet_dir, "audiomoth_field_data.xlsx"), 
    overwrite = T
  )


