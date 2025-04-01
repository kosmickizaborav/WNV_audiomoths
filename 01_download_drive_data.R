#' ---
#' title: "01_download_drive_data"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#' **SECTION 1 - List all files from drive** 
#' get list of all files that we want to download from drive. 
#' Alex' census data is located in the folder
#' "BBDD_ALEX_Ocells_Aiguamolls", organized by year and transect.
#' 
#' **SECTION 2 - Download Alex data**
#' download alex census data
#' 
#' **SECTION 3 - Download audiomoth fieldsheet**
#' downloading the excel where we enter the basic audiomoth field information

# 0 - Packages and directories --------------------------------------------

library(tidyverse)
library(here)
library(readxl)
library(googledrive)

# login to google account
drive_auth("ninaupupina@gmail.com")


# folder name where the data will be downloaded
data_dir <- here("Data")
census_dir <- file.path(data_dir, "Census")
birdnet_dir <- file.path(data_dir,"Birdnet")

if(!dir.exists(census_dir) | !dir.exists(birdnet_dir)){
  
  if(!dir.exists(data_dir)) { data_dir |> dir.create() }
  
  census_dir |> dir.create()
  birdnet_dir |> dir.create()
  
}


# 1 - List all files from drive -------------------------------------------


# folder name on the google drive where Alex uploads the data
drive_dir <- "BBDD_ALEX_Ocells_Aiguamolls"
# years of the study to download (sub-folders in the main drive folder)
years <- c(2023, 2024)


# get all the files listed from drive
data_list <- drive_get(drive_dir) |> 
  # data organizes in years
  drive_ls(type = "folder") |> 
  mutate(year = name) |> 
  group_split(year) |> 
  map(~{
    
    year_data <- .x
    
    # year folders organized in
    year_data |> 
      drive_ls(type = "folder") |> 
      mutate(transect_folder = name) |> 
      group_split(transect_folder) |> 
      map(~{
        transect_data <- .x
        
        transect_data |> 
          drive_ls(type = "xlsx") |> 
          as_tibble() |> 
          mutate(transect_folder = unique(transect_data$transect_folder))
        
      }) |>
      list_rbind() |> 
      bind_rows(drive_ls(year_data, type = "spreadsheet")) |>
      mutate(year = unique(year_data$year)) 
  }) |> 
  list_rbind()

# modifying the names of the data so that they are neatly organized
months <- c(
  "gener", "febrer", "març", "abril", "maig", "juny", 
  "juliol", "agost", "setembre", "octubre", "novembre", "desembre"
) |> str_c(collapse = "|")

data_list <- data_list |> 
  mutate(
    # checked they are 5 unique with this correction
    transect = str_remove_all(transect_folder, " ") |> 
      str_replace_all("[:punct:]", "_"), 
    month = str_extract(str_to_lower(name), months), 
    file_name = str_c(str_c(transect, year, month, sep = "_"), ".xlsx"), 
    file_name = ifelse(
      is.na(file_name), 
      str_replace_all(name, "[:punct:]|xlsx", " ") |> 
        str_squish() |> str_replace_all(" ", "_") |> str_c(".xlsx"), 
      file_name
    )
  ) |> 
  select(-transect_folder)

data_list |> 
  write_csv(file.path(data_dir, "01_files_downloaded_from_drive.csv"))


# 2 - Download Alex data -----------------------------------------------------

data_list |> 
  group_split(year) |> 
  walk(~{
    
    data_list_year <- .x 
    
    year_folder <- file.path(census_dir, unique(data_list_year$year))
    
    if(!dir.exists(year_folder)) { dir.create(year_folder) }
    
    data_list_year |> 
      group_split(file_name) |> 
      walk(~{
        
        file_info <- .x 
        fout <- file_info$file_name
        
        file_info |> 
          drive_download(
            path = file.path(year_folder, fout), 
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


