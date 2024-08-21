library(tidyverse)
library(here)
library(googledrive)

# login to google account
drive_auth("ninaupupina@gmail.com")



# 1 - Downloading census files from Google drive-------------------------------

# folder name on the google drive where Alex uploads the data
drive_dir <- "BBDD_ALEX_Ocells_Aiguamolls"
# years of the study to download (subfolders in the main drive folder)
years <- c("2024")
# folder name where the data will be downloaded
census_dir <- here(here("Data", "Census_alex"))

if(!dir.exists(census_dir)){
  
  if(!dir.exists(here("Data"))) { here("Data") |> dir.create() }
  
  census_dir |> dir.create()

  }



drive_get(drive_dir) |> 
  drive_ls() |> 
  as_tibble() |> 
  # selecting only the years of interest (for now only 2024)
  filter(name %in% years) |> 
  # split by year
  group_split(name) |> 
  map(~{
    
    # create the folder for each year
    if(!dir.exists(here(census_dir, .x$name))) {
      dir.create(here(census_dir, .x$name))
    }
    
    # list all the transect folders
    .x |> 
      drive_ls(type = "folder") |> 
      as_tibble() |> 
      mutate(year = .x$name)
    
  }) |> 
  bind_rows() |> 
  # split by transect folders
  group_split(name) |> 
  map(~{
    
    # create a folder for each transect
    trans_dir <- here(census_dir, year, .x$name)
    
    if(!dir.exists(trans_dir)) {
      dir.create(trans_dir)
    }
    
    year <- .x$year
    
    # list the files within each transect folder and download the data
    drive_ls(.x, type = "xlsx") |> 
      as_tibble() |> 
      mutate(year = .x$year) |> 
      bind_rows() |> 
      group_split(name) |> 
      map(~{
        
        f_name <- .x$name |> 
          str_remove(".xlsx") |> 
          janitor::make_clean_names() |> 
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



# 2 - Downloading audiomoth fieldsheet ------------------------------------


# download the file where we keep audiomoth deployment info
drive_find(
  "Birds_Audio", 
  shared_drive = "Mosquito Alert Drive", 
  type = "folder"
  ) |>
  drive_ls() |> 
  filter(name == "AudioMoth") |> 
  drive_download(
    path = here("Data", "audiomoth_field_data.xlsx"), 
    overwrite = T
  )

