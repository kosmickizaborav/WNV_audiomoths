library(tidyverse)
library(here)
library(googledrive)

# login to google account
# drive_auth()


if(!dir.exists(census_dir)){
  
  if(!dir.exists(here("Data"))) { here("Data") |> dir.create() }
  
  census_dir |> dir.create()
}

# 1 - Downloading census files from Google drive-------------------------------

drive_dir <- "BBDD_ALEX_Ocells_Aiguamolls"

census_dir <- here(here("Data", "Census_alex"))

years <- c("2024")


drive_get(drive_dir) |> 
  drive_ls() |> 
  as_tibble() |> 
  filter(name %in% years) |> 
  group_split(name) |> 
  map(~{
    .x |> 
      drive_ls(type = "folder") |> 
      as_tibble() |> 
      mutate(year = .x$name)
    
  }) |> 
  bind_rows() |> 
  group_split(name) |> 
  map(~{
    
    drive_ls(.x, type = "xlsx") |> 
      as_tibble() |> 
      mutate(year = .x$year)
    
  }) |> 
  bind_rows() |> 
  group_split(name) |> 
  map(~{
    
    f_name <- .x$name |> 
      str_remove(".xlsx") |> 
      janitor::make_clean_names() |> 
      str_replace_all(c("cens0" = "cens_0", "vila_sacra" = "vilasacra"))
      
    
    drive_download(
      .x, 
      path = here("Data", "Census_alex", str_c(.x$year, "_", f_name, ".xlsx")), 
      overwrite = T
    )
  })



# 2 - Downloading audiomoth fieldsheet ------------------------------------


# audiomoth file
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

