#' ---
#' title: "01_download_and_prepare_data"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#' **SECTION 1 - Downloading census files from Google drive** 
#' download census data from Alex. All data is located in the folder
#' "BBDD_ALEX_Ocells_Aiguamolls", organized by year and transect.
#' kept the same structure when downloading. 
#' 
#' **SECTION 2 - Standardizing census data for analysis**
#' reformating alex tables to be easier for analysis. 
#' 
#' **SECTION 3 - Downloading audiomoth fieldsheet**
#' downloading the excel where we enter the basic audiomoth field information


# packages and directories ------------------------------------------------

library(tidyverse)
library(here)
library(readxl)
library(googledrive)

# login to google account
drive_auth("ninaupupina@gmail.com")


# 1 - Downloading census files from Google drive-------------------------------

# folder name on the google drive where Alex uploads the data
drive_dir <- "BBDD_ALEX_Ocells_Aiguamolls"
# years of the study to download (subfolders in the main drive folder)
years <- c("2024")
# folder name where the data will be downloaded
census_dir <- here("Data", "Census_alex")

if(!dir.exists(census_dir)){
  
  if(!dir.exists(here("Data"))) { here("Data") |> dir.create() }
  
  census_dir |> dir.create()

}

drive_dir <- "BBDD_ALEX_Ocells_Aiguamolls"

years <- c("2024")


drive_get(drive_dir) |> 
  drive_ls() |> 
  as_tibble() |> 
  # selecting only the years of interest (for now only 2024)
  filter(name %in% years) |> 
  # split by year
  group_split(name) |> 
  map(~{
    
    year_dir <- here(census_dir, .x$name)
    
    # create the folder for each year
    if(!dir.exists(year_dir)) { year_dir |> dir.create() }
    
    # list all the transect folders
    .x |> 
      drive_ls(type = "folder") |> 
      as_tibble() |> 
      mutate(year = .x$name)
    
  }) |> 
  bind_rows() |> 
  group_split(name) |> 
  map(~{
    
    year <- .x$year
    
    # create a folder for each transect
    trans_dir <- here(census_dir, year, .x$name)
    
    if(!dir.exists(trans_dir)) { trans_dir |> dir.create() }
    
    # list the files within each transect folder and download the data
    .x |> 
      drive_ls(type = "xlsx") |> 
      as_tibble() |> 
      mutate(year = .x$year) |> 
      bind_rows() |> 
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


# 2 - Standardizing census data for analysis ------------------------------

# census info columns
info_cols <- c("data", "transecte", "horari_inici", "horari_final", "ornitoleg")  

# for renameing columns
repl <- c("x4" = "s1", "x6" = "s2", "x8" = "s3", "x10" = "s4", "x12" = "s5", 
          "x14" = "s6")

years |> 
  map(~{
    
    trans_dirs <- here("Data", "Census_alex", .x) |> 
      list.dirs() 
    
    # [-1] to remove the main directory (here("Data", "Census_alex", .x))
    trans_dirs[-1] |> 
      map(~{
        
        trans_dir <- .x 
        
        trans_dir |> 
          list.files() |> 
          map(~{
            
            fname <- here(trans_dir, .x)
            
            info <- fname |> 
              read_xlsx(col_names = F) |> 
              janitor::clean_names() |> 
              select(1) |> 
              # first 5 rows have census info data
              filter(row_number() < 6) |> 
              # all info are in one column, separate them
              separate_wider_delim(
                x1, ":", names = c("var", "value"), 
                too_many = "merge", 
                too_few = "align_start"
              ) |>
              # change some noted typos
              mutate(
                var = str_replace(var, " ", "_"),
                value = str_replace(value, "'", ":")
              ) |> 
              # make columns for each info value
              pivot_wider(names_from = var, values_from = value) |>
              janitor::clean_names() |> 
              select(any_of(info_cols)) |> 
              mutate_all(str_squish)
            
            # loading transect data
            trans_df <- fname |>
              # skip info columns
              read_xlsx(skip = 5) |>
              janitor::clean_names() |>
              rename(total_radius = x15, total_transect = x16) |>
              select(-starts_with("total")) |>
              filter(row_number() != 1) |>
              # to remove extra NA fields that sometimes occur in the excel
              # e.g. they have banda value but nothing else
              # also so that I can do species below
              filter(row_number() <= max(which(!is.na(especie))+2)) |>
              # for each species there are 3 rows, 
              # indicating the distance of detection, 
              # but they are not explicitly written
              mutate(
                species = rep(especie[which(!is.na(especie))], each = 3)
              ) |>
              # these are the columns for the sector
              pivot_longer(
                cols = 3:14, names_to = "sector", values_to = "count"
              ) |>
              mutate(
                count = as.numeric(count),
                # if it contains s, it is mascla, otherwise the column name is x_,
                # it's a by_product of how alex's table is structured
                bird_cat = if_else(str_detect(sector, "s"), "mascla", "altra"),
                # renaming x_ with the adequate segment names
                sector = str_replace_all(sector,  repl)
              ) |>
              summarise(
                total = sum(count, na.rm = T),
                .by = c("species", "sector", "banda")
              ) |>
              # mutate(
              #   tname_alex = tname
              # ) |>
              bind_cols(info) 
            
          }
          ) |> 
          bind_rows() |> 
          write_csv(here(str_c(trans_dir, "_complete_data.csv")))
      }
      )
  }
  )



# 3 - Downloading audiomoth fieldsheet ------------------------------------


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


