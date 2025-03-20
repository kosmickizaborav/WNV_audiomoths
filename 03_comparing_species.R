#' ---
#' title: "02_prepare_data_for_analysis"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#' **SECTION 1 - Standardizing census data for analysis**
#' reformatting alex' tables to be easier for analysis. 
#' 
#' **SECTION 2 - Gathering results from BirdNet analysis**
#' gathering all the BirdNet data available for the transect in one file

# packages and directories ------------------------------------------------

library(tidyverse)
library(here)
library(readxl)

# folder name where the data will be downloaded
census_dir <- here("Data", "Census")
years <- c("2024")

birdnet_dir <- here("Data", "Birdnet")
census_dir <- here("Data", "Census")


# loading the names from the birdnet
bird_names <- here("Data", "BirdNET_GLOBAL_6K_V2.4_Labels.txt") |> 
  read_delim(delim = "_", col_names = F) |> 
  rename_with(~c("scientific_name", "common_name"), 1:2)


pnae <- here("Data", "Llista-PNAE-v5.0.-11-25.xlsx") |> 
  read_xlsx(skip = 1) 
  filter(!is.na(species))

  
  # file for frederica

  df <- census_dir |>
    list.files(pattern = ".csv", full.names = T) |>
    map(~{
      .x |> 
        read_csv(show_col_types = F) |> 
        mutate(data = ifelse(is.na(data), calendar_date, data)) |> 
        select(trans_official_name, species, sector, data, total)
    }) |> 
    list_rbind() |> 
    filter(total > 0) |> 
    summarize(
      total_count = sum(total), 
      .by = c(trans_official_name, species, data)
    ) |>
    mutate(
      data = as.Date(data, format = "%d/%m/%Y"), 
      year = year(data)
    ) |> 
    rename(date = data) |> 
    select(year, everything())
  
  
  df |> 
    write_csv(here("Data", "Census_data_summary_for_Fatima.csv"))
  
  