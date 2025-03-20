#' ---
#' title: "02_prepare_data_for_analysis"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#' **SECTION 1 - Standardizing census data for analysis**
#' reformatting alex' tables to be easier for analysis. 
#' the script is tailored so that it includes data from 2023 and 2024 and the 
#' way it was formatted
#' 
#' **SECTION 2 - Gathering results from BirdNet analysis**
#' gathering all the BirdNet data available for the transect in one file
#' the results were obtained running this:
#' python3 /home/nina/BirdNET-Analyzer/analyze.py 
#' --i /home/nina/Audiomoths/Files --o /home/nina/Audiomoths/Results 
#' --lat 42.225039 --lon 3.092257




# packages and directories ------------------------------------------------

library(tidyverse)
library(here)
library(readxl)

# folder name where the data will be downloaded
census_dir <- here("Data", "Census")
years <- c(2023, 2024)

birdnet_dir <- here("Data", "Birdnet")
audiomoth_results <- "/home/nina/Audiomoths/Results"

# 1 - Standardizing census data for analysis ------------------------------

# census info columns
info_cols <- c("data", "transecte", "horari_inici", "horari_final", "ornitoleg")  

# for renameing columns
repl <- c("x4" = "s1", "x6" = "s2", "x8" = "s3", "x10" = "s4", "x12" = "s5", 
          "x14" = "s6")


check <- years |> 
  map(~{
    
    year <- .x 
    
    trans_dirs <- here(census_dir, year) |> 
      list.dirs() 
    
    if(year == 2024){
      
      calendar <- here(census_dir, year) |> 
        list.files(pattern = "calendar", full.names = T) |> 
        read_xlsx(n_max = 10) |> 
        rename(month = 1) |> 
        mutate(month = tolower(month)) |> 
        rename_with(~str_remove(.x, "\\*"), everything()) |> 
        pivot_longer(
          cols = -month, 
          names_to = "trans_official_name", 
          values_to = "calendar_date"
        )
      
    }
    
    # [-1] to remove the main directory (here(census_dir, year))
    trans_dirs[-1] |> 
      map(~{
        
        trans_dir <- .x 
        
        print(trans_dir)
        
        # take the folder name as the transect name in order to standardize it 
        # easier
        trans_name <- str_split_i(trans_dir, "/", -1)
        
        trans_dir |> 
          list.files() |> 
          map(~{
            
            fname <- .x
            
            print(fname)
            
            keys <- fname |> 
              str_remove(".xlsx") |> 
              str_replace_all("_", "|") |> 
              reduce(str_c)
            
            info <- here(trans_dir, fname) |> 
              read_xlsx(col_names = F) |> 
              janitor::clean_names() |> 
              select(1)
            
            # getting row where the list of the species start
            sp_row <- which(str_to_lower(as_vector(info)) == "especie")
            
            info <- info |> 
              # first 5 rows have census info data
              filter(row_number() < sp_row) |> 
              filter(!is.na(x1)) |> 
              # all info are in one column, separate them
              separate_wider_delim(
                x1, ":", names = c("var_name", "value"), 
                too_many = "merge", 
                too_few = "align_start"
              ) |> 
              # change some observed typos
              mutate(
                var_name = str_replace(var_name, " ", "_") |> 
                  str_to_lower(),
                value = str_replace(value, "'", ":")
              ) |> 
              filter(var_name %in% info_cols) |>
              #make columns for each info value
              pivot_wider(names_from = var_name, values_from = value) |>
              # remove extra spaces
              mutate_all(str_squish) |>
              mutate(trans_official_name = trans_name)

            
            if(year == 2024){

              c_date <- calendar |>
                filter(str_detect(month, keys)) |>
                filter(str_detect(tolower(trans_official_name), keys))

              info <- info |>
                left_join(c_date, by = "trans_official_name")

              # loading transect data
              trans_df <- here(trans_dir, fname)|>
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
                  .by = any_of(c("species", "sector", "banda"))
                ) |>
                mutate(
                  trans_official_name = trans_name,
                  file = fname
                ) |>
                left_join(info, by = "trans_official_name")

            }


            if(year == 2023){

              # loading transect data
              trans_df <- here(trans_dir, fname)|>
                # skip info columns
                read_xlsx(skip = sp_row-1) |>
                janitor::clean_names() |>
                select(-starts_with("total")) |>
                mutate(species = str_squish(especie)) |>
                # to eliminate the columns at the end that have total value per
                # sector
                filter(!is.na(species) & str_to_lower(species) != "total") |>
                # in some counts there was labels like 30+, 40+
                mutate(
                  across(
                    any_of(matches("^s[1-9]$")),
                    ~as.numeric(str_remove_all(.x, "[^0-9]"))
                  )
                ) |>
                pivot_longer(
                  cols = any_of(matches("^s[1-9]$")),
                  names_to = "sector",
                  values_to = "count"
                ) |>
                mutate(count = as.numeric(count)) |>
                summarise(
                  total = sum(count, na.rm = T),
                  .by = c("species", "sector")
                ) |>
                mutate(
                  trans_official_name = trans_name,
                  file = fname
                ) |>
                left_join(info, by = "trans_official_name")
            }
            
            trans_df

          }) |> 
          bind_rows() |> 
          write_csv(
            here(
              census_dir,
              str_c(
                year, "_", str_remove(trans_name, " "), "_complete_data.csv")
              )
            )
      }
      )
    print(paste(year, "DONE!"))
  }
  )


# 2 - Gathering BirdNet results -----------------------------------------------

# because there is no data for that year
years <- years[years != 2023]

audiomoths <- here(birdnet_dir, "audiomoth_field_data.xlsx")|> 
  read_xlsx() |> 
  janitor::clean_names() |> 
  filter(!is.na(data_id)) |>
  rename(tname_alex = transect_name_alex) |> 
  mutate(year = year(start_date)) |> 
  select(year, tname_alex, sector, data_id)

audiomoths |> 
  group_split(tname_alex, year) |> 
  map(~{
    
    transect_name <- unique(.x$tname_alex)
    year <- unique(.x$year)
    
    print(str_c(year, transect_name, "STARTED!", sep = " "))
    
    .x$data_id |> 
      map(~{
        
        data_id <- .x
        print(data_id)
        
        file.path(audiomoth_results, data_id) |>  
          # listing sub-folders for every day
          list.files(full.names = T) |> 
          # list the files in each subfolder and load them
          map(~list.files(.x, full.names = T)) |> 
          map(~read_tsv(.x, show_col_types = F)) |>
          bind_rows() |> 
          janitor::clean_names() |> 
          mutate(
            data_id = data_id,
            # getting the .WAV file name
            recording = str_split_i(begin_path, "/", -1), 
            datetime = as_datetime(
              str_remove(recording, ".WAV"), format = "%Y%m%d_%H%M%S"
            ), 
            transect_name = transect_name
          )
        
        }
      ) |> 
      bind_rows() |> 
      write_csv(
        here(
          birdnet_dir,
          str_c(
          year, "_", transect_name, "_complete_data.csv"
          )
        )
      )
    
    print(str_c(year, transect_name, "DONE!", sep = " "))
    
  }, 
  .progress = T
  )



