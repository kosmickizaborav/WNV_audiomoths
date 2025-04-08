#' ---
#' title: "02_prepare_data_for_analysis"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#' **SECTION 1 -  Extract transect information**
#' from each transect files, extract info about the times and dates
#' of transect as well as the ornitologist id. 
#' 
#' **SECTION 2 - Prepare transect files 2024**
#' reformatting alex' tables to be easier for analysis, 
#' gather all information for that year in one file. 
#' done for the two years separately because the data format is not identical
#' 
#' **SECTION 3 - Prepare transect files 2023**
#' reformatting alex' tables to be easier for analysis, 
#' gather all information for that year in one file. 
#' done for the two years separately because the data format is not identical
#' 
#' **SECTION 2 - Gather irdNet results**
#' gathering all the BirdNet data available for the year in one file
#' the results were obtained running this:
#' python3 /home/nina/BirdNET-Analyzer/analyze.py 
#' --i /home/nina/Audiomoths/Files --o /home/nina/Audiomoths/Results 
#' --lat 42.225039 --lon 3.092257

# 0 - Packages and directories --------------------------------------------

library(tidyverse)
library(here)
library(readxl)
library(janitor)
source(here("00_functions.R"))

# folder name where the data will be downloaded
data_dir <- here("Data")
census_dir <- file.path(data_dir, "Census")
birdnet_dir <- file.path(data_dir,"Birdnet")
audiomoth_results_dir <- "/home/nina/Audiomoths/Results"

years <- c(2023, 2024)

# 1 - Extract transect information --------------------------------------------

data_list <- file.path(data_dir, "01_files_downloaded_from_drive.csv") |> 
  read_csv(show_col_types = F) |> 
  rename(original_file_name = name) |> 
  select(contains("file_name"), transect, month, year) |> 
  mutate(year_folder = file.path(census_dir, year))


# only for this year we have the calendar list
calendar2024 <- data_list |> 
  filter(file_name == "Calendari_censos.xlsx") |> 
  mutate(file_path = file.path(year_folder, file_name)) |> 
  pull(file_path) |> 
  read_xlsx(n_max = 10) |> 
  rename(month = 1) |> 
  mutate(month = tolower(month)) |> 
  rename_with(~str_remove(.x, "\\*"), everything()) |> 
  pivot_longer(
    cols = -month, 
    names_to = "trans_official_name", 
    values_to = "calendar_date"
  ) |> 
  mutate(
    trans_official_name = str_replace_all(
      str_remove_all(trans_official_name, " "), "[:punct:]", "_"
    ), 
    year = 2024, 
    calendar_date = as_date(calendar_date)
  )

# information columns that we want to extract from the transect files
info_cols <- c("data", "transecte", "horari_inici", "horari_final", "ornitoleg")  

info_all <- data_list |> 
  filter(!is.na(transect)) |> 
  group_split(file_name) |> 
  map(~{
    
    file_info <- .x
    
    trans_name <- file_info$transect
    year_dir <- file_info$year_folder
    
    fin <- file_info$file_name
    
    # all the relevant info is in the first column
    info <- file.path(year_dir, fin) |> 
      read_xlsx(col_names = F) |> 
      clean_names() |> 
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
        var_name = make_clean_names(var_name),
        value = str_replace(value, "'", ":")
      ) |> 
      filter(var_name %in% info_cols) |>
      #make columns for each info value
      pivot_wider(names_from = var_name, values_from = value) |>
      # remove extra spaces
      mutate_all(str_squish) |>
      mutate(
        trans_official_name = trans_name, 
        file_name = fin, 
        sp_row = sp_row
      ) 
    
  }) |> 
  list_rbind() |> 
  mutate(data = as_date(data, format = "%d/%m/%Y"))

transect_info <- data_list |> 
  filter(!is.na(transect)) |>
  left_join(info_all, by = "file_name") |> 
  left_join(calendar2024, by = c("trans_official_name", "month", "year")) |> 
  select(-transecte, -trans_official_name) |> 
  rename(start_time = horari_inici, end_time = horari_final) |> 
  rename(
    date_transect_file = data, 
    date_calendar2024 = calendar_date
  ) |> 
  mutate(
    date = if_else(
      is.na(date_transect_file), date_calendar2024, date_transect_file
    )
  )

rm(info_all, calendar2024, data_list)

transect_info |> 
  select(-year_folder, -sp_row) |> 
  write_rds(file.path(census_dir, "02_transect_info.rds"))


# 2 - Prepare transect files 2024----------------------------------------------

# for renameing columns
repl <- c("x4" = "s1", "x6" = "s2", "x8" = "s3", "x10" = "s4", "x12" = "s5", 
          "x14" = "s6")

transect_info |> 
  filter(year == 2024) |> 
  select(year_folder, file_name) |> 
  group_split(file_name) |> 
  map(~{
    
    fin <- .x$file_name
    fin_path <- file.path(.x$year_folder, fin)
    
    # loading transect data
    fin_path |> 
      # skip info columns
      read_xlsx(skip = 5) |> 
      clean_names() |>
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
      filter(total > 0) |> 
      mutate(file_name = fin)
    
  }) |> 
  list_rbind() |>
  left_join(
    transect_info |> select(file_name, date, transect), by = "file_name"
  ) |> 
  mutate(
    species_corr = str_remove_all(species, "[0-9[:punct:]]") |> str_squish()
  ) |> 
  check_birdlife(species_corr) |> 
  mutate(
    birdlife_name = if_else(
      str_detect(species_corr, " sp$") | str_count(species_corr, " ") > 1, 
      NA, 
      birdlife_name
    )
  ) |> 
  select(-species_corr) |> 
  write_rds(file.path(census_dir, "2024_complete_data.rds"))


# 3 - Prepare transect files 2023----------------------------------------------


transect_info |> 
  filter(year == 2023) |> 
  select(year_folder, file_name, sp_row) |> 
  group_split(file_name) |> 
  map(~{
    
    fin <- .x$file_name
    fin_path <- file.path(.x$year_folder, fin)
    sp_row <- .x$sp_row
    
    # loading transect data
    fin_path |> 
      # skip info columns
      read_xlsx(skip = sp_row-1) |>
      clean_names() |>
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
      filter(total > 0) |> 
      mutate(file_name = fin)
    
  }) |> 
  list_rbind() |> 
  left_join(
    transect_info |> select(file_name, date, transect), by = "file_name"
  ) |> 
  mutate(
    species_corr = str_remove_all(species, "[0-9[:punct:]]") |> str_squish()
  ) |> 
  check_birdlife(species_corr) |> 
  mutate(
    birdlife_name = if_else(
      str_detect(species_corr, " sp$") | str_count(species_corr, " ") > 1, 
      NA, 
      birdlife_name
    )
  ) |> 
  select(-species_corr) |> 
  write_rds(file.path(census_dir, "2023_complete_data.rds"))

# 4 - Gathering BirdNet results -----------------------------------------------

sp_cortalet <- file.path(data_dir, "species_cortalet.txt") |> 
  read_delim(
    col_names = c("scientific_name", "common_name"), 
    delim = "_", 
    show_col_types = F
  )
            
audiomoths <- here(birdnet_dir, "audiomoth_field_data.xlsx")|> 
  read_xlsx() |> 
  clean_names() |> 
  filter(!is.na(data_id)) |>
  rename(transect = transect_name_alex) |> 
  mutate(
    year = year(start_date), 
    transect = str_replace_all(
      str_remove_all(transect, " "), "[:punct:]", "_"
    )
  ) |> 
  select(year, transect, sector, data_id) 

audiomoths |> 
  group_split(year) |> 
  map(~{
    
    year_data <- .x
    
    year <- unique(year_data$year)
    
    year_data |> 
      group_split(data_id) |> 
      map(~{
        
        data_id <- .x$data_id
        
        print(paste(year, data_id, "processing!"))
        
        file.path(audiomoth_results_dir, data_id) |>  
          # listing sub-folders for every day
          list.files(full.names = T) |> 
          # list the files in each subfolder and load them
          map(~list.files(.x, full.names = T)) |> 
          map(~read_tsv(.x, show_col_types = F)) |>
          list_rbind() |> 
          clean_names() |> 
          mutate(
            data_id = data_id,
            # getting the .WAV file name
            recording = str_split_i(begin_path, "/", -1), 
            datetime = as_datetime(
              str_remove(recording, ".WAV"), format = "%Y%m%d_%H%M%S"
            )
          ) |> 
          filter(species_code != "nocall") |> 
          left_join(year_data, by = "data_id")
        
      }) |> 
      list_rbind() |> 
      left_join(sp_cortalet, by = "common_name") |> 
      write_rds(
        file.path(birdnet_dir, str_c(year, "_complete_data.rds"))
      )
    
    print(paste(year, "DONE!"))
    
  })


# 5 - Prepare abundance file ----------------------------------------------

# aiguamolls abundance files, downloaded from:
# https://acrobat.adobe.com/id/urn:aaid:sc:EU:6e6ba7bd-a979-44e5-aadb-7b51b6489701
# transformed by adone to excel
# (R) Resident = Espècie sedentària i reproductora, amb possibles moviments dispersius fora del període de nidificació. 
# (V) Visitant = Espècie no reproductora observable durant tot l'any, a partir d'exemplars residents en àrees poc o molt properes. 
# (E) Estival = Espècie reproductora present només durant el període de nidificació.  
# (Eu) Estiuejant = Espècie no reproductora present només durant el període de nidificació. S’inclouen també les espècies que utilitzen el PNAE i/o la badia de Roses com a zones d’alimentació durant la seva reproducció en àrees poc o molt properes. 
# (H) Hivernant = Espècie present durant el període d'hivernada, de manera estable o en trànsit. 
# (M) Migrant = Espècie present durant els períodes migratoris. S'inclouen també les espècies que arriben en dispersió al PNAE i/o la badia de Roses abans o després de la seva reproducció en àrees poc o molt properes. 
# Els valors utilitzats per a concretar el grau d’abundància de les espècies serien els següents:  
# (0) Extingit = Espècie extingida en la categoria fenològica indicada. 
# (1) Accidental = Espècie amb un màxim de 10 citacions -d’un o més exemplars- en la categoria fenològica indicada. Si es tracta d’ocells reproductors, es considera l’existència de menys de 5 episodis de cria comprovats, protagonitzats per parelles aïllades o per més parelles si es tracta de colònies. 
# (2) Molt escàs = Espècie de presència anual o quasi anual, habitualment amb menys de 10 exemplars, però a vegades amb alguna desena d’exemplars en la categoria fenològica indicada.   
# (3) Escàs = Espècie amb presència anual de desenes o, a vegades, algun centenar d'exemplars en la categoria fenològica indicada. 
# (4) Comú = Espècie amb presència anual de centenars o, a vegades, algun miler d'exemplars en la categoria fenològica indicada. 
# (5) Abundant = Espècie amb presència anual de milers d'exemplars en la categoria fenològica indicada. 

file.path(data_dir, "Llista-PNAE-v5.0.-11-25.xlsx") |> 
  read_xlsx(skip = 1) |> 
  janitor::clean_names() |> 
  rename(scientific_name = x2) |> 
  filter(!is.na(scientific_name)) |> 
  select(scientific_name, r, v, e, eu, h, m) |> 
  pivot_longer(
    cols = -scientific_name, 
    names_to = "category", 
    values_to = "abundance", 
    values_drop_na = T
  ) |> 
  mutate(
    category_english = case_when(
      category == "r" ~ "resident", 
      category == "v" ~ "visitor",
      category == "e" ~ "reproducing",
      category == "eu" ~ "summer visitor",
      category == "h" ~ "wintering",
      category == "m" ~ "migrating"
    ),
    category = case_when(
      category == "r" ~ "resident", 
      category == "v" ~ "visitant",
      category == "e" ~ "estival",
      category == "eu" ~ "estiuejant",
      category == "h" ~ "hivernant",
      category == "m" ~ "migrant"
    ), 
    abundance_eng = case_when(
      abundance == 0 ~ "extinct",
      abundance == 1 ~ "accidental",
      abundance == 2 ~ "very rare",
      abundance == 3 ~ "rare",
      abundance == 4 ~ "common",
      abundance == 5 ~ "abundant"
    )
  ) |> 
  write_csv(file.path(data_dir, "02_augamolls_species_abundance.csv"))
  



