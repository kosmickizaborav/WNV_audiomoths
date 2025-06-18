library(tidyverse)
source(here::here("00_functions.R"))

# filename for census and birdnet data
fin <- "2024_complete_data.rds"

# main folder paths
data_dir <- here::here("Data")
census_dir <- file.path(data_dir, "Census")
birdnet_dir <- file.path(data_dir,"Birdnet")

# 1 - Prepare non audio data----------------------------------------------------


### PREVALENCE ####

wnvp <- file.path(data_dir, "Palearctic_prevalence bird_species.xlsx") |> 
  readxl::read_xlsx() |>        
  janitor::clean_names() |>     
  # rename columns for clarity and consistency
  rename(
    birdlife_prevalence_name = bird_life_name, 
    n_studies_prevalence = k_number_studies
  ) |> 
  # match the species names to the current birdlife classification
  # using a custom function
  check_birdlife(species_name = birdlife_prevalence_name, birdnet_check = F) |> 
  # Format prevalence and its confidence intervals so that it's easier to read
  mutate(
    ci_prevalence = if_else(
      ci_lb == ci_ub, 
      paste0(round(ci_lb * 100, 2), "%"),
      paste0("[", round(ci_lb * 100, 2), "% - ", round(ci_ub * 100, 2), "%]")
    ), 
    group_prevalence = round(group_prevalence * 100, 2)
  ) |>
  # select only relevant columns
  select(
    birdlife_prevalence_name, 
    birdlife_name, 
    group_prevalence, 
    ci_prevalence, 
    n_studies_prevalence
  )



### CENSUS ####

census_df <- file.path(census_dir, fin) |>
  read_rds() |> 
  # manually fix problematic species names
  mutate(
    scientific_name = case_when(
      # Correct species name typos and synonyms
      species == "Periparus cyaneus" ~ "Cyanistes caeruleus",
      species == "Cyaneus caeruleus" ~ "Cyanistes caeruleus",
      species == "Cyanecula cyaneus" ~ "Cyanistes caeruleus",
      birdlife_name == "Cyanistes cyanus" ~ "Cyanistes caeruleus",
      birdlife_name == "Saxicola torquatus" ~ "Saxicola rubicola",
      birdlife_name == "Curruca cantillans" ~ "Curruca iberiae", 
      .default = birdlife_name
    )
  ) |> 
  # remove previous birdlife classification, to complete it with manual corrections
  select(-birdlife_name, -birdnet_name, -name_type) |> 
  # match the species names to the current birdlife classification
  # using a custom function
  check_birdlife(scientific_name) |>                    
  # keep only species that have birdlife_name cause we use it to merge
  filter(!is.na(birdlife_name)) |>                      
  # aggregate total census counts by species
  summarize(total_census = sum(total), .by = birdlife_name) 


### ABUNDANCE PNAE ####

abundance_pnae <- file.path(data_dir, "02_augamolls_species_abundance.csv") |> 
  read_csv(show_col_types = F) |> 
  # set factor for abundance and category for ordering
  mutate(
    abundance_eng = factor(
      abundance_eng, 
      levels = c(
        "abundant", "common", "rare", "very rare", "accidental", "extinct" 
      )
    ), 
    category_eng = factor(
      category_eng, 
      levels = c(
        "resident", "reproducing", "wintering", 
        "visitor", "summer visitor", "migrating"
      )
    )
  ) |> 
  group_by(scientific_name) |> 
  # if species is in multiple categories, take the one with highest abundance
  filter(n() == 1 | n() > 1 & abundance == max(abundance)) |>
  # arrange by abundance and category within each group
  arrange(abundance_eng, category_eng, .by_group = T) |> 
  # keep only one row per species
  distinct(scientific_name, .keep_all = T) |> 
  ungroup() |> 
  # match the species names to the current birdlife classification
  check_birdlife(scientific_name) |>   
  # remove unmatched species
  filter(!is.na(birdlife_name)) |>     
  # select relevant columns
  select(birdlife_name, abundance_eng, category_eng) |> 
  # rename columns to indicate data source
  rename_with(~str_replace(.x, "_eng$", "_pnae"), everything()) 


### ABUNDANCE ATLAS ####

abundance_atlas <- file.path(data_dir, "llista_abundancia_julia.xlsx") |> 
  readxl::read_xlsx() |> 
  # validate species names against BirdLife taxonomy
  check_birdlife(atlas_name, birdnet_check = F) |> 
  # rename abundance column for clarity
  rename(abundance_atlas = abundancia) |>
  # select only relevant columns
  select(birdlife_name, abundance_atlas)

### PHYLOGENY ####

bird_file <- "Handbook of the Birds of the World and BirdLife International Digital Checklist of the Birds of the World_Version_9.xlsx"

birdlife_phylo <- here::here(bird_file) |> 
  readxl::read_xlsx(skip = 2) |> 
  janitor::clean_names() |> 
  select(seq, scientific_name, family_name, order) |> 
  # keep only rows with valid sequence and family
  filter(!is.na(seq) & !is.na(family_name)) |> 
  # drop sequence column
  select(-seq) |> 
  # remove duplicate rows
  distinct() |> 
  # fix order name from all capitals to normal
  mutate(
    order = paste0(
      toupper(substring(order, 1, 1)), tolower(substring(order, 2))
    )
  ) |> 
  # rename to match data source
  rename(
    birdlife_family = family_name, 
    birdlife_name = scientific_name, 
    birdlife_order = order
  ) 




# 2 - Merge to audio ------------------------------------------------------

audio_df <- file.path(birdnet_dir, fin) |>
  read_rds() |> 
  select(scientific_name, confidence) |> 
  # classify detection confidence into bins that are relevant for validation
  # correct ons species name according to Alex Olle
  mutate(
    confidence_range = cut(confidence, c(0.09, seq(0.2, 1, 0.1)), right = T), 
    birdnet_name = scientific_name, 
    scientific_name = ifelse(
      scientific_name == "Curruca iberiae", "Sylvia cantillans", scientific_name
    )
  ) |> 
  # summarize number of detection per species/confidence bin
  summarize(
    n_detect = n(), 
    .by = c(scientific_name, birdnet_name, confidence_range)
  ) |> 
  # calculate total detection per species
  mutate(total_audiomoth = sum(n_detect), .by = scientific_name) |> 
  # reshape: confidence bins become columns
  pivot_wider(
    names_from = confidence_range, 
    values_from = n_detect, 
    values_fill = 0, 
    names_sort = T, 
    names_prefix = "conf_"
  ) |> 
  # match species names to BirdLife taxonomy
  check_birdlife(species_name = scientific_name, birdnet_check = F) |>
  select(-scientific_name) |> 
  # merge with prevalence, census, abundance, and phylogeny tables
  left_join(wnvp, by = "birdlife_name") |> 
  left_join(census_df, by = "birdlife_name") |>
  left_join(abundance_pnae, by = "birdlife_name") |> 
  left_join(abundance_atlas, by = "birdlife_name") |> 
  left_join(birdlife_phylo, by = "birdlife_name") |> 
  # select and order columns for output table
  select(
    matches("^birdlife_[nfo]"), 
    birdnet_name, 
    starts_with("total"), 
    contains("atlas"), 
    contains("pnae"), 
    contains("prevalence"), 
    starts_with("conf_"), 
    everything()
  ) |> 
  # order species by group prevalence and total detection
  arrange(desc(group_prevalence), desc(total_audiomoth))


rm(birdlife_phylo, abundance_atlas, abundance_pnae, census_df, wnvp)

# save the final merged table to CSV
audio_df |> 
  write_csv(file.path(data_dir, "04_data_for_species_selection.csv"))

  