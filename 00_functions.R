library(tidyverse)
library(readxl)
library(here)
library(stringdist)


# 1 match_to_birdlife --------------------------------------------------------

# 1.0. small function for matching scientific names in case of typos --------

match_names <- function(
    species, 
    scientific_names, 
    maxDist = 2, 
    matchNA = F, 
    method = "lv"
) {
  
  loc <- amatch(
    species, 
    scientific_names, 
    maxDist = maxDist, 
    matchNA = matchNA, 
    method = method
  )
  
  scientific_names[loc]
  
}


# 1.1. full function for checking names -----------------------------------

# used for testing
# species_name <- bird_db$scientific_name



check_birdlife <- function(
    df, species_name, max_d = 3, synonyms_to_sci_names = F
    ) {
  
  # downloaded from: 
  # https://datazone.birdlife.org/about-our-science/taxonomy
  bird_file <- "Handbook of the Birds of the World and BirdLife International Digital Checklist of the Birds of the World_Version_9.xlsx"
  
  birdlife <- here(bird_file) |> 
    read_xlsx(skip = 2) |> 
    # take only the species level, not subspecies
    filter(!is.na(SISRecID)) |> 
    janitor::clean_names() |> 
    select(scientific_name, synonyms, family_name) |> 
    # remove genus in parenthesis if provided
    mutate(synonyms = str_remove_all(synonyms, "\\([^\\)]*\\)")) |> 
    # change all the punctuation to ";"
    mutate(synonyms = str_replace_all(synonyms, "[:punct:]", ";")) |> 
    # extract synonyms one per row
    separate_longer_delim(synonyms, delim = ";") |> 
    # clean extra white spaces
    mutate(synonyms = str_squish(synonyms)) |>
    # eliminate duplicates and repeated name as species and synonyms
    distinct(scientific_name, synonyms, family_name) |> 
    filter(scientific_name != synonyms | is.na(synonyms))  |> 
    # assign a id number just for easier organization
    mutate(sp_id = 1:n())
  
  # extract all the names and synonyms in one table
  bln <- birdlife |> 
    pivot_longer(
      cols = c(scientific_name, synonyms), 
      names_to = "name_type", 
      values_to = "sci_name", 
      values_drop_na = T
    ) |> 
    filter(str_count(sci_name, " ") == 1) |> 
    distinct(sci_name, name_type, .keep_all = T) |> 
    # if a scientific name is also found in synonyms keep the scientific name
    filter(
      n_distinct(name_type) == 1 |
      n_distinct(name_type) == 2 & name_type == "scientific_name", 
      .by = sci_name
    ) |> 
    # extract genus and species epithaf
    mutate(
      gen = str_split_i(sci_name, " ", 1), 
      epi = str_split_i(sci_name, " ", 2)
    )
  
  # extract genus and family names
  bln_gens <- bln |> 
    distinct(gen, family_name, name_type) |> 
    # if the genus is duplicated and the same type take the first one, 
    # otherwise take the one that is labeled as scientific_name
    filter(
      n_distinct(name_type) == 1 & n() == 1 |
        n_distinct(name_type) == 2 & name_type == "scientific_name", 
      .by = gen
    ) |> 
    select(-name_type) |> 
    # if the genuses are still duplicated keep the family that is closest to
    # the genus name
    filter(!duplicated(gen) | str_detect(gen, family_name)) 
  
  # copy the species name column from the original data for easier manipulation
  df <- df |> 
    mutate(spn = {{species_name}}) 
  
  # find the species names that are correct
  correct <- df |> 
    distinct(spn) |> 
    left_join(
      bln |> 
        select(sci_name, sp_id), 
      by = c("spn" = "sci_name")
    )
  
  # correct the genus and then correct the epithaph of the species within
  # that genus
  genus_correct <- correct |> 
    filter(is.na(sp_id)) |> 
    select(spn) |> 
    mutate(
      genus = str_split_i(spn, " ", 1), 
      epith = str_split_i(spn, " ", 2), 
      # correct the genus
      genus_corr = match_names(
        genus, bln_gens$gen, maxDist = max_d
      ), 
      epi_corr = match_names(
        epith, bln$epi[bln$gen == genus_corr], maxDist = max_d
      ), 
      spn_corr = str_c(genus_corr, epi_corr, sep = " "),
      .by = spn
    ) |> 
    left_join(
      bln |> 
        select(sp_id, sci_name),
      by = c("spn_corr" = "sci_name")
    ) |> 
    # add family name for the last correction
    left_join(
      bln_gens |> 
        select(gen, family_name),
      by = c("genus" = "gen")
    ) |> 
    select(spn, spn_corr, sp_id, family_name) 
  
  # if no match was found in the previous correction, match the genus, and
  # search within the family for the species
  species_correct <- genus_correct |> 
    filter(is.na(spn_corr)) |> 
    mutate(
      spn_corr = match_names(
          spn, birdlife$scientific_name[birdlife$family_name == family_name], 
          maxDist = max_d
      ),
      .by = spn
    ) |> 
    select(spn, spn_corr)
    
  # output
  out <- correct |> 
    # remove species that were not correct
    filter(!is.na(sp_id)) |> 
    select(spn) |> 
    # add species that are found by genus
    bind_rows(
      genus_correct |> 
        filter(!is.na(sp_id)) |> 
        select(spn, spn_corr) 
    ) |> 
    # add the rest
    bind_rows(species_correct) |> 
    # if there is a corrected name take it otherwise keep the original
    mutate(
      birdlife_name = if_else(!is.na(spn_corr), spn_corr, spn)
    ) |> 
    # add the name type
    left_join(
      bln |> 
        select(sp_id, sci_name, name_type), 
      by = c("birdlife_name" = "sci_name")
    ) |> 
    # if the name was not matches with birdlife return NA
    mutate(
      birdlife_name = ifelse(
        is.na(name_type), 
        NA, 
        birdlife_name
      )
    ) |>
    mutate(
      alternative_for_synonym = ifelse(
        str_detect(name_type, "synonym"), 
        birdlife$scientific_name[match(birdlife_name, birdlife$synonyms)], 
        NA
        )
    ) |>
    select(spn, birdlife_name, name_type, alternative_for_synonym) 
  
  df <- df |> 
    left_join(out, by = "spn") |> 
    select(-spn)
  
  # if we want to change the synonyms direclty to the accepted scientific names
  if(synonyms_to_sci_names) {
    
    df <- df |> 
      mutate(
        birdlife_name = if_else(
          str_detect(name_type, "synonym"), 
          alternative_for_synonym, 
          birdlife_name
        ) 
      ) |> 
      select(-alternative_to_synonym)
          
  }
  
  return(df)
  
}

