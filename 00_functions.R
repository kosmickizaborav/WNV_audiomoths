library(tidyverse)
library(readxl)
library(here)
library(stringdist)


# 1 - FUNCTION: match_names -----------------------------------------------

# small function for matching scientific names in case of typos 
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



# FUNCTION: check_birdlife ------------------------------------------------


check_birdlife <- function(df, species_name, max_d = 3, birdnet_check = T) {
  
  # obtained from BirdNet Analyser files in section Data
  birdnet_sp <- here("BirdNET_GLOBAL_6K_V2.4_Labels.txt") |> 
    read_delim(
      col_names = c("scientific_name", "common_name"), 
      delim = "_", 
      show_col_types = F
    ) |> 
    pull(scientific_name)
  
  # downloaded from: 
  # https://datazone.birdlife.org/about-our-science/taxonomy
  bird_file <- "Handbook of the Birds of the World and BirdLife International Digital Checklist of the Birds of the World_Version_9.xlsx"
  
  birdlife <- here(bird_file) |> 
    read_xlsx(skip = 2) |> 
    # take only the species level, not subspecies
    janitor::clean_names() |> 
    rename(sp_status = x2024_iucn_red_list_category) |> 
    select(seq, scientific_name, synonyms, family_name, sp_status) |> 
    filter(!is.na(seq)) |> 
    distinct(seq, .keep_all = T) |> 
    # remove information in parenthesis is added to the synonyms column
    mutate(synonyms = str_remove_all(synonyms, "\\([^\\)]*\\)")) |> 
    # change all the punctuation to ";"
    mutate(synonyms = str_replace_all(synonyms, "[:punct:]", ";")) |> 
    # extract synonyms one per row
    separate_longer_delim(synonyms, delim = ";") |> 
    # remove labels for variants and subspecies
    # remove synonyms that are not a full species name but just an epitaph or
    # genus, it's the product of previous steps
    mutate(synonyms = str_remove_all(synonyms, "\\s(sp$|var$|ssp$)")) |>
    # making sure there is no extra white spaces
    mutate(across(everything(), str_squish)) |> 
    mutate(
      # removing the synonyms that are just the epitaph or genus name
      # or the ones that are identical to the scientific name
      synonyms = case_when(
        str_count(synonyms, " ") !=1 ~ NA,
        scientific_name == synonyms ~ NA,
        .default = synonyms
      ), 
      # change the status to NR - not recognized and R - regonized
      sp_status = if_else(sp_status == "NR", "NR", "R")
    ) |> 
    distinct(scientific_name, synonyms, family_name, sp_status, .keep_all = T) |> 
    mutate(
      # checked that they don't have any synonyms, so I just manually added them
      synonyms = case_when(
        scientific_name == "Myiopsitta monachus" ~ "Psittacula monachus",
        scientific_name == "Cyanistes caeruleus" ~ "Parus caeruleus",
        scientific_name == "Larus melanocephalus" ~ "Ichthyaetus melanocephalus",
        scientific_name == "Larus genei" ~ "Chroicocephalus genei",
        scientific_name == "Larus audouinii" ~ "Ichthyaetus audouinii",
        scientific_name == "Corvus monedula" ~ "Coloeus monedula", 
        .default = synonyms
      ), 
      synonyms = if_else(
        str_detect(scientific_name, "Curruca") & is.na(synonyms), 
        paste("Sylvia", str_split_i(scientific_name, " ", 2)), 
        synonyms
      )
    ) |> 
    bind_rows(
      tribble(
        ~scientific_name, ~synonyms, ~family_name, ~sp_status,
        "Curruca curruca", "Sylvia curruca", "Sylviidae", "R"
      )
    ) |> 
    # check how many times the species appears, 
    # whether it is both listed as recognized and not
    # how many distinct synonyms it has
    mutate(
      n_sci = n(),
      both_r_nr = sum(c("R", "NR") %in% sp_status) == 2, 
      dist_syn = n_distinct(synonyms, na.rm = T),
      .by = scientific_name
    ) |> 
    # check unique combos
    mutate(n_pair = n(), .by = c(scientific_name, synonyms)) |> 
    mutate(
      sci_in_syn = scientific_name %in% unique(synonyms), 
      syn_in_sci = synonyms %in% unique(scientific_name)
    ) |> 
    mutate(
      sci_name = case_when(
        # if scientific name is both listed as recognized and not recognized, 
        # remove the non recognized if it has no additional synonyms
        n_sci > 1 & both_r_nr & sp_status == "NR" & is.na(synonyms) ~ NA,
        # if it does have synonyms, make sure they are identical as in recognized
        n_sci > 1 & both_r_nr & sp_status == "NR" & n_pair > 1 ~ NA,
        # if the scientific name is duplicated but one of the entries doesn't
        # have synonym provided, remove it
        n_sci > 1 & dist_syn > 0 & is.na(synonyms) ~ NA, 
        # if the scientific name is provided also found in the synonyms, but
        # it's listed as not recognized, remove it if there is no synonym provided
        sci_in_syn & sp_status == "NR" & is.na(synonyms) ~ NA,
        .default = scientific_name
      )
    ) |> 
    filter(!is.na(sci_name)) |> 
    mutate(
      sp_status = if_else(sp_status == "NR" & both_r_nr == T, "R", sp_status),
      syn_in_sci = synonyms %in% unique(scientific_name), 
      synonyms = if_else(syn_in_sci, NA, synonyms)
    ) |> 
    filter(
      !(n() > 1 & n_distinct(synonyms, na.rm = T) > 0 & is.na(synonyms)), 
      .by = scientific_name
    ) |> 
    select(scientific_name, synonyms, family_name, sp_status) |>
    # assign a id number just for easier organization
    mutate(sp_id = 1:n()) |> 
    rename(synonym = synonyms) |> 
    mutate(
      birdnet_name = case_when(
        scientific_name %in% birdnet_sp ~ scientific_name,
        synonym %in% birdnet_sp ~ synonym,
        .default = NA
      )
    )
  
  # extract all the names and synonyms in one table
  bln <- birdlife |> 
    pivot_longer(
      cols = c(scientific_name, synonym), 
      names_to = "name_type", 
      values_to = "sci_name", 
      values_drop_na = T
    ) |>  
    # unique scientific names and synonyms
    distinct(sci_name, name_type, .keep_all = T) |> 
    mutate(
      name_type = if_else(
        sp_status == "R", 
        paste("recognized" , str_replace(name_type, "_", " ")), 
        paste("not recognized", str_replace(name_type, "_", " "))
      )
    ) |> 
    # if a scientific name is also found in synonyms keep the scientific name
    # extract genus and species epithaf
    mutate(
      gen = str_split_i(sci_name, " ", 1), 
      epi = str_split_i(sci_name, " ", 2)
    )
  
  # extract genus and family names
  bln_gens <- bln |> 
    distinct(gen, family_name) |> 
    mutate(str_sim = stringsim(gen, family_name)) |> 
    filter(n() == 1 | str_sim == max(str_sim), .by = gen) |> 
    distinct(gen, family_name)
  
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
  
  # correct the genus and then correct the epitaph of the species within
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
        birdlife$scientific_name[match(birdlife_name, birdlife$synonym)], 
        NA
        )
    ) |>
    select(spn, birdlife_name, name_type, alternative_for_synonym) 
  
  df <- df |> 
    left_join(out, by = "spn") |> 
    select(-spn)
  
  df <- df |> 
    mutate(
      birdlife_name = if_else(
        str_detect(name_type, "synonym"), 
        alternative_for_synonym, 
        birdlife_name
      ) 
    ) |> 
    select(-alternative_for_synonym)
  
  if(birdnet_check){
    
    df <- df |> 
      mutate(
        birdnet_name = birdlife$birdnet_name[match(birdlife_name, birdlife$scientific_name)], 
      )
    
  }
  
  return(df)
  
}

