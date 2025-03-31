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



check_birdlife <- function(df, species_name) {
  
  bird_file <- "Handbook of the Birds of the World and BirdLife International Digital Checklist of the Birds of the World_Version_9.xlsx"
  
  birdlife <- here(bird_file) |> 
    read_xlsx(skip = 2) |> 
    filter(!is.na(SISRecID)) |> 
    janitor::clean_names() |> 
    select(scientific_name, synonyms, family_name) |> 
    separate_longer_delim(synonyms, delim = ";") |>
    separate_longer_delim(synonyms, delim = ",") |>
    mutate(synonyms = str_squish(synonyms)) |>
    distinct(scientific_name, synonyms, family_name) |> 
    filter(scientific_name != synonyms | is.na(synonyms))  |> 
    mutate(sp_id = 1:n())
  
  bln <- birdlife |> 
    pivot_longer(
      cols = c(scientific_name, synonyms), 
      names_to = "name_type", 
      values_to = "sci_name", 
      values_drop_na = T
    ) |> 
    distinct(sci_name, name_type, .keep_all = T) |> 
    filter(
      n_distinct(name_type) == 1 & n() == 1 |
        n_distinct(name_type) == 2 & name_type == "scientific_name", 
      .by = sci_name
    ) |> 
    mutate(
      gen = str_split_i(sci_name, " ", 1), 
      epi = str_split_i(sci_name, " ", 2)
    )

  df <- df |> 
    mutate(spn = {{species_name}}) 
  
  correct <- df |> 
    distinct(spn) |> 
    left_join(
      bln |> 
        select(sci_name, sp_id), 
      by = c("spn" = "sci_name")
    )
  
  bln_gens <- bln |> 
    distinct(gen, family_name, name_type) |> 
    filter(!is.na(gen)) |> 
    filter(
      n_distinct(name_type) == 1 & n() == 1 |
        n_distinct(name_type) == 2 & name_type == "scientific_name", 
      .by = gen
    ) |> 
    select(-name_type) |> 
    filter(!duplicated(gen) | str_detect(gen, family_name)) 
  
  genus_correct <- correct |> 
    filter(is.na(sp_id)) |> 
    select(spn) |> 
    mutate(
      genus = str_split_i(spn, " ", 1), 
      epith = str_split_i(spn, " ", 2), 
      genus_corr = match_names(genus, bln_gens$gen, maxDist = 3), 
      epi_corr = match_names(epith, bln$epi[bln$gen == genus_corr], maxDist = 3), 
      spn_corr = str_c(genus_corr, epi_corr, sep = " "),
      .by = spn
    ) |> 
    left_join(
      bln |> 
        select(sp_id, sci_name),
      by = c("spn_corr" = "sci_name")
    ) |> 
    left_join(
      bln_gens |> 
        select(gen, family_name),
      by = c("genus" = "gen")
    ) |> 
    select(spn, spn_corr, sp_id, family_name) 
  
  
  species_correct <- genus_correct |> 
    filter(is.na(spn_corr)) |> 
    mutate(
      spn_corr = match_names(
          spn, birdlife$scientific_name[birdlife$family_name == family_name], 
          maxDist = 3
      ),
      .by = spn
    ) |> 
    select(spn, spn_corr)
    
  
  out <- correct |> 
    filter(!is.na(sp_id)) |> 
    select(spn) |> 
    bind_rows(
      genus_correct |> 
        filter(!is.na(sp_id)) |> 
        select(spn, spn_corr) 
    ) |> 
    bind_rows(species_correct) |> 
    mutate(
      birdlife_name = if_else(!is.na(spn_corr), spn_corr, spn)
    ) |> 
    left_join(
      bln |> 
        select(sp_id, sci_name, name_type), 
      by = c("birdlife_name" = "sci_name")
    ) |> 
    mutate(
      birdlife_name = if_else(
        is.na(name_type), 
        NA, 
        birdlife_name
      )
    ) |>
    mutate(
      alternative_for_synonym = if_else(
        str_detect(name_type, "synonym"), 
        birdlife$scientific_name[match(birdlife_name, birdlife$synonyms)], 
        NA
        )
    ) |>
    select(spn, birdlife_name, name_type, alternative_for_synonym) 
  
  df <- df |> 
    left_join(out, by = "spn") |> 
    select(-spn)
  
  return(df)
}


# 
# bird_file <- "Handbook of the Birds of the World and BirdLife International Digital Checklist of the Birds of the World_Version_9.xlsx"
# 
# birdlife <- here(bird_file) |> 
#   read_xlsx(skip = 2) |> 
#   janitor::clean_names() |> 
#   filter(subsp_seq == 0) |> 
#   select(scientific_name, synonyms) |> 
#   separate_longer_delim(synonyms, delim = ";") |>
#   separate_longer_delim(synonyms, delim = ",") |>
#   mutate(synonyms = str_squish(synonyms)) |> 
#   filter(scientific_name != synonyms | is.na(synonyms)) 
# 
# gen_nam <- unique(str_split_i(birdlife$scientific_name, " ", 1)) 
# gen_syn <- unique(str_split_i(birdlife$synonyms, " ", 1)) |>  na.omit()
# 
# df <- df |> 
#   mutate(spn = {{species_name}})
# 
# df_sp <- df |> 
#   distinct(spn) |> 
#   mutate(
#     in_birdlife = case_when(
#       is.na(spn) ~ NA, 
#       spn %in% birdlife$scientific_name ~ "correct_name", 
#       spn %in% birdlife$synonyms ~ "correct_synonym",
#       .default = "not_found"
#     )
#   ) |> 
#   select(spn, in_birdlife)
# 
# match_genus <- df_sp |> 
#   filter(in_birdlife == "not_found") |>
#   mutate(
#     genus = str_split_i(spn, " ", 1), 
#     epith = str_split_i(spn, " ", 2), 
#     gen_nam_match = match_names(genus, gen_nam, maxDist = 3),
#     gen_syn_match = match_names(genus, gen_syn, maxDist = 3), 
#     gen_corr = case_when(
#       !is.na(gen_nam_match) ~ gen_nam_match,
#       !is.na(gen_syn_match) ~ gen_syn_match,
#       .default = NA
#     ), 
#     gen_type = case_when(
#       !is.na(gen_nam_match) ~ "name_match",
#       !is.na(gen_syn_match) ~ "synonym_match",
#       .default = NA
#     ), 
#     spn_corr = paste(gen_corr, epith, sep = " ")
#   ) |> 
#   select(gen_corr, gen_type, spn_corr, spn) |> 
#   mutate(
#     in_birdlife = case_when(
#       is.na(spn) ~ NA, 
#       spn_corr %in% birdlife$scientific_name ~ "genus_match_name", 
#       spn_corr %in% na.omit(birdlife$synonyms) ~ "genus_match_synonym",
#       .default = "not_found"
#     )
#   ) 
# 
# match_sp <- match_genus |> 
#   filter(in_birdlife == "not_found") |> 
#   mutate(
#     spn_match = case_when(
#       gen_type == "name_match" ~ match_names(
#         spn_corr, birdlife$scientific_name[birdlife$gen_nam == gen_corr], 
#         maxDist = 5
#       ),
#       gen_type == "synonym_match" ~ match_names(
#         spn_corr, birdlife$synonyms[birdlife$gen_syn == gen_corr],
#         maxDist = 5
#       ), 
#       .default = NA
#     ), 
#     .by = spn
#   ) |> 
#   mutate(
#     in_birdlife = case_when(
#       is.na(spn) ~ NA, 
#       is.na(spn_match) ~ "not_fount",
#       gen_type == "name_match"~ "species_match_name_genus_corrected", 
#       gen_type == "synonym_match" ~ "species_match_synonym_genus_corrected"
#     )
#   ) 
# 
# 
# 
# 
# # searching for the match in the base (assuming typos - max 2) 
# corrected_names <- df_sp |> 
#   filter(in_birdlife == "not_found") |> 
#   mutate(
#     genus = str_split_i(spn, " ", 1), 
#     epith = str_split_i(spn, " ", 2), 
#     gen_nam_match = match_names(genus, birdlife$gen_nam, maxDist = 3),
#     gen_syn_match = match_names(genus, birdlife$gen_syn, maxDist = 3), 
#     gen_corr = case_when(
#       !is.na(gen_nam_match) ~ gen_nam_match,
#       !is.na(gen_syn_match) ~ gen_syn_match,
#       .default = NA
#     ), 
#     gen_type = case_when(
#       !is.na(gen_nam_match) ~ "name_match",
#       !is.na(gen_syn_match) ~ "synonym_match",
#       .default = NA
#     ), 
#     spn_corr = paste(gen_corr, epith, sep = " ")
#   ) |> 
#   select(-genus, gen_nam_match, gen_syn_match) |> 
#   mutate(
#     birdlife_match = case_when(
#       gen_type == "name_match" ~ match_names(
#         spn_corr, birdlife$scientific_name[birdlife$gen_nam == gen_corr],
#         maxDist = 5
#       ), 
#       gen_type == "synonym_match" ~ match_names(
#         spn_corr, birdlife$scientific_name[birdlife$gen_syn == gen_corr],
#         maxDist = 5
#       ),
#       .default = NA
#     ), 
#     .by = spn
#   )
# # name_match = match_names(spn, birdlife$scientific_name),
# # synonym_match = match_names(spn, birdlife$synonyms),
# pivot_longer(
#   cols = contains("_match"), 
#   names_to = "genus_type", 
#   values_to = "genus_corr", 
#   values_drop_na = T
# ) 
# # mutate(
# #   name_match = match_names(spn, birdlife$scientific_name),
# #   synonym_match = match_names(spn, birdlife$synonyms),
# # ) |>
# pivot_longer(
#   cols = contains("_match"), 
#   names_to = "match_type", 
#   values_to = "birdlife_match", 
#   values_drop_na = T
# ) |> 
#   select(match_type, birdlife_match, n_id)
# 
# # update the scientific names and give a column that suggest so
# out <- df_sp |> 
#   left_join(corrected_names, by = "n_id") |> 
#   mutate(
#     birdlife_status = if_else(
#       !is.na(match_type), 
#       match_type, 
#       in_birdlife, 
#       missing = NA
#     )
#   ) |> 
#   mutate(
#     birdlife_name = if_else(
#       str_detect(birdlife_status, "_match"), 
#       birdlife_match, 
#       spn, 
#       missing = NA 
#     )
#   ) |> 
#   mutate(
#     alternative_for_synonym = if_else(
#       str_detect(birdlife_status, "synonym"), 
#       birdlife$scientific_name[match(birdlife_name, birdlife$synonyms)], 
#       NA
#     )
#   ) |>
#   select(n_id, birdlife_name, birdlife_status, alternative_for_synonym) 
# 
# df <- df |> 
#   select(-spn) |> 
#   left_join(out, by = "n_id")
# 
# return(df)
# 
