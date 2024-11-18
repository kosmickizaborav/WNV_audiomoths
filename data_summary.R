library(tidyverse)
library(here)
library(readxl)
library(paletteer)
library(ggpubr)
library(ggrepel)

species_cortalet <- here("Data", "species_cortalet.txt") |> 
  read_tsv(col_names = "name") |> 
  separate_wider_delim(name, delim = "_", names = c("species", "common_name"))

census <- here("Data", "Census") |> 
  list.files(pattern = ".csv", full.names = T) |> 
  map(~read_csv(.x, show_col_types = F)) |> 
  bind_rows() |> 
  rename(transect_name = trans_official_name) |> 
  mutate(
    transect_name = str_replace_all(transect_name, " ", ""), 
    species = str_trim(species) |> 
      str_replace("sp$", "sp.")
  ) |> 
  summarize(
    total_census = sum(total), 
    .by = c(species, transect_name, calendar_date, month)
  ) |> 
  filter(!str_detect(species, " sp.$")) 
  

audiomoth_field <- here("Data", "Birdnet", "audiomoth_field_data.xlsx")|> 
  read_xlsx() |> 
  janitor::clean_names() |> 
  filter(!is.na(data_id)) |>
  rename(transect_name = transect_name_alex) |> 
  select(habitat, transect_name, data_id, start_date, end_date) |> 
  group_split(data_id) |> 
  map(~{
    
    m <- census |>
      distinct(transect_name, calendar_date, month) |> 
      filter(calendar_date >= .x$start_date & calendar_date <= .x$end_date) |> 
      distinct(month) |> 
      pull()
    
    if(length(m) != 0){
      .x |> 
        mutate(month = m)
    } else(
      .x
    )
    
  }) |> 
  bind_rows() 
  
audiomoths <- here("Data", "Birdnet") |> 
  list.files(pattern = "_complete_data.csv", full.names = T) |> 
  map(~read_csv(.x, show_col_types = F)) |> 
  bind_rows() |> 
  filter(species_code != "nocall") |>
  filter(confidence >= 0.8) |> 
  left_join(audiomoth_field, by = c("data_id", "transect_name")) |> 
  left_join(species_cortalet, by = "common_name") |> 
  summarize(
    total_audiomoth = n(), 
    .by = c(species, month, transect_name)
  ) 
  # mutate(id = str_c(transect_name, "_", month))


month_map <- c("abril" = "April", "maig" = "May", "juny" = "June", 
               "juliol" = "July", "agost" = "August", "setembre" = "September", 
               "octubre" = "October", "novembre" = "November")

month_map <- c("abril" = 4, "maig" = 5, "juny" = 6, 
               "juliol" = 7, "agost" = 8, "setembre" = 9, 
               "octubre" = 10, "novembre" = 11)

sp_df <- census |> 
  summarize(
    n_sp = length(unique(species)),
    .by = c(transect_name, month)
  ) |> 
  mutate(type = "census") |> 
  bind_rows(
    audiomoths |>  
      summarize(
        n_sp = length(unique(species)), 
        .by = c(transect_name, month)
      ) |> 
      mutate(type = "audiomoth")
  ) |> 
  left_join(
    audiomoth_field |> 
      distinct(transect_name, habitat)
  ) |> 
  mutate(month_eng = month_map[month] |> month(label = T))
  

# audiomoths_sp <- audiomoths |>  
#   summarize(
#     n_sp = length(unique(species)), 
#     .by = c(transect_name, month)
#   )

pal <- paletteer_d("vangogh::SelfPortrait")


p1 <- sp_df |> 
  ggplot() +
  geom_bar(
    aes(x = habitat, y = n_sp, fill = type), 
    stat="identity", 
    color = "gray33",
    position=position_dodge(), 
    alpha = 0.7
  ) + 
  facet_wrap(~month_eng, ncol = 2) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(
    y = "total number of species detected", 
    fill = "method"
  ) + 
  scale_fill_manual(values = pal[c(1,5)]) 
  
  annotate_figure(
    p1, 
    top = text_grob(
      "Total number of species detected by different methods across months and habitats", 
      face = "bold", size = 12
      #paste(sp, "- distances |", f_id), face = "bold", size = 12
    )
  )
#paletteer_d("lisa::FridaKahlo")[c(2, 4)]
  

ggsave(here("total_number_of_species.pdf"), width = 20, units = "cm")


top_10_census <- census |> 
  left_join(
    audiomoth_field |> 
      distinct(transect_name, habitat)
  ) |> 
  group_split(habitat) |> 
  map(~{
    .x |> 
      summarize(
      total_census = sum(total_census), 
      .by = species
    ) |> 
      slice_max(n = 5, order_by = total_census) |> 
      mutate(habitat = unique(.x$habitat))
  }) |> 
  bind_rows()
  

top_10_audiomoth <- audiomoths |> 
  left_join(
    audiomoth_field |> 
      distinct(transect_name, habitat)
  ) |> 
  group_split(habitat) |> 
  map(~{
    .x |> 
      summarize(
        total_audiomoth = sum(total_audiomoth), 
        .by = species
      ) |> 
      slice_max(n = 5, order_by = total_audiomoth) |> 
      mutate(habitat = unique(.x$habitat))
  }) |> 
  bind_rows()
  




top_10_df <- top_10_census |> 
  full_join(top_10_audiomoth) |> 
  mutate(
    method = case_when(
      !is.na(total_census) & !is.na(total_audiomoth) ~ "both", 
      !is.na(total_census) & is.na(total_audiomoth) ~ "census", 
      is.na(total_census) & !is.na(total_audiomoth) ~ "audiomoth"
    )
  ) |> 
  mutate(across(contains("total"), ~ifelse(is.na(.x), 0, .x))) 

top_10_df |> 
  ggplot() +
  # geom_label_repel(
  #   aes(x = total_census, y = total_audiomoth, label = species),  
  #   force = 10
  # ) +
  geom_point(aes(x = total_census, y = total_audiomoth, color = method)) +
  facet_wrap(~habitat, scales = "free") + 
  theme_bw() +
  scale_color_manual(values = pal[c(1,3,5)])
  scale_color_manual(values = c("#D55E00", "black", "#009E73"))
 

full_df |> 
  summarize(
    n_species = length(unique(species)), 
    n_rows = n()
  ) |>

  
