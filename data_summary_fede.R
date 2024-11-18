library(tidyverse)
library(here)
library(readxl)
library(paletteer)
library(ggpubr)
library(ggrepel)


census <- here("Data", "Census") |> 
  list.files(pattern = "_complete_data.csv", full.names = T) |> 
  map(~read_csv(.x, show_col_types = F)) |> 
  bind_rows() |> 
  rename(transect_name = trans_official_name) |> 
  mutate(
    transect_name = str_replace_all(transect_name, " ", ""), 
    species = str_trim(species) |> 
      str_replace("sp$", "sp.")
  ) |> 
  summarize(
    total_census = sum(total, na.rm = T), 
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
  filter(species_code != "nocall", confidence >= 0.8) |> 
  left_join(audiomoth_field, by = c("data_id", "transect_name")) |> 
  left_join(
    here("Data", "species_cortalet.txt") |> 
      read_tsv(col_names = "name") |> 
      separate_wider_delim(
        name, 
        delim = "_", 
        names = c("species", "common_name")), 
    by = "common_name"
  ) |> 
  summarize(
    total_audiomoth = n(), 
    .by = c(species, month, transect_name, habitat)
  ) 
  # mutate(id = str_c(transect_name, "_", month))


month_map <- c("abril" = 4, "maig" = 5, "juny" = 6, 
               "juliol" = 7, "agost" = 8, "setembre" = 9, 
               "octubre" = 10, "novembre" = 11)

species_df_monthly <- census |> 
  left_join(
    audiomoth_field |> 
      distinct(transect_name, habitat),
    by = "transect_name"
  ) |> 
  select(-calendar_date) |> 
  full_join(
    audiomoths, by =  c("species", "habitat", "transect_name", "month")
  ) |> 
  mutate(
    method = case_when(
      !is.na(total_census) & !is.na(total_audiomoth) ~ "both",
      !is.na(total_census) & is.na(total_audiomoth) ~ "census",
      is.na(total_census) & !is.na(total_audiomoth) ~ "audiomoth"
    )
  ) |> 
  mutate(month = month_map[month] |> month(label = T)) 

species_df_monthly |> 
  write_csv(here("Data", "total_counts_audiomoth_vs_census.csv"))

species_df_monthly <- species_df_monthly |> 
  pivot_longer(
    cols = contains("total"), 
    values_to = "total", 
    names_to = "total_type",
    values_drop_na = T
  ) 



species_df <- census |> 
  left_join(
    audiomoth_field |> 
      distinct(transect_name, habitat),
    by = "transect_name"
  ) |> 
  summarize(
    total_census = sum(total_census), 
    .by = c(species, transect_name, habitat)
  ) |> 
  full_join(
    audiomoths |> 
      summarize(
        total_audiomoth = sum(total_audiomoth), 
        .by = c(species, transect_name, habitat)
      ), 
    by =  c("species", "habitat", "transect_name")
  ) |> 
  mutate(
    method = case_when(
      !is.na(total_census) & !is.na(total_audiomoth) ~ "both",
      !is.na(total_census) & is.na(total_audiomoth) ~ "census",
      is.na(total_census) & !is.na(total_audiomoth) ~ "audiomoth"
    )
  ) 

rm(audiomoth_field)


pal <- c(audiomoth = "#4F3855FF", census = "#9BB655FF", both = "#D89873FF")



# p1 ----------------------------------------------------------------------



{
  species_df_monthly |> 
    distinct(species, habitat, month, method) |> 
    summarize(
      n_sp = length(unique(species)), 
      .by = c(habitat, month, method)
    ) |> 
    ggplot() +
    geom_bar(
      aes(x = habitat, y = n_sp, fill = method), 
      stat="identity", 
      color = "gray33",
      position=position_dodge(), 
      alpha = 0.7
    ) + 
    facet_wrap(~month, ncol = 2) +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(
      y = "total number of species detected", 
      fill = "method"
    ) + 
    scale_fill_manual(values = pal)
  
  } |> 
  #scale_fill_paletteer_d("ggthemes::excel_Office_2007_2010")
  annotate_figure(
    top = text_grob(
      "Total number of species detected by different methods across months and habitats", 
      face = "bold", size = 12
      #paste(sp, "- distances |", f_id), face = "bold", size = 12
    )
  )
#paletteer_d("lisa::FridaKahlo")[c(2, 4)]
  

ggsave(here("Data", "fede_total_number_of_species.pdf"), width = 20, units = "cm")



# p2 ----------------------------------------------------------------------



top_5 <- species_df_monthly |> 
  summarize(
    total = sum(total), 
    .by = c(species, total_type, habitat)
  ) |> 
  group_by(total_type, habitat) |> 
  arrange(desc(total), .by_group = T) |> 
  slice_head(n = 5) |> 
  ungroup() |> 
  group_by(total_type) |> 
  arrange(desc(total_type)) |> 
  ungroup()

lvs <- top_5 |> 
  distinct(species)
  
{
  top_5 |> 
    mutate(
      species = factor(species, levels = lvs$species), 
      method = str_split_i(total_type, "_", 2)
    ) |> 
    ggplot() +
    geom_bar(
      aes(x = total, y = species, fill = method), 
      stat = "identity", 
      color = "gray33", 
      alpha = .7
    ) + 
    facet_wrap(~habitat, nrow = 1, scales = "free_x") + 
    labs(x = "times detected") +
    theme_bw() + 
    scale_fill_manual(values = pal) +
    #scale_fill_paletteer_d("ggthemes::excel_Office_2007_2010") +
    theme(legend.position = "bottom")
  } |> 
 annotate_figure(
   top = text_grob(
     "Top 5 species by frequency of detection by different methods", 
     face = "bold", size = 12
     #paste(sp, "- distances |", f_id), face = "bold", size = 12
   )
 )
 
 ggsave(here("Data", "fede_top_5_species_general.pdf"), width = 25, units = "cm")
 

# p3 ----------------------------------------------------------------------

 both_df <- species_df|> 
   filter(method == "both") |> 
   group_by(habitat) |> 
   arrange(desc(total_audiomoth)) |> 
   slice_head(n = 5) |> 
   ungroup() |> 
   bind_rows(
     species_df |> 
       filter(method == "both") |> 
       group_by(habitat) |> 
       arrange(desc(total_census)) |> 
       slice_head(n = 5) |> 
       ungroup() 
   ) |> 
   pivot_longer(
     cols = contains("total"), 
     values_to = "total", 
     names_to = "total_type",
     values_drop_na = T
   ) |> 
   mutate(method = str_split_i(total_type, "_", 2))
 
 lvs <- both_df |> 
   distinct(species)
 
 {
   both_df |> 
     mutate(species = factor(species, levels = lvs$species)) |> 
     ggplot() +
     geom_bar(
       aes(x = total, y = species, fill = method), 
       stat = "identity", 
       color = "gray33",
       position = position_dodge(),
       alpha = .7
     ) + 
     facet_wrap(~habitat, nrow = 1, scales = "free_x") + 
     labs(x = "times detected") +
     theme_bw() + 
     scale_fill_manual(values = pal) +
     #scale_fill_paletteer_d("ggthemes::excel_Office_2007_2010") +
     theme(legend.position = "bottom")
   
   } |> 
   annotate_figure(
     top = text_grob(
       "Top 5 species by frequency of detection registered by both methods", 
       face = "bold", size = 12
       #paste(sp, "- distances |", f_id), face = "bold", size = 12
     )
   )
 
 ggsave(here("Data", "fede_top_5_species_matching.pdf"), width = 25, units = "cm")
 
 

# p4 ----------------------------------------------------------------------

target_sp <- c("Anas platyrhynchos", "Columba livia", 
               "Turdus merula", "Circus aeruginosus" , "Sturnus vulgaris")
 
 
 {
   species_df_monthly |> 
     filter(
       species %in% target_sp
     ) |> 
     mutate(method2 = str_split_i(total_type, "_", 2)) |> 
     summarize(
       total = sum(total), 
       .by = c(species, month, method2)
     ) |> 
     ggplot() +
     geom_bar(
       aes(y = species, x = total, fill = method2), 
       stat="identity", 
       position = position_dodge(),
       color = "gray33",
       alpha = 0.7
     ) + 
     facet_wrap(~month, ncol = 2, scales = "free_x") +
     theme_bw() +
     theme(legend.position = "bottom") +
     labs(
       y = "total number of species detected", 
       fill = "method"
     ) + 
     scale_fill_manual(values = pal)
   
 } |> 
   #scale_fill_paletteer_d("ggthemes::excel_Office_2007_2010")
   annotate_figure(
     top = text_grob(
       "Performance of different methods for target species", 
       face = "bold", size = 12
       #paste(sp, "- distances |", f_id), face = "bold", size = 12
     )
   )
 #paletteer_d("lisa::FridaKahlo")[c(2, 4)]
 
 
 ggsave(here("Data", "fede_target_species.pdf"), width = 20, units = "cm")
 
 
  
