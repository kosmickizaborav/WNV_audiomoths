#' ---
#' title: "03_confidence_score_data"
#' output: github_document
#' ---

# INFO --------------------------------------------------------------------

#' **SECTION 1 - Standardize species to birdnet names**
#' check that scientific names from census data match birdnet names, 
#' there is automatic check in check_birdlife function that tries to match
#' the names to birdnet species list
#' 
#' **SECTION 2 - BirdNet confidence quantiles**


# 0 - Packages and directories --------------------------------------------

library(tidyverse)
library(here)
library(readxl)
library(EMbC)
library(ggrepel)
library(ggridges)
library(scales)
source(here("00_functions.R"))

# folder name where the data will be downloaded
data_dir <- here("Data")
census_dir <- file.path(data_dir, "Census")
birdnet_dir <- file.path(data_dir,"Birdnet")

gdir <- file.path(data_dir, "Graphs")
if(!dir.exists(gdir)){ dir.create(gdir) }

fin <- "2024_complete_data.rds"


# 1 - Standardize species to birdnet names---------------------------------

# list of the species that BirdNet would detect on the coordinates of cortalet
sp_cortalet <- file.path(data_dir, "species_cortalet.txt") |> 
  read_delim(
    col_names = c("scientific_name", "common_name"), 
    delim = "_", 
    show_col_types = F
  ) 

# data from atlas of nesting bird species in the park, provided by Julia
# all 91 species in the list of sp_cortalet, detectable by audiomoth
# sum(abundance_df$scientific_name %in% sp_cortalet$scientific_name)
abundance_df <- here("Data", "llista_abundancia_julia.xlsx") |> 
  read_xlsx() |> 
  check_birdlife(atlas_name) |> 
  # the following species was not found in sp_cortalet
  # Sylvia cantillans - Curruca iberiae - changed after consulting with Alex
  mutate(
    birdnet_name = if_else(
      atlas_name == "Sylvia cantillans", "Curruca iberiae", birdnet_name
    )
  ) |> 
  rename(scientific_name = birdnet_name, abundance_atlas = abundancia) |> 
  select(scientific_name, abundance_atlas)

# abundance information from the document on bird from Aiguamolls de l'Empordà
# abundance is indicated as category
abundance_aiguamolls <- file.path(
  data_dir, "02_augamolls_species_abundance.csv"
  ) |> 
  read_csv(show_col_types = F) |> 
  mutate(
    abundance_eng = factor(
      abundance_eng, 
      levels = c(
        "extinct", "accidental", "very rare", "rare", "common", "abundant"
      )
    ), 
    category_eng = factor(
      category_eng, 
      levels = c(
        "resident", "visitor", "reproducing",
        "wintering", "summer visitor", "migrating"
      )
    )
  ) |> 
  # if species was mentioned in 2 categories take the one with higher abundance
  # or the first one after arranging by category
  filter(
    n() == 1 | n() > 1 & abundance == max(abundance), .by = scientific_name
  ) |> 
  arrange(category_eng) |> 
  distinct(scientific_name, .keep_all = T) |> 
  # try to match to birdnet names
  check_birdlife(scientific_name) |> 
  mutate(
    birdnet_name = if_else(
      scientific_name == "Sylvia cantillans", "Curruca iberiae", birdnet_name
    )
  ) |> 
  # remove the species that don't have birdnet match
  filter(!is.na(birdnet_name)) |> 
  select(birdnet_name, abundance_eng, category_eng) |> 
  rename_with(~str_replace(.x, "_eng$", "_pnae"), everything()) |> 
  rename(scientific_name = birdnet_name) 

# there are 23 species that seem not to be in the list:
# sp_cortalet$scientific_name[!sp_cortalet$scientific_name %in% abundance_aiguamolls$scientific_name]
# "Sitta europaea"          
# "Pyrrhocorax pyrrhocorax"
# "Picus viridis"          
# "Carduelis citrinella"   
# "Pyrrhula pyrrhula"     
# "Prunella collaris"     
# "Cinclus cinclus"         
# "Poecile palustris"
# "Dryocopus martius"       
# "Pyrrhocorax graculus"    
# "Certhia familiaris"      
# "Tichodroma muraria"     
# "Passer hispaniolensis"   
# "Galerida theklae"        
# "Carduelis corsicana"     
# "Leiothrix lutea"        
# "Acrocephalus palustris"  
# "Euodice malabarica"      
# "Aegypius monachus"       
# "Sitta whiteheadi"       
# "Montifringilla nivalis"  
# "Passer italiae"          
# "Dendrocoptes medius"    

census_df <- file.path(census_dir, fin) |>
  read_rds() |> 
  # manually changing some names, because they are beyond automatic corrections
  mutate(
    scientific_name = case_when(
      # typos that couldn't be corrected automatically
      species == "Periparus cyaneus" ~ "Cyanistes caeruleus",
      species == "Cyaneus caeruleus" ~ "Cyanistes caeruleus",
      # alex said it's this
      species == "Cyanecula cyaneus" ~ "Cyanistes caeruleus",
      # this species is legitimate species, but it is not present in the park, 
      # it's just result of a typo
      birdlife_name == "Cyanistes cyanus" ~ "Cyanistes caeruleus",
      # alex said he was using this as a synonym, and he meant Saxicola rubicola
      birdlife_name == "Saxicola torquatus" ~ "Saxicola rubicola",
      birdlife_name == "Curruca cantillans" ~ "Curruca iberiae", 
      .default = birdlife_name
    )
  ) |> 
  select(-birdlife_name, -birdnet_name, -name_type) |> 
  check_birdlife(scientific_name) |> 
  # the only species that is not matched with birdnet names is Aquila fasciata, 
  # and it only has 2 records so it's removed from the analysis 
  filter(!is.na(birdnet_name)) |> 
  # 6 species are not present in sp_cortalet but are detected my Alex, 
  # so we excluded them, they exist in BirdNet so maybe one day we should rerun
  # with these species included
  # "Alopochen aegyptiaca"   
  # "Estrilda astrild"
  # "Phoenicopterus ruber"
  # "Pluvialis fulva"    
  # "Branta leucopsis"
  # "Limnodromus scolopaceus"
  filter(birdnet_name %in% sp_cortalet$scientific_name) |> 
  summarize(
    total = sum(total), 
    .by = c(birdnet_name, sector, transect, file_name, date)
  ) |> 
  mutate(sector = as.numeric(str_remove(sector, "s"))) |> 
  rename(scientific_name = birdnet_name) |> 
  left_join(abundance_df, by = "scientific_name") |> 
  left_join(abundance_aiguamolls, by = "scientific_name") 

census_df |> 
  write_rds(file.path(census_dir, str_c("03_birdnet_clean_", fin)))

census_total <- census_df |> 
  summarize(census_total = sum(total), .by = scientific_name) |> 
  left_join(abundance_df, by = "scientific_name") |> 
  left_join(abundance_aiguamolls, by = "scientific_name") 

rm(sp_cortalet, abundance_df, abundance_aiguamolls)



# 2 - BirdNet confidence quantiles----------------------------------------------

audio_df2 <- file.path(birdnet_dir, fin) |>
  read_rds() |> 
  select(scientific_name, confidence) |> 
  group_split(scientific_name) |> 
  map(~{
    
    sp_df <- .x
    
    if(nrow(sp_df) > 10){
      
      q_split <- quantile(sp_df$confidence, c(0.25, 0.5, 0.75, 1))
     
      sp_df |> 
        mutate(
          quant_cat = cut(
            confidence, 
            c(0.09, q_split), 
            right = T, 
            labels = c("(0-25%]", "(25-50%]", "(50-75%]", "(75-100%]")
          ) 
        ) |> 
        summarize(count = n(), .by = c(scientific_name, quant_cat)) |> 
        bind_cols(
          tribble(
            ~q1, ~q2, ~q3, ~q4,
            q_split["25%"], q_split["50%"], q_split["75%"], q_split["100%"]
          )
        ) 
      
    } else{
      
      tibble(
        scientific_name = unique(sp_df$scientific_name),
        comment = "less than 10 audiomoth detections"
      )
      
    }
    
  }) |> 
  list_rbind() |> 
  left_join(census_total, by = "scientific_name") |> 
  arrange(
    desc(abundance_atlas), 
    desc(abundance_pnae), 
    desc(census_total), 
    scientific_name, 
    quant_cat
  ) |> 
  pivot_wider(
    names_from = quant_cat, 
    values_from = count
  ) |> 
  select(-"NA") |> 
  select(scientific_name, contains("%"), contains("q[1-4]"), everything()) |> 
  select(-comment, comment)


audio_df |> 
  write_csv(file.path(birdnet_dir, "03_birdnet_confidence_ranges.csv"))

# ALL THE FUNCTIONS i USED FOR PLOTTING, NEEDS CLEANING
# 
# # FUNCTION: plot_heat_counts ----------------------------------------------
# 
# # setting color palette and its breaks
# pal_breaks <- c(0, 5, 10, 15, 25, 50, 100, 200, 500, Inf)
# pal <- paletteer::paletteer_d("trekcolors::lcars_2357") 
# names(pal) <- levels(cut(1, pal_breaks, right = T))
# 
# plot_heat_counts <- function(
#     df, x, y, fill, pal, 
#     x_tick_labs = NULL,
#     xlab = "confidence range", 
#     ylab = "species [ranked by abundance]", 
#     flab = NULL, 
#     title = NULL, 
#     subtitle = NULL
# ){
#   
#   p <- df |>
#     ggplot() +
#     geom_tile(aes(y = {{y}}, x = {{x}}, fill = {{fill}}), color = "black") +
#     scale_fill_manual(values = pal) +
#     labs(x = xlab, y = ylab, fill = flab, title = title, subtitle = subtitle) +
#     theme_minimal() +
#     theme(legend.position = "bottom") +
#     guides(fill = guide_legend(nrow = 1)) 
#   
#   if(!is.null(x_tick_labs)){
#     p <- p + scale_x_discrete(labels = x_tick_labs)
#   }
#   
#   return(p)
# }
# 
# 
# # 1 - Make plots for different confidence ranges --------------------------
# 
# 
# # define ranges of confidence scores
# conf_breaks <- tibble(
#   start = c(0.15, 0.2, 0.25), 
#   end = 1, 
#   step = c(0.05, 0.1, 0.15), 
#   name = c("0.05", "0.1", "0.15")
# )
# 
# # PLOT 1: counts per transect ---------------------------------------------
# 
# df_int <- conf_breaks |> 
#   group_split(name) |> 
#   map(~{
#     
#     # sequence of confidence ranges
#     cb <- seq(.x$start, .x$end, .x$step)
#     # plot name
#     int <- .x$name
#     pout <- str_c("4_conf_per_transect_int", int, ".jpeg")
#     
#     df <- am_df |> 
#       # only include the species that have abundance information
#       filter(!is.na(abundance)) |> 
#       mutate(scientific_name = fct_reorder(scientific_name, abundance)) |> 
#       # bin the confidence into ranges defined by cb
#       mutate(confidence_range = cut(confidence, c(0.09, cb), right = T)) |> 
#       # summarize the counts
#       summarize(
#         n_detect = n(), 
#         .by = c(scientific_name, confidence_range, transect)
#       )
# 
#     x_labs <- levels(df$confidence_range)
#     x_labs[seq(1, length(x_labs), 2)] <- ""
#     
#     if(int == 0.05){
#       x_labs[seq(4, length(x_labs), 4)] <- ""
#     }
#     
#     p1 <- df |> 
#       mutate(col_rank = cut(n_detect, pal_breaks, right = T)) |> 
#       plot_heat_counts(
#         x = confidence_range, 
#         y = scientific_name, 
#         x_tick_labs = x_labs,
#         fill = col_rank, 
#         pal = pal, 
#         flab = "detection count",
#         title = paste(
#           "Detections per transect, species and confidence score with the step of",
#           int
#         )
#       ) +
#       facet_wrap(~transect, nrow = 1) +
#       theme(panel.spacing = unit(1, "lines"))
#     
#     
#     p1 |> 
#       ggsave(
#         filename = file.path(gdir, pout), 
#         width = 45, 
#         height = 30, 
#         unit = "cm"
#       )
#     
#     
#     df |> 
#       mutate(int = int)
#         
#   })
#     
# 
# # PLOT 2: overview transects and intervals -------------------------------------
# 
# p2 <- df_int |> 
#   list_rbind() |> 
#   summarize(
#     n_detect = sum(n_detect), 
#     .by = c(scientific_name, confidence_range, int)
#   ) |> 
#   mutate(
#     col_rank = cut(n_detect, pal_breaks, right = T),
#     int = paste("confidence interval:", int)
#   ) |> 
#   plot_heat_counts(
#     x = confidence_range, 
#     y = scientific_name, 
#     fill = col_rank, 
#     pal = pal, 
#     flab = "detection count", 
#     title = "Distribution of confidence ranges across all transects"
#   ) +
#   facet_wrap(~int, nrow = 1, scales = "free_x") +
#   theme(panel.spacing = unit(1, "lines")) +
#   scale_x_discrete(guide = guide_axis(n.dodge = 2))
#   
# p2 |> 
#   ggsave(
#     filename = file.path(gdir, "4_all_transects_and_intervals.jpeg"), 
#     width = 45, 
#     height = 30, 
#     unit = "cm"
#   )
# 
# # 2 - EmBC classes ------------------------------------------------------------
# 
# df_embc <- am_df |> 
#   filter(!is.na(abundance)) |>
#   summarize(n_detect = n(), .by = c(scientific_name, abundance)) |> 
#   mutate(
#     abundance_log10 = log10(abundance),
#     n_detect_log10 = log10(n_detect)
#   ) 
#   # without two outliers, in the plot "4_embc_86species.jpeg"
#   #filter(!scientific_name %in% c("Passer domesticus", "Luscinia megarhynchos"))
# 
# # running EmBC on original and logaritmic values
# bc_out <- embc(as.matrix(df_embc[,c("abundance", "n_detect")]))
# bc_out_log <- embc(as.matrix(df_embc[,c("abundance_log10", "n_detect_log10")]))
# 
# # embc categories and colors used for plotting
# bc_labs <- data.frame(
#   value = 1:4,
#   name = c("LL", "LH", "HL", "HH"), 
#   col = c("#99EEFFFF", "#0055FFFF", "#FFCC66FF", "#FF5500FF") 
#   )
# 
# # mergina info from logartimic and original values
# df_embc_long <- df_embc |> 
#   mutate(
#     bc = bc_out@A, 
#     bc_log = bc_out_log@A
#   ) |> 
#   pivot_longer(
#     cols = c(bc, bc_log), 
#     names_to = "bc_type", 
#     values_to = "bc"
#   ) |> 
#   mutate(
#     # rename the classes
#     bc_lab = bc_labs$name[value = bc], 
#     bc_type = if_else(
#       str_detect(bc_type, "log"), 
#       "B) Classification based on logaritmic scale (log10)", 
#       "A) Classification based on the original values"
#       ), 
#     # fix abundances and detection counts
#     abundance_plot = case_when(
#       str_detect(bc_type, "logaritmic") ~ abundance_log10,
#       str_detect(bc_type, "original") ~ abundance,
#     ),
#     n_detect_plot = case_when(
#       str_detect(bc_type, "logaritmic") ~ n_detect_log10,
#       str_detect(bc_type, "original") ~ n_detect
#     )
#   ) |> 
#   select(-abundance, -abundance_log10, -n_detect, -n_detect_log10)
# 
# 
# # PLOT 3: Abundance vs. detection -----------------------------------------
# 
# pembc <- df_embc_long |> 
#   ggplot() + 
#   geom_point(
#     aes(x = abundance_plot, y = n_detect_plot, color = bc_lab), 
#     size = 2
#   ) + 
#   geom_text_repel(
#     aes(x = abundance_plot, y = n_detect_plot, label = scientific_name), 
#     max.overlaps = 10, 
#     box.padding = 0.5
#   ) +
#   theme_bw() +
#   scale_color_manual(values = bc_labs$col, breaks = bc_labs$name) +
#   labs(
#     y = "total number of detections", 
#     x = "abundance",
#     title = "EmBC classes based on number of detections and species abundance", 
#     subtitle = "Passer domesticus and Luscinia megarhynchos excluded", 
#     color = "BC class"
#   ) + 
#   theme(legend.position = "bottom") +
#   facet_wrap(~bc_type, scale = "free", ncol = 1)
# 
# pembc |>
#   ggsave(
#     filename = file.path(gdir, "4_embc_88species.jpeg"), 
#     width = 30, 
#     height = 30, 
#     unit = "cm"
#   )
# 
# 
# 
# # 3 - Species high detection no abundance --------------------------------------
# 
# df_counts <- am_df |> 
#   summarize(n_detect = n(), .by = c(scientific_name, abundance)) |> 
#   filter(n_detect >= median(df_embc$n_detect)) |> 
#   filter(is.na(abundance)) |>
#   arrange(desc(n_detect)) |>
#   select(-abundance) 
# 
# df_counts |> 
#   write_csv(here("Data", "4_detections_no_abundance.csv"))
# 
# 
# # 4 - Density plots of conficence ----------------------------------------------
# 
# am_df_sub <- am_df |> 
#   filter(!is.na(abundance)) |>
#   mutate(
#     scientific_name = fct_reorder(scientific_name, abundance), 
#     abundance_range = cut(
#       abundance, c(0, 100, 200, 1000, max(abundance)), 
#       right = T, 
#       labels = c("(0-100]", "(100-200]", "(200-1000]", "(1000-16000]")
#     ) 
#   ) 
#   
# 
# # PLOT5: Confidence scores density plots ----------------------------------
# 
# 
# am_df_sub |> 
#   group_split(abundance_range) |> 
#   map(~{
#     
#     df <- .x
#     abd_range <- unique(df$abundance_range)
#     
#     p <- df |> 
#       ggplot(
#         aes(
#           x = confidence, 
#           y = scientific_name, 
#           fill = factor(after_stat(quantile)), 
#         )
#       ) + 
#       geom_density_ridges_gradient(
#         scale = 1,
#         calc_ecdf = TRUE,
#         quantiles = 4, 
#         quantile_lines = TRUE, 
#         bandwidth = 0.05
#       ) +
#       scale_fill_viridis_d(name = "quantiles", alpha = 0.6) +
#       theme_minimal() +
#       theme(legend.position = "right") +
#       labs(
#         y = "", 
#         title = "Density distribution of confidence scores", 
#         subtitle = paste("for species with abundance within the range of", abd_range), 
#         caption = "bandwidth set to 0.05"
#       )
#     
#     p |>
#       ggsave(
#         filename = file.path(gdir, str_c("5_dens_", abd_range, ".jpeg")), 
#         width = 25, 
#         height = 35, 
#         unit = "cm"
#       )
#     
#     
#   })
#  
#   
# 
# # from the cow script 
# # bcsmth_0.9 <- smth(bc, dlta = 0.9)
# # save(bcsmth_0.9, file = file.path(out, "EMBC_output_smth_dlta_0.9.RData")) 
# # 
# # bcsmth_1 <- smth(bc)
# # save(bcsmth_1, file = file.path(out, "EMBC_output_smth_dlta_1.RData")) 
# 
# 
# 
# # 5 - Compare species -----------------------------------------------------
# 
# 
