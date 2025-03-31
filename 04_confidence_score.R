# packages and directories ------------------------------------------------

library(tidyverse)
library(here)
library(readxl)
library(EMbC)
library(ggrepel)
library(ggridges)
library(scales)
source(here("00_functions.R"))


gdir <- here("Data", "Graphs")
if(!dir.exists(gdir)){ dir.create(gdir) }

birdnet_dir <- here("Data", "Birdnet")
census_dir <- here("Data", "Census")


# 0 - Data preparation ----------------------------------------------------

# list of the species that BirdNet would detect on the coordinates of cortalet
sp_cortalet <- here("Data", "species_cortalet.txt") |> 
  read_delim(col_names = c("scientific_name", "common_name"), delim = "_") 

# abundance data provided by Julia
abundance_df <- here("Data", "llista_abundancia_julia.xlsx") |> 
  read_xlsx() |> 
  # the following two species are not present in the sp_cortalet list, but
  # their synonims are, so we change them to synonims so that we can merge
  # data later
  # Sylvia melanocephala -> Curruca melanocephala
  # Sylvia hortensis - not in sp_cortalet, but synonim Curruca hortensis     
  mutate(
    scientific_name = case_when(
      atlas_name == "Sylvia melanocephala" ~ "Curruca melanocephala",
      atlas_name == "Sylvia hortensis" ~ "Curruca hortensis",
      atlas_name == "Sylvia cantillans" ~ "Curruca iberiae",
      .default = atlas_name
    )
  ) |> 
  rename(abundance = abundancia) |> 
  select(scientific_name, abundance, atlas_name)

# audiomoth data detected per transect
am_df <- birdnet_dir |> 
  list.files(pattern= "complete_data.csv") |> 
  map(~{
    
    fin <- .x 
    
    # loading audiomoth data
    here(birdnet_dir, fin) |> 
      read_csv(show_col_types = F) |> 
      mutate(in_file = fin) |> 
      filter(species_code != "nocall") |> 
      select(common_name, confidence, data_id, datetime, transect_name)
    
  }) |> 
  bind_rows() |> 
  # adding scientific name to the codes that BirdNet uses
  left_join(sp_cortalet, by = "common_name") |> 
  # 88 species/out of 91 are common for both datasets
  # missing species: 
  # "Charadrius alexandrinus" - present in sp_cortalet just not detected
  # "Egretta garzetta" - present in sp_cortalet just not detected  
  # "Sylvia cantillans" - not present in sp_cortalet, with Fede we found the synonym
  # Curruca iberiae  
  left_join(abundance_df, by = "scientific_name") |> 
  arrange(
    desc(abundance), 
    desc(confidence), 
    transect_name, 
    scientific_name
  ) |> 
  mutate(
    transect_name = str_replace_all(
      str_remove(transect_name, "0"), "_|\\.", "-"
    )
  )

# Alex's transect data 
alex_counts <- census_dir |> 
  list.files(pattern = "2024_", full.names = T) |> 
  map(~read_csv(., show_col_types = F)) |> 
  list_rbind() |> 
  filter(total > 0) |> 
  select(species, total, trans_official_name) |> 
  mutate(
    species = str_remove_all(species, "[0-9[:punct:]]") |> str_squish()
  ) |> 
  filter(!str_detect(species, " sp$"), str_count(species, " ") == 1) |> 
  # what is this
  # Cyanecula cyaneus
  # Periparus cyaneus
  # 
  mutate(
    species = case_when(
      species == "Sylvia melanocephala" ~ "Curruca melanocephala",
      species == "Cyaneus caeruleus" ~ "Cyanistes caeruleus",
      species == "Parus caeruleus" ~ "Cyanistes caeruleus",
      species == "Psittacula monachus" ~ "Myiopsitta monachus",
      .default = species
    )
  ) |> 
  check_birdlife(species) |> 
  mutate(
    scientific_name = if_else(
      name_type == "synonym", alternative_for_synonym, birdlife_name
    )
  ) |> 
  select(scientific_name, total, trans_official_name) |> 
  summarize(
    total = sum(total), 
    .by = c(scientific_name, trans_official_name)
  )

  

rm(abundance_df, sp_cortalet)




# FUNCTION: plot_heat_counts ----------------------------------------------

# setting color palette and its breaks
pal_breaks <- c(0, 5, 10, 15, 25, 50, 100, 200, 500, Inf)
pal <- paletteer::paletteer_d("trekcolors::lcars_2357") 
names(pal) <- levels(cut(1, pal_breaks, right = T))

plot_heat_counts <- function(
    df, x, y, fill, pal, 
    x_tick_labs = NULL,
    xlab = "confidence range", 
    ylab = "species [ranked by abundance]", 
    flab = NULL, 
    title = NULL, 
    subtitle = NULL
){
  
  p <- df |>
    ggplot() +
    geom_tile(aes(y = {{y}}, x = {{x}}, fill = {{fill}}), color = "black") +
    scale_fill_manual(values = pal) +
    labs(x = xlab, y = ylab, fill = flab, title = title, subtitle = subtitle) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(nrow = 1)) 
  
  if(!is.null(x_tick_labs)){
    p <- p + scale_x_discrete(labels = x_tick_labs)
  }
  
  return(p)
}


# 1 - Make plots for different confidence ranges --------------------------


# define ranges of confidence scores
conf_breaks <- tibble(
  start = c(0.15, 0.2, 0.25), 
  end = 1, 
  step = c(0.05, 0.1, 0.15), 
  name = c("0.05", "0.1", "0.15")
)

# PLOT 1: counts per transect ---------------------------------------------

df_int <- conf_breaks |> 
  group_split(name) |> 
  map(~{
    
    # sequence of confidence ranges
    cb <- seq(.x$start, .x$end, .x$step)
    # plot name
    int <- .x$name
    pout <- str_c("4_conf_per_transect_int", int, ".jpeg")
    
    df <- am_df |> 
      # only include the species that have abundance information
      filter(!is.na(abundance)) |> 
      mutate(scientific_name = fct_reorder(scientific_name, abundance)) |> 
      # bin the confidence into ranges defined by cb
      mutate(confidence_range = cut(confidence, c(0.09, cb), right = T)) |> 
      # summarize the counts
      summarize(
        n_detect = n(), 
        .by = c(scientific_name, confidence_range, transect_name)
      )

    x_labs <- levels(df$confidence_range)
    x_labs[seq(1, length(x_labs), 2)] <- ""
    
    if(int == 0.05){
      x_labs[seq(4, length(x_labs), 4)] <- ""
    }
    
    p1 <- df |> 
      mutate(col_rank = cut(n_detect, pal_breaks, right = T)) |> 
      plot_heat_counts(
        x = confidence_range, 
        y = scientific_name, 
        x_tick_labs = x_labs,
        fill = col_rank, 
        pal = pal, 
        flab = "detection count",
        title = paste(
          "Detections per transect, species and confidence score with the step of",
          int
        )
      ) +
      facet_wrap(~transect_name, nrow = 1) +
      theme(panel.spacing = unit(1, "lines"))
    
    
    p1 |> 
      ggsave(
        filename = file.path(gdir, pout), 
        width = 45, 
        height = 30, 
        unit = "cm"
      )
    
    
    df |> 
      mutate(int = int)
        
  })
    

# PLOT 2: overview transects and intervals -------------------------------------

p2 <- df_int |> 
  list_rbind() |> 
  summarize(
    n_detect = sum(n_detect), 
    .by = c(scientific_name, confidence_range, int)
  ) |> 
  mutate(
    col_rank = cut(n_detect, pal_breaks, right = T),
    int = paste("confidence interval:", int)
  ) |> 
  plot_heat_counts(
    x = confidence_range, 
    y = scientific_name, 
    fill = col_rank, 
    pal = pal, 
    flab = "detection count", 
    title = "Distribution of confidence ranges across all transects"
  ) +
  facet_wrap(~int, nrow = 1, scales = "free_x") +
  theme(panel.spacing = unit(1, "lines")) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))
  
p2 |> 
  ggsave(
    filename = file.path(gdir, "4_all_transects_and_intervals.jpeg"), 
    width = 45, 
    height = 30, 
    unit = "cm"
  )

# 2 - EmBC classes ------------------------------------------------------------

df_embc <- am_df |> 
  filter(!is.na(abundance)) |>
  summarize(n_detect = n(), .by = c(scientific_name, abundance)) |> 
  mutate(
    abundance_log10 = log10(abundance),
    n_detect_log10 = log10(n_detect)
  ) 
  # without two outliers, in the plot "4_embc_86species.jpeg"
  #filter(!scientific_name %in% c("Passer domesticus", "Luscinia megarhynchos"))

# running EmBC on original and logaritmic values
bc_out <- embc(as.matrix(df_embc[,c("abundance", "n_detect")]))
bc_out_log <- embc(as.matrix(df_embc[,c("abundance_log10", "n_detect_log10")]))

# embc categories and colors used for plotting
bc_labs <- data.frame(
  value = 1:4,
  name = c("LL", "LH", "HL", "HH"), 
  col = c("#99EEFFFF", "#0055FFFF", "#FFCC66FF", "#FF5500FF") 
  )

# mergina info from logartimic and original values
df_embc_long <- df_embc |> 
  mutate(
    bc = bc_out@A, 
    bc_log = bc_out_log@A
  ) |> 
  pivot_longer(
    cols = c(bc, bc_log), 
    names_to = "bc_type", 
    values_to = "bc"
  ) |> 
  mutate(
    # rename the classes
    bc_lab = bc_labs$name[value = bc], 
    bc_type = if_else(
      str_detect(bc_type, "log"), 
      "B) Classification based on logaritmic scale (log10)", 
      "A) Classification based on the original values"
      ), 
    # fix abundances and detection counts
    abundance_plot = case_when(
      str_detect(bc_type, "logaritmic") ~ abundance_log10,
      str_detect(bc_type, "original") ~ abundance,
    ),
    n_detect_plot = case_when(
      str_detect(bc_type, "logaritmic") ~ n_detect_log10,
      str_detect(bc_type, "original") ~ n_detect
    )
  ) |> 
  select(-abundance, -abundance_log10, -n_detect, -n_detect_log10)


# PLOT 3: Abundance vs. detection -----------------------------------------

pembc <- df_embc_long |> 
  ggplot() + 
  geom_point(
    aes(x = abundance_plot, y = n_detect_plot, color = bc_lab), 
    size = 2
  ) + 
  geom_text_repel(
    aes(x = abundance_plot, y = n_detect_plot, label = scientific_name), 
    max.overlaps = 10, 
    box.padding = 0.5
  ) +
  theme_bw() +
  scale_color_manual(values = bc_labs$col, breaks = bc_labs$name) +
  labs(
    y = "total number of detections", 
    x = "abundance",
    title = "EmBC classes based on number of detections and species abundance", 
    subtitle = "Passer domesticus and Luscinia megarhynchos excluded", 
    color = "BC class"
  ) + 
  theme(legend.position = "bottom") +
  facet_wrap(~bc_type, scale = "free", ncol = 1)

pembc |>
  ggsave(
    filename = file.path(gdir, "4_embc_88species.jpeg"), 
    width = 30, 
    height = 30, 
    unit = "cm"
  )



# 3 - Species high detection no abundance --------------------------------------

df_counts <- am_df |> 
  summarize(n_detect = n(), .by = c(scientific_name, abundance)) |> 
  filter(n_detect >= median(df_embc$n_detect)) |> 
  filter(is.na(abundance)) |>
  arrange(desc(n_detect)) |>
  select(-abundance) 

df_counts |> 
  write_csv(here("Data", "4_detections_no_abundance.csv"))


# 4 - Density plots of conficence ----------------------------------------------

am_df_sub <- am_df |> 
  filter(!is.na(abundance)) |>
  mutate(
    scientific_name = fct_reorder(scientific_name, abundance), 
    abundance_range = cut(
      abundance, c(0, 100, 200, 1000, max(abundance)), 
      right = T, 
      labels = c("(0-100]", "(100-200]", "(200-1000]", "(1000-16000]")
    ) 
  ) 
  

# PLOT5: Confidence scores density plots ----------------------------------


am_df_sub |> 
  group_split(abundance_range) |> 
  map(~{
    
    df <- .x
    abd_range <- unique(df$abundance_range)
    
    p <- df |> 
      ggplot(
        aes(
          x = confidence, 
          y = scientific_name, 
          fill = factor(after_stat(quantile)), 
        )
      ) + 
      geom_density_ridges_gradient(
        scale = 1,
        calc_ecdf = TRUE,
        quantiles = 4, 
        quantile_lines = TRUE, 
        bandwidth = 0.05
      ) +
      scale_fill_viridis_d(name = "quantiles", alpha = 0.6) +
      theme_minimal() +
      theme(legend.position = "right") +
      labs(
        y = "", 
        title = "Density distribution of confidence scores", 
        subtitle = paste("for species with abundance within the range of", abd_range), 
        caption = "bandwidth set to 0.05"
      )
    
    p |>
      ggsave(
        filename = file.path(gdir, str_c("5_dens_", abd_range, ".jpeg")), 
        width = 25, 
        height = 35, 
        unit = "cm"
      )
    
    
  })
 
  

# from the cow script 
# bcsmth_0.9 <- smth(bc, dlta = 0.9)
# save(bcsmth_0.9, file = file.path(out, "EMBC_output_smth_dlta_0.9.RData")) 
# 
# bcsmth_1 <- smth(bc)
# save(bcsmth_1, file = file.path(out, "EMBC_output_smth_dlta_1.RData")) 



# 5 - Compare species -----------------------------------------------------


