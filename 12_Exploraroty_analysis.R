#################### Exploratory analysis: acoustic vs census ##################
# Libraries
library(terra)
library(sf)
library(tidyverse)
library(patchwork)
library(lme4)

rm(list = ls())
# Directories ------------------------------------------------------------------
loc.data <- paste0(getwd(), "/DATA/")
loc.census <- paste0(getwd(), "/DATA/BBDD_ALEX_Ocells_Aiguamolls/")
loc.out <- paste0(getwd(), "/OUTPUT/")
loc.fig <- paste0(getwd(), "/FIGURES/Exploratory/")

# Loading data -----------------------------------------------------------------
# Audios:
audios_data <- readRDS(file = paste0(loc.out, "audios_raw_data.txt"))

audios <- audios_data %>%
  filter(confidence > 0.8) %>%
  mutate(id = substr(id, 1, 14),
         habitat = substr(id, 1, 2), 
         transect = substr(transect, 1, 2),
         transect_code = paste0(transect, "_", sector))

# # IMPORTANT!!! In order to obtain a list of species names, run in bash the following: 
# python3 species.py --lat 42.225039 --lon 3.092257 --week -1 --o /home/catuxa/Documents/E4WARNING/From_suitability_to_abundances/species.txt
species_names <- read_tsv("species_cortalet.txt", col_names = FALSE) %>%
  separate(X1, c("scientific_name", "common_name"), "_")
audios <- merge(audios, species_names, by = "common_name", all.x = TRUE)
rm(audios_data, config_data, species_names)

audios <- audios %>%
  mutate(
    month = month(collocation_date),
  ) %>% 
  mutate(
    season = case_when(
      month %in% c(4, 5) ~ "spring",
      month %in% c(6, 7, 8) ~ "summer",
      month %in% c(9, 10, 11) ~ "fall"
    )
  )

# Census:
birds <- readRDS(paste0(loc.out, "full_wider_db.rds")) 
birds <- birds %>%
  mutate(
    sector = gsub("^S0", "S", Sample.Label),
    transect_code = case_when(
      Region.Label == "Vila_Sacra" ~ "CS",
      Region.Label == "Gallinera" ~ "AT",
      Region.Label == "Fluvia" ~ "RF",
      Region.Label == "Cortalet" ~ "WE",
      Region.Label == "Muga" ~ "PE"
    )
  ) %>%
  mutate(month = case_when(
    month == "abril" ~ 4,
    month == "maig" ~ 5,
    month == "juny" ~ 6,
    month == "juliol" ~ 7,
    month == "agost" ~ 8,
    month == "setembre" ~ 9,
    month == "octubre" ~ 10,
    month == "novembre" ~ 11)
  ) %>%
  mutate(
    transect_code = paste0(transect_code, "_", sector),
    transect = substr(transect_code, 1, 2),
    season = case_when(
      month %in% c("abril", "maig") ~ "spring",
      month %in% c("juny", "juliol", "agost") ~ "summer",
      month %in% c("setembre", "octubre", "novembre") ~ "fall"
    )) %>% 
  filter(size != 0)

# Selecting sector where the audiomoths are located
sector_audios <-  c("CS_S4", "CS_S6", "AT_S1", "AT_S4", "RF_S1", "RF_S2", "WE_S1", 
                    "WE_S4", "PE_S1", "PE_S5")

# Number of species ------------------------------------------------------------
print(paste0("Acoustic: ", length(unique(audios$species_code)), "; census: ", length(unique(birds$species))))

audios %>% dplyr::select(scientific_name, transect) %>% unique() %>%
  group_by(transect) %>% summarise(n = n())
# I need the scientific names to compare
audio_names <- audios %>% 
  dplyr::select(scientific_name, transect) %>% unique()
audio_names <- audio_names %>% dplyr::select(scientific_name) %>% unique()

# By sector
birds %>% filter(transect_code %in% sector_audios) %>% 
  dplyr::select(species, Region.Label) %>% unique() %>%
  group_by(Region.Label) %>% summarise(n = n())
birds %>% dplyr::select(species, Region.Label) %>% unique() %>%
  group_by(Region.Label) %>% summarise(n = n())
birds_names <- birds %>% dplyr::select(species) %>% unique()

check_names <- audio_names %>%
  filter(!(scientific_name %in% birds_names$species))

rm(check_names, birds_names, audio_names)

# Here, we plot the number of species detected by each method.
audio_richness <- audios %>%
  group_by(transect_code) %>%
  summarise(unique_species = n_distinct(scientific_name), .groups = "drop") %>%
  mutate(method = "Acoustic") 
birds_richness <- birds %>%
  filter(transect_code %in% sector_audios) %>% 
  # mutate(transect = substr(transect_code, 1, 2)) %>%
  group_by(transect_code) %>%
  summarise(unique_species = n_distinct(species), .groups = "drop") %>%
  mutate(method = "Census")
common_species_by_sector <- birds %>%
  filter(transect_code %in% sector_audios) %>%
  select(species, transect_code) %>%
  inner_join(audios %>% select(scientific_name, transect_code), 
             by = c("species" = "scientific_name", "transect_code")) %>% 
  unique()  %>%
  group_by(transect_code) %>%
  summarise(unique_species = n_distinct(species), .groups = "drop") %>%
  mutate(
    method = "Both"
  )

richness_sector <- bind_rows(audio_richness, birds_richness, 
                             common_species_by_sector) 
  
a <- ggplot(data = richness_sector, aes(x = transect_code, y = unique_species, fill = method)) +
  geom_bar(stat = "identity", position = "dodge", show.legend = FALSE) +
  scale_fill_manual(values = c("Census" = "#abdda4", "Acoustic" = "#3288bd", "Both" = "#8968CD")) +
  labs(
    x = "Sector",
    y = "Number of species",
    fill = "Method"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45,  hjust = 1))
  
audio_richness <- audios %>%
  group_by(transect) %>%
  summarise(unique_species = n_distinct(common_name), .groups = "drop") %>%
  mutate(method = "Acoustic")
birds_richness <- birds %>%
  mutate(transect = substr(transect_code, 1, 2)) %>%
  group_by(transect) %>%
  summarise(unique_species = n_distinct(species), .groups = "drop") %>%
  mutate(method = "Census")
common_species_by_transect <- birds %>%
  select(species, transect) %>%
  inner_join(audios %>% select(scientific_name, transect), 
             by = c("species" = "scientific_name", "transect")) %>% 
  unique()  %>%
  group_by(transect) %>%
  summarise(unique_species = n_distinct(species), .groups = "drop") %>%
  mutate(
    method = "Both"
  )

richness_transect <- bind_rows(audio_richness, birds_richness, common_species_by_transect)

b <- ggplot(data = richness_transect, aes(x = transect, y = unique_species, fill = method)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Census" = "#abdda4", "Acoustic" = "#3288bd", "Both" = "#8968CD")) +
  labs(
    x = "Transect",
    y = "Number of species",
    fill = "Method"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    legend.position = "right"
  )

a + b + plot_annotation(tag_levels = 'A', tag_suffix = ')')

ggsave(filename = paste0(loc.fig, "raw_nSpecies_method.png"),
       width = 25, height = 10,  units = "cm")

rm(a, b, richness_transect, audio_richness, birds_richness, richness_sector, 
   common_species_by_sector, common_species_by_transect) 

# Relationship between abundances of each specie -------------------------------
# We used the number of detections as proxy of abundances
# at season scale, summing the number of individuals and detection by season
audio_abundances <- audios %>% 
  group_by(scientific_name, month, transect_code, transect) %>% 
  summarise(n = n(), .groups = "drop") %>%
  rename("species" = "scientific_name") %>%
  mutate(method = "Acoustic")

birds_abundances <- birds %>%
  filter(transect_code %in% sector_audios) %>%
  group_by(species, month, transect_code, transect)  %>%
  summarise(n = sum(size), .groups = "drop") %>%
  # dplyr::select(month, transect_code, n, scientific_name) %>%
  mutate(method = "Census")

# Dispersion plot
abundance_comparison <- full_join(birds_abundances, audio_abundances, 
                                  by = c("species", "transect_code", "transect", "month"),
                                  suffix = c("_census", "_acoustic")) %>%
  mutate(n_census = replace_na(n_census, 0), 
         n_acoustic = replace_na(n_acoustic, 0))
ggplot(abundance_comparison, aes(x = n_census, y = n_acoustic)) +
  geom_point(alpha = 0.6, color = "#3288bd") +
  geom_smooth(method = "lm", se = FALSE, color = "#8968CD") +  
  facet_wrap(~transect*month, scales = "free") +  
  labs(x = "Relative abundancia (Census)",
       y = "Relative abundancia (Acoustic)") +
  theme_minimal()

ggplot(abundance_comparison, aes(x = method_census, y = n_census, fill = "Census")) +
  geom_boxplot(alpha = 0.5) +
  geom_boxplot(aes(x = method_acoustic, y = n_acoustic, fill = "Acoustic"), alpha = 0.5) +
  scale_fill_manual(values = c("Census" =  "#abdda4", "Acoustic" = "#3288bd")) +
  labs(x = "Method",
       y = "Relative abundance") +
  theme_minimal()

# By species level
abundance_species_month <- full_join(birds_abundances, audio_abundances, 
                                    by = c("species", "month", "transect_code"),
                                    suffix = c("_census", "_acoustic")) %>%
  mutate(n_census = replace_na(n_census, 0), 
         n_acoustic = replace_na(n_acoustic, 0)) # Reemplazar NA por 0
head(abundance_species_month)

cor_species <- abundance_species_month %>%
  group_by(species) %>%
  summarise(cor = cor(n_census, n_acoustic, method = "spearman", use = "complete.obs")) %>%
  arrange(desc(cor))
summary(cor_species$cor)
top <- cor_species$species[1:10]

top_plot <- list()
for (spp in 1:length(top)){
  top_plot[[spp]] <- ggplot(abundance_comparison %>% filter(species == top[spp]), 
                            aes(x = n_census, y = n_acoustic)) +
    geom_point(alpha = 0.6, color = "#3288bd") +
    geom_smooth(method = "lm", se = TRUE, color = "#8968CD") +  
    labs(
      title = top[spp],
      x = "Relative abundancia (Census)",
      y = "Relative abundancia (Acoustic)") +
    theme_minimal()
}
ggpubr::ggarrange(top_plot[[1]], top_plot[[2]], top_plot[[3]], top_plot[[4]],
                  top_plot[[5]], top_plot[[6]], top_plot[[7]], top_plot[[8]],
                  top_plot[[9]], top_plot[[10]], nrow = 2, ncol = 5)
ggsave(filename = paste0(loc.fig, "raw_disperse_topspp_0.8filter.png"),
       width = 35, height = 25,  units = "cm", bg = "white")

# Modeling the relationship ----------------------------------------------------
abundance_species_month <- abundance_species_month %>%
  mutate(
    
  )
lmm_model <- lmer(n_census ~ n_acoustic + (1 | month) + (1 | species) + (1 | transect_code), 
                  data = abundance_species_month)
summary(lmm_model)

# Selecting competence species for WNV -----------------------------------------
# variables of interest: peak titer and transmission_efficiency_corrected_mortality
experiments <-readxl::read_excel(paste0(loc.data, "WNV host experiments.xlsx")) %>%
  janitor::clean_names() %>%
  dplyr::select(species_4, peak_titer_6, transmission_efficiency_corrected_mortality) %>%
  rename("species" = "species_4", "peak_titer" = "peak_titer_6", "transmision_eff" = "transmission_efficiency_corrected_mortality") %>%
  separate(species, c("genera", "spp"))
summary(experiments)

birds_competence <- birds %>% 
  separate(species, c("genera", "spp"), sep = " ") %>%
  filter(genera %in% experiments$genera)

audios_competence <- audios %>% 
  filter(common_name != "nocall") %>%
  filter(confidence > 0.8) %>%
  separate(scientific_name, c("genera", "spp"), sep = " ") %>%
  filter(genera %in% experiments$genera)

print(paste0("Acoustic: ", length(unique(paste(audios_competence$genera, audios_competence$spp))),
             "; census: ", length(unique(paste(birds_competence$genera, birds_competence$spp)))))
sum(unique(paste(audios_competence$genera, audios_competence$spp)) %in%
  unique(paste(birds_competence$genera, birds_competence$spp)))

species_in_common <- merge(audios_competence %>% dplyr::select(genera, spp) %>% distinct(),
                           birds_competence %>% dplyr::select(genera, spp)  %>% distinct(),
                           by = c("genera", "spp"))

# Week VAI by habitats-species-month ----------------------------------------------
config_data <- readRDS(file = paste0(loc.out, "audios_config_data.txt"))
audios <- merge(audios, config_data, by = "id", all.x = TRUE)

vai_week <- audios %>%
  filter(confidence > 0.8) %>%
  filter(common_name != "nocall") %>%  
  group_by(transect, transect_code, scientific_name, month) %>%
  reframe(
    detections = n(),
    detection_time = n() * 3,
    total_recording_time_sec = sum(total_recording_time_sec),
    vai = detection_time / total_recording_time_sec * 100
  ) %>%
  unique()  %>%
  rename("species" = "scientific_name") %>%
  dplyr::select(-detections, -detections, -total_recording_time_sec) %>%
  mutate(method = "Acoustic")

birds_abundances <- birds %>%
  filter(transect_code %in% sector_audios) %>%
  group_by(species, month, transect_code, transect)  %>%
  summarise(n = sum(size), .groups = "drop") %>%
  mutate(method = "Census")
  # dplyr::select(month, transect_code, n, scientific_name)

# Dispersion plot

abundance_comparison <- full_join(birds_abundances, vai_week, 
                                  by = c("species", "transect_code", "transect", "month"),
                                  suffix = c("_census", "_acoustic")
                                  ) %>%
  mutate(
    n_census = replace_na(n, 0), 
    n_acoustic = replace_na(vai, 0)
    )

cor_species <- abundance_comparison %>%
  group_by(species) %>%
  summarise(cor = cor(n_census, n_acoustic, method = "spearman", use = "complete.obs")) %>%
  arrange(desc(cor))
summary(cor_species$cor)
top <- cor_species$species[1:10]

ggplot(abundance_comparison, aes(x = n_census, y = n_acoustic)) +
  geom_point(alpha = 0.6, color = "#3288bd") +
  geom_smooth(method = "lm", se = FALSE, color = "#8968CD") +  
  facet_wrap(~transect*month, scales = "free") +  
  labs(x = "Relative abundancia (Census)",
       y = "Relative abundancia (Acoustic)") +
  theme_minimal()

ggplot(abundance_comparison, aes(x = method_census, y = n_census, fill = "Census")) +
  geom_boxplot(alpha = 0.5) +
  geom_boxplot(aes(x = method_acoustic, y = n_acoustic, fill = "Acoustic"), alpha = 0.5) +
  scale_fill_manual(values = c("Census" =  "#abdda4", "Acoustic" = "#3288bd")) +
  labs(x = "Method",
       y = "Relative abundance") +
  theme_minimal()

# Some estimates (no realible) for comparing -----------------------------------
estimates <- data.frame()
estimates <- rbind(estimates, # Calculating by hand, it is not automaticed
c("Anas platyrhynchos"      , 3715  , 2076   , 1710  , 3.069),
c("Sturnus vulgaris"        , 4025  , 2889   , 816   , 1.746),
c("Turdus merula"           , 138   , 927    , 12225 , 3.440),
c("Larus michahellis"       , 608   , 939    , 516   , 0.824),
c("Serinus serinus"         , 435   , 1168   , 16488 , 3.385),
c("Pica pica"               , 199   , 998    , 3978  , 2.429),
c("Corvus corax"            , 46    , 286    , 351   , 1.323),
c("Corvus corone"           , 23    , 710    , 156   , 1.306),
c("Corvus monedula"         , 20    , 207    , 111   , 0.879),
c("Alectoris rufa"          , 0     , 0      , 189   , 0.729),
c("Falco tinnunculus!!"     , 77    , 944    , 594   , 1.938),
c("Falco naumanni!!"        , 11    , 112    , 405   , 2.074),
c("Buteo buteo!!"          , 30    , 1075   , 939   , 2.122),
c("Nycticorax nycticorax"   , 9     , 181    , 426   , 1.972),
c("Streptopelia decaocto"   , 634   , 1075   , 2310  , 1.732),
c("Fulica atra"             , 140   , 291    , 186   , 1.665),
c("Coturnix coturnix"       , 14    , 715    , 987   , 2.395))
colnames(estimates) <- c("Species", "census", "DS", "detections", "WVAI")

estimates <- estimates %>%
  mutate(
    census = as.numeric(census),
    DS = as.numeric(DS),
    detections = as.numeric(detections),
    WVAI = as.numeric(WVAI)
  )


a <- ggplot(estimates, aes(x = WVAI, y = DS, color = Species)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "#8968CD") +   
  labs(x = "Weekly Vocal Acoustic Index(WVAI)",
       y = "Relative abundance\n(Distance Sampling Model)",
       caption = paste0("Y = ", 
                       round(lm(estimates$DS ~ estimates$WVAI)$coefficients[1], 2),
                       " + ", round(lm(estimates$DS ~ estimates$WVAI)$coefficients[2], 2), 
                       "X; pearson = ", 
                       round(cor(estimates$DS, estimates$WVAI, method = "pearson"), 2))) +
  theme_minimal() +
  theme(legend.position="none")

b <- ggplot(estimates, aes(x = WVAI, y = census, color = Species)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "#8968CD") +   
  labs(x = "Weekly Vocal Acoustic Index\n(WVAI)",
       y = "Raw abundance (Census)",
       caption = paste0("Y = ", 
                        round(lm(estimates$census ~ estimates$WVAI)$coefficients[1], 2),
                        " + ", round(lm(estimates$census ~ estimates$WVAI)$coefficients[2], 2), 
                        "X; pearson = ", 
                        round(cor(estimates$census, estimates$WVAI, method = "pearson"), 2))) +
  theme_minimal() +
  theme(legend.position="none")

c <- ggplot(estimates, aes(x = detections, y = census, color = Species)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "#8968CD") +   
  labs(x = "Number of detections (Acoustic)",
       y = "Raw abundance (Census)",
       caption = paste0("Y = ", 
                        round(lm(estimates$census ~ estimates$detections)$coefficients[1], 2),
                        " + ", round(lm(estimates$census ~ estimates$detections)$coefficients[2], 2), 
                        "X; pearson = ", 
                        round(cor(estimates$census, estimates$detections, method = "pearson"), 2))) +
  theme_minimal() +
  theme(legend.position="none")

d <- ggplot(estimates, aes(x = detections, y = DS, color = Species)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "#8968CD") +   
  labs(x = "Number of detections (Acoustic)",
       y = "Relative abundance\n(Distance Sampling Model)",
       caption = paste0("Y = ", 
                        round(lm(estimates$DS ~ estimates$detections)$coefficients[1], 2),
                        " + ", round(lm(estimates$DS ~ estimates$detections)$coefficients[2], 2), 
                        "X; pearson = ", 
                        round(cor(estimates$DS, estimates$detections, method = "pearson"), 2))) +
  theme_minimal() +
  theme(legend.position="none")

a + d + c + d +
  plot_annotation(tag_levels = "A", tag_suffix = ")") +
  plot_layout(guides = "collect") & theme(legend.position = 'right')

ggsave(filename = paste0(loc.fig, "global_cor_estimates.png"),
       width = 25, height = 15,  units = "cm", bg = "white")
