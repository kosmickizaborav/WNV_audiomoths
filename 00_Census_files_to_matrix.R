#################### Procesing data from census files ##########################
# We need obtain the data in unmarked format

# Libraries
library(tidyverse)
library(here)
library(readxl)

rm(list = ls())
# Directories ------------------------------------------------------------------
loc.data <- paste0(getwd(), "/DATA/")
loc.census <- paste0(getwd(), "/DATA/BBDD_ALEX_Ocells_Aiguamolls/")
loc.out <- paste0(getwd(), "/OUTPUT/")

# Distance data frame  ---------------------------------------------------------
fls <- list.files(loc.census)[3:7]

full_data <- data.frame()
for (i in fls){
  fl <- read_csv(paste0(loc.census, i), col_names = TRUE)
  
  full_data <- rbind(full_data, fl)
}

# Preparing data for Distance algorithm
full = data.frame(
  species = full_data$species,
  Region.Label = case_when(
    full_data$trans_official_name == "01. Vilasacra_Fortia" ~ "Vila_Sacra",
    full_data$trans_official_name == "02. Gallinera" ~ "Gallinera",
    full_data$trans_official_name == "03. Fluvia" ~ "Fluvia",
    full_data$trans_official_name == "04. Cortalet_Mata" ~ "Cortalet",
    full_data$trans_official_name == "05. Muga" ~ "Muga"
  ),
  Sample.Label = case_when(
    full_data$sector == "s1" ~ "S01",
    full_data$sector == "s2" ~ "S02",
    full_data$sector == "s3" ~ "S03",
    full_data$sector == "s4" ~ "S04",
    full_data$sector == "s5" ~ "S05",
    full_data$sector == "s6" ~ "S06",
  ),
  Area = 240000,
  Effort = 600,
  distbegin = case_when(
    full_data$banda == "0-25 m" ~ 0,
    full_data$banda == "25-100 m" ~ 25,
    full_data$banda == ">100 m" ~ 100
  ),
  distend = case_when(
    full_data$banda == "0-25 m" ~ 25,
    full_data$banda == "25-100 m" ~ 100,
    full_data$banda == ">100 m" ~ 250
  ),
  size = full_data$total,
  data = full_data$calendar_date,
  month = full_data$month,
  ornitoleg = full_data$ornitoleg,
  horari_inici = full_data$horari_inici,
  horari_final = full_data$horari_final
  ) %>%
  mutate(
    id = paste0(Region.Label, "_", Sample.Label),
    object = c(1:nrow(full_data))
  )

# Checking names of species ----------------------------------------------------
nms <- data.frame(nams = unique(full$species))

full_correct <- full %>%
  mutate(
    species = case_when(
      species %in% c("Alctitis hypoleuca", "Actitis hypoleuca", 
                     "Actitis hupoleuca", "Actitis hypoleucos") ~ "Actitis hypoleucos",
      species %in% c("Aegithalus caudatus") ~ "Aegithalos caudatus",
      species %in% c("Alopochen aegyptiacus", "Alopochen aegyptius") ~ "Alopochen aegyptiaca",
      species %in% c("Anas plathynchos", "Anas plathyrynchos",
                     "Anas platryrhynchos", "Anas platyrhynchos",
                     "Anas platyrhynchus", "Anas platyrynchos") ~ "Anas platyrhynchos",
      species %in% c("Apus sp") ~ "Apus sp.",
      species %in% c("Ardea puprpurea", "Ardea purpurea") ~ "Ardea purpurea",
      species %in% c("Carcuelis carduelis", "Carduelis carduelis") ~ "Carduelis carduelis",
      species %in% c("Certhia brachydactyla", "Certhya brachydactila",
                     "Certhya brachydactyla") ~ "Certhia brachydactyla",
      species %in% c("Chlidonias hybrida", "Chlydonias hybrida",
                     "Chlydonias hybryda") ~ "Chlidonias hybridus",
      species %in% c("Croicochepalus ridibundus", "Croicocephalus ridibundus",
                     "Chroicocephalus ridibundus", "Larus ridibundus") ~ "Chroicocephalus ridibundus",
      species %in% c("Cyaneus caeruleus", "Cyanistes caeruleus") ~ "Cyanistes caeruleus",
      species %in% c("Cyanistes cyaneus", "Cyanistes cyanus") ~ "Cyanistes cyanus",
      species %in% c("Cyanecula cyaneus") ~ "Cyanopica cyanus",
      species %in% c("Delichon urbica", "Delichum urbicum") ~ "Delichon urbicum",
      species %in% c("Dencrocopus minor", "Dendrocopos minor", 
                     "Dendrocopus minor") ~ "Dryobates minor",
      species %in% c("Dendrocopus major", "Dendrocopos major") ~ "Dendrocopos major",
      species %in% c("Emberiza calandra", "Cyanistes cyanus") ~ "Emberiza calandra",
      species %in% c("Emberiza schoeniclus", "Emberiza calandria") ~ "Emberiza calandra",
      species %in% c("Estrilda astrid", "Estrilda astrild") ~ "Estrilda astrild",
      species %in% c("Falco naumanii", "Falco naumanni") ~ "Falco naumanni",
      species %in% c("Falco naumanni / tinnunculus", "Falco tinnunculus/naumanni") ~ "Falco tinnunculus/naumanni",
      species %in% c("Ficedula hpoleuca", "Ficedula hypoleuca") ~ "Ficedula hypoleuca",
      species %in% c("Fringilla coelebs", "Fringilla colebes") ~ "Fringilla coelebs",
      species %in% c("Galerida cristata", "Galerida cristata3") ~ "Galerida cristata",
      species %in% c("Gallinula chlorophus", "Gallinula chloropus",
                     "Gallinula clorophus", "Gallinula ochropus") ~ "Gallinula ochropus",
      species %in% c("Hippoalais polyglotta", "Hippolais polyglotta") ~ "Hippolais polyglotta",
      species %in% c("Larus michaellis", "Larus michahellis") ~ "Larus michahellis",
      species %in% c("Luscinia megarhynchos", "Luscinia megarynchos") ~ "Luscinia megarhynchos",
      species %in% c("Merops apiaster", "Merops apìaster") ~ "Merops apiaster",
      species %in% c("Muscicapa astriata", "Muscicapa atriata",
                     "Muscicapa striata") ~ "Muscicapa striata",
      species %in% c("Nycticorax nycticorax", "Nyctycorax nyctycorax") ~ "Nycticorax nycticorax",
      species %in% c("Passer sp", "Passer sp.") ~ "Passer sp.",
      species %in% c("Parus ater") ~ "Periparus ater",
      species %in% c("Parus caeruleus") ~ "Cyanistes caeruleus",
      species %in% c("Phylloscopus colluybita", "Phylloscopus collybita") ~ "Phylloscopus collybita",
      species %in% c("Phylloscopus trochilus", "Phylloscopus troquilus") ~ "Phylloscopus trochilus",
      species %in% c("Psittacula krameri", "Psittakula krameri") ~ "Psittacula krameri",
      species %in% c("Recurviristra avosetta", "Recurvirostra avosetta") ~ "Recurvirostra avosetta",
      species %in% c("Streptopelia turtur", "Streptopelia turtus") ~ "Streptopelia turtur",
      species %in% c("Psittacula krameri", "Psittakula krameri") ~ "Psittacula krameri",
      species %in% c("Sturnus vuilgaris", "Sturnus vulgaris") ~ "Sturnus vulgaris",
      species %in% c("Troglodytes troglodtes", "Troglodytes troglodytes") ~ "Troglodytes troglodytes",
      species %in% c("Psittacula krameri", "Psittakula krameri") ~ "Psittacula krameri",
      .default = species
    )
  )

nms2 <- data.frame(nams = unique(full_correct$species))

saveRDS(full_correct, file = paste0(loc.out, "full_wider_db.rds"))
