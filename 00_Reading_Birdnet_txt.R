########################## Convert txt files from BirdNET ######################
# Here, we create a function to read the outputs files of BridNet algorithm and
# join all in the sample dataframe.

library(tidyverse)
library(readr) # The txt file have space in the common names, so read.table() is useless
library(janitor)

rm(list = ls())

# Directories: WARNING!!! ------------------------------------------------------
# We run this directly in cluster
# The files are on leov folder in cluster
PATH <- "/leov1/audiomoths/Results"
loc.output <- paste0(getwd(), "/Audiomoths/OUTPUT/")
  
# Function ---------------------------------------------------------------------
list_results <- list.files(PATH)
audios_data <- data.frame()

for (trap_day in list_results){
  # Path for audios folders (labelled by ID and day)
  cat(paste("---", trap_day, "\n"))
  list_day <- list.files(paste0(PATH, "/", trap_day))

  # Path for days folder
  for (d in list_day){
    cat(paste("------", d, "\n"))
    list_hour <- list.files(paste0(PATH, "/", trap_day, "/", d))

    # Path for record files within a day and audiomoths
    for (h in list_hour){
      cat(paste("---------", h, "\n"))
      audio_sample <-read_delim(paste0(PATH, "/", trap_day, "/", d, "/", h), delim = "\t") %>%
        clean_names() %>%
        mutate(
          collocation_date = as.Date(
            paste0(substr(trap_day, 9, 12), "20",substr(trap_day, 13, 14)),
            format = "%d%m%Y"),
          date = as.POSIXct(substr(h, 1, 15), format = "%Y%m%d_%H%M%S"),
          id = paste0(trap_day, substr(h, 9, 15)),
          transect = substr(trap_day, 1, 4),
          sector = substr(trap_day, 6, 7),
        ) %>%
        mutate(
          begin_date = date + begin_time_s,
          end_date = date + end_time_s
        )
      audios_data <- rbind(audios_data, audio_sample)
    }
  }
}

audios_data <- audios_data %>% dplyr::select(
  id, transect, sector, collocation_date, date, begin_date, begin_time_s, end_date, end_time_s, low_freq_hz, high_freq_hz, confidence,
  common_name, species_code, file_offset_s, selection, view, channel
)

saveRDS(audios_data, file = paste0(loc.output, "audios_raw_data.txt"))

# Extract the config information of audiomoths ---------------------------------

# Funtion to structure txt file
read_config <- function(archive, trap_day) {
  # Read file aviding empty lines
  lines <- readLines(archive) %>% 
    str_subset(".+")  
  
  # Extract key values
  dt <- lines %>%
    str_split_fixed(":", 2) %>% 
    as_tibble() %>%
    set_names(c("key", "value")) %>%
    mutate(
      key = str_trim(key), 
      value = str_trim(value),
      id = trap_day
    ) 
  
  dt <- dt %>%
    pivot_wider(names_from = key, values_from = value) %>%
    relocate(id, .before = everything()) 
  
  return(dt)
}

PATH <- "/leov1/audiomoths/My_audios"
name_files <- "CONFIG.TXT"

list_audios <- list.files(PATH)
config_data <- data.frame()

for (trap_day in list_audios){
  # Path for audios folders (labelled by ID and day)
  cat(paste("---", trap_day, "\n"))
  fls <- list.files(paste0(PATH, "/", trap_day))
  
  if (name_files %in% fls){
    config_file <- paste0(PATH, "/", trap_day, "/", name_files)
    
    config_file <- read_config(config_file, trap_day)
    
    config_data <- rbind(config_data, config_file) 
  } else {
    config_file <- as.data.frame(t(data.frame(rep(NA, 27)))) 
    colnames(config_file) <- names(config_data)
    config_file$id <- trap_day
    rownames(config_file) <- NULL
    
    config_data <- rbind(config_data, config_file) 
  }
}

config_data <- config_data %>%
  clean_names() %>%
  dplyr::select(id, firmware, sample_rate_hz, gain, sleep_duration_s, recording_duration_s,
                recording_period_1, recording_period_2) %>%
  separate(recording_period_1, c("start_time", "end_time"), " - ",  remove = FALSE) %>%
  separate(end_time, c("end_time", "utc"), " ") %>%
  mutate(
    start_time =  as.POSIXct(start_time, format="%H:%M"),
    end_time =  as.POSIXct(end_time, format="%H:%M"),
    active_hours_1 = as.numeric(difftime(end_time, start_time, units = "hours"))
  ) %>%
  dplyr::select(-start_time, - end_time, - utc) %>%
  separate(recording_period_2, c("start_time", "end_time"), " - ",  remove = FALSE) %>%
  separate(end_time, c("end_time", "utc"), " ") %>%
  mutate(
    start_time =  as.POSIXct(start_time, format="%H:%M"),
    end_time =  as.POSIXct(end_time, format="%H:%M"),
    active_hours_2 = as.numeric(difftime(end_time, start_time, units = "hours"))
  ) %>%
  dplyr::select(-start_time, - end_time, - utc) %>%
  mutate(
    recording_time_sec_1 = active_hours_1 * 6 * 60, # In one hour only 6 minutes recording
    recording_time_sec_2 = active_hours_2 * 6 * 60,
    total_recording_time_sec = recording_time_sec_1 + recording_time_sec_2
  )

# We have two rows with NA, copy the config for audiomoths displayed the same day
config_data[12, 2:13] <- config_data[4, 2:13]
config_data[3, 2:13] <- config_data[10, 2:13]

saveRDS(config_data, file = paste0(loc.output, "audios_config_data.txt"))
