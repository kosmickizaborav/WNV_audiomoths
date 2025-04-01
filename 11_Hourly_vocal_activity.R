############################# Vocal Activity Index #############################

library(tidyverse)

rm(list = ls())
# Directories ------------------------------------------------------------------
loc.output <- paste0(getwd(), "/OUTPUT/")

# Loading file -----------------------------------------------------------------
audios_data <- readRDS(file = paste0(loc.output, "audios_raw_data.txt"))
config_data <- readRDS(file = paste0(loc.output, "audios_config_data.txt"))

# To calculate the hourly vocal activity, we need to correct the difference between 
# total time in an hour and the recording time in this hour.
# So the function would be:
# Hourly vocal Activity = Detection per hour * (3600 / recording time) [in seconds]
# WARNING! Detection per 3 sec: detections * 3 [in seconds]
audios_data <- audios_data %>%
  mutate(id = substr(id, 1, 14),
         habitat = substr(id, 1, 2)
         )
audios <- merge(audios_data, config_data, by = "id", all.x = TRUE)

# General vocal activity index (VAI) -------------------------------------------
# Frequency of detection considering the recording time
detections <- audios_data %>%
  filter(common_name != "nocall")

total_number <- nrow(audios_data)
total_detections <- nrow(detections)
total_recording_time <- sum(config_data$total_recording_time_sec)

vai <- (total_detections*3)/total_recording_time

print(paste("General Vocal Activity Index (VAI):", round(vai, 4)))

## Daily VAI -------------------------------------------------------------------
vai_day <- audios %>%
  filter(common_name != "nocall") %>%  
  mutate(date = as.Date(date)) %>% 
  group_by(date) %>%
  reframe(
    detections = n(),
    detection_time = n() * 3,
    vai = detection_time / total_recording_time_sec
  ) %>%
  unique()
vai_day

ggplot(vai_day, aes(x = date, y = vai)) +
  geom_point() +
  geom_smooth() +
  labs(
    y = "Daily vocal activity",
    x = "Date"
  ) +
  theme_classic()

## Daily VAI by habitats -------------------------------------------------------
vai_day_habitat <- audios %>%
  filter(common_name != "nocall") %>%  
  mutate(date = as.Date(date)) %>% 
  group_by(date, habitat) %>%
  reframe(
    detections = n(),
    detection_time = n() * 3,
    vai = detection_time / total_recording_time_sec
  ) %>%
  unique()

ggplot(vai_day_habitat, aes(x = date, y = vai)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~habitat) +
  labs(
    y = "Daily vocal activity",
    x = "Date"
  ) +
  theme_classic() 

## VAI by habitats-species -----------------------------------------------------
vai_spp_habitat <- audios %>%
  filter(common_name != "nocall") %>%  
  mutate(date = as.Date(date)) %>% 
  group_by(common_name, habitat) %>%
  reframe(
    detections = n(),
    detection_time = n() * 3,
    vai = detection_time / total_recording_time_sec
  ) %>%
  unique()

ggplot(vai_spp_habitat, aes(x = common_name, y = vai)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~habitat) +
  labs(
    y = "Daily vocal activity",
    x = "Date"
  ) +
  theme_classic() 
  
## Daily VAI by habitats-species -----------------------------------------------
vai_day_hab_sp <- audios %>%
filter(common_name != "nocall") %>%  
  mutate(date = as.Date(date)) %>% 
  group_by(date, habitat, common_name) %>%
  reframe(
    detections = n(),
    detection_time = n() * 3,
    vai = detection_time / total_recording_time_sec
  ) %>%
  unique()

ggplot(vai_day_hab_sp, aes(x = date, y = vai)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~habitat) +
  labs(
    y = "Daily vocal activity",
    x = "Date"
  ) +
  theme_classic() 


