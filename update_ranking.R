library(tidyverse)
library(sf)

# Load the main dataset
load("final_recommendation_data.RData")

# Load Overture results
parking_data <- read_csv("overture_parking_results.csv")

# Exclusions
exclusion_types <- c("Chapel", "College/University", "Military Base", "Airport")
exclusion_keywords <- "(?i)Chapel|School|Academy|University|College|Campus"

# Join and update the scores + Apply exclusions
final_data <- final_data %>%
  select(-any_of(c("sam_parking_area_m2", "overture_parking_area_m2", "has_sam", "has_overture_parking"))) %>%
  left_join(parking_data, by = "psuedo_id") %>%
  mutate(
    has_overture_parking = !is.na(overture_parking_area_m2),
    # Calculate adjusted score
    redev_score_adj = ifelse(has_overture_parking, 
                             redev_score + (log1p(overture_parking_area_m2) / 10 * 0.2), 
                             redev_score)
  ) %>%
  # Filter out schools and chapels
  filter(!(church_type_name %in% exclusion_types)) %>%
  filter(!str_detect(name, exclusion_keywords))

# Save the updated dataset back to the RData file
save(final_data, file = "final_recommendation_data.RData")

cat("Updated final_recommendation_data.RData: Excluded schools/chapels and added Overture results.\n")
