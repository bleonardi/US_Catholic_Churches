library(tidyverse)
library(sf)
library(leaflet)

# Load data
places <- read_csv("procession_places.csv")
segments <- read_csv("procession_segments.csv")

# Convert to SF
places_sf <- st_as_sf(places, coords = c("lon", "lat"), crs = 4326)
segments_sf <- st_as_sf(segments, wkt = "wkt", crs = 4326)

# Define safety score based on road class
# Higher is safer for a procession
segments_sf <- segments_sf %>%
  mutate(safety_score = case_when(
    class %in% c("pedestrian", "footway", "path") ~ 5,
    class %in% c("living_street", "residential") ~ 4,
    class %in% c("unclassified", "service") ~ 3,
    class %in% c("tertiary", "secondary") ~ 2,
    class %in% c("primary", "trunk", "motorway") ~ 0,
    TRUE ~ 1
  ))

# Interaction Score: Density of nearby POIs
# We'll buffer segments and count POIs within 50 meters
segments_proj <- st_transform(segments_sf, 3857) # Project for buffering
places_proj <- st_transform(places_sf, 3857)

# Count POIs near each segment
poi_counts <- st_intersects(st_buffer(segments_proj, 50), places_proj)
segments_sf$interaction_score <- sapply(poi_counts, length)

# Combined Score
segments_sf <- segments_sf %>%
  mutate(total_score = safety_score * 0.4 + interaction_score * 0.6)

# Suggest a route: Just the top segments for now, or a "Heatmap"
# Filter out unsafe segments
safe_segments <- segments_sf %>% filter(safety_score > 0)

# Visualization
library(ggplot2)

p <- ggplot() +
  geom_sf(data = safe_segments, aes(color = total_score, size = total_score)) +
  scale_color_viridis_c(option = "plasma") +
  geom_sf(data = places_sf, color = "red", size = 1, alpha = 0.5) +
  labs(title = "Corpus Christi Procession: Interaction & Safety Analysis",
       subtitle = "Yellow/Purple lines indicate high interaction potential and pedestrian safety.",
       color = "Route Score",
       size = "Route Score") +
  theme_minimal()

ggsave("procession_analysis.png", p, width = 12, height = 10)

# Also save a summary
summary <- safe_segments %>%
  st_drop_geometry() %>%
  group_by(class) %>%
  summarize(avg_score = mean(total_score), count = n()) %>%
  arrange(desc(avg_score))

write_csv(summary, "procession_summary.csv")

cat("Analysis complete. Visualization saved to procession_analysis.png\n")
