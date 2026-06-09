library(tidyverse)
library(sf)

# Load data
churches <- read_csv("cin_churches_coords.csv")
places <- read_csv("diocese_places.csv")
segments <- read_csv("diocese_segments.csv")

# Convert to SF
churches_sf <- st_as_sf(churches, coords = c("longitude", "latitude"), crs = 4326)
places_sf <- st_as_sf(places, coords = c("lon", "lat"), crs = 4326)
segments_sf <- st_as_sf(segments, wkt = "wkt", crs = 4326)

# Project to Planar (UTM 16N for Ohio)
churches_proj <- st_transform(churches_sf, 32616)
places_proj <- st_transform(places_sf, 32616)
segments_proj <- st_transform(segments_sf, 32616)

# Analysis Parameters
buffer_dist <- 500 # meters (approx procession length)

print("Calculating interaction scores (POI density near churches)...")
# Create buffers
church_buffers <- st_buffer(churches_proj, dist = buffer_dist)

# Interaction Score: Count POIs in buffer
poi_counts <- st_intersects(church_buffers, places_proj)
churches_proj$interaction_count <- sapply(poi_counts, length)

print("Calculating safety scores (total safe walking infrastructure length)...")
# Safety Score: Sum of segment lengths in buffer
# We'll use st_intersection which can be slow for 211k segments. 
# Optimized: Crop segments to the bounding box of all buffers first.
segments_cropped <- st_filter(segments_proj, st_union(church_buffers))

# Intersection to get segments inside buffers
# Note: This assigns segment fragments to churches
intersections <- st_intersection(segments_cropped, church_buffers)

# Calculate length of the intersection fragments
intersections$fragment_len <- as.numeric(st_length(intersections))

# Aggregate length by church
infrastructure_length <- intersections %>%
  st_drop_geometry() %>%
  group_by(psuedo_id) %>%
  summarize(total_infrastructure_m = sum(fragment_len))

# Join back to churches
final_analysis <- churches_proj %>%
  left_join(infrastructure_length, by = "psuedo_id") %>%
  mutate(total_infrastructure_m = replace_na(total_infrastructure_m, 0))

# Normalize and Index
# We want a balance of Interaction (Exposure) and Infrastructure (Safety)
final_analysis <- final_analysis %>%
  mutate(
    norm_interaction = (interaction_count - min(interaction_count)) / (max(interaction_count) - min(interaction_count)),
    norm_infra = (total_infrastructure_m - min(total_infrastructure_m)) / (max(total_infrastructure_m) - min(total_infrastructure_m)),
    procession_index = (norm_interaction * 0.6 + norm_infra * 0.4) * 100
  ) %>%
  arrange(desc(procession_index))

# Save results
write_csv(st_drop_geometry(final_analysis), "diocese_procession_ranking.csv")

# Visualization: Top 20 Churches
top_20 <- final_analysis %>% head(20)

p <- ggplot(top_20, aes(x = reorder(name, procession_index), y = procession_index, fill = procession_index)) +
  geom_col() +
  coord_flip() +
  scale_fill_viridis_c() +
  labs(title = "Top 20 Churches for Corpus Christi Processions",
       subtitle = "Diocese of Cincinnati: Ranked by Interaction Potential and Safe Infrastructure",
       x = "Church Name",
       y = "Procession Impact Index (0-100)") +
  theme_minimal()

ggsave("diocese_procession_top_20.png", p, width = 10, height = 8)

print("Top 10 Churches:")
print(head(final_analysis %>% select(name, interaction_count, total_infrastructure_m, procession_index), 10))

cat("\nAnalysis complete. Results saved to diocese_procession_ranking.csv and diocese_procession_top_20.png\n")
