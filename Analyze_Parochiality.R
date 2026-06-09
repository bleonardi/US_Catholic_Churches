library(tidyverse)
library(sf)

# Load data
df <- read_csv("national_church_data.csv") %>%
  filter(diocese_name %in% c("Cincinnati", "Covington")) %>%
  filter(!is.na(latitude), !is.na(longitude))

# Convert to SF for spatial ops
df_sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)

# To perform Voronoi correctly, we should project to a planar CRS (e.g., UTM)
# Zone 16N is good for OH/KY
df_sf_proj <- st_transform(df_sf, crs = 32616)

# Create Voronoi polygons
# 1. Combine points
combined_points <- st_union(df_sf_proj)
# 2. Compute Voronoi
voronoi_polys <- st_voronoi(combined_points)
# 3. Extract individual polygons
voronoi_sf <- st_collection_extract(voronoi_polys)

# Match back to original data
# st_join will join by intersection (the points are inside their Voronoi cells)
voronoi_data <- st_sf(geometry = voronoi_sf) %>%
  st_join(df_sf_proj)

# Visualization
p <- ggplot() +
  geom_sf(data = voronoi_data, aes(fill = diocese_name), alpha = 0.5, color = "white", size = 0.1) +
  geom_sf(data = df_sf_proj, size = 0.5) +
  scale_fill_manual(values = c("Cincinnati" = "blue", "Covington" = "red")) +
  labs(title = "Voronoi Parochiality: Cincinnati vs. Covington",
       subtitle = "Geometric boundaries often ignore Diocesan (and state) lines.",
       fill = "Diocese") +
  theme_minimal()

ggsave("voronoi_contrast.png", p, width = 10, height = 8)

# QUANTITATIVE ANALYSIS
# We want to see if any Voronoi cell for a Cincinnati church crosses into Kentucky
# and vice versa. 
# For this, we'll use the 'church_address_providence_name' as the 'Legal State'

leakage <- voronoi_data %>%
  mutate(legal_state = ifelse(diocese_name == "Cincinnati", "Ohio", "Kentucky")) %>%
  # In a real analysis, we'd intersect with the actual state boundary.
  # Here, let's see which Voronoi cells have neighbors from the other diocese.
  
  # Area of each cell (in sq km)
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6)

# Summary of 'Market Area' by Diocese
summary_stats <- leakage %>%
  st_drop_geometry() %>%
  group_by(diocese_name) %>%
  summarize(num_churches = n(),
            total_voronoi_area = sum(area_km2),
            avg_service_area = mean(area_km2))

print(summary_stats)

cat("\nAnalysis complete. Visualization saved to voronoi_contrast.png\n")
