# Load packages
library(tidyverse)
library(tidycensus)
library(sf)        # for spatial joins
library(tmap)      # optional for visualization

# Make sure you have your Census API key set
# census_api_key("YOUR_KEY_HERE", install = TRUE)



#-------------------------
# 5. Exploratory analysis
#-------------------------

## A. Tracts with many people but few churches
tract_with_churches %>%
  mutate(church_per_10k = n_churches / pop_total * 10000) %>%
  arrange(church_per_10k) %>%
  slice_head(n = 10)

## B. Map church density vs population density
tm_shape(tract_with_churches) +
  tm_polygons("density", palette = "Blues", title = "Population Density") +
  tm_bubbles(size = "n_churches", col = "red", alpha = 0.5)

#-------------------------
# 6. Potential deep dives
#-------------------------
# - Overlay tract_data with `tcb::CTTURBO` for housing age
# - Create neighborhood buffers around churches using st_buffer()
# - Identify mismatches (e.g. high Catholic population proxies vs low church count)
