library(tidyverse)
library(sf)
library(tidycensus)
library(tigris)
options(tigris_use_cache = TRUE)

# Density thresholds (population per sq mile)
URBAN_THRESHOLD  <- 5000
SUBURB_THRESHOLD <- 1000

classify_density <- function(density) {
  case_when(
    density >= URBAN_THRESHOLD  ~ "urban",
    density >= SUBURB_THRESHOLD ~ "suburban",
    TRUE                        ~ "rural"
  )
}

county_urban_shares <- function(tracts_df) {
  tracts_df %>%
    group_by(county_fips) %>%
    summarize(
      tot_pop     = sum(pop, na.rm = TRUE),
      urban_pop   = sum(pop[ur_class == "urban"],    na.rm = TRUE),
      suburban_pop = sum(pop[ur_class == "suburban"], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      urban_share   = urban_pop   / tot_pop,
      suburban_share = suburban_pop / tot_pop
    )
}

# ── 1950 tracts (NHGIS) ───────────────────────────────────────────────────────
# Shapefile is already Albers Equal Area (meters) — SHAPE_AREA is m²
# NHGISST/NHGISCTY have a trailing zero: "550" = state "55", "0790" = county "079"

cat("Loading 1950 tract shapefile...\n")
shp_1950 <- st_read(
  "nhgis_shape/nhgis0001_shape/tract_1950/US_tract_1950.shp",
  quiet = TRUE
) %>%
  st_drop_geometry() %>%
  transmute(
    GISJOIN,
    county_fips = paste0(
      str_pad(as.integer(NHGISST)  %/% 10, 2, pad = "0"),
      str_pad(as.integer(NHGISCTY) %/% 10, 3, pad = "0")
    ),
    area_sqmi = SHAPE_AREA / 2589988   # m² → sq mi
  )

pop_1950 <- read_csv(
  "nhgis_csv/nhgis0001_csv/nhgis0001_ds82_1950_tract.csv",
  show_col_types = FALSE
) %>%
  transmute(GISJOIN, pop = BZ8001)

tracts_1950 <- shp_1950 %>%
  left_join(pop_1950, by = "GISJOIN") %>%
  filter(!is.na(pop), area_sqmi > 0) %>%
  mutate(
    density  = pop / area_sqmi,
    ur_class = classify_density(density)
  )

cat(sprintf("1950: %d tracts with data\n", nrow(tracts_1950)))
county_urban_1950 <- county_urban_shares(tracts_1950)
cat(sprintf("1950: %d counties with tract coverage\n", nrow(county_urban_1950)))

# ── 1970 tracts (NHGIS) ───────────────────────────────────────────────────────

cat("Loading 1970 tract shapefile...\n")
shp_1970 <- st_read(
  "nhgis0002_shape/nhgis0002_shape/tract_1970/US_tract_1970.shp",
  quiet = TRUE
) %>%
  st_drop_geometry() %>%
  transmute(
    GISJOIN,
    county_fips = paste0(
      str_pad(as.integer(NHGISST)  %/% 10, 2, pad = "0"),
      str_pad(as.integer(NHGISCTY) %/% 10, 3, pad = "0")
    ),
    area_sqmi = SHAPE_AREA / 2589988
  )

pop_1970 <- read_csv(
  "nhgis0002_csv/nhgis0002_csv/nhgis0002_ds98_1970_tract.csv",
  show_col_types = FALSE
) %>%
  transmute(GISJOIN, pop = C1I001)

tracts_1970 <- shp_1970 %>%
  left_join(pop_1970, by = "GISJOIN") %>%
  filter(!is.na(pop), area_sqmi > 0) %>%
  mutate(
    density  = pop / area_sqmi,
    ur_class = classify_density(density)
  )

cat(sprintf("1970: %d tracts with data\n", nrow(tracts_1970)))
county_urban_1970 <- county_urban_shares(tracts_1970)
cat(sprintf("1970: %d counties with tract coverage\n", nrow(county_urban_1970)))

# ── 2000 tracts via tidycensus (proxy for 1980 ARDA data) ────────────────────
# Note: tidycensus decennial goes back to 2000 only; 2000 tracts are a reasonable
# proxy for 1980 urban footprint since suburban expansion largely occurred 1950-1980

fetch_tract_density <- function(year, pop_var = "P001001") {
  # 2000 decennial requires state-by-state pulls; 2010 supports national
  state_fips <- tigris::fips_codes %>%
    distinct(state_code) %>%
    filter(as.integer(state_code) <= 56,   # exclude territories
           !state_code %in% c("03","07","14","43","52")) %>%
    pull(state_code)

  cat(sprintf("\nFetching %d tract population by state (%d states)...\n",
              year, length(state_fips)))

  pop <- map_dfr(state_fips, function(st) {
    tryCatch(
      get_decennial(geography = "tract", variables = pop_var,
                    year = year, state = st, geometry = FALSE, output = "wide"),
      error = function(e) { cat(sprintf("  skip state %s: %s\n", st, e$message)); NULL }
    )
  }) %>%
    transmute(GEOID, pop = !!sym(pop_var), county_fips = str_sub(GEOID, 1, 5))

  cat(sprintf("  %d tracts fetched\n", nrow(pop)))

  cat(sprintf("Fetching %d tract geometries via tigris...\n", year))
  geo <- map_dfr(state_fips, function(st) {
    tryCatch(
      tigris::tracts(state = st, year = year, cb = TRUE) %>% st_transform(5070),
      error = function(e) NULL
    )
  }) %>%
    mutate(
      GEOID     = paste0(STATEFP, COUNTYFP, TRACT),
      area_sqmi = as.numeric(st_area(geometry)) / 2589988
    ) %>%
    st_drop_geometry() %>%
    select(GEOID, area_sqmi)

  pop %>%
    left_join(geo, by = "GEOID") %>%
    filter(!is.na(area_sqmi), area_sqmi > 0) %>%
    mutate(
      density  = pop / area_sqmi,
      ur_class = classify_density(density)
    )
}

tracts_2000_raw <- fetch_tract_density(2000)
cat(sprintf("2000: %d tracts\n", nrow(tracts_2000_raw)))
county_urban_2000 <- county_urban_shares(tracts_2000_raw)

# ── 2010 tracts ───────────────────────────────────────────────────────────────

tracts_2010_raw <- fetch_tract_density(2010)
cat(sprintf("2010: %d tracts\n", nrow(tracts_2010_raw)))
county_urban_2010 <- county_urban_shares(tracts_2010_raw)

# ── Save ──────────────────────────────────────────────────────────────────────

write_csv(county_urban_1950, "county_urban_shares_1950.csv")
write_csv(county_urban_1970, "county_urban_shares_1970.csv")
write_csv(county_urban_2000, "county_urban_shares_2000.csv")
write_csv(county_urban_2010, "county_urban_shares_2010.csv")
cat("\nSaved county_urban_shares_1950/1970/2000/2010.csv\n")

# ── Sanity check: Columbus (Franklin OH) vs Boston (Suffolk MA) ───────────────

cat("\n=== Sanity check ===\n")
targets <- c("39049" = "Franklin OH (Columbus)",
             "25025" = "Suffolk MA (Boston)",
             "36029" = "Erie NY (Buffalo)",
             "17031" = "Cook IL (Chicago)")

for (yr in c("1950", "1970", "2000", "2010")) {
  d <- get(paste0("county_urban_", yr))
  cat(sprintf("\n%s tracts:\n", yr))
  print(
    d %>%
      filter(county_fips %in% names(targets)) %>%
      mutate(
        label        = targets[county_fips],
        urban_share  = scales::percent(urban_share,   accuracy = 0.1),
        suburb_share = scales::percent(suburban_share, accuracy = 0.1)
      ) %>%
      select(label, tot_pop, urban_share, suburb_share) %>%
      arrange(label)
  )
}
