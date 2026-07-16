suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
  library(spdep)
  library(tidycensus)
  library(tigris)
  library(fixest)
  library(fuzzyjoin)
  library(stringdist)
})
options(tigris_use_cache = TRUE)

# ── 1. Load AOD outcome data ──────────────────────────────────────────────────

aod <- read_csv("aod_parish_data.csv", show_col_types = FALSE) |>
  left_join(
    read_csv("aod_geography.csv", show_col_types = FALSE) |>
      select(path, pct_within_parish, pct_outside_region),
    by = "path"
  )

# ── 2. Load Detroit-area parishes from masstimes (has lat/lng) ───────────────

zip_county <- read_csv("zip_county_crosswalk.csv", show_col_types = FALSE) |>
  transmute(
    zip         = str_pad(as.character(ZCTA5), 5, pad = "0"),
    county_fips = str_pad(as.character(GEOID),  5, pad = "0")
  ) |>
  group_by(zip) |> slice(1) |> ungroup()

DETROIT_COUNTIES <- c("Wayne|Oakland|Macomb|Monroe|Washtenaw|Livingston")

mt_det <- read_csv("national_church_data.csv", show_col_types = FALSE) |>
  filter(
    church_address_country_territory_name == "United States",
    str_detect(church_address_county, DETROIT_COUNTIES),
    !is.na(latitude), !is.na(longitude),
    latitude > 40, latitude < 45,       # sanity clip to Michigan
    church_type_name %in% c("Parish", "Cathedral", "Basilica", "Mission", "Shrine")
  ) |>
  select(psuedo_id, name, latitude, longitude, church_address_postal_code) |>
  mutate(zip = str_pad(str_sub(church_address_postal_code, 1, 5), 5, pad = "0")) |>
  left_join(zip_county, by = "zip")

cat(sprintf("Detroit-area masstimes parishes (for Voronoi): %d\n", nrow(mt_det)))

# ── 3. Fuzzy-join AOD parishes → masstimes to get lat/lng ────────────────────

clean_nm <- function(x) {
  x |> str_to_lower() |>
    str_remove("\\s+parish$|\\s+church$") |>
    str_remove_all("[^a-z ]") |>
    str_squish()
}

aod2 <- aod |> mutate(name_key = clean_nm(short_name))
mt2  <- mt_det |> mutate(name_key = clean_nm(name))

aod_geo <- stringdist_left_join(
  aod2, mt2, by = "name_key", method = "jw", max_dist = 0.25
) |>
  group_by(path) |>
  slice_min(stringdist(name_key.x, name_key.y, method = "jw"), with_ties = FALSE) |>
  ungroup() |>
  filter(!is.na(latitude), !is.na(longitude))

cat(sprintf("AOD parishes matched with coordinates: %d / %d\n",
            nrow(aod_geo), nrow(aod)))

# ── 4. County suburban crossing years ────────────────────────────────────────

urb <- bind_rows(
  read_csv("county_urban_shares_1950.csv", show_col_types = FALSE) |> mutate(snap_year = 1950),
  read_csv("county_urban_shares_1970.csv", show_col_types = FALSE) |> mutate(snap_year = 1970),
  read_csv("county_urban_shares_2000.csv", show_col_types = FALSE) |> mutate(snap_year = 2000),
  read_csv("county_urban_shares_2010.csv", show_col_types = FALSE) |> mutate(snap_year = 2010)
)
crossing_yr <- urb |>
  arrange(county_fips, snap_year) |>
  group_by(county_fips) |>
  group_modify(function(d, k) {
    d <- arrange(d, snap_year)
    crossed <- d$suburban_share >= 0.40
    if (!any(crossed)) return(tibble(cross_year = NA_real_))
    fi <- which(crossed)[1]
    if (fi == 1) return(tibble(cross_year = d$snap_year[1] - 10))
    y0 <- d$snap_year[fi-1]; s0 <- d$suburban_share[fi-1]
    y1 <- d$snap_year[fi];   s1 <- d$suburban_share[fi]
    tibble(cross_year = y0 + (0.40 - s0) / (s1 - s0) * (y1 - y0))
  }) |>
  ungroup()

aod_geo <- aod_geo |>
  left_join(crossing_yr, by = "county_fips") |>
  mutate(
    founding_lag = founding_year - cross_year,
    cohort = case_when(
      founding_lag < -10   ~ "Pre-suburban",
      founding_lag <  20   ~ "Early post-suburban",
      founding_lag <  40   ~ "Late post-suburban",
      !is.na(founding_lag) ~ "Exurban (1990s+)"
    ) |> factor(levels = c("Pre-suburban", "Early post-suburban",
                            "Late post-suburban", "Exurban (1990s+)"))
  )

# ── 5. Build sf point layer ───────────────────────────────────────────────────

parishes_sf <- aod_geo |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(26917)   # NAD83 / UTM Zone 17N — metres, good for Michigan

# For Voronoi use ALL masstimes parishes (denser tessellation)
all_sf <- mt_det |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(26917)

# ── 6. Detroit MSA boundary (6-county clip) ───────────────────────────────────

det_counties <- counties(state = "MI", cb = TRUE, progress_bar = FALSE) |>
  filter(NAME %in% c("Wayne","Oakland","Macomb","Monroe","Washtenaw","Livingston")) |>
  st_transform(26917) |>
  st_union()

# ── 7. Voronoi tessellation ───────────────────────────────────────────────────
# Use all masstimes parishes so tessellation is complete, then filter to AOD set

vor_raw <- all_sf |>
  st_union() |>
  st_voronoi(envelope = det_counties) |>
  st_collection_extract("POLYGON") |>
  st_as_sf() |>
  st_set_crs(26917) |>
  st_intersection(det_counties)    # clip to MSA boundary

# Spatial join: assign each Voronoi cell to the nearest parish point
vor_with_id <- vor_raw |>
  st_join(all_sf |> select(psuedo_id, name), join = st_nearest_feature)

cat(sprintf("Voronoi polygons generated: %d\n", nrow(vor_with_id)))

# Keep only polygons whose parish is in the AOD set
aod_vor <- vor_with_id |>
  inner_join(
    aod_geo |> as_tibble() |> select(-any_of(c("geometry"))),
    by = c("psuedo_id", "name")
  )

cat(sprintf("AOD parishes with Voronoi polygons: %d\n", nrow(aod_vor)))

# ── 8. ACS block-group data (areal interpolation into Voronoi) ───────────────

bg_vars <- c(
  pop         = "B01003_001",
  median_age  = "B01002_001",
  white_nh    = "B03002_003",
  med_income  = "B19013_001",
  foreign_born = "B05001_006",
  med_home_val = "B25077_001"
)

bg <- get_acs(
  geography = "block group",
  variables = bg_vars,
  state     = "MI",
  county    = c("Wayne","Oakland","Macomb","Monroe","Washtenaw","Livingston"),
  year      = 2022,
  geometry  = TRUE,
  output    = "wide"
) |>
  st_transform(26917) |>
  select(GEOID,
         pop          = popE,
         median_age   = median_ageE,
         white_nh     = white_nhE,
         med_income   = med_incomeE,
         foreign_born = foreign_bornE,
         med_home_val = med_home_valE) |>
  mutate(
    pct_white_nh  = white_nh  / pmax(pop, 1),
    pct_foreign   = foreign_born / pmax(pop, 1)
  )

cat(sprintf("Block groups loaded: %d\n", nrow(bg)))

# Extensive variables (totals): area-weighted sum — drop non-numeric GEOID
bg_ext <- bg |> select(pop, white_nh, foreign_born)
vor_ext <- st_interpolate_aw(bg_ext, aod_vor, extensive = TRUE) |>
  st_drop_geometry()

# Intensive variables (rates/medians): area-weighted mean
bg_int <- bg |> select(median_age, pct_white_nh, pct_foreign, med_income, med_home_val)
vor_int <- st_interpolate_aw(bg_int, aod_vor, extensive = FALSE) |>
  st_drop_geometry()

aod_vor <- aod_vor |>
  bind_cols(vor_ext) |>
  bind_cols(vor_int)

aod_vor <- aod_vor |>
  mutate(
    voronoi_area_sqkm = as.numeric(st_area(aod_vor)) / 1e6,
    pop_density  = pop / voronoi_area_sqkm,
    pct_white_nh = white_nh / pmax(pop, 1)
  )

cat(sprintf(
  "ACS interpolation complete. Sample: median age range %.1f–%.1f\n",
  min(aod_vor$median_age, na.rm=TRUE), max(aod_vor$median_age, na.rm=TRUE)
))

# ── 9. Voronoi adjacency spatial weights ──────────────────────────────────────

nb_vor  <- poly2nb(aod_vor, queen = TRUE)
lw_vor  <- nb2listw(nb_vor, style = "W", zero.policy = TRUE)

# Global Moran's I on founding lag
vor_lag <- aod_vor |> filter(!is.na(founding_lag))
lw_lag  <- nb2listw(
  poly2nb(vor_lag, queen = TRUE),
  style = "W", zero.policy = TRUE
)

mi_lag  <- moran.test(vor_lag$founding_lag, lw_lag, zero.policy = TRUE)
cat(sprintf(
  "\nMoran's I (founding lag): I=%.3f  p=%.4f\n",
  mi_lag$estimate[1], mi_lag$p.value
))

# Also test sacramental outcomes
vor_conf <- aod_vor |> filter(!is.na(confirmations_pct_chg),
                               abs(confirmations_pct_chg) <= 1)
lw_conf  <- nb2listw(poly2nb(vor_conf, queen = TRUE), style="W", zero.policy=TRUE)
mi_conf  <- moran.test(vor_conf$confirmations_pct_chg, lw_conf, zero.policy=TRUE)
cat(sprintf("Moran's I (confirmations %%chg): I=%.3f  p=%.4f\n",
            mi_conf$estimate[1], mi_conf$p.value))

# ── 10. LISA — Local Moran's I on founding lag ───────────────────────────────

lisa_lag <- localmoran(vor_lag$founding_lag, lw_lag, zero.policy = TRUE)
lag_z    <- scale(vor_lag$founding_lag)[,1]
sp_lag   <- lag.listw(lw_lag, lag_z, zero.policy = TRUE)

vor_lag <- vor_lag |>
  mutate(
    lisa_ii   = lisa_lag[, "Ii"],
    lisa_p    = lisa_lag[, "Pr(z != E(Ii))"],
    lisa_type = case_when(
      lisa_p < 0.05 & lag_z > 0 & sp_lag > 0 ~ "HH — late-founding cluster",
      lisa_p < 0.05 & lag_z < 0 & sp_lag < 0 ~ "LL — early-founding cluster",
      lisa_p < 0.05 & lag_z > 0 & sp_lag < 0 ~ "HL — outlier (late among early)",
      lisa_p < 0.05 & lag_z < 0 & sp_lag > 0 ~ "LH — outlier (early among late)",
      TRUE ~ "Not significant"
    )
  )

cat("\nLISA cluster counts (founding lag):\n")
print(table(vor_lag$lisa_type))

# ── 11. Getis-Ord G* (hotspot analysis on founding lag) ──────────────────────

nb_star <- include.self(poly2nb(vor_lag, queen = TRUE))
lw_star <- nb2listw(nb_star, style = "B", zero.policy = TRUE)
gstar   <- localG(vor_lag$founding_lag, lw_star, zero.policy = TRUE)

vor_lag <- vor_lag |>
  mutate(
    gstar_z    = as.numeric(gstar),
    hotspot    = case_when(
      gstar_z >  2.576 ~ "Hot spot (99%)",
      gstar_z >  1.960 ~ "Hot spot (95%)",
      gstar_z >  1.645 ~ "Hot spot (90%)",
      gstar_z < -2.576 ~ "Cold spot (99%)",
      gstar_z < -1.960 ~ "Cold spot (95%)",
      gstar_z < -1.645 ~ "Cold spot (90%)",
      TRUE ~ "Not significant"
    )
  )

cat("\nG* hotspot classification (founding lag):\n")
print(table(vor_lag$hotspot))

# ── 12. Spatial lag regression ───────────────────────────────────────────────
# Use the full Voronoi set: founding_lag + W(founding_lag) as predictors.
# Outcomes: confirmations % chg and mass attendance % capacity.
# Keep ACS as optional controls — but don't require them (many NAs post-join).

reg_sf <- aod_vor |>
  filter(!is.na(founding_lag))

nb_reg  <- poly2nb(reg_sf, queen = TRUE, snap = 1)
lw_reg  <- nb2listw(nb_reg, style = "W", zero.policy = TRUE)
reg_df  <- reg_sf |> st_drop_geometry()

# Spatial lag of the key predictor
reg_df$w_founding_lag <- lag.listw(lw_reg, reg_df$founding_lag, zero.policy = TRUE)

# --- Confirmations % change ---
conf_df <- reg_df |> filter(!is.na(confirmations_pct_chg), abs(confirmations_pct_chg) <= 1)
nb_c    <- poly2nb(reg_sf |> filter(!is.na(confirmations_pct_chg),
                                    abs(confirmations_pct_chg) <= 1), queen=TRUE, snap=1)
lw_c    <- nb2listw(nb_c, style="W", zero.policy=TRUE)
conf_df$w_outcome <- lag.listw(lw_c, conf_df$confirmations_pct_chg, zero.policy=TRUE)

m1 <- feols(confirmations_pct_chg ~ founding_lag, data=conf_df, vcov="hetero")
m2 <- feols(confirmations_pct_chg ~ founding_lag + w_founding_lag, data=conf_df, vcov="hetero")
m3 <- feols(confirmations_pct_chg ~ founding_lag + w_outcome, data=conf_df, vcov="hetero")

# --- Mass attendance ---
mass_df <- reg_df |> filter(!is.na(mass_pct_capacity))
nb_m    <- poly2nb(reg_sf |> filter(!is.na(mass_pct_capacity)), queen=TRUE, snap=1)
lw_m    <- nb2listw(nb_m, style="W", zero.policy=TRUE)
mass_df$w_outcome <- lag.listw(lw_m, mass_df$mass_pct_capacity, zero.policy=TRUE)

m4 <- feols(mass_pct_capacity ~ founding_lag, data=mass_df, vcov="hetero")
m5 <- feols(mass_pct_capacity ~ founding_lag + w_founding_lag, data=mass_df, vcov="hetero")
m6 <- feols(mass_pct_capacity ~ founding_lag + w_outcome, data=mass_df, vcov="hetero")

cat("\n=== SPATIAL REGRESSION ===\n")
etable(m1, m2, m3, m4, m5, m6,
       headers = c("Confirms OLS","Confirms + W(X)","Confirms + W(Y)",
                   "Mass OLS","Mass + W(X)","Mass + W(Y)"),
       digits = 4, fitstat = ~ r2 + n)

# --- Territorial cohesion ---
gap_df <- reg_df |> filter(!is.na(pct_within_parish))
m7 <- feols(pct_within_parish ~ founding_lag + median_age + pct_white_nh,
            data = gap_df, vcov = "hetero")
cat("\n=== Territorial cohesion ~ founding lag + Voronoi demographics ===\n")
etable(m7, digits = 3)

# ── 13. Save outputs ──────────────────────────────────────────────────────────

st_write(aod_vor,  "aod_voronoi.gpkg",    delete_dsn = TRUE, quiet = TRUE)
st_write(vor_lag,  "aod_voronoi_lisa.gpkg", delete_dsn = TRUE, quiet = TRUE)

write_csv(aod_vor |> st_drop_geometry(), "aod_voronoi_data.csv")

cat("\nOutputs saved:\n")
cat("  aod_voronoi.gpkg        — Voronoi polygons with ACS + AOD attributes\n")
cat("  aod_voronoi_lisa.gpkg   — LISA / G* cluster assignments\n")
cat("  aod_voronoi_data.csv    — flat CSV for QMD code chunks\n")
