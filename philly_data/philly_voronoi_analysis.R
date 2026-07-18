library(tidyverse)
library(sf)
library(spdep)

# ── Load data ──────────────────────────────────────────────────────────────────
philly_path <- "/Users/benedictleonardi/Library/CloudStorage/GoogleDrive-benedict.r.leonardi@gmail.com/My Drive/Personal/Random Code/Github_Projects/US_Catholic_Churches/philly_data/philly_analysis.csv"
philly <- read_csv(philly_path, show_col_types = FALSE)

# ── One row per parish (mid-panel year 2016) ──────────────────────────────────
parishes <- philly |>
  filter(year == 2016) |>
  select(fs_number, parish_name, latitude, longitude,
         founding_year, founding_lag, cohort, county, avg_weekend_attendance) |>
  filter(!is.na(latitude), !is.na(longitude))

cat(sprintf("Parishes with coordinates: %d\n", nrow(parishes)))

# ── Build sf point layer ───────────────────────────────────────────────────────
pts <- st_as_sf(parishes, coords = c("longitude", "latitude"), crs = 4326)

# ── Philadelphia MSA bounding box ─────────────────────────────────────────────
# Roughly: lon -76.0 to -74.8, lat 39.7 to 40.3
bbox_poly <- st_as_sfc(st_bbox(c(
  xmin = -76.0, xmax = -74.8,
  ymin =  39.7, ymax =  40.3
), crs = 4326))

# Clip points to bbox
pts_clip <- pts[st_within(pts, bbox_poly, sparse = FALSE)[, 1], ]
cat(sprintf("Parishes within MSA bbox: %d\n", nrow(pts_clip)))

# ── Voronoi tessellation ───────────────────────────────────────────────────────
# Project to a planar CRS for tessellation (PA South State Plane, EPSG:32129)
pts_proj <- st_transform(pts_clip, 32129)
bbox_proj <- st_transform(bbox_poly, 32129)

# st_voronoi requires a MULTIPOINT geometry
vor_raw <- st_voronoi(st_union(pts_proj), envelope = bbox_proj)

# Explode into individual polygons
vor_polys <- st_collection_extract(vor_raw, "POLYGON")

# ── Join parish attributes back to polygons ───────────────────────────────────
# Match each polygon to its generating point via spatial join
vor_sf  <- st_sf(geometry = vor_polys) |> st_transform(4326)
pts_wgs <- pts_clip |> st_transform(4326)

# Join parish attributes before clipping: nearest feature is robust to boundary issues
joined <- st_join(vor_sf, pts_wgs, join = st_nearest_feature)

cat(sprintf("Voronoi polygons after join: %d\n", nrow(joined)))

# Clip to bbox
philly_vor <- st_intersection(joined, bbox_poly)

# Reorder cohort factor
# Use actual cohort strings from the data (with en-dash U+2013)
cohort_levels <- sort(unique(parishes$cohort[!is.na(parishes$cohort)]))
cat("Cohort levels:\n"); print(cohort_levels)

philly_vor <- philly_vor |>
  mutate(cohort = factor(cohort, levels = cohort_levels))

cat(sprintf("Final Voronoi polygons: %d\n", nrow(philly_vor)))
print(table(philly_vor$cohort, useNA = "ifany"))

# ── Moran's I on founding_lag ──────────────────────────────────────────────────
vor_lag <- philly_vor |> filter(!is.na(founding_lag))

nb  <- poly2nb(vor_lag, queen = TRUE, snap = 0.01)
lw  <- nb2listw(nb, style = "W", zero.policy = TRUE)
mi  <- moran.test(vor_lag$founding_lag, lw, zero.policy = TRUE)

cat(sprintf(
  "\nGlobal Moran's I (founding lag): I = %.3f   E[I] = %.4f   p = %.4f\n",
  mi$estimate[1], mi$estimate[2], mi$p.value
))

# ── Local Moran's I (LISA) ────────────────────────────────────────────────────
lisa <- localmoran(vor_lag$founding_lag, lw, zero.policy = TRUE)

vor_lag <- vor_lag |>
  mutate(
    local_I   = lisa[, "Ii"],
    local_p   = lisa[, "Pr(z != E(Ii))"],
    lag_z     = scale(founding_lag)[, 1],
    lag_lag_z = lag.listw(lw, lag_z, zero.policy = TRUE),
    lisa_type = case_when(
      local_p >= 0.05                       ~ "Not significant",
      lag_z > 0 & lag_lag_z > 0            ~ "HH — late-founding cluster",
      lag_z < 0 & lag_lag_z < 0            ~ "LL — early-founding cluster",
      lag_z > 0 & lag_lag_z < 0            ~ "HL — outlier (late among early)",
      lag_z < 0 & lag_lag_z > 0            ~ "LH — outlier (early among late)",
      TRUE                                  ~ "Not significant"
    )
  )

cat("\nLISA types:\n")
print(table(vor_lag$lisa_type))

# ── Save outputs ──────────────────────────────────────────────────────────────
out_dir <- "/Users/benedictleonardi/Library/CloudStorage/GoogleDrive-benedict.r.leonardi@gmail.com/My Drive/Personal/Random Code/Github_Projects/US_Catholic_Churches/philly_data"

saveRDS(philly_vor, file.path(out_dir, "philly_voronoi.rds"))
saveRDS(vor_lag,    file.path(out_dir, "philly_voronoi_lisa.rds"))

# Also save Moran's I result for inline use
morans_result <- list(I = mi$estimate[1], EI = mi$estimate[2], p = mi$p.value)
saveRDS(morans_result, file.path(out_dir, "philly_morans.rds"))

cat("\nSaved: philly_voronoi.rds, philly_voronoi_lisa.rds, philly_morans.rds\n")
