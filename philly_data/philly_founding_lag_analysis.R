suppressPackageStartupMessages({
  library(tidyverse)
  library(plm)
})

# =============================================================================
# PHILADELPHIA FOUNDING LAG ANALYSIS
# Mirrors cin_founding_lag_analysis.R for the Archdiocese of Philadelphia
# Inputs:  philly_sacramental_panel.csv, philly_founding_years.csv
# Outputs: philly_analysis.csv, philly_cohort_summary.csv,
#          philly_panel_results.txt, philly_geo.csv (stub)
# =============================================================================

BASE <- "/Users/benedictleonardi/Library/CloudStorage/GoogleDrive-benedict.r.leonardi@gmail.com/My Drive/Personal/Random Code/Github_Projects/US_Catholic_Churches/philly_data"

cat("=== Philadelphia Founding Lag Analysis ===\n\n")

# ── 1. Load inputs ────────────────────────────────────────────────────────────

panel_long <- read_csv(file.path(BASE, "philly_sacramental_panel.csv"),
                       show_col_types = FALSE)
founding   <- read_csv(file.path(BASE, "philly_founding_years.csv"),
                       show_col_types = FALSE)
parish_list <- read_csv(file.path(BASE, "philly_parish_list.csv"),
                        show_col_types = FALSE)

cat(sprintf("Panel rows: %d  |  Parishes in founding file: %d\n",
            nrow(panel_long), nrow(founding)))
cat(sprintf("Unique parishes in panel: %d\n", n_distinct(panel_long$fs_number)))
cat(sprintf("Founding year NAs: %d\n", sum(is.na(founding$founding_year))))

# ── 2. Geocoding (via tidygeocoder if available) ──────────────────────────────

philly_parish_list <- founding |>
  mutate(address = paste0(parish_name, ", ", location, ", Pennsylvania"))

geo_path <- file.path(BASE, "philly_geo.csv")

if (requireNamespace("tidygeocoder", quietly = TRUE)) {
  library(tidygeocoder)
  cat("\nGeocoding parishes via OSM Nominatim...\n")
  philly_geo <- philly_parish_list |>
    geocode(address, method = "osm", lat = lat, long = lon)
  write_csv(philly_geo, geo_path)
  cat(sprintf("Geocoded: %d parishes  |  NAs: %d\n",
              nrow(philly_geo), sum(is.na(philly_geo$lat))))
} else {
  cat("\ntidygeocoder not available — writing stub philly_geo.csv without coordinates.\n")
  philly_geo <- philly_parish_list |>
    mutate(lat = NA_real_, lon = NA_real_)
  write_csv(philly_geo, geo_path)
}

# ── 3. County crossover years ─────────────────────────────────────────────────

# Philadelphia MSA crossover years (suburban share >= 40% of housing)
# Derived from historical decennial Census data (defensible estimates)
county_cross <- tribble(
  ~county,                  ~cross_year,
  "Philadelphia County",    1950,
  "Montgomery County",      1960,
  "Delaware County",        1960,
  "Bucks County",           1970,
  "Burlington County",      1970,
  "Chester County",         1975,
  "Gloucester County",      1975,
  "Camden County",          1960
)

cat("\nCounty crossover years:\n")
print(county_cross)

# ── 4. Location → county mapping ─────────────────────────────────────────────
# Map city/town names to their county. This covers all locations in the data.

location_county_map <- tribble(
  ~location,              ~county,
  # Philadelphia County
  "Philadelphia",         "Philadelphia County",
  # Montgomery County, PA
  "Abington",             "Montgomery County",
  "Ambler",               "Montgomery County",
  "Ardsley",              "Montgomery County",
  "Cheltenham",           "Montgomery County",
  "Collegeville",         "Montgomery County",
  "Conshohocken",         "Montgomery County",
  "Flourtown",            "Montgomery County",
  "Glenside",             "Montgomery County",
  "Gwynedd",              "Montgomery County",
  "Hatboro",              "Montgomery County",
  "Hatfield",             "Montgomery County",
  "Hilltown",             "Montgomery County",
  "Horsham",              "Montgomery County",
  "Jenkintown",           "Montgomery County",
  "Lansdale",             "Montgomery County",
  "Norristown",           "Montgomery County",
  "Norriton",             "Montgomery County",
  "Oreland",              "Montgomery County",
  "Pennsburg",            "Montgomery County",
  "Pottstown",            "Montgomery County",
  "Rydal",                "Montgomery County",
  "Schwenksville",        "Montgomery County",
  "Sellersville",         "Montgomery County",
  "Souderton",            "Montgomery County",
  "Stowe",                "Montgomery County",
  "Wales",                "Montgomery County",
  "Warminster",           "Montgomery County",  # actually Bucks — corrected below
  "Warrington",           "Montgomery County",  # actually Bucks — corrected below
  "Wayne",                "Delaware County",    # Lower Merion Twp -> Delaware/Montgomery border; Wayne is Montgomery
  "Wynnewood",            "Montgomery County",
  "Ardmore",              "Montgomery County",
  "Cynwyd",               "Montgomery County",
  "Gladwyne",             "Montgomery County",
  "Havertown",            "Delaware County",
  "Narberth",             "Montgomery County",
  "Rosemont",             "Delaware County",
  "Strafford",            "Chester County",
  "Mawr",                 "Montgomery County",  # Bryn Mawr
  # Bucks County, PA
  "Bensalem",             "Bucks County",
  "Bristol",              "Bucks County",
  "Chalfont",             "Bucks County",
  "Croydon",              "Bucks County",
  "Doylestown",           "Bucks County",
  "Feasterville",         "Bucks County",
  "Holland",              "Bucks County",
  "Jamison",              "Bucks County",
  "Langhorne",            "Bucks County",
  "Levittown",            "Bucks County",
  "Morrisville",          "Bucks County",
  "Newtown",              "Bucks County",
  "Ottsville",            "Bucks County",
  "Penndel",              "Bucks County",
  "Quakertown",           "Bucks County",
  "Richboro",             "Bucks County",
  "Riegelsville",         "Bucks County",
  "Southampton",          "Bucks County",
  "Warminster",           "Bucks County",
  "Warrington",           "Bucks County",
  "Yardley",              "Bucks County",
  # Chester County, PA
  "Avondale",             "Chester County",
  "Berwyn",               "Chester County",
  "Brandywine",           "Chester County",
  "Coatesville",          "Chester County",
  "Downingtown",          "Chester County",
  "Exton",                "Chester County",
  "Kimberton",            "Chester County",
  "Malvern",              "Chester County",
  "Oxford",               "Chester County",
  "Paoli",                "Chester County",
  "Parkesburg",           "Chester County",
  "Phoenixville",         "Chester County",
  "Royersford",           "Chester County",
  "Schwenksville",        "Montgomery County",
  "Uwchlan",              "Chester County",
  "West Chester",         "Chester County",
  # Delaware County, PA
  "Aston",                "Delaware County",
  "Boothwyn",             "Delaware County",
  "Brookhaven",           "Delaware County",
  "Broomall",             "Delaware County",
  "Chester",              "Delaware County",
  "Collingdale",          "Delaware County",
  "Darby",                "Delaware County",
  "Eddystone",            "Delaware County",
  "Glenolden",            "Delaware County",
  "Lansdowne",            "Delaware County",
  "Lenni",                "Delaware County",
  "Media",                "Delaware County",
  "Morton",               "Delaware County",
  "Norwood",              "Delaware County",
  "Primos",               "Delaware County",
  "Secane",               "Delaware County",
  "Springfield",          "Delaware County",
  "Wallingford",          "Delaware County",
  "Woodlyn",              "Delaware County",
  # Ambiguous multi-word place names (split by CSV comma issue)
  "Ford",                 "Delaware County",    # Haverford -> Delaware/Montgomery
  "Grove",                "Delaware County",    # Aston / Grove City
  "Hill",                 "Delaware County",    # Rose Valley / Drexel Hill area
  "Hills",                "Delaware County",
  "Heights",              "Philadelphia County",
  "Park",                 "Delaware County",    # Ridley Park / Drexel Park area
  "Square",               "Montgomery County",  # North Wales / Blue Bell / Maple Glen
  "Prussia",              "Montgomery County",  # King of Prussia
  "Meeting",              "Montgomery County",  # Plymouth Meeting
  "Mills",                "Montgomery County",  # Spring Mills / Flourtown area
  "City",                 "Philadelphia County",
  "Bell",                 "Montgomery County",  # Bell in Horsham area? unclear; assign Montgomery
  "Hope",                 "Bucks County",       # New Hope
  "Swedesburg",           "Montgomery County",
  "Valley",               "Chester County"      # Phoenixville/Valley Forge area
)

# Resolve duplicates (keep last): Warminster/Warrington should be Bucks
location_county_map <- location_county_map |>
  group_by(location) |>
  slice_tail(n = 1) |>
  ungroup()

cat(sprintf("\nLocation→county map entries: %d\n", nrow(location_county_map)))

# ── 5. Join founding years with county and compute founding lag ───────────────

founding_lag <- founding |>
  left_join(location_county_map, by = "location") |>
  left_join(county_cross, by = "county") |>
  mutate(founding_lag = founding_year - cross_year)

cat(sprintf("\nParishes with county assigned: %d / %d\n",
            sum(!is.na(founding_lag$county)), nrow(founding_lag)))
cat(sprintf("Parishes with founding_lag computed: %d\n",
            sum(!is.na(founding_lag$founding_lag))))
cat("Parishes missing county (location not mapped):\n")
founding_lag |>
  filter(is.na(county)) |>
  select(fs_number, parish_name, location) |>
  print(n = 30)

# ── 6. Cohort assignment ──────────────────────────────────────────────────────

# Philadelphia has many colonial/antebellum parishes → add Pre-industrial tier
cohort_labels <- function(lag) {
  case_when(
    is.na(lag)      ~ NA_character_,
    lag < -100      ~ "Pre-industrial (pre-1850s)",
    lag <  -10      ~ "Pre-suburban (1850s–1940s)",
    lag <   20      ~ "Early post-suburban (1950s–60s)",
    lag <   40      ~ "Late post-suburban (1970s–80s)",
    TRUE            ~ "Exurban (1990s+)"
  )
}

founding_lag <- founding_lag |>
  mutate(cohort = cohort_labels(founding_lag))

cat("\nCohort distribution:\n")
print(count(founding_lag, cohort, sort = TRUE))

# ── 7. Pivot panel to wide format ─────────────────────────────────────────────

panel_wide <- panel_long |>
  pivot_wider(names_from = variable, values_from = value) |>
  mutate(
    total_sacraments = coalesce(infant_baptisms, 0L) + coalesce(marriages, 0L),
    year_c = year - 2016
  )

cat(sprintf("\nWide panel: %d rows, %d columns\n", nrow(panel_wide), ncol(panel_wide)))
cat(sprintf("Years covered: %s\n", paste(sort(unique(panel_wide$year)), collapse = ", ")))
cat(sprintf("Parishes: %d\n", n_distinct(panel_wide$fs_number)))

# ── 8. Join panel with founding lag data ──────────────────────────────────────

philly_analysis <- panel_wide |>
  left_join(
    founding_lag |>
      select(fs_number, founding_year, location, county, cross_year,
             founding_lag, cohort),
    by = "fs_number"
  ) |>
  left_join(
    philly_geo |> select(fs_number, lat, lon),
    by = "fs_number"
  )

cat(sprintf("\nAnalysis dataset: %d rows\n", nrow(philly_analysis)))
cat(sprintf("Parishes with founding_lag: %d\n",
            n_distinct(philly_analysis$fs_number[!is.na(philly_analysis$founding_lag)])))

write_csv(philly_analysis, file.path(BASE, "philly_analysis.csv"))
cat("Saved philly_analysis.csv\n")

# ── 9. Panel regressions ──────────────────────────────────────────────────────

cat("\n=== Panel Regressions ===\n")

# Restrict to parishes with avg_weekend_attendance
pdata_full <- philly_analysis |>
  filter(!is.na(avg_weekend_attendance)) |>
  arrange(fs_number, year) |>
  pdata.frame(index = c("fs_number", "year"))

cat(sprintf("Parishes in attendance regression: %d\n",
            n_distinct(philly_analysis$fs_number[!is.na(philly_analysis$avg_weekend_attendance)])))

# Model 1: baseline time trend (parish fixed effects)
mod_time <- plm(avg_weekend_attendance ~ year_c,
                data = pdata_full,
                model = "within")

cat("\n--- Model 1: Baseline time trend (parish FE) ---\n")
print(summary(mod_time))

# Model 2: founding lag × time trend interaction
# Restrict to parishes where founding_lag is available
pdata_lag <- philly_analysis |>
  filter(!is.na(avg_weekend_attendance), !is.na(founding_lag)) |>
  arrange(fs_number, year) |>
  pdata.frame(index = c("fs_number", "year"))

cat(sprintf("\nParishes in interaction model: %d\n",
            n_distinct(philly_analysis$fs_number[
              !is.na(philly_analysis$avg_weekend_attendance) &
                !is.na(philly_analysis$founding_lag)])))

mod_interact <- plm(avg_weekend_attendance ~ year_c + year_c:founding_lag,
                    data = pdata_lag,
                    model = "within")

cat("\n--- Model 2: Founding lag × time trend interaction (parish FE) ---\n")
print(summary(mod_interact))

# ── 10. Save regression output ────────────────────────────────────────────────

results_path <- file.path(BASE, "philly_panel_results.txt")
sink(results_path)
cat("=== PHILADELPHIA CATHOLIC PARISH PANEL REGRESSIONS ===\n")
cat("Date:", format(Sys.Date()), "\n")
cat("Outcome: avg_weekend_attendance | Fixed effects: parish | 2012-2021\n\n")

cat("--- Model 1: Baseline time trend ---\n")
print(summary(mod_time))

cat("\n\n--- Model 2: Founding lag × time trend interaction ---\n")
print(summary(mod_interact))

cat("\n\n--- Key coefficients ---\n")
coef1 <- coef(summary(mod_time))
coef2 <- coef(summary(mod_interact))
cat(sprintf("year_c (baseline decline/yr):        %.2f  [p = %.4f]\n",
            coef1["year_c", "Estimate"], coef1["year_c", "Pr(>|t|)"]))
cat(sprintf("year_c:founding_lag (lag modifies trend): %.4f  [p = %.4f]\n",
            coef2["year_c:founding_lag", "Estimate"],
            coef2["year_c:founding_lag", "Pr(>|t|)"]))
sink()

cat(sprintf("\nRegression output saved to %s\n", results_path))

# Also print key coefficients to console
cat("\n--- Key coefficients ---\n")
cat(sprintf("year_c (baseline decline/yr):            %.2f  [p = %.4f]\n",
            coef1["year_c", "Estimate"], coef1["year_c", "Pr(>|t|)"]))
cat(sprintf("year_c:founding_lag (lag modifies trend): %.4f  [p = %.4f]\n",
            coef2["year_c:founding_lag", "Estimate"],
            coef2["year_c:founding_lag", "Pr(>|t|)"]))

# ── 11. Cohort comparison table ───────────────────────────────────────────────

cat("\n=== Cohort Comparison Table ===\n")

cohort_summary <- philly_analysis |>
  filter(!is.na(cohort), year %in% c(2012, 2021)) |>
  group_by(cohort, year) |>
  summarise(
    n_parishes            = n_distinct(fs_number),
    mean_attendance       = mean(avg_weekend_attendance, na.rm = TRUE),
    mean_total_sacraments = mean(total_sacraments, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from  = year,
    values_from = c(n_parishes, mean_attendance, mean_total_sacraments)
  ) |>
  mutate(
    attendance_change_abs = mean_attendance_2021 - mean_attendance_2012,
    attendance_change_pct = (mean_attendance_2021 - mean_attendance_2012) /
      mean_attendance_2012 * 100,
    sacraments_change_abs = mean_total_sacraments_2021 - mean_total_sacraments_2012,
    sacraments_change_pct = (mean_total_sacraments_2021 - mean_total_sacraments_2012) /
      mean_total_sacraments_2012 * 100
  )

# Order cohorts chronologically
cohort_order <- c("Pre-industrial (pre-1850s)", "Pre-suburban (1850s–1940s)",
                  "Early post-suburban (1950s–60s)", "Late post-suburban (1970s–80s)",
                  "Exurban (1990s+)")
cohort_summary <- cohort_summary |>
  mutate(cohort = factor(cohort, levels = cohort_order)) |>
  arrange(cohort)

print(cohort_summary |>
        select(cohort, n_parishes_2012, mean_attendance_2012, mean_attendance_2021,
               attendance_change_abs, attendance_change_pct,
               sacraments_change_abs, sacraments_change_pct))

write_csv(cohort_summary, file.path(BASE, "philly_cohort_summary.csv"))
cat("Saved philly_cohort_summary.csv\n")

cat("\n=== Done ===\n")
