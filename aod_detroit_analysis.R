suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
  library(fuzzyjoin)
  library(stringdist)
})

# ── Load AOD data + geography ─────────────────────────────────────────────────
aod <- read_csv("aod_parish_data.csv", show_col_types = FALSE)
geo <- read_csv("aod_geography.csv",    show_col_types = FALSE) |>
  select(path, pct_within_parish, pct_outside_region)
aod <- aod |> left_join(geo, by = "path")
cat("AOD parishes:", nrow(aod), "  with geography:", sum(!is.na(aod$pct_within_parish)), "\n")

# ── Detroit-area masstimes parishes ──────────────────────────────────────────
zip_county <- read_csv("zip_county_crosswalk.csv", show_col_types = FALSE) |>
  transmute(
    zip         = str_pad(as.character(ZCTA5), 5, pad = "0"),
    county_fips = str_pad(as.character(GEOID), 5, pad = "0")
  ) |>
  group_by(zip) |> slice(1) |> ungroup()

mt <- read_csv("national_church_data.csv", show_col_types = FALSE) |>
  filter(
    church_address_country_territory_name == "United States",
    str_detect(church_address_county, "Wayne|Oakland|Macomb|Monroe|Washtenaw|Livingston")
  ) |>
  select(psuedo_id, name, church_address_postal_code) |>
  mutate(zip = str_pad(str_sub(church_address_postal_code, 1, 5), 5, pad = "0")) |>
  left_join(zip_county, by = "zip")

# ── County suburban crossing years ───────────────────────────────────────────
urb <- bind_rows(
  read_csv("county_urban_shares_1950.csv", show_col_types = FALSE) |> mutate(snap_year = 1950),
  read_csv("county_urban_shares_1970.csv", show_col_types = FALSE) |> mutate(snap_year = 1970),
  read_csv("county_urban_shares_2000.csv", show_col_types = FALSE) |> mutate(snap_year = 2000),
  read_csv("county_urban_shares_2010.csv", show_col_types = FALSE) |> mutate(snap_year = 2010)
)

crossing_year <- urb |>
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

mt <- mt |> left_join(crossing_year, by = "county_fips")
cat("MT Detroit parishes with cross_year:", sum(!is.na(mt$cross_year)), "\n")

# ── Fuzzy join AOD ↔ masstimes ───────────────────────────────────────────────
clean_name <- function(x) {
  x |>
    str_to_lower() |>
    str_remove("\\s+parish$|\\s+church$") |>
    str_remove_all("[^a-z ]") |>
    str_squish()
}

aod2 <- aod |> mutate(name_key = clean_name(short_name))
mt2  <- mt  |> mutate(name_key = clean_name(name))

joined <- stringdist_left_join(aod2, mt2, by = "name_key", method = "jw", max_dist = 0.25) |>
  group_by(path) |>
  slice_min(stringdist(name_key.x, name_key.y, method = "jw"), with_ties = FALSE) |>
  ungroup()

cat("AOD parishes matched with cross_year:", sum(!is.na(joined$cross_year)), "\n")

# ── Founding lag and cohort ───────────────────────────────────────────────────
joined <- joined |>
  mutate(
    founding_lag = founding_year - cross_year,
    cohort = case_when(
      founding_lag < -10   ~ "Pre-suburban",
      founding_lag <  20   ~ "Early post-suburban (1950s-60s)",
      founding_lag <  40   ~ "Late post-suburban (1970s-80s)",
      !is.na(founding_lag) ~ "Exurban (1990s+)"
    ) |>
      factor(levels = c(
        "Pre-suburban",
        "Early post-suburban (1950s-60s)",
        "Late post-suburban (1970s-80s)",
        "Exurban (1990s+)"
      ))
  )

# ── COHORT OUTCOMES ───────────────────────────────────────────────────────────
cat("\n=== COHORT OUTCOMES ===\n")
cohort_summary <- joined |>
  filter(!is.na(cohort)) |>
  group_by(cohort) |>
  summarize(
    n          = n(),
    baptisms   = round(mean(baptisms_pct_chg_15_24  * 100, na.rm = TRUE), 1),
    confirms   = round(mean(confirmations_pct_chg    * 100, na.rm = TRUE), 1),
    mass_cap   = round(mean(mass_pct_capacity        * 100, na.rm = TRUE), 1),
    pct_within = round(mean(pct_within_parish,              na.rm = TRUE), 1),
    n_geo      = sum(!is.na(pct_within_parish)),
    deferred_k = round(mean(deferred_maint_usd / 1000,      na.rm = TRUE), 0),
    .groups = "drop"
  )
print(cohort_summary)

# ── REGRESSIONS ───────────────────────────────────────────────────────────────
cat("\n=== REGRESSIONS (founding lag → outcomes) ===\n")
r <- joined |> filter(!is.na(founding_lag))

m1 <- feols(confirmations_pct_chg  ~ founding_lag, data = r |> filter(!is.na(confirmations_pct_chg)),  vcov = "hetero")
m2 <- feols(baptisms_pct_chg_15_24 ~ founding_lag, data = r |> filter(!is.na(baptisms_pct_chg_15_24)), vcov = "hetero")
m3 <- feols(pct_within_parish      ~ founding_lag, data = r |> filter(!is.na(pct_within_parish)),       vcov = "hetero")
m4 <- feols(mass_pct_capacity      ~ founding_lag, data = r |> filter(!is.na(mass_pct_capacity)),       vcov = "hetero")

etable(m1, m2, m3, m4,
       headers = c("Confirms %chg", "Baptisms %chg", "% Within boundary", "Mass % capacity"),
       digits = 4)

cat("\n=== KEY COEFFICIENTS ===\n")
cat(sprintf("Confirmations beta = %.4f  (p = %.4f)  -- per year later founded\n",
    coef(m1)["founding_lag"], pvalue(m1)["founding_lag"]))
cat(sprintf("Pct within parish beta = %.2f pp per year (p = %.4f)\n",
    coef(m3)["founding_lag"], pvalue(m3)["founding_lag"]))

# Investigate pre-suburban confirmations
cat("\n=== FOUNDING LAG DISTRIBUTION ===\n")
joined |> filter(!is.na(founding_lag)) |>
  summarize(
    min=min(founding_lag), q25=quantile(founding_lag,0.25),
    med=median(founding_lag), q75=quantile(founding_lag,0.75),
    max=max(founding_lag), n=n()
  ) |> print()

cat("\n=== PRE-SUBURBAN PARISHES (sample) ===\n")
joined |> filter(!is.na(cohort), cohort=="Pre-suburban") |>
  select(short_name, founding_year, cross_year, founding_lag, confirmations_pct_chg) |>
  arrange(founding_lag) |>
  slice_head(n=15) |> print()

cat("\n=== CONFIRMATION PCT CHANGE DISTRIBUTION ===\n")
summary(joined$confirmations_pct_chg)

write_csv(joined, "aod_detroit_analysis.csv")
message("Saved aod_detroit_analysis.csv")
