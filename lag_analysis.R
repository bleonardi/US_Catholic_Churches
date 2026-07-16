library(tidyverse)
library(readxl)
library(sf)
library(fixest)

# ── Config ────────────────────────────────────────────────────────────────────
SUBURB_THRESHOLD <- 1000   # pop/sq mi — tract is "suburban" below urban threshold
URBAN_THRESHOLD  <- 5000

# ── 1. Load tract density snapshots ───────────────────────────────────────────
# One row per county per year, with urban/suburban population shares

urb <- bind_rows(
  read_csv("county_urban_shares_1950.csv", show_col_types = FALSE) %>% mutate(snap_year = 1950),
  read_csv("county_urban_shares_1970.csv", show_col_types = FALSE) %>% mutate(snap_year = 1970),
  read_csv("county_urban_shares_2000.csv", show_col_types = FALSE) %>% mutate(snap_year = 2000),
  read_csv("county_urban_shares_2010.csv", show_col_types = FALSE) %>% mutate(snap_year = 2010)
) %>%
  select(county_fips, snap_year, urban_share, suburban_share)

# ── 2. Estimate tract suburban-crossing year per county ───────────────────────
# "Crossing year" = when the county's suburban share first exceeded 40%
# (i.e., more people lived at suburban density than urban density)
# Interpolate linearly between snapshot years

SUBURB_CROSS_THRESHOLD <- 0.40

crossing_year <- urb %>%
  arrange(county_fips, snap_year) %>%
  group_by(county_fips) %>%
  group_modify(function(d, k) {
    d <- arrange(d, snap_year)
    crossed <- d$suburban_share >= SUBURB_CROSS_THRESHOLD

    # Never crossed threshold in any available snapshot
    if (!any(crossed)) {
      return(tibble(cross_year = NA_real_, cross_precision = "never", already_suburban_1950 = FALSE))
    }

    first_cross_idx <- which(crossed)[1]

    # Already suburban at first available snapshot
    if (first_cross_idx == 1) {
      # Extrapolate backward: assume linear trend, but cap at 1940
      if (nrow(d) >= 2 && d$suburban_share[2] > d$suburban_share[1]) {
        rate <- (d$suburban_share[2] - d$suburban_share[1]) / (d$snap_year[2] - d$snap_year[1])
        est  <- d$snap_year[1] - (d$suburban_share[1] - SUBURB_CROSS_THRESHOLD) / rate
        est  <- max(est, 1940)
      } else {
        est <- d$snap_year[1] - 10  # conservative: 10 years before first observation
      }
      already_1950 <- any(d$snap_year == 1950 & d$suburban_share >= SUBURB_CROSS_THRESHOLD)
      return(tibble(cross_year = est, cross_precision = "extrapolated_before", already_suburban_1950 = already_1950))
    }

    # Interpolate between the snapshot before and after crossing
    prev_idx <- first_cross_idx - 1
    y0 <- d$snap_year[prev_idx];       s0 <- d$suburban_share[prev_idx]
    y1 <- d$snap_year[first_cross_idx]; s1 <- d$suburban_share[first_cross_idx]
    est <- y0 + (SUBURB_CROSS_THRESHOLD - s0) / (s1 - s0) * (y1 - y0)

    precision <- if (y1 - y0 <= 20) "interpolated_tight" else "interpolated_wide"
    tibble(cross_year = est, cross_precision = precision, already_suburban_1950 = FALSE)
  }) %>%
  ungroup()

cat("Counties with estimated crossing year:", sum(!is.na(crossing_year$cross_year)), "\n")
cat("Precision breakdown:\n")
print(table(crossing_year$cross_precision, useNA="ifany"))

cat("Counties with estimated crossing year:", sum(!is.na(crossing_year$cross_year)), "\n")
cat("Already suburban by 1950:", sum(crossing_year$already_suburban_1950, na.rm=TRUE), "\n")

# ── 3. Assign churches to counties, attach crossing year ──────────────────────

nchs <- read_excel("NCHSURCodes2013.xlsx") %>%
  transmute(
    county_fips = str_pad(as.character(`FIPS code`), 5, pad = "0"),
    cbsa_title  = `CBSA title`
  )

zip_county <- read_csv("zip_county_crosswalk.csv", show_col_types = FALSE) %>%
  transmute(
    zip         = str_pad(as.character(ZCTA5), 5, pad = "0"),
    county_fips = str_pad(as.character(GEOID), 5, pad = "0"),
    area_pct    = ZPOPPCT
  ) %>%
  group_by(zip) %>%
  slice_max(area_pct, n = 1, with_ties = FALSE) %>%
  ungroup()

churches <- read_csv("national_church_data.csv", show_col_types = FALSE) %>%
  filter(
    church_address_country_territory_name == "United States",
    !is.na(latitude), !is.na(longitude), is.finite(latitude), is.finite(longitude),
    latitude > 18, latitude < 72, longitude > -180, longitude < -60,
    church_type_name %in% c("Parish", "Cathedral", "Basilica", "Mission", "Shrine")
  ) %>%
  mutate(zip = str_pad(str_sub(church_address_postal_code, 1, 5), 5, pad = "0")) %>%
  left_join(zip_county, by = "zip") %>%
  left_join(nchs,       by = "county_fips") %>%
  left_join(crossing_year, by = "county_fips")

# ── 4. Classify each church's location as urban/suburban using 2000 tracts ────
# (2000 is post-wave — reflects the built suburban landscape at its extent)
# For the lag analysis, we want suburban parishes: churches in suburban-density counties

urb_2000_church <- read_csv("county_urban_shares_2000.csv", show_col_types = FALSE) %>%
  transmute(
    county_fips,
    church_ur = case_when(
      urban_share   >= 0.5 ~ "urban",
      suburban_share >= 0.3 ~ "suburban",
      TRUE                  ~ "mixed/rural"
    )
  )

churches <- churches %>%
  left_join(urb_2000_church, by = "county_fips")

cat("\nChurch location classification (2000 density):\n")
print(table(churches$church_ur, useNA = "ifany"))

# ── 5. Metro Catholic concentration 1952 ──────────────────────────────────────

ccm_1952 <- read_excel("arda_ccm_1952_county.xls") %>%
  transmute(
    county_fips  = str_pad(as.character(STCODE * 1000 + CCODE), 5, pad = "0"),
    totpop_52    = TOTPOP,
    pct_cath_52  = coalesce(CATH_M, 0) / TOTPOP
  )

metro_cath_52 <- ccm_1952 %>%
  left_join(nchs, by = "county_fips") %>%
  filter(!is.na(cbsa_title)) %>%
  group_by(cbsa_title) %>%
  summarize(
    metro_pct_cath_52 = weighted.mean(pct_cath_52, totpop_52, na.rm = TRUE),
    metro_pop_52      = sum(totpop_52, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(metro_pop_52 >= 250000) %>%
  mutate(
    cath_q_1952      = ntile(metro_pct_cath_52, 5),
    log_metro_pop_52 = log(metro_pop_52)
  )

region_lookup <- bind_rows(
  tibble(state_fips = c("09","23","25","33","34","36","42","44","50"), region = "Northeast"),
  tibble(state_fips = c("17","18","19","20","26","27","29","31","38","39","46","55"), region = "Midwest"),
  tibble(state_fips = c("01","05","10","11","12","13","21","22","24","28","37","40","45","47","48","51","54"), region = "South"),
  tibble(state_fips = c("02","04","06","08","15","16","30","32","35","41","49","53","56"), region = "West")
)
metro_region <- nchs %>%
  mutate(state_fips = str_sub(county_fips, 1, 2)) %>%
  left_join(region_lookup, by = "state_fips") %>%
  filter(!is.na(region), !is.na(cbsa_title)) %>%
  group_by(cbsa_title) %>%
  summarize(region = names(sort(table(region), decreasing=TRUE))[1], .groups="drop")

metro_cath_52 <- metro_cath_52 %>% left_join(metro_region, by = "cbsa_title")

# ── 6. Founding year data (load checkpoint — whatever's been scraped so far) ──

CHECKPOINT <- "founding_years_checkpoint.csv"

if (file.exists(CHECKPOINT) && file.info(CHECKPOINT)$size > 100) {
  founding <- read_csv(CHECKPOINT, show_col_types = FALSE) %>%
    filter(!is.na(founding_year), founding_year >= 1800, founding_year <= 2024) %>%
    distinct(psuedo_id, .keep_all = TRUE)
  cat(sprintf("\nFounding years available: %d parishes\n", nrow(founding)))
} else {
  cat("\nNo founding year data yet — run scrape_founding_years.R first.\n")
  cat("Saving analysis skeleton for use once scraper completes.\n")
  founding <- tibble(psuedo_id = character(), founding_year = integer())
}

# ── 7. Analysis dataset: suburban parishes with founding years ────────────────

analysis <- churches %>%
  filter(
    !is.na(cbsa_title),
    church_ur == "suburban",
    !is.na(cross_year),
    !is.na(psuedo_id)
  ) %>%
  left_join(founding %>% select(psuedo_id, founding_year), by = "psuedo_id") %>%
  left_join(metro_cath_52, by = "cbsa_title") %>%
  filter(!is.na(cath_q_1952)) %>%
  mutate(
    # Lag = founding year minus county suburban crossing year
    # Positive = parish founded AFTER suburbanization (Church followed)
    # Negative = parish founded BEFORE suburbanization (Church led / pre-existed)
    parish_lag = founding_year - cross_year
  )

cat(sprintf("\nSuburban parishes in analysis metros: %d\n", nrow(analysis)))
cat(sprintf("With founding years: %d (%.1f%%)\n",
            sum(!is.na(analysis$founding_year)),
            mean(!is.na(analysis$founding_year)) * 100))

# ── 8. Metro-level lag summary ────────────────────────────────────────────────

metro_lag <- analysis %>%
  filter(!is.na(founding_year)) %>%
  group_by(cbsa_title, cath_q_1952, metro_pct_cath_52, log_metro_pop_52, region) %>%
  summarize(
    n_suburban_parishes   = n(),
    median_founding_year  = median(founding_year, na.rm = TRUE),
    median_lag            = median(parish_lag,    na.rm = TRUE),
    pct_pre_suburbanization = mean(parish_lag < 0, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n=== Metro lag by Catholic quintile ===\n")
print(
  metro_lag %>%
    group_by(cath_q_1952) %>%
    summarize(
      n_metros            = n(),
      pct_cath            = scales::percent(mean(metro_pct_cath_52), accuracy=0.1),
      med_founding_yr     = round(median(median_founding_year), 0),
      med_lag_yrs         = round(median(median_lag, na.rm=TRUE), 1),
      pct_pre_suburb      = scales::percent(mean(pct_pre_suburbanization, na.rm=TRUE), accuracy=0.1),
      .groups = "drop"
    )
)

# ── 9. Regression: metro median lag ~ Catholic concentration ──────────────────

cat("\n=== REGRESSION: parish founding lag ~ Catholic concentration ===\n")
cat("DV: median years between county suburban crossing and parish founding\n")
cat("(positive = Church lagged suburbanization; negative = Church preceded it)\n\n")

reg_data <- metro_lag %>% filter(!is.na(median_lag), !is.na(region))

m1 <- feols(median_lag ~ metro_pct_cath_52 + log_metro_pop_52,
            data = reg_data, vcov = "hetero")
m2 <- feols(median_lag ~ metro_pct_cath_52 + log_metro_pop_52 | region,
            data = reg_data, vcov = "hetero")
m3 <- feols(median_founding_year ~ metro_pct_cath_52 + log_metro_pop_52 | region,
            data = reg_data, vcov = "hetero")

etable(m1, m2, m3,
       headers = c("Lag (no FE)", "Lag (region FE)", "Founding year (region FE)"),
       digits = 3)

cat(sprintf("\nKey β (lag ~ cath, region FE): %.3f  p=%.4f\n",
            coef(m2)["metro_pct_cath_52"], pvalue(m2)["metro_pct_cath_52"]))
cat(sprintf("10pp more Catholic → %+.1f years longer lag\n",
            coef(m2)["metro_pct_cath_52"] * 0.10))

# ── 10. Coverage check: does hit rate vary by Catholic quintile? ──────────────
cat("\n=== SCRAPER COVERAGE CHECK (selection bias test) ===\n")
cat("If hit rate correlates with Catholic quintile, founding year data is biased.\n\n")
print(
  analysis %>%
    group_by(cath_q_1952) %>%
    summarize(
      n_parishes   = n(),
      n_with_year  = sum(!is.na(founding_year)),
      hit_rate     = scales::percent(mean(!is.na(founding_year)), accuracy=0.1),
      .groups = "drop"
    )
)
