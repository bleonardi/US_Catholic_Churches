library(tidyverse)
library(readxl)
library(fixest)
library(fuzzyjoin)

# =============================================================================
# TWO-ACT ANALYSIS: Catholic Suburbanization → Parish Health Crisis
#
# Act 1 (National): Catholic concentration → pre-suburban parish founding
#   (lag_analysis.R establishes this; we re-run it here as context)
#
# Act 2 (Detroit AOD): Founding lag → contemporary parish health decline
#   - Source: AOD restructuring workbooks (209 parishes, Archdiocese of Detroit)
#   - Outcomes: mass attendance %, deferred maintenance, baptism/funeral ratio,
#               confirmations decline, net income trend
#   - Predictor: founding lag (years before/after county suburban crossing)
#
# National extension: Detroit lag-health relationship + ARDA national trends
# =============================================================================

SUBURB_THRESHOLD   <- 1000
URBAN_THRESHOLD    <- 5000
SUBURB_CROSS_THRESH <- 0.40

# ─────────────────────────────────────────────────────────────────────────────
# PART A: Replicate lag analysis (abridged) to get Detroit parish lags
# ─────────────────────────────────────────────────────────────────────────────

# ── A1. County suburban crossing years ───────────────────────────────────────

urb <- bind_rows(
  read_csv("county_urban_shares_1950.csv", show_col_types = FALSE) |> mutate(snap_year = 1950),
  read_csv("county_urban_shares_1970.csv", show_col_types = FALSE) |> mutate(snap_year = 1970),
  read_csv("county_urban_shares_2000.csv", show_col_types = FALSE) |> mutate(snap_year = 2000),
  read_csv("county_urban_shares_2010.csv", show_col_types = FALSE) |> mutate(snap_year = 2010)
) |> select(county_fips, snap_year, urban_share, suburban_share)

crossing_year <- urb |>
  arrange(county_fips, snap_year) |>
  group_by(county_fips) |>
  group_modify(function(d, k) {
    d <- arrange(d, snap_year)
    crossed <- d$suburban_share >= SUBURB_CROSS_THRESH
    if (!any(crossed))
      return(tibble(cross_year = NA_real_, cross_precision = "never"))
    first_idx <- which(crossed)[1]
    if (first_idx == 1) {
      if (nrow(d) >= 2 && d$suburban_share[2] > d$suburban_share[1]) {
        rate <- (d$suburban_share[2] - d$suburban_share[1]) / (d$snap_year[2] - d$snap_year[1])
        est  <- max(d$snap_year[1] - (d$suburban_share[1] - SUBURB_CROSS_THRESH) / rate, 1940)
      } else {
        est <- d$snap_year[1] - 10
      }
      return(tibble(cross_year = est, cross_precision = "extrapolated_before"))
    }
    y0 <- d$snap_year[first_idx - 1]; s0 <- d$suburban_share[first_idx - 1]
    y1 <- d$snap_year[first_idx];     s1 <- d$suburban_share[first_idx]
    est <- y0 + (SUBURB_CROSS_THRESH - s0) / (s1 - s0) * (y1 - y0)
    tibble(cross_year = est,
           cross_precision = if_else(y1 - y0 <= 20, "interpolated_tight", "interpolated_wide"))
  }) |>
  ungroup()

# ── A2. National church dataset (masstimes) ───────────────────────────────────

nchs <- read_excel("NCHSURCodes2013.xlsx") |>
  transmute(
    county_fips = str_pad(as.character(`FIPS code`), 5, pad = "0"),
    cbsa_title  = `CBSA title`
  )

zip_county <- read_csv("zip_county_crosswalk.csv", show_col_types = FALSE) |>
  transmute(
    zip         = str_pad(as.character(ZCTA5), 5, pad = "0"),
    county_fips = str_pad(as.character(GEOID), 5, pad = "0"),
    area_pct    = ZPOPPCT
  ) |>
  group_by(zip) |>
  slice_max(area_pct, n = 1, with_ties = FALSE) |>
  ungroup()

churches <- read_csv("national_church_data.csv", show_col_types = FALSE) |>
  filter(
    church_address_country_territory_name == "United States",
    !is.na(latitude), !is.na(longitude), is.finite(latitude), is.finite(longitude),
    latitude > 18, latitude < 72, longitude > -180, longitude < -60,
    church_type_name %in% c("Parish", "Cathedral", "Basilica", "Mission", "Shrine")
  ) |>
  mutate(zip = str_pad(str_sub(church_address_postal_code, 1, 5), 5, pad = "0")) |>
  left_join(zip_county,    by = "zip") |>
  left_join(nchs,          by = "county_fips") |>
  left_join(crossing_year, by = "county_fips")

# ── A3. Detroit metro churches with founding lags ─────────────────────────────
# Detroit CBSA: "Detroit-Warren-Dearborn, MI"

founding <- read_csv("founding_years_checkpoint.csv", show_col_types = FALSE) |>
  filter(!is.na(founding_year), founding_year >= 1800, founding_year <= 2024) |>
  distinct(psuedo_id, .keep_all = TRUE)

detroit_masstimes <- churches |>
  filter(str_detect(coalesce(cbsa_title, ""), "Detroit")) |>
  left_join(founding |> select(psuedo_id, founding_year), by = "psuedo_id") |>
  mutate(parish_lag = founding_year - cross_year)

cat(sprintf("Detroit parishes in masstimes: %d\n", nrow(detroit_masstimes)))
cat(sprintf("  With founding year: %d (%.0f%%)\n",
    sum(!is.na(detroit_masstimes$founding_year)),
    100 * mean(!is.na(detroit_masstimes$founding_year))))

# =============================================================================
# PART B: Load AOD data and fuzzy-join to masstimes
# =============================================================================

# ── B1. Load AOD extracted data ───────────────────────────────────────────────

aod_raw <- read_csv("aod_parish_data.csv", show_col_types = FALSE)

cat(sprintf("\nAOD parishes extracted: %d\n", nrow(aod_raw)))
cat(sprintf("  Founding year: %d (%.0f%%)\n",
    sum(!is.na(aod_raw$founding_year)), 100 * mean(!is.na(aod_raw$founding_year))))
cat(sprintf("  Mass %% capacity: %d (%.0f%%)\n",
    sum(!is.na(aod_raw$mass_pct_capacity)), 100 * mean(!is.na(aod_raw$mass_pct_capacity))))
cat(sprintf("  Deferred maint: %d (%.0f%%)\n",
    sum(!is.na(aod_raw$deferred_maint_usd)), 100 * mean(!is.na(aod_raw$deferred_maint_usd))))

# ── B2. Clean AOD parish names for joining ────────────────────────────────────

clean_name <- function(x) {
  x |>
    str_to_lower() |>
    str_remove_all("parish|basilica|shrine|cathedral|mission|church") |>
    str_remove_all("saint|st\\.?|our lady|holy|sacred|blessed|most") |>
    str_remove_all("[^a-z0-9 ]") |>
    str_squish()
}

aod <- aod_raw |>
  mutate(
    city      = str_extract(parish_name, "[^,]+$") |> str_trim(),
    short_name = str_remove(parish_name, ",.*$"),
    name_key  = clean_name(short_name)
  )

masstimes_detroit <- detroit_masstimes |>
  mutate(
    mt_name  = coalesce(name, ""),
    name_key = clean_name(mt_name),
    city_key = str_to_lower(str_trim(coalesce(church_address_county, "")))
  )

# ── B3. Fuzzy join on cleaned name + city ─────────────────────────────────────

aod_joined <- stringdist_left_join(
  aod,
  masstimes_detroit |> select(psuedo_id, mt_name, name_key, city_key,
                               county_fips, cross_year, founding_year,
                               parish_lag, latitude, longitude),
  by       = "name_key",
  method   = "jw",         # Jaro-Winkler: good for proper names
  max_dist = 0.25,
  distance_col = "name_dist"
) |>
  # If multiple matches, take closest
  group_by(parish_name) |>
  slice_min(name_dist, n = 1, with_ties = FALSE) |>
  ungroup()

cat(sprintf("\nAOD parishes matched to masstimes: %d / %d (%.0f%%)\n",
    sum(!is.na(aod_joined$psuedo_id)),
    nrow(aod),
    100 * mean(!is.na(aod_joined$psuedo_id))))

# ── B4. Reconcile founding years ─────────────────────────────────────────────
# AOD founding year (from parish history text) is authoritative where available.
# Use it to validate / override masstimes LLM extractions.

aod_joined <- aod_joined |>
  mutate(
    founding_year_aod = founding_year.x,   # from PDF history text
    founding_year_llm = founding_year.y,   # from masstimes LLM extraction
    founding_year_final = coalesce(founding_year_aod, founding_year_llm),
    # Recompute lag with best founding year
    parish_lag_final = founding_year_final - cross_year,
    # Flag: was AOD year very different from LLM extraction? (>5 yr = suspicious)
    year_discrepancy = abs(founding_year_aod - founding_year_llm)
  )

# Validation summary
matched_both <- aod_joined |>
  filter(!is.na(founding_year_aod), !is.na(founding_year_llm))

cat(sprintf("\n── Founding Year Validation ──\n"))
cat(sprintf("  Parishes with both AOD + LLM year: %d\n", nrow(matched_both)))
if (nrow(matched_both) > 0) {
  cat(sprintf("  Median absolute discrepancy: %.0f years\n",
      median(matched_both$year_discrepancy, na.rm = TRUE)))
  cat(sprintf("  Within 5 years: %.0f%%\n",
      100 * mean(matched_both$year_discrepancy <= 5, na.rm = TRUE)))
  cat(sprintf("  Within 1 year:  %.0f%%\n",
      100 * mean(matched_both$year_discrepancy <= 1, na.rm = TRUE)))
  cat("\n  Largest discrepancies:\n")
  matched_both |>
    filter(!is.na(year_discrepancy)) |>
    arrange(desc(year_discrepancy)) |>
    select(parish_name, founding_year_aod, founding_year_llm, year_discrepancy) |>
    slice_head(n = 10) |>
    print()
}

# =============================================================================
# PART C: ACT 2 — Founding lag → contemporary parish health (Detroit)
# =============================================================================

# ── C1. Build analysis dataset ────────────────────────────────────────────────

act2 <- aod_joined |>
  filter(!is.na(parish_lag_final), !is.na(cross_year)) |>
  mutate(
    # Core outcomes
    mass_pct          = mass_pct_capacity,
    ln_deferred_maint = log1p(deferred_maint_usd),
    net_margin_fy2425 = net_income_fy2425 / pmax(rev_fy2425, 1, na.rm = TRUE),

    # Composite decline index (standardize each component then average)
    # Higher = worse health. Flip signs so all point in same direction.
    z_mass        = -scale(mass_pct_capacity)[,1],           # low attendance = bad
    z_baptisms    = -scale(baptisms_pct_chg_15_24)[,1],      # decline = bad
    z_confirm     = -scale(confirmations_pct_chg)[,1],       # decline = bad
    z_deferred    =  scale(ln_deferred_maint)[,1],           # high maintenance = bad
    z_net_margin  = -scale(net_margin_fy2425)[,1],           # deficit = bad

    # Baptism/funeral ratio: values < 1 mean more funerals than baptisms
    # Use raw counts where available (not just % change)
    # For now proxy: parishes with large baptism decline more likely ratio < 1
    decline_index = rowMeans(
      cbind(z_mass, z_baptisms, z_confirm, z_deferred, z_net_margin),
      na.rm = TRUE
    ),

    # Categorical: founded before vs. after county suburbanization
    pre_suburban = parish_lag_final < 0,

    # Founding era (for descriptive breakdowns)
    founding_era = case_when(
      founding_year_final < 1920 ~ "Pre-1920 (urban era)",
      founding_year_final < 1950 ~ "1920-1949 (interwar)",
      founding_year_final < 1970 ~ "1950-1969 (postwar boom)",
      founding_year_final < 1990 ~ "1970-1989 (late suburban)",
      TRUE                        ~ "1990+ (exurban)"
    ) |> factor(levels = c("Pre-1920 (urban era)", "1920-1949 (interwar)",
                            "1950-1969 (postwar boom)", "1970-1989 (late suburban)",
                            "1990+ (exurban)"))
  )

cat(sprintf("\n── Act 2 Analysis Dataset ──\n"))
cat(sprintf("  Detroit AOD parishes: %d\n", nrow(act2)))
cat(sprintf("  Pre-suburban: %d (%.0f%%)\n",
    sum(act2$pre_suburban, na.rm = TRUE),
    100 * mean(act2$pre_suburban, na.rm = TRUE)))

# ── C2. Descriptive: health by founding era ───────────────────────────────────

cat("\n=== Health indicators by founding era ===\n")
act2 |>
  filter(!is.na(founding_era)) |>
  group_by(founding_era) |>
  summarize(
    n                    = n(),
    mass_pct_cap         = scales::percent(mean(mass_pct, na.rm = TRUE), accuracy = 1),
    med_lag_yrs          = round(median(parish_lag_final, na.rm = TRUE)),
    baptisms_pct_chg     = scales::percent(mean(baptisms_pct_chg_15_24, na.rm = TRUE), accuracy = 0.1),
    confirmations_pct_chg = scales::percent(mean(confirmations_pct_chg, na.rm = TRUE), accuracy = 0.1),
    med_deferred_maint   = scales::dollar(median(deferred_maint_usd, na.rm = TRUE), accuracy = 1000),
    pct_deficit_fy2425   = scales::percent(mean(net_income_fy2425 < 0, na.rm = TRUE), accuracy = 1),
    .groups = "drop"
  ) |>
  print(n = Inf)

# ── C3. Pre- vs. post-suburban comparison ────────────────────────────────────

cat("\n=== Pre-suburban vs. Post-suburban parishes ===\n")
act2 |>
  filter(!is.na(pre_suburban)) |>
  group_by(pre_suburban) |>
  summarize(
    label                = if_else(first(pre_suburban), "Pre-suburban", "Post-suburban"),
    n                    = n(),
    mass_pct_cap         = scales::percent(mean(mass_pct, na.rm = TRUE), accuracy = 1),
    baptisms_pct_chg     = scales::percent(mean(baptisms_pct_chg_15_24, na.rm = TRUE), accuracy = 0.1),
    confirmations_pct_chg = scales::percent(mean(confirmations_pct_chg, na.rm = TRUE), accuracy = 0.1),
    med_deferred_maint   = scales::dollar(median(deferred_maint_usd, na.rm = TRUE), accuracy = 1000),
    pct_deficit          = scales::percent(mean(net_income_fy2425 < 0, na.rm = TRUE), accuracy = 1),
    mean_decline_index   = round(mean(decline_index, na.rm = TRUE), 2),
    .groups = "drop"
  ) |>
  select(-pre_suburban) |>
  print()

# ── C4. Regressions: lag → each health outcome ───────────────────────────────

cat("\n=== ACT 2 REGRESSIONS: Founding lag → Parish health (Detroit AOD) ===\n")
cat("DV positive direction = better health\n")
cat("Coefficient on parish_lag: negative = parishes founded earlier are WORSE off\n\n")

reg <- act2 |> filter(!is.na(parish_lag_final))

# Mass attendance % capacity
m_mass <- feols(mass_pct ~ parish_lag_final,
                data = reg |> filter(!is.na(mass_pct)), vcov = "hetero")

# Baptisms % change
m_bapt <- feols(baptisms_pct_chg_15_24 ~ parish_lag_final,
                data = reg |> filter(!is.na(baptisms_pct_chg_15_24)), vcov = "hetero")

# Confirmations % change
m_conf <- feols(confirmations_pct_chg ~ parish_lag_final,
                data = reg |> filter(!is.na(confirmations_pct_chg)), vcov = "hetero")

# Log deferred maintenance (higher = worse, so expect negative coef on lag)
m_maint <- feols(ln_deferred_maint ~ parish_lag_final,
                 data = reg |> filter(!is.na(ln_deferred_maint)), vcov = "hetero")

# Composite decline index (higher = worse health)
m_index <- feols(decline_index ~ parish_lag_final,
                 data = reg |> filter(!is.na(decline_index)), vcov = "hetero")

etable(m_mass, m_bapt, m_conf, m_maint, m_index,
       headers = c("Mass % cap", "Baptisms Δ", "Confirmations Δ",
                   "ln(Deferred maint)", "Decline index"),
       digits = 4)

cat(sprintf("\nKey: 10 more years of lag (founded later) → mass attendance change: %+.1f pp\n",
            coef(m_mass)["parish_lag_final"] * 10 * 100))

# ── C5. Deferred maintenance deep dive ────────────────────────────────────────

cat("\n=== DEFERRED MAINTENANCE: Pre- vs. Post-suburban ===\n")
dm_data <- act2 |> filter(!is.na(deferred_maint_usd), !is.na(parish_lag_final))

# Per-revenue-dollar burden (maintenance / annual revenue)
dm_data <- dm_data |>
  mutate(maint_per_rev = deferred_maint_usd / pmax(rev_fy2425, 1, na.rm = TRUE))

cat(sprintf("Pre-suburban median deferred maintenance: %s\n",
    scales::dollar(median(dm_data$deferred_maint_usd[dm_data$pre_suburban], na.rm=TRUE))))
cat(sprintf("Post-suburban median deferred maintenance: %s\n",
    scales::dollar(median(dm_data$deferred_maint_usd[!dm_data$pre_suburban], na.rm=TRUE))))

m_dm_ratio <- feols(maint_per_rev ~ parish_lag_final,
                    data = dm_data, vcov = "hetero")
cat(sprintf("\nDeferred maint / revenue ~ lag: β = %.3f (p = %.3f)\n",
    coef(m_dm_ratio)["parish_lag_final"],
    pvalue(m_dm_ratio)["parish_lag_final"]))
cat("(Negative β: parishes founded earlier carry heavier maintenance burden per revenue dollar)\n")

# =============================================================================
# PART D: NATIONAL EXTENSION
# Combine Act 1 (national founding lag) with Act 2 (Detroit health evidence)
# =============================================================================

cat("\n\n=== NATIONAL EXTENSION ===\n")
cat("Strategy: Act 1 establishes CAUSE (Catholic concentration → early parish founding)\n")
cat("          Act 2 establishes EFFECT (early founding → health crisis) using Detroit\n")
cat("          National implication: metros with high Catholic concentration have the\n")
cat("          SAME pre-suburban parish stock as Detroit, facing the same crisis.\n\n")

# Load Act 1 results
ccm_1952 <- read_excel("arda_ccm_1952_county.xls") |>
  transmute(
    county_fips  = str_pad(as.character(STCODE * 1000 + CCODE), 5, pad = "0"),
    totpop_52    = TOTPOP,
    pct_cath_52  = coalesce(CATH_M, 0) / TOTPOP
  )

metro_cath_52 <- ccm_1952 |>
  left_join(nchs, by = "county_fips") |>
  filter(!is.na(cbsa_title)) |>
  group_by(cbsa_title) |>
  summarize(
    metro_pct_cath_52 = weighted.mean(pct_cath_52, totpop_52, na.rm = TRUE),
    metro_pop_52      = sum(totpop_52, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(metro_pop_52 >= 250000) |>
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
metro_region <- nchs |>
  mutate(state_fips = str_sub(county_fips, 1, 2)) |>
  left_join(region_lookup, by = "state_fips") |>
  filter(!is.na(region), !is.na(cbsa_title)) |>
  group_by(cbsa_title) |>
  summarize(region = names(sort(table(region), decreasing=TRUE))[1], .groups="drop")

metro_cath_52 <- metro_cath_52 |> left_join(metro_region, by = "cbsa_title")

# National analysis: % pre-suburban parishes by Catholic quintile
founding <- read_csv("founding_years_checkpoint.csv", show_col_types = FALSE) |>
  filter(!is.na(founding_year), founding_year >= 1800, founding_year <= 2024) |>
  distinct(psuedo_id, .keep_all = TRUE)

urb_2000 <- read_csv("county_urban_shares_2000.csv", show_col_types = FALSE) |>
  transmute(county_fips, church_ur = case_when(
    urban_share    >= 0.5 ~ "urban",
    suburban_share >= 0.3 ~ "suburban",
    TRUE                  ~ "mixed/rural"))

national_analysis <- churches |>
  filter(!is.na(cbsa_title), !is.na(cross_year)) |>
  left_join(urb_2000,  by = "county_fips") |>
  left_join(founding |> select(psuedo_id, founding_year), by = "psuedo_id") |>
  left_join(metro_cath_52, by = "cbsa_title") |>
  filter(!is.na(cath_q_1952), church_ur == "suburban", !is.na(founding_year)) |>
  mutate(parish_lag = founding_year - cross_year)

# National pre-suburban share by Catholic quintile
cat("National % pre-suburban parishes by Catholic concentration quintile:\n")
national_analysis |>
  group_by(cath_q_1952) |>
  summarize(
    n_parishes   = n(),
    pct_catholic = scales::percent(mean(metro_pct_cath_52), accuracy=0.1),
    med_lag      = round(median(parish_lag)),
    pct_pre_sub  = scales::percent(mean(parish_lag < 0), accuracy=1),
    .groups = "drop"
  ) |>
  print()

# Extrapolate deferred maintenance burden nationally using Detroit ratio
# (What would the national pre-suburban parish maintenance backlog look like
#  if Detroit's ratio holds?)
dm_pre  <- median(dm_data$maint_per_rev[dm_data$pre_suburban],  na.rm = TRUE)
dm_post <- median(dm_data$maint_per_rev[!dm_data$pre_suburban], na.rm = TRUE)

n_pre_sub_national  <- sum(national_analysis$parish_lag < 0)
n_post_sub_national <- sum(national_analysis$parish_lag >= 0)

# Average suburban parish revenue (rough: use median from available data)
med_rev_national <- median(act2$rev_fy2425, na.rm = TRUE)

implied_pre_maint  <- n_pre_sub_national  * dm_pre  * med_rev_national
implied_post_maint <- n_post_sub_national * dm_post * med_rev_national

cat(sprintf("\n── Implied National Deferred Maintenance Backlog (Detroit ratio applied) ──\n"))
cat(sprintf("Pre-suburban parishes (national): %d\n", n_pre_sub_national))
cat(sprintf("Post-suburban parishes (national): %d\n", n_post_sub_national))
cat(sprintf("Detroit median maint/revenue ratio — pre: %.2f  post: %.2f\n", dm_pre, dm_post))
cat(sprintf("Implied pre-suburban maintenance burden: %s\n",
    scales::dollar(implied_pre_maint, scale = 1e-9, suffix = "B", accuracy = 0.1)))
cat(sprintf("Implied post-suburban maintenance burden: %s\n",
    scales::dollar(implied_post_maint, scale = 1e-9, suffix = "B", accuracy = 0.1)))
cat("\nNote: rough order-of-magnitude estimate assuming Detroit ratios are representative.\n")

# =============================================================================
# PART E: Save outputs
# =============================================================================

write_csv(act2, "aod_act2_analysis.csv")
write_csv(national_analysis |>
            mutate(parish_lag = parish_lag) |>
            select(psuedo_id, cbsa_title, county_fips, founding_year, cross_year,
                   parish_lag, metro_pct_cath_52, cath_q_1952, region),
          "national_lag_analysis.csv")

cat("\n\nOutputs written:\n")
cat("  aod_act2_analysis.csv   — Detroit parish-level Act 2 data\n")
cat("  national_lag_analysis.csv — National Act 1 data\n")
cat("\nNext: run Suburbanization_Analysis.qmd to render full paper.\n")
