library(tidyverse)
library(readxl)
library(fixest)

# ── Load tract-based urban shares ─────────────────────────────────────────────

urb_1950 <- read_csv("county_urban_shares_1950.csv", show_col_types = FALSE) %>%
  select(county_fips, urban_share_1950 = urban_share, suburb_share_1950 = suburban_share)

urb_2000 <- read_csv("county_urban_shares_2000.csv", show_col_types = FALSE) %>%
  select(county_fips, urban_share_2000 = urban_share, suburb_share_2000 = suburban_share)

urb_2010 <- read_csv("county_urban_shares_2010.csv", show_col_types = FALSE) %>%
  select(county_fips, urban_share_2010 = urban_share, suburb_share_2010 = suburban_share)

# ── Load county population data ───────────────────────────────────────────────

ccm_1952 <- read_excel("arda_ccm_1952_county.xls") %>%
  transmute(
    county_fips  = str_pad(as.character(STCODE * 1000 + CCODE), 5, pad = "0"),
    totpop_52    = TOTPOP,
    cath_memb_52 = coalesce(CATH_M, 0),
    pct_cath_52  = cath_memb_52 / TOTPOP
  )

arda_raw <- read_excel("arda_longitudinal_1980_2010.xlsx") %>%
  mutate(county_fips = str_pad(as.character(FIPSMERG), 5, pad = "0"))

pop_county <- arda_raw %>%
  filter(YEAR %in% c(1980, 2010)) %>%
  distinct(county_fips, year = YEAR, totpop = TOTPOP)

# Census region lookup
region_lookup <- bind_rows(
  tibble(state_fips = c("09","23","25","33","34","36","42","44","50"), region = "Northeast"),
  tibble(state_fips = c("17","18","19","20","26","27","29","31","38","39","46","55"), region = "Midwest"),
  tibble(state_fips = c("01","05","10","11","12","13","21","22","24","28","37","40","45","47","48","51","54"), region = "South"),
  tibble(state_fips = c("02","04","06","08","15","16","30","32","35","41","49","53","56"), region = "West")
)

# CBSA membership (use NCHS just for metro assignment, not ur_type)
nchs <- read_excel("NCHSURCodes2013.xlsx") %>%
  transmute(
    county_fips = str_pad(as.character(`FIPS code`), 5, pad = "0"),
    cbsa_title  = `CBSA title`
  )

# ── Build county-level urban/suburban population using tract shares ────────────
# For counties with no 1950 tract coverage, use 2000 shares as fallback
# (these are mostly rural counties — low urban share either way)

county_1952 <- ccm_1952 %>%
  left_join(urb_1950, by = "county_fips") %>%
  left_join(urb_2000, by = "county_fips") %>%
  mutate(
    # fallback to 2000 shares if no 1950 tract coverage
    urban_sh  = coalesce(urban_share_1950,  urban_share_2000,  0),
    suburb_sh = coalesce(suburb_share_1950, suburb_share_2000, 0),
    urban_pop_52  = totpop_52 * urban_sh,
    suburb_pop_52 = totpop_52 * suburb_sh
  )

county_1980 <- pop_county %>%
  filter(year == 1980) %>%
  left_join(urb_2000, by = "county_fips") %>%   # 2000 tracts proxy for 1980
  mutate(
    urban_sh  = coalesce(urban_share_2000,  0),
    suburb_sh = coalesce(suburb_share_2000, 0),
    urban_pop_80  = totpop * urban_sh,
    suburb_pop_80 = totpop * suburb_sh
  )

county_2010 <- pop_county %>%
  filter(year == 2010) %>%
  left_join(urb_2010, by = "county_fips") %>%
  mutate(
    urban_sh  = coalesce(urban_share_2010,  0),
    suburb_sh = coalesce(suburb_share_2010, 0),
    urban_pop_10  = totpop * urban_sh,
    suburb_pop_10 = totpop * suburb_sh
  )

# ── Aggregate to metro ────────────────────────────────────────────────────────

metro_pop <- function(county_df, pop_cols, cbsa = nchs) {
  county_df %>%
    left_join(cbsa, by = "county_fips") %>%
    filter(!is.na(cbsa_title)) %>%
    group_by(cbsa_title) %>%
    summarize(across(all_of(pop_cols), ~sum(.x, na.rm = TRUE)), .groups = "drop")
}

metro_52 <- metro_pop(county_1952,
                      c("totpop_52","cath_memb_52","urban_pop_52","suburb_pop_52"))

metro_80 <- metro_pop(county_1980,
                      c("totpop","urban_pop_80","suburb_pop_80")) %>%
  rename(totpop_80 = totpop)

metro_10 <- metro_pop(county_2010,
                      c("totpop","urban_pop_10","suburb_pop_10")) %>%
  rename(totpop_10 = totpop)

# Metro Catholic share 1952 (weighted by county population)
metro_cath_52 <- county_1952 %>%
  left_join(nchs, by = "county_fips") %>%
  filter(!is.na(cbsa_title)) %>%
  group_by(cbsa_title) %>%
  summarize(
    metro_pct_cath_52 = weighted.mean(pct_cath_52, totpop_52, na.rm = TRUE),
    metro_pop_52      = sum(totpop_52, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(metro_pop_52 >= 250000)

# Region from urban core counties
metro_region <- nchs %>%
  mutate(state_fips = str_sub(county_fips, 1, 2)) %>%
  left_join(region_lookup, by = "state_fips") %>%
  filter(!is.na(region), !is.na(cbsa_title)) %>%
  group_by(cbsa_title) %>%
  summarize(region = names(sort(table(region), decreasing = TRUE))[1], .groups = "drop")

# ── Assemble analysis dataset ──────────────────────────────────────────────────

metro_df <- metro_cath_52 %>%
  left_join(metro_52, by = "cbsa_title") %>%
  left_join(metro_80,  by = "cbsa_title") %>%
  left_join(metro_10,  by = "cbsa_title") %>%
  left_join(metro_region, by = "cbsa_title") %>%
  filter(!is.na(region)) %>%
  mutate(
    # Suburbanization = suburban share of (urban + suburban) population
    sub_share_52 = suburb_pop_52 / (urban_pop_52 + suburb_pop_52),
    sub_share_80 = suburb_pop_80 / (urban_pop_80 + suburb_pop_80),
    sub_share_10 = suburb_pop_10 / (urban_pop_10 + suburb_pop_10),
    d_sub_52_80  = sub_share_80 - sub_share_52,
    d_sub_80_10  = sub_share_10 - sub_share_80,
    # Urban core % change
    urb_pct_chg_52_80 = (urban_pop_80 - urban_pop_52) / urban_pop_52,
    log_metro_pop_52   = log(metro_pop_52),
    cath_q_1952        = ntile(metro_pct_cath_52, 5)
  ) %>%
  filter(
    is.finite(sub_share_52), is.finite(sub_share_80),
    urban_pop_52 > 0, suburb_pop_52 > 0
  )

cat("Metro observations:", nrow(metro_df), "\n")
cat("By region:\n"); print(table(metro_df$region))

# ── Descriptive by quintile ───────────────────────────────────────────────────

cat("\n=== Tract-based suburbanization by 1952 Catholic quintile ===\n")
print(
  metro_df %>%
    group_by(cath_q_1952) %>%
    summarize(
      n        = n(),
      pct_cath = scales::percent(mean(metro_pct_cath_52), accuracy = 0.1),
      sub_52   = scales::percent(mean(sub_share_52, na.rm = TRUE), accuracy = 0.1),
      sub_80   = scales::percent(mean(sub_share_80, na.rm = TRUE), accuracy = 0.1),
      d_52_80  = scales::percent(mean(d_sub_52_80,  na.rm = TRUE), accuracy = 0.1),
      .groups = "drop"
    )
)

cat("\nQ5 metros (most Catholic):\n")
print(
  metro_df %>% filter(cath_q_1952 == 5) %>%
    arrange(desc(metro_pct_cath_52)) %>%
    mutate(across(c(metro_pct_cath_52, sub_share_52, sub_share_80, d_sub_52_80),
                  ~scales::percent(., accuracy = 0.1))) %>%
    select(cbsa_title, metro_pct_cath_52, sub_share_52, sub_share_80, d_sub_52_80, region)
)

cat("\nQ1 metros (least Catholic):\n")
print(
  metro_df %>% filter(cath_q_1952 == 1) %>%
    arrange(metro_pct_cath_52) %>%
    mutate(across(c(metro_pct_cath_52, sub_share_52, sub_share_80, d_sub_52_80),
                  ~scales::percent(., accuracy = 0.1))) %>%
    select(cbsa_title, metro_pct_cath_52, sub_share_52, sub_share_80, d_sub_52_80, region) %>%
    head(10)
)

# ── Regressions ───────────────────────────────────────────────────────────────

cat("\n=== REGRESSION (tract-based DV) ===\n")

m1 <- feols(d_sub_52_80 ~ metro_pct_cath_52 + log_metro_pop_52,
            data = metro_df, vcov = "hetero")
m2 <- feols(d_sub_52_80 ~ metro_pct_cath_52 + log_metro_pop_52 | region,
            data = metro_df, vcov = "hetero")
m3 <- feols(urb_pct_chg_52_80 ~ metro_pct_cath_52 + log_metro_pop_52 | region,
            data = metro_df, vcov = "hetero")
m4 <- feols(d_sub_80_10 ~ metro_pct_cath_52 + log_metro_pop_52 | region,
            data = metro_df %>% filter(is.finite(d_sub_80_10)), vcov = "hetero")

etable(m1, m2, m3, m4,
       headers = c("ΔSub share (no FE)", "ΔSub share (region FE)",
                   "Urban core pop Δ% (region FE)", "ΔSub share 80-10 (region FE)"),
       digits = 4)

cat(sprintf("\nKey β (region FE): %.4f  p=%.4f\n",
            coef(m2)["metro_pct_cath_52"], pvalue(m2)["metro_pct_cath_52"]))
cat(sprintf("10pp more Catholic → %+.2fpp suburban share shift\n",
            coef(m2)["metro_pct_cath_52"] * 0.10 * 100))

# ── Within-region ─────────────────────────────────────────────────────────────

cat("\n=== WITHIN-REGION (tract-based) ===\n")
for (reg in c("Northeast", "Midwest", "South")) {
  d <- metro_df %>% filter(region == reg)
  if (nrow(d) < 5) next
  m_sub <- lm(d_sub_52_80        ~ metro_pct_cath_52 + log_metro_pop_52, data = d)
  m_urb <- lm(urb_pct_chg_52_80  ~ metro_pct_cath_52 + log_metro_pop_52, data = d)
  sub_b <- coef(m_sub)["metro_pct_cath_52"]
  sub_p <- summary(m_sub)$coef["metro_pct_cath_52","Pr(>|t|)"]
  urb_b <- coef(m_urb)["metro_pct_cath_52"]
  urb_p <- summary(m_urb)$coef["metro_pct_cath_52","Pr(>|t|)"]
  cat(sprintf("--- %s (n=%d, cath range %.1f%%–%.1f%%) ---\n",
              reg, nrow(d), min(d$metro_pct_cath_52)*100, max(d$metro_pct_cath_52)*100))
  cat(sprintf("  ΔSub share:        β=%.4f p=%.3f → 10pp Catholic = %+.2fpp\n", sub_b, sub_p, sub_b*.1*100))
  cat(sprintf("  Urban core Δpop%%: β=%.4f p=%.3f → 10pp Catholic = %+.2fpp\n\n", urb_b, urb_p, urb_b*.1*100))
}

# ── City spotlight ────────────────────────────────────────────────────────────

cat("\n=== NORTHEAST + MIDWEST CITY SPOTLIGHT ===\n")
print(
  metro_df %>%
    filter(region %in% c("Northeast", "Midwest")) %>%
    arrange(desc(metro_pct_cath_52)) %>%
    mutate(across(c(metro_pct_cath_52, sub_share_52, sub_share_80, d_sub_52_80, urb_pct_chg_52_80),
                  ~scales::percent(., accuracy = 0.1))) %>%
    select(cbsa_title, region, metro_pct_cath_52, sub_share_52, sub_share_80, d_sub_52_80, urb_pct_chg_52_80),
  n = 25
)
