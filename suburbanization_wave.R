library(tidyverse)
library(readxl)
library(scales)

# ── Data ─────────────────────────────────────────────────────────────────────

nchs <- read_excel("NCHSURCodes2013.xlsx") %>%
  transmute(
    county_fips = str_pad(as.character(`FIPS code`), 5, pad = "0"),
    cbsa_title  = `CBSA title`,
    ur_code     = `2013 code`,
    ur_type     = case_when(
      ur_code == 1 ~ "Urban core",
      ur_code == 2 ~ "Suburban",
      ur_code %in% 3:4 ~ "Smaller metro",
      ur_code %in% 5:6 ~ "Rural"
    )
  )

arda <- read_excel("arda_longitudinal_1980_2010.xlsx") %>%
  mutate(county_fips = str_pad(as.character(FIPSMERG), 5, pad = "0"))

# Catholic congregations + total population by county-year
cath <- arda %>%
  filter(GRPCODE == "081") %>%
  transmute(county_fips, year = YEAR, totpop = TOTPOP,
            cath_cong = coalesce(CONGREG, 0L),
            cath_adh  = coalesce(ADHERENT, 0L),
            pct_cath  = cath_adh / totpop)

# All denominations total congregations (for Catholic share of congregations)
all_cong <- arda %>%
  group_by(county_fips, year = YEAR) %>%
  summarize(tot_cong = sum(coalesce(CONGREG, 0L), na.rm = TRUE),
            totpop   = first(TOTPOP), .groups = "drop")

county_data <- cath %>%
  left_join(all_cong %>% select(county_fips, year, tot_cong), by = c("county_fips","year")) %>%
  left_join(nchs, by = "county_fips") %>%
  filter(!is.na(ur_type))

# ── 1. National: Catholic congregation share by ur_type over time ─────────────

cat("\n=== Catholic congregation share by urban-rural type ===\n")
cong_ur <- county_data %>%
  group_by(ur_type, year) %>%
  summarize(
    cath_cong = sum(cath_cong, na.rm = TRUE),
    tot_cong  = sum(tot_cong,  na.rm = TRUE),
    totpop    = sum(totpop,    na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  mutate(cath_cong_share = cath_cong / tot_cong,
         cath_per_100k   = cath_cong / totpop * 100000)

print(cong_ur %>%
  select(ur_type, year, cath_cong, cath_cong_share, cath_per_100k) %>%
  arrange(ur_type, year))

# ── 2. Indexed growth: congregations vs. population (1980 = 100) ─────────────

cat("\n=== Indexed growth 1980=100 ===\n")
indexed <- cong_ur %>%
  group_by(ur_type) %>%
  mutate(idx_cong    = cath_cong / cath_cong[year == 1980] * 100,
         idx_pop     = totpop    / totpop[year == 1980]    * 100,
         cong_vs_pop = idx_cong / idx_pop * 100) %>%
  ungroup()

print(indexed %>% select(ur_type, year, idx_cong, idx_pop, cong_vs_pop) %>% arrange(ur_type, year))

# ── 3. By metro Catholic concentration quintile ───────────────────────────────

cat("\n=== By metro Catholic concentration quintile (1980 baseline) ===\n")

# Metro Catholic share in 1980 — use CBSA-level weighted mean
metro_cath_1980 <- county_data %>%
  filter(year == 1980, !is.na(cbsa_title)) %>%
  group_by(cbsa_title) %>%
  summarize(metro_pct_cath_1980 = weighted.mean(pct_cath, totpop, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(metro_cath_q = ntile(metro_pct_cath_1980, 5))

county_q <- county_data %>%
  filter(!is.na(cbsa_title)) %>%
  left_join(metro_cath_1980, by = "cbsa_title") %>%
  filter(!is.na(metro_cath_q))

# Among urban-core and suburban counties only: how did congregation counts shift?
shift_by_q <- county_q %>%
  filter(ur_type %in% c("Urban core", "Suburban")) %>%
  group_by(metro_cath_q, ur_type, year) %>%
  summarize(cath_cong = sum(cath_cong, na.rm = TRUE),
            totpop    = sum(totpop,    na.rm = TRUE),
            .groups   = "drop") %>%
  group_by(metro_cath_q, ur_type) %>%
  mutate(idx_cong = cath_cong / first(cath_cong[year == 1980]) * 100,
         idx_pop  = totpop    / first(totpop[year == 1980])    * 100) %>%
  ungroup()

cat("\nCongregation index by Catholic concentration quintile + ur_type:\n")
print(shift_by_q %>% filter(year %in% c(1980, 2010)) %>%
  select(metro_cath_q, ur_type, year, idx_cong, idx_pop) %>%
  arrange(metro_cath_q, ur_type, year), n = 40)

# ── 4. The key ratio: suburban share of Catholic congregations over time ───────

cat("\n=== Suburban share of Catholic congregations (metro counties only) ===\n")
suburban_share <- county_q %>%
  filter(ur_type %in% c("Urban core", "Suburban")) %>%
  group_by(metro_cath_q, year, ur_type) %>%
  summarize(cath_cong = sum(cath_cong, na.rm = TRUE), .groups = "drop") %>%
  group_by(metro_cath_q, year) %>%
  mutate(suburban_cong_share = cath_cong / sum(cath_cong)) %>%
  ungroup() %>%
  filter(ur_type == "Suburban")

print(suburban_share %>% select(metro_cath_q, year, suburban_cong_share) %>%
  arrange(metro_cath_q, year))
