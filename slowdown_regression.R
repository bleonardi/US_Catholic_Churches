library(tidyverse)
library(readxl)
library(fixest)

# ── Data ──────────────────────────────────────────────────────────────────────

nchs <- read_excel("NCHSURCodes2013.xlsx") %>%
  transmute(
    county_fips = str_pad(as.character(`FIPS code`), 5, pad = "0"),
    cbsa_title  = `CBSA title`,
    ur_code     = `2013 code`,
    ur_type     = case_when(
      ur_code == 1 ~ "Urban core",
      ur_code == 2 ~ "Suburban",
      ur_code %in% 3:4 ~ "Smaller metro",
      TRUE ~ "Rural"
    )
  )

ccm_1952 <- read_excel("arda_ccm_1952_county.xls") %>%
  transmute(
    county_fips  = str_pad(as.character(STCODE * 1000 + CCODE), 5, pad = "0"),
    totpop_52    = TOTPOP,
    cath_memb_52 = coalesce(CATH_M, 0),
    pct_cath_52  = cath_memb_52 / TOTPOP
  )

# Total county population 1980 from ARDA (TOTPOP is county total, same across denominations)
arda_raw <- read_excel("arda_longitudinal_1980_2010.xlsx") %>%
  mutate(county_fips = str_pad(as.character(FIPSMERG), 5, pad = "0"))

pop_county <- arda_raw %>%
  filter(YEAR %in% c(1980, 2010)) %>%
  distinct(county_fips, year = YEAR, totpop = TOTPOP)

# Census region lookup (state_fips → region)
northeast <- c("09","23","25","33","34","36","42","44","50")
midwest   <- c("17","18","19","20","26","27","29","31","38","39","46","55")
south     <- c("01","05","10","11","12","13","21","22","24","28","37","40","45","47","48","51","54")
west      <- c("02","04","06","08","15","16","30","32","35","41","49","53","56")

region_lookup <- bind_rows(
  tibble(state_fips = northeast, region = "Northeast"),
  tibble(state_fips = midwest,   region = "Midwest"),
  tibble(state_fips = south,     region = "South"),
  tibble(state_fips = west,      region = "West")
)

# ── Metro-level population by ur_type ─────────────────────────────────────────

# 1952: urban core and suburban pop from CCM
pop_1952_metro <- ccm_1952 %>%
  left_join(nchs %>% select(county_fips, cbsa_title, ur_type), by = "county_fips") %>%
  filter(!is.na(cbsa_title), ur_type %in% c("Urban core", "Suburban")) %>%
  group_by(cbsa_title, ur_type) %>%
  summarize(pop = sum(totpop_52, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = ur_type, values_from = pop,
              names_glue = "{gsub(' ','_',tolower(ur_type))}_pop_52")

# 1980: same geography
pop_1980_metro <- pop_county %>%
  filter(year == 1980) %>%
  left_join(nchs %>% select(county_fips, cbsa_title, ur_type), by = "county_fips") %>%
  filter(!is.na(cbsa_title), ur_type %in% c("Urban core", "Suburban")) %>%
  group_by(cbsa_title, ur_type) %>%
  summarize(pop = sum(totpop, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = ur_type, values_from = pop,
              names_glue = "{gsub(' ','_',tolower(ur_type))}_pop_80")

# 2010
pop_2010_metro <- pop_county %>%
  filter(year == 2010) %>%
  left_join(nchs %>% select(county_fips, cbsa_title, ur_type), by = "county_fips") %>%
  filter(!is.na(cbsa_title), ur_type %in% c("Urban core", "Suburban")) %>%
  group_by(cbsa_title, ur_type) %>%
  summarize(pop = sum(totpop, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = ur_type, values_from = pop,
              names_glue = "{gsub(' ','_',tolower(ur_type))}_pop_10")

# ── Metro Catholic concentration 1952 ─────────────────────────────────────────

metro_cath_1952 <- ccm_1952 %>%
  left_join(nchs %>% select(county_fips, cbsa_title), by = "county_fips") %>%
  filter(!is.na(cbsa_title)) %>%
  group_by(cbsa_title) %>%
  summarize(
    metro_pct_cath_52 = weighted.mean(pct_cath_52, totpop_52, na.rm = TRUE),
    metro_pop_52      = sum(totpop_52, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(metro_pop_52 >= 250000)

# ── Region: assign from modal state of metro's urban core counties ─────────────

metro_region <- nchs %>%
  filter(ur_type == "Urban core", !is.na(cbsa_title)) %>%
  mutate(state_fips = str_sub(county_fips, 1, 2)) %>%
  left_join(region_lookup, by = "state_fips") %>%
  filter(!is.na(region)) %>%
  group_by(cbsa_title) %>%
  summarize(region = names(sort(table(region), decreasing = TRUE))[1], .groups = "drop")

# ── Assemble analysis dataset ──────────────────────────────────────────────────

metro_df <- metro_cath_1952 %>%
  left_join(pop_1952_metro, by = "cbsa_title") %>%
  left_join(pop_1980_metro, by = "cbsa_title") %>%
  left_join(pop_2010_metro, by = "cbsa_title") %>%
  left_join(metro_region,   by = "cbsa_title") %>%
  filter(
    !is.na(urban_core_pop_52), !is.na(suburban_pop_52),
    !is.na(urban_core_pop_80), !is.na(suburban_pop_80),
    !is.na(region)
  ) %>%
  mutate(
    # Suburbanization rate: shift in suburban share of metro (core+suburb) population
    sub_share_52    = suburban_pop_52 / (urban_core_pop_52 + suburban_pop_52),
    sub_share_80    = suburban_pop_80 / (urban_core_pop_80 + suburban_pop_80),
    sub_share_10    = suburban_pop_10 / (urban_core_pop_10 + suburban_pop_10),
    # DV: change in suburban share 1952→1980 (the suburbanization wave)
    d_sub_share_52_80 = sub_share_80 - sub_share_52,
    # Also: 1980→2010 tail
    d_sub_share_80_10 = sub_share_10 - sub_share_80,
    # Urban core absolute pop change
    urb_core_pct_chg_52_80 = (urban_core_pop_80 - urban_core_pop_52) / urban_core_pop_52,
    log_metro_pop_52 = log(metro_pop_52),
    cath_q_1952 = ntile(metro_pct_cath_52, 5)
  )

cat("Metro observations:", nrow(metro_df), "\n")
cat("By region:\n")
print(table(metro_df$region))

# ── Descriptive: suburbanization shift by Catholic quintile ───────────────────

cat("\n=== Suburbanization shift by 1952 Catholic quintile ===\n")
print(
  metro_df %>%
    group_by(cath_q_1952) %>%
    summarize(
      n          = n(),
      pct_cath   = scales::percent(mean(metro_pct_cath_52), accuracy = 0.1),
      sub_sh_52  = scales::percent(mean(sub_share_52, na.rm = TRUE), accuracy = 0.1),
      sub_sh_80  = scales::percent(mean(sub_share_80, na.rm = TRUE), accuracy = 0.1),
      d_52_80    = scales::percent(mean(d_sub_share_52_80, na.rm = TRUE), accuracy = 0.1),
      .groups = "drop"
    )
)

cat("\nQ5 metros (most Catholic):\n")
print(
  metro_df %>% filter(cath_q_1952 == 5) %>%
    arrange(desc(metro_pct_cath_52)) %>%
    mutate(
      pct_cath  = scales::percent(metro_pct_cath_52, accuracy = 0.1),
      sub_52    = scales::percent(sub_share_52, accuracy = 0.1),
      sub_80    = scales::percent(sub_share_80, accuracy = 0.1),
      d_52_80   = scales::percent(d_sub_share_52_80, accuracy = 0.1)
    ) %>%
    select(cbsa_title, pct_cath, sub_52, sub_80, d_52_80, region)
)

cat("\nQ1 metros (least Catholic):\n")
print(
  metro_df %>% filter(cath_q_1952 == 1) %>%
    arrange(metro_pct_cath_52) %>%
    mutate(
      pct_cath  = scales::percent(metro_pct_cath_52, accuracy = 0.1),
      sub_52    = scales::percent(sub_share_52, accuracy = 0.1),
      sub_80    = scales::percent(sub_share_80, accuracy = 0.1),
      d_52_80   = scales::percent(d_sub_share_52_80, accuracy = 0.1)
    ) %>%
    select(cbsa_title, pct_cath, sub_52, sub_80, d_52_80, region) %>%
    head(12)
)

# ── Regressions ───────────────────────────────────────────────────────────────

cat("\n=== REGRESSION: DV = change in suburban pop share, 1952→1980 ===\n")

# Bivariate
m1 <- feols(d_sub_share_52_80 ~ metro_pct_cath_52 + log_metro_pop_52,
            data = metro_df, vcov = "hetero")

# Region FE
m2 <- feols(d_sub_share_52_80 ~ metro_pct_cath_52 + log_metro_pop_52 | region,
            data = metro_df, vcov = "hetero")

# Robustness: DV = urban core % pop change (decline story)
m3 <- feols(urb_core_pct_chg_52_80 ~ metro_pct_cath_52 + log_metro_pop_52 | region,
            data = metro_df, vcov = "hetero")

# Tail: 1980→2010
m4 <- feols(d_sub_share_80_10 ~ metro_pct_cath_52 + log_metro_pop_52 | region,
            data = metro_df %>% filter(!is.na(d_sub_share_80_10)), vcov = "hetero")

etable(m1, m2, m3, m4,
       headers = c("ΔSuburban share (no FE)", "ΔSuburban share (region FE)",
                   "Urban core pop Δ% (region FE)", "ΔSub share 1980-2010 (region FE)"),
       digits = 4)

cat("\nInterpretation check — β on metro_pct_cath_52 in m2:\n")
# IV in decimal (0.42), DV in decimal (0.10) → coef * 0.10 * 100 for "10pp higher → Xpp shift"
cat(sprintf("  A 10pp higher Catholic share → %.2f pp change in suburban share shift\n",
            coef(m2)["metro_pct_cath_52"] * 0.10 * 100))
cat(sprintf("  p = %.4f\n", pvalue(m2)["metro_pct_cath_52"]))

# ── (a) Urban core population as DV ───────────────────────────────────────────

cat("\n\n=== (a) URBAN CORE POPULATION STORY ===\n")
cat("DV: % change in urban core population 1952→1980\n\n")

cat("By quintile:\n")
print(
  metro_df %>%
    group_by(cath_q_1952) %>%
    summarize(
      n           = n(),
      pct_cath    = scales::percent(mean(metro_pct_cath_52), accuracy = 0.1),
      urb_chg     = scales::percent(mean(urb_core_pct_chg_52_80, na.rm = TRUE), accuracy = 0.1),
      urb_pop_52  = scales::comma(mean(urban_core_pop_52, na.rm = TRUE), accuracy = 1),
      urb_pop_80  = scales::comma(mean(urban_core_pop_80, na.rm = TRUE), accuracy = 1),
      .groups = "drop"
    )
)

# Full regression table: urban core DV
m_urb1 <- feols(urb_core_pct_chg_52_80 ~ metro_pct_cath_52 + log_metro_pop_52,
                data = metro_df, vcov = "hetero")
m_urb2 <- feols(urb_core_pct_chg_52_80 ~ metro_pct_cath_52 + log_metro_pop_52 | region,
                data = metro_df, vcov = "hetero")

etable(m_urb1, m_urb2,
       headers = c("Urban core pop Δ% (no FE)", "Urban core pop Δ% (region FE)"),
       digits = 4)

cat(sprintf("\nβ on Catholic share (region FE): %.4f  (p=%.4f)\n",
            coef(m_urb2)["metro_pct_cath_52"],
            pvalue(m_urb2)["metro_pct_cath_52"]))
cat(sprintf("A 10pp higher Catholic share → %.2f pp change in urban core pop growth\n",
            coef(m_urb2)["metro_pct_cath_52"] * 0.10 * 100))

# ── (b) Within-region subsets ──────────────────────────────────────────────────

cat("\n\n=== (b) WITHIN-REGION REGRESSIONS ===\n")
cat("(Only Midwest+Northeast have enough Catholic variation to be meaningful)\n\n")

for (reg in c("Northeast", "Midwest", "South")) {
  d <- metro_df %>% filter(region == reg)
  cat(sprintf("--- %s (n=%d, Catholic range: %.1f%%–%.1f%%) ---\n",
              reg, nrow(d),
              min(d$metro_pct_cath_52) * 100,
              max(d$metro_pct_cath_52) * 100))
  if (nrow(d) < 5) { cat("  Too few observations\n\n"); next }

  # Suburban share DV
  m_sub <- lm(d_sub_share_52_80 ~ metro_pct_cath_52 + log_metro_pop_52, data = d)
  # Urban core DV
  m_urb <- lm(urb_core_pct_chg_52_80 ~ metro_pct_cath_52 + log_metro_pop_52, data = d)

  sub_b <- coef(m_sub)["metro_pct_cath_52"]
  sub_p <- summary(m_sub)$coef["metro_pct_cath_52", "Pr(>|t|)"]
  urb_b <- coef(m_urb)["metro_pct_cath_52"]
  urb_p <- summary(m_urb)$coef["metro_pct_cath_52", "Pr(>|t|)"]

  cat(sprintf("  ΔSuburban share:   β=%.4f (p=%.3f) → 10pp Catholic = %+.2fpp sub shift\n",
              sub_b, sub_p, sub_b * 0.10 * 100))
  cat(sprintf("  Urban core Δpop%%: β=%.4f (p=%.3f) → 10pp Catholic = %+.2fpp urban core growth\n\n",
              urb_b, urb_p, urb_b * 0.10 * 100))
}

# ── City-level spotlight for the narrative ────────────────────────────────────

cat("\n=== CITY SPOTLIGHT: Northeast + Midwest Q4/Q5 vs Q1/Q2 ===\n")
print(
  metro_df %>%
    filter(region %in% c("Northeast", "Midwest")) %>%
    arrange(desc(metro_pct_cath_52)) %>%
    mutate(
      pct_cath = scales::percent(metro_pct_cath_52, accuracy = 0.1),
      sub_52   = scales::percent(sub_share_52, accuracy = 0.1),
      sub_80   = scales::percent(sub_share_80, accuracy = 0.1),
      d_52_80  = scales::percent(d_sub_share_52_80, accuracy = 0.1),
      urb_chg  = scales::percent(urb_core_pct_chg_52_80, accuracy = 0.1)
    ) %>%
    select(cbsa_title, region, pct_cath, sub_52, sub_80, d_52_80, urb_chg),
  n = 25
)
