library(tidyverse); library(readxl)

nchs <- read_excel("NCHSURCodes2013.xlsx") %>%
  transmute(county_fips = str_pad(as.character(`FIPS code`), 5, pad = "0"),
            cbsa_title = `CBSA title`, ur_code = `2013 code`,
            ur_type = case_when(ur_code == 1 ~ "Urban core", ur_code == 2 ~ "Suburban",
                                ur_code %in% 3:4 ~ "Smaller metro", TRUE ~ "Rural"))

ccm_1952 <- read_excel("arda_ccm_1952_county.xls") %>%
  transmute(county_fips  = str_pad(as.character(STCODE * 1000 + CCODE), 5, pad = "0"),
            cath_cong_52 = coalesce(CATH_C, 0), cath_memb_52 = coalesce(CATH_M, 0),
            totpop_52 = TOTPOP, pct_cath_1952 = cath_memb_52 / totpop_52)

arda <- read_excel("arda_longitudinal_1980_2010.xlsx") %>%
  mutate(county_fips = str_pad(as.character(FIPSMERG), 5, pad = "0"))

cath_long <- arda %>% filter(GRPCODE == "081") %>%
  transmute(county_fips, year = YEAR, totpop = TOTPOP,
            cath_cong = coalesce(CONGREG, 0L), cath_adh = coalesce(ADHERENT, 0L))

cin_fips <- nchs %>% filter(str_detect(cbsa_title, "Cincinnati")) %>% pull(county_fips)

# 1980-2010 trajectory
cin_trend <- cath_long %>%
  filter(county_fips %in% cin_fips) %>%
  left_join(nchs %>% select(county_fips, ur_type), by = "county_fips") %>%
  group_by(year, ur_type) %>%
  summarize(cath_cong = sum(cath_cong), totpop = sum(totpop),
            cath_adh = sum(cath_adh), .groups = "drop") %>%
  mutate(pct_cath = cath_adh / totpop)

cat("Cincinnati 1980-2010 by ur_type:\n")
print(cin_trend %>% arrange(ur_type, year) %>%
  mutate(pct_cath = scales::percent(pct_cath, accuracy = 0.1)))

cat("\nSuburban share of Cincinnati Catholic congregations:\n")
print(cin_trend %>%
  group_by(year) %>%
  mutate(sub_share = cath_cong / sum(cath_cong)) %>%
  filter(ur_type == "Suburban") %>%
  select(year, cath_cong, sub_share) %>%
  mutate(sub_share = scales::percent(sub_share, accuracy = 0.1)))

# quintile placement
metro_cath_1952 <- ccm_1952 %>%
  left_join(nchs %>% select(county_fips, cbsa_title), by = "county_fips") %>%
  filter(!is.na(cbsa_title)) %>%
  group_by(cbsa_title) %>%
  summarize(metro_pct_cath_1952 = weighted.mean(pct_cath_1952, totpop_52, na.rm = TRUE),
            metro_pop_1952 = sum(totpop_52, na.rm = TRUE), .groups = "drop") %>%
  filter(metro_pop_1952 >= 250000) %>%
  mutate(cath_q_1952 = ntile(metro_pct_cath_1952, 5),
         rank = rank(desc(metro_pct_cath_1952)))

cat("\nCincinnati quintile:\n")
print(metro_cath_1952 %>% filter(str_detect(cbsa_title, "Cincinnati")) %>%
  mutate(pct = scales::percent(metro_pct_cath_1952, accuracy = 0.1)) %>%
  select(cbsa_title, pct, cath_q_1952, rank, metro_pop_1952))

cat("\nQ4 metros for context:\n")
print(metro_cath_1952 %>% filter(cath_q_1952 == 4) %>%
  arrange(desc(metro_pct_cath_1952)) %>%
  mutate(pct = scales::percent(metro_pct_cath_1952, accuracy = 0.1)) %>%
  select(cbsa_title, pct))
