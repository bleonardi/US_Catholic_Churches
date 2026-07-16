library(tidyverse); library(readxl)

nchs <- read_excel("NCHSURCodes2013.xlsx") %>%
  transmute(county_fips = str_pad(as.character(`FIPS code`), 5, pad = "0"),
            cbsa_title  = `CBSA title`, ur_code = `2013 code`,
            ur_type = case_when(ur_code == 1 ~ "Urban core", ur_code == 2 ~ "Suburban",
                                ur_code %in% 3:4 ~ "Smaller metro", TRUE ~ "Rural"))

ccm_1952 <- read_excel("arda_ccm_1952_county.xls") %>%
  transmute(county_fips  = str_pad(as.character(STCODE * 1000 + CCODE), 5, pad = "0"),
            cath_cong_52 = coalesce(CATH_C, 0), totpop_52 = TOTPOP,
            pct_cath_1952 = coalesce(CATH_M, 0) / TOTPOP)

arda <- read_excel("arda_longitudinal_1980_2010.xlsx") %>%
  mutate(county_fips = str_pad(as.character(FIPSMERG), 5, pad = "0"))

cath_long <- arda %>% filter(GRPCODE == "081") %>%
  transmute(county_fips, year = YEAR, totpop = TOTPOP, cath_cong = coalesce(CONGREG, 0L))

county_data <- cath_long %>%
  left_join(nchs, by = "county_fips") %>% filter(!is.na(ur_type))

metro_cath_1952 <- ccm_1952 %>%
  left_join(nchs %>% select(county_fips, cbsa_title), by = "county_fips") %>%
  filter(!is.na(cbsa_title)) %>%
  group_by(cbsa_title) %>%
  summarize(metro_pct_cath_1952 = weighted.mean(pct_cath_1952, totpop_52, na.rm = TRUE),
            metro_pop_1952 = sum(totpop_52, na.rm = TRUE), .groups = "drop") %>%
  filter(metro_pop_1952 >= 250000) %>%
  mutate(cath_q_1952 = ntile(metro_pct_cath_1952, 5))

cat("Q5 metros (most Catholic 1952, pop > 250k):\n")
print(metro_cath_1952 %>% filter(cath_q_1952 == 5) %>%
  arrange(desc(metro_pct_cath_1952)) %>%
  mutate(pct = scales::percent(metro_pct_cath_1952, accuracy = 0.1)) %>%
  select(cbsa_title, pct))

cat("\nQ1 metros (least Catholic 1952, pop > 250k):\n")
print(metro_cath_1952 %>% filter(cath_q_1952 == 1) %>%
  arrange(metro_pct_cath_1952) %>%
  mutate(pct = scales::percent(metro_pct_cath_1952, accuracy = 0.1)) %>%
  select(cbsa_title, pct) %>% head(10))

county_q52 <- county_data %>%
  filter(!is.na(cbsa_title), ur_type %in% c("Urban core", "Suburban")) %>%
  left_join(metro_cath_1952, by = "cbsa_title") %>%
  filter(!is.na(cath_q_1952))

sub_share <- county_q52 %>%
  group_by(cath_q_1952, ur_type, year) %>%
  summarize(cath_cong = sum(cath_cong, na.rm = TRUE), .groups = "drop") %>%
  group_by(cath_q_1952, year) %>%
  mutate(sub_share = cath_cong / sum(cath_cong)) %>%
  ungroup() %>%
  filter(ur_type == "Suburban")

cat("\nSuburban share of Catholic congregations by 1952 quintile:\n")
print(sub_share %>% select(cath_q_1952, year, cath_cong, sub_share) %>%
  mutate(sub_share = scales::percent(sub_share, accuracy = 0.1)) %>%
  arrange(cath_q_1952, year))
