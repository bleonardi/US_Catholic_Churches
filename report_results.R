library(tidyverse); library(readxl); library(sf); library(spdep); library(fixest)

church_data <- read_csv("national_church_data.csv", show_col_types=FALSE) %>%
  filter(church_address_country_territory_name=="United States",
         !is.na(latitude), !is.na(longitude), is.finite(latitude), is.finite(longitude),
         latitude > 18, latitude < 72, longitude > -180, longitude < -60,
         church_type_name %in% c("Parish","Cathedral","Basilica","Mission","Shrine"))

worship <- read_csv("national_worship_times.csv", show_col_types=FALSE) %>%
  group_by(psuedo_id, service_typename) %>% summarize(ct=n(),.groups="drop") %>%
  pivot_wider(names_from=service_typename, values_from=ct, values_fill=0L) %>%
  rename_with(~paste0("n_",str_to_lower(str_replace_all(.," ","_"))),.cols=-psuedo_id)
church_data <- church_data %>% left_join(worship, by="psuedo_id")

nchs <- read_excel("NCHSURCodes2013.xlsx") %>%
  transmute(county_fips=str_pad(as.character(`FIPS code`),5,pad="0"),
            cbsa_title=`CBSA title`, ur_code=`2013 code`,
            ur_type=case_when(ur_code==1~"Urban core",ur_code==2~"Suburban",
                              ur_code%in%3:4~"Smaller metro",ur_code%in%5:6~"Rural"))

zip_county <- read_csv("zip_county_crosswalk.csv", show_col_types=FALSE) %>%
  transmute(zip=str_pad(as.character(ZCTA5),5,pad="0"),
            county_fips=str_pad(as.character(GEOID),5,pad="0"), area_pct=ZPOPPCT) %>%
  group_by(zip) %>% slice_max(area_pct,n=1,with_ties=FALSE) %>% ungroup()

county_pop <- tidycensus::get_acs("county","B01003_001",year=2019) %>%
  transmute(county_fips=GEOID, county_pop=estimate)

arda_raw <- read_excel("arda_longitudinal_1980_2010.xlsx") %>%
  mutate(county_fips=str_pad(as.character(FIPSMERG),5,pad="0"))
cath_long <- arda_raw %>% filter(GRPCODE=="081") %>%
  transmute(county_fips, year=YEAR, totpop=TOTPOP,
            cath_adh=coalesce(ADHERENT,0L), pct_cath=cath_adh/totpop)
county_cath <- cath_long %>%
  pivot_wider(id_cols=county_fips, names_from=year,
              values_from=c(totpop,cath_adh,pct_cath), names_glue="{.value}_{year}") %>%
  mutate(cath_share_change_80_10=pct_cath_2010-pct_cath_1980,
         cath_quintile_1980=ntile(pct_cath_1980,5))

church_data <- church_data %>%
  mutate(zip=str_pad(str_sub(church_address_postal_code,1,5),5,pad="0")) %>%
  left_join(zip_county,by="zip") %>% left_join(county_pop,by="county_fips") %>%
  left_join(nchs,by="county_fips") %>% left_join(county_cath,by="county_fips") %>%
  mutate(n_weekend=coalesce(n_weekend,0L), n_confessions=coalesce(n_confessions,0L))

# ── Within-metro FE ──────────────────────────────────────────────────────────
cat("\n===== WITHIN-METRO FIXED EFFECTS =====\n")
mod_data <- church_data %>%
  filter(!is.na(cbsa_title), !is.na(ur_type), !is.na(pct_cath_2010),
         ur_type %in% c("Urban core","Suburban")) %>%
  mutate(is_suburban   = as.integer(ur_type=="Suburban"),
         log_county_pop = log(county_pop+1),
         n_weekend_trim = if_else(n_weekend>0 & n_weekend<15, n_weekend, NA_integer_)) %>%
  group_by(cbsa_title) %>% filter(n_distinct(ur_type)==2, n()>=10) %>% ungroup()

cat("N parishes:", nrow(mod_data), " | N CBSAs:", n_distinct(mod_data$cbsa_title), "\n\n")

fe_mass    <- feols(n_weekend_trim ~ is_suburban + pct_cath_2010 + log_county_pop | cbsa_title,
                    data=mod_data %>% filter(!is.na(n_weekend_trim)), vcov="hetero")
fe_conf    <- feols(n_confessions ~ is_suburban + pct_cath_2010 + log_county_pop | cbsa_title,
                    data=mod_data, vcov="hetero")
fe_conf_any <- feols(I(n_confessions>0) ~ is_suburban + pct_cath_2010 + log_county_pop | cbsa_title,
                     data=mod_data, vcov="hetero")

cat(sprintf("Weekend masses     — suburban β: %+.3f  (p=%.4f)\n",
            coef(fe_mass)["is_suburban"], pvalue(fe_mass)["is_suburban"]))
cat(sprintf("Confession sessions— suburban β: %+.3f  (p=%.4f)\n",
            coef(fe_conf)["is_suburban"], pvalue(fe_conf)["is_suburban"]))
cat(sprintf("Any confession LPM — suburban β: %+.3f  (p=%.4f)\n",
            coef(fe_conf_any)["is_suburban"], pvalue(fe_conf_any)["is_suburban"]))

# ── Interaction ───────────────────────────────────────────────────────────────
cat("\n===== INTERACTION: SUBURBAN x METRO CATHOLIC SHARE (1980) =====\n")
metro_cath_1980 <- church_data %>% filter(!is.na(cbsa_title),!is.na(pct_cath_1980)) %>%
  group_by(cbsa_title) %>%
  summarize(metro_pct_cath_1980=weighted.mean(pct_cath_1980,county_pop,na.rm=TRUE),.groups="drop")
mod_int <- mod_data %>% left_join(metro_cath_1980, by="cbsa_title")
fe_mass_int <- feols(n_weekend_trim ~ is_suburban*metro_pct_cath_1980 + log_county_pop | cbsa_title,
                     data=mod_int %>% filter(!is.na(n_weekend_trim)), vcov="hetero")
fe_conf_int <- feols(I(n_confessions>0) ~ is_suburban*metro_pct_cath_1980 + log_county_pop | cbsa_title,
                     data=mod_int, vcov="hetero")

cat(sprintf("Mass: suburban β: %+.3f  interaction β: %+.3f  (p=%.4f)\n",
            coef(fe_mass_int)["is_suburban"],
            coef(fe_mass_int)["is_suburban:metro_pct_cath_1980"],
            pvalue(fe_mass_int)["is_suburban:metro_pct_cath_1980"]))
cat(sprintf("Conf: suburban β: %+.3f  interaction β: %+.3f  (p=%.4f)\n",
            coef(fe_conf_int)["is_suburban"],
            coef(fe_conf_int)["is_suburban:metro_pct_cath_1980"],
            pvalue(fe_conf_int)["is_suburban:metro_pct_cath_1980"]))

# ── Moran's I ─────────────────────────────────────────────────────────────────
cat("\n===== GLOBAL MORANS I =====\n")
church_sf <- church_data %>%
  filter(!is.na(n_weekend), !is.na(latitude), !is.na(longitude),
         is.finite(latitude), is.finite(longitude),
         latitude > 18, latitude < 72, longitude > -180, longitude < -60) %>%
  mutate(n_weekend_z = as.numeric(scale(n_weekend)),
         conf_any    = as.integer(n_confessions > 0),
         conf_any_z  = as.numeric(scale(conf_any))) %>%
  st_as_sf(coords=c("longitude","latitude"), crs=4326) %>% st_transform(5070)

coords <- st_coordinates(church_sf)
lw8    <- nb2listw(knn2nb(knearneigh(coords,k=8)), style="W", zero.policy=TRUE)

mi_mass <- moran.test(church_sf$n_weekend_z, lw8, zero.policy=TRUE)
mi_conf <- moran.test(church_sf$conf_any_z,  lw8, zero.policy=TRUE)
cat(sprintf("Weekend masses  — Moran I: %.4f  (p < 2.2e-16: %s)\n",
            mi_mass$estimate[1], mi_mass$p.value < 2.2e-16))
cat(sprintf("Any confession  — Moran I: %.4f  (p < 2.2e-16: %s)\n",
            mi_conf$estimate[1], mi_conf$p.value < 2.2e-16))

# ── LISA × urban-rural ────────────────────────────────────────────────────────
cat("\n===== LISA x URBAN-RURAL (weekend masses) =====\n")
lisa_mass <- localmoran(church_sf$n_weekend_z, lw8, zero.policy=TRUE)
lag_z     <- lag.listw(lw8, church_sf$n_weekend_z, zero.policy=TRUE)
church_sf$lisa <- case_when(
  lisa_mass[,"Pr(z != E(Ii))"] < 0.05 & church_sf$n_weekend_z > 0 & lag_z > 0 ~ "HH",
  lisa_mass[,"Pr(z != E(Ii))"] < 0.05 & church_sf$n_weekend_z < 0 & lag_z < 0 ~ "LL",
  lisa_mass[,"Pr(z != E(Ii))"] < 0.05 ~ "Outlier",
  TRUE ~ "NS")

print(
  church_sf %>% st_drop_geometry() %>%
    filter(!is.na(ur_type), lisa %in% c("HH","LL")) %>%
    count(ur_type, lisa) %>%
    group_by(ur_type) %>% mutate(pct=round(n/sum(n)*100,1)) %>% ungroup() %>%
    arrange(lisa, ur_type)
)

cat("\nOf ALL LL parishes, share by ur_type:\n")
print(
  church_sf %>% st_drop_geometry() %>%
    filter(lisa=="LL", !is.na(ur_type)) %>%
    count(ur_type) %>% mutate(pct=round(n/sum(n)*100,1)) %>% arrange(desc(n))
)
