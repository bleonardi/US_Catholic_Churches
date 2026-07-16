library(tidyverse)
library(rvest)
library(httr)
library(furrr)

CHECKPOINT_FILE <- "founding_years_checkpoint.csv"
FULLTEXT_FILE   <- "scrape_fulltext.csv"
BACKFILL_FILE   <- "backfill_fulltext_checkpoint.csv"
WORKERS         <- 8
RATE_LIMIT_S    <- 1.5

# ── Identify targets: not_found rows missing from fulltext file ───────────────

checkpoint <- read_csv(CHECKPOINT_FILE, show_col_types = FALSE)
not_found  <- checkpoint %>% filter(source == "not_found")

already_have <- if (file.exists(FULLTEXT_FILE) && file.info(FULLTEXT_FILE)$size > 100) {
  read_csv(FULLTEXT_FILE, show_col_types = FALSE) %>% pull(psuedo_id)
} else {
  character(0)
}

already_backfilled <- if (file.exists(BACKFILL_FILE) && file.info(BACKFILL_FILE)$size > 100) {
  read_csv(BACKFILL_FILE, show_col_types = FALSE) %>% pull(psuedo_id)
} else {
  character(0)
}

targets <- not_found %>%
  filter(!psuedo_id %in% already_have, !psuedo_id %in% already_backfilled)

message(nrow(not_found), " total not_found rows")
message(length(already_have), " already have full text")
message(nrow(targets), " to backfill")

if (nrow(targets) == 0) {
  message("Nothing to do.")
  quit(save = "no")
}

# Join back to church data to get URLs
church_data <- read_csv("national_church_data.csv", show_col_types = FALSE) %>%
  filter(
    church_address_country_territory_name == "United States",
    !is.na(url), url != "",
    church_type_name %in% c("Parish", "Cathedral", "Basilica", "Mission", "Shrine")
  ) %>%
  mutate(
    clean_url = str_trim(url),
    clean_url = if_else(str_detect(clean_url, "^http"), clean_url, paste0("http://", clean_url))
  ) %>%
  select(psuedo_id, clean_url)

targets <- targets %>% left_join(church_data, by = "psuedo_id") %>%
  filter(!is.na(clean_url))

message(nrow(targets), " targets with URLs")

# Initialise backfill checkpoint
if (!file.exists(BACKFILL_FILE)) {
  write_csv(tibble(psuedo_id = character(), status = character()), BACKFILL_FILE)
}

# ── Scraping helpers (same as main scraper) ───────────────────────────────────

USER_AGENTS <- c(
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36",
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:125.0) Gecko/20100101 Firefox/125.0",
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 14_4) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.4 Safari/605.1.15"
)

wayback_url <- function(original_url) {
  api  <- paste0("https://archive.org/wayback/available?url=", URLencode(original_url, reserved = TRUE))
  resp <- tryCatch(GET(api, timeout(10), user_agent(USER_AGENTS[1])), error = function(e) NULL)
  if (is.null(resp) || status_code(resp) != 200) return(NA_character_)
  parsed <- tryCatch(content(resp, "parsed", simplifyVector = TRUE), error = function(e) NULL)
  url <- parsed$archived_snapshots$closest$url
  if (is.null(url) || length(url) == 0 || is.na(url)) return(NA_character_)
  url
}

scrape_text_from_url <- function(target_url, ua) {
  sess <- session(target_url, user_agent(ua), timeout(15))
  if (httr::status_code(sess$response) != 200) return(NA_character_)
  links     <- html_elements(sess, "a")
  link_text <- html_text(links) %>% str_trim()
  link_href <- html_attr(links, "href")
  idx <- grep("History|Our Story|Parish History|About Us|About Our Parish|Heritage|Who We Are",
              link_text, ignore.case = TRUE)[1]
  if (is.na(idx)) idx <- grep("About|Welcome|Mission", link_text, ignore.case = TRUE)[1]
  if (!is.na(idx) && !is.na(link_href[idx])) {
    abs_url <- url_absolute(link_href[idx], base = sess$url)
    sess <- tryCatch(session_jump_to(sess, abs_url), error = function(e) sess)
  }
  text <- sess %>% html_element("body") %>% html_text2()
  if (is.null(text) || is.na(text) || length(text) == 0) return(NA_character_)
  str_sub(text, 1, 12000)
}

fetch_page_text <- function(base_url) {
  if (is.na(base_url) || base_url == "") return(NA_character_)
  for (ua in USER_AGENTS) {
    text <- tryCatch(scrape_text_from_url(base_url, ua), error = function(e) NA_character_)
    if (!is.na(text) && nchar(text) > 200) return(text)
  }
  archived <- wayback_url(base_url)
  if (!is.na(archived)) {
    text <- tryCatch(scrape_text_from_url(archived, USER_AGENTS[1]), error = function(e) NA_character_)
    if (!is.na(text) && nchar(text) > 200) return(text)
  }
  NA_character_
}

# ── Parallel backfill ─────────────────────────────────────────────────────────

plan(multisession, workers = WORKERS)

BATCH_SIZE <- 100
batches    <- split(seq_len(nrow(targets)), ceiling(seq_len(nrow(targets)) / BATCH_SIZE))

message("Starting backfill: ", length(batches), " batches of up to ", BATCH_SIZE)

for (b in seq_along(batches)) {
  idx   <- batches[[b]]
  batch <- targets[idx, ]

  results <- future_pmap_dfr(
    list(psuedo_id = batch$psuedo_id, clean_url = batch$clean_url),
    function(psuedo_id, clean_url) {
      Sys.sleep(RATE_LIMIT_S + runif(1, 0, 0.5))
      text <- fetch_page_text(clean_url)
      tibble(psuedo_id = psuedo_id, full_text = coalesce(text, NA_character_))
    },
    .options = furrr_options(seed = TRUE)
  )

  # append fetched text to fulltext file
  got_text <- results %>% filter(!is.na(full_text))
  if (nrow(got_text) > 0) {
    write_csv(got_text, FULLTEXT_FILE, append = TRUE)
  }

  # mark all as attempted in backfill checkpoint
  write_csv(
    results %>% transmute(psuedo_id, status = if_else(is.na(full_text), "no_text", "ok")),
    BACKFILL_FILE, append = TRUE
  )

  message(sprintf("Backfill batch %d/%d — %d/%d got text",
                  b, length(batches), nrow(got_text), nrow(batch)))
}

plan(sequential)

final_fulltext <- read_csv(FULLTEXT_FILE, show_col_types = FALSE)
message("\nDone. Total rows in scrape_fulltext.csv: ", nrow(final_fulltext))
