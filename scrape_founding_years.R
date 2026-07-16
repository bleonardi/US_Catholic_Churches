library(tidyverse)
library(rvest)
library(httr)
library(furrr)
library(ellmer)
library(stringr)

CHECKPOINT_FILE <- "founding_years_checkpoint.csv"
FULLTEXT_FILE   <- "scrape_fulltext.csv"   # full page text for not_found rows only
WORKERS         <- 8
RATE_LIMIT_S    <- 1.5  # seconds between requests per worker

# ---------------------------------------------------------------------------
# 1. Load church data, skip already-processed rows
# ---------------------------------------------------------------------------

church_data <- read_csv("national_church_data.csv", show_col_types = FALSE) %>%
  filter(
    church_address_country_territory_name == "United States",
    !is.na(url), url != "",
    church_type_name %in% c("Parish", "Cathedral", "Basilica", "Mission", "Shrine")
  ) %>%
  mutate(
    clean_url = str_trim(url),
    clean_url = if_else(str_detect(clean_url, "^http"), clean_url, paste0("http://", clean_url))
  )

if (file.exists(CHECKPOINT_FILE)) {
  done <- read_csv(CHECKPOINT_FILE, show_col_types = FALSE)
  church_data <- church_data %>% filter(!psuedo_id %in% done$psuedo_id)
  message(nrow(done), " already processed. ", nrow(church_data), " remaining.")
} else {
  write_csv(
    tibble(psuedo_id = character(), founding_year = integer(),
           source = character(), raw_text_snippet = character()),
    CHECKPOINT_FILE
  )
  message(nrow(church_data), " churches to process.")
}

# Initialise fulltext file if needed
if (!file.exists(FULLTEXT_FILE)) {
  write_csv(
    tibble(psuedo_id = character(), full_text = character()),
    FULLTEXT_FILE
  )
}

# ---------------------------------------------------------------------------
# 2. Scraping helpers
# ---------------------------------------------------------------------------

# Fast regex pass — no LLM cost for unambiguous matches
extract_year_regex <- function(text) {
  if (is.na(text) || nchar(text) < 10) return(NA_integer_)

  patterns <- c(
    # "founded/established/organized in YYYY"
    "(?:founded|established|organized|incorporated|dedicated|built|erected|opened|began|started)\\s+(?:in\\s+)?(1[789]\\d{2}|20[012]\\d)",
    # "since YYYY" near parish/church/congregation
    "(?:since|as early as)\\s+(1[789]\\d{2}|20[012]\\d)",
    # "in the year YYYY" / "the year YYYY"
    "in\\s+the\\s+year\\s+(1[789]\\d{2}|20[012]\\d)",
    # "YYYY — founding/establishment"
    "(1[789]\\d{2}|20[012]\\d)\\s*[—–-]\\s*(?:founded|established|organized|incorporated)"
  )

  for (p in patterns) {
    m <- str_match(str_to_lower(text), p)
    if (!is.na(m[1, 1])) {
      yr <- as.integer(m[1, 2])
      if (!is.na(yr) && yr >= 1780 && yr <= 2024) return(yr)
    }
  }
  NA_integer_
}

USER_AGENTS <- c(
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36",
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:125.0) Gecko/20100101 Firefox/125.0",
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 14_4) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.4 Safari/605.1.15"
)

# Returns an archived URL from Wayback Machine, or NA if not found
wayback_url <- function(original_url) {
  api <- paste0("https://archive.org/wayback/available?url=", URLencode(original_url, reserved = TRUE))
  resp <- tryCatch(
    GET(api, timeout(10), user_agent(USER_AGENTS[1])),
    error = function(e) NULL
  )
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

  # try live site with rotating user agents
  for (ua in USER_AGENTS) {
    text <- tryCatch(scrape_text_from_url(base_url, ua), error = function(e) NA_character_)
    if (!is.na(text) && nchar(text) > 200) return(text)
  }

  # fallback: Wayback Machine (bypasses Cloudflare)
  archived <- wayback_url(base_url)
  if (!is.na(archived)) {
    text <- tryCatch(scrape_text_from_url(archived, USER_AGENTS[1]), error = function(e) NA_character_)
    if (!is.na(text) && nchar(text) > 200) return(text)
  }

  NA_character_
}

# ---------------------------------------------------------------------------
# 3. Per-row processing function (runs inside each parallel worker)
# ---------------------------------------------------------------------------

process_church <- function(psuedo_id, clean_url, name, chat_fn) {
  Sys.sleep(RATE_LIMIT_S + runif(1, 0, 0.5))

  text <- fetch_page_text(clean_url)

  # fast path: regex
  yr_regex <- extract_year_regex(text)
  if (!is.na(yr_regex)) {
    return(tibble(
      psuedo_id      = psuedo_id,
      founding_year  = yr_regex,
      source         = "regex",
      raw_text_snippet = str_sub(text, 1, 200)
    ))
  }

  # slow path: LLM (only if we got real page text)
  if (!is.na(text) && nchar(text) > 100) {
    prompt <- paste0(
      "Extract the year this Catholic parish was canonically founded or established as a parish. ",
      "This means the year the parish was officially erected or first organized as a congregation — NOT the year a building was constructed, renovated, or dedicated, and NOT a school founding year. ",
      "If the text mentions both a parish founding year and a building/renovation year, return only the parish founding year. ",
      "Return ONLY a 4-digit integer year (e.g. 1923), or null if not clearly stated. No explanation.\n\nText:\n",
      str_sub(text, 1, 8000)
    )
    raw <- tryCatch(chat_fn(prompt), error = function(e) NA_character_)
    yr  <- suppressWarnings(as.integer(str_extract(raw, "1[789]\\d{2}|20[012]\\d")))
    if (!is.na(yr) && yr >= 1780 && yr <= 2024) {
      return(tibble(
        psuedo_id      = psuedo_id,
        founding_year  = yr,
        source         = "llm",
        raw_text_snippet = str_sub(text, 1, 200)
      ))
    }
  }

  tibble(
    psuedo_id        = psuedo_id,
    founding_year    = NA_integer_,
    source           = if_else(is.na(text), "no_page", "not_found"),
    raw_text_snippet = str_sub(coalesce(text, ""), 1, 200),
    full_text_       = if_else(is.na(text), NA_character_, text)  # stash for batch write
  )
}

# ---------------------------------------------------------------------------
# 4. Parallel execution with checkpointing
# ---------------------------------------------------------------------------

plan(multisession, workers = WORKERS)

# split into batches so we checkpoint frequently
BATCH_SIZE <- 100
batches    <- split(seq_len(nrow(church_data)), ceiling(seq_len(nrow(church_data)) / BATCH_SIZE))

message("Starting scrape: ", length(batches), " batches of ", BATCH_SIZE)

for (b in seq_along(batches)) {
  idx   <- batches[[b]]
  batch <- church_data[idx, ]

  results <- future_pmap(
    list(
      psuedo_id = batch$psuedo_id,
      clean_url = batch$clean_url,
      name      = batch$name
    ),
    function(psuedo_id, clean_url, name) {
      # each worker creates its own chat session
      chat <- ellmer::chat_google_gemini(model = "gemini-2.0-flash-lite")
      chat_fn <- function(prompt) chat$chat(prompt)
      process_church(psuedo_id, clean_url, name, chat_fn)
    },
    .options = furrr_options(seed = TRUE)
  ) %>% bind_rows()

  # append full text for not_found rows to separate file
  fulltext_rows <- results %>%
    filter(source == "not_found", !is.na(full_text_)) %>%
    transmute(psuedo_id, full_text = full_text_)
  if (nrow(fulltext_rows) > 0) {
    write_csv(fulltext_rows, FULLTEXT_FILE, append = TRUE)
  }

  # append to checkpoint (drop the stashed full_text_ column)
  write_csv(results %>% select(-any_of("full_text_")), CHECKPOINT_FILE, append = TRUE)

  found <- sum(!is.na(results$founding_year))
  message(sprintf("Batch %d/%d done — %d/%d found this batch", b, length(batches), found, nrow(batch)))
}

plan(sequential)

# ---------------------------------------------------------------------------
# 5. Summarise final coverage
# ---------------------------------------------------------------------------

final <- read_csv(CHECKPOINT_FILE, show_col_types = FALSE)
message("\n--- Final coverage ---")
message("Total processed: ", nrow(final))
message("Founding year found: ", sum(!is.na(final$founding_year)),
        " (", round(mean(!is.na(final$founding_year)) * 100, 1), "%)")
message("Source breakdown:")
print(table(final$source))
