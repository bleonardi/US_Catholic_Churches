library(tidyverse)
library(rvest)
library(httr)
library(furrr)
library(ellmer)
library(stringr)

RATE_LIMIT_S <- 1.5

extract_year_regex <- function(text) {
  if (is.na(text) || nchar(text) < 10) return(NA_integer_)
  patterns <- c(
    "(?:founded|established|organized|incorporated|dedicated|built|erected|opened|began|started)\\s+(?:in\\s+)?(1[789]\\d{2}|20[012]\\d)",
    "(?:since|as early as)\\s+(1[789]\\d{2}|20[012]\\d)",
    "in\\s+the\\s+year\\s+(1[789]\\d{2}|20[012]\\d)",
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

fetch_page_text <- function(base_url) {
  if (is.na(base_url) || base_url == "") return(NA_character_)
  tryCatch({
    sess <- session(
      base_url,
      user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) Chrome/124.0.0.0 Safari/537.36"),
      timeout(12)
    )
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
  }, error = function(e) NA_character_)
}

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

set.seed(99)
test_sample <- church_data %>% sample_n(20)

plan(multisession, workers = 4)

results <- future_pmap(
  list(
    psuedo_id = test_sample$psuedo_id,
    clean_url = test_sample$clean_url,
    name      = test_sample$name
  ),
  function(psuedo_id, clean_url, name) {
    library(tidyverse)
    library(rvest)
    library(httr)
    library(ellmer)
    library(stringr)

    Sys.sleep(1.5 + runif(1, 0, 0.5))

    extract_year_regex <- function(text) {
      if (is.na(text) || nchar(text) < 10) return(NA_integer_)
      patterns <- c(
        "(?:founded|established|organized|incorporated|dedicated|built|erected|opened|began|started)\\s+(?:in\\s+)?(1[789]\\d{2}|20[012]\\d)",
        "(?:since|as early as)\\s+(1[789]\\d{2}|20[012]\\d)",
        "in\\s+the\\s+year\\s+(1[789]\\d{2}|20[012]\\d)",
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

    user_agents <- c(
      "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36",
      "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:125.0) Gecko/20100101 Firefox/125.0",
      "Mozilla/5.0 (Macintosh; Intel Mac OS X 14_4) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.4 Safari/605.1.15"
    )

    wayback_url <- function(original_url) {
      api  <- paste0("https://archive.org/wayback/available?url=", URLencode(original_url, reserved = TRUE))
      resp <- tryCatch(GET(api, timeout(10), user_agent(user_agents[1])), error = function(e) NULL)
      if (is.null(resp) || status_code(resp) != 200) return(NA_character_)
      parsed <- tryCatch(content(resp, "parsed", simplifyVector = TRUE), error = function(e) NULL)
      url <- parsed$archived_snapshots$closest$url
      if (is.null(url) || length(url) == 0 || is.na(url)) return(NA_character_)
      url
    }

    scrape_text_from_url <- function(target_url, ua) {
      sess <- session(target_url, user_agent(ua), timeout(15))
      # reject non-200 responses (e.g. Cloudflare 403 block pages)
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
      for (ua in user_agents) {
        text <- tryCatch(scrape_text_from_url(base_url, ua), error = function(e) NA_character_)
        if (!is.na(text) && nchar(text) > 200) return(text)
      }
      archived <- wayback_url(base_url)
      if (!is.na(archived)) {
        text <- tryCatch(scrape_text_from_url(archived, user_agents[1]), error = function(e) NA_character_)
        if (!is.na(text) && nchar(text) > 200) return(text)
      }
      NA_character_
    }

    text     <- fetch_page_text(clean_url)
    yr_regex <- extract_year_regex(text)

    if (!is.na(yr_regex)) {
      return(tibble(psuedo_id = psuedo_id, name = name, founding_year = yr_regex, source = "regex"))
    }

    if (!is.na(text) && nchar(text) > 100) {
      chat   <- ellmer::chat_google_gemini(model = "gemini-2.0-flash-lite")
      prompt <- paste0(
        "Extract the founding/establishment year of the Catholic parish from the text below. ",
        "Return ONLY a 4-digit integer year (e.g. 1923), or null if not found. No explanation.\n\nText:\n",
        str_sub(text, 1, 8000)
      )
      raw <- tryCatch(chat$chat(prompt), error = function(e) NA_character_)
      yr  <- suppressWarnings(as.integer(str_extract(raw, "1[789]\\d{2}|20[012]\\d")))
      if (!is.na(yr) && yr >= 1780 && yr <= 2024) {
        return(tibble(psuedo_id = psuedo_id, name = name, founding_year = yr, source = "llm"))
      }
    }

    tibble(
      psuedo_id    = psuedo_id,
      name         = name,
      founding_year = NA_integer_,
      source       = if_else(is.na(text), "no_page", "not_found")
    )
  },
  .options = furrr_options(seed = TRUE)
) %>% bind_rows()

plan(sequential)

print(results %>% select(name, founding_year, source), n = 20)
cat("\nFound:", sum(!is.na(results$founding_year)), "/", nrow(results), "\n")
cat("Sources:\n"); print(table(results$source))
