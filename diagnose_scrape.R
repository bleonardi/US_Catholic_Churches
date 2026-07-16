library(tidyverse)
library(rvest)
library(httr)
library(stringr)

# Check a handful of no_page churches manually to diagnose why they fail
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

user_agents <- c(
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36",
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:125.0) Gecko/20100101 Firefox/125.0",
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 14_4) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.4 Safari/605.1.15"
)

diagnose_url <- function(url, name) {
  for (ua in user_agents) {
    resp <- tryCatch(
      GET(url, user_agent(ua), timeout(10), config(followlocation = TRUE)),
      error = function(e) NULL
    )
    if (!is.null(resp)) {
      sc <- status_code(resp)
      ct <- headers(resp)$`content-type` %||% ""
      text_len <- nchar(content(resp, "text", encoding = "UTF-8", warn = FALSE))
      cat(sprintf("%-45s | %s | %d | len=%d | ua=%s\n",
                  str_sub(name, 1, 45), str_sub(url, 1, 40), sc, text_len,
                  str_sub(ua, 13, 30)))
      if (sc == 200) break
    }
  }
}

cat("name | url | status | text_len | user_agent\n")
cat(strrep("-", 100), "\n")
walk2(test_sample$clean_url, test_sample$name, diagnose_url)
