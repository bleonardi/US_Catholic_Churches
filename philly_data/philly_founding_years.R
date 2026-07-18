library(tidyverse)
library(ellmer)

# Paths
project_dir <- "/Users/benedictleonardi/Library/CloudStorage/GoogleDrive-benedict.r.leonardi@gmail.com/My Drive/Personal/Random Code/Github_Projects/US_Catholic_Churches"
input_path  <- file.path(project_dir, "philly_data/philly_parish_list.csv")
output_path <- file.path(project_dir, "philly_data/philly_founding_years.csv")

# API key
GOOGLE_KEY <- Sys.getenv("GOOGLE_API_KEY")
if (nchar(GOOGLE_KEY) == 0) stop("GOOGLE_API_KEY not set in environment")

# Load parishes
parishes <- read_csv(input_path, show_col_types = FALSE)

# Resume from checkpoint if it exists
if (file.exists(output_path)) {
  done <- read_csv(output_path, show_col_types = FALSE)
  processed_ids <- done$fs_number
  cat(sprintf("Checkpoint found: %d already processed. Resuming...\n", nrow(done)))
} else {
  done <- tibble(
    fs_number    = integer(),
    parish_name  = character(),
    location     = character(),
    founding_year = integer(),
    raw_response = character()
  )
  processed_ids <- integer(0)
}

# Filter to unprocessed
remaining <- parishes %>% filter(!fs_number %in% processed_ids)
cat(sprintf("%d parishes remaining to process.\n", nrow(remaining)))

# Accumulator for this session
new_results <- vector("list", nrow(remaining))

for (i in seq_len(nrow(remaining))) {
  row <- remaining[i, ]
  prompt <- sprintf(
    "What year was %s Catholic parish in %s, Pennsylvania (Archdiocese of Philadelphia) founded or established? Give only the 4-digit year, nothing else. If unknown, say unknown.",
    row$parish_name, row$location
  )

  chat <- ellmer::chat_google_gemini(
    model   = "gemini-2.5-flash",
    api_key = GOOGLE_KEY
  )

  raw <- tryCatch(chat$chat(prompt), error = function(e) NA_character_)

  # Extract 4-digit year
  year_match <- regmatches(raw, regexpr("1[6789]\\d{2}|20[012]\\d", raw))
  founding_year <- if (length(year_match) == 1) as.integer(year_match) else NA_integer_

  new_results[[i]] <- tibble(
    fs_number     = row$fs_number,
    parish_name   = row$parish_name,
    location      = row$location,
    founding_year = founding_year,
    raw_response  = as.character(raw)
  )

  cat(sprintf("[%d/%d] %s (%s) -> %s\n",
              i, nrow(remaining),
              row$parish_name, row$location,
              ifelse(is.na(founding_year), "NA", as.character(founding_year))))

  # Incremental save every 10 parishes
  if (i %% 10 == 0 || i == nrow(remaining)) {
    batch <- bind_rows(new_results[1:i])
    all_results <- bind_rows(done, batch)
    write_csv(all_results, output_path)
    cat(sprintf("  -> Checkpoint saved (%d total rows)\n", nrow(all_results)))
  }

  Sys.sleep(0.3)
}

# Final summary
final <- read_csv(output_path, show_col_types = FALSE)
n_total <- nrow(final)
n_year  <- sum(!is.na(final$founding_year))
n_na    <- sum(is.na(final$founding_year))
cat(sprintf("\n=== DONE ===\n"))
cat(sprintf("Total parishes: %d\n", n_total))
cat(sprintf("Got year: %d\n", n_year))
cat(sprintf("NA: %d\n", n_na))
if (n_year > 0) {
  cat(sprintf("Year range: %d – %d\n",
              min(final$founding_year, na.rm = TRUE),
              max(final$founding_year, na.rm = TRUE)))
}
