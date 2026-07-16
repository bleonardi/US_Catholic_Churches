library(tidyverse)
library(ellmer)
library(furrr)


CHECKPOINT_FILE <- "founding_years_checkpoint.csv"
FULLTEXT_FILE   <- "scrape_fulltext.csv"
WORKERS         <- 8

# ── 1. Identify targets ───────────────────────────────────────────────────────

checkpoint    <- read_csv(CHECKPOINT_FILE, show_col_types = FALSE)
already_found <- checkpoint |> filter(!is.na(founding_year)) |> pull(psuedo_id)

fulltext <- read_csv(FULLTEXT_FILE, show_col_types = FALSE) |>
  filter(!is.na(full_text), nchar(full_text) > 200) |>
  filter(!psuedo_id %in% already_found)

# backfill text was appended directly to scrape_fulltext.csv — nothing extra to load

message(nrow(fulltext), " pages to send to LLM")
if (nrow(fulltext) == 0) { message("Nothing to do."); quit(save = "no") }

# ── 2. Extraction function ────────────────────────────────────────────────────

extract_year <- function(psuedo_id, full_text) {
  prompt <- paste0(
    "Extract the year this Catholic parish was canonically founded or established as a parish. ",
    "This means the year the parish was officially erected or first organized as a congregation ",
    "— NOT the year a building was constructed, renovated, or dedicated, and NOT a school founding year. ",
    "If the text mentions both a parish founding year and a building/renovation year, return only the parish founding year. ",
    "Return ONLY a 4-digit integer year (e.g. 1923), or null if not clearly stated. No explanation.\n\nText:\n",
    str_sub(full_text, 1, 8000)
  )

  raw <- tryCatch({
    chat <- chat_ollama(model = "gemma3:12b", echo = FALSE)
    chat$chat(prompt)
  }, error = function(e) NA_character_)

  yr <- suppressWarnings(as.integer(str_extract(as.character(raw), "1[789]\\d{2}|20[012]\\d")))
  yr <- if (!is.na(yr) && yr >= 1780 && yr <= 2024) yr else NA_integer_

  tibble(
    psuedo_id        = psuedo_id,
    founding_year    = yr,
    source           = if_else(!is.na(yr), "llm_batch", "not_found_llm"),
    raw_text_snippet = str_sub(coalesce(as.character(raw), ""), 1, 200)
  )
}

# ── 3. Parallel extraction with checkpointing ─────────────────────────────────

plan(multisession, workers = WORKERS)

BATCH_SIZE <- 200
batches    <- split(seq_len(nrow(fulltext)), ceiling(seq_len(nrow(fulltext)) / BATCH_SIZE))

message("Starting LLM extraction: ", length(batches), " batches of up to ", BATCH_SIZE)

for (b in seq_along(batches)) {
  idx   <- batches[[b]]
  batch <- fulltext[idx, ]

  results <- future_pmap_dfr(
    list(psuedo_id = batch$psuedo_id, full_text = batch$full_text),
    extract_year,
    .options = furrr_options(seed = TRUE)
  )

  # Replace stale not_found rows for these ids, then append new results
  checkpoint <- read_csv(CHECKPOINT_FILE, show_col_types = FALSE) |>
    filter(!psuedo_id %in% results$psuedo_id |
           (!is.na(founding_year) & source != "not_found"))

  write_csv(bind_rows(checkpoint, results), CHECKPOINT_FILE)

  n_found <- sum(!is.na(results$founding_year))
  message(sprintf("Batch %d/%d — %d/%d found", b, length(batches), n_found, nrow(batch)))
}

plan(sequential)

# ── 4. Summary ────────────────────────────────────────────────────────────────

final <- read_csv(CHECKPOINT_FILE, show_col_types = FALSE)
message(sprintf("\nDone. Founding years: %d / %d total (%.1f%%)",
                sum(!is.na(final$founding_year)), nrow(final),
                100 * mean(!is.na(final$founding_year))))
cat("\nSource breakdown:\n")
print(table(final$source))
message("\nRun lag_analysis.R to re-run analysis with updated coverage.")
