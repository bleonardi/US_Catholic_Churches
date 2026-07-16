library(tidyverse)
library(pdftools)
library(furrr)

PDF_DIR <- "aod_pdfs"
OUT_FILE <- "aod_geography.csv"

pdfs <- tibble(
  path        = list.files(PDF_DIR, pattern = "\\.pdf$", full.names = TRUE),
  parish_name = list.files(PDF_DIR, pattern = "\\.pdf$") |>
    str_replace_all("_", " ") |>
    str_replace("\\.pdf$", "") |>
    str_remove(",$")
)
message(nrow(pdfs), " PDFs found")

# Key name used to match parish row in the planning-area "Where Parishioners Live" table.
# Strip city suffix after the last comma.
short_name <- function(parish_name) {
  str_remove(parish_name, ",\\s*[^,]+$") |> str_trim()
}

extract_geography <- function(path, parish_name) {
  pages <- tryCatch(pdf_text(path), error = function(e) NULL)
  if (is.null(pages)) return(tibble(path = path, parish_name = parish_name,
                                    pct_within_parish = NA_real_,
                                    pct_outside_region = NA_real_))

  # The "Where Parishioners Live" table appears in the planning-area section
  # (pp 20-37).  Find ALL such pages (there may be more than one planning area).
  geo_pages <- which(str_detect(pages, "Where Parishioners Live"))

  sname <- short_name(parish_name)
  # Also try just the first 30 chars for long names
  sname_short <- str_sub(sname, 1, 30)

  pct_parish <- NA_real_
  pct_outside <- NA_real_

  for (pg in geo_pages) {
    lines <- str_split(pages[[pg]], "\n")[[1]]
    # Find line(s) containing this parish's short name
    match_idx <- which(str_detect(lines, fixed(sname, ignore_case = TRUE)) |
                       str_detect(lines, fixed(sname_short, ignore_case = TRUE)))
    if (length(match_idx) == 0) next

    # Some parish names wrap to 2 lines; concatenate with next line
    for (idx in match_idx) {
      combined <- paste(lines[idx],
                        if (idx < length(lines)) lines[idx + 1] else "",
                        collapse = " ")
      pcts <- str_extract_all(combined, "\\d+%")[[1]] |>
        str_remove("%") |> as.integer()
      if (length(pcts) >= 2) {
        pct_parish  <- pcts[1]    # first % = within parish boundary
        pct_outside <- pcts[length(pcts)]  # last % = outside region
        break
      }
      # If only 1 % on the row itself, check if the table skips within-parish
      # for national/ethnic parishes that don't have a defined territory.
      if (length(pcts) == 1) {
        pct_outside <- pcts[1]
      }
    }
    if (!is.na(pct_parish)) break
  }

  tibble(path = path, parish_name = parish_name,
         pct_within_parish  = pct_parish,
         pct_outside_region = pct_outside)
}

plan(multisession, workers = 6)
geo <- future_pmap_dfr(
  list(path = pdfs$path, parish_name = pdfs$parish_name),
  extract_geography,
  .options = furrr_options(seed = TRUE)
)
plan(sequential)

write_csv(geo, OUT_FILE)

cat(sprintf("\nExtracted geography for %d / %d parishes\n",
            sum(!is.na(geo$pct_within_parish)), nrow(geo)))
cat(sprintf("pct_within_parish range: %d%% – %d%%\n",
            min(geo$pct_within_parish, na.rm = TRUE),
            max(geo$pct_within_parish, na.rm = TRUE)))
print(head(geo, 15))
