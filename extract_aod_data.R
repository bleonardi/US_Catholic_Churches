library(tidyverse)
library(pdftools)
library(ellmer)
library(furrr)
library(jsonlite)

PDF_DIR      <- "aod_pdfs"
OUT_FILE     <- "aod_parish_data.csv"
CHECKPOINT   <- "aod_extract_checkpoint.csv"
WORKERS      <- 6

# ── 1. List all PDFs ──────────────────────────────────────────────────────────

pdfs <- tibble(
  path         = list.files(PDF_DIR, pattern = "\\.pdf$", full.names = TRUE),
  parish_name  = list.files(PDF_DIR, pattern = "\\.pdf$") |>
    str_replace_all("_", " ") |>
    str_replace("\\.pdf$", "") |>
    str_remove(",$")
)
message(nrow(pdfs), " PDFs found")

# ── 2. Load checkpoint ────────────────────────────────────────────────────────

if (file.exists(CHECKPOINT)) {
  ckpt   <- read_csv(CHECKPOINT, show_col_types = FALSE)
  done   <- ckpt$path
} else {
  ckpt   <- tibble()
  done   <- character(0)
}

pdfs_todo <- pdfs |> filter(!path %in% done)
message(nrow(pdfs_todo), " PDFs to extract")

# ── 3. Extraction function ────────────────────────────────────────────────────
# Each 56-page workbook has the same structure:
#   pp 1-19:  archdiocese boilerplate
#   pp 20-37: planning area tables (revenue, expense summaries for all parishes)
#   p  38:    Section 3 title slide
#   p  39:    Parish History  <- founding year
#   p  40:    US Census Data  (skip — we have ACS directly)
#   p  41:    Worshipping Households <- mass attendance %, seating cap, trend
#   p  42:    Where Parishioners Live (skip for now)
#   p  43:    Sacraments of Life Trend <- baptism/funeral % change
#   p  44:    Youth Evangelization <- confirmations % change, relig. ed. enrollment
#   p  45:    OCIA (skip)
#   p  46:    Financial field definitions (skip)
#   p  47:    Revenue 3 Year Trend
#   p  48:    Operating Expenses 3 Year Trend
#   p  49:    Parish Deferred Maintenance <- total $ deferred maintenance
#   p  50:    Capital Expenses (skip — included in expenses above)
#   p  51:    Revenue and Expenses summary <- net income FY22-25
#   p  52:    Catholic Education Investment (skip)
#   p  53:    Savings & Investments
#   pp 54-56: additional or overflow pages

extract_parish <- function(path, parish_name) {

  # Read all pages as text
  pages <- tryCatch(
    pdf_text(path),
    error = function(e) NULL
  )
  if (is.null(pages) || length(pages) < 40) {
    return(tibble(path = path, parish_name = parish_name, status = "pdf_error"))
  }

  # Anchor on content markers (Section Three title is an image, not text)
  p_history    <- which(str_detect(pages, "Parish History"))[1]
  p_households <- which(str_detect(pages, "Worshipping Households"))[1]
  p_sacraments <- which(str_detect(pages, "Sacraments of Life Trend"))[1]
  p_youth      <- which(str_detect(pages, "Youth Evangelization"))[1]
  p_deferred   <- which(str_detect(pages, "Parish Deferred Maintenance"))[1]
  p_revexp     <- which(str_detect(pages, "Revenue and Expenses") &
                        str_detect(pages, "Net Income"))[1]
  p_savings    <- which(str_detect(pages, "Savings.*Investments|Cash and Savings"))[1]

  # Fall back to fixed offsets from last planning-area page if anchors fail
  get_page <- function(idx, default_offset) {
    if (!is.na(idx)) pages[[idx]] else {
      last_pa <- max(which(str_detect(pages[1:40], "Planning Area|Section Two")), na.rm = TRUE)
      if (is.finite(last_pa)) pages[[min(last_pa + default_offset, length(pages))]] else ""
    }
  }

  ph  <- get_page(p_history,    19)
  phh <- get_page(p_households, 21)
  ps  <- get_page(p_sacraments, 23)
  py  <- get_page(p_youth,      24)
  pd  <- get_page(p_deferred,   29)
  pre <- get_page(p_revexp,     31)
  psv <- get_page(p_savings,    33)

  # ── Founding year ──────────────────────────────────────────────────────────
  # "Parish was founded in 1991" / "established in" / "erected in"
  founding_year <- NA_integer_
  yr_match <- str_extract(ph,
    "(?:founded|established|erected|organized|created)\\s+(?:as a parish\\s+)?in\\s+(1[789]\\d{2}|20[012]\\d)")
  if (!is.na(yr_match)) {
    founding_year <- as.integer(str_extract(yr_match, "\\d{4}"))
  } else {
    yr_all <- str_extract_all(ph, "1[789]\\d{2}|20[012]\\d")[[1]]
    if (length(yr_all) > 0) founding_year <- as.integer(yr_all[1])
  }

  # ── Mass attendance % capacity ────────────────────────────────────────────
  # Text layout on p41: sidebar has "39%" as a standalone number before year labels
  # Pattern: the % near end of page after "capacity"
  mass_pct_capacity <- NA_real_
  # The sidebar block: "capacity\n   39%\n  2016"
  m <- str_extract(phh, "capacity\\s*\n\\s*(\\d{1,3})%")
  if (!is.na(m)) {
    mass_pct_capacity <- as.numeric(str_extract(m, "\\d+")) / 100
  } else {
    # Fallback: last standalone % on the page before year labels
    all_pct <- str_extract_all(phh, "(?m)^\\s*(\\d{1,3})%\\s*$")[[1]]
    if (length(all_pct) > 0)
      mass_pct_capacity <- as.numeric(str_extract(tail(all_pct, 1), "\\d+")) / 100
  }

  # Seating capacity: "Seating capacity\n   975"
  seating_cap <- NA_integer_
  m_seat <- str_extract(phh, "Seating capacity\\s*\n\\s*(\\d{1,4})")
  if (!is.na(m_seat)) seating_cap <- as.integer(str_extract(m_seat, "\\d+$"))

  # Number of weekend masses
  n_masses <- NA_integer_
  m_nm <- str_extract(phh, "weekend masses\\s*\n\\s*(\\d{1,2})")
  if (!is.na(m_nm)) n_masses <- as.integer(str_extract(m_nm, "\\d+$"))

  # ── Sacraments % change 2015-2024 ─────────────────────────────────────────
  # Text layout: "Funerals\n -8.2%"  "Infant & Minor\n    Baptisms\n -40.4%"
  # Extract all standalone signed-% values: they appear in order Funerals, Baptisms, Marriages
  sac_pcts <- str_extract_all(ps, "(?m)^\\s*(-?\\d+\\.\\d)%\\s*$")[[1]] |>
    str_trim() |>
    str_remove("%") |>
    as.numeric()

  funerals_pct_chg  <- if (length(sac_pcts) >= 1) sac_pcts[1] / 100 else NA_real_
  baptisms_pct_chg  <- if (length(sac_pcts) >= 2) sac_pcts[2] / 100 else NA_real_
  marriages_pct_chg <- if (length(sac_pcts) >= 3) sac_pcts[3] / 100 else NA_real_

  # ── Youth % change 2015-2024 ──────────────────────────────────────────────
  youth_pcts <- str_extract_all(py, "(?m)^\\s*(-?\\d+\\.\\d)%\\s*$")[[1]] |>
    str_trim() |>
    str_remove("%") |>
    as.numeric()

  confirmations_pct_chg   <- if (length(youth_pcts) >= 1) youth_pcts[1] / 100 else NA_real_
  first_communion_pct_chg <- if (length(youth_pcts) >= 2) youth_pcts[2] / 100 else NA_real_
  relig_ed_pct_chg        <- if (length(youth_pcts) >= 3) youth_pcts[3] / 100 else NA_real_

  # ── Deferred maintenance ──────────────────────────────────────────────────
  # Text: " $2,305,800\nTotal Estimated Cost"
  deferred_maint <- NA_real_
  dm <- str_extract(pd, "\\$([0-9,]+)\\s*\nTotal Estimated Cost")
  if (!is.na(dm)) {
    deferred_maint <- as.numeric(str_remove_all(str_extract(dm, "[0-9,]+"), ","))
  } else {
    # Fallback: first large $ amount on the page
    dm2 <- str_extract(pd, "\\$([1-9][0-9]{2,},[0-9]{3})")
    if (!is.na(dm2)) deferred_maint <- as.numeric(str_remove_all(str_extract(dm2, "[0-9,]+"), ","))
  }

  # ── Net income FY22/23, FY23/24, FY24/25 ─────────────────────────────────
  # Text: "$101,720  $329,583  $169,528" (green) or "($122,625)" for deficits
  parse_dollar <- function(x) {
    x <- str_trim(x)
    negative <- str_detect(x, "^\\(")
    x <- str_remove_all(x, "[\\$,()\\s]")
    v <- suppressWarnings(as.numeric(x))
    if (!is.na(v) && negative) -v else v
  }

  net_matches <- str_extract_all(pre, "\\(?\\$[0-9,]+\\)?")[[1]]
  net_fy2223 <- if (length(net_matches) >= 1) parse_dollar(net_matches[1]) else NA_real_
  net_fy2324 <- if (length(net_matches) >= 2) parse_dollar(net_matches[2]) else NA_real_
  net_fy2425 <- if (length(net_matches) >= 3) parse_dollar(net_matches[3]) else NA_real_

  # Total revenue FY24/25: largest dollar value on the revenue page (p47)
  p_rev <- which(str_detect(pages, "Revenue.*3 Year Trend|Revenue.*Year Trend"))[1]
  rev_fy2425 <- NA_real_
  if (!is.na(p_rev)) {
    rev_vals <- str_extract_all(pages[[p_rev]], "\\$([0-9,]{5,})")[[1]] |>
      str_remove_all("[$,]") |> as.numeric() |> na.omit()
    if (length(rev_vals) > 0) rev_fy2425 <- max(rev_vals)
  }

  # ── Cash/savings unrestricted ─────────────────────────────────────────────
  cash_savings <- NA_real_
  cs_vals <- str_extract_all(psv, "\\$([0-9,]{4,})")[[1]] |>
    str_remove_all("[$,]") |> as.numeric() |> na.omit()
  if (length(cs_vals) > 0) cash_savings <- max(cs_vals)

  tibble(
    path                    = path,
    parish_name             = parish_name,
    status                  = "ok",
    founding_year           = founding_year,
    mass_pct_capacity       = mass_pct_capacity,
    seating_capacity        = seating_cap,
    n_weekend_masses        = n_masses,
    funerals_pct_chg_15_24  = funerals_pct_chg,
    baptisms_pct_chg_15_24  = baptisms_pct_chg,
    marriages_pct_chg_15_24 = marriages_pct_chg,
    confirmations_pct_chg   = confirmations_pct_chg,
    first_communion_pct_chg = first_communion_pct_chg,
    relig_ed_pct_chg        = relig_ed_pct_chg,
    deferred_maint_usd      = deferred_maint,
    net_income_fy2223       = net_fy2223,
    net_income_fy2324       = net_fy2324,
    net_income_fy2425       = net_fy2425,
    rev_fy2425              = rev_fy2425,
    cash_savings_usd        = cash_savings
  )
}

# ── 4. Run extraction in parallel ─────────────────────────────────────────────

if (nrow(pdfs_todo) == 0) {
  message("Nothing new to extract.")
} else {
  plan(multisession, workers = WORKERS)

  results <- future_pmap_dfr(
    list(path = pdfs_todo$path, parish_name = pdfs_todo$parish_name),
    extract_parish,
    .options = furrr_options(seed = TRUE)
  )

  plan(sequential)

  # Append to checkpoint
  all_results <- bind_rows(ckpt, results)
  write_csv(all_results, CHECKPOINT)
  message(sprintf("Extracted %d parishes; %d ok, %d errors",
                  nrow(results),
                  sum(results$status == "ok"),
                  sum(results$status != "ok")))
}

# ── 5. Final output ───────────────────────────────────────────────────────────

final <- read_csv(CHECKPOINT, show_col_types = FALSE) |>
  filter(status == "ok") |>
  mutate(
    city  = str_extract(parish_name, "[^,]+$") |> str_trim(),
    short_name = str_remove(parish_name, ",.*$")
  )

write_csv(final, OUT_FILE)
message(sprintf("\nFinal dataset: %d parishes -> %s", nrow(final), OUT_FILE))

cat("\nFounding year extraction rate:\n")
cat(sprintf("  %d / %d (%.1f%%)\n",
    sum(!is.na(final$founding_year)),
    nrow(final),
    100 * mean(!is.na(final$founding_year))))

cat("\nDeferred maintenance coverage:\n")
cat(sprintf("  %d / %d (%.1f%%)\n",
    sum(!is.na(final$deferred_maint_usd)),
    nrow(final),
    100 * mean(!is.na(final$deferred_maint_usd))))

cat("\nSample founding years:\n")
final |>
  filter(!is.na(founding_year)) |>
  select(parish_name, founding_year, mass_pct_capacity, deferred_maint_usd) |>
  slice_head(n = 10) |>
  print()
