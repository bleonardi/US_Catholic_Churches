#!/usr/bin/env Rscript

library(httr)
library(jsonlite)
library(dplyr)

# -------------------------------
# 1. Function to Fetch Data
# -------------------------------
get_masstimes <- function(lat, long, delay = c(1, 3)) {
  all_data <- list()
  pg <- 1
  
  repeat {
    url <- paste0("https://masstimes.org/Churchs/?lat=", lat, "&long=", long, "&pg=", pg)
    response <- GET(url, add_headers(
      `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36",
      `Accept` = "application/json, text/javascript, */*; q=0.01",
      `X-Requested-With` = "XMLHttpRequest",
      `Referer` = "https://masstimes.org/"
    ))
    
    # Check status
    if (status_code(response) != 200) {
      message("Failed on page ", pg, " with status ", status_code(response))
      break
    }
    
    raw_text <- content(response, "text", encoding = "UTF-8")
    
    # If response is empty or invalid, stop
    if (nchar(raw_text) == 0 || raw_text == "[]") {
      message("No more data after page ", pg - 1)
      break
    }
    
    # Convert to JSON
    data <- fromJSON(raw_text, flatten = TRUE)
    if (length(data) == 0) {
      message("No data returned on page ", pg)
      break
    }
    
    all_data[[pg]] <- data
    pg <- pg + 1
    
    Sys.sleep(runif(1, delay[1], delay[2]))
  }
  
  if (length(all_data) > 0) {
    return(dplyr::bind_rows(all_data))
  } else {
    return(NULL)
  }
}

# -------------------------------
# 2. Generate a Grid of Coordinates
# -------------------------------
lats <- seq(24.5, 49.5, by = 2)   # step ~138 miles north-south
lons <- seq(-125, -66, by = 2)    # step ~106 miles east-west (at 40° lat)

grid <- expand.grid(lat = lats, lon = lons)

# -------------------------------
# 3. Scrape Data Across Grid
# -------------------------------
national_data <- list()

for (i in seq_len(nrow(grid))) {
  lat <- grid$lat[i]
  lon <- grid$lon[i]
  message("Fetching data for point ", i, "/", nrow(grid), " (", lat, ", ", lon, ")")
  
  data <- get_masstimes(lat, lon, delay = c(1, 3))
  
  if (!is.null(data)) {
    national_data[[i]] <- data
  }
  
  Sys.sleep(runif(1, 2, 5))  # Additional delay between grid points
}

# -------------------------------
# 4. Combine & Deduplicate
# -------------------------------
final_df <- bind_rows(national_data) %>%
  mutate(psuedo_id = paste0(latitude,
                            "_", 
                            longitude,
                            "_", 
                            email))

# Deduplicate if there is a unique ChurchID field
churches_df <- final_df %>% distinct(psuedo_id, .keep_all = TRUE)

worship_times_df <- churches_df %>%
  mutate(church_worship_times = map2(
    church_worship_times,
    psuedo_id,
    ~ if (is.data.frame(.x)) {
      mutate(.x, psuedo_id = .y)
      } else {
        tibble(psuedo_id = .y) # fallback if not a df
        })) %>% # add id column inside each tibble
  pull(church_worship_times) %>%
  bind_rows()

# -------------------------------
# 5. Save Data
# -------------------------------
# writes the CSVs properly
# # splits out times df properly
write.csv(churches_df %>% select(-church_worship_times), "national_church_data.csv", row.names = FALSE)
write.csv(worship_times_df, "national_worship_times.csv", row.names = FALSE)
