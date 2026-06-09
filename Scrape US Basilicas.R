library(rvest)
library(dplyr)

url <- "https://en.wikipedia.org/wiki/List_of_Catholic_basilicas#Basilicas_in_North_and_Central_America_and_the_Caribbean"

# Read all tables on the page
basilica_df <- url %>%
  read_html() %>%
  html_table(fill = TRUE)

us_basilicas_df <- basilica_df %>%
  .[[3]] %>%
  filter(Country == "United States")

# Check how many tables are there
length(tables)
