#### Collaboration analysis

######## Author: Yan Han with help of Gemini / Cursor
######## Updated: March 1, 2025
##### Analyze an institution authors' and his/her co-authors nation and institutions 
# OpenAlex R Documentation: https://github.com/ropensci/openalexR

install.packages("dplyr")
install.packages("tidyverse")
install.packages("countrycode")

library(openalexR)
packageVersion("openalexR")

library(jsonlite)
library(dplyr)
library(tidyverse)

# free unused obj to manage memory
gc()
rm(list=ls())
gc()

options("max.print" = 100000)
options (openalexR.mailto="yhan@arizona.edu")
getwd()
setwd("/home/yhan/Documents/openalexR-test/")

works_published_2023 <- readRDS("../works_published_2023.rds")

works_published <- works_published_2023

##### Steps:
### 1. Get the authors of the works
works_published_authors <- works_published %>% select(id, title, author, topics)

### Step 2: Identify multi-author papers
works_published_multi_authors <- works_published_authors %>%
  mutate(
    author_count = map_int(author, nrow), # Get the number of rows in each nested df
    has_multiple_authors = author_count > 1 # Check if there are multiple rows
  )

### Step 3: Split it into two works df: US and nonUS authors
# works_published_multi_authors has a new column:
# - nonus_author: TRUE if at least one author is from a non-US country, FALSE otherwise.

works_published_multi_authors_nonus <- works_published_multi_authors %>%
  mutate(
    nonus_author = map_lgl(author, function(author_df) {
      if (nrow(author_df) == 0) {
        return(FALSE) # No authors, so no non-US authors
      } else if (!"institution_country_code" %in% names(author_df)) {
        warning("Nested author data frame missing 'institution_country_code' column.")
        return(FALSE) # Missing column, assume no non-US authors
      } else {
        any(author_df$institution_country_code != "US" & !is.na(author_df$institution_country_code))
      }
    })
  ) %>% 
  filter(nonus_author)

# US authors only. For future use. 
works_published_multi_authors_us <- works_published_multi_authors %>%
  mutate(
    nonus_author = map_lgl(author, function(author_df) {
      if (nrow(author_df) == 0) {
        return(FALSE) # No authors, so no non-US authors
      } else if (!"institution_country_code" %in% names(author_df)) {
        warning("Nested author data frame missing 'institution_country_code' column.")
        return(FALSE) # Missing column, assume no non-US authors
      } else {
        any(author_df$institution_country_code != "US" & !is.na(author_df$institution_country_code))
      }
    })
  ) %>% 
  filter(!nonus_author)

### Step 4: Use NonUS author works to figure out collaboration.
head(works_published_multi_authors_nonus)

# Example: Get all country codes from all nested author data frames
all_country_codes <- works_published_multi_authors_nonus %>%
  pull(author) %>%
  map_dfr(function(author_df) {
    if ("institution_country_code" %in% names(author_df)) {
      return(data.frame(country_code = author_df$institution_country_code))
    } else {
      return(data.frame(country_code = character(0))) # Handle missing column
    }
  })

# Count the occurrences of each country code
country_code_counts <- table(all_country_codes$country_code)

# Convert to a new df 
country_ranking <- as.data.frame(country_code_counts)
names(country_ranking) <- c("country_code", "count")

library(countrycode)
country_ranking <- country_ranking %>%
  mutate(
    country_name = countrycode(country_code, origin = "iso2c", destination = "country.name")
  ) %>%
  select(country_code, country_name, count) # Rearrange columns

# We know that the df at least containing one UA author. So just need to find out a specific country
# Example: Find publications with authors from Canada
ua_ca <- works_published_multi_authors_nonus %>%
  filter(
    map_lgl(author, function(author_df) {
      "US" %in% author_df$institution_country_code & "CA" %in% author_df$institution_country_code
    })
  )

ua_in <- works_published_multi_authors_nonus %>%
  filter(
    map_lgl(author, function(author_df) {
      "CA" %in% author_df$institution_country_code
    })
  )

diff <- setdiff(ua_ca,ua_in)
diff <- setdiff(ua_in, ua_ca)

### NEED CHECK>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
###################################


### Step 5: Topics 
all_topics <- works_published_multi_authors_nonus %>%
  pull(topics) %>%
  map_dfr(function(topic_df) {
    if ("display_name" %in% names(topic_df)){
      return(data.frame(topic = topic_df$display_name))
    } else {
      return(data.frame(topic = character(0)))
    }
    
  })

topic_counts <- table(all_topics$topic)
print(topic_counts)

