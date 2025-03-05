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

ua_other_nations_collaboration_percent <- (nrow(works_published_multi_authors_nonus) / nrow(works_published)) * 100
print(ua_other_nations_collaboration_percent)

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

# Count the occurrences of each country code. 
# 2023: 144 countries
country_code_counts <- table(all_country_codes$country_code)

# Convert to a new df 
country_counts_full_name <- as.data.frame(country_code_counts)
names(country_counts_full_name) <- c("country_code", "count")

library(countrycode)
country_counts_full_name <- country_counts_full_name %>%
  mutate(
    country_name = countrycode(country_code, origin = "iso2c", destination = "country.name")
  ) %>%
  select(country_code, country_name, count) # Rearrange columns

# ranking by # of collaborated authors' 
country_ranking <- country_counts_full_name %>%
  mutate(
    country_name = countrycode(country_code, origin = "iso2c", destination = "country.name")
  ) %>%
  arrange(desc(count)) %>% # Sort by count in ascending order
  select(country_code, country_name, count) # Rearrange columns

# We know that the df at least containing one UA author. So just need to find out a specific country
# Example: Find publications with authors from Mexico

ua_mx <- works_published_multi_authors_nonus %>%
  filter(
    map_lgl(author, function(author_df) {
      "MX" %in% author_df$institution_country_code
    })
  )
unique_titles <- unique(ua_mx$title)
print(unique_titles)

ua_in <- works_published_multi_authors_nonus %>%
  filter(
    map_lgl(author, function(author_df) {
      "IN" %in% author_df$institution_country_code
    })
  )
unique_titles <- unique(ua_in$title)
print(unique_titles)




diff <- setdiff(ua_ca,ua_in)
diff <- setdiff(ua_in, ua_ca)

### NEED CHECK>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
###################################


### Step 5: Topics 
ua_country_collaboration <- ua_in

all_topics <- ua_country_collaboration %>%
  pull(topics) %>%
  map_dfr(function(topic_df) {
    if ("display_name" %in% names(topic_df)) {
      return(data.frame(topic = topic_df$display_name))
    } else {
      return(data.frame(topic = character(0))) # Handle missing column
    }
  })

topic_counts <- all_topics %>%
  group_by(topic) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

print(topic_counts)

### Step 6: Unique institutions at that country
nrow(ua_country_collaboration)
names(ua_country_collaboration$author[[1]])

ua_country_unnested <- ua_country_collaboration %>%
  unnest(cols = c(author))

# Get all the institutions from the specific country
ua_country_institutions <- ua_country_unnested %>%
  filter(institution_country_code == "IN") %>% # Filter the country using the country code
  select(id, title, au_id, au_display_name, institution_display_name, institution_country_code, topics) # Get the institutions

# Sort these institutions by number of times appearing 
ua_country_institutions_sorted <- ua_country_institutions %>%
  group_by(institution_display_name) %>% # Group by institution name
  summarise(count = n()) %>% # Count occurrences
  arrange(desc(count)) # Sort in descending order

# Within the specific institutions to find out the topics. 
# Get the top institution name (the one with the highest count)
top_institution <- ua_country_institutions_sorted$institution_display_name[1]
print(top_institution)

# Find the topics for the top institutions
top_institution_topics <- ua_country_institutions %>%
  filter(institution_display_name == top_institution) %>%
  pull(topics) %>%
  map_dfr(function(topic_df) {
    if ("display_name" %in% names(topic_df)) {
      return(data.frame(topic = topic_df$display_name))
    } else {
      return(data.frame(topic = character(0)))
    }
  }) %>%
  distinct(topic) %>%
  count()

print(top_institution_topics)

#To list the topics instead of the count
top_institution_topics_list <- ua_country_institutions %>%
  filter(institution_display_name == top_institution) %>%
  pull(topics) %>%
  map_dfr(function(topic_df) {
    if ("display_name" %in% names(topic_df)) {
      return(data.frame(topic = topic_df$display_name))
    } else {
      return(data.frame(topic = character(0)))
    }
  }) %>%
  distinct(topic)

print(top_institution_topics_list)


# output in multipel worksheets with Excel
# 1. Details . 2. sorting results. 3. Topics. 

