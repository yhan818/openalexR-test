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

works_published_2022 <- readRDS("../works_published_2022.rds")

works_published <- works_published_2022

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

### Count the occurrences of each country code. 
# 2023: 144 countries
# 2022: 139 countries
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

### Change country code here: "IN", "MX", 
ua_country <- works_published_multi_authors_nonus %>%
  filter(
    map_lgl(author, function(author_df) {
      "IN" %in% author_df$institution_country_code
    })
  )
unique_titles <- unique(ua_country$title)
print(unique_titles)

head(ua_country)

### From here, we can find out the author relationship between UA authors and other country authors. 
# extract U of Arizona au_affiliation_raw from each work: So that we know which dept/author with that country dept/author

arizona_affiliations <- ua_country %>%
  mutate(
    arizona_authors = map(author, function(author_df) {
      author_df %>%
        filter(institution_display_name == "University of Arizona") %>%
        pull(au_affiliation_raw)
    })
  ) %>%
  select(id, title, arizona_authors)

##############################################33

ua_country_affiliations <- ua_country %>%
  mutate(
    combined_authors = map(author, function(author_df) {
      combined <- author_df %>%
        filter(institution_display_name == "University of Arizona" | institution_country_code == "IN") %>%
        select(au_affiliation_raw, institution_display_name, institution_country_code)
      
      return(combined)
    })
  ) %>%
  unnest(cols = c(combined_authors)) %>%
  select(id, title, au_affiliation_raw, institution_display_name, institution_country_code)


#diff <- setdiff(ua_ca,ua_in)
#diff <- setdiff(ua_in, ua_ca)

### NEED CHECK>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
###################################


### Step 5: Topics 
#ua_country_collaboration <- ua_in

all_topics <- ua_country %>%
  pull(topics) %>%
  map_dfr(function(topic_df) {
    if ("display_name" %in% names(topic_df)) {
      return(data.frame(topic = topic_df$display_name))
    } else {
      return(data.frame(topic = character(0))) # Handle missing column
    }
  })

# All the collaboration topics sorted 
all_topics_sorted <- all_topics %>%
  group_by(topic) %>%
  summarise(count = n()) %>%
  arrange(desc(count))


### Step 6: Unique institutions at that country
nrow(ua_country)
names(ua_country$author[[1]])

ua_country_unnested <- ua_country %>%
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
  distinct(topic)

print(top_institution_topics)


# output in multiple worksheets with Excel
# 1. Details . 2. sorting results. 3. Topics. 

write_df_to_excel <- function(df, file_path_prefix = "collaborations/") {
  df_name <- deparse(substitute(df))
  file_name <- paste0(df_name, ".xlsx")
  file_path <- paste0(file_path_prefix, file_name)
  
  tryCatch({
    write_xlsx(df, file_path)
    message(paste("Successfully wrote", df_name, "to", file_path))
  }, error = function(e) {
    message(paste("Error writing", df_name, "to Excel:", e))
    print(e)
  })
}

# ua_in <- ua_in
# ua_country_institutions_sorted
# ua_country_all_topics
# 

write_df_to_excel(ua_country_institutions)
write_df_to_excel(ua_country_affiliations)
write_df_to_excel(ua_country_institutions_sorted)
write_df_to_excel(all_topics_sorted)
write_df_to_excel(top_institution_topics)

# 2. Combine Excel Files
excel_files <- file.path("collaborations", c("ua_country_institutions.xlsx", "ua_country_affiliations.xlsx", "ua_country_institutions_sorted.xlsx", "all_topics_sorted.xlsx", "top_institution_topics.xlsx"))

tryCatch({
  wb <- createWorkbook()
  
  for (i in seq_along(excel_files)) {
    if (file.exists(excel_files[i])) {
      df <- read.xlsx(excel_files[i])
      sheet_name <- gsub("collaborations/(.*)\\.xlsx", "\\1", excel_files[i])
      addWorksheet(wb, sheetName = sheet_name)
      writeData(wb, sheet = sheet_name, x = df)
    } else {
      message("File not found: ", excel_files[i])
    }
  }
  
  saveWorkbook(wb, "collaborations/ua_in_combined_2022.xlsx", overwrite = TRUE)
  message("!!! Combination successful!")
  
}, error = function(e) {
  message("Combination failed: ", e)
  print(e)
})



