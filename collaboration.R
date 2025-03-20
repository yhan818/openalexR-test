#### Collaboration analysis
######## Author: Yan Han with help of Gemini / Cursor
######## Updated: March 10, 2025
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
rm(list=ls())
gc()

options("max.print" = 100000)
options (openalexR.mailto="yhan@arizona.edu")
getwd()
setwd("/home/yhan/Documents/openalexR-test/")

source("my_functions.R")

works_published_2024 <- readRDS("../works_published_2024.rds")

works_published <- works_published_2024
head(works_published)

##### Steps:
### Step 1: filter by type = article 
works_published_type_articles    <- works_published %>% filter(type == "article")
works_published_type_nonarticles <- works_published %>% filter(type != "article")

### Step 2: Get the authors of the works and related topic-subfield-field-domain structure
# including primary_topic_display_name, primary_topic_subfield_display_name, primary_topic_field_display_name, and primary_topic_domain_display_name
#
# works_published_type_article_authors <- works_published_type_article %>% select(id, doi, title, publication_date, so, host_organization, author, type, referenced_works, topics)

### Step 2: Identify multi-author papers
works_published_type_articles_authors <- works_published_type_articles %>%
  mutate(
    author_count = map_int(author, nrow), # Get the number of rows in each nested df
    has_multiple_authors = author_count > 1 # Check if there are multiple rows
  )

### Step 3: Split it into two works df: US and nonUS authors
# works_published_multi_authors has a new column:
# - nonus_author: TRUE if at least one author is from a non-US country, FALSE otherwise.

works_published_type_articles_authors_nonus <- works_published_type_articles_authors %>%
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
works_published_type_authors_us 


### Step 4: Use NonUS author works to figure out collaboration.
head(works_published_type_articles_authors_nonus)

# Add country codes summary for each work
works_published_w_country_codes <- works_published_type_articles_authors_nonus %>%
  mutate(
    country_codes_summary = map(author, function(author_df) {
      if ("institution_country_code" %in% names(author_df)) {
        codes <- unique(author_df$institution_country_code[!is.na(author_df$institution_country_code)])
        return(paste(sort(codes), collapse = ", "))
      } else {
        return(NA_character_)
      }
    })
  ) %>%
  select(id, title, country_codes_summary, everything())

# Display the results
head(works_published_w_country_codes %>% select(title, country_codes_summary))

# Example: Get all country codes from all nested author data frames. Unnested. 
# all_country_codes <- works_published_multi_authors_nonus %>%
#  pull(author) %>%
#  map_dfr(function(author_df) {
#    if ("institution_country_code" %in% names(author_df)) {
#      return(data.frame(country_code = author_df$institution_country_code))
#    } else {
#      return(data.frame(country_code = character(0))) # Handle missing column
#    }
#  })
#country_code_counts <- table(all_country_codes$country_code)

### Change country code here: "IN", "MX", 
# Create a variable for the target country code
target_country_code <- "IN"  # Change this value for different countries

works_published_ua_country <- works_published_w_country_codes %>%
  filter(
    map_lgl(author, function(author_df) {
      target_country_code %in% author_df$institution_country_code
    })
  )


### Step 6: Find the dept/unit within the University of Arizona. 
### From here, we can find out the author relationship between UA authors and other country authors. 
# extract U of Arizona au_affiliation_raw from each work: So that we know which dept/author with that country dept/author


### Step 7: Topics 
# primary_topics <- extract_topics_by_level(works_cited_type_articles_publisher, 1)

# Check if parameters exist and are valid
if (!exists("works_published_ua_country")) {
  stop("Required data frame ' not found")
}

# Use the correct variable name and add error handling
works_published_ua_country_topics <- tryCatch({
  extract_topics_by_level(works_published_ua_country, 1)
}, error = function(e) {
  message("Error extracting topics: ", e)
  return(NULL)
})

head(works_published_ua_country_topics)

### Step 6: Unique institutions at that country
# Extract all institutions from the target country

library(dplyr)
library(purrr)

 target_country_code <- "US"  # Change this value for different countries

# One work can only count one institution once even multiple authors from the same insitution.
ua_country_institutions <- works_published_ua_country_topics %>%
  mutate(institutions = map(author, ~ {
    if (is.null(.x) || length(.x) == 0) {
      return(tibble(institution = character(0)))
    }
    bind_rows(.x) %>%
      filter(!is.na(institution_country_code) & institution_country_code == target_country_code) %>%
      distinct(institution_display_name) %>% # Get distinct institutions per article
      rename(institution = institution_display_name)
  })) %>%
  unnest(institutions) %>%
  group_by(institution) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count))


# This code will count multiple
# One institution can be counted multiple times if a works has multiple authors from the same institution. 
ua_country_institutions2 <- works_published_ua_country_topics %>%
  mutate(institutions = map(author, function(author_df) {
    # Filter for authors from the target country
    country_authors <- author_df %>%
      filter(institution_country_code == target_country_code)
    
    # Extract unique institutions
    unique_institutions <- unique(country_authors$institution_display_name)
    return(data.frame(institution = unique_institutions))
  })) %>%
  unnest(institutions) %>%
  group_by(institution) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Sort institutions by frequency
ua_country_institutions_sorted <- ua_country_institutions %>%
  arrange(desc(count))

# Within the specific institutions to find out the topics. 
# Get the top institution name (the one with the highest count)


# Find the topics for the top institutions

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
  
  saveWorkbook(wb, "collaborations/ua_in_combined_2024.xlsx", overwrite = TRUE)
  message("!!! Combination successful!")
  
}, error = function(e) {
  message("Combination failed: ", e)
  print(e)
})

############# Testing

target_id <- "https://openalex.org/W4393428092"
result <- works_published_ua_country_topics %>% filter(id == target_id)
library(dplyr)

if (nrow(result) > 0) { # Check if the id was found.
  
  author_df <- result$author[[1]] # Extract the author dataframe.
  
  if (!is.null(author_df) && nrow(author_df) > 0) { # Check that there is an author df.
    distinct_countries <- author_df %>%
      distinct(institution_country_code) %>%
      pull(institution_country_code) # Extract country codes
    
    distinct_institutions <- author_df %>%
      distinct(institution_display_name) %>%
      pull(institution_display_name) # Extract institution names
    
    print("Distinct Countries:")
    print(distinct_countries)
    
    print("Distinct Institutions:")
    print(distinct_institutions)
    
    print(paste("Number of Distinct Countries:", length(distinct_countries)))
    print(paste("Number of Distinct Institutions:", length(distinct_institutions)))
    
  } else {
    print("No author data found.")
  }
  
} else {
  print("ID not found.")
}

