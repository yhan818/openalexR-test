#### Journal analysis ##### 
# Author: Yan Han with assistance of ChatGPT 4o
# Updated: Oct 7, 2024

library(httr)
library(jsonlite)
library(openalexR)
library(dplyr)
library(tidyverse)
library(writexl)

options("max.print" = 100000)
options (openalexR.mailto="yhan@arizona.edu")
getwd()
setwd("/home/yhan/Documents/openalexR-test/")

### JPE https://openalex.org/S161564205
### JPE article example: https://openalex.org/W2772802389

# Define the journal ID
journal_id <- "S161564205"

# find the journal info
journal <- oa_fetch(
  entity = "source", 
  identifier = journal_id
)

view (journal)

test_article <- oa_fetch (
  entity = "works", 
  identifier = "https://openalex.org/W2772802389"
)

# JPE has 926 articles 
# Questions for openAlex: API shows 926 works (using best_oa_location.source.id), 
#           GUI says: 834 , Why? https://openalex.org/works?filter=primary_location.source.id%3As161564205
#

journal_articles <- oa_fetch(
  entity = "works", 
  best_oa_location.source.id  = c(journal_id)
)

# Find if "so" contains # Find the unique values in the $so column
journal_articles_so_unique_values <- unique(tolower(journal_articles$so))

# Find if multiple "issn_l" with the journal
journal_articles_issn_unique_values <- unique(tolower(journal_articles$issn_l))

# Sort the data frame by $cited_by_count in descending order
journal_articles_cited_by_count <- journal_articles[order(-journal_articles$cited_by_count), ]

# View the sorted data
print(journal_articles_cited_by_count)

write_xlsx(journal_articles_cited_by_count, "citations/JPE_articles_cited_by_count.xlsx")





