###################### 2. Using found openAlex IDs to get publications #####
######## Author: Yan Han
######## Date: June 5, 2023
######## Updated: June 5, 2023
##### Search authors' publication using openAlex data ####
# OpenAlex R Documentation: https://github.com/ropensci/openalexR
# OpenAlex Beta explorer: https://explore.openalex.org/ (the explorer seems not to display all the possible researchers. In ohter words, You shall use API
# The explorer can be only used as a verification/testing purpose!!!

install.packages("remotes")
remotes::install_github("ropensci/openalexR", force=TRUE)

#install.packages("openalexR")  
install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("knitr")
install.packages("testthat")

# common libraries
library(openalexR)
library(dplyr)
library(ggplot2)
library(knitr)
library(testthat)
library(readr)

citation("openalexR")

# check to see if openAlexR has the latest entities in OpenAlex (OpenAlex updated its data model(Entities) in June 2023)
# Before April 2023: they are [1] "works"        "authors"      "venues"       "institutions" "concepts"    
# If not, need to use openalexR developer's version
oa_entities()

options (openalexR.mailto="yhan@arizona.edu")
getwd()
setwd("/home/yhan/Documents/openalexR-test")

### Test data
test_data_COM_authors     <- c("Phillip Kuo", "Bekir Tanriover", "Ahlam Saleh")
test_data_affiliation <- c("University of Arizona")
test_data_year <- c("2022", "2021", "2020", "2012")

author_from_names <- oa_fetch(entity = "author", search = "Bekir Tanriover" )
author_from_names <- oa_fetch(entity = "author", search = test_data_not_updated_authors[1] )
author_from_names <- oa_fetch(entity = "author", search = "Haw-chih Tai")

# Test works 
works_from_dois <- oa_fetch(entity = "works", doi = c("https://doi.org/10.1681/asn.2012070664", "https://doi.org/10.1007/s11192-013-1221-3"),  verbose = TRUE)
# To see is_oa field

# use "search" option with no middle name 
author_from_names <- oa_fetch(entity = "author", search = test_data_COM_authors[2]) 

# first checking if author exists
if (!is.null(author_from_names)) {
  print("author found")
  # Filter out not "University of Arizona" authors using "affiliation_display_name" column.
  # other filtering fields can be "affiliation_id", "affiliation_ror"
  filtered_authors <- subset(author_from_names, grepl("University of Arizona", affiliation_display_name, ignore.case=TRUE))
  if (nrow(filtered_authors) == 0 )  {
    print("The filtered_authors dataframe is empty")
  }
}
# print openalex ID
print(filtered_authors)
print(filtered_authors[1][1])


## Testing 
openalex_author_id <- filtered_authors$id[1]
author_works <- oa_fetch ( entity = "works",
                           author.id=(openalex_author_id),
                           verbose = TRUE )
object_type <- class(author_works)
print(object_type)

# Filter publications after year 2020
filtered_author_works <- author_works %>%
  filter(substr(publication_date, 1,4) >"2020")

# Filter source of work
so_values <- filtered_author_works$so

#convert author_works$author (list) to , separated by "," using sapply
# List "author" contains all the authors info
filtered_author_works$author <- sapply(filtered_author_works$author, paste, collapse = ",")
# List "ids" contains openAlexId, doi, and pmid
filtered_author_works$ids <- sapply(filtered_author_works$ids, paste, collapse = ",")
# List "referenced_works" contains references
filtered_author_works$referenced_works <- sapply(filtered_author_works$referenced_works, paste, collapse = ",")
# List "related_works" contains related works
filtered_author_works$related_works <- sapply(filtered_author_works$related_works, paste, collapse = ",")

filtered_author_works$counts_by_year <- NULL
filtered_author_works$concepts <- NULL

field_names <-colnames(filtered_author_works)
print(field_names)



selected_columns <- filtered_author_works[c("id", "display_name", "author", "ab", "publication_date",  "relevance_score", "so", "so_id", "host_organization", "issn_l", "url"
                                            , "pdf_url", "license", "version", "first_page", "last_page", "volume", "issue", "is_oa", "cited_by_count", "publication_year"
                                            ,"cited_by_api_url", "ids", "doi", "type", "referenced_works", "related_works")]
write.csv(selected_columns, file = "author_works.csv", row.names = TRUE)

# get work
works1 <- "W4213067910"
test1 <- oa_fetch(
  identifier = works1,
  entity = "works"
)
print(works1)

## Questions for openAlex
# host_organization: https://openalex.org/P4310320990 (does not display anything ) (organization from https://openalex.org/W2167151078 << search from "Bekir Tanriover"
# so_id: https://openalex.org/S157664895 (does not display anything)









# Testing multiple authors
openalex_ids_authors <- c("A4353685810", "A4354460443")

# Create an empty list to store the downloaded data for each author
authors_data <- list()

# Iterate over the author IDs and download data for each
for (id in openalex_ids_authors) {
  authors_data[[id]] <- oa_fetch(entity = "author", openalex = id)
}

# 3. Now that we have data in list, iterate over the list elements
for (author_data in authors_data) {
  #print(author_data)
  print(author_data$works_count)
  print(author_data$counts_by_year)
  
}


