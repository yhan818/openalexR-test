###################### 2. Using found openAlex IDs to get publications #####
######## Author: Yan Han
######## Date: June 5, 2023
##### Search authors' publication using openAlex data ####
# OpenAlex R Documentation: https://github.com/ropensci/openalexR
# OpenAlex Beta explorer: https://explore.openalex.org/ (the explorer seems not to display all the possible researchers. In ohter words, You shall use API
# The explorer can be only used as a verification/testing purpose!!!

install.packages("openalexR")
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

options (openalexR.mailto="yhan@arizona.edu")

### Test data
test_data_COM_authors     <- c("Phillip Kuo", "Bekir Tanriover", "Ahlam Saleh")


test_data_affiliation <- c("University of Arizona")
test_data_year <- c("2022", "2021", "2020", "2012")

# use "search" option with no middle name 
author_from_names <- oa_fetch(entity = "author", search = test_data_COM_authors[2] ) 

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
show_works(author_works)
print(author_works)

# Filter publications after year 2020
filtered_works_after_year_2020 <- author_works %>%
  filter(substr(publication_date, 1,4) >"2020")

# Filter source of work
so_values <- filtered_works_after_year_2020$so

## Questions for openAlex
# https://openalex.org/P4310320990 (does not display anything )
# https://openalex.org/S157664895 (does not display anything)










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


