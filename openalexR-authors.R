############# Author's search ##########
######## Author: Yan Han 
######## Date: May 9, 2023
##### Search authors' publication using openAlex data ####  
# OpenAlex R Documentation: https://github.com/ropensci/openalexR
# OpenAlex Beta exploer: https://explore.openalex.org/

install.packages("openalexR")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("knitr")

# common libraries
library(openalexR)
library(dplyr)
library(ggplot2)
library(knitr)

# For openAlex to get faster response
options (openalexR.mailto="yhan@arizona.edu")

################################ Author ##############################
# Filter Doc: https://github.com/ropensci/openalexR/blob/main/vignettes/articles/Filters.Rmd

#### 1. First do a fuzzy search on author's name ##########################
###  do NOT use display_name because it requires an exact match. Often there are multiple middle names for an author
author_from_names <- oa_fetch(entity = "author",
                               search = "Phillip Kuo" ) ### "search" syntax allows fuzzy search for middle name
#grep("Arizona*", authors_from_names$affiliation_display_name, value=TRUE, ignore.case=TRUE) 

# Filter out not "University of Arizona" authors using "affiliation_display_name" column. 
# other filtering fields can be "affiliation_id", "affiliation_ror"
filtered_authors <- subset(author_from_names, grepl("University of Arizona", affiliation_display_name, ignore.case=TRUE)) 
# Showing the results
filtered_authors |> 
  show_authors() |>
  knitr::kable()             

# search and filter using openAlex institution ID "affiliation_id"
author2_from_names <- oa_fetch(entity = "author", search = "Yan Han" ) ### "search" syntax allows fuzzy search for middle name
filtered_author2 <- subset(author2_from_names, grepl("https://openalex.org/I138006243", affiliation_id, ignore.case=TRUE)) 

# display_name and filter using ROR "affiliation_ror"
author3_from_names <- oa_fetch(entity = "author", display_name = c("Bekir Tanriover", "Ahlam Saleh") ) ### "search" syntax allows fuzzy search for middle name
filtered_author3 <- subset(author3_from_names, grepl("https://ror.org/03m2x1q45", affiliation_ror, ignore.case=TRUE)) 

##### 2. Using found openAlex IDs to get publications #####

openalex_ids_author <-oa_fetch(entity = "author", openalex = "A4353996111" )
openalex_id_author

author_works <- oa_fetch ( entity = "works",
                           author.id=("A4353996111"),
                           verbose = TRUE )  
show_works(author_works) 

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


##### Testing with multiple authors
unit_authors_names <- list( "Phillip Kuo", "Bekir Tanriover", "Ahlam Saleh") 
unit_authors_list <- list()

# 1. Filter author one by one using his/her name using fuzzy search option
for (unit_author in unit_authors_names) {
  # find an author's name using fuzzy search
  unit_author_from_names <- oa_fetch(entity = "author", search = unit_author) ### "search" syntax allows fuzzy search
  # create an empty df
  unit_author <- data.frame() 
  # filter author based on his/her affiliation_display_name 
  unit_author <- subset(unit_author_from_names, grepl("University of Arizona", affiliation_display_name, ignore.case=TRUE))
  
  # Append the author (df) as an element to the list
  unit_authors_list <- append(unit_authors_list, list(unit_author))
  
  # Clear unit_author
  unit_author <- NULL
  
}
