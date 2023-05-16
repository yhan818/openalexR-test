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
install.packages("testthat")

# common libraries
library(openalexR)
library(dplyr)
library(ggplot2)
library(knitr)
library(testthat)

# For openAlex to get faster response
options (openalexR.mailto="yhan@arizona.edu")

### Test data
test_data_UAL_authors     <- c("Yan Han", "Ellen Dubinski", "Fernando Rios", "Ahlam Saleh")
test_data_COM_authors     <- c("Phillip Kuo", "Bekir Tanriover", "Ahlam Saleh")
test_data_science_authors <- c("Marek Rychlik", "Ali Bilgin", "Beichuan Zhang")
test_data_ischool_authors <- c("Hong Cui")
test_data_others          <- c("Leila Hudson", "Mona Hymel")


test_data_affiliation <- c("University of Arizona")
test_data_year <- c("2022", "2021", "2020", "2012")

################################ Author ##############################
# Filter Doc: https://github.com/ropensci/openalexR/blob/main/vignettes/articles/Filters.Rmd

#### 1. First do a fuzzy search on author's name ##########################
###  do NOT use display_name because it requires an exact match. Often there are multiple middle names for an author
author_from_names <- oa_fetch(entity = "author",
                               search = test_data_COM_authors[1] ) ### "search" syntax allows fuzzy search for middle name

# Filter out not "University of Arizona" authors using "affiliation_display_name" column. 
# other filtering fields can be "affiliation_id", "affiliation_ror"
filtered_authors <- subset(author_from_names, grepl("University of Arizona", affiliation_display_name, ignore.case=TRUE)) 
# Showing the results
filtered_authors |> 
  show_authors() |>
  knitr::kable()             

# output works_count in 2022
# works_count is a list
works_count  <- filtered_authors$counts_by_year
print(works_count)

# cited_by_count 
cited_by_count <- filtered_authors$cited_by_count

# Testing data
works_sum_year <- 0
total_works_sum_year <- 0
cited_sum_year <- 0
total_cited_sum_year <-0

### Handle empty list
df <-works_count[[20]]
if (is.null(df)) { 
  print("Emement is null")
} else {
    print("CONT")
  }


filtered_df_2022 <- df[df$year == 2022, ]

# Loop through each data frame in the list 
length(works_count)
for (i in 1:length(works_count)) {
  # Access the data frame within the list
  df <- works_count[[i]]
  
  
  if (is.data.frame(df)) {
    ##### Note: If you see error msg: Error: $ operator is invalid for atomic vectors
    # That means certain works_count  is logical and has no data
  ########## Building block for count each year
  # Filter the data frame for the year 2022
  filtered_df_2022 <- df[df$year == 2022, ]
  # Calculate the sum of the filtered 'works_count' column
  works_sum_2022 <- sum(filtered_df_2022$works_count)
  total_works_sum_2022 <- total_works_sum_2022 + works_sum_2022
  # Calculte the sum of the filtered 'cited_by_count' column
  cited_sum_2022 <- sum(filtered_df_2022$cited_by_count)
  total_cited_sum_2022 <- total_cited_sum_2022 + cited_sum_2022
  
  # reset this number to 0 
  works_sum_2022 <- 0 
  cited_sum_2022 <- 0
  } else {
    print("This is NOT a dataframe. Data wrong")
  }
  
}

total_cited_sum_2022 <- 0
total_works_sum_2022 <- 0

#############################
#####################################################
# Function: Calculate works count
#####################################################
calculate_works_count <- function(author, affiliation, year) {
  # getting data from openAlexR API
  author_from_names <- oa_fetch(entity = "author", search = author )
  # Filter out not "University of Arizona" authors using "affiliation_display_name" column. 
  # other filtering fields can be "affiliation_id", "affiliation_ror"
  filtered_authors <- subset(author_from_names, grepl(affiliation, affiliation_display_name, ignore.case=TRUE)) 
  print(filtered_authors)
  
  # works_count is a list, getting "counts_by_year" column
  works_count  <- filtered_authors$counts_by_year
  print(works_count)
  works_sum_year <- 0
  total_works_sum_year <- 0
  
  # cited_by_count is a list
  cited_by_count <- filtered_authors$cited_by_count
  cited_sum_year <- 0
  total_cited_sum_year <-0
  
  for (i in 1:length(works_count)) {
    # Access the data frame within the list
    df <- works_count[[i]]

    if (is.data.frame(df)) {
      # Filter the data frame by year
      filtered_df_year <- df[df$year == year, ]
  
      # Calculate the sum of the filtered 'works_count' column
      works_sum_year <- sum(filtered_df_year$works_count)
      total_works_sum_year <- total_works_sum_year + works_sum_year
      # Calculate the sum of the filtered 'cited_by_count' column
      cited_sum_year <- sum(filtered_df_year$cited_by_count)
      total_cited_sum_year <- total_cited_sum_year + cited_sum_year
    
      # reset this number to 0 after each iteration
      works_sum_year <- 0 
      cited_sum_year <- 0
    } else {
      ##### Note: If you see error msg: Error: $ operator is invalid for atomic vectors
      # That means certain works_count  is logical and has no data
      
      print("This is NOT a dataframe. Data Wrong")
      # set value as -1 for warning message
      Total_sum_of_works = -1
      Total_cited_sum_year = -1
    }
  }
  # Build output dataframe author_stats
  author_stats <-data.frame (
    Name = author,
    OpenAlexId = filtered_authors$id,
    Year = year,
    Total_sum_of_works = total_works_sum_year,
    Total_sum_of_cited = total_cited_sum_year
  )
  
  # reset the var after done
  total_works_sum_year <- 0
  total_cited_sum_year <- 0
  
  return (author_stats)
  }

########################## TESTING PEOPLE I KNOW ####################
### Format: Name: Year: Works/Cited
### Yan Han: 2022: 0/0
author_stats <- calculate_works_count(test_data_UAL_authors[1], test_data_affiliation[1], test_data_year[1])
rm(author_stats)

############# College of Medicine Tucson Test Date:  2023-05-14: If test in a different date, result may vary 
#### Phillip Kuo: 2022: 30/133: 26 IDs
author_stats <- calculate_works_count(test_data_COM_authors[1], test_data_affiliation[1], test_data_year[2])
### Benkir Tanriover returns 5 openAlex ID: 2022: works 11, cited 166; 2021: works 4 cited 167
author_stats <- calculate_works_count(test_data_COM_authors[2], test_data_affiliation[1], test_data_year[1])
author_stats <- calculate_works_count(test_data_COM_authors[2], test_data_affiliation[1], test_data_year[2])
###  Ahlam Saleh returns 0, because of "One list does not contain a valid OpenAlex collection" ????
author_stats <- calculate_works_count(test_data_COM_authors[3], test_data_affiliation[1], test_data_year[1])

######## Science
### Marek Rychlik returns : works 0, cited 11
author_stats <- calculate_works_count(test_data_science_authors[1], test_data_affiliation[1], test_data_year[1])
### Ali Bilgin: works 4, cited 187
author_stats <- calculate_works_count(test_data_science_authors[2], test_data_affiliation[1], test_data_year[1])

### Hong Cui: Error in works_count[[i]], subscript out of bonds! need to test!!! 
author_stats <- calculate_works_count(test_data_ischool_authors[1], test_data_affiliation[1], test_data_year[1])

###### Others
### Leila Hudson: 2022: 0/2
author_stats <- calculate_works_count(test_data_others[1], test_data_affiliation[1], test_data_year[1])

# Error in works_count[[i]] : subscript out of bounds

author_stats <- calculate_works_count(test_data_others[2], test_data_affiliation[1], test_data_year[1])



###########################################

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
  unit_author_from_names <- oa_fetch(entity = "author", search = unit_author) 
  # create an empty df
  unit_author <- data.frame() 
  # filter author based on his/her affiliation_display_name 
  unit_author <- subset(unit_author_from_names, grepl("University of Arizona", affiliation_display_name, ignore.case=TRUE))
  
  # Append the author (df) as an element to the list
  unit_authors_list <- append(unit_authors_list, list(unit_author))
  
  # Clear unit_author
  unit_author <- NULL
  
}

# 2. Output

for (i in 1:length(unit_authors_list)) {
 print(unit_authors_list[[i]]) 
  # Loop through each element in the author list to calculate the sum of publications 
  author_works_count <-sum (unit_authors_list[[i]]$works_count)
  output_string <- "Researcher" + unit_authors_list[[i]]$display_name + "Year" + counts_by_year
  
  }


