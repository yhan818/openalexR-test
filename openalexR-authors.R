############# Author's search ##########
######## Author: Yan Han
######## Date: May 9, 2023
######## Updated: June 21, 2023
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

getwd()
setwd("/home/yhan/Documents/openalexR-test")

### LDAP search
#LDAP query against ldap.arizona.edu (public, no account required), e.g.
# Via linux terminal: ldapsearch -H ldap://ldap.arizona.edu -D "" -b "o=University of Arizona,c=US" -w -x 'departmentNumber=1705' givenName sn

# To find Dept HR code: log into apps.iam.arizona.edu to search person >> OrgSearch (partent org, child orgs) 
# Example: https://apps.iam.arizona.edu/orgs/ua_orgs/view/1705

# College of Medicine Department of: code : 0713

# R does not have LDAP packages??
# clean all objects from the environment to start
rm(list = ls())

# For openAlex to get faster response
options (openalexR.mailto="yhan@arizona.edu")

### Test data
test_data_UAL_authors     <- c("Yan Han", "Ellen Dubinski", "Fernando Rios", "Ahlam Saleh")
test_data_COM_authors     <- c("Phillip Kuo", "Bekir Tanriover", "Ahlam Saleh")
test_data_COM_Tucson_authors <- c("Che Carrie Liu", "Robert M. Aaronson", "Alexa Aasronson", "Mohammed Abbas", "")
test_data_science_authors <- c("Marek Rychlik", "Ali Bilgin", "Beichuan Zhang")
test_data_ischool_authors <- c("Hong Cui")
test_data_others          <- c("Leila Hudson", "Mona Hymel")

test_data_affiliation <- c("University of Arizona")
test_data_year <- c("2022", "2021", "2020", "2012")

# Read test data from CSV: column: LastName, FirstName
file_path <- "test_authors_COM_0713.csv"    #"test_authors_1.csv"
data <- read.csv(file_path) 

# create an empty vector
data_unit_staff <- list()
# iterate 
for (i in 1:nrow(data)) {
  firstname <- as.character(data[i, "givenName"])
  lastname <- as.character(data[i, "sn"])
  data_unit_person <- paste(firstname, lastname)
  data_unit_staff  <- c(data_unit_staff, data_unit_person)
}

### Testing 
author_from_names <- oa_fetch(entity = "author", search = data_unit_staff[2]) 

################################ Finding Author's name, affiliation, sum of works, and total citations ##############################
# Filter Doc: https://github.com/ropensci/openalexR/blob/main/vignettes/articles/Filters.Rmd

#### 1. First do a fuzzy search on author's name ##########################
###  do NOT use display_name because it requires an exact match. Often there are multiple middle names for an author
author_from_names <- oa_fetch(entity = "author",
                               search = test_data_COM_authors[2] ) ### "search" syntax allows fuzzy search for middle name
# first checking
if (!is.null(author_from_names)) {
  print("author found")
  # Filter out not "University of Arizona" authors using "affiliation_display_name" column.
  # other filtering fields can be "affiliation_id", "affiliation_ror"
  filtered_authors <- subset(author_from_names, grepl("University of Arizona", affiliation_display_name, ignore.case=TRUE))
  # Showing the results
  filtered_authors |>
    show_authors() |>
    knitr::kable()
  if (nrow(filtered_authors) == 0 )  {
    print("The filtered_authors dataframe is empty")
  }
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
}

#####################################################
# Function: Calculate works count
#####################################################
calculate_works_count <- function(author, affiliation, year) {
  # getting data from openAlexR API
  author_from_names <- oa_fetch(entity = "author", search = author )
  if (is.null(author_from_names)) {
    author_stats <- NULL
  }
  else {
    # Filter out not "University of Arizona" authors using "affiliation_display_name" column.
    # other filtering fields can be "affiliation_id", "affiliation_ror"
    filtered_authors <- subset(author_from_names, grepl(affiliation, affiliation_display_name, ignore.case=TRUE))
    print(filtered_authors)

    # If NOT found the author, return NULL
    if (nrow(filtered_authors) == 0 )  {
      author_stats <- NULL
      }
    else {
      # works_count is a list, getting "counts_by_year" column
     works_count  <- filtered_authors$counts_by_year
     print(works_count)
     works_sum_year <- 0
     total_works_sum_year <- 0

      # cited_by_count is a list
      cited_by_count <- filtered_authors$cited_by_count
      cited_sum_year <- 0
      total_cited_sum_year <-0

      # check the works_count
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
      } # for loop

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
    } # if

   return (author_stats)
  }
}

########################## TESTING PEOPLE ####################
### Format: Name: Year: Works/Cited

############# College of Medicine Tucson Test Date:  2023-05-14: If test in a different date, result may vary
#### U of Arizona College of Medicine Faculty and Staff Directory https://medicine.arizona.edu/directory/faculty-staff
#### Phillip Kuo: 2022: 30/133: 26 IDs
author_stats <- calculate_works_count(test_data_COM_authors[1], test_data_affiliation[1], test_data_year[1])
### Benkir Tanriover returns 5 openAlex ID: 2022: works 11, cited 166; 2021: works 4 cited 167
author_stats <- calculate_works_count(test_data_COM_authors[2], test_data_affiliation[1], test_data_year[1])
author_stats <- calculate_works_count(test_data_COM_authors[2], test_data_affiliation[1], test_data_year[2])
###  Ahlam Saleh returns 0, because of "One list does not contain a valid OpenAlex collection" ????
author_stats <- calculate_works_count(test_data_COM_authors[3], test_data_affiliation[1], test_data_year[1])

###### College of Medicine Tucson
author_stats <- calculate_works_count(test_data_COM_Tucson_authors[1], test_data_affiliation[1], test_data_year[1])
author_stats <- calculate_works_count(test_data_COM_Tucson_authors[2], test_data_affiliation[1], test_data_year[1])
author_stats <- calculate_works_count(test_data_COM_Tucson_authors[3], test_data_affiliation[1], test_data_year[1])
author_stats <- calculate_works_count(test_data_COM_Tucson_authors[4], test_data_affiliation[1], test_data_year[1])
author_stats <- calculate_works_count(test_data_COM_Tucson_authors[5], test_data_affiliation[1], test_data_year[1])


######## Science
### Marek Rychlik returns : works 0, cited 11
author_stats <- calculate_works_count(test_data_science_authors[1], test_data_affiliation[1], test_data_year[1])
### Ali Bilgin: works 4, cited 187
author_stats <- calculate_works_count(test_data_science_authors[2], test_data_affiliation[1], test_data_year[1])

####### iSchool
### Hong Cui: NOT found: 2022: NULL
author_stats <- calculate_works_count(test_data_ischool_authors[1], test_data_affiliation[1], test_data_year[1])

###### Others
### Leila Hudson: 2022: 0/2
author_stats <- calculate_works_count(test_data_others[1], test_data_affiliation[1], test_data_year[1])

# Error in works_count[[i]] : subscript out of bounds
author_stats <- calculate_works_count(test_data_others[2], test_data_affiliation[1], test_data_year[1])


##########################################################
#### testing all authors in College of Medicine - Tucson ###
### Including all faculty and staff at https://medicine.arizona.edu/directory/faculty-staff
rm(unit_authors_list)

# 10 people per line
# Author's name only contain First name and Last name.
# Author's middle name is not included due to index often does not contain such.
test_data_COM_Tucson_authors <- c("Che Liu", "Robert Aaronson", "Alexa Aaronson", "Joy Abaidoo", "Stephanie Abalos", "Mohammed Abbas", "Adnan Abbasi",  "Kristopher Abbate", "Michelle Abbate", "Lara Abbottt",
                                  "Gebran Abbound", "Mazin Abdaigadir", "Adel Abdallah", "Arwa Abdel-Raheem", "Yasmeen Abdulrahman", "Kalkidan Abebe", "Michael Abecassis", "Anne Abel", "Ty Abel", "Yulia Abidov",
                                  "Richard Ablin", "Ahmed Aboul-Nasr", "Arin Aboulian", "Suzanne Abrahamson", "Amber Abrams", "Artyom Abramyan", "Edward Abril", "Anas Husni Mohamad Abu Assi", "Laila Zaid", "Joe Abuhakmeh",
                                  "Xiaohong Zhang", "Hui Zhang")


data_COM_dept_0713_staff <- c("Andrea Morton", "Maryam Emami Neyestanak", "Zerema Nagoyev", "Haw-chih Tai", "Dianesh Bharti", "Darleen Redondo", "Rachael Bendall", "Karen Padilla", "James Liao",
                                "Jazmine Aguilar", "Aleksandr Dekan", "Ivonne Bello", "Gary Langworthy", "Alyussa Campbell", "Yvette Marinez", "Hannah Cowling", "Hannah Gannon", "Tera Bolton",
                                "Dorothy Campos", "Sharon Halvorsen", "Ian Boggs", "Beverly Gordon", "Palash Mallick", "Gabriela Montenegro Vargas", "Bekir Tanriover", "Meghan Gerhart",
                                "Shasta McManus", "Katherine Mendoza", "Keyu Song", "Aline Kellerman", "Luz Badilla", "Sarah Yates", "Suzann Duan", "Edward Gelmann", "Nicole Marquez", "Courtney Smith",
                                "Joel James", "Katherine Sepulveda", "Lin Ding", "Sulaiman Sheriff", "Neil MacDonald", "Sarah Munoz", "Juanita Merchang", "Nicole Sullivan", "Carolyn Bothwell",
                                "Rachna Shroff", "Matthew Ollerton", "Karen Railey", "Luis Benitez", "Vivian Kominos", "Huashi Li", "Mathews Valuparampil Varghese", "Christeana Castro", "Fariba Donovan",
                                "Xingnan Li", "Baltazar Campos", "Deborah Meyers", "Eugnene Bleecker", "Lizette Martinez", "Sicily La Rue", "Paul Langlais", "Krystal Fimbres", "Wayne Willis", "Rocio Zapata Bustos"
                                )

unit_authors_list <- list(author_stats)
unit_authors_list_org_arizona <- list(author_stats)
#author_stats <-calculate_works_count(test_data_COM_Tucson_authors[1], "University of Arizona",2022)
#unit_authors_list <- append(unit_authors_list, list(author_stats))
#unit_authors_list

###
authors <- data_COM_dept_0713_staff

authors <- data_unit_staff

#### Retrieving one author at a time.
for (author in authors) {
  print(author)
  # Setting org_name can be critical for the # of results got.
  # If org_name = "", there will be many unrelated people.
  # College of Medicine OpenAlex data some do not have affiliation with University of Arizona.
  org_name = "university"
  temp_author_status <- calculate_works_count(author, org_name, 2022)
  temp_author_status
  unit_authors_list <- append(unit_authors_list, list(temp_author_status))
  }

unit_authors_list_org_arizona <- unit_authors_list


###########################################

# search and filter using openAlex institution ID "affiliation_id"
author2_from_names <- oa_fetch(entity = "author", search = "Yan Han" ) ### "search" syntax allows fuzzy search for middle name
filtered_author2 <- subset(author2_from_names, grepl("https://openalex.org/I138006243", affiliation_id, ignore.case=TRUE))

# display_name and filter using ROR "affiliation_ror"
author3_from_names <- oa_fetch(entity = "author", display_name = c("Bekir Tanriover", "Ahlam Saleh") ) ### "search" syntax allows fuzzy search for middle name
filtered_author3 <- subset(author3_from_names, grepl("https://ror.org/03m2x1q45", affiliation_ror, ignore.case=TRUE))



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


