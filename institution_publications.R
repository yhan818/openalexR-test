############# Institution authors publication analysis and Collection Management ##########
######## Author: Yan Han with help of ChatGPT 4
######## Updated: Oct 1, 2024
######## Updated: Fixed NA issue with host_organization
##### Search an institution authors' publication using openAlex data ####
# OpenAlex R Documentation: https://github.com/ropensci/openalexR

install.packages("remotes")
remotes::install_github("ropensci/openalexR", force=TRUE) 
install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2")

library(openalexR)
library(dplyr)
library(tidyverse)
library(writexl)
# free unused obj to manage memory
gc()
rm(list=ls())
gc()

options("max.print" = 100000)

options (openalexR.mailto="yhan@arizona.edu")
getwd()
setwd("/home/yhan/Documents/openalexR-test/")

# Banner-University Medical Center Tucson. 399 works.
UAUMC.df <-oa_fetch(
  entity="works",
  institutions.ror=c("02xbk5j62"),
  from_publication_date ="2024-01-01")

##### 1. Getting data. (retrieved 2024-09-02)
# Retrieving all publications association with UArizona's ROR (Research Organization Registry) ID.
# UA publications per year is ~9,000. For running 2 years data, need better computer or crashed R studio.
# Year 2023: 9,384 works.
# Year 2021: 9,473 works
# Year 2020: 
# Year 2019: 8,847 
# 2023-current: 14,660 works : 5 min to get UAworks with 3 GB mem, 264 mins to pull 372,000 reference's data with 8.6 GB  
# 2022-current: 23,360 works: 10 mins to get UAWorks with 6 GB RAM, 450 mins to pull 560,000 citedWorks's data with 12 GB. crashed R studio.
# 2020-current: 
# 2014-current: 86,000 works : 15 mins to run, and used 7GB RAM. 
# 2013-current: 50,000 records: 

### 1.1 Getting the count only. This is the quick way to find out the total number of works. 
# Typically only some seconds
UAworks_count <-oa_fetch(
  entity="works",
  institutions.ror=c("03m2x1q45"),
  from_publication_date ="2020-01-01",
  to_publication_date = "2020-12-31",
  count_only = TRUE
)

### 1.2 Getting all the works based on the institution ROR and publication date. It takes longer time. 
# see above for the running time
# 2019 
org_works_2019 <-oa_fetch(
  entity="works",
  institutions.ror=c("03m2x1q45"),
  from_publication_date ="2020-01-01",
  to_publication_date = "2020-12-31"
)

org_works_2020 <-oa_fetch(
  entity="works",
  institutions.ror=c("03m2x1q45"),
  from_publication_date ="2020-01-01",
  to_publication_date = "2020-12-31"
)

org_works_2022 <-oa_fetch(
  entity="works",
  institutions.ror=c("03m2x1q45"),
  from_publication_date ="2022-01-01",
  to_publication_date = "2022-12-31"
)

org_works_2023 <-oa_fetch(
  entity="works",
  institutions.ror=c("03m2x1q45"),
  from_publication_date ="2023-01-01",
  to_publication_date = "2023-12-31"
)


# saveRDS(org_works_2019, "../org_works_2019.rds")
# saveRDS(org_works_2020, "../org_works_2020.rds")
# saveRDS(org_works_2021, "../org_works_2021.rds")
# saveRDS(org_works_2022, "../org_works_2022.rds")
 saveRDS(org_works_2023, "../org_works_2023.rds")

 org_works_2019 <- readRDS("../org_works_2019.rds")
# org_works_2020 <- readRDS("../org_works_2020.rds")
# org_works_2021 <- readRDS("../org_works_2021.rds")
# org_works_2022 <- readRDS("../org_works_2022.rds")
org_works_2023 <- readRDS("../org_works_2023.rds")

# change working data here 
 org_works <- org_works_2019
#org_works <- org_works_2023


##### 2. Checking and verifying data
##### 2.1 Route 1: Getting citation data from $referenced_works
##### Route 2: Getting author's data? 
###### change this line only to update the right dataset.
org_works_ref <- org_works$referenced_works
#########################

# Find "NA" indexes: 18- 25% no references 
# Questions for openAlex: 
# 1. Is this normal? any plan to improve? 
# 2. I checked ~3500 records (1% ), Field “issn_l” has values, but “host_organization” field has no values. 
# 3. 

# Year 2019: 1575 / 8848 referenced works value="NA", while $type is "article". 18%
# Year 2023: 1534 / 9384 referenced works value="NA", while $type is "article". 

# Filter the rows where $reference_works is NA and $type is "article"
works_na_referenced_works <- org_works %>%
  filter(is.na(referenced_works) & type == "article")
write_xlsx(works_na_referenced_works, "citations/works_2019_na_referenced_works.xlsx")

# this na_indices include type: article, books, errata, letter, and other types
na_indices <- which(sapply(org_works_ref, function(x) is.logical(x) && is.na(x))) 
na_count <- sum(sapply(org_works_ref, function(x) is.logical(x) && is.na(x)))
na_percent <- na_count/length(org_works_ref) * 100

### 2.2 Combine all the references and do further data analysis
# Avg # of references per article: ~50
# Year 2023 total references: 364,304: unique 281,470 / 351,479: more cited: ~77,000 
# Year 2022 total references: 345,904
# Year 2019 total references: 352,509: articles 329,000 

# Remove NA, logical(0) from list (Meaning: no references) 
org_works_ref <- Filter(function(x) length(x) > 0, org_works_ref)
class(org_works_ref)

org_works_ref_combined <- unlist(org_works_ref, use.names = FALSE)
org_works_ref_combined <- org_works_ref_combined[!is.na(org_works_ref_combined)]  # Remove NA values

### 2.21 finding these duplicates, which mean the duplicates have been cited multiple times 
# (probably more important to have these journals subscribed!)
# cited more: ~20% - 25%  (2022, 2023 UArizona data)
org_works_ref_more_cited <- org_works_ref_combined[duplicated(org_works_ref_combined)]
org_works_ref_unique <- org_works_ref_combined[!duplicated(org_works_ref_combined)]

### Method 2: there are different
citation_counts <- table(org_works_ref_combined)
head(citation_counts)
# Extract citations that occur more than 80 times (i.e., duplicates)
org_works_ref_more_cited2 <- names(citation_counts[citation_counts > 80])
head(org_works_ref_more_cited2)

############################################################
### 2.23 For Testing purpose: Trace back from the cited article -> $referenced_works -> original published article
# Find the index of multiple samples
head(org_works$referenced_works)
head(org_works_ref_unique)

# Use sapply to find matching elements in the org_works_ref for testing. 
matching_indices <- which(sapply(org_works_ref, function(x) 
  any(x %in% c("https://openalex.org/W1624352668", "https://openalex.org/W1548779692")))) # https://openalex.org/W1624352668 were cited on 2021 and 2023 data
print(matching_indices)

# We can see the original works for samples
org_works[2, "id"]
org_works[174, "id"]

# Test to see how many times a work is cited. 
index <- which(org_works_ref_more_cited == "https://openalex.org/W4247665917")
print(index)

# https://openalex.org/W4247665917 were cited in 2019, 2021 and 2023 data
index <- which(org_works_ref_unique == "https://openalex.org/W4247665917")
print(index)
org_works_ref_unique[136]

###########################################################

##### 3. From authors' DF. 
# Flattening authors fields from the DF (multiple authors per work). 
# 426,000 obs (multiple authors) from 50,400 obs (works)
org_works_since <- org_works

#### Year 2022: 
# -- org_works_authors: 75,222
# -- org_works_UAauthors: 16,432
# -- org_works_ua_authors_ref_combined: 656,712
# -- org_works_ua_authors_ref_cited: 249,629
# 
org_works_authors<-org_works_since%>%
  mutate(author=lapply(author, function(x){
    names(x) <-paste0(names(x), "author")
    return(x)
  }))%>%
  unnest(author)

rm(list = c("org_works_authors", "na_percent" ))

UAauthors <-unique(org_works_authors)
#write_xlsx(UAauthors2, "UAauthors.xlsx")

# After flattening, authors' fields (e.g. au_idauthor, institution_rorauthor) are displayed
colnames(org_works)
colnames(org_works_authors)

#Creating an empty dataframe to store the results of the for loop.
works_cited <-data.frame()

# Getting these works' metadata. This takes long time to run. 
# Warnings(). a work > 100 authors will be truncated 
# 2024: 
# 2023: 352,509 (checked) out of 364,304 
# 2022: 249,629 (re-check), 174 min to fetch all the works; file size 438 M
# 2021: 
# 2019: 331,657 (checked).


#####################################################
#################### 3.3 TESTING!!!#################
####################################################
# Then extract UArizona authors only
# 94,500 obs from 426,000 obs (UA authors only).  
## https://openalex.org/A5033317672 Saurav Mallik (is at two affiliations for https://api.openalex.org/works/W4389611927. Harvard and University of Arizona)
### https://openalex.org/W4401226694 author Renu Malhotra has two affiliations. 
oa_fetch_test1 <-oa_fetch( entity="works",  id="https://openalex.org/W4401226694")
oa_fetch_test1$author
view(oa_fetch_test1[[4]][[1]])

oa_fetch_test2 <-oa_fetch( entity="authors",  id="https://openalex.org/A5003933592")

#### This is not 100% accurate because UArizona has child organization whose ROR is associated with an article. By filtering institution_rorauthor
# to UArizona's ROR, certain articles are left out!!! 
# 2024-09: I am currently working with openAlexR developers to fix this. 
org_works_authors_ua <- org_works_authors%>%filter(institution_rorauthor== "https://ror.org/03m2x1q45")
org_works_authors_ua_unique <- unique (org_works_authors_ua)
duplicates <- org_works_authors_ua[duplicated(org_works_authors_ua), ]

# 3.32 
### Note: one article can be authored by multiple UA authors. However, the references cited are the same. 
### This data can study UA internal collaboration! 

### 3.33 Testing if a cited work is found. 
# Deep Learning, Nature, by Yann LeCun, Yoshua Bengio, Geoffrey Hinton. Cited by: 62,210
search_string <- "https://openalex.org/W2919115771"
result <- lapply(org_works_ref_combined, function(x) grep(search_string, x, value = TRUE))
print(result)
matches <- result[sapply(result, length) > 0]
print(matches)
indices <- which(sapply(org_works_ref_combined, function(x) any(grepl(search_string, x))))
for (i in indices) {
  cat("Index:", i, "\n")
  cat("Element:\n", org_works_ref_combined[[i]], "\n\n")
}
# Find it from the original article
search_string <- "https://openalex.org/W2594545996"  # this article was cited 81 times in 2019, cited 16 times in 2023 
indices_with_string <- which(sapply(org_works$referenced_works, function(x) search_string %in% x))
print(indices_with_string)
org_works[indices_with_string, ]$id

# test case 2: cited 6 from microbiology
# both final published version and pre-print existing: https://openalex.org/works/W4379795917 and https://openalex.org/W4319339791 
search_string <- "https://openalex.org/W2128159409"
indices_with_string <- which(sapply(org_works$referenced_works, function(x) search_string %in% x))
print(indices_with_string)
org_works[indices_with_string, ]$id


##### 3.34  Fetch time 
# the number of works to fetch at a time has little influence the time to run oa_fetch
# 2024-09: fetch_number = 1,000, reduced the total running time of 10% comparing to fetch_number 100
# 2024-09: fetching 241,000 works took 188 minutes
# optimize code: ... <to do> 
# 

########################################
### Testing optimization of rbind and oa_fetch
### 2024-09-21: 10,000 works in R old version (4.1.2): 270 seconds
### 2024-09-23: 10,000 works in R latest version (4.4.1): 112 seconds
install.packages("profvis")
library(profvis)

malaria_topic <- oa_fetch(entity = "topics", search = "malaria") %>% 
  filter(display_name == "Malaria") %>% 
  pull(id)
malaria_topic
#> [1] "https://openalex.org/T10091"
system.time({
  res <- oa_fetch(
    topics.id = malaria_topic,
    entity = "works",
    verbose = TRUE,
    options = list(sample = 10000, seed = 1),
    output = "list"
  )
})

rm(res)

fetch_number <- 100
num_of_works <- 10000
### The only difference from the above oa_fetch is the topics.id vs. id
## maybe it is my network?? 
# 2024-09-23: 10,000 works: 1.7GB data (my internet 90M/b about 155 second to download) : 766 seconds (real time): TBD in UA

res <-list()
#profvis({
system.time({
  batch_identifiers <-org_works_ref_unique[1:num_of_works]
  res <-oa_fetch(identifier=batch_identifiers, 
                 entity = "works",
                 options= list(sample=fetch_number, seed=1), 
                 output="list")
})

library(data.table)
fetch_number <- 50
# num_of_works <- length (org_works_ref_unique)
num_of_works <- length (org_works_ref_combined)

range_i <- seq(1, num_of_works, by=fetch_number)
works_cited_ls <- vector("list", length = length(range_i))
time_taken <-system.time({
  for (idx in seq_along(range_i)) {
    i <- range_i[idx]
    batch_identifiers <-org_works_ref_unique[i:min(i+fetch_number-1, num_of_works)]
    batch_data <-oa_fetch(entity="works", identifier=batch_identifiers, 
                          options= list(sample=fetch_number, seed=1), output="list")
    works_cited_ls[[idx]] <- batch_data
  }
})
print(paste("fetch time: ", time_taken["elapsed"] / 60, "minutes"))

### convert list to DF with measuring the performance  
time_taken2 <- system.time ({
  works_cited <- rbindlist(works_cited_ls, use.names=TRUE, fill=TRUE) 
})
print(paste("rbind time: ", time_taken2["elapsed"] / 60, "minutes"))

#########################

 time_taken <- system.time({ 
  for(i in seq(1, num_of_works, by=fetch_number)){
    batch_identifiers <-org_works_ref_combined[i:min(i+fetch_number-1, num_of_works)]
    batch_data <-oa_fetch(identifier=batch_identifiers)
    works_cited<-rbind(works_cited, batch_data)
  }
})
print(paste("time to run: ", time_taken["elapsed"] / 60, "minutes"))

########################

### Count how many multiple cited. 
class(org_works_ref_more_cited)
# Count the occurrences of each unique element in the vector
#works_ref_more_cited_counts <- table(org_works_ref_more_cited)
works_cited <- org_works_ref_combined
#### Step 1: Re-generate a new row if it matches (meaning; cited multiple times.)
works_cited_final <- works_cited

#### need to recheck the numbers
# Step 2: Add these matching rows as new rows 
# matching_rows <- works_cited[works_cited$id %in% names(works_ref_more_cited_counts), ]
# Step 3: Repeat each row in the DataFrame based on the count in org_works_ref_more_cited
# matching_rows_expanded <- matching_rows[rep(1:nrow(matching_rows), times = works_ref_more_cited_counts[matching_rows$id]), ]

#matching_rows <- works_cited[works_cited$id %in% names(citation_counts), ]
# Step 3: Repeat each row in the DataFrame based on the count in org_works_ref_more_cited
#matching_rows_expanded <- matching_rows[rep(1:nrow(matching_rows), times = citation_counts[matching_rows$id]), ]

# Step4: We have the final works cited, including multiple occurances of a work
# works_cited_final <- rbind(works_cited_final, matching_rows_expanded)


### Questions: 
# 1. I fetched 316,401 unique works, but returned 329,720 (about 2% more... )
# 2. 

############# Testing

difference_df1_df2 <- setdiff(works_cited_final$id, org_works_ref_combined)
difference_df2_df1 <- setdiff(org_works_ref_combined, works_cited_final$id)
head(difference_df2_df1)
head(works_cited_final$id)
head(matching_rows$id)

index <- which(works_cited_final$id == "https://openalex.org/W1927648166")
print(index)
works_cited_final$id[1]
works_cited_final$id[328131]

index <- which(matching_rows$id == "https://openalex.org/W1927648166")
print(index)

index <- which(matching_rows_expanded$id == "https://openalex.org/W1927648166")
print(index)

index <- which(works_ref_more_cited_counts == "https://openalex.org/W1927648166")
print(index)

######################


saveRDS(works_cited_final, "../works_cited_final_2023.rds")

works_cited_final <- readRDS("../works_cited_final_2019.rds")
# works_cited_final <- readRDS("../works_cited_final_2020.rds")
# works_cited_final <- readRDS("../works_cited_final_2021.rds")
# works_cited_final <- readRDS("../works_cited_final_2022.rds")
works_cited_final <- readRDS("../works_cited_final_2023.rds")


###################### Citation Analysis ####################################
# 1. Analyse journal usage
#  - remove any row whose col "issn_l" is empty or NULL 
# 2023: 329,389 articles out of 352,509 works: 94%
# 2022: 382,495 articles out of 421,866 works: 91%
# 2019: 291,705 articles out of 323,779 works: 90%

articles_cited <- works_cited_final[!(is.na(works_cited_final$issn_l)), ]
articles_cited <- articles_cited[!(is.na(articles_cited$issn_l) | articles_cited$issn_l == ""), ]
nrow(articles_cited)

# saveRDS(articles_cited, "../articles_cited_2019.rds")

# Trim and normalize the host_organization column
articles_cited$host_organization <- trimws(articles_cited$host_organization)
articles_cited$issn_l <- trimws(articles_cited$issn_l)

# Empty or NULL records
count_null_empty_id <- sum(is.na(articles_cited$id) | trimws(articles_cited$id) == "")
count_null_empty_id


# publisher: host_organization
unique_publishers <- unique(articles_cited$host_organization)
# number of publishers: ~1,600
num_unique_publishers <- length(unique_publishers)
# list top 50 publishers
print(unique_publishers[1:50])
# list NULL publishers = 5%
num_na <- sum(is.na(articles_cited$host_organization))

# Replace NA values and empty strings with "NA"
articles_cited$host_organization[is.na(articles_cited$host_organization) | trimws(articles_cited$host_organization) == ""] <- "NA"

# Dealing with "NA" data in "host_organization" field.
# 1. First, showing all NA publisher: meaning publisher info is not available. 
publisher_NA <- articles_cited[articles_cited$host_organization == "NA", ]

publisher_NA_id <-unique(publisher_NA$id)
# Check if any row in the df 'publisher_NA' contains a non-missing value in the "issn_l" column
publisher_NA_with_issn <- publisher_NA[!is.na(publisher_NA$`issn_l`) & publisher_NA$`issn_l` != "", ]
print(publisher_NA_with_issn)

# Extract unique ISSNs from the 'issn_l' column: 1235 unique issns
unique_issn <- unique(publisher_NA$`issn_l`)
print(unique_issn)



# Check if any 'id' values are duplicated
any_duplicated_ids <- any(duplicated(publisher_NA$id))


# Not using unnect() because it flattens out every article per author, which creates a lot of duplicated info
library(jsonlite)

# Convert the 'author' dataframe to JSON for each row
publisher_NA <- publisher_NA %>%
  mutate(author = sapply(author, function(x) toJSON(x)))

# Truncate only strings that exceed Excel's 32,767 character limit
publisher_NA <- publisher_NA %>%
  mutate(across(where(is.character), ~ ifelse(nchar(.) > 32767, substr(., 1, 32767), .)))

publisher_name <- "Microbiology society"
publisher_microbiology <- articles_cited[grepl(publisher_name, articles_cited$host_organization, ignore.case = TRUE), ]

publisher_elsevier <- articles_cited[grepl("Elsevier BV", articles_cited$host_organization, ignore.case = TRUE), ]

publisher_springer <- articles_cited[tolower(articles_cited$host_organization) == tolower("Springer Science+Business Media"), ]
#publisher_springer <- articles_cited[grepl("Springer Science+Business Media", articles_cited$host_organization, ignore.case = TRUE), ]

publisher_plos <- articles_cited[grepl("Public Library of Science", articles_cited$host_organization, ignore.case = TRUE), ]

publisher_aaas <- articles_cited[grepl("American Association for the Advancement of Science", articles_cited$host_organization, ignore.case = TRUE), ]

publisher_nature <- articles_cited[grepl("Nature Portfolio", articles_cited$host_organization, ignore.case = TRUE), ]

publisher_cdc <- articles_cited[grepl("Centers for Disease Control and Prevention", articles_cited$host_organization, ignore.case = TRUE), ]

# University of Arizona
publisher_ua <- articles_cited[grepl("University of Arizona", articles_cited$host_organization, ignore.case = TRUE), ]
publisher_uap <- articles_cited[grepl("University of Arizona Press", articles_cited$host_organization, ignore.case = TRUE), ]

# Need to study more. 276 cited (2023), 2019
publisher_emerald <- articles_cited[grepl("Emerald Publishing", articles_cited$host_organization, ignore.case = TRUE), ]

### origin works: test case: 

### Test cases for AAAS
search_string <- "https://openalex.org/W2083070320"

id_counts <-table(publisher_elsevier$id)
duplicateds <- id_counts[id_counts > 60]
print(duplicateds)

id_counts <-table(publisher_aaas$id)
duplicateds <- id_counts[id_counts > 10]
print(duplicateds)

### Test cases for PLOS
search_string <- "https://openalex.org/W2125300654"
id_counts <-table(publisher_plos$id)
duplicateds <- id_counts[id_counts > 10]
print(duplicateds)

###############################################################
# Verify any cited work using the function search_references()
# Define the function to search for a string in the referenced_works column and print the output
##############################################3
search_references <- function(search_string, df) {
  indices_with_string <- which(sapply(df$referenced_works, function(x) search_string %in% x))
  print(indices_with_string)
  print(df[indices_with_string, ]$id)
}

# Example usage
search_string <- "https://openalex.org/W1604958295" 
# openAlex questions: type = "book-chapter", while it has ISSN

search_string <- "https://openalex.org/W1607198972"
indices_with_string <- which(sapply(org_works$referenced_works, function(x) search_string %in% x))

search_references(search_string, org_works)

### Test cases for Microbiology
# both final published version and pre-print existing: https://openalex.org/works/W4379795917 and https://openalex.org/W4319339791 
# Org_works ids are: "https://openalex.org/W4379795917" "https://openalex.org/W4317888776" "https://openalex.org/W4385752148" "https://openalex.org/W4319339791" "https://openalex.org/W4323537660"
# "https://openalex.org/W4323309440"
search_string <- "https://openalex.org/W2128159409"  # Microbiology articles
search_string <- "https://openalex.org/W2017185349" # Microbiology


# Publishers test case : cited 6 from microbiology
# both final published version and pre-print existing: https://openalex.org/works/W4379795917 and https://openalex.org/W4319339791 
publisher_article_indicies <- which(sapply(publisher_microbiology$id, function(x) search_string %in% x))
print(publisher_article_indicies)
publisher_microbiology[publisher_article_indicies, ]$id

# Test case: cited 47 times in 2023. Verified! 
# The Gaia mission
search_string <- "https://openalex.org/W147232447"

# Test case: cited > 80 times in 2019. verified. 18 times in 2023
# https://openalex.org/W2066340221 cited > 80 times in 2019.
search_string <- "https://openalex.org/W2066340221"


search_references(search_string, org_works)

########################################################################
###################### End of Testing ##################################
########################################################################

#### Find duplicates and frequencies #####
# change DF here
df <-articles_cited
# Find the rows that are duplicated
duplicate_rows <- df[duplicated(df) | duplicated(df, fromLast = TRUE), ]
# Create a table to count the frequency of duplicated rows
#duplicate_frequency <- table(apply(duplicate_rows, 1, paste, collapse = "-"))
duplicate_frequency <- table(duplicate_rows$id)
# show more than 10 times cited. change "10" to any number
duplicate_ids <- names(duplicate_frequency[duplicate_frequency > 10])

duplicate_multi_cited_rows <- df[df$id %in% duplicate_ids, ]

duplicate_multi_cited_rows <- duplicate_multi_cited_rows %>%   
  mutate(across(where(is.character), ~ ifelse(nchar(.) > 32767, substr(., 1, 32767), .)))

# Remove duplicate rows from duplicate_multi_cited_rows
duplicate_multi_cited_rows_unique <- duplicate_multi_cited_rows[!duplicated(duplicate_multi_cited_rows), ]

write_xlsx(duplicate_multi_cited_rows, "citations/duplicate_multi_cited_2023.xlsx")
write_xlsx(duplicate_multi_cited_rows_unique, "citations/duplicate_multi_cited_unique_2023.xlsx")

# Save the modified dataset to Excel
write_xlsx(publisher_NA, "citations/publisher_NA_2019.xlsx")

write_xlsx(publisher_aaas, "citations/publisher_aaas_2019.xlsx")
write_xlsx(publisher_nature, "citations/publisher_nature_2019.xlsx")

write_xlsx(publisher_plos, "citations/publisher_plos_2019.xlsx")
write_xlsx(publisher_microbiology, "citations/publisher_microbiology_2019.xlsx")

write_xlsx(publisher_emerald, "citations/publisher_emerald_2019.xlsx")
######################################
######################################
### Function: To count journal occurrences for a given publisher
# @param: dataframe articles_cited
#          publisher_name
# return: journal and counts cited 
count_journals_by_publisher <- function(articles_cited, publisher_name) {
  # Filter rows where host_organization matches the specified publisher
  publisher1 <-  articles_cited[grepl(publisher_name, articles_cited$host_organization, ignore.case = TRUE), ]
  
  # Count the occurrences of each journal under the specified publisher
  journal_counts <- table(publisher1$so)
  journal_counts_df <- as.data.frame(journal_counts)
  
  return(journal_counts_df)
}



publisher_name <- "Microbiology society"
publisher1 <-  articles_cited[grepl(publisher_name, articles_cited$host_organization, ignore.case = TRUE), ]
journal_counts_df <- count_journals_by_publisher(articles_cited, publisher_name)
print(journal_counts_df)
# Note: Errors
# https://openalex.org/W2165027548 (1994 v44n3, Journal name changes and ISSN changed)


publisher_name <- "Optica Publishing Group"
publisher1 <-  articles_cited[grepl(publisher_name, articles_cited$host_organization, ignore.case = TRUE), ]
journal_counts_df <- count_journals_by_publisher(articles_cited, publisher_name)
print(journal_counts_df)

publisher_name <- "Canadian Science Publishing"
publisher1 <-  articles_cited[grepl(publisher_name, articles_cited$host_organization, ignore.case = TRUE), ]
journal_counts_df <- count_journals_by_publisher(articles_cited, publisher_name)
print(journal_counts_df)

publisher_name <- "Emerald Publishing"
publisher1 <-  articles_cited[grepl(publisher_name, articles_cited$host_organization, ignore.case = TRUE), ]
journal_counts_df <- count_journals_by_publisher(articles_cited, publisher_name)
print(journal_counts_df)
write_xlsx(journal_counts_df, "citations/publisher_emerald_2019_counts.xlsx")

search_references("https://openalex.org/W1584410718", org_works)


# Group by 'host_organization' and count the number of articles for each publisher
publisher_ranking <- articles_cited %>%
  group_by(host_organization) %>%
  summarise(article_count = n()) %>%
  arrange(desc(article_count))

# Calculate the total number of articles across all publishers
total_article_count <- sum(publisher_ranking$article_count)

# Calculate the percentage for each publisher relative to the total article count
publisher_ranking <- publisher_ranking %>%
  mutate(percentage = (article_count / total_article_count) * 100)

library(ggplot2)
top_20_publishers <- publisher_ranking %>% slice(1:20)
top_20_publishers$percentage <- (top_20_publishers$article_count / total_article_count) * 100
top_20_publishers$host_organization <- substr(top_20_publishers$host_organization, 1, 10)

# top 50
top_50_publishers <- publisher_ranking %>% slice(1:50)
top_50_publishers$percentage <- (top_50_publishers$article_count / total_article_count) * 100
top_50_publishers$host_organization <- substr(top_50_publishers$host_organization, 1, 10)

# top 100
top_100_publishers <- publisher_ranking %>% slice(1:100)
top_100_publishers$percentage <- (top_100_publishers$article_count / total_article_count) * 100
top_100_publishers$host_organization <- substr(top_100_publishers$host_organization, 1, 10)


# Bar plot for top 20 publishers
ggplot(top_20_publishers, aes(x = reorder(host_organization, -article_count), y = article_count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  # Real number (article count) inside the bar
  geom_text(aes(label = article_count), vjust = 0.5, hjust = 1.2, size = 2.5, color = "white") +  
  # Adjust hjust and color for positioning inside
  # Percentage outside the bar
  geom_text(aes(label = sprintf("(%.1f%%)", percentage)), vjust = 0.5, hjust = -0.2, size = 3) +  
  # Adjust hjust for positioning outside
  coord_flip() +  # Flip the axis for better readability
  labs(x = "Publisher", y = "Number of Articles", title = "2019 UA Top 20 Publishers (Number of Articles Cited)") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 7))  # Reduce font size of publisher names

# Calculate the percentage of the top 20, top 50, and top 100 publishers over the total
total_article_count <- sum(publisher_ranking$article_count) # Total articles in all publishers
top_20_total_count <- sum(top_20_publishers$article_count)  
top_50_total_count <- sum(top_50_publishers$article_count)  
top_100_total_count <- sum(top_100_publishers$article_count)  

# Calculate the percentage
# Top 20: 76%
# Top 50: 90.4%
# Top 100: 95.4%
top_20_percentage_of_total <- (top_20_total_count / total_article_count) * 100
top_50_percentage_of_total <- (top_50_total_count / total_article_count) * 100
top_100_percentage_of_total <- (top_100_total_count / total_article_count) * 100

# Print the result
print(paste("Top 20 publishers represent", round(top_20_percentage_of_total, 2), "% of the total articles."))
print(paste("Top 50 publishers represent", round(top_50_percentage_of_total, 2), "% of the total articles."))
print(paste("Top 100 publishers represent", round(top_100_percentage_of_total, 2), "% of the total articles."))

view(publisher_ranking)
# View the top 50 publishers.  
# Top 10: Elsevier (20%), Wiley (9%), Oxford University Press (7%), ICP (5%), Springer(5%), Nature,
# IOP Publishing, Lippincott Williams & Wilkins, Taylor & Francis, SAGE Publishing (2%)

################### Analyze top journals for each publisher ############
# Function to rank top cited journals
# Usage example:
#rank_top_cited_journals(publisher_nature, "so", 10)  # Top 10 cited journals

rank_top_cited_journals <- function(data, journal_col, top_n = 10) {
  top_cited_journals <- data %>%
    group_by(!!sym(journal_col)) %>%      # Group by the journal names (column provided by the user)
    summarise(citation_count = n()) %>%   # Count the number of articles per journal
    arrange(desc(citation_count)) %>%     # Sort by citation count in descending order
    slice(1:top_n)                        # Select top 'n' journals
  
  print(top_cited_journals, n = top_n)
}

rank_top_cited_journals(publisher_plos, "so")
rank_top_cited_journals(publisher_aaas, "so")
rank_top_cited_journals(publisher_nature, "so")

