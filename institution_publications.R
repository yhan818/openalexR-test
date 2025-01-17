############# Institution authors publication analysis and Collection Management ##########
######## Author: Yan Han with help of ChatGPT 4
######## Updated: Dec 14, 2024
######## Updated: Fixed NA issue with host_organization
##### Search an institution authors' publication using openAlex data ####
# OpenAlex R Documentation: https://github.com/ropensci/openalexR

install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages('data.table')

install.packages("openalexR")
install.packages("remotes")
# remotes::install_github("ropensci/openalexR", force=TRUE) 

library(openalexR)
packageVersion("openalexR")

library(dplyr)
library(tidyverse)
library(data.table)
library(writexl)
# free unused obj to manage memory
gc()
rm(list=ls())
gc()

options("max.print" = 100000)

options (openalexR.mailto="yhan@arizona.edu")
getwd()
setwd("/home/yhan/Documents/openalexR-test/")

##### 1. Getting data. (retrieved 2024-09-02)
# Retrieving all publications association with UArizona's ROR (Research Organization Registry) ID.
# UA publications per year is ~9,000. For running 2 years data, need better computer or crashed R studio.
# Year 2023: 10,559 works (2025-01) <<< 9,384 works (2024-10)
# Year 2022: 8,833 works (2024-10-18) <<< 8,674 works (2024-09-09)
# Year 2021: 9,323 works (7,048 type-journal articles and reviews)
# Year 2020: 
# Year 2019: 8,847 
# 2023-current: 14,660 works : 5 min to get UAworks with 3 GB mem, 264 mins to pull 372,000 reference's data with 8.6 GB  
# 2022-current: 23,360 works: 10 mins to get UAWorks with 6 GB RAM, 450 mins to pull 560,000 citedWorks's data with 12 GB. crashed R studio.
# 2020-current: 
# 2014-current: 86,000 works : 15 mins to run, and used 7GB RAM. 
# 2013-current: 50,000 records: 

### 1.1 Getting the count only. This is the quick way to find out the total number of works. 
### There are two type:
####### a) Any type (broader): journals, repositories (PubMed, arXiv etc). 
####### b) limited to "journal" type only by: #primary_location.source.type = "journal",

# Typically only some seconds
UAworks_count <-oa_fetch(
  entity="works",
  institutions.ror=c("03m2x1q45"),
  from_publication_date ="2023-01-01",
  to_publication_date = "2023-12-31",
  #primary_location.source.type = "journal",
  count_only = TRUE
)

### 1.2 Getting all the works based on the institution ROR and publication date. It takes longer time. 
# see above for the running time
# 2019 
org_works_2019 <-oa_fetch(
  entity="works",
  institutions.ror=c("03m2x1q45"),
  from_publication_date ="2020-01-01",
  to_publication_date = "2020-01-01"
)

org_works_2021 <-oa_fetch(
  entity="works",
  institutions.ror=c("03m2x1q45"),
  from_publication_date ="2021-01-01",
  to_publication_date = "2021-12-31",
  # primary_location.source.type = "journal"
)

org_works_2022 <-oa_fetch(
  entity="works",
  institutions.ror=c("03m2x1q45"),
  from_publication_date ="2022-01-01",
  to_publication_date = "2022-12-31",
  # primary_location.source.type = "journal"
)

# SHALL get all works, then filter them if needed. 
# 2023: All works: 9,384 without type =journal (2024-09) 
# 2023: All works: 10,559 (2025-01) 
# 2023: journal only: 6,903 using primary_location.source.type = "journal" as a filter (not including type="repository")
# 
org_works_2023 <-oa_fetch(
  entity="works",
  institutions.ror=c("03m2x1q45"),
  from_publication_date ="2023-01-01",
  to_publication_date = "2023-12-31",
  #primary_location.source.type = "journal"
)

# Save data
# saveRDS(org_works_2019, "../org_works_2019.rds")
# saveRDS(org_works_2020, "../org_works_2020.rds")
saveRDS(org_works_2021, "../org_works_2021.rds")
# saveRDS(org_works_2022, "../org_works_journal_2022.rds")
saveRDS(org_works_2023, "../org_works_2023.rds")

# Load data 
org_works_2019 <- readRDS("../org_works_2019.rds")
org_works <- org_works_2019

org_works_2020 <- readRDS("../org_works_2020.rds")
org_works <- org_works_2020

org_works_2021 <- readRDS("../org_works_2021.rds")
org_works_2021_journal <- readRDS("../org_works_journal_2021.rds")
org_works <- org_works_2021

org_works_2022 <- readRDS("../org_works_2022.rds")
org_works <- org_works_2022

org_works_2023 <- readRDS("../org_works_2023.rds")
# to filter "journal" works only. I feel it shall not be this restrict. (other works like grey literature are good too)
org_works <- org_works_2023


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
# "type" is "source.type" ??? 
# Year 2019: 1575 / 8848 referenced works value="NA", while $type is "article". 18%
# Year 2020: 1868 / 10161 referenced works value="NA", while $type is "article". 
# Year 2021: 1921 / 9336 referenced works value="NA", while $type is "article". 
# Year 2022: 1224 / 8674  referenced works value="NA", while $type is "article". 
# Year 2023: 1534 / 9384 referenced works value="NA", while $type is "article". 
# 2023: 1217 / 6889 published article, primary_location_type = journal, $type = article: 17%

# There are NA references. So we need to remove them. 
# ~14% works have no references (2021)
# This na_indices include type: article, books, errata, letter, and other types
na_indices <- which(sapply(org_works_ref, function(x) is.logical(x) && is.na(x))) 
na_count <- sum(sapply(org_works_ref, function(x) is.logical(x) && is.na(x)))
na_percent <- na_count/length(org_works_ref) * 100

# Remove duplicate rows from the data frame
unique_org_works <- unique(org_works)
org_works_ref <- unique(org_works_ref) # this actually also remove NA lists.

# Filter the rows where $reference_works is NA and $type is "article"
works_na_referenced_works <- org_works %>%
  filter(is.na(referenced_works) & type == "article")

#write_xlsx(works_na_referenced_works, "citations/works_journal_2023_na_referenced_works.xlsx") # send this to OpenAlex

### 2.2 Combine all the references and do further data analysis
# Avg # of references per article: ~50
# Year 2023 total references: 364,304: total journal article: 308,359:  unique 281,470 / 351,479: more cited: ~77,000 
# Year 2022 total references: 354,355: 
# Year 2021 total references: 382,965: 
# Year 2020 total references: 392,992: article 
# Year 2019 total references: 352,509: articles 329,000  

# rm(org_works_ref_combined)
org_works_ref_combined <- unlist(org_works_ref, use.names = FALSE)
org_works_ref_combined <- org_works_ref_combined[!is.na(org_works_ref_combined)]  # Remove NA values

### 2.21 finding these duplicates, which mean the duplicates have been cited multiple times 
# (probably more important to have these journals subscribed!)
# cited more: ~20% - 25%  (2019, 2020, 2021, 2022, 2023 UArizona data)
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
# 21 times (2020); 22 times(2021), 26 times(2022), 18 times(2023)
# https://openalex.org/W4247665917 were cited in 2019, 2021, 2022 and 2023 data
index <- which(org_works_ref_combined == "https://openalex.org/W4247665917")
print(index)

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

matches <- result[sapply(result, length) > 0]
indices <- which(sapply(org_works_ref_combined, function(x) any(grepl(search_string, x))))
for (i in indices) {
  cat("Index:", i, "\n")
  cat("Element:\n", org_works_ref_combined[[i]], "\n\n")
}

# Find it from the original article
search_string <- "https://openalex.org/W2594545996"  
# this article was cited 81 (2019, 130 (2020), 90 (2021), 52 (2022), 16 (2023)
indices_with_string <- which(sapply(org_works$referenced_works, function(x) search_string %in% x))
print(indices_with_string)
org_works[indices_with_string, ]$id

# test case 2: cited 6 from microbiology, multiple times for 2019, 2020, 2021, 2022
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

#Creating an empty dataframe to store the results of the for loop.
works_cited <-data.frame()
#rm(work_cited_final)

# Getting these works' metadata. This takes long time to run. 
# Warnings(). a work > 100 authors will be truncated 
# 2024: 
# 2023: 352,509 (checked) out of 364,304 : article  / 308,359
# 2022: 345,813 (checked) : article / 325,520 (type = journal)
# 2021: 384,886 (checked) out of 384,886
# 2019: 331,657 (checked).
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


fetch_number <- 50
num_of_works <- length (org_works_ref_combined)

range_i <- seq(1, num_of_works, by=fetch_number)
works_cited_ls <- vector("list", length = length(range_i))

### Code has bugs??a lot of these have NA value???
time_taken <-system.time({
  for (idx in seq_along(range_i)) {
    i <- range_i[idx]
    batch_identifiers <-org_works_ref_combined[i:min(i+fetch_number-1, num_of_works)]
    batch_data <-oa_fetch(identifier=batch_identifiers, primary_location.source.type = "journal", )
                          #output="list", )
    works_cited_ls[[idx]] <- batch_data
  }
})
print(paste("fetch time: ", time_taken["elapsed"] / 60, "minutes"))

tail(works_cited_ls)

works_cited <- rbindlist(works_cited_ls, use.names=TRUE, fill=TRUE) 




#########################
# Ensure oa_fetch() is receiving the correct input and create a new dataframe for results.
works_cited <- data.frame()
works_cited2 <-data.frame()

fetch_number <- 50
#num_of_works <-1000
num_of_works <- length (org_works_ref_combined)

# Loop to fetch data in batches
time_taken <- system.time({
  for(i in seq(1, num_of_works, by = fetch_number)) {
    batch_identifiers <- org_works_ref_combined[i:min(i + fetch_number - 1, num_of_works)]
    
    # Check if the batch_identifiers is a valid vector
    if (length(batch_identifiers) > 0 && !all(is.na(batch_identifiers))) {
      # Fetch data from OpenAlex using oa_fetch, ensure proper identifier input
      batch_data <- tryCatch({
        # Have to use "primary_location.source.type = journal" to filter out non-journal.
        # issn_l cannot be used alone (there are book chapters which have issn per OpenAlex)
        oa_fetch(identifier = batch_identifiers) 
                 #, primary_location.source.type = "journal")
      }, error = function(e) {
        message("Error fetching data: ", e)
        return(NULL)
      })
     # Only bind non-null data
      if (!is.null(batch_data) && nrow(batch_data) >0 ) {
        # Ensure consistent columns
        batch_data <- data.table::setDT(batch_data)[, setdiff(names(works_cited), names(batch_data)) := NA]
        works_cited <- rbindlist(list(works_cited, batch_data), use.names = TRUE, fill = TRUE)
      }
    }
  }
})
print(time_taken)


head(works_cited)
setdiff(works_cited, works_cited2)
setdiff(works_cited2, works_cited)

######################################################
### There are two types of citations 
#
# 1. works_cited_final_2022.rds --> work cited by UA authors of any type publications (e.g. journal articles, journal reviews, reports, repository items)
############################################

# 2. works_cited_type_journal_2022.rds --> type = journal (work cited by UA authors of only journal type such as articles and reviews.  
# 

#### Step 1: Re-generate a new row if it matches (meaning; cited multiple times.)
works_cited_final <- works_cited

saveRDS(works_cited_final, "../works_cited_final_2021.rds")
saveRDS(works_cited, "../works_cited_type_journal_2023.rds")


saveRDS(works_cited_final, "../works_cited_2023.rds")

works_cited_final <- readRDS("../works_cited_final_2019.rds")
works_cited_final <- readRDS("../works_cited_final_2020.rds")
works_cited_final <- readRDS("../works_cited_final_2021.rds")
works_cited_final <- readRDS("../works_cited_type_journal_2022.rds")
                             #works_cited_final_2022.rds")


works_cited_final <- readRDS("../works_cited_final_2022.rds")

#works_cited_final <- readRDS("../works_cited_final_journal_2023.rds")

# One is primary.source.type = journal, the other (works_cited_final2) contains everything
# For year 2022, 325,520 : 345,813. 

### If not filtering by "primary_location:source=journal", there are more.
# For example, https://api.openalex.org/works/W2984048300 (source = null)

difference_df1_df2 <- setdiff(works_cited_final$id, works_cited_final2$id)
difference_df2_df1 <- setdiff(works_cited_final2$id, works_cited_final$id)
head(difference_df2_df1)

############# Testing
difference_df1_df2 <- setdiff(works_cited_final$id, org_works_ref_combined)
difference_df2_df1 <- setdiff(org_works_ref_combined, works_cited_final$id)
head(difference_df2_df1)
head(works_cited_final$id)
head(matching_rows$id)
######################


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
# 1. I fetched 354,355 unique works, returned 325,520 
# 2. 

# Count the occurrences of each unique element in the vector
#works_ref_more_cited_counts <- table(org_works_ref_more_cited)
# works_cited <- org_works_ref_combined


###################### Citation Analysis ####################################
# 1. Analyse journal usage
#  - remove any row whose col "issn_l" is empty or NULL 
# Date fetched: 2024-10 and 2024-12:

### Works consists of two main types: a) journal (from journals) and b) non-journals (book etc)
### (a) type = journal: the source is journal. Journal works consist of articles, reviews, conference papers.
### (b) type != journal: the source is non-journal. Non-journal works consist of articles, books, etc from other sources such as repositories (arXiv, PubMed) 

### works_cited (everything) = type_journal_works + type_non_journal_works
### type_journal_works (all works in journal) = journal_articles_cited + journal_non_articles_cited
### type_non_journal_works = non_journal_articles_cited + non_journal_non_articles_cited


# 2023: 353,424 (works_cited) = 330,005 (works_cited_issn) + 23,419 (works_cited_non_issn)
##### 330,005 (works_cited_issn) = 287,142 (works_cited_issn_article_cited = journal_articles_cited) + 42,863 (works_cited_issn_non_article_cited=journal_non_articles_cited) 
##### 23,419 (work_cited_non_issn) = 9,335 (non_journal_articles_cited, e.g. arXiv, PubMed, proceedings) + 14,084 (non_journal_non_articles_cited, e.g. preprint, book, book-chapter)


# 2022: 345,813 (work cited) = 323,221 (works_cited_issn) + 22,592 (works_cited_non_issn)
###### 323,221 (work_cited_issn) = 279,258 (works_cited_issn_article_cited = journal_articles_cited) + 43,963 (works_cited_issn_non_article_cited=journal_non_articles_cited)
###### 22,592 (work_cited_non_issn) = 8,571 (non_journal_articles_cited, e.g. arXiv, PubMed, proceedings) + 14,021 (non_journal_non_articles_cited, e.g. preprint, book, book-chapter)


# 2021: 374,067(work cited) = 341,738(works_with_issn) + 32,329 (non_issn)
######## 341,738 (type_journal_works)  = 297,819 (journal_articles_cited) + 43,919 (journal_non_articles_cited)
######## 32,329 (non_issn) = 13,150(non_journal_article) + 19,179 (non_journal_non_articles_cited)

# 2020: 382,495 articles out of 421,866 works: 91%
# 2019: 291,705 articles out of 323,779 works: 90%

# Filter rows where issn_l is neither NA nor an empty string
works_cited_issn_index <- !is.na(works_cited_final$issn_l) & works_cited_final$issn_l != ""
# Subset 
works_cited_issn <- works_cited_final[works_cited_issn_index, ]

# Subset non_articles_cited (the rest)
works_cited_non_issn <- works_cited_final[!works_cited_issn_index, ]


#############################
# Filter records where type is "article" (excluding conference paper etc )


journal_articles_cited <-     works_cited_issn[works_cited_issn$type == "article", ]
journal_non_articles_cited <- works_cited_issn[works_cited_issn$type != "article", ]

non_journal_articles_cited <-     works_cited_non_issn[works_cited_non_issn$type == "article", ]
non_journal_non_articles_cited <- works_cited_non_issn[works_cited_non_issn$type != "article", ]

difference_df1_df2 <- setdiff(articles_cited$id, articles_cited2$id)
difference_df2_df1 <- setdiff(works_cited_final2$id, works_cited_final$id)
head(difference_df1_df2)


# 2021: 
# Truncate strings in all character columns to 32,767 characters
non_articles_cited <- non_articles_cited %>%
  mutate(across(where(is.character), ~ ifelse(nchar(.) > 32767, substr(., 1, 32767), .)))

write_xlsx(non_articles_cited, "citations/non_articles_cited_2023.xlsx")

# Empty or NULL records
count_null_empty_id <- sum(is.na(works_cited_issn$id) | trimws(works_cited_issn$id) == "")
count_null_empty_id

# publisher: host_organization
unique_publishers <- unique(works_cited_issn$host_organization)
# number of publishers: ~1,600
num_unique_publishers <- length(unique_publishers)
# list top 50 publishers
print(unique_publishers[1:50])
# list NULL publishers ~ 1 %
# 2023: 2,227 (probably need ISSN matching) / 2,922 NA/

# 2022: 3,312 NA / 323,221
# 2021: 3,687 NA / 341,738 
# 2020: 4,039 NA / 382,495
num_na <- sum(is.na(works_cited_issn$host_organization))

# Replace NA values and empty strings with "NA"
works_cited_issn$host_organization[is.na(works_cited_issn$host_organization) | trimws(works_cited_issn$host_organization) == ""] <- "NA"

# Dealing with "NA" data in "host_organization" field.
# 1. First, showing all NA publisher: meaning publisher info is not available. 
publisher_NA <- works_cited_issn[works_cited_issn$host_organization == "NA", ]

publisher_NA_id <-unique(publisher_NA$id)
# Check if any row in the df 'publisher_NA' contains a non-missing value in the "issn_l" column
publisher_NA_with_issn <- publisher_NA[!is.na(publisher_NA$`issn_l`) & publisher_NA$`issn_l` != "", ]
print(publisher_NA_with_issn)

# Extract unique ISSNs from the 'issn_l' column: 1235 unique issns
# 2023: 1,236 / 3,489 NA
# 2022: 1,110 / 3,312 NA
# 2021: 1,204 / 3,687 NA
# 2020: 1,737 / 4,039 NA 
unique_issn <- unique(publisher_NA$`issn_l`)
print(unique_issn)

# Not using unnect() because it flattens out every article per author, which creates a lot of duplicated info
library(jsonlite)

# Convert the 'author' dataframe to JSON for each row
publisher_NA <- publisher_NA %>%
  mutate(author = sapply(author, function(x) toJSON(x)))

# Truncate only strings that exceed Excel's 32,767 character limit
publisher_NA <- publisher_NA %>%
  mutate(across(where(is.character), ~ ifelse(nchar(.) > 32767, substr(., 1, 32767), .)))

publisher_name <- "Microbiology society"
publisher_microbiology <- works_cited_issn[grepl(publisher_name, works_cited_issn$host_organization_name, ignore.case = TRUE), ]

publisher_elsevier <- works_cited_issn[grepl("Elsevier BV", works_cited_issn$host_organization_name, ignore.case = TRUE), ]

publisher_springer <- works_cited_issn[tolower(works_cited_issn$host_organization_name) == tolower("Springer Science+Business Media"), ]
#publisher_springer <- works_cited_issn[grepl("Springer Science+Business Media", works_cited_issn$host_organization, ignore.case = TRUE), ]

publisher_plos <- works_cited_issn[grepl("Public Library of Science", works_cited_issn$host_organization_name, ignore.case = TRUE), ]

publisher_aaas <- works_cited_issn[grepl("American Association for the Advancement of Science", works_cited_issn$host_organization_name, ignore.case = TRUE), ]

publisher_nature <- works_cited_issn[grepl("Nature Portfolio", works_cited_issn$host_organization_name, ignore.case = TRUE), ]

# University of Arizona
publisher_ua  <- works_cited_issn[grepl("University of Arizona",       works_cited_issn$host_organization, ignore.case = TRUE), ]
publisher_uap <- works_cited_issn[grepl("University of Arizona Press", works_cited_issn$host_organization, ignore.case = TRUE), ]

# Need to study more. 
# Emerald: cited (yyyy): 395 (2020), 257 (2021), 322 (2022), 276 (2023), 
publisher_emerald <- works_cited_issn[grepl("Emerald Publishing", works_cited_issn$host_organization, ignore.case = TRUE), ]

# IWA: cited (yyyy): 19 (2019), 34 (2020), 21 (2021), 19 (2022),   
publisher_iwa <- works_cited_issn[grepl("IWA Publishing", works_cited_issn$host_organization_name, ignore.case = TRUE), ]

id_counts <-table(publisher_iwa$id)
duplicateds <- id_counts[id_counts >= 1]
print(id_counts)

### Cell press
publisher_cell_press <- works_cited_issn[grepl("Cell Press", works_cited_issn$host_organization, ignore.case = TRUE), ]

publisher_cell_press_unique <- unique(publisher_cell_press)
df <-publisher_cell_press
# Use table to count occurrences of each duplicated row
row_counts <- as.data.frame(table(apply(df, 1, paste, collapse = "-")))
duplicates_with_counts <- row_counts[row_counts$Freq > 1, ]


### origin works: test case: 
difference_df1_df2 <- setdiff(publisher_emerald$id, publisher_emerald2$id)
head(difference_df1_df2)
difference_df2_df1 <- setdiff(publisher_emerald2$id, publisher_emerald$id)
head(difference_df2_df1)

# Find IDs common to both publisher_emerald and publisher_emerald2. 225 
common_ids <- intersect(publisher_emerald$id, publisher_emerald2$id)
head(common_ids)


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
 

# APS: 
# 2023: journal (article, review): 166; Non-journal (book-chapter): 0
# 2022: journal (article, review): 230; Non-journal (book-chapter): 2
# 2021: journal (article, review) : 170; Non-journal (book-chapter) : 2

publisher_aps  <- works_cited_issn[grepl("American Phytopathological Society", works_cited_issn$host_organization, ignore.case = TRUE), ]
publisher_aps2 <- works_cited_non_issn[grepl("American Phytopathological Society", works_cited_non_issn$host_organization, ignore.case = TRUE), ]

# BMJ:
# 2023: journal (article, review): 1,694; Non-journal: 0
# 2022: journal (article, review):    ; Non-journal: 0
# 2021: journal (article, review):    ; Non-journal: 0

publisher_bmj  <- works_cited_issn[grepl("BMJ", works_cited_issn$host_organization, ignore.case = TRUE), ]
publisher_bmj2 <- works_cited_non_issn[grepl("BMJ", works_cited_non_issn$host_organization, ignore.case = TRUE), ]



###########################################
### Search if a publisher is in a DF
# Output the publisher 
# @ return: the indices of the publisher
search_publisher <- function(publisher_string, df) {
  # Find indices where the host_organization contains the publisher string (case insensitive)
  indices_with_string <- which(grepl(publisher_string, df$host_organization, ignore.case = TRUE))
  
  print(df[indices_with_string, ]$host_organization)
  print(df[indices_with_string, ]$id)
  return(indices_with_string)
}

# Example usage:
publisher_string <- "Emerald Publishing"
result_indices <- search_publisher(publisher_string, works_cited_issn)

# Print the indices
print(result_indices)

#### Function: search_work_publisher(): 
## Search a work's publisher and output the publisher
# @return: index of the DF
search_work_publisher <- function(search_string, df) {
  # Find indices where the host_organization contains the search string (case insensitive)
  indices_with_string <- which(sapply(df$id, function(x) !is.na(x) && search_string %in% x))
  
  print(df[indices_with_string, ]$host_organization)
  print(indices_with_string)
  return(indices_with_string)
}

# Example usage:
search_string <- "https://openalex.org/W2963276645"
result_indices <- search_work_publisher(search_string, org_works)

###############################################################
# Verify any cited work using the function search_references()
# Define the function to search for a string in the referenced_works column and print the output
##############################################3
search_references <- function(search_string, df) {
  indices_with_string <- which(sapply(df$referenced_works, function(x) search_string %in% x))
  print(indices_with_string)
  print(df[indices_with_string, ]$id)
}

# Example usage:
search_string <- "Emerald Publishing"
result_indices <- search_publisher(search_string, org_works)
print(result_indices)

# Example usage
search_string <- "https://openalex.org/W1604958295" 
# openAlex questions: type = "book-chapter", while it has ISSN

search_string <- "https://openalex.org/W1607198972"
#search_string <- "https://openalex.org/W3216054981"
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

# Test case: 
# cited 32 times (2020), 53(2022), 47(2023)
# The Gaia mission
search_string <- "https://openalex.org/W147232447"
search_references(search_string, org_works)

# Test case: 
# cited: >80 (2019), 123 times (2020), 40 (2022), 16 (2023)
# https://openalex.org/W2066340221 cited > 80 times in 2019.
search_string <- "https://openalex.org/W2066340221"
search_references(search_string, org_works)

# Test case: Emerald (2022)
search_string <- "https://openalex.org/W1998245073"
search_string <- "https://openalex.org/W2508822998" # (3 times), 2(2023)
search_string <- "https://openalex.org/W1607198972"
search_string <- "https://openalex.org/W2011490204"
search_references(search_string, org_works)


# Test case: IWA. 1 (2022)
search_string <- "https://openalex.org/W2045185088"
search_publisher("IWA", org_works) # UA author published in IWA in 2014. not in 2019, 2020, and 2021

search_string <- "https://openalex.org/W2130109162"
search_string <- "https://openalex.org/W1965549985"

search_references(search_string, org_works)

# https://openalex.org/W2130109162 same record, different publication date? 
matches <- which(tolower(works_cited_issn$id) == tolower(search_string))
view(works_cited_issn[matches, ])
print(works_cited_issn$id[matches])

# Test case: Cell Press(2023)
# UA authors publish in Cell Press journals
search_publisher("Cell Press", org_works)

## search these cell press journals articles do UA authors cited.
search_string <- "https://openalex.org/W2511428910"
search_string <- "https://openalex.org/W2125987139"
search_stirng <- "https://openalex.org/W2002490399"
search_references(search_string, org_works)

# Test case: APS (2021)
# UA authors publish the journals
search_publisher("American Phytopathological Society", org_works)

### Test data for APS: 2024-12
## 2021: search journals articles do UA authors cited.
search_string <- "https://openalex.org/W2070851128"
search_string <- "https://openalex.org/W2125987139"

# 2022
search_string <- "https://openalex.org/W2088715433"  # 2 times
search_string <- "https://openalex.org/W2057480435"  # 3 times

# 2023 
search_string <- "https://openalex.org/W2802507504" # 3 times
search_string <- "https://openalex.org/W4226087454" # 4 times 
search_references(search_string, org_works)
# UA authors publish the journals
search_publisher("American Phytopathological Society", org_works)

### Test data for BMJ: 2025-01
## 2021: search journals articles do UA authors cited.
search_string <- ""

# 2022
search_string <- ""
search_string <- ""  # 3 times

# 2023 
search_string <- "https://openalex.org/W1967057044" # 4 times
search_string <- "https://openalex.org/W2157823046" # 7 times 
search_references(search_string, org_works)
#
search_publisher("American Phytopathological Society", org_works)



########################################################################
###################### End of Testing ##################################
########################################################################

#### Find duplicates and frequencies #####
# change DF here
df <-works_cited_issn
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

# write_xlsx(duplicate_multi_cited_rows, "citations/duplicate_multi_cited_2023.xlsx")
# write_xlsx(duplicate_multi_cited_rows_unique, "citations/duplicate_multi_cited_unique_2023.xlsx")

# Save the modified dataset to Excel
write_xlsx(publisher_NA, "citations/publisher_jouranl_NA_2023.xlsx")
write_xlsx(publisher_aaas, "citations/publisher_aaas_2023.xlsx")
write_xlsx(publisher_nature, "citations/publisher_nature_2023.xlsx")
write_xlsx(publisher_plos, "citations/publisher_plos_2023.xlsx")
write_xlsx(publisher_microbiology, "citations/publisher_microbiology_2023.xlsx")

write_xlsx(publisher_emerald, "citations/publisher_journal_emerald_2023.xlsx")
write_xlsx(publisher_emerald2, "citations/publisher_emerald_2023.xlsx")

write_xlsx(publisher_iwa, "citations/publisher_iwa_2023.xlsx")

publisher_cell_press <- publisher_cell_press %>%
  mutate(across(where(is.character), ~ ifelse(nchar(.) > 32767, substr(., 1, 32767), .)))
write_xlsx(publisher_cell_press, "citations/publisher_journal_cell_press_2022.xlsx")

publisher_aps <- publisher_aps %>%
  mutate(across(where(is.character), ~ ifelse(nchar(.) > 32767, substr(., 1, 32767), .)))
write_xlsx(publisher_aps, "citations/publisher_aps_journal_2023.xlsx")

publisher_aps2 <- publisher_aps2 %>%
  mutate(across(where(is.character), ~ ifelse(nchar(.) > 32767, substr(., 1, 32767), .)))
write_xlsx(publisher_aps2, "citations/publisher_aps_non_journal_2022.xlsx")

# Create a list to hold the data frames
cited_all_types <- list(
  APS_journal_type = publisher_aps, 
  APS_non_journal_type = publisher_aps2  
)
# Write the list to an Excel file with each data frame as a separate sheet
write_xlsx(cited_all_types, "citations/publisher_aps_cited_works_2022.xlsx")


######################################
######################################
### Function: To count issns occurrences for a given publisher (note: issns count is more accurate)
# @param: dataframe issns_articles_cited
#          publisher_name
# return: issns and counts cited 
count_issns_by_publisher <- function(works_cited_issn, publisher_name) {
  # Filter rows where host_organization matches the specified publisher
  publisher1 <-  works_cited_issn[grepl(publisher_name, works_cited_issn$host_organization, ignore.case = TRUE), ]
  
  # Count the occurrences of each journal under the specified publisher
  issns_counts <- table(publisher1$so)
  issns_counts_df <- as.data.frame(issns_counts)
  
  return(issns_counts_df)
}


publisher_name <- "Microbiology society"
publisher1 <-  works_cited_issn[grepl(publisher_name, works_cited_issn$host_organization, ignore.case = TRUE), ]
journal_counts_df <- count_issns_by_publisher(works_cited_issn, publisher_name)
print(journal_counts_df)
# Note: Errors
# https://openalex.org/W2165027548 (1994 v44n3, Journal name changes and ISSN changed)


publisher_name <- "Optica Publishing Group"
publisher1 <-  works_cited_issn[grepl(publisher_name, works_cited_issn$host_organization, ignore.case = TRUE), ]
journal_counts_df <- count_issns_by_publisher(works_cited_issn, publisher_name)
print(journal_counts_df)

publisher_name <- "Canadian Science Publishing"
publisher1 <-  works_cited_issn[grepl(publisher_name, works_cited_issn$host_organization, ignore.case = TRUE), ]
journal_counts_df <- count_issns_by_publisher(works_cited_issn, publisher_name)
print(journal_counts_df)

publisher_name <- "IWA publishing"
publisher1 <-  works_cited_issn[grepl(publisher_name, works_cited_issn$host_organization, ignore.case = TRUE), ]
journal_counts_df <- count_issns_by_publisher(works_cited_issn, publisher_name)
print(journal_counts_df)

publisher_name <- "Emerald Publishing"
publisher1 <-  works_cited_issn[grepl(publisher_name, works_cited_issn$host_organization, ignore.case = TRUE), ]
journal_counts_df <- count_issns_by_publisher(works_cited_issn, publisher_name)
print(journal_counts_df)
write_xlsx(journal_counts_df, "citations/publisher_emerald_2023_counts.xlsx")

publisher_name <- "American Phytopathological Society"
publisher1 <-  works_cited_issn[grepl(publisher_name, works_cited_issn$host_organization, ignore.case = TRUE), ]

issns_counts_df <- count_issns_by_publisher(works_cited_issn, publisher_name)
print(issns_counts_df)

search_string <- "https://openalex.org/W2070851128"
search_references(search_string, org_works)



# Group by 'host_organization' and count the number of articles for each publisher
publisher_ranking <- works_cited_issn %>%
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
  labs(x = "Publisher", y = "Number of Articles", title = "2022 UA Top 20 Publishers (Number of Articles Cited)") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 7))  # Reduce font size of publisher names

# Calculate the percentage of the top 20, top 50, and top 100 publishers over the total
total_article_count <- sum(publisher_ranking$article_count) # Total articles in all publishers
top_20_total_count <- sum(top_20_publishers$article_count)  
top_50_total_count <- sum(top_50_publishers$article_count)  
top_100_total_count <- sum(top_100_publishers$article_count)  

# Calculate the percentage for year 2019, 2020, 2021, 2022, 2023
# Top  20: ~74-76%
# Top  50: ~90%
# Top 100: ~95%
top_20_percentage_of_total <- (top_20_total_count / total_article_count) * 100
top_50_percentage_of_total <- (top_50_total_count / total_article_count) * 100
top_100_percentage_of_total <-(top_100_total_count/ total_article_count) * 100

print(paste("Top 20 publishers represent",  round(top_20_percentage_of_total, 0), "% of the total articles."))
print(paste("Top 50 publishers represent",  round(top_50_percentage_of_total, 0), "% of the total articles."))
print(paste("Top 100 publishers represent", round(top_100_percentage_of_total, 0), "% of the total articles."))

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

