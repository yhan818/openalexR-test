############# Institution authors publication analysis and Collection Management ##########
######## Author: Yan Han with help of ChatGPT 4
######## Date: Sep 6, 2024
######## Updated: Fixed NA issue with host_organization
##### Search an institution authors' publication using openAlex data ####
# OpenAlex R Documentation: https://github.com/ropensci/openalexR

install.packages("remotes")
remotes::install_github("ropensci/openalexR", force=TRUE) 
#install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2")

library(openalexR)
#library(dplyr)
library(tidyverse)

# free unused obj to manage memory
rm(list=ls())
gc
options("max.print" = 100000)

options (openalexR.mailto="yhan@arizona.edu")
getwd()
setwd("/home/yhan/Documents/openalexR-test/")

# Load saved rds file into a data frame. This is year 2023 UA works
works_cited <- readRDS("../works_cited.rds")

# Banner-University Medical Center Tucson. 399 works.
UAUMC.df <-oa_fetch(
  entity="works",
  institutions.ror=c("02xbk5j62"),
  from_publication_date ="2024-01-01")

##### 1. Getting data. (retrieved 2024-09-02)
# Retrieving all publications association with UArizona's ROR (Research Organization Registry) ID.
# UA publications per year is ~9,000. For running 2 years data, need better computer or crashed R studio.
# Year 2023: 9,157 works.
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
  from_publication_date ="2021-01-01",
  to_publication_date = "2021-12-31",
  count_only = TRUE
)

### 1.2 Getting all the works based on the institution ROR and publication date. It takes longer time. 
# see above for the running time
org_works_2023 <-oa_fetch(
  entity="works",
  institutions.ror=c("03m2x1q45"),
  from_publication_date ="2023-01-01",
  to_publication_date = "2023-12-31"
)
saveRDS(org_works_2023, "../org_works_2023.rds")

org_works_2022 <-oa_fetch(
  entity="works",
  institutions.ror=c("03m2x1q45"),
  from_publication_date ="2022-01-01",
  to_publication_date = "2022-12-31"
)
saveRDS(org_works_2022, "../org_works_2022.rds")

# change working data here 
org_works <- org_works_2022

##### 2. Checking and verifying data
###### change this line only to update the right dataset.
ref_works <- org_works$referenced_works
#########################

### 2.1 Checking Column referenced_works:  a list
class(ref_works)

# Find "NA" indexes 
na_indices <- which(sapply(ref_works, function(x) is.logical(x) && is.na(x)))
print(na_indices)
# count how many "NA" in referenced_works col. ~ 15%-20%  of works contain "NA"
na_count <- sum(sapply(ref_works, function(x) is.logical(x) && is.na(x)))
print(na_count)

### 2.2 Combine all the references and do further data analysis
ref_works_combined <- unlist(ref_works, use.names = FALSE)
ref_works_combined <- ref_works_combined[!is.na(ref_works_combined)]  # Remove NA values
summary(ref_works_combined)

### 2.21 finding these duplicates, which mean the duplicates have been cited multiple times 
# (probably more important to have these journals subscribed!)
# cited more: ~20% - 25%  (2022, 2023 UArizona data)
ref_works_more_cited <- ref_works_combined[duplicated(ref_works_combined)]

table(ref_works_more_cited)

### 2.23 For Testing purpose
# Find the index of multiple samples
which(ref_works_combined %in% c("https://openalex.org/W4292309267", "https://openalex.org/W621173178"))
# Find the index of "ref1" in the published works' referenced_works. 
which(sapply(org_works$referenced_works, function(x) "https://openalex.org/W4292309267" %in% x))
# We can see the original works for samples
org_works[706, "id"]
org_works[779, "id"]

# 2.22 remove the duplicates for further processing. 
# cited:  
ref_works_cited <- unique(ref_works_combined)
summary(ref_works_cited)

# 3. Data cleanup.
# Flattening authors fields from the DF (multiple authors per work). 
# 426,000 obs (multiple authors) from 50,400 obs (works)
org_works_since <- org_works

org_works_authors<-org_works_since%>%
  mutate(author=lapply(author, function(x){
    names(x) <-paste0(names(x), "author")
    return(x)
  }))%>%
  unnest(author)

# After flattening, authors' fields (e.g. au_idauthor, institution_rorauthor) are displayed
colnames(org_works)
colnames(org_works_authors)

#### 3.3 TESTING!!!
# Then extract UArizona authors only
# 94,500 obs from 426,000 obs (UA authors only).  
## https://openalex.org/A5033317672 Saurav Mallik (is at two affiliations for https://api.openalex.org/works/W4389611927. Harvard and University of Arizona)
### https://openalex.org/W4401226694 author Renu Malhotra has two affiliations. 
oa_fetch_test1 <-oa_fetch( entity="works",  id="https://openalex.org/W4401226694")
oa_fetch_test1$author
view(oa_fetch_test1[[4]][[1]])

oa_fetch_test2 <-oa_fetch_test2 <-oa_fetch( entity="authors",  id="https://openalex.org/A5003933592")

#### This is not 100% accurate because UArizona has child organization whose ROR is associated with an article. By filtering institution_rorauthor
# to UArizona's ROR, certain articles are left out!!! 
# 2024-09: I am currently working with openAlexR developers to fix this. 
org_works_UAauthors <- org_works_authors%>%filter(institution_rorauthor== "https://ror.org/03m2x1q45")

## why https://openalex.org/W2991357209 (can be filtered out)

# 3. check work_cited
# 8.2 million 
org_ref_works <- org_works_UAauthors$referenced_works
#########################

# Combine all the references and do further data analysis.
# Giving avg 30-40 references per work, the size will be 40x.
org_ref_works_combined <- unlist(org_ref_works, use.names = FALSE)
org_ref_works_combined <- org_ref_works_combined[!is.na(org_ref_works_combined)]  # Remove NA values

# finding these duplicates, which mean the duplicates have been cited multiple times 
# Cited multiple times = 65% 
org_ref_works_more_cited <- org_ref_works_combined[duplicated(org_ref_works_combined)]

# 2.22 remove the duplicates for further processing. unique works cited = 38% 
org_ref_works_cited <- unique(org_ref_works_combined)

# Find the indices of elements matching a pattern
matching_indices <- grep("https://openalex.org/W4401226694", org_ref_works)
print(matching_indices)  # Returns the index(es) of matching elements
matching_indices <- grep("https://openalex.org/W3203440266", ref_works )
print(matching_indices)  # Returns the index(es) of matching elements

#Running the loop to retrieve works cited data (may take someref_works_cited2#Running the loop to retrieve works cited data (may take some time to run)
# 1,000 for testing ONLY
num_of_works <- 1000

# the real number of works cited
# 2023-current: 379,978
# 2022-current: 571,227. Note: This crashed R studio with 12 GB memeroy used. showing 560,000
num_of_works <- length(org_ref_works_cited)

### Testing if a work is found. 
search_string <- "https://openalex.org/W2919115771"
result <- lapply(ref_works2, function(x) grep(search_string, x, value = TRUE))
print(result)
matches <- result[sapply(result, length) > 0]
print(matches)
indices <- which(sapply(org_ref_works, function(x) any(grepl(search_string, x))))
for (i in indices) {
  cat("Index:", i, "\n")
  cat("Element:\n", org_ref_works[[i]], "\n\n")
}

#Creating an empty dataframe to store the results of the for loop.
works_cited_df <-data.frame()
#WorksCited2.df <-data.frame()
# getting these works' metadata. 100 or 1,000 works a time (It will take 60 - 180 mins to run!!!) 
# 18,211 works for 2024 citations
# 2023: 241,252. 171 min to run

# the number of works to fetch at a time has little influence the time to run oa_fetch
# 2024-09: fetch_number = 1,000, reduced the total running time of 10% comparing to fetch_number 100
# 2024-09: fetching 241,000 works took 188 minutes
# test 100 records per time

fetch_number <- 100
time_taken <- system.time({ 
  for(i in seq(1, num_of_works, by=fetch_number)){
    batch_identifiers <-org_ref_works_cited[i:min(i+fetch_number-1, num_of_works)]
    batch_data <-oa_fetch(identifier=batch_identifiers)
    works_cited_df<-rbind(works_cited_df, batch_data)
  }
})
print(paste("time to run: ", time_taken["elapsed"] / 60, "minutes"))

UAauthors <-unique(org_works_UAauthors)

# Saving data frames to RDS files
library(writexl)
saveRDS(works_cited_df, "../works_cited_2022.rds")

#write_xlsx(UAauthors2, "UAauthors.xlsx")


###################### Citation Analysis
# 372,000 works use 0.2 GB memory

articles_cited_df <- works_cited_df
# 1. Analyse journal usage
#  - remove any row whose col "issn_l" is empty or NULL 
#  - 2023: 224,000 articles out of 241,000 works
articles_cited_df <- articles_cited_df[!(is.na(articles_cited_df$issn_l) | articles_cited_df$issn_l == ""), ]
nrow(articles_cited_df)

# articles_cited_df_2022: 226,471 out of 243,452 
# articles_cited_df_2022: 224,000 
saveRDS(articles_cited_df, "../articles_cited_df_2022.rds")

# Trim and normalize the host_organization column
articles_cited_df$host_organization <- trimws(articles_cited_df$host_organization)
articles_cited_df$issn_l <- trimws(articles_cited_df$issn_l)

# Empty or NULL records
count_null_empty_id <- sum(is.na(articles_cited_df$id) | trimws(articles_cited_df$id) == "")
count_null_empty_id

articles_cited_df <- articles_cited_df[!(is.na(articles_cited_df$id) | trimws(articles_cited_df$id) == ""), ]

# publisher: host_organization

### CDC has a weekly journal with ISSN: https://portal.issn.org/resource/ISSN/1545-8636

unique_publishers <- unique(articles_cited_df$host_organization)
# number of publishers: ~1,600
num_unique_publishers <- length(unique_publishers)
# list top 50 publishers
print(unique_publishers[1:50])
# list NULL publishers = 5%
num_na <- sum(is.na(articles_cited_df$host_organization))

# Replace NA values and empty strings with "NA"
articles_cited_df$host_organization[is.na(articles_cited_df$host_organization) | trimws(articles_cited_df$host_organization) == ""] <- "NA"

# 1. First, showing all NA publisher: meaning publisher info is not available. 
publisher_NA <- articles_cited_df[articles_cited_df$host_organization == "NA", ]
write_xlsx(publisher_NA, "publisher_NA.xlsx")

# 2. Elsevier
publisher_elsevier <- articles_cited_df[articles_cited_df$host_organization == "Elsevier BV", ]

# 3. Springer
publisher_springer <- articles_cited_df[articles_cited_df$host_organization == "Springer Science+Business Media", ]

# showing all CDC's journals
publisher_cdc <- articles_cited_df[articles_cited_df$host_organization == "Centers for Disease Control and Prevention", ]

publisher_ncte <- articles_cited_df[articles_cited_df$host_organization == "National Council of Teachers of English", ]

publisher_ua <- articles_cited_df[articles_cited_df$host_organization == "University of Arizona", ]
publisher_uap <- articles_cited_df[articles_cited_df$host_organization == "University of Arizona Press", ]

# Group by 'host_organization' and count the number of articles for each publisher
publisher_ranking <- articles_cited_df %>%
  group_by(host_organization) %>%
  summarise(article_count = n()) %>%
  arrange(desc(article_count))

library(ggplot2)
top_10_publishers <- publisher_ranking %>% slice(1:10)

# Bar plot for top 10 publishers
ggplot(top_10_publishers, aes(x = reorder(host_organization, -article_count), y = article_count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Flip the axis for better readability
  labs(x = "Publisher", y = "Number of Articles", title = "Top 10 Publishers by Number of Articles Cited") +
  theme_minimal()


top_20_publishers <- publisher_ranking %>% slice(1:20)
# Bar plot for top 10 publishers
ggplot(top_20_publishers, aes(x = reorder(host_organization, -article_count), y = article_count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Flip the axis for better readability
  labs(x = "Publisher", y = "Number of Articles", title = "Top 20 Publishers by Number of Articles Cited") +
  theme_minimal()


view(publisher_ranking)
# View the top 50 publishers.  
# Top 10: Elsevier (20%), Wiley (9.4%), Oxford University Press (6.3%), Springer(5.7%), Nature Portfolio,
#  IOP Publishing, Lippincott Williams & Wilkins, Taylor & Francis, SAGE Publishing (2.6%)
