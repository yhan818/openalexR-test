############# Institution authors publication analysis and Collection Management ##########
######## Author: Yan Han
######## Date: Sep 3, 2024
######## Updated: 
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
setwd("/home/yhan/Documents/UA-datasets/openalexR-test/")

# Banner-University Medical Center Tucson. 399 works.
UAUMC.df <-oa_fetch(
  entity="works",
  institutions.ror=c("02xbk5j62"),
  from_publication_date ="2024-01-01")

##### 1. Getting data. (retrieved 2024-09-02)
# Retrieving all publications association with UArizona's ROR (Research Organization Registry) ID.
# Need to run 2 years data. or need better computer. 
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
  from_publication_date ="2022-01-01",
  count_only = TRUE
)


### 1.2 Getting all the works based on the institution ROR and publication date. It takes longer time. 
# see above for the running time
UAworks <-oa_fetch(
  entity="works",
  institutions.ror=c("03m2x1q45"),
  from_publication_date ="2023-01-01"
  # , to_publication_date = "2021-12-31"
  )

# alternative to get referenced works (to verify: The below is to make sure UA authors only)
# 15k works ==> 20 M references??? 

##### 2. Checking and verifying data
###### change this line only to update the right dataset.
ref_works <- UAworks$referenced_works
#########################

### 2.1 Checking Column referenced_works:  a list
class(ref_works)

# Find "NA" indexes 
na_indices <- which(sapply(ref_works, function(x) is.logical(x) && is.na(x)))
print(na_indices)
# count how many "NA" in referenced_works col. ~ 20% of works contain "NA"
na_count <- sum(sapply(ref_works, function(x) is.logical(x) && is.na(x)))
print(na_count)

### 2.2 Combine all the references and do further data analysis
ref_works_combined <- unlist(ref_works, use.names = FALSE)
ref_works_combined <- ref_works_combined[!is.na(ref_works_combined)]  # Remove NA values
print(ref_works_combined)
summary(ref_works_combined)

### 2.21 finding these duplicates, which mean the duplicates have been cited multiple times 
# (probably more important to have these journals subscribed!)
# cited more: ~25% (2023 data)
ref_works_more_cited <- ref_works_combined[duplicated(ref_works_combined)]
print(ref_works_more_cited)
summary(ref_works_more_cited)

# 2.22 remove the duplicates for further processing. 
# cited:  
ref_works_cited <- unique(ref_works_combined)
summary(ref_works_cited)

# 3. Data cleanup.
# Flattening authors fields from the DF (multiple authors per work). 
# 426,000 obs (multiple authors) from 50,400 obs (works)
UAworks_since <- UAworks

UAWorks_authors<-UAworks_since%>%
  mutate(author=lapply(author, function(x){
    names(x) <-paste0(names(x), "author")
    return(x)
  }))%>%
unnest(author)

#### TESTING!!!
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
UAWorks_UAauthors2 <- UAWorks_authors%>%filter(institution_rorauthor== "https://ror.org/03m2x1q45")

## why https://openalex.org/W2991357209 (can be filtered out)

# 3. check workcited
# 8.2 million 
ref_works2 <- UAWorks_UAauthors2$referenced_works
#########################

# Combine all the references and do further data analysis
ref_works_combined2 <- unlist(ref_works2, use.names = FALSE)
ref_works_combined2 <- ref_works_combined2[!is.na(ref_works_combined2)]  # Remove NA values
summary(ref_works_combined2)

# finding these duplicates, which mean the duplicates have been cited multiple times 
# Cited multiple times: 65% ?
ref_works_more_cited2 <- ref_works_combined[duplicated(ref_works_combined2)]
summary(ref_works_more_cited2)

# 2.22 remove the duplicates for further processing. 
# Cited: 
ref_works_cited2 <- unique(ref_works_combined2)
summary(ref_works_cited2)

######################## TEST
# Elements in vector1 but not in vector2
diff1 <- setdiff(ref_works_cited, ref_works_cited2)
print(diff1)

diff2 <- setdiff(ref_works_cited2, ref_works_cited)
print(diff2)

# Find the indices of elements matching a pattern
matching_indices <- grep("https://openalex.org/W4401226694", ref_works2 )
print(matching_indices)  # Returns the index(es) of matching elements

matching_indices <- grep("https://openalex.org/W3203440266", ref_works )
print(matching_indices)  # Returns the index(es) of matching elements


#Running the loop to retrieve works cited data (may take someref_works_cited2#Running the loop to retrieve works cited data (may take some time to run)
# 1,000 for testing ONLY
num_of_works <- 1000

# the real number of works cited
# 2023: 379,978
# 2022: 571,227. Note: This crashed R studio with 12 GB memeroy used. showing 560,000
num_of_works <- length(ref_works_cited2)
summary(ref_works2)

### Testing if a work is found. 
search_string <- "https://openalex.org/W2919115771"
result <- lapply(ref_works2, function(x) grep(search_string, x, value = TRUE))
print(result)
matches <- result[sapply(result, length) > 0]
print(matches)
indices <- which(sapply(ref_works2, function(x) any(grepl(search_string, x))))
for (i in indices) {
  cat("Index:", i, "\n")
  cat("Element:\n", ref_works2[[i]], "\n\n")
}


#Creating an empty dataframe to store the results of the for loop.
WorksCited2.df <-data.frame()
# getting these works' metadata. 50 works a time (It will take 20-40 mins to run!!!) 
# 18,211 works for 2024 citations
# 2023: 379,978 works: 2 hrs to run? 

fetch_number <- 1000
time_taken <- system.time({
  # Take by=100 one time gets 205 obs.  cannot set it too high. 500 will miss works.
  for(i in seq(1, num_of_works, by=fetch_number)){
    batch_identifiers <-ref_works_cited2[i:min(i+fetch_number-1, num_of_works)]
    batch_data <-oa_fetch(identifier=batch_identifiers)
    WorksCited2.df<-rbind(WorksCited2.df, batch_data)
  }
})
print(paste("time to run: ", time_taken["elapsed"] / 60, "minutes"))

# 372,000 works use 0.2 GB memory
df <- WorksCited2.df
# further analysis
# publisher: host_organization
unique_publishers <- unique(df$host_organization)
# number of publishers
num_unique_publishers <- length(unique_publishers)
# list top 50 publishers
print(unique_publishers[1:50])
# list NULL publishers. 18,000 / 372,000 works
num_na <- sum(is.na(df$host_organization))
print(num_na)



#############################################
WorksCited <- as.list(unique(do.call(rbind,WorksUnique.df$referenced_works)))
#Removing any values of NA and any duplicate values
WorksCited <-unique(WorksCited) %>%discard(is.na)
#Creating an empty dataframe to store the results of the for loop.
WorksCited.df <-data.frame()
#Running the loop to retrieve works cited data (may take some time to run)
for(i in seq(1, length(WorksCited), by=50)){
  batch_identifiers <-WorksCited[i:min(i+49, length(WorksCited))]
  batch_data <-oa_fetch(identifier=batch_identifiers)
  WorksCited.df<-rbind(WorksCited.df, batch_data)
}
