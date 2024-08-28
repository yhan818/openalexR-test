############# Institution authors publication analysis and Collection Management ##########
######## Author: Yan Han
######## Date: Aug 23, 2024
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

options (openalexR.mailto="yhan@arizona.edu")
getwd()
setwd("/home/yhan/Documents/UA-datasets/openalexR-test/")

# Banner-University Medical Center Tucson. 399 works.
UAUMC.df <-oa_fetch(
  entity="works",
  institutions.ror=c("02xbk5j62"),
  from_publication_date ="2024-01-01")

# 1. Getting data. (retrieved 2024-08-26)
# retrieving all publications association with UArizona's ROR (Research Organization Registry) ID.
# 2023-current: 14,500 works. 
# 2013- current: 50,000 records 
# 2014- current: 86,000 works 

# Note: for 2014 data, it took about 15 mins to run, and used 7GB memory. 
UAworks1 <-oa_fetch(
  entity="works",
  institutions.ror=c("03m2x1q45"),
  from_publication_date ="2024-08-01"
  #count_only = TRUE
  )

UAworks2 <-oa_fetch(
  entity="works",
  institutions.ror=c("03m2x1q45"),
  from_publication_date ="2014-01-01",
  count_only = TRUE
  )

# alternative to get referenced works (to verify: The below is to make sure UA authors only)
# 15k works ==> 20 M references??? 

##### 2. Checking and verifying data
###### change this line only to update the right dataset.
ref_works <- UAworks1$referenced_works
#########################

# 2.1 Checking Column referenced_works:  a list
class(ref_works)

# Find "NA" indexes 
na_indices <- which(sapply(ref_works, function(x) is.logical(x) && is.na(x)))
print(na_indices)
# count how many "NA" in referenced_works col. ~ 20% of works contain "NA"
na_count <- sum(sapply(ref_works, function(x) is.logical(x) && is.na(x)))
print(na_count)

# 2.2 Combine all the references and do further data analysis
ref_works_combined <- unlist(ref_works, use.names = FALSE)
ref_works_combined <- ref_works_combined[!is.na(ref_works_combined)]  # Remove NA values
print(ref_works_combined)
summary(ref_works_combined)

# 2.21 finding these duplicates, which mean the duplicates have been cited multiple times (probably more important to have these journals subscribed!)
ref_works_more_cited <- ref_works_combined[duplicated(ref_works_combined)]
print(ref_works_more_cited)
summary(ref_works_more_cited)

# 2.22 remove the duplicates for further processing. 
ref_works_cited <- unique(ref_works_combined)
summary(ref_works_cited)


# 3. Data cleanup.
# Flattening authors fields from the DF (multiple authors per work). 
# 426,000 obs (multiple authors) from 50,400 obs (works)
UAworks_since <- UAworks1

UAWorks_authors<-UAworks_since%>%
  mutate(author=lapply(author, function(x){
    names(x) <-paste0(names(x), "author")
    return(x)
  }))%>%
unnest(author)

# Then extract UArizona authors only
# 94,500 obs from 426,000 obs (UA authors only).  
## https://openalex.org/A5033317672 Saurav Mallik (is at two affiliations for https://api.openalex.org/works/W4389611927. Harvard and University of Arizona)
### https://openalex.org/W4401226694 author Renu Malhotra has two affiliations. 
oa_fetch_test1 <-oa_fetch( entity="works",  id="https://openalex.org/W4401226694")
oa_fetch_test1$author
view(oa_fetch_test1[[4]][[1]])

oa_fetch_test2 <-oa_fetch_test2 <-oa_fetch( entity="authors",  id="https://openalex.org/A5003933592")

#UAWorks_UAauthors3 <- UAWorks_authors%>%filter(institution_lineageauthor== "https://ror.org/03m2x1q45")

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

# finding these duplicates, which mean the duplicates have been cited multiple times (probably more important to have these journals subscribed!)
ref_works_more_cited2 <- ref_works_combined[duplicated(ref_works_combined2)]
summary(ref_works_more_cited2)

# 2.22 remove the duplicates for further processing. 
ref_works_cited2 <- unique(ref_works_combined2)
summary(ref_works_cited2)

# Elements in vector1 but not in vector2
diff1 <- setdiff(ref_works_cited, ref_works_cited2)
print(diff1)


# Find the indices of elements matching a pattern
matching_indices <- grep("https://openalex.org/W4401226694", ref_works2 )
print(matching_indices)  # Returns the index(es) of matching elements


#Removing any values of NA and any duplicate values
# reduced to 1.07 million (hm... need to check )
WorksCited <-unique(WorksCited) %>%discard(is.na)



#Creating an empty dataframe to store the results of the for loop.
WorksCited.df <-data.frame()

#Running the loop to retrieve works cited data (may take some time to run)
for(i in seq(1, length(WorksCited), by=50)){
  batch_identifiers <-WorksCited[i:min(i+49, length(WorksCited))]
  batch_data <-oa_fetch(identifier=batch_identifiers)
  WorksCited.df<-rbind(WorksCited.df, batch_data)
}


