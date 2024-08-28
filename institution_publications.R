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
  from_publication_date ="2023-01-01")

# 1. Getting data
# retrieving all publications association with UArizona's ROR (Research Organization Registry) id.  For this project, I'm using 5 years of data. 
# 2024-08-26: 50,000 records (10-year data since 2014 - 2024: 86,000 records, 11-year data since 2013: 100,000 records)
# It took about 15 mins to run, and used 7GB memory. 
UAworks1 <-oa_fetch(
  entity="works",
  institutions.ror=c("03m2x1q45"),
  from_publication_date ="2023-01-01"
  #count_only = TRUE
  )

UAworks2 <-oa_fetch(
  entity="works",
  institutions.ror=c("03m2x1q45"),
  from_publication_date ="2014-01-01",
  count_only = TRUE
  )

# alternative to get referenced works (to verify: The below is to make sure UA authors only)
WorksCited_alt <- as.list(unique(do.call(rbind, UAworks1$referenced_works)))

# 2. Data cleanup
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
# 94,500 obs from 426,000 obs (UA authors only)
UAWorks_UAauthors2 <- UAWorks_authors%>%filter(institution_rorauthor== "https://ror.org/03m2x1q45")

# 3. check workcited
# 8.2 million 
WorksCited <- as.list(unique(do.call(rbind, UAWorks_UAauthors2$referenced_works)))

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


