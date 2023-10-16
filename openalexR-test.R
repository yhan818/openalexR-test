##################################################
######## Author: Yan Han 
######## Date: Jan 16, 2023; Updated: Oct 6, 2023
#### 2023-05: 
#### 2023-08: OpenAlex changed its old authorID
#### 2023-09: Compare UArizona ROR, display_name, and OpenAlexID
#### 2023-10: Add each unit of Banner Health query and dedup

# OpenAlex R Documentation: https://github.com/ropensci/openalexR
#install.packages("openalexR")
install.packages("remotes")
remotes::install_github("ropensci/openalexR", force=TRUE) 

install.packages("dplyr")
install.packages("ggplot2")
install.packages("knitr")
install.packages("writexl")

# Before we go any further, we highly recommend you set openalexR.mailto option so that your requests go to the polite pool for faster response times: 
options (openalexR.mailto="yhan@arizona.edu")

# common libraries to add
library(openalexR)
library(dplyr)
library(ggplot2)
library(knitr)
library(writexl)

############################################################
#### Using API directly
# Info about UArizona ror":"https://ror.org/03m2x1q45
# https://api.openalex.org/institutions/ror:03m2x1q45

# Get the number of scholarly works produced in UArizona grouped by their open access status (green, bronze, gold, hybrid, or closed):
#  https://api.openalex.org/works?filter=institutions.ror:https://ror.org/050qmg959&group_by=oa_status

# Get an author info

# Get retracted papers by UArizona:
# https://api.openalex.org/works?filter=institutions.ror:03m2x1q45&group_by=is_retracted
# https://api.openalex.org/works?filter=institutions.ror:https://ror.org/03m2x1q45&group_by=is_retracted




################################ Author ##############################
# Filter Doc: https://github.com/ropensci/openalexR/blob/main/vignettes/articles/Filters.Rmd
### The followings are examples 

############################## ORCID as filter ####################################
# Download all works published by a set of authors using ORCIDs
# use author.orcid as a filter
# https://api.openalex.org/authors/https://orcid.org/0000-0001-9518-2684
### NOTE: Not all the works are there, discussed with OpenAlex. Most likely the disambigation alg not working well. https://docs.openalex.org/api-entities/authors

works_from_orcids <- oa_fetch(
  entity = "works",
  author.orcid = c("0000-0001-9518-2684"),  
  # Yan Han ORCID: 0000-0001-9518-2684; have 14 works at ORCID's website. OpenAlex does not pull works from ORCID at the moment. It pulls majorly from Microsoft academic graph
  # Yan Han OpenAlex ID. https://api.openalex.org/people/A2108267685
  
  # author.orcid = c("0000-0001-6187-6610", "0000-0002-8517-9411"),
  verbose = TRUE  
) 

works_from_orcids |>
  show_works()  |>
  knitr::kable()
#> Requesting url: https://api.openalex.org/works?filter=author.orcid%3A0000-0001-9518-2684%7C0000-0002-8517-9411
# > output to a file 

### Aug 2023: Yan Han: affiliation Jilin university. Wrong
##### Getting authors' info using their ORCIDs
authors_from_orcids <- oa_fetch(
  entity = "authors",
   orcid =  c("0000-0001-6187-6610", "0000-0002-8517-9411", "0000-0003-1613-5981", "0000-0001-9518-2684")
)

str(authors_from_orcids) # show the object

authors_from_orcids |> 
 show_authors() |>
 knitr::kable()

#################### Author's openAlex ID ###########################
### Sep 2023: old authorID was removed. 
author_from_openalex_id <-oa_fetch(entity = "author", openalex = "A4353996111" )

###################### Author's name ####################################
###  use search for fuzzy name (middle name), 
###  do NOT use display_name because it requires an exact match. Often there are multiple middle names for an author
authors_from_names <- oa_fetch(entity = "author",
                              search = "Phillip Kuo")  ### "search" syntax allowes fuzzy search for middle name

authors_from_names
authors_from_names$id
authors_from_names$affiliation_display_name
grep("Arizona*", authors_from_names$affiliation_display_name, value=TRUE, ignore.case=TRUE) 

authors_from_names |> 
  show_authors() |>
  knitr::kable()

#################### University (institution.id) as filter ######################
# Download all authors' records (>1 publications) who currently work at the University of Arizona (OpenAlex ID: I138006243. looks like this is the "I"+"mag") 
# id":"https://openalex.org/I138006243","ror":"https://ror.org/03m2x1q45","display_name":"University of Arizona","relevance_score":82564.97,"country_code":"US","type":"education","homepage_url":"http://www.arizona.edu/",

# All authors
org_args <- list(
  entity = "authors",
  last_known_institution.id = "I138006243", # University of Arizona OpenAlex ID
  works_count = ">0"
)
# July 2023: 58,183 records. 
# Sep 2023: 26,801 records. (Note: author disambuition system changed in Aug 2023)
do.call(oa_fetch, c(org_args, list(count_only = TRUE)))

# Download the list
all_authors <- do.call(oa_fetch, org_args) |>
  show_authors() |>
  knitr::kable()
show(all_authors) # already sorted by total number of works_count

# Top authors
org_args2 <- list(
  entity = "authors",
  last_known_institution.id = "I138006243", # University of Arizona OpenAlex ID
  works_count = ">499"
)
# 72 authors
do.call(oa_fetch, c(org_args2, list(count_only = TRUE)))

top_authors <- do.call(oa_fetch, org_args) |>
  show_authors() |>
  knitr::kable()
show(top_authors)
# see the head of the authors' list
install.packages("listviewer")
library(listviewer)
jsonedit(top_authors)
head(top_authors)
###############

#### University's multiple IDs'
### Go to: https://api.openalex.org/institutions/ror:03m2x1q45 (which can be saved as a JSON file) or : https://api.openalex.org/institutions/I138006243 (which can be saved as a JSON file) 
### Both JSON files are extact the same. 
### On the above page: Check "ids", which shows that UArizona 
### OpenAlex ID: I138006243, ROR: https://ror.org/03m2x1q45 , mag: "138006243", grid: "grid.134563.6", wikipedia: "https://en.wikipedia.org/wiki/University%20of%20Arizona", wikidata:"https://www.wikidata.org/wiki/Q503419"
###
### Sep 2023: Works count: 189,945, Citation count: 6,114,948

### Removing Identifier will return all of U.S. institutions, currently 4,344
### Flitering using name "University of Arizona" only (which is in the above openAlex id and ror record showing "display_name" as "University of Arizona")
### 2022: 7856 : 444545  (total works: citations)
### 2021: 8499 : 448383
### 2020: 9007: 406828

#### Method 1: using ROR ID
university1 <-oa_fetch(
  entity = "institutions",
  identifier = "ror:03m2x1q45",
  country_code = "us",
  type = "education",
  verbose = TRUE
)
### Method 2: using openAlex ID (MAG ID)
university2 <-oa_fetch(
  entity = "institutions",
  identifier = "I138006243",
  country_code = "us",
  type = "education",
  verbose = TRUE
)

#### Method 3: using dplyr package
institutions <-oa_fetch(
  entity = "institutions",
  country_code = "us",
  type = "education",
  verbose = TRUE
)

### filtering by name
filtered_university1 <- institutions %>% 
  filter(display_name == "University of Arizona")

filtered_university2 <- institutions %>% 
  filter(display_name == "Arizona")

####################################
banner2 <-oa_fetch(
  entity = "institutions",
  identifier = "ror:039wwwz66",
  country_code = "us",
  type = "education",
  verbose = TRUE
)

banner3 <-oa_fetch(
  entity = "institutions",
  identifier = "ror:01cjjjf51",
  country_code = "us",
  type = "education",
  verbose = TRUE
)

### banner2 <- lapply(banner2, function(x) if(is.list(x)) toString(x) else x)
## write.csv(banner2, file="banner2.csv")

# why no colleciton found?? 
banner2_authors <- oa_fetch(entity = "author", last_known_institution.ror="039wwwz66" )

### Getting all the works for an institution! 
banner2_works <- oa_fetch (entity = "works", authorships.institutions.ror="039wwwz66", verbose = TRUE)

# you can ALSO use library (dplyr) to run: 
# filtered_works_2020_2 <- works_from_institution %>% filter(publication_year == 2020)
banner2_works_2020 <- subset(banner2_works, publication_year == 2020)
banner2_works_2021 <- subset(banner2_works, publication_year == 2021)
banner2_works_2022 <- subset(banner2_works, publication_year == 2022)

#### output to CSV. File is too big.  
# banner2_works_2020_df <- lapply(banner2_works_2020, function(x) if(is.list(x)) toString(x) else x)
# class(banner2_works_2020_df)
# write.csv(banner2_works_2020_df, file = "banner2_works_2020.csv", row.names = FALSE, fileEncoding = "UTF-8")

### banner3: 
banner3_works <- oa_fetch (entity = "works", authorships.institutions.ror="01cjjjf51", verbose = TRUE)
banner3_works_2020 <- subset(banner3_works, publication_year == 2020)
banner3_works_2021 <- subset(banner3_works, publication_year == 2021)
banner3_works_2022 <- subset(banner3_works, publication_year == 2022)

banner3_works_2020_df <-as.data.frame(banner3_works_2020)
write_xlsx(banner3_works_2020_df, "banner3_works_2020.xlsx")

duplicates3 <-banner3_works_2020$display_name %in% banner2_works_2020$display_name
duplicates4 <-banner4_works_2020$display_name %in% banner3_works_2020$display_name
duplicates5 <-banner4_works_2020$display_name %in% banner2_works_2020$display_name

################################################################################
#############################################################################
### Define a custom function to fetch and subset data
fetch_ror_year <- function(ror_id, year) {
  # Fetch data for the given ROR ID
  data <- oa_fetch(entity = "works", authorships.institutions.ror = ror_id, verbose = TRUE)
  # Subset data for the specified year
  subset_data <- subset(data, publication_year == year)
  
  return(subset_data)
}


### Define a function to find a string in a column 



# find all the records by ROR and year. 
# Ran on 2023-10-06:
# Banner Health
# Note: b[0-9] number is based on the row number on the banner_healty_entity.xlsx 
b2_works_2020 <- fetch_ror_year("039wwwz66", 2020)  # 2023-10-06: 92 
# Banner – University Medical Center Phoenix
b3_works_2020 <- fetch_ror_year("01cjjjf51", 2020)  # 2023-10-06: 139


b6_works_2020 <- fetch_ror_year("023jwkg52", 2020) # 2023-10-06: 0




b13_works_2020 <- fetch_ror_year("00sr2h055", 2020) # 2023-10-06: 0

b15_works_2020 <- fetch_ror_year("01jjm6w53", 2020) # 4

b16_works_2020 <- fetch_ror_year("04mvgap27", 2020) # 3
# Banner Estrella Medical Center
b17_works_2020 <- fetch_ror_year("05ct0ag17", 2020) # 11 

b19_works_2020 <- fetch_ror_year("05gfbdk85", 2020) # 0
b20_works_2020 <- fetch_ror_year("049c9q337", 2020) # 0


b23_works_2020 <- fetch_ror_year("03y8jje75", 2020) # 1

b25_works_2020 <- fetch_ror_year("03vq5n859", 2020) # 0




b29_works_2020 <- fetch_ror_year("02s49nq19", 2020) # 0

b31_works_2020 <- fetch_ror_year("033a24x98", 2020) # 0

# Banner Sun Health Research Institute
# check "Highly Sensitive and Multiplexed In-Situ Protein Profiling with Cleavable Fluorescent Streptavidin". why included? 
b35_works_2020 <- fetch_ror_year("04gjkkf30", 2020) # 55


# Banner Thunderbird Medical Center
b37_works_2020 <- fetch_ror_year("01kqrgb09", 2020) # 18 

# Banner - University Medical Center Tucson
b39_works_2020 <- fetch_ror_year("02xbk5j62", 2020) # 2023-10-13: 243 


b42_works_2020 <- fetch_ror_year("035dcj063", 2020) # 4





b51_works_2020 <- fetch_ror_year("05e33tw76", 2020) # 6
b53_works_2020 <- fetch_ror_year("01phkkj35", 2020) # 4

#### Merge all the units' df
banner_works_2020 <- rbind(b2_works_2020, b3_works_2020, b15_works_2020, b16_works_2020, b17_works_2020)
# Continue to merge b20 - b50 
banner_works_2020 <- rbind(banner_works_2020, b23_works_2020, b29_works_2020, b35_works_2020, b37_works_2020, b39_works_2020, b42_works_2020, b51_works_2020, b53_works_2020)
# This is the final. 2023-10-10: 554 
all_banner_works_openalex_2020 <- unique(banner_works_2020)

### Verify if "Banner" "Banner Health" in author
###################################################################

# Assuming all_banner_works_2020$author is a list of data frames
column_name <- "institution_display_name"

# Use sapply() to check the class of the specified column in each data frame
column_classes <- sapply(all_banner_works_openalex_2020$author, function(df) {
  class(df[[column_name]])
})

# Assuming all_banner_works_2020$author is a list of data frames
word_to_find <- "Banner"

# Use lapply() to search for the word in each data frame within the list
word_found_list <- lapply(all_banner_works_openalex_2020$author, function(df) {
  grepl(word_to_find, df$institution_display_name)
})

# Assuming word_found_list is a list containing logical vectors
# Check if the first logical vector contains at least one "TRUE" value
contains_true <- any(word_found_list[[1]])
# The 'contains_true' variable will be TRUE if at least one "TRUE" is present, otherwise FALSE

contains_true_list <- sapply(word_found_list, function(vector) {
  any(vector)
})
# The 'contains_true_list' will be a logical vector indicating whether each vector contains at least one "TRUE"
# Find the row numbers where contains_true_list is FALSE
false_rows <- which(!contains_true_list)


# Assuming you have two data frames: b2_works_2020 and b3_works_2020
# Find duplicate rows in b2_works_2020 and b3_works_2020
duplicates_b2 <- banner_works_2020[duplicated(banner_works_2020), ]
duplicates_b3 <- all_banner_works_openalex_2020[duplicated(all_banner_works_2020), ]

write_xlsx(duplicates_b2, "final_banner_collab_works_2020.xls")

# Output to xls 
getwd()
setwd("/home/yhan/Documents/UA-datasets/openalexR-test")
write_xlsx(all_banner_works_openalex_2020, "final_banner_works_2020.xls")

#######################################################################
###################### compare openAlex data with Scopus data #########
#######################################################################

scopus_data <- read.csv("scopus_banner_health_2020.csv")

# we will compare the titles on the two datasets: 
titles_scopus <- scopus_data$Title
titles_openalex <- all_banner_works_openalex_2020$display_name
class(titles_openalex)
is.vector(titles_openalex)

# All titles are converted to lower cases, trim white space and handle special characters. Because if you do not do that, there will be many mis-matches. 
# cannot use tolower() and trimws() along, because of special characters like '. 
# Write my own function to improve results, increasing matching of 10%. Convert to lowercase and remove special characters and whitespace
clean_string <- function(input_str) { 
  cleaned_str <- tolower(gsub("[^A-Za-z0-9]+", "", input_str))
  return(cleaned_str)
}


############################## Common titles 166 ####### 

# 1. Normalized the string to remove special char, white space etc. and save it back to the DF
clean_titles_scopus   <- clean_string(titles_scopus)                   # 556 
clean_titles_openalex <- clean_string(titles_openalex)                 # 357

all_banner_works_openalex_2020$normlized_display_name <- clean_titles_openalex
scopus_data$normalized_display_name <- clean_titles_scopus

common_titles <-intersect(clean_titles_scopus, clean_titles_openalex)  # 166 same ones
distinct_titles <- setdiff(clean_titles_scopus, clean_titles_openalex) # 189 different
print(common_titles) # 166 common ones vs. 141 if using tolower()


########################################### Distinct titles from OpenAlex ######### 
#### Analysis: openAlex has distinct titles of 556 - 166 = 390 (not available from Scopus)

distinct_titles_openalex <- setdiff( titles_openalex, titles_scopus)
class(distinct_titles_openalex)
print(distinct_titles_openalex)
writeLines(distinct_titles_openalex, "distinct_title_openalex.txt")


### scopus distinct titles: 234 
distinct_titles_scopus <- setdiff( titles_scopus, titles_openalex)


############ Testing using scopus_string
matching_titles_scopus_in_openalex <- data.frame()
for (string in ) {
  
  cs_str <- clean_string(titles_scopus)
  co_str <- clean_string(all_banner_works_openalex_2020$display_name)
  matching_rows <- all_banner_works_openalex_2020[grep(cs_str, co_str), ]
  #matching_rows <- all_banner_works_openalex_2020[grep(string, all_banner_works_openalex_2020$display_name), ]
  stinct_titles_openalex_df <- rbind(matching_titles_scopus_in_openalex, matching_rows)
}




war# Initialize an empty data frame to store matching rows
distinct_titles_openalex_df <- data.frame()
for (string in distinct_titles_openalex) {
  matching_rows <- all_banner_works_openalex_2020[grep(string, all_banner_works_openalex_2020$display_name), ]
  distinct_titles_openalex_df <- rbind(distinct_titles_openalex_df, matching_rows)
}
# 
write_xlsx(distinct_titles_openalex_df, "distinct_titles_openalex_df.xls")



### to see record: add "api" to the URL and open it in Firefox:  https://openalex.org/A5063303709  >>>>>>>>> https://api.openalex.org/A5063303709 (since A indicateds "author")
### How to search via title: https://api.openalex.org/works?filter=title.search:patient travel concerns after treatment with 177lu-dotatate 

#######################################################################
distinct_titles_scopus <- setdiff(titles_scopus, titles_openalex)
print(distinct_titles_scopus)
writeLines(distinct_titles_scopus, "distinct_title_scopus.txt")

########################################### Distinct titles from Scopus ######### 
### Analysis: Scopus has 191 unique titles out of 357 (53.5%) (not available from OpenAlex)
# For example. Scopus has this title "Noninvasive Input.." https://doi.org/10.1109/trpms.2020.3010844 
# OpenAlex has this at https://openalex.org/W3045489656 
# It  has the author "Kewei Chen", https://openalex.org/A5063303709   https://orcid.org/0000-0001-8497-3069, which has the following 
# raw_author_name: "Kewei Chen",
# raw_affiliation_string: "Banner Alzheimer’s Institute, Phoenix, Arizona, AZ, USA",
# raw_affiliation_strings: ["Banner Alzheimer’s Institute, Phoenix, Arizona, AZ, USA" ]
### Reason:  the "institutions:" [] is empty. so when I run search on ROR "023jwkg52" , this article will not be found 

### Record [214] "development of the nec-zero toolkit:
# https://openalex.org/W2999936980
### Reason: This record shall not be included. There is no author affiliated with Banner

### Record [205] "risk factors for alzheimer’s disease and related dementia diagnoses in american indians"                                                                                                                                                                                                                                                                                                           
# https://api.openalex.org/works/W3089091526 
# Saner, Don: Banner Alzheimer's institute
# raw_author_name	"Don Saner"
# raw_affiliation_string	"Banner Alzheimer's Institute, Phoenix, AZ."
### Reason: the "institutions:" [] is empty. so when I run search on ROR "023jwkg52" , this article will not be found 

### [172] "patient travel concerns after treatment with 177lu-dotatate"   
# https://openalex.org/W3011305417                                                                                                                                                                                                                                                                                                                                       
# Naraev, Boris: BAnner M.D. Anderson Cancer Center
# raw_affiliation_string	"Banner M.D. Anderson Cancer Center, Gilbert, Arizona."
# the "institutions:" [] is empty
### Reason: Same as the other

### 147: brain imaging measurements of fibrillar amyloid-β burden
# https://openalex.org/W2996615980
# 
#raw_affiliation_strings	
# 0	"Arizona Alzheimer's Consortium, Phoenix, AZ, USA"
#  1	"Banner Alzheimer's Institute, Phoenix, AZ, USA"
# institutions	0	 id	"https://openalex.org/I1279571640" display_name	"Alzheimer's Association"
### Reason: institutions only list 1, while the author has two affiliations! 


### [24] "when it comes to snakebites, kids are little adults: a comparison of adults and children with rattlesnake bites"                                                                                                       
# https://openalex.org/W4205513522
# This is a different case: # BAnner - University Medical Center Phoenix is listed .https://ror.org/01cjjjf51 
### This is in all_banner_works.... DO NOT KNOW WHY? R library has a bug??

### [98] about that leaky ostomy pouch
# MEDSURG NursingVolume 29, Issue 5, Pages 347 and 354September-October 2020 
# OpenAlex does not have this record. (or I cannot find it)

### [119] cottonmouth snake bites reported to the toxic north american snakebite registry 2013–2017
# Scoups: puslished year 2020 with Clinical ToxicologyVolume 58, Issue 3, Pages 178 - 1823 March 2020
# DOI: 10.1080/15563650.2019.1627367
# openalex: published in 2019. 
### Reason: publishing year has discrepancy. 

########################################### Distinct titles from OpenAlex ################
# Analysis: OpenAlex has ~397 distinct titles (where 6 has slightly different character in titles)

# Scoupus is down this morning. I was able to search a few titles from OpenAlex. Scopus has no such data. 

### 1. Clinician Practice Patterns That Result in the Diagnosis of Coccidioidomycosis Before or During Hospitalization
###  https://openalex.org/W3033861128     doi: 10.1093/cid/ciaa739
### OpenAlex: publication year 2020 
### Scopus: publication year 2020 Clinical Infectious DiseasesVolume 73, Issue 7, Pages E1587 - E15931 October 2021
### Journal: June 2020 https://academic.oup.com/cid/article/73/7/e1587/5854736 

### 2. 372. Comparing the Outcome of COVID-19 in Cancer and Non-Cancer Patients: an International Multicenter Study
### https://openalex.org/W3119074239
### Scopus: no data
### PubMed: Oct 2020 https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7776865/ 

#### 3. Antifibrotics in COVID-19 Lung Disease: Let Us Stay Focused 
## https://openalex.org/W3084380850 
#### OpenAlex: Title "Antifibrotics in COVID-19 Lung Disease: Let Us Stay Focused"
### Year: 
### Scopus: slight diff title "Corrigendum: Antifibrotics in COVID-19 Lung Disease: Let Us Stay Focused"
### Frontiers in MedicineOpen AccessVolume 711 March 2021 Article number 604640 
### PubMed: 2020 Sep. https://pubmed.ncbi.nlm.nih.gov/33072773/ 

### Population Level Analysis of Adhesive Small Bowel Obstruction
### OpenAlex: https://openalex.org/W2903107208
### Scopus: No data
### PubMed: https://pubmed.ncbi.nlm.nih.gov/30499802/



# try finding alike titles
install.packages("stringdist")
library(stringdist)
# calculate Jaccard similarity
find_similar_strings <- function(query, string_array, threshold = 0.8) {
  # Calculate Jaccard similarity between the query string and the array
  similarity <- stringdist::stringdistmatrix(query, string_array, method = "cosine")
  # Find indices of similar strings
  similar_indices <- which(similarity > threshold)
  # Extract the similar strings from the array
  similar_strings <- string_array[similar_indices]
  
  return(similar_strings)
}

print(scopus_data_titles[1])
### does not seem working right
similar_string <- find_similar_strings(scopus_data_titles[1], openalex_data_titles)
print(similar_string)

print(common_titles)

####################################################################
search_string_in_array <- function(search_string, string_array) {
  # Use grepl() with ignore.case = TRUE to perform a case-insensitive search
  is_string_present <- grepl(search_string, string_array, ignore.case = TRUE)
  
  # If is_string_present is TRUE, the string is found; if FALSE, it's not found
  if (any(is_string_present)) {
    message(paste(search_string, "is found in the array (case-insensitive search)."))
    return(TRUE)
  } else {
    message(paste(search_string, "is not found in the array (case-insensitive search)."))
    return(FALSE)
  }
}

count =0
for (string in scopus_data_titles) {
  is_found <-search_string_in_array(string, openalex_data_titles)
  if (is_found) {
    count <- count+1
  }
}



###################### Rank institutions by the number of citations ############### 
italy_insts <- oa_fetch(
  entity = "institutions",
  country_code = "it",
  type = "education",
  verbose = TRUE
)
#> Requesting url: https://api.openalex.org/institutions?filter=country_code%3Ait%2Ctype%3Aeducation
#> Getting 2 pages of results with a total of 231 records...

italy_insts |>
  slice_max(cited_by_count, n = 8) |>
  mutate(display_name = forcats::fct_reorder(display_name, cited_by_count)) |>
  ggplot() +
  aes(x = cited_by_count, y = display_name, fill = display_name) +
  geom_col() +
  scale_fill_viridis_d(option = "E") +
  guides(fill = "none") +
  labs(
    x = "Total citations", y = NULL,
    title = "Italian references"
  ) +
  coord_cartesian(expand = FALSE)

########################################################
# U.S. institutions (country_code:us) are classified as educational (type:education)
country_insts <- oa_fetch(
  entity = "institutions",
  country_code = "us",
  type = "education",
  verbose = TRUE
)
#> Requesting url: https://api.openalex.org/institutions?filter=country_code%3Aus%2Ctype%3Aeducation
# 22 pages of results with a total of 4334 records... 

country_insts |>
  slice_max(cited_by_count, n = 40) |>    # U Arizona is ranked as 35
  mutate(display_name = forcats::fct_reorder(display_name, cited_by_count)) |>
  ggplot() +
  aes(x = cited_by_count, y = display_name, fill = display_name) +
  geom_col() +
  scale_fill_viridis_d(option = "E") +
  guides(fill = "none") +
  labs(
    x = "Total citations", y = NULL,
    title = "USA Citation Ranking"
  ) 
  coord_cartesian(expand = FALSE)

  
  ## +++++++++++ Concept cloud 
  concept_cloud <- country_insts %>%
    select(inst_id = id, x_concepts) %>%
    tidyr::unnest(x_concepts) %>%
    filter(level == 1) %>%
    select(display_name, score) %>%
    group_by(display_name) %>%
    summarise(score = sum(score))
  
  pal <- c("black", scales::brewer_pal(palette = "Set1")(5))
  set.seed(1)
  wordcloud::wordcloud(
    concept_cloud$display_name,
    concept_cloud$score,
    scale = c(2, .4),
    colors = pal
  )
  
############################ Filters: # of citations && keyword in the title && sorted #################################
# Get works cited > 50 tiles published 2020-2021, and include the string "bibliometrics analysis" or "AI" in the title.
# Sort the results by total citations in a descending order

works_search <- oa_fetch(
  entity = "works",
  title.search = c("bibliometric analysis", "science mapping"),
  cited_by_count = ">50",
  from_publication_date = "2020-01-01",
  to_publication_date = "2021-12-31",
  sort = "cited_by_count:desc",
  verbose = TRUE
) 
  #> Requesting url: https://api.openalex.org/works?filter=title.search%3Abibliometric%20analysis%7Cscience%20mapping%2Ccited_by_count%3A%3E50%2Cfrom_publication_date%3A2020-01-01%2Cto_publication_date%3A2021-12-31&sort=cited_by_count%3Adesc
  #> Getting 1 page of results with a total of 76 records...
  
  works_search |>
    show_works() |>
    knitr::kable()


#################### Concepts as filter #######################33
  library(gghighlight)
  concept_df <- oa_fetch(
    entity = "concepts",
    level = 1,
    ancestors.id = "https://openalex.org/C86803240", # Biology
    works_count = ">1000000"
  )
  
  concept_df |>
    select(display_name, counts_by_year) |>
    tidyr::unnest(counts_by_year) |>
    filter(year < 2022) |>
    ggplot() +
    aes(x = year, y = works_count, color = display_name) +
    facet_wrap(~display_name) +
    geom_line(linewidth = 0.7) +
    scale_color_brewer(palette = "Dark2") +
    labs(
      x = NULL, y = "Works count",
      title = "Virology spiked in 2020."
    ) +
    guides(color = "none") +
    gghighlight(
      max(works_count) > 244000, 
      label_params = list(nudge_y = 10^5, segment.color = NA)
    )
  #> Warning: Ignoring unknown parameters: linewidth
  #> label_key: display_name


############################ Visualize big journals' topics ########################3
# The package wordcloud needs to be installed to run this chunk
library(wordcloud)

  
concept_cloud <- us_insts |>
  select(inst_id = id, x_concepts) |>
  tidyr::unnest(x_concepts) |>
  filter(level == 1) |>
  select(display_name, score) |>
  group_by(display_name) |>
  summarise(score = sum(score))

pal <- c("black", scales::brewer_pal(palette = "Set1")(5))
set.seed(1)
wordcloud::wordcloud(
  concept_cloud$display_name,
  concept_cloud$score,
  scale = c(2, .4),
  colors = pal
)

# The package ggtext needs to be installed to run this chunk
library(ggtext)

jours <- oa_fetch(
  entity = "venues",
  works_count = ">500000",
  verbose = TRUE
) |>
  filter(is.na(publisher)|!grepl("Elsevier", publisher)) |>
  distinct(display_name, .keep_all = TRUE) |>
  select(jour = display_name, x_concepts) |>
  tidyr::unnest(x_concepts) |>
  filter(level == 0) |>
  left_join(concept_abbrev) |>
  mutate(abbreviation = gsub(" ", "<br>", abbreviation)) |>
  tidyr::complete(jour, abbreviation, fill = list(score = 0)) |>
  group_by(jour) |>
  mutate(
    color = if_else(score > 10, "#1A1A1A", "#D9D9D9"), # CCCCCC
    label = paste0("<span style='color:", color, "'>", abbreviation, "</span>")
  )

jours |>
  ggplot() +
  aes(fill = jour, y = score, x = abbreviation, group = jour) +
  facet_wrap(~jour) +
  geom_hline(yintercept = c(45, 90), colour = "grey90", linewidth = 0.2) +
  geom_segment(
    aes(x = abbreviation, xend = abbreviation, y = 0, yend = 100),
    color = "grey95"
  ) +
  geom_col(color = "grey20") +
  coord_polar(clip = "off") +
  theme_bw() +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.text = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  ggtext::geom_richtext(
    aes(y = 120, label = label),
    fill = NA, label.color = NA, size = 3
  ) +
  scale_fill_brewer(palette = "Set1") +
  guides(fill = "none") +
  labs(y = NULL, x = NULL, title = "Journal clocks")
#> Warning: Ignoring unknown parameters: linewidth




######## Snowball Search #####################

library(ggraph)
library(tidygraph)
#> 
#> Attaching package: 'tidygraph'
#> The following object is masked from 'package:stats':
#> 
#>     filter

snowball_docs <- oa_snowball(
  identifier = c("W1964141474", "W1963991285"),
  verbose = TRUE
)
#> Requesting url: https://api.openalex.org/works?filter=openalex_id%3AW1964141474%7CW1963991285
#> Getting 1 page of results with a total of 2 records...
#> Collecting all documents citing the target papers...
#> Requesting url: https://api.openalex.org/works?filter=cites%3AW1963991285%7CW1964141474
#> Getting 3 pages of results with a total of 473 records...
#> Collecting all documents cited by the target papers...
#> Requesting url: https://api.openalex.org/works?filter=cited_by%3AW1963991285%7CW1964141474
#> Getting 1 page of results with a total of 87 records...

ggraph(graph = as_tbl_graph(snowball_docs), layout = "stress") +
  geom_edge_link(aes(alpha = after_stat(index)), show.legend = FALSE) +
  geom_node_point(aes(fill = oa_input, size = cited_by_count), shape = 21, color = "white") +
  geom_node_label(aes(filter = oa_input, label = id), nudge_y = 0.2, size = 3) +
  scale_edge_width(range = c(0.1, 1.5), guide = "none") +
  scale_size(range = c(3, 10), guide = "none") +
  scale_fill_manual(values = c("#a3ad62", "#d46780"), na.value = "grey", name = "") +
  theme_graph() +
  theme(legend.position = "bottom") +
  guides(fill = "none")

######### error 


######################### N-grams #############################3
library(openalexR)
ngrams_data <- oa_ngrams(  ## Error in oa_ngrams(works_identifier = c("W1964141474", "W1963991285") could not find function "oa_ngrams"
  works_identifier = c("W1964141474", "W1963991285"),
  verbose = TRUE
)
#> Use `{curl}` >= v5.0.0 for a faster implementation of `oa_ngrams`

ngrams_data
#> # A tibble: 2 × 4
#>   id                               doi                              count ngrams
#>   <chr>                            <chr>                            <int> <list>
#> 1 https://openalex.org/W1964141474 https://doi.org/10.1016/j.conb.…  2733 <df>  
#> 2 https://openalex.org/W1963991285 https://doi.org/10.1126/science…  2338 <df>

lapply(ngrams_data$ngrams, head, 3)
#> [[1]]
#>                                        ngram ngram_tokens ngram_count
#> 1                 brain basis and core cause            5           2
#> 2                     cause be not yet fully            5           2
#> 3 include structural and functional magnetic            5           2
#>   term_frequency
#> 1   0.0006637902
#> 2   0.0006637902
#> 3   0.0006637902
#> 
#> [[2]]
#>                                          ngram ngram_tokens ngram_count
#> 1          intact but less accessible phonetic            5           1
#> 2 accessible phonetic representation in Adults            5           1
#> 3       representation in Adults with Dyslexia            5           1
#>   term_frequency
#> 1   0.0003756574
#> 2   0.0003756574
#> 3   0.0003756574

ngrams_data |>
  tidyr::unnest(ngrams) |>
  filter(ngram_tokens == 2) |>
  select(id, ngram, ngram_count) |>
  group_by(id) |>
  slice_max(ngram_count, n = 10, with_ties = FALSE) |>
  ggplot(aes(ngram_count, forcats::fct_reorder(ngram, ngram_count))) +
  geom_col(aes(fill = id), show.legend = FALSE) +
  facet_wrap(~ id, scales = "free_y") +
  labs(
    title = "Top 10 fulltext bigrams",
    x = "Count",
    y = NULL
  )
