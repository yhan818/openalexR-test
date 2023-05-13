##################################################
######## Author: Yan Han 
######## Date: Jan 16, 2023; Updated: March 15, 2023

# OpenAlex R
# Documentation: https://github.com/massimoaria/openalexR/tree/b3541e4f2695da771f15d0d92a7a050757e67e9c 

# Install the released version of openalexR from CRAN 
install.packages("openalexR")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("knitr")

# Before we go any further, we highly recommend you set openalexR.mailto option so that your requests go to the polite pool for faster response times: 
# yhan: No documentation link to the polite pool. So I am not sure if an email will get you a higher priority to get response??? 
options (openalexR.mailto="yhan@arizona.edu")

# common libraries to add
library(openalexR)
library(dplyr)
library(ggplot2)
library(knitr)

############################# DOI as filter ########################################
# Getting record using DOIs
works_from_dois <- oa_fetch( 
  doi = c("https://doi.org/10.6017/ital.v40i1.12553", "10.1016/j.joi.2017.08.007"),
  entity = "works",
  verbose = TRUE
) 

# show the records
str(works_from_dois, max.level=2) 
head(works_from_dois)
show_works(works_from_dois)

works_from_dois |> 
  show_works() |>
  knitr::kable()

#> Requesting url: https://api.openalex.org/works?filter=doi%3A10.1016%2Fj.joi.2017.08.007%7Chttps%3A%2F%2Fdoi.org%2F10.1093%2Fbioinformatics%2Fbtab727

############################## ORCID as filter ####################################
# Download all works published by a set of authors using ORCIDs
# use author.orcid as a filter
# https://api.openalex.org/authors/https://orcid.org/0000-0001-9518-2684
### NOTE: Not all the works are there, NEED TO discuss with OpenAlex. Most likely the disambigation alg not working well. https://docs.openalex.org/api-entities/authors

works_from_orcids <- oa_fetch(
  entity = "works",
  author.orcid = c("0000-0001-9518-2684"),  
  # Yan Han ORCID: 0000-0001-9518-2684; Not sure why there is only 1 record. I have 14 works at ORCID's website
  # Yan Han OpenAlex ID. https://api.openalex.org/people/A2108267685
  
  # author.orcid = c("0000-0001-6187-6610", "0000-0002-8517-9411"),
  verbose = TRUE  
) 

works_from_orcids |>
  show_works()  |>
  knitr::kable()
#> Requesting url: https://api.openalex.org/works?filter=author.orcid%3A0000-0001-9518-2684%7C0000-0002-8517-9411
# > output to a file 

##### Getting authors' info using their ORCIDs
authors_from_orcids <- oa_fetch(
  entity = "authors",
  # orcid = c("0000-0001-9518-2684"),
  orcid =  c("0000-0001-6187-6610", "0000-0002-8517-9411", "0000-0003-1613-5981") # working right
)

str(authors_from_orcids) # show the object

authors_from_orcids |> 
  show_authors() |>
  knitr::kable()

###################### Author's name ####################################


# au_result <- oa_fetch(entity="author", search = "Phillip Kuo")
# au_result[, c(1,2,10)]


###  use disp_name
# authors_from_names <- oa_fetch(entity = "authors",
authors_from_names <- oa_fetch(entity = "author",
                  
                               search = "Phillip Kuo")  ### Search allowing fuzzy search for middle name
#search ="Mona  Hymel") 
### display_name is an exact match, which does not work for someone having a middle name
# display_name = c("Yan Han", "Trang T. Le"), 

##### Test case 1 UA: Yan Han
# display_name = c("Yan Han"),  has_orcid = TRUE
### TRUE:  "Yan Han" "has_orcid=TRUE" has 16 obs. can work with filtering the institution.
### FALSE: "Yan Han" "has_orcid=FALSE" has 117 obs. but cannot find the real one??? NEED TO discuss

##### Test case 2: iSchool Prof: Hong Cui. No record. In Google Scholar and 
# display_name = c("Hong Cui"), has_orcid = FALSE
### FALSE: "Hong Cui" "has_orcid=TRUE". No record in OpenAlex
### FALSE: "Hong Cui" "has_orcid=FALSE" has 40 obs. No her real record. not "Arizona" 

##### Test case 3: Math Prof: Marek Rychlik
# display_name = c("Marek Rychlik"),  has_orcid = FALSE
### TRUE:  "Marek Rychlik" "has_orcid=FALSE". 

##### Test case 4: Medicine Prof: Bekir Tanriover
# display_name = c("Bekir Tanriover"),  has_orcid = FALSE
### FALSE: "Bekir Tanriover" "has_orcid=FALSE" returns 8 obs. 
### three obs are the same person: https://explore.openalex.org/authors/A4297457107 https://explore.openalex.org/authors/A4222101404 https://explore.openalex.org/authors/A4313773255
### TRUE:  "Bekir Tanriover" "has_orcid=TRUE"  returns 1 obs. with a diff id https://explore.openalex.org/authors/A1710357775 

##### Test case 5: Medicine Prof: Phillip H. Kuo. https://kmap.arizona.edu/map/people/pkuo
# display_name = c("Phillip H Kuo"),  has_orcid = FALSE

### TRUE: "Phillip H. Kuo" "has_orcid=FALSE" return 5 obs. 3 diff. IDs refer to the same person.  "https://openalex.org/A2268313489" 
### FALSE: "Phillip Kuo" "has_orcid=TRUE"  returns 0 obs. 
### FALSE: "Phillip Kuo" "has_orcid=FALSE" returns 0 obs. 
### NOTE: MUST be exactly the same string. 
### "Philiip * Kuo" generates null, "Phillip H. Kuo" returns 5, while "Phillip H Kuo" returns 4. "Phillip Kuo" returns 0.

##### Test case 6: CS Prof: Beichuan Zhang
#display_name = c("Beichuan Zhang"),  has_orcid = FALSE
### TRUE:  "Beichuan Zhang" "has_orcid=FALSE" returns 1 obs.  https://openalex.org/A2345815135
### FALSE: "Beichuan Zhang" "has_orcid=TRUE"  returns 0 obs. 

##### Test case 7: Biomed Eng and ECE Prof: Ali Bilgin
# display_name = c("Ali Bilgin"),  has_orcid = TRUE
### FALSE: "Ali Bilgin" "has_orcid=FALSE" returns 1 obs. https://explore.openalex.org/institutions/I161597582
### TRUE:  "Ali Bilgin" "has_orcid=TRUE"  returns 1 obs. https://explore.openalex.org/authors/A2153928507

##### Test case 8: Middle Eastern an North African Prof: Leila Hudson 
# display_name = c("Leila Hudson"),  has_orcid = FALSE
### FALSE: "Leila Hudson" "has_orcid=FALSE" returns 1 obs. "https://openalex.org/A2170186064" "https://openalex.org/A4344778395" "https://openalex.org/A4348995530"
### TRUE:  "Leila Hudson" "has_orcid=TRUE"  

##### Test case 9: Law school Prof: Mona Hymel
# display_name = c("Mona  Hymel"),  has_orcid = FALSE 
### FALSE: "Mona Hymel" "has_orcid=FALSE" returns  "The list does not contain a valid OpenAlex collection." "Mona L. Hymel" string needs exact match
### TRUE:  "Mona Hymel" "has_orcid=TRUE" returns "The list does not contain a valid OpenAlex collection."
# )

authors_from_names
authors_from_names$id
authors_from_names$affiliation_display_name
grep("Arizona*", authors_from_names$affiliation_display_name, value=TRUE, ignore.case=TRUE) 

authors_from_names |> 
  show_authors() |>
  knitr::kable()
