############# Institution authors publication analysis and Collection Management ##########
######## Author: Yan Han
######## Date: Aug 23, 2024
######## Updated: 
##### Search an institution authors' publication using openAlex data ####
# OpenAlex R Documentation: https://github.com/ropensci/openalexR

install.packages("remotes")
remotes::install_github("ropensci/openalexR", force=TRUE) 
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")

library(openalexR)
library(dplyr)
library(tidyr)
library(tidyverse)

options (openalexR.mailto="yhan@arizona.edu")
getwd()
setwd("/home/yhan/Documents/UA-datasets/openalexR-test/UA")

# Banner-University Medical Center Tucson
UAUMC.df <-oa_fetch(
  entity="works",
  institutions.ror=c("02xbk5j62"),
  from_publication_date ="2023-01-01")

# Univesrity of Arizona:  https://api.openalex.org/institutions/I138006243

UArizona_works <-oa_fetch(
  entity="works",
  #authorships.affiliations.institution_ids = "I138006243"
  #affiliation_ror=c("03m2xlq45"),
  institutions.ror=c("03m2xlq45"),
  from_publication_date ="2013-01-01"
  )


# Will take about 2 mins to run. 
UArizona_works2 <- oa_fetch(
  entity = "works",
  # UA campus repository ID does not work as a filter
  best_oa_location.source.host_organization = "https://openalex.org/I138006243",
  from_publication_date ="2013-01-01",
  # If only need count, uncomment the below line for a quick run.   
  count_only = TRUE
  # If only need some samples. using the below line.
  # options = list(sample = 100, seed = 1)
)
