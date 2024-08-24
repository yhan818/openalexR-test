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

options (openalexR.mailto="yhan@arizona.edu")
getwd()
setwd("/home/yhan/Documents/UA-datasets/openalexR-test/UA")

# Banner-University Medical Center Tucson
UAUMC.df <-oa_fetch(
  entity="works",
  institutions.ror=c("02xbk5j62"),
  from_publication_date ="2023-01-01")

# Univesrity of Arizona:  https://api.openalex.org/institutions/I138006243

# Just run 5-year data, 50,000 records
# It took about 15 mins to run. 
UAworks1 <-oa_fetch(
  entity="works",
  institutions.ror=c("03m2x1q45"),
  from_publication_date ="2019-01-01"
  #count_only = TRUE
  )

# Since 2013, the query takes 15 mins to run. close to 100,000 records
# Since 2014, close to 86,000 records
UAworks2 <-oa_fetch(
  entity="works",
  institutions.ror=c("03m2x1q45"),
  from_publication_date ="2014-01-01",
  count_only = TRUE
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
