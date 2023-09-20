##################################################
######## Author: Yan Han 
######## Date: Jan 16, 2023; Updated: May 3, 2023

# OpenAlex R
# Documentation: https://github.com/ropensci/openalexR

install.packages("openalexR")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("knitr")

# Before we go any further, we highly recommend you set openalexR.mailto option so that your requests go to the polite pool for faster response times: 
options (openalexR.mailto="yhan@arizona.edu")

# common libraries to add
library(openalexR)
library(dplyr)
library(ggplot2)
library(knitr)

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

author_from_openalex_id <-oa_fetch(entity = "author",
                                   openalex = "A4353996111" )


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

### Fitering using name "University of Arizona" only (which is in the above openAlex id and ror record showing "display_name" as "University of Arizona")




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
