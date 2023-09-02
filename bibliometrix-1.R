
install.packages("bibliometrix")
library('bibliometrix')
#> To cite bibliometrix in publications, please use:
#> 
#> Aria, M. & Cuccurullo, C. (2017) bibliometrix: An R-tool for comprehensive science mapping analysis, 
#>                                  Journal of Informetrics, 11(4), pp 959-975, Elsevier.
#>                         
#> 
#> https://www.bibliometrix.org
#> 
#>                         
#> For information and bug reports:
#>                         - Send an email to info@bibliometrix.org   
#>                         - Write a post on https://github.com/massimoaria/bibliometrix/issues
#>                         
#> Help us to keep Bibliometrix free to download and use by contributing with a small donation to support our research team (https://bibliometrix.org/donate.html)
#> 
#>                         
#> To start with the shiny web-interface, please digit:
#> biblioshiny()
#> 
#> ## An example from bibliometrix vignettes

file <- c("https://www.bibliometrix.org/datasets/management1.txt","https://www.bibliometrix.org/datasets/management2.txt")

getwd() 

file <- c("~/documnents/ua-dataset /management1.txt",".txt")
M <- convert2df(file = file, dbsource = "scopus", format = "")

M <- convert2df(file = file, dbsource = "wos", format = "plaintext")

results <- biblioAnalysis(M, sep = ";")

options(width=100)
S <- summary(object = results, k = 10, pause = FALSE)
plot(x = results, k = 10, pause = FALSE)


CR <- citations(M, field = "article", sep = ";")
cbind(CR$Cited[1:10])

CR <- citations(M, field = "author", sep = ";")
cbind(CR$Cited[1:10])

# most frequent local cited authors
CR <- localCitations(M, sep = ";")
CR$Authors[1:10,]

CR$Papers[1:10,]

DF <- dominance(results, k = 10)
DF

# H index
indices <- Hindex(M, field = "author", elements="BORNMANN L", sep = ";", years = 10)

# Bornmann's impact indices:
indices$H

# Bornmann's citations
indices$CitationList

authors=gsub(","," ",names(results$Authors)[1:10])

indices <- Hindex(M, field = "author", elements=authors, sep = ";", years = 50)

indices$H


topAU <- authorProdOverTime(M, k = 10, graph = TRUE)

L <- lotka(results)

# Author Productivity. Empirical Distribution
L$AuthorProd

#> 
#> Converting your wos collection into a bibliographic dataframe
#> 
#> Done!
#> 
#> 
#> Generating affiliation field tag AU_UN from C1:  Done!
#> 
#> 