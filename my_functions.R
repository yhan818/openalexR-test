#### my_functions.R
### Common functions

##########################################
############# Search Functions #######################
### Search if a publisher is in a DF
# Output the publisher 
# @ return: the indices of the publisher
search_publisher <- function(publisher_string, df) {
  # Find indices where the host_organization contains the publisher string (case insensitive)
  indices_with_string <- which(grepl(publisher_string, df$host_organization, ignore.case = TRUE))
  
  print(df[indices_with_string, ]$host_organization)
  print(df[indices_with_string, ]$id)
  return(indices_with_string)
}

# Example usage:
# publisher_string <- "Brill"
# result_indices <- search_publisher(publisher_string, works_cited_type_articles)

#### Function: search_work_publisher(): 
## Search a work's publisher and output the publisher
# @return: index of the DF
search_work_publisher <- function(search_string, df) {
  # Find indices where the host_organization contains the search string (case insensitive)
  indices_with_string <- which(sapply(df$id, function(x) !is.na(x) && search_string %in% x))
  
  print(df[indices_with_string, ]$host_organization)
  print(indices_with_string)
  return(indices_with_string)
}

# Example usage:
# search_string <- "https://openalex.org/W2944198613"
# result_indices <- search_work_publisher(search_string, works_published)

###############################################################
# Verify any cited work using the function search_references()
# Define the function to search for a string in the referenced_works column and print the output
##############################################3
search_references <- function(search_string, df) {
  indices_with_string <- which(sapply(df$referenced_works, function(x) search_string %in% x))
  print(indices_with_string)
  print(df[indices_with_string, ]$id)
}

# Example usage:
search_string <- "Emerald Publishing"
search_string <- "Brill"
# result_indices <- search_publisher(search_string, works_published)

# Example usage
search_string <- "https://openalex.org/W2176010001"

search_string <- "https://openalex.org/W2944198613"
search_string <- "https://openalex.org/W2465933872"
# indices_with_string <- which(sapply(works_published$referenced_works, function(x) search_string %in% x))


##### Handling works "topic": OpenAlex's new topic has a hierarchical structure:
### domain-field-subfield-topic system (https://docs.google.com/document/d/1bDopkhuGieQ4F8gGNj7sEc8WSE8mvLZS/edit)
### Example: https://api.openalex.org/works/W2944198613 (search for primary_topic: )
## A work may have multiple domain-field-subfield-topic. Primary topic has a number "1" in "i", the 2nd has "2", and so on.
# The function adds 4 new cols: topic, subfield, field, and domain. 
# Default: level = 1, it is primary 
#         level = 2, it is secondry. Most works have 1 to 3 topic-subfield-field-domains, and do not have 4th topic. 

extract_topics_by_level <- function(data, level = 1) {
  
  # --- Input Validation ---
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.")
  }
  if (!("topics" %in% names(data))) {
    stop("The 'topics' column is missing from the input data frame.")
  }
  if (!is.numeric(level) || length(level) != 1 || level < 1 || level != as.integer(level)) {
    stop("'level' must be a positive integer.")
  }
  
  # --- Data Extraction ---
  extracted_data <- data %>%
    select(id, title, topics) %>%
    # Rename the nested 'id' column *before* unnesting
    mutate(topics = map(topics, ~{
      if (is.data.frame(.x) && "id" %in% names(.x)) {
        rename(.x, topic_id = id)
      } else {
        .x
      }
    })) %>%
    unnest(cols = c(topics)) %>%
    filter(i == level) %>%
    # Select the relevant info
    select(id, title, level_name = name, display_name)
  
  # Handle the case where no rows match the level
  if (nrow(extracted_data) == 0) {
    warning(paste("No data found for level", level,
                  ". Returning an empty data frame with appropriate columns."))
    # Create an empty data frame with the correct structure
    empty_df <- data %>%
      select(id, title) %>%  # Keep id and title
      mutate(topic = NA_character_,
             subfield = NA_character_,
             field = NA_character_,
             domain = NA_character_)
    return(empty_df)
  }
  # Pivot wider to create separate columns for each level_name
  extracted_data <- extracted_data %>%
    pivot_wider(id_cols = c(id, title), names_from = level_name, values_from = display_name, values_fn = ~paste(unique(.x), collapse = ", "))
  
  # --- Left Join ---
  final_data <- data %>%
    left_join(extracted_data, by = c("id", "title"))  # Join back to original data
  
  return(final_data)
}

# use: 
# works_cited_type_articles_publisher <- works_cited_type_articles_brill

# primary_topics <- extract_topics_by_level(works_cited_type_articles_publisher, 1)
# second_topics  <- extract_topics_by_level(primary_topics, 2)
# third_topics   <- extract_topics_by_level(works_cited_type_articles_publisher, 3)
# fourth_topics  <- extract_topics_by_level(works_cited_type_articles_publisher, 4)
# fifth_topics   <- extract_topics_by_level(works_cited_type_articles_publisher, 5)

################### Analyze top journals for each publisher ############
# Function to rank top cited journals
# Usage example:
#rank_top_cited_journals(publisher_nature, "so", 10)  # Top 10 cited journals

rank_top_cited_journals <- function(data, journal_col, top_n = 30, output_dir = "citations") {
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data.frame or tibble.")
  }
  if (!is.character(journal_col) || length(journal_col) != 1) {
    stop("Input 'journal_col' must be a single string.")
  }
  if (!(journal_col %in% names(data))) {
    stop("journal_col is not found")
  }
  
  top_cited_journals <- data %>%
    group_by(!!sym(journal_col)) %>%
    summarise(citation_count = n(), .groups = "drop") %>%
    arrange(desc(citation_count)) %>%
    rename("Journal Title" = !!sym(journal_col))
  
  # Print all rows if top_n is NULL or larger than number of rows
  if (is.null(top_n) || top_n >= nrow(top_cited_journals)) {
    print(as.data.frame(top_cited_journals))  # Convert to data.frame and print all
  } else {
    # Print only the requested top_n journals.
    print(head(as.data.frame(top_cited_journals), top_n))
    top_cited_journals <- top_cited_journals %>%
      slice(1:top_n) #keep top_n for writing to file
  }
  
  # --- File Output ---
  # Get the name of the input data frame
  df_name <- deparse(substitute(data))
  
  # Create the output file path
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  output_file <- file.path(output_dir, paste0(df_name, "_top_cited_journals.xlsx"))
  
  # Write to Excel
  tryCatch({
    write_xlsx(list("Top Cited Journals" = as.data.frame(top_cited_journals)), output_file)
    message(paste("Successfully wrote top cited journals to:", output_file))
  }, error = function(e) {
    message(paste("Error writing to Excel:", e))
    print(e)  # Print the full error object
  })
  
  return(top_cited_journals)
}


# Write df to excel files
write_df_to_excel <- function(df, file_path_prefix = "citations/") {
  df_name <- deparse(substitute(df))
  file_name <- paste0(df_name, ".xlsx")
  file_path <- paste0(file_path_prefix, file_name)
  sheet_name <- df_name
  
  # Limit sheet name to 31 characters, replacing invalid characters
  sheet_name <- gsub("[[:punct:]]", "_", sheet_name) # Replace punctuation
  sheet_name <- substr(sheet_name, 1, 31)       # Truncate
  
  tryCatch({
    write_xlsx(df, file_path)
    message(paste("Successfully wrote", df_name, "to", file_path))
  }, error = function(e) {
    message(paste("Error writing", df_name, "to Excel:", e))
    print(e)
  })
}
