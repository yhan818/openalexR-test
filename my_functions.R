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





library(openxlsx)
library(dplyr)

write_df_to_excel <- function(df, file_path_prefix = "citations/", max_chars = 32000) {
  df_name <- deparse(substitute(df))
  file_name <- paste0(df_name, ".xlsx")
  file_path <- paste0(file_path_prefix, file_name)
  
  # Function to process a single value with depth tracking
  process_value <- function(x, max_chars, depth = 0, col_name = "") {
    indent <- paste(rep("  ", depth), collapse = "")
    
    if (is.null(x) || length(x) == 0) {
      return(NA_character_)
    }
    
    tryCatch({
      if (all(is.na(x))) {
        return(NA_character_)
      } else if (col_name == "author") {
        message("\nDEBUG: Processing author data")
        # Extract the data frame from the list
        author_df <- x[[1]]
        message("DEBUG: Number of authors: ", nrow(author_df))
        
        # Process each author
        row_strings <- character(nrow(author_df))
        for(i in 1:nrow(author_df)) {
          # Get specific fields in desired order
          author_info <- c(
            author_df$au_id[i],
            author_df$au_display_name[i],
            author_df$au_orcid[i],
            author_df$author_position[i],
            author_df$is_corresponding[i],
            author_df$au_affiliation_raw[i],
            author_df$institution_id[i],
            author_df$institution_display_name[i],
            author_df$institution_ror[i],
            author_df$institution_country_code[i],
            author_df$institution_type[i],
            author_df$institution_lineage[i]
          )
          row_strings[i] <- paste(author_info, collapse = ": ")
          message("DEBUG: Author ", i, " values: ", row_strings[i])
        }
        
        full_string <- paste(row_strings, collapse = "; ")
        message("DEBUG: Authors final string: ", full_string)
        return(full_string)
        
      } else if (col_name == "topics") {
        message("\nDEBUG: Processing topics data")
        # Extract the data frame from the list
        topics_df <- x[[1]]
        message("DEBUG: Number of topics: ", nrow(topics_df))
        
        # Process each topic row
        row_strings <- character(nrow(topics_df))
        for(i in 1:nrow(topics_df)) {
          topic_values <- c(
            topics_df$i[i],
            topics_df$score[i],
            topics_df$name[i],
            topics_df$id[i],
            topics_df$display_name[i]
          )
          row_strings[i] <- paste(topic_values, collapse = ": ")
          message("DEBUG: Topic ", i, " values: ", row_strings[i])
        }
        
        full_string <- paste(row_strings, collapse = "; ")
        message("DEBUG: Topics final string: ", full_string)
        return(full_string)
        
      } else if (is.data.frame(x)) {
        row_strings <- character(nrow(x))
        for(i in 1:nrow(x)) {
          row_values <- as.character(unlist(x[i,]))
          row_strings[i] <- paste(row_values, collapse = ": ")
        }
        full_string <- paste(row_strings, collapse = "; ")
        return(full_string)
        
      } else if (is.list(x) && !is.data.frame(x)) {
        unlisted <- unlist(x)
        if (is.null(unlisted) || length(unlisted) == 0) {
          return(NA_character_)
        }
        unlisted <- unlisted[!is.null(unlisted) & !is.na(unlisted)]
        if (length(unlisted) == 0) {
          return(NA_character_)
        }
        full_string <- paste(unlisted, collapse = ": ")
        if (nchar(full_string) > max_chars) {
          return(paste0(substr(full_string, 1, max_chars), " [truncated...]"))
        }
        return(full_string)
      } else {
        char_val <- as.character(x)
        if (length(char_val) > 1) {
          char_val <- paste(char_val, collapse = ": ")
        }
        if (nchar(char_val) > max_chars) {
          return(paste0(substr(char_val, 1, max_chars), " [truncated...]"))
        }
        return(char_val)
      }
    }, error = function(e) {
      warning(paste("Error processing value:", e$message))
      return(NA_character_)
    })
  }
  
  # Convert data.table to data.frame if necessary
  if (inherits(df, "data.table")) {
    df <- as.data.frame(df)
  }
  
  # Create output dataframe
  df_processed <- data.frame(matrix(nrow = nrow(df), ncol = ncol(df)))
  colnames(df_processed) <- colnames(df)
  
  # Process each row
  for (i in seq_len(nrow(df))) {
    message(sprintf("\nProcessing main row %d:", i))
    current_row <- df[i, , drop = FALSE]  # Keep as dataframe
    processed_row <- sapply(names(current_row), function(col) {
      result <- process_value(current_row[[col]], max_chars, depth = 1, col_name = col)
      message("DEBUG: Final result for column ", col, ": ", result)
      return(result)
    })
    df_processed[i,] <- processed_row
  }
  
  tryCatch({
    write_xlsx(df_processed, file_path)
    message(paste("Successfully wrote", df_name, "to", file_path))
  }, error = function(e) {
    message(paste("Error writing", df_name, "to Excel:", e))
    print(e)
  })
}