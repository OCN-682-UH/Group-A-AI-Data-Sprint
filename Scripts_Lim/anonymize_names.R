### Anonymizing names
### Created by: Wei Shen Lim
### Created on: 2026-10-27
################################################################

## Load libraries
library(tidyverse)

#Set working directory to your file that has all the private data files

############################### Function 1 ####################################
#Anonymize School names for data files from 2004 to 2020
anonymize_school_data_batch <- function(input_files) {
  
  # Step 1: Scan ALL files combined to build a unified Master Lookup Sheet
  master_lookup <- input_files |> 
    map(read_lines) |>                   #read every file in vector as raw character vector of lines
    flatten_chr() |>                     #merges the split lists of lines into one large, single vector of text strings
    keep(~ str_starts(.x, "School,")) |> #filters lines to keep only rows that begin with "School,"
    str_split(",") |>                    #breaks those comma-separated rows apart into individual cell strings
    flatten_chr() |>                     #flattens nested list of row items into a single flat character vector
    str_trim() |>                        #drops any accidental leading or trailing white spaces from each cell string
    keep(~ .x != "School" & .x != "") |> #drops empty text spaces and the original label "School"
    unique() |>                          #filters items down to keep only distinct school entries
    tibble(original_name = _) |>         #converts final vector into a tidyverse data frame under column "origianl_name"
    mutate(school_id = paste0("School_", row_number())) #creates sequential IDs based on row counts
  
  # Write out the single combined lookup sheet
  write_csv(master_lookup, "combined_school_lookup_2004_2020.csv")
  
  # Step 2: Loop through each file individually to anonymize its text columns
  walk(input_files, function(input_file) {
    # Dynamically generate clean names for the output data files
    base_name <- str_remove(input_file, "\\.[cC][sS][vV]$")    #trims the .csv/.CSV extension off filename string
    data_output <- paste0(base_name, "_anonymized_output.csv") #appends a standardized suffix to create output filename
    
    read_lines(input_file) |> 
      tibble(text_line = _) |> #load raw file lines into a single-column tracking data frame
      rowwise() |>             #compute subsequent mutate changes line-by-line instead of as a block vector
      mutate(
        text_line = if (str_starts(text_line, "School,")) { #checks if row is the target metadata line
          elements <- str_split(text_line, ",")[[1]]        #splits the comma-separated string row into a clean, flat character vector
          
          anon_elements <- map_chr(elements, function(x) { #loops through every column cell inside this text row one by one
            clean_x <- str_trim(x)                         #temporarily cleans up the text cell to evaluate its contents accurately
            if (clean_x == "School" || clean_x == "") { #checks if the cell is part of the blank spacing or the row header label
              return(x)                                 #returns the exact original text string, no modification or substitution
            } else if (clean_x %in% master_lookup$original_name) {                    #checks if the cell value matches an entry in our master tracking key
              return(master_lookup$school_id[master_lookup$original_name == clean_x]) #swaps out original name for its matching ID
            } else {
              return(x)                                 #default fallback: returns original cell text if no conditions are matched
            }
          })
          
          paste(anon_elements, collapse = ",") #glues the individual anonymized column cells back into a unified CSV row string
        } else {
          text_line                            #conditional fallback: leaves all rows not starting with "School," completely unmodified
        }
      ) |> 
      ungroup() |>             #dissolves the rowwise grouping structure to return the data frame to its normal state
      pull(text_line) |>       #extracts the text columns back out into a raw vector of layout text lines
      write_lines(data_output) #saves final structural text file to working directory
  })
}

## Test function 1
old_files <- c("2004_datasheet.csv", "2005_datasheet.csv", "2006_and_2007_datasheet.csv", "2008_datasheet.csv", "2016_datasheet.csv", "2017_datasheet.csv", "2018_datasheet.csv", "2019_datasheet.csv", "2020_datasheet.csv")

# Run the entire batch operation in one go
anonymize_school_data_batch(old_files)

############################### Function 2 ####################################
#Anonymize student/teacher names for data files from 2024 to 2026.
anonymize_data_entry_batch <- function(input_files) {
  
  # Step 1: Scan all files to extract unique "Data Entry By" names for the Master Lookup
  master_lookup <- input_files |> 
    map_df(~ {                                          #maps across files and row-binds the resulting data frames together automatically
      read_csv(.x, skip = 1, show_col_types = FALSE) |> #skips row 1, captures actual column header strings on row 2
        select(`Data Entry By`)                         #keeps only "Data Entry By" column, discards all other
    }) |> 
    drop_na(`Data Entry By`) |>                            #filters out missing cells or blank data entries from the table rows
    mutate(`Data Entry By` = str_trim(`Data Entry By`)) |> #strips off accidental leading or trailing white spaces from names
    filter(`Data Entry By` != "") |>                       #filters out any rows where the name is a blank text string ("")
    distinct() |>                                          #drops any duplicate names extracted across multiple days or files
    rename(original_name = `Data Entry By`) |>             #renames column to "original_name" to serve as a clean reference column
    mutate(coder_id = paste0("Coder_", row_number()))      #generates sequential IDs based on row numbers in this unique list
  
  # Save out the unified master lookup table
  write_csv(master_lookup, "combined_data_entry_lookup_2024_2026.csv")
  
  # Step 2: Loop back through each file, replace names, and write out cleanly
  walk(input_files, function(input_file) {
    base_name <- str_remove(input_file, "\\.[cC][sS][vV]$")    #trims the .csv/.CSV extension off filename string
    data_output <- paste0(base_name, "_anonymized_output.csv") #appends a standardized suffix to create output filename
    
    # Read the banner row separate to preserve it at the top of our output file
    banner_row <- read_lines(input_file, n_max = 1)
    
    # Read the main data matrix
    data_matrix <- read_csv(input_file, skip = 1, show_col_types = FALSE) |> #loads data grid from row 2 down
      mutate(`Data Entry By` = str_trim(`Data Entry By`)) |>                 #strips off accidental leading or trailing white spaces from names
      left_join(master_lookup, by = c("Data Entry By" = "original_name")) |> #merges master lookup key into this file by matching local names to "original_name"
      mutate(`Data Entry By` = coder_id) |>                                  #overwrites original names with anonymized IDs
      select(-coder_id)                                                      #removes the temporary helper "coder_id" column generated by the merge step
    
    # Write the banner row back manually, then append the anonymized dataset
    write_lines(banner_row, data_output)
    write_csv(data_matrix, data_output, append = TRUE, col_names = TRUE) #appends modified data table with its column headers directly underneath that first line/row
  })
}

## Test function 2
recent_files <- c("2024_datasheet.csv", "2024_datasheet_copy.csv", "2025_datasheet_BIOL499.csv", "2026_datasheet.csv", "2026_datasheet_Mauliola.csv")

# Run the entire batch operation in one go
anonymize_data_entry_batch(recent_files)

############################### Function 3 ####################################
#Anonymize team leaders and members' names for metadata files from 2025 & 2026
anonymize_team_batch <- function(input_files) {
  
  # Helper to isolate individual names from multi-person strings safely
  extract_individual_names <- function(text_vector) {
    text_vector |> 
      str_split(",|\\band\\b") |>                                #splits strings by commas or the word "and"
      flatten_chr() |>                                           #condenses the list arrays down into a single flat vector of discrete string pieces
      str_trim() |>                                              #drops hidden whitespaces surrounding the freshly separated text names
      keep(~ !.x %in% c("", "N/A", "NA", "N/A,", "N/A ", NA)) |> #filters out placeholder text, missing cells, and artifact strings
      unique()                                                   #eliminates duplicate names, each individuals get one permanent ID
  }
  
  # Step 1: Scan all files to extract every unique name across both target columns
  master_lookup <- input_files |> 
    map_df(~ {                                   #loops over metadata files and unifies outputs into a combined data frame
      df <- read_csv(.x, show_col_types = FALSE) #reads standard data frames natively from disk paths
      
      # Select target columns dynamically using loose regex names
      tl_col <- names(df)[str_detect(str_to_lower(names(df)), "team\\s*leader")]
      tm_col <- names(df)[str_detect(str_to_lower(names(df)), "team\\s*member")]
      
      # Extract raw contents from whatever matching columns exist
      raw_tl <- if(length(tl_col) > 0) df[[tl_col[1]]] else character() #grabs content if column exists, otherwise provides a default blank vector
      raw_tm <- if(length(tm_col) > 0) df[[tm_col[1]]] else character() #grabs field array if present, otherwise provides a default blank vector
      
      # Packages all found name fields into a temporary data frame stack
      tibble(raw_names = c(as.character(raw_tl), as.character(raw_tm))) 
    }) |> 
    pull(raw_names) |>                                  #converts unified list column back into character vector for extraction processing
    extract_individual_names() |>                       #processes the composite strings to capture separate identities using our nested helper function
    tibble(original_name = _) |>                        #converts final atomic name list into a master lookup dataframe column
    mutate(person_id = paste0("Person_", row_number())) #grants a permanent ID to each distinct person
  
  # Write out the single unified master lookup sheet
  write_csv(master_lookup, "combined_team_lookup_metadata.csv")
  
  # Step 2: Loop back through files and replace names inside their text contexts
  walk(input_files, function(input_file) {
    base_name <- str_remove(input_file, "\\.[cC][sS][vV]$")    #trims the .csv/.CSV extension off filename string
    data_output <- paste0(base_name, "_anonymized_output.csv") #appends a standardized suffix to create output filename
    
    # Read the data natively
    df <- read_csv(input_file, show_col_types = FALSE)
    
    # Identify target columns again for this specific file
    tl_col <- names(df)[str_detect(str_to_lower(names(df)), "team\\s*leader")]
    tm_col <- names(df)[str_detect(str_to_lower(names(df)), "team\\s*member")]
    
    # Internal text replacement logic that keeps formatting structures intact
    replace_names_in_text <- function(cell_value) {
      #immediately returns unmapped if field is empty or missing
      if (is.na(cell_value) || str_trim(cell_value) %in% c("", "N/A", "NA")) return(cell_value)
      
      # Capture the structural delimiters used (commas vs 'and')
      has_and <- str_detect(cell_value, "\\band\\b")
      
      # Extract the separate elements
      #splits compound cell contents into individual names character array and clean spaces
      parts <- str_split(cell_value, ",|\\band\\b")[[1]] |> str_trim()
      
      # Convert each piece using the master key
      anon_parts <- map_chr(parts, function(p) {
        if (p %in% master_lookup$original_name) {                   #checks if individual name exists in master lookup key
          master_lookup$person_id[master_lookup$original_name == p] #extracts the matching ID from master catalog where the original name matches
        } else {
          p                                                         #fallback: if name is not in lookup data, leaves the cell string as is
        }
      })
      
      # Stitch back together using the original delimiter style
      if (has_and && length(anon_parts) == 2) { #checks if "and" was originally used and exactly 2 items are present
        paste(anon_parts, collapse = " and ")   #combines the two IDs back together using original spacing format
      } else {
        paste(anon_parts, collapse = ", ")      #fallback: combines multiple IDs back together into clean, comma-separated list
      }
    }
    
    # Apply substitutions across the active target columns
    if (length(tl_col) > 0) {                                            #checks if a column matching "Team Leader" actually exists
      df[[tl_col[1]]] <- map_chr(df[[tl_col[1]]], replace_names_in_text) #updates column by applying the name-swapping function to every row cell
    }
    if (length(tm_col) > 0) {                                            #checks if a column matching "Team Members" actually exists
      df[[tm_col[1]]] <- map_chr(df[[tm_col[1]]], replace_names_in_text) #updates column by applying the name-swapping function to every row cell
    }
    
    # Save the clean table row format
    write_csv(df, data_output)
  })
}

## Test function 3
metadata_files <- c("2025_metadata_BIOL499.csv", "2026_metadata.csv", "2026_metadata_Mauliola.csv")

# Run the entire batch operation in one go
anonymize_team_batch(metadata_files)
