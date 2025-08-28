# Script to process multiple experiment data files and extract specific columns
# Required packages
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

# Function to process files
process_experiment_files <- function(input_folder, output_file) {
  # Get list of all CSV files in the specified folder
  csv_files <- list.files(path = input_folder, 
                          pattern = "*.csv", 
                          full.names = TRUE)
  
  # Check if any files were found
  if (length(csv_files) == 0) {
    stop("No CSV files found in the specified folder.")
  }
  
  # Display number of files found
  cat("Found", length(csv_files), "CSV files to process.\n")
  
  # Create empty data frame to store combined results
  combined_data <- data.frame()
  
  # Process each file
  for (file in csv_files) {
    cat("Processing file:", basename(file), "\n")
    
    # Read the CSV file
    tryCatch({
      # Read data and handle potential issues like quoted values
      current_data <- read_csv(file, col_types = cols(.default = col_character()))
      
      # Check if the file has at least 150 rows
      if (nrow(current_data) < 150) {
        cat("Skipping file", basename(file), "as it contains fewer than 150 rows (", nrow(current_data), "rows).\n")
        next
      }
      
      # Select only the columns we want to keep
      if (all(c("participant_id", "trial", "color", "faceIdentity", "fixationTime", 
                "exp_type", "indexSelected", "meanVal", "array_values", "array_length") %in% colnames(current_data))) {
        
        current_data <- current_data %>%
          select(participant_id, trial, color, faceIdentity, fixationTime, 
                 exp_type, indexSelected, meanVal, array_values, array_length) %>%
          # Remove rows with any NA values
          na.omit()
        
        # Log how many rows have been removed because of NA values
        rows_before <- nrow(current_data %>% 
                              select(participant_id, trial, color, faceIdentity, fixationTime, 
                                     exp_type, indexSelected, meanVal, array_values, array_length))
        rows_after <- nrow(current_data)
        rows_removed <- rows_before - rows_after
        
        if (rows_removed > 0) {
          cat("Removed", rows_removed, "rows with NA values from", basename(file), "\n")
        }
        
        # Add to the combined data frame
        combined_data <- bind_rows(combined_data, current_data)
        cat("Added", nrow(current_data), "complete rows from", basename(file), "\n")
        
      } else {
        missing_cols <- setdiff(
          c("participant_id", "trial", "color", "faceIdentity", "fixationTime", 
            "exp_type", "indexSelected", "meanVal", "array_values", "array_length"),
          colnames(current_data)
        )
        warning("File ", basename(file), " is missing these columns: ", 
                paste(missing_cols, collapse = ", "), 
                ". Skipping this file.")
      }
    }, error = function(e) {
      warning("Error reading file ", basename(file), ": ", e$message)
    })
  }
  
  # Check if we have data to save
  if (nrow(combined_data) == 0) {
    stop("No data was extracted from the files. Check if the required columns exist in your CSV files or if all files have fewer than 150 rows.")
  }
  
  # Create directory for output file if it doesn't exist
  output_dir <- dirname(output_file)
  if (!dir.exists(output_dir) && output_dir != ".") {
    dir.create(output_dir, recursive = TRUE)
    cat("Created output directory:", output_dir, "\n")
  }
  
  # Write the combined data to a CSV file
  write_csv(combined_data, output_file)
  
  # Verify the file was created
  if (file.exists(output_file)) {
    cat("Processing complete.\n")
    cat("Combined data saved to:", output_file, "\n")
    cat("Total rows in combined data:", nrow(combined_data), "\n")
  } else {
    stop("Failed to create output file. Check write permissions for directory:", output_dir)
  }
  
  return(combined_data)
}



# Example usage (uncomment and modify to run):
 input_folder <- "/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/parliminery data"  # Replace with your folder path
 output_file <- "combined_experiment_data.csv"  # Name for the output file
 combined_data <- process_experiment_files(input_folder, output_file)

