library(dplyr)

# Set your folder path where all files are located
data_folder <- "/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/exp_data"  # ⬅️ change this to your actual folder path

#Get list of all CSV files
files <- list.files(data_folder, pattern = "\\.csv$", full.names = TRUE)

# Initialize an empty data frame
all_data <- data.frame()

for (file in files) {
  df <- read.csv(file)
  
  # Check that needed columns exist
  required_cols <- c("participant_id", "trial", "fixationTime", "meanVal", 
                     "indexSelected", "array_length", "color", "type", "rt")
  
  if (all(required_cols %in% names(df))) {
    
    # Make sure trial is numeric
    df$trial <- as.numeric(df$trial)
    
    # Filter for relevant rows
    filtered_df <- df %>%
      filter(grepl("response", type, ignore.case = TRUE), trial > 20) %>%
      select(participant_id, trial, fixationTime, meanVal, indexSelected, array_length, color, rt)
    
    # Add to master data
    all_data <- bind_rows(all_data, filtered_df)
  }
}

# Save combined data to CSV
write.csv(all_data, file = file.path(data_folder, "summary_all_participants.csv"), row.names = FALSE)