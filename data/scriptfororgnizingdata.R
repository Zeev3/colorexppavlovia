library(dplyr)

data_folder <- "/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/greaternoise/colorsquersexpgreaternoise"

# List CSV files, but avoid re-reading the summary file if it exists
files <- list.files(data_folder, pattern = "\\.csv$", full.names = TRUE)
files <- files[!grepl("summary_all_participants\\.csv$", files)]

all_data <- data.frame()

for (file in files) {
  df <- read.csv(file)
  
  required_cols <- c("participant_id", "trial", "fixationTime", "meanVal", 
                     "indexSelected", "array_length", "color", "type", "rt")
  
  if (all(required_cols %in% names(df))) {
    
    df$trial <- as.numeric(df$trial)
    
    filtered_df <- df %>%
      filter(
        grepl("response", type, ignore.case = TRUE),
        !is.na(color),
        color != ""
      ) %>%
      select(
        participant_id, trial, fixationTime,
        meanVal, indexSelected, array_length,
        color, rt
      )
    
    all_data <- bind_rows(all_data, filtered_df)
  }
}

# Ensure max 1 row per participant × trial
all_data <- all_data %>%
  arrange(participant_id, trial) %>%
  distinct(participant_id, trial, .keep_all = TRUE)

write.csv(
  all_data,
  file = file.path(data_folder, "summary_all_participants.csv"),
  row.names = FALSE
)
 all_data %>%
      count(participant_id) %>%
       arrange(n)
