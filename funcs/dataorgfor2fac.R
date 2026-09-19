library(dplyr)
library(readr)
library(jsonlite)

data_folder <- "/Users/zeevbenamos/Documents/GitHub/2facolorexp/data/pilot12fac"

files <- list.files(data_folder, pattern = "\\.csv$", full.names = TRUE)
files <- files[!grepl("summary_all_participants\\.csv$", files)]

all_data <- data.frame()
participant_file_check <- lapply(files, function(file) {
  
  df <- read_csv(file, show_col_types = FALSE)
  
  df %>%
    filter(Name == "response", is.na(is_practice), !is.na(color), color != "") %>%
    summarise(
      source_file = basename(file),
      participant_id = first(participant_id),
      n_rows = n()
    )
}) %>%
  bind_rows()

participant_file_check %>%
  count(participant_id) %>%
  filter(n > 1)

for (file in files) {
  
  df <- read_csv(file, show_col_types = FALSE)
  
  required_cols <- c(
    "participant_id", "trial_index", "Name", "rt",
    "is_practice", "meanVal", "color",
    "chosenSide", "chosenIdx", "leftIdx", "rightIdx",
    "choseHigher", "chosenSigned", "delta",
    "array_values", "config"
  )
  
  if (all(required_cols %in% names(df))) {
    
    filtered_df <- df %>%
      filter(
        Name == "response",
        is.na(is_practice),      # removes practice trials
        !is.na(color),
        color != ""
      ) %>%
      mutate(
        trial = row_number(),
        array_length = as.numeric(gsub(
          '.*"array_length":([0-9]+).*',
          "\\1",
          config
        )),
        indexSelected = chosenIdx
      ) %>%
      select(
        participant_id,
        trial,
        trial_index,
        meanVal,
        indexSelected,
        chosenSide,
        chosenIdx,
        leftIdx,
        rightIdx,
        choseHigher,
        chosenSigned,
        delta,
        array_length,
        array_values,
        color,
        rt
      )
    
    all_data <- bind_rows(all_data, filtered_df)
  }
}

all_data <- all_data %>%
  arrange(participant_id, trial) %>%
  distinct(participant_id, trial, .keep_all = TRUE)

write_csv(
  all_data,
  file.path(data_folder, "summary_all_participants.csv")
)

all_data %>%
  count(participant_id) %>%
  arrange(n)