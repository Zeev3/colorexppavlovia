library(dplyr)
library(readr)
library(purrr)
library(jsonlite)
library(stringr)

data_folder <- "/Users/zeevbenamos/Documents/GitHub/2facolorexp/data/pilot12fac"

files <- list.files(data_folder, pattern = "\\.csv$", full.names = TRUE)

files <- files[
  !grepl(
    "summary_all_participants|data_cleaned|removed_trials|free_text_answers|choice_proportions",
    basename(files)
  )
]

free_text_answers <- map_dfr(files, function(file) {
  
  df <- read_csv(file, show_col_types = FALSE)
  
  if (!all(c("trial_type", "responses") %in% names(df))) {
    return(data.frame())
  }
  
  survey_rows <- df %>%
    filter(
      trial_type == "survey-text",
      !is.na(responses),
      responses != ""
    )
  
  extracted <- map_dfr(seq_len(nrow(survey_rows)), function(i) {
    
    resp <- survey_rows$responses[i]
    
    parsed <- tryCatch(
      fromJSON(resp),
      error = function(e) NULL
    )
    
    if (is.null(parsed)) {
      return(data.frame())
    }
    
    keys <- names(parsed)
    
    strategy_key <- keys[str_detect(keys, "strategy")]
    
    if (length(strategy_key) == 0) {
      return(data.frame())
    }
    
    data.frame(
      source_file = basename(file),
      participant_id = survey_rows$participant_id[i],
      trial_index = survey_rows$trial_index[i],
      strategy_type = strategy_key[1],
      strategy_response = parsed[[strategy_key[1]]]
    )
  })
  
  extracted
})

write_csv(
  free_text_answers,
  file.path(data_folder, "free_text_answers.csv")
)

free_text_answers

free_text_diagnostic <- map_dfr(files, function(file) {
  
  df <- read_csv(file, show_col_types = FALSE)
  
  if (!all(c("trial_type", "responses") %in% names(df))) {
    return(data.frame(
      source_file = basename(file),
      has_trial_type = "trial_type" %in% names(df),
      has_responses = "responses" %in% names(df),
      n_survey_text_rows = NA,
      n_nonempty_responses = NA,
      responses_found = NA_character_
    ))
  }
  
  survey_rows <- df %>%
    filter(trial_type == "survey-text")
  
  data.frame(
    source_file = basename(file),
    has_trial_type = TRUE,
    has_responses = TRUE,
    n_survey_text_rows = nrow(survey_rows),
    n_nonempty_responses = sum(!is.na(survey_rows$responses) & survey_rows$responses != ""),
    responses_found = paste(na.omit(survey_rows$responses), collapse = " | ")
  )
})

free_text_diagnostic %>%
  arrange(n_survey_text_rows, n_nonempty_responses)
free_text_diagnostic %>%
  filter(
    is.na(n_survey_text_rows) |
      n_survey_text_rows == 0 |
      n_nonempty_responses == 0
  )
free_text_diagnostic %>%
  filter(n_nonempty_responses > 0) %>%
  select(source_file, responses_found)
