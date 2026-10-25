# Export the cleaned, corrected trial data (both instructions) to plain CSVs: output/data/<exp>_clean_conservative.csv.
# Columns: participant_id, condition (average / experience), noise_level (squares in Exps 1-3, SD in Exp 4), trial,
# meanVal (true colour = average of the squares shown), indexSelected (report, + 1 corrected).
OUT <- commandArgs(trailingOnly = TRUE)[1]; dir.create(file.path(OUT, "data"), showWarnings = FALSE)
for (id in c("exp1", "exp2", "exp3", "exp4")) {
  d <- readRDS(file.path(OUT, paste0(id, "_conservative.rds")))$data
  d$noise_level <- if (id == "exp4") d$variance_level else d$array_length
  write.csv(as.data.frame(d)[, c("participant_id", "condition", "noise_level", "trial", "meanVal", "indexSelected")],
            file.path(OUT, "data", paste0(id, "_clean_conservative.csv")), row.names = FALSE)
  cat(id, nrow(d), "trials\n")
}
