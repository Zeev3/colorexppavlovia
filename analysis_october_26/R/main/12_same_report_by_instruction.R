# "Under noisier conditions, extreme reports come from less extreme arrays", tested so that the
# NUMBER of extreme reports at each level cannot drive it:
#  - for each report value v in a tail (<= 14 or >= 36) and each level: mean true colour behind v
#  - compare two levels at the SAME v; average the differences over report values with EQUAL weight
#    (each report value counts once, however many trials it has)
#  - only report values with >= MIN trials at both levels
#  - 95% CI by resampling participants (2000 bootstrap samples, via per-person sums)
# Sign: "less_extreme" > 0 = at the noisier level the array behind the same report was nearer the middle.
SP <- commandArgs(trailingOnly = TRUE)[1]
suppressPackageStartupMessages({ library(dplyr); library(tidyr) })
set.seed(1); MIN <- 5; B <- 2000
res <- list(); counts <- list(); pervalue <- list()
for (id in c("exp1", "exp2", "exp3", "exp4")) for (cond in c("average", "experience")) {
  d <- readRDS(file.path(SP, paste0(id, "_conservative.rds")))$data; d <- d[as.character(d$condition) == cond, ]
  noise <- if (id == "exp4") "variance_level" else "array_length"; d$lvl <- as.character(d[[noise]]); lv <- levels(d[[noise]])
  d$tail <- case_when(d$indexSelected <= 14 ~ "low", d$indexSelected >= 36 ~ "high", TRUE ~ NA_character_)
  counts[[paste(id, cond)]] <- d %>% filter(!is.na(tail)) %>% count(lvl, tail) %>% left_join(d %>% count(lvl, name = "all"), by = "lvl") %>% mutate(experiment = id, condition = cond, share = n / all)
  comps <- if (id == "exp4") list(c("3", "5"), c("3", "7"), c("5", "7")) else if (id == "exp1") list(c("2", "12"), c("2", "8")) else list(c("2", "10"), c("2", "20"))
  for (cp in comps) for (side in c("low", "high")) {
    x <- d %>% filter(tail == side, lvl %in% cp) %>% mutate(key = paste(indexSelected, lvl, sep = "|"))
    agg <- x %>% group_by(participant_id, key) %>% summarise(s = sum(meanVal), n = n(), .groups = "drop")
    S <- agg %>% select(-n) %>% pivot_wider(names_from = key, values_from = s, values_fill = 0); N <- agg %>% select(-s) %>% pivot_wider(names_from = key, values_from = n, values_fill = 0)
    pids <- S$participant_id; Sm <- as.matrix(S[, -1]); Nm <- as.matrix(N[, colnames(S)[-1]])
    keys <- colnames(Sm); vals <- sort(unique(as.integer(sub("\\|.*", "", keys))))
    est_fun <- function(w) {
      s <- colSums(Sm * w); n <- colSums(Nm * w)
      less <- sapply(vals, function(v) { a <- paste(v, cp[1], sep = "|"); b <- paste(v, cp[2], sep = "|")
        if (!(a %in% keys && b %in% keys) || n[a] < MIN || n[b] < MIN) return(NA)
        dif <- s[b] / n[b] - s[a] / n[a]; if (side == "high") -dif else dif })
      less
    }
    l0 <- est_fun(rep(1, length(pids))); okv <- !is.na(l0)
    boot <- replicate(B, { w <- tabulate(sample.int(length(pids), replace = TRUE), nbins = length(pids)); l <- est_fun(w); mean(l[okv], na.rm = TRUE) })
    res[[length(res) + 1]] <- data.frame(experiment = id, condition = cond, comparison = paste(cp[2], "vs", cp[1]), tail = side, n_report_values = sum(okv),
      report_range = paste(range(vals[okv]), collapse = "-"), less_extreme = mean(l0[okv]),
      ci_low = unname(quantile(boot, .025, na.rm = TRUE)), ci_high = unname(quantile(boot, .975, na.rm = TRUE)),
      p_two_sided = min(1, 2 * min(mean(boot <= 0, na.rm = TRUE), mean(boot >= 0, na.rm = TRUE))))
    pervalue[[length(pervalue) + 1]] <- data.frame(experiment = id, condition = cond, comparison = paste(cp[2], "vs", cp[1]), tail = side, report = vals[okv], less_extreme = l0[okv])
    message(id, " ", cond, " ", paste(cp, collapse = "-"), " ", side, " done")
  }
}
write.csv(do.call(rbind, res), file.path(SP, "same_report_tests_by_instruction.csv"), row.names = FALSE)
write.csv(bind_rows(counts), file.path(SP, "extreme_counts_by_instruction.csv"), row.names = FALSE)
write.csv(do.call(rbind, pervalue), file.path(SP, "same_report_values_by_instruction.csv"), row.names = FALSE)
