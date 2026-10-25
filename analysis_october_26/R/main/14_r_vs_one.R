# Is tracking (r) different from 1 at each level? Experience only, every experiment.
# One-sample t-test of the people's own r against 1 (Holm across levels within an experiment).
# Caveat: r cannot exceed 1 and every person's r is below 1, so this test is close to guaranteed;
# it is reported for symmetry with the slope-vs-1 test. Output: r_vs_one.csv.
OUT <- commandArgs(trailingOnly = TRUE)[1]
suppressPackageStartupMessages(library(dplyr))
c <- read.csv(file.path(OUT, "experience_only_cells.csv"))
res <- c %>% group_by(experiment, level) %>% summarise(n = n(), mean_r = mean(r), r_fisher_mean = tanh(mean(z)),
  ci_low = t.test(r)$conf.int[1], ci_high = t.test(r)$conf.int[2], n_below_1 = sum(r < 1), min_r = min(r), max_r = max(r),
  t = unname(t.test(r, mu = 1)$statistic), df = unname(t.test(r, mu = 1)$parameter), p = t.test(r, mu = 1)$p.value, .groups = "drop") %>%
  group_by(experiment) %>% mutate(p_holm = p.adjust(p, "holm")) %>% ungroup()
write.csv(res, file.path(OUT, "r_vs_one.csv"), row.names = FALSE); print(as.data.frame(res), digits = 3)
