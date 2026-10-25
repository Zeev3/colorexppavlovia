# Raw-data views for every experiment, experience instruction only (PNG; too many cells for editable charts).
#   A  rawA_<exp>.png: where the clean trials are (grey 2-D density, one cell per shade x shade) and the average
#      report at each true colour (red line, 95% CI; per person first, then across people; values with >= 8 people).
#   B  rawB_<exp>.png: signed error (report - true colour) at each true colour, one line per noise level, 95% CI.
#      A downward-sloping line = reports pulled toward the middle; the levels side by side in one panel.
HERE <- commandArgs(trailingOnly = TRUE)[1]; OUT <- commandArgs(trailingOnly = TRUE)[2]
suppressPackageStartupMessages({ library(dplyr); library(ggplot2) })
LVS <- list(exp1 = c(2, 6, 8, 12), exp2 = c(2, 10, 20), exp3 = c(2, 10, 20), exp4 = c(3, 5, 7))
lab <- function(id, l) if (id == "exp4") paste("SD", l) else paste(l, "sq.")
th <- theme_minimal(base_size = 13) + theme(panel.grid.minor = element_blank(), legend.position = "bottom", legend.title = element_blank(),
  strip.text = element_text(face = "bold", size = 13), plot.title = element_text(face = "bold", size = 13), panel.spacing = unit(1.2, "lines"))
for (id in names(LVS)) {
  levs <- sapply(LVS[[id]], function(l) lab(id, l))
  cols <- setNames(if (length(levs) == 4) c("#2E7D4F", "#6C9A3B", "#B7790F", "#B03A2E") else c("#2E7D4F", "#B7790F", "#B03A2E"), levs)
  d <- read.csv(file.path(HERE, "data", paste0(id, "_clean_conservative.csv"))) %>% filter(condition == "experience") %>%
    mutate(lv = factor(sapply(noise_level, function(l) lab(id, l)), levels = levs), tv = round(meanVal), err = indexSelected - meanVal)
  per <- function(v) d %>% group_by(lv, participant_id, tv) %>% summarise(y = mean(.data[[v]]), .groups = "drop") %>% group_by(lv, tv) %>%
    summarise(M = mean(y), se = sd(y) / sqrt(n()), n = n(), .groups = "drop") %>% filter(n >= 8)
  cm <- per("indexSelected"); em <- per("err")
  pA <- ggplot(d, aes(meanVal, indexSelected)) + geom_bin2d(binwidth = c(1, 1)) + scale_fill_gradient(low = "grey94", high = "grey20", guide = "none") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey35") +
    geom_ribbon(data = cm, aes(x = tv, ymin = M - 1.96 * se, ymax = M + 1.96 * se), inherit.aes = FALSE, fill = "#B03A2E", alpha = .28) +
    geom_line(data = cm, aes(tv, M), colour = "#B03A2E", linewidth = 1.1) + facet_wrap(~ lv, nrow = 1) +
    coord_cartesian(xlim = c(0, 50), ylim = c(0, 50), expand = FALSE) + scale_x_continuous(breaks = seq(0, 50, 10)) + scale_y_continuous(breaks = seq(0, 50, 10)) +
    labs(x = "True colour (average of the squares shown)", y = "Report", title = "A. Every clean trial (grey density) and the average report at each true colour (red, 95% CI)") + th
  ggsave(file.path(OUT, paste0("rawA_", id, ".png")), pA, width = 12.1, height = 3.3, dpi = 200, bg = "white")
  pB <- ggplot(em, aes(tv, M, colour = lv, fill = lv)) + geom_hline(yintercept = 0, colour = "grey35") +
    geom_vline(xintercept = mean(d$meanVal), linetype = "dotted", colour = "grey55") +
    geom_ribbon(aes(ymin = M - 1.96 * se, ymax = M + 1.96 * se), alpha = .14, colour = NA) + geom_line(linewidth = 1.1) +
    scale_colour_manual(values = cols) + scale_fill_manual(values = cols) +
    labs(x = "True colour", y = "Report - true colour", title = "B. Error at each true colour (mean, 95% CI)") + th + theme(legend.position = "right")
  ggsave(file.path(OUT, paste0("rawB_", id, ".png")), pB, width = 7.6, height = 3.2, dpi = 200, bg = "white")
  cat(id, "done\n")
}
