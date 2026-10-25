# The same forward and reverse slope per level, computed four ways (experience instruction only, every experiment):
#   1 linked mixed model           (our main model; linked_models.csv)
#   2 mean of per-person slopes    (each person's own OLS slope, averaged; the per-person ANOVA's numbers)
#   3 pooled, people centred       (one OLS over all trials after centring each person on their own means; the ellipse figure)
#   4 pooled, raw ("overall")      (one OLS over all trials, ignoring who gave them)
# Writes slopes_four_ways.csv and slopes_four_ways_<exp>.png.
HERE <- commandArgs(trailingOnly = TRUE)[1]; OUT <- commandArgs(trailingOnly = TRUE)[2]
suppressPackageStartupMessages({ library(dplyr); library(ggplot2); library(tidyr) })
LVS <- list(exp1 = c(2, 6, 8, 12), exp2 = c(2, 10, 20), exp3 = c(2, 10, 20), exp4 = c(3, 5, 7))
lab <- function(id, l) if (id == "exp4") paste("SD", l) else paste(l, "sq.")
th <- theme_minimal(base_size = 13) + theme(panel.grid.minor = element_blank(), legend.position = "bottom", legend.title = element_blank(),
  strip.text = element_text(face = "bold", size = 13), plot.title = element_text(face = "bold", size = 13))
link <- read.csv(file.path(HERE, "linked_models.csv")) %>% filter(condition == "experience")
cells <- read.csv(file.path(HERE, "experience_only_cells.csv"))
M <- c("1 Linked mixed model", "2 Mean of per-person slopes", "3 Pooled trials, people centred", "4 Pooled trials, raw (overall)")
all <- list()
for (id in names(LVS)) {
  d <- read.csv(file.path(HERE, "data", paste0(id, "_clean_conservative.csv"))) %>% filter(condition == "experience") %>%
    group_by(participant_id) %>% mutate(tc = meanVal - mean(meanVal), rc = indexSelected - mean(indexSelected)) %>% ungroup()
  for (l in LVS[[id]]) {
    x <- d %>% filter(noise_level == l); cc <- cells %>% filter(experiment == id, level == l)
    lk <- link %>% filter(experiment == id, level == l)
    all[[length(all) + 1]] <- data.frame(experiment = id, level = l, method = rep(M, each = 2), direction = rep(c("Forward: report on true colour", "Reverse: true colour on report"), 4),
      slope = c(lk$slope[lk$direction == "forward"], lk$slope[lk$direction == "reverse"],
                mean(cc$slope_fwd), mean(cc$slope_rev),
                coef(lm(rc ~ tc, x))[2], coef(lm(tc ~ rc, x))[2],
                coef(lm(indexSelected ~ meanVal, x))[2], coef(lm(meanVal ~ indexSelected, x))[2]))
  }
}
res <- bind_rows(all) %>% mutate(lv = mapply(lab, experiment, level))
write.csv(res, file.path(OUT, "slopes_four_ways.csv"), row.names = FALSE)
COL <- setNames(c("#B03A2E", "#2C6E9B", "#B7790F", "#55616D"), M)
for (id in names(LVS)) {
  r <- res %>% filter(experiment == id) %>% mutate(lv = factor(lv, levels = sapply(LVS[[id]], function(l) lab(id, l))))
  p <- ggplot(r, aes(lv, slope, colour = method, group = method)) + geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
    geom_line(linewidth = 1, position = position_dodge(.25)) + geom_point(size = 2.8, position = position_dodge(.25)) +
    facet_wrap(~ direction, scales = "free_y") + scale_colour_manual(values = COL) + guides(colour = guide_legend(nrow = 2)) +
    labs(x = NULL, y = "Slope", title = "The same slopes computed four ways (experience)") + th
  ggsave(file.path(OUT, paste0("slopes_four_ways_", id, ".png")), p, width = 11, height = 5.4, dpi = 180, bg = "white")
}
print(res %>% filter(experiment == "exp4") %>% select(lv, direction, method, slope) %>% mutate(slope = round(slope, 3)) %>% pivot_wider(names_from = lv, values_from = slope), width = 200)
