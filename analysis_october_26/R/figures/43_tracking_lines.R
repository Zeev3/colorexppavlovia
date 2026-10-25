# Tracking as fitted regression lines, styled like the linked-model line graphs: one plot per direction,
# all levels in the same plot, experience instruction only, every experiment.
# Units: z-scores (true colour and report standardised within each person and level). In z units a regression
# slope is the correlation r, so the lines show tracking. Forward: report z on true z. Backward: true z on report z
# (axes swapped). Lines = linked mixed model on the z data:
#   y_z ~ x_z * level + (0 + x_z | participant) + (0 + I(x_z * (level == L)) | participant) ...   (no intercepts: all z means are 0)
# Writes tracking_lines.pptx (editable charts: per experiment, forward then backward) + PNGs + tracking_lines_slopes.csv.
HERE <- commandArgs(trailingOnly = TRUE)[1]; OUT <- commandArgs(trailingOnly = TRUE)[2]
suppressPackageStartupMessages({ library(dplyr); library(ggplot2); library(lme4); library(lmerTest); library(emmeans); library(officer); library(rvg) })
emm_options(lmerTest.limit = 30000, pbkrtest.limit = 30000)
ctrl <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
LVS <- list(exp1 = c(2, 6, 8, 12), exp2 = c(2, 10, 20), exp3 = c(2, 10, 20), exp4 = c(3, 5, 7))
lab <- function(id, l) if (id == "exp4") paste("SD", l) else paste(l, "sq.")
th <- theme_minimal(base_size = 12, base_family = "Calibri") + theme(panel.grid.minor = element_blank(), legend.position = "bottom", legend.title = element_blank(),
  plot.title = element_text(face = "bold", size = 13))
doc <- read_pptx(); slopes <- list()
for (id in names(LVS)) {
  levs <- sapply(LVS[[id]], function(l) lab(id, l))
  cols <- setNames(if (length(levs) == 4) c("#2E7D4F", "#6C9A3B", "#B7790F", "#B03A2E") else c("#2E7D4F", "#B7790F", "#B03A2E"), levs)
  d <- read.csv(file.path(HERE, "data", paste0(id, "_clean_conservative.csv"))) %>% filter(condition == "experience") %>%
    mutate(lv = factor(sapply(noise_level, function(l) lab(id, l)), levels = levs)) %>%
    group_by(participant_id, lv) %>% mutate(zt = as.numeric(scale(meanVal)), zr = as.numeric(scale(indexSelected))) %>% ungroup() %>% filter(is.finite(zt), is.finite(zr))
  contrasts(d$lv) <- contr.sum(nlevels(d$lv))
  for (dir in c("forward", "backward")) {
    x <- if (dir == "forward") "zt" else "zr"; y <- if (dir == "forward") "zr" else "zt"
    dev <- paste(sprintf("(0 + I(%s * (lv == \"%s\")) | participant_id)", x, levs), collapse = " + ")
    m <- suppressWarnings(suppressMessages(lmer(as.formula(sprintf("%s ~ 0 + %s * lv + (0 + %s | participant_id) + %s", y, x, x, dev)), data = d, control = ctrl)))
    tr <- as.data.frame(summary(emtrends(m, ~ lv, var = x), infer = TRUE)); a <- anova(m)[paste0(x, ":lv"), ]
    slopes[[length(slopes) + 1]] <- data.frame(experiment = id, direction = dir, level = tr$lv, slope = tr[[paste0(x, ".trend")]], ci_low = tr$lower.CL, ci_high = tr$upper.CL,
                                               F = a$`F value`, df1 = a$NumDF, df2 = a$DenDF, p = a$`Pr(>F)`)
    lines <- bind_rows(lapply(seq_along(levs), function(k) { xs <- seq(-2, 2, length.out = 30); data.frame(lv = levs[k], x = xs, y = tr[[paste0(x, ".trend")]][k] * xs) }))
    lines$lv <- factor(lines$lv, levels = levs)
    labs_ <- tr %>% transmute(lv = factor(lv, levels = levs), txt = sprintf("%s: %.2f [%.2f, %.2f]", lv, .data[[paste0(x, ".trend")]], lower.CL, upper.CL))
    p <- ggplot(lines, aes(x, y, colour = lv)) + geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") + geom_line(linewidth = 1.2) +
      geom_text(data = labs_, aes(x = -2.4, y = 2.35 - .32 * (as.integer(lv) - 1), label = txt, colour = lv), hjust = 0, size = 3.9, family = "Calibri", show.legend = FALSE) +
      scale_colour_manual(values = cols) + coord_equal(xlim = c(-2.5, 2.5), ylim = c(-2.5, 2.5), expand = FALSE) +
      labs(x = if (dir == "forward") "True colour (z within person and level)" else "Report (z within person and level)",
           y = if (dir == "forward") "Predicted report (z)" else "Predicted true colour (z)",
           title = paste0(if (dir == "forward") "Tracking, forward: report on true colour" else "Tracking, backward: true colour on report", " (experience; slope = r)")) + th
    ggsave(file.path(OUT, sprintf("tracking_%s_%s.png", dir, id)), p, width = 6.4, height = 6.0, dpi = 200, bg = "white")
    doc <- add_slide(doc, layout = "Blank", master = "Office Theme")
    doc <- ph_with(doc, dml(ggobj = p, fonts = list(sans = "Calibri")), location = ph_location(left = 0.6, top = 1.5, width = 6.4, height = 5.4))
    cat(id, dir, "done\n")
  }
}
print(doc, target = file.path(OUT, "tracking_lines.pptx"))
write.csv(bind_rows(slopes), file.path(OUT, "tracking_lines_slopes.csv"), row.names = FALSE)
