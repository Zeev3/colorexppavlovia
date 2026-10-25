# Experience-instruction-only figures for both decks (editable rvg graphics + raw-data PNGs).
# Data: analysis_conservative/data/<exp>_clean_conservative.csv and the experience-only results.
# Writes figures_experience.pptx (editable charts, one per slide) + figures_experience_index.csv + rawx_<exp>.png in OUTD.
args <- commandArgs(trailingOnly = TRUE); AC <- args[1]; OUTD <- args[2]
suppressPackageStartupMessages({ library(officer); library(rvg); library(ggplot2); library(dplyr); library(tidyr); library(patchwork); library(lme4); library(lmerTest); library(emmeans) })
emm_options(lmerTest.limit = 30000, pbkrtest.limit = 30000)
ctrl <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
EXPC <- "#B03A2E"; LV3 <- c("#2E7D4F", "#B7790F", "#B03A2E"); LV4 <- c("#2E7D4F", "#6C9A3B", "#B7790F", "#B03A2E")
th <- theme_minimal(base_size = 12, base_family = "Calibri") + theme(panel.grid.minor = element_blank(), legend.position = "bottom", legend.title = element_blank(),
  plot.title = element_text(face = "bold", size = 13), strip.text = element_text(face = "bold", size = 12), panel.spacing = unit(1.4, "lines"))
figs <- list(); add <- function(name, p, l, t, w, h) figs[[name]] <<- list(p = p, loc = c(l, t, w, h))
lvlab <- function(id, l) if (id == "exp4") paste("SD", l) else paste(l, "sq.")
LVS <- list(exp1 = c(2, 6, 8, 12), exp2 = c(2, 10, 20), exp3 = c(2, 10, 20), exp4 = c(3, 5, 7))
dat <- function(id) read.csv(file.path(AC, "data", paste0(id, "_clean_conservative.csv"))) %>% filter(condition == "experience") %>%
  mutate(level = factor(noise_level, levels = LVS[[id]]), lv = factor(sapply(noise_level, function(l) lvlab(id, l)), levels = sapply(LVS[[id]], function(l) lvlab(id, l))))
cells <- read.csv(file.path(AC, "experience_only_cells.csv"))
s1 <- read.csv(file.path(AC, "slope_vs_one.csv"))
link <- read.csv(file.path(AC, "linked_models.csv")) %>% filter(condition == "experience")
cols <- function(id) if (id == "exp1") LV4 else LV3

line_plot <- function(id, d, dir) {
  x <- if (dir == "forward") "true_c" else "resp_c"; y <- if (dir == "forward") "indexSelected" else "meanVal"; raw <- if (dir == "forward") "meanVal" else "indexSelected"
  d <- d %>% group_by(participant_id) %>% mutate(true_c = meanVal - mean(meanVal), resp_c = indexSelected - mean(indexSelected)) %>% ungroup()
  contrasts(d$level) <- contr.sum(nlevels(d$level))
  drp <- strsplit(as.character((link %>% filter(experiment == id, direction == dir))$dropped[1]), ",")[[1]]; keep <- setdiff(levels(d$level), drp)
  base <- if (dir == "forward") sprintf("(1 + %s | participant_id)", x) else sprintf("(0 + %s | participant_id)", x)
  f <- as.formula(paste(sprintf("%s ~ %s * level +", y, x), paste(c(base, if (length(keep)) sprintf("(0 + I(%s * (level == \"%s\")) | participant_id)", x, keep)), collapse = " + ")))
  m <- suppressWarnings(suppressMessages(lmer(f, data = d, control = ctrl)))
  centre <- d %>% group_by(participant_id) %>% summarise(c = mean(.data[[raw]])) %>% pull(c) %>% mean()
  out <- bind_rows(lapply(levels(d$level), function(l) { r <- d[[raw]][d$level == l]; xs <- seq(quantile(r, .02), quantile(r, .98), length.out = 30) - centre
    em <- as.data.frame(summary(emmeans(m, as.formula(paste("~", x, "| level")), at = setNames(list(xs, l), c(x, "level")), lmer.df = "asymptotic")))
    data.frame(level = l, x = em[[x]] + centre, y = em$emmean) }))
  out$lv <- factor(sapply(out$level, function(l) lvlab(id, l)), levels = levels(d$lv))
  ggplot(out, aes(x, y, colour = lv)) + geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") + geom_line(linewidth = 1.2) +
    scale_colour_manual(values = cols(id)) + coord_equal(xlim = c(0, 50), ylim = c(0, 50), expand = FALSE) +
    scale_x_continuous(breaks = seq(0, 50, 10)) + scale_y_continuous(breaks = seq(0, 50, 10)) +
    labs(x = if (dir == "forward") "True colour (displayed average)" else "Report", y = if (dir == "forward") "Predicted report" else "Predicted true colour",
         title = paste0(if (dir == "forward") "Regression lines: report on true colour" else "Regression lines: true colour on report", " (experience)")) + th
}
slope_plot <- function(id, dir) {
  s <- s1 %>% filter(experiment == id, direction == dir, source == "linked model") %>% mutate(lv = factor(sapply(level, function(l) lvlab(id, l)), levels = sapply(LVS[[id]], function(l) lvlab(id, l))))
  ggplot(s, aes(lv, slope, group = 1)) + geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = .1, colour = EXPC) + geom_line(colour = EXPC, linewidth = .9) + geom_point(colour = EXPC, size = 2.4) +
    labs(x = NULL, y = "Slope (95% CI)", title = paste(if (dir == "forward") "Forward" else "Reverse", "slope per level (experience)")) + th
}
galton_plot <- function(id, d, dir) {
  g <- if (dir == "forward") d %>% mutate(v = round(meanVal)) %>% group_by(lv, participant_id, v) %>% summarise(o = mean(indexSelected), .groups = "drop") else
                             d %>% group_by(lv, participant_id, v = indexSelected) %>% summarise(o = mean(meanVal), .groups = "drop")
  g <- g %>% group_by(lv, v) %>% summarise(o = mean(o), n = n(), .groups = "drop") %>% filter(n >= 5)
  col <- if (dir == "forward") "#B03A2E" else "#2C6E9B"
  ggplot(g) + geom_segment(aes(x = 0, xend = 1, y = v, yend = o), colour = col, alpha = .55, linewidth = .45) + geom_point(aes(x = 0, y = v), size = .8, colour = "grey25") +
    geom_point(aes(x = 1, y = o), size = .8, colour = col) + facet_wrap(~ lv, nrow = 1) +
    scale_x_continuous(breaks = c(0, 1), labels = if (dir == "forward") c("True", "Report") else c("Report", "True"), expand = c(.1, .1)) + scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, 10)) +
    labs(x = NULL, y = "Shade", title = if (dir == "forward") "Galton: true colour to mean report (experience)" else "Galton: report to mean true colour (experience)") + th + theme(panel.grid.major.x = element_blank())
}
slope1_plot <- function(id) {
  c1 <- cells %>% filter(experiment == id) %>% select(participant_id, level, slope_fwd, slope_rev) %>% pivot_longer(c(slope_fwd, slope_rev), names_to = "dir", values_to = "slope") %>%
    mutate(dir = ifelse(dir == "slope_fwd", "Forward slope", "Reverse slope"), lv = factor(sapply(level, function(l) lvlab(id, l)), levels = sapply(LVS[[id]], function(l) lvlab(id, l))))
  m <- s1 %>% filter(experiment == id, source == "per-person t-test") %>% mutate(dir = ifelse(direction == "forward", "Forward slope", "Reverse slope"),
    lv = factor(sapply(level, function(l) lvlab(id, l)), levels = levels(c1$lv)), lab = ifelse(p_holm < .001, "p < .001", sprintf("p = %s", sub("^0", "", sprintf("%.3f", p_holm)))))
  ggplot(c1, aes(lv, slope)) + geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") + geom_jitter(width = .12, height = 0, alpha = .35, size = 1.3, colour = "grey45") +
    geom_errorbar(data = m, aes(y = slope, ymin = ci_low, ymax = ci_high), width = .15, colour = EXPC, linewidth = 1) + geom_point(data = m, aes(y = slope), colour = EXPC, size = 3) +
    geom_label(data = m, aes(y = 2.05, label = lab), size = 3.4, family = "Calibri", colour = EXPC, label.size = 0, fill = "white") + facet_wrap(~ dir) +
    scale_y_continuous(limits = c(0, 2.15)) + labs(x = NULL, y = "Each person's slope (dots); mean and 95% CI", title = "Is the slope different from 1 at each level? (experience; p vs 1, Holm)") + th
}
people_plot <- function(id) {
  c1 <- cells %>% filter(experiment == id) %>% select(participant_id, level, slope_fwd, slope_rev) %>% pivot_longer(c(slope_fwd, slope_rev), names_to = "dir", values_to = "slope") %>%
    mutate(dir = ifelse(dir == "slope_fwd", "Forward slope", "Reverse slope"), lv = factor(sapply(level, function(l) lvlab(id, l)), levels = sapply(LVS[[id]], function(l) lvlab(id, l))))
  m <- c1 %>% group_by(dir, lv) %>% summarise(M = mean(slope), se = sd(slope) / sqrt(n()), .groups = "drop")
  ggplot(c1, aes(lv, slope)) + geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") + geom_line(aes(group = participant_id), colour = "grey70", alpha = .5, linewidth = .35) +
    geom_errorbar(data = m, aes(y = M, ymin = M - 1.96 * se, ymax = M + 1.96 * se), width = .12, colour = EXPC, linewidth = .9) +
    geom_line(data = m, aes(y = M, group = 1), colour = EXPC, linewidth = 1.2) + geom_point(data = m, aes(y = M), colour = EXPC, size = 2.4) +
    facet_wrap(~ dir, ncol = 1, scales = "free_y") + labs(x = NULL, y = "Each person's own OLS slope", title = "Each person's own slopes (grey), mean and 95% CI (experience)") + th
}
track_plot <- function(id) {
  c1 <- cells %>% filter(experiment == id) %>% group_by(level) %>% summarise(m = mean(z), se = sd(z) / sqrt(n()), .groups = "drop") %>%
    mutate(r = tanh(m), lo = tanh(m - 1.96 * se), hi = tanh(m + 1.96 * se), lv = factor(sapply(level, function(l) lvlab(id, l)), levels = sapply(LVS[[id]], function(l) lvlab(id, l))))
  ggplot(c1, aes(lv, r, group = 1)) + geom_errorbar(aes(ymin = lo, ymax = hi), width = .1, colour = EXPC) + geom_line(colour = EXPC, linewidth = .9) + geom_point(colour = EXPC, size = 2.6) +
    scale_y_continuous(limits = c(.6, .9)) + labs(x = NULL, y = "Mean r (95% CI)", title = "Tracking: correlation of report with true colour (experience)") + th
}
hist_plot <- function(d) {
  h <- bind_rows(d %>% transmute(what = "True colour (displayed average)", v = meanVal), d %>% transmute(what = "Report", v = indexSelected))
  ggplot(h, aes(v, fill = what)) + geom_histogram(binwidth = 1, position = "identity", alpha = .55, boundary = .5) +
    scale_fill_manual(values = c("Report" = "#2E7D4F", "True colour (displayed average)" = "grey55")) + scale_x_continuous(limits = c(0, 51), breaks = seq(0, 50, 10)) +
    labs(x = "Shade", y = "Trials", title = "Clean trials, experience: shown and reported") + th
}
exag_plot <- function(id, d) {
  w <- d %>% mutate(win = case_when(abs(indexSelected - 10) <= 2 ~ "Reports near 10", abs(indexSelected - 40) <= 2 ~ "Reports near 40")) %>% filter(!is.na(win)) %>% mutate(analysis = "Report windows (+/- 2)")
  f <- d %>% mutate(win = case_when(indexSelected < 14 ~ "Reports below 14", indexSelected > 36 ~ "Reports above 36")) %>% filter(!is.na(win)) %>% mutate(analysis = "Fixed cutoffs")
  em <- bind_rows(lapply(split(bind_rows(w, f), list(bind_rows(w, f)$analysis, bind_rows(w, f)$win), drop = TRUE), function(x) {
    if (any(table(droplevels(x$lv)) < 8) || nlevels(droplevels(x$lv)) < nlevels(d$lv)) return(NULL)
    m <- lmer(meanVal ~ lv + (1 | participant_id), data = x); e <- as.data.frame(emmeans(m, ~ lv)); e$win <- x$win[1]; e$analysis <- x$analysis[1]; e }))
  pa <- ggplot(em, aes(lv, emmean, colour = win, group = win)) + geom_hline(yintercept = 25, linetype = "dotted", colour = "grey60") + geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = .1) +
    geom_line(linewidth = .9) + geom_point(size = 2.2) + facet_wrap(~ analysis) +
    scale_colour_manual(values = c("Reports near 10" = "#2C6E9B", "Reports below 14" = "#6FA0C8", "Reports near 40" = "#B03A2E", "Reports above 36" = "#D98477")) +
    labs(x = NULL, y = "True colour behind the reports (+/- SE)", title = "What lay behind extreme reports (experience)") + th
  ex <- cells %>% filter(experiment == id) %>% group_by(level) %>% summarise(M = mean(extreme), se = sd(extreme) / sqrt(n()), .groups = "drop") %>% mutate(lv = factor(sapply(level, function(l) lvlab(id, l)), levels = levels(d$lv)))
  pb <- ggplot(ex, aes(lv, M, group = 1)) + geom_errorbar(aes(ymin = M - se, ymax = M + se), width = .1, colour = EXPC) + geom_line(colour = EXPC, linewidth = .9) + geom_point(colour = EXPC, size = 2.2) +
    scale_y_continuous(labels = scales::percent) + labs(x = NULL, y = "Share of extreme reports (+/- SE)", title = "How often reports were extreme") + th
  pa + pb + plot_layout(widths = c(2, 1))
}
same_plot <- function(id) {
  HL <- c(exp1 = "12 vs 2", exp2 = "20 vs 2", exp3 = "20 vs 2", exp4 = "7 vs 3")[[id]]
  tl <- function(x) ifelse(x == "low", "Pale end (reports up to 14)", "Vivid end (reports 36 and up)")
  a <- read.csv(file.path(AC, "same_report_values_by_instruction.csv")) %>% filter(experiment == id, condition == "experience", comparison == HL) %>% mutate(tail = tl(tail))
  b <- read.csv(file.path(AC, "same_report_tests_by_instruction.csv")) %>% filter(experiment == id, condition == "experience", comparison == HL) %>% mutate(tail = tl(tail))
  ggplot(a, aes(report, less_extreme)) + geom_hline(yintercept = 0, colour = "grey50") + geom_rect(data = b, aes(xmin = -Inf, xmax = Inf, ymin = ci_low, ymax = ci_high), inherit.aes = FALSE, fill = EXPC, alpha = .12) +
    geom_hline(data = b, aes(yintercept = less_extreme), colour = EXPC, linewidth = 1) + geom_point(size = 2.6, colour = "grey30") + facet_wrap(~ tail, scales = "free_x") +
    labs(x = "Report value", y = "How much less extreme the array was", title = paste0("Same report, ", HL, if (id == "exp4") " (SD)" else " squares", ", experience (each report value counted once)")) + th
}
pervalue_plot <- function(id, d) {
  pv <- function(xv) d %>% mutate(v = round(.data[[xv]]), side = sign(v - 25)) %>% filter(side != 0) %>% mutate(m = (indexSelected - meanVal) * side) %>%
    group_by(lv, participant_id, v) %>% summarise(m = mean(m), .groups = "drop") %>% group_by(lv, v) %>% summarise(M = mean(m), n = n(), .groups = "drop") %>% filter(n >= 8)
  p <- function(df, xl, tt, yl) ggplot(df, aes(v, M, colour = lv)) + geom_hline(yintercept = 0, colour = "grey50") + geom_vline(xintercept = 25, linetype = "dotted", colour = "grey60") +
    geom_line(linewidth = .9) + geom_point(size = 1.4) + scale_colour_manual(values = cols(id)) + labs(x = xl, y = yl, title = tt) + th
  p(pv("meanVal"), "True colour", "Forward: (report - true) x side of the true colour", "< 0 = pulled to the middle") +
    p(pv("indexSelected"), "Report", "Reverse: (report - true) x side of the report", "> 0 = report more extreme than array")
}
raw_png <- function(id, d) {
  p <- ggplot(d, aes(meanVal, indexSelected)) + geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey40") +
    geom_jitter(width = .15, height = .15, alpha = if (nrow(d) > 6000) .07 else .14, size = .55, colour = "#1E2A36") + geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = EXPC, linewidth = .8) +
    facet_wrap(~ lv, nrow = 1) + coord_equal(xlim = c(0, 50), ylim = c(0, 50), expand = FALSE) + scale_x_continuous(breaks = seq(0, 50, 10)) + scale_y_continuous(breaks = seq(0, 50, 10)) +
    labs(x = "True colour (displayed average)", y = "Report", title = "Experience instruction: every clean trial") + theme_minimal(base_size = 11, base_family = "Calibri") +
    theme(panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"), panel.spacing = unit(1, "lines"))
  ggsave(file.path(OUTD, paste0("rawx_", id, ".png")), p, width = if (id == "exp1") 12 else 9.6, height = if (id == "exp1") 3.9 else 4.1, dpi = 220, bg = "white")
}

for (id in names(LVS)) {
  d <- dat(id); k <- sub("exp", "x", id)
  raw_png(id, d)
  add(paste0(k, "_hist"), hist_plot(d), 6.95, 1.55, 5.78, 3.9)
  add(paste0(k, "_track"), track_plot(id), 0.6, 1.55, 6.6, 4.6)
  for (dir in c("forward", "reverse")) { p_ <- if (dir == "forward") "fwd" else "rev"
    add(paste0(k, "_", p_, "_lines"), line_plot(id, d, dir), 0.6, 1.5, 6.4, 3.9)
    add(paste0(k, "_", p_, "_slopes"), slope_plot(id, dir), 7.25, 1.5, 5.48, 2.0)
    add(paste0(k, "_", p_, "_galton"), galton_plot(id, d, dir), 7.25, 3.55, 5.48, 3.4) }
  add(paste0(k, "_slope1"), slope1_plot(id), 0.6, 1.5, 7.9, 5.3)
  add(paste0(k, "_people"), people_plot(id), 0.6, 1.5, 8.0, 5.35)
  add(paste0(k, "_exag"), exag_plot(id, d), 0.6, 1.5, 12.13, 4.0)
  add(paste0(k, "_same"), same_plot(id), 0.6, 1.5, 7.6, 4.7)
  add(paste0(k, "_pervalue"), pervalue_plot(id, d), 0.6, 1.5, 12.13, 4.1)
  if (id != "exp4") add(paste0(k, "_sum"), (slope_plot(id, "forward") + labs(title = "Forward slope (experience)")) + (slope_plot(id, "reverse") + labs(title = "Reverse slope (experience)")), 0.6, 1.5, 7.6, 4.3)
  message(id, " done")
}
# Exp 4: closing the fan, experience cells
cl <- cells %>% filter(experiment == "exp4") %>% mutate(G = sd_rep / sd_true) %>% group_by(level) %>%
  summarise(`Tracking r` = tanh(mean(z)), `Spread ratio G (SD reports / SD true)` = mean(G), `Forward slope = r x G` = mean(slope_fwd), `Reverse slope = r / G` = mean(slope_rev), .groups = "drop") %>%
  pivot_longer(-level) %>% mutate(lvl = paste("SD", level), panel = factor(ifelse(grepl("slope", name), "...give the two slopes", "Two ingredients..."), levels = c("Two ingredients...", "...give the two slopes")))
add("x4_fan", ggplot(cl, aes(lvl, value, colour = name, group = name)) + geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") + geom_line(linewidth = 1.1) + geom_point(size = 2.4) +
      geom_text(aes(label = sprintf("%.2f", value)), vjust = -0.9, size = 3.6, show.legend = FALSE, family = "Calibri") + facet_wrap(~ panel) +
      scale_colour_manual(values = c("Tracking r" = "#55616D", "Spread ratio G (SD reports / SD true)" = "#B7790F", "Forward slope = r x G" = "#B03A2E", "Reverse slope = r / G" = "#2C6E9B")) +
      scale_y_continuous(limits = c(.55, 1.45)) + guides(colour = guide_legend(nrow = 2)) + labs(x = NULL, y = NULL, title = "Why the forward fan closes and the reverse slope stays flat (Exp 4, experience, per person)") + th,
    0.6, 1.5, 7.4, 5.2)
# Exps 2 vs 3, experience: forward slope per size
fr <- link %>% filter(experiment %in% c("exp2", "exp3"), direction == "forward") %>% mutate(frame = ifelse(experiment == "exp2", "Exp 2: no frame", "Exp 3: Mondrian frame"), lvl = factor(paste(level, "sq."), levels = c("2 sq.", "10 sq.", "20 sq.")))
add("x23_frame", ggplot(fr, aes(lvl, slope, colour = frame, group = frame)) + geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
      geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = .1, position = position_dodge(.3)) + geom_line(position = position_dodge(.3), linewidth = .9) + geom_point(position = position_dodge(.3), size = 2.2) +
      scale_colour_manual(values = c("Exp 2: no frame" = "#2C6E9B", "Exp 3: Mondrian frame" = "#2E7D4F")) + labs(x = NULL, y = "Forward slope (95% CI)", title = "Does the frame change the slope? (experience)") + th, 7.0, 1.5, 5.73, 3.3)
# slope vs measured variance, experience
sdl <- read.csv("/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/analysis/output/stimulus_audit/item_sd_by_level.csv") %>% filter(experiment %in% names(LVS)) %>% group_by(experiment, level) %>% summarise(item_sd = mean(item_sd), .groups = "drop")
sp <- link %>% filter(direction == "forward") %>% select(experiment, level, slope) %>% left_join(sdl, by = c("experiment", "level")) %>% arrange(experiment, item_sd)
EXL <- c(exp1 = "Exp 1: array size", exp2 = "Exp 2: array size", exp3 = "Exp 3: size + frame", exp4 = "Exp 4: variance (SD)")
add("x_sum_var", ggplot(sp, aes(item_sd, slope, colour = experiment)) + geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") + geom_path(linewidth = .9) + geom_point(size = 2.4) +
      ggrepel::geom_text_repel(aes(label = ifelse(experiment == "exp4", paste0("SD ", level), paste0(level, " sq."))), size = 3.4, family = "Calibri", show.legend = FALSE, seed = 1) +
      scale_colour_manual(values = c(exp1 = "grey50", exp2 = "#2C6E9B", exp3 = "#2E7D4F", exp4 = "#B03A2E"), labels = EXL) +
      labs(x = "Measured variance of the shades (SD)", y = "Forward slope (experience)", title = "The slope follows the variance of the shades (experience)") + th, 0.6, 1.5, 7.2, 4.6)

doc <- read_pptx(); idx <- data.frame(name = names(figs), slide = seq_along(figs))
for (nm in names(figs)) { doc <- add_slide(doc, layout = "Blank", master = "Office Theme"); L <- figs[[nm]]$loc
  doc <- ph_with(doc, dml(ggobj = figs[[nm]]$p, fonts = list(sans = "Calibri")), location = ph_location(left = L[1], top = L[2], width = L[3], height = L[4])) }
print(doc, target = file.path(OUTD, "figures_experience.pptx")); write.csv(idx, file.path(OUTD, "figures_experience_index.csv"), row.names = FALSE)
cat("figures:", nrow(idx), "\n")
