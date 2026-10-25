# Tracking shown as fitted regression lines, experience instruction only, every experiment (PNG).
#   trackz_<exp>.png  z-scores: true colour and report standardised within each person and level; in these units the
#                     regression slope equals r, so one line per level with slope = mean r (Fisher-z average) is a picture
#                     of tracking. (Forward and reverse lines are identical in z units.)
#   twolines_<exp>.png original shades (each person centred, grand mean added back): forward line (report on true) and
#                     reverse line (true on report, drawn in the same axes) per level; they coincide only if r = 1, and the
#                     angle between them opens as r falls. Shows r and the narrowing of the reports (G) together.
HERE <- commandArgs(trailingOnly = TRUE)[1]; OUT <- commandArgs(trailingOnly = TRUE)[2]
suppressPackageStartupMessages({ library(dplyr); library(ggplot2); library(tidyr) })
LVS <- list(exp1 = c(2, 6, 8, 12), exp2 = c(2, 10, 20), exp3 = c(2, 10, 20), exp4 = c(3, 5, 7))
lab <- function(id, l) if (id == "exp4") paste("SD", l) else paste(l, "sq.")
th <- theme_minimal(base_size = 13) + theme(panel.grid.minor = element_blank(), legend.position = "bottom", legend.title = element_blank(),
  strip.text = element_text(face = "bold"), plot.title = element_text(face = "bold", size = 13))
for (id in names(LVS)) {
  levs <- sapply(LVS[[id]], function(l) lab(id, l))
  cols <- setNames(if (length(levs) == 4) c("#2E7D4F", "#6C9A3B", "#B7790F", "#B03A2E") else c("#2E7D4F", "#B7790F", "#B03A2E"), levs)
  d <- read.csv(file.path(HERE, "data", paste0(id, "_clean_conservative.csv"))) %>% filter(condition == "experience") %>%
    mutate(lv = factor(sapply(noise_level, function(l) lab(id, l)), levels = levs))
  z <- d %>% group_by(participant_id, lv) %>% mutate(zt = as.numeric(scale(meanVal)), zr = as.numeric(scale(indexSelected))) %>% ungroup() %>% filter(is.finite(zt), is.finite(zr))
  rr <- d %>% group_by(participant_id, lv) %>% summarise(r = cor(meanVal, indexSelected), .groups = "drop") %>% group_by(lv) %>% summarise(r = tanh(mean(atanh(pmin(r, .9999)))), .groups = "drop")
  ln <- rr %>% crossing(x = c(-2.5, 2.5)) %>% mutate(y = r * x)
  set.seed(1)
  pz <- ggplot(z %>% slice_sample(prop = min(1, 4000 / nrow(z))), aes(zt, zr)) + geom_point(alpha = .05, size = .6) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey40") + geom_line(data = ln, aes(x, y, colour = lv), linewidth = 1.3) +
    geom_text(data = rr, aes(x = -2.5, y = 2.5 - .35 * (as.integer(lv) - 1), label = sprintf("%s: r = %.2f", lv, r), colour = lv), hjust = 0, size = 4.4, show.legend = FALSE) +
    scale_colour_manual(values = cols) + coord_equal(xlim = c(-2.6, 2.6), ylim = c(-2.6, 2.6)) +
    labs(x = "True colour (z within person and level)", y = "Report (z within person and level)", title = "Tracking as regression lines: in z units the slope is r") + th
  ggsave(file.path(OUT, paste0("trackz_", id, ".png")), pz, width = 6.4, height = 5.6, dpi = 200, bg = "white")
  c0 <- d %>% group_by(participant_id) %>% mutate(t = meanVal - mean(meanVal) + mean(d$meanVal), rp = indexSelected - mean(indexSelected) + mean(d$indexSelected)) %>% ungroup()
  fits <- c0 %>% group_by(lv) %>% summarise(bf = coef(lm(rp ~ t))[2], br = coef(lm(t ~ rp))[2], mt = mean(t), mr = mean(rp), .groups = "drop")
  xr <- c(0, 50)
  lines <- bind_rows(fits %>% crossing(x = xr) %>% mutate(y = mr + bf * (x - mt), dir = "Forward: report on true"),
                     fits %>% crossing(x = xr) %>% mutate(y = mr + (x - mt) / br, dir = "Reverse: true on report"))
  pt <- ggplot(c0 %>% slice_sample(prop = min(1, 6000 / nrow(c0))), aes(t, rp)) + geom_point(alpha = .05, size = .5) +
    geom_abline(slope = 1, intercept = mean(d$indexSelected) - mean(d$meanVal), linetype = "dashed", colour = "grey45") +
    geom_line(data = lines, aes(x, y, colour = dir), linewidth = 1.1) +
    geom_text(data = fits, aes(x = 1.5, y = 49, label = sprintf("forward %.2f\nreverse %.2f", bf, br)), hjust = 0, vjust = 1, size = 3.6) +
    facet_wrap(~ lv, nrow = 1) + coord_equal(xlim = c(0, 50), ylim = c(0, 50), expand = FALSE) + scale_x_continuous(breaks = seq(0, 50, 10)) + scale_y_continuous(breaks = seq(0, 50, 10)) +
    scale_colour_manual(values = c("Forward: report on true" = "#B03A2E", "Reverse: true on report" = "#2C6E9B")) +
    labs(x = "True colour (shades, each person centred)", y = "Report", title = "Original shades: both regression lines; the angle between them opens as tracking falls") + th
  ggsave(file.path(OUT, paste0("twolines_", id, ".png")), pt, width = 12, height = 4.6, dpi = 200, bg = "white")
  cat(id, "done\n")
}
