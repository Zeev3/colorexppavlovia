# Drafts: tracking in the original shades, experience only, Exp 4.
#   ellipse: one data ellipse per level (covers ~68% of trials, i.e. 1 SD); each person centred on their own means
#            (grand means added back) so differences between people do not widen the clouds. Thin = high r.
#   band:    per level, the regression line (report on true colour) with a band of +/- 1 residual SD:
#            how far trials typically fall from the line. Narrow = close tracking.
HERE <- commandArgs(trailingOnly = TRUE)[1]; OUT <- commandArgs(trailingOnly = TRUE)[2]
suppressPackageStartupMessages({ library(dplyr); library(ggplot2); library(tidyr) })
COL <- c("SD 3" = "#2E7D4F", "SD 5" = "#B7790F", "SD 7" = "#B03A2E")
th <- theme_minimal(base_size = 13) + theme(panel.grid.minor = element_blank(), legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(face = "bold", size = 13))
d <- read.csv(file.path(HERE, "data", "exp4_clean_conservative.csv")) %>% filter(condition == "experience") %>% mutate(lv = factor(paste("SD", noise_level))) %>%
  group_by(participant_id) %>% mutate(t = meanVal - mean(meanVal) + mean(.$meanVal), rp = indexSelected - mean(indexSelected) + mean(.$indexSelected)) %>% ungroup()
st <- d %>% group_by(lv) %>% summarise(r = cor(t, rp), sdt = sd(t), sdr = sd(rp), .groups = "drop")
pe <- ggplot(d, aes(t, rp, colour = lv, fill = lv)) + geom_abline(slope = 1, intercept = mean(d$rp) - mean(d$t), linetype = "dashed", colour = "grey50") +
  stat_ellipse(type = "norm", level = .68, geom = "polygon", alpha = .10, linewidth = 1.2) +
  geom_text(data = st, aes(x = 1.5, y = 48.5 - 2.8 * (as.integer(lv) - 1), label = sprintf("%s: r = %.2f, SD of reports %.1f", lv, r, sdr), colour = lv), inherit.aes = FALSE, hjust = 0, size = 4.2, show.legend = FALSE) +
  scale_colour_manual(values = COL) + scale_fill_manual(values = COL) + coord_equal(xlim = c(0, 50), ylim = c(0, 50), expand = FALSE) + scale_x_continuous(breaks = seq(0, 50, 10)) + scale_y_continuous(breaks = seq(0, 50, 10)) +
  labs(x = "True colour (shades)", y = "Report (shades)", title = "Data ellipses (68% of trials): thinner = closer tracking") + th
ggsave(file.path(OUT, "draft_tracking_ellipses_exp4.png"), pe, width = 6.6, height = 7.2, dpi = 180, bg = "white")
fits <- d %>% group_by(lv) %>% group_modify(~ { m <- lm(rp ~ t, data = .x); tibble(a = coef(m)[1], b = coef(m)[2], s = sigma(m), lo = quantile(.x$t, .02), hi = quantile(.x$t, .98)) }) %>% ungroup()
bl <- fits %>% rowwise() %>% reframe(lv = lv, x = seq(lo, hi, length.out = 30), y = a + b * seq(lo, hi, length.out = 30), s = s)
pb <- ggplot(bl, aes(x, y, colour = lv, fill = lv)) + geom_abline(slope = 1, intercept = mean(d$rp) - mean(d$t), linetype = "dashed", colour = "grey50") +
  geom_ribbon(aes(ymin = y - s, ymax = y + s), alpha = .12, colour = NA) + geom_line(linewidth = 1.2) +
  geom_text(data = fits, aes(x = 1.5, y = 48.5 - 2.8 * (as.integer(lv) - 1), label = sprintf("%s: slope %.2f, band +/- %.1f shades", lv, b, s), colour = lv), inherit.aes = FALSE, hjust = 0, size = 4.2, show.legend = FALSE) +
  scale_colour_manual(values = COL) + scale_fill_manual(values = COL) + coord_equal(xlim = c(0, 50), ylim = c(0, 50), expand = FALSE) + scale_x_continuous(breaks = seq(0, 50, 10)) + scale_y_continuous(breaks = seq(0, 50, 10)) +
  labs(x = "True colour (shades)", y = "Report (shades)", title = "Lines with +/- 1 residual SD: wider = looser tracking") + th
ggsave(file.path(OUT, "draft_tracking_bands_exp4.png"), pb, width = 6.6, height = 7.2, dpi = 180, bg = "white")
print(st); print(fits)
