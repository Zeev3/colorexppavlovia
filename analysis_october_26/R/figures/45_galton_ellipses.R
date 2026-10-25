# Galton-style figure, experience instruction only: one panel per noise level.
#   cloud    every clean trial (each person centred on their own means, grand means added back, so differences
#            between people do not widen the cloud; small jitter because values are whole shades)
#   ellipse  the 68% data ellipse of the cloud (bivariate normal with the cloud's means, SDs and correlation)
#   lines    forward regression (report on true colour) and reverse regression (true colour on report), drawn in
#            the same axes. The forward line passes through the ellipse's leftmost and rightmost points, the reverse
#            line through its lowest and highest points (marked) - Galton's construction.
# Writes galton_ellipse_<exp>.png (all levels side by side) and galton_ellipse_<exp>_<level>.png (one per level).
HERE <- commandArgs(trailingOnly = TRUE)[1]; OUT <- commandArgs(trailingOnly = TRUE)[2]
suppressPackageStartupMessages({ library(dplyr); library(ggplot2) })
LVS <- list(exp1 = c(2, 6, 8, 12), exp2 = c(2, 10, 20), exp3 = c(2, 10, 20), exp4 = c(3, 5, 7))
lab <- function(id, l) if (id == "exp4") paste("SD", l) else paste(l, "sq.")
th <- theme_minimal(base_size = 13) + theme(panel.grid.minor = element_blank(), legend.position = "bottom", legend.title = element_blank(),
  strip.text = element_text(face = "bold", size = 14), plot.title = element_text(face = "bold", size = 13), panel.spacing = unit(2, "lines"))
ell <- function(x, y, level = .68, n = 400) {
  mu <- c(mean(x), mean(y)); S <- cov(cbind(x, y)); L <- t(chol(S)); k <- sqrt(qchisq(level, 2)); a <- seq(0, 2 * pi, length.out = n)
  pts <- t(mu + k * L %*% rbind(cos(a), sin(a))); data.frame(ex = pts[, 1], ey = pts[, 2])
}
for (id in names(LVS)) {
  levs <- sapply(LVS[[id]], function(l) lab(id, l))
  d <- read.csv(file.path(HERE, "data", paste0(id, "_clean_conservative.csv"))) %>% filter(condition == "experience")
  gm_t <- mean(d$meanVal); gm_r <- mean(d$indexSelected)
  d <- d %>% group_by(participant_id) %>% mutate(t = meanVal - mean(meanVal) + gm_t, rp = indexSelected - mean(indexSelected) + gm_r) %>% ungroup() %>%
    mutate(lv = factor(sapply(noise_level, function(l) lab(id, l)), levels = levs))
  E <- list(); Ln <- list(); Tg <- list(); St <- list()
  for (l in levs) {
    x <- d$t[d$lv == l]; y <- d$rp[d$lv == l]
    e <- ell(x, y); e$lv <- l; E[[l]] <- e
    bf <- cov(x, y) / var(x); br <- cov(x, y) / var(y); mx <- mean(x); my <- mean(y)
    xs <- c(0, 50)
    Ln[[l]] <- bind_rows(data.frame(lv = l, x = xs, y = my + bf * (xs - mx), what = "Forward: report on true colour"),
                         data.frame(lv = l, x = xs, y = my + (xs - mx) / br, what = "Reverse: true colour on report"))
    Tg[[l]] <- bind_rows(e[c(which.min(e$ex), which.max(e$ex)), ] %>% mutate(what = "Forward: report on true colour"),
                         e[c(which.min(e$ey), which.max(e$ey)), ] %>% mutate(what = "Reverse: true colour on report"))
    St[[l]] <- data.frame(lv = l, txt = sprintf("r = %.2f\nforward slope %.2f\nreverse slope %.2f", cor(x, y), bf, br))
  }
  E <- bind_rows(E); Ln <- bind_rows(Ln); Tg <- bind_rows(Tg); St <- bind_rows(St)
  for (df in c("E", "Ln", "Tg", "St")) { z <- get(df); z$lv <- factor(z$lv, levels = levs); assign(df, z) }
  LC <- c("Forward: report on true colour" = "#B03A2E", "Reverse: true colour on report" = "#2C6E9B")
  base <- function(dd, ee, ll, tg, st) ggplot(dd, aes(t, rp)) +
    geom_jitter(width = .25, height = .25, alpha = .10, size = .7, colour = "grey30") +
    geom_abline(slope = 1, intercept = gm_r - gm_t, linetype = "dashed", colour = "grey45") +
    geom_polygon(data = ee, aes(ex, ey), inherit.aes = FALSE, fill = "#B7790F", alpha = .10, colour = "#B7790F", linewidth = 1.1) +
    geom_line(data = ll, aes(x, y, colour = what), linewidth = 1.2) + geom_point(data = tg, aes(ex, ey, colour = what), size = 3) +
    geom_text(data = st, aes(x = 1.5, y = 49, label = txt), inherit.aes = FALSE, hjust = 0, vjust = 1, size = 3.9, lineheight = .95) +
    scale_colour_manual(values = LC) + coord_equal(xlim = c(0, 50), ylim = c(0, 50), expand = FALSE) + scale_x_continuous(breaks = seq(0, 50, 10)) + scale_y_continuous(breaks = seq(0, 50, 10)) +
    labs(x = "True colour (shades; each person centred)", y = "Report (shades)") + th
  p <- base(d, E, Ln, Tg, St) + facet_wrap(~ lv, nrow = 1) +
    labs(title = "Every trial (grey), the 68% ellipse (yellow), and both regression lines; dots = where each line touches the ellipse")
  ggsave(file.path(OUT, paste0("galton_ellipse_", id, ".png")), p, width = 4.3 * length(levs), height = 6.4, dpi = 180, bg = "white")
  for (l in levs) {
    q <- base(d %>% filter(lv == l), E %>% filter(lv == l), Ln %>% filter(lv == l), Tg %>% filter(lv == l), St %>% filter(lv == l)) + labs(title = l)
    ggsave(file.path(OUT, sprintf("galton_ellipse_%s_%s.png", id, gsub("[ .]", "", l))), q, width = 5.4, height = 6.6, dpi = 180, bg = "white")
  }
  cat(id, "done\n")
}
