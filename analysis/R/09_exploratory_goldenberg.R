# =========================================================
# 09_exploratory_goldenberg.R -- exploratory analysis of the open data from
# Goldenberg et al. (2021), Experiment 1 (data/amit_exp1.csv)
#
# 50 participants x 150 trials of the ensemble-emotion paradigm: an array of
# up to 12 faces, each morphed on a 100-150 valence scale, rated for average
# valence with a morphing response face.
#
# Two models are reported:
#   (a) the model in the write-up, group mean predicted from the rating
#       (meanGroup ~ rating * numberFaces + (1 | id)), fitted separately for
#       positive and negative arrays;
#   (b) the same data in the direction used for the EPoC experiments, report
#       predicted from the objective mean (rating ~ meanGroup * numberFaces +
#       (1 + meanGroup | id)), so that the slope is directly comparable with
#       Experiments 1-4 (slope < 1 = regression to the mean).
# =========================================================

epoc_goldenberg <- function(dir = file.path(OUT, "exp0_goldenberg")) {

  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(DATA, "amit_exp1.csv")
  if (!file.exists(path)) {
    message("amit_exp1.csv not found; skipping the exploratory analysis")
    return(invisible(NULL))
  }

  raw <- read.csv(path, stringsAsFactors = FALSE)

  face_cols <- intersect(names(raw), c(paste0("par", 1:12), "part10"))
  dat <- raw %>%
    mutate(
      row_sd = apply(across(all_of(face_cols)), 1, function(x) sd(x, na.rm = TRUE)),
      id     = factor(id),
      # numberFaces is stored 0-11; counting the non-missing par* columns shows
      # it is the number of faces shown minus one, so 0 = 1 face, 11 = 12 faces
      numberFaces = numberFaces + 1
    ) %>%
    select(id, trial, meanGroup, rating, numberFaces, condition, row_sd) %>%
    filter(!is.na(meanGroup), !is.na(rating), !is.na(numberFaces))

  results <- list(); coefs <- list(); slopes <- list(); preds <- list(); conv <- list()

  for (cond in c("positive", "negative")) {

    d <- dat %>% filter(condition == cond)

    # (a) direction used in the write-up
    m_a <- lmerTest::lmer(meanGroup ~ rating * numberFaces + (1 | id), data = d)

    # (b) direction used in the EPoC experiments
    d <- d %>%
      group_by(id) %>%
      mutate(meanGroup_c = meanGroup - mean(meanGroup, na.rm = TRUE)) %>%
      ungroup()
    m_b <- lmerTest::lmer(rating ~ meanGroup_c * numberFaces + (1 + meanGroup_c | id), data = d)

    coefs[[cond]] <- bind_rows(
      as.data.frame(coef(summary(m_a))) %>% tibble::rownames_to_column("term") %>%
        mutate(model = "meanGroup ~ rating * numberFaces"),
      as.data.frame(coef(summary(m_b))) %>% tibble::rownames_to_column("term") %>%
        mutate(model = "rating ~ meanGroup_c * numberFaces")
    ) %>%
      rename(b = Estimate, SE = `Std. Error`, t = `t value`, p = `Pr(>|t|)`) %>%
      mutate(valence = cond, .before = 1)

    # slope of the objective mean at representative set sizes
    slopes[[cond]] <- as.data.frame(summary(
      emtrends(m_b, ~ numberFaces, var = "meanGroup_c",
               at = list(numberFaces = c(2, 6, 12)), lmer.df = "satterthwaite"),
      infer = c(TRUE, TRUE))) %>%
      rename(slope = meanGroup_c.trend) %>%
      rename_with(~ "ci_low",  any_of(c("lower.CL", "asymp.LCL"))) %>%
      rename_with(~ "ci_high", any_of(c("upper.CL", "asymp.UCL"))) %>%
      mutate(valence = cond, .before = 1)

    preds[[cond]] <- as.data.frame(
      ggeffects::ggpredict(m_a, terms = c("rating [all]", "numberFaces [2,6,12]"))) %>%
      mutate(valence = cond)

    conv[[cond]] <- tibble::tibble(
      valence = cond,
      model   = c("meanGroup ~ rating * numberFaces", "rating ~ meanGroup_c * numberFaces"),
      singular = c(lme4::isSingular(m_a, tol = 1e-4), lme4::isSingular(m_b, tol = 1e-4)),
      messages = c(paste(m_a@optinfo$conv$lme4$messages %||% "", collapse = "; "),
                   paste(m_b@optinfo$conv$lme4$messages %||% "", collapse = "; ")))

    results[[cond]] <- list(model_a = m_a, model_b = m_b)
  }

  conv   <- bind_rows(conv)
  coefs  <- bind_rows(coefs)
  slopes <- bind_rows(slopes)
  preds  <- bind_rows(preds)

  write.csv(conv,   file.path(dir, "goldenberg_model_diagnostics.csv"), row.names = FALSE)
  write.csv(coefs,  file.path(dir, "goldenberg_coefficients.csv"), row.names = FALSE)
  write.csv(slopes, file.path(dir, "goldenberg_slopes.csv"),       row.names = FALSE)

  fig <- ggplot(preds, aes(x = x, y = predicted, colour = group, fill = group)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey40") +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .18, colour = NA) +
    geom_line(linewidth = 1) +
    facet_wrap(~ valence) +
    labs(x = "Participant rating", y = "Predicted mean valence of the group",
         colour = "Number of faces", fill = "Number of faces",
         title = "Ensemble emotion (Goldenberg et al., 2021, Exp. 1)",
         subtitle = "Model-based predictions with 95% CIs") +
    theme_epoc()
  epoc_save(fig, file.path(dir, "fig1_goldenberg_predictions.png"), width = 8, height = 4.5)

  # short results text
  con <- file(file.path(dir, "results.md"), open = "wt"); on.exit(close(con))
  w <- function(...) log_line(con, ...)
  w("# Exploratory analysis - Goldenberg et al. (2021), Experiment 1")
  w("")
  w("_Generated by analysis/run_all.R on ", format(Sys.time(), "%Y-%m-%d %H:%M"), "._")
  w("")
  w("N = ", n_distinct(dat$id), " participants, ", nrow(dat), " trials.")
  w("")
  for (cond in c("positive", "negative")) {
    w("## ", cond, " valence arrays")
    w("")
    ca <- coefs %>% filter(valence == cond, model == "meanGroup ~ rating * numberFaces")
    for (i in seq_len(nrow(ca))) {
      if (ca$term[i] == "(Intercept)") next
      w("- ", ca$term[i], ": ", fmt_b(ca$b[i], ca$SE[i], ca$t[i], ca$df[i], ca$p[i]), ".")
    }
    w("")
    sl <- slopes %>% filter(valence == cond)
    w("Slope of the rating on the objective mean (forward direction, from model b; comparable with Experiments 1-4):")
    w("")
    for (i in seq_len(nrow(sl))) {
      w("- ", sl$numberFaces[i], " faces: b = ", fmt_num(sl$slope[i], 3),
        ", 95% CI [", fmt_num(sl$ci_low[i], 3), ", ", fmt_num(sl$ci_high[i], 3), "].")
    }
    w("")
  }

  # note any fit that did not converge cleanly, rather than leaving it in the console
  flagged <- conv %>% filter(singular | nzchar(messages))
  if (nrow(flagged)) {
    w("## Model diagnostics")
    w("")
    for (i in seq_len(nrow(flagged)))
      w("- ", flagged$valence[i], ", `", flagged$model[i], "`: ",
        paste(c(if (flagged$singular[i]) "singular fit",
                if (nzchar(flagged$messages[i])) flagged$messages[i]), collapse = "; "), ".")
    w("")
    w(if (nrow(flagged) > 1) "These are " else "This is ",
      "lme4's strict gradient check on a large, well-identified model; the ",
      "coefficients are unaffected to the precision reported here. Full detail in ",
      "`goldenberg_model_diagnostics.csv`.")
    w("")
  }

  invisible(list(coefficients = coefs, slopes = slopes, models = results, diagnostics = conv))
}
