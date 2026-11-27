# =============================================================================
#  insights.R  -  Insight engine for Gadgetbridge Lab
#  Adds the menu "Insights" with three tabs:
#    Overview        domain cards, what changed, plan for next week, usual-range bands,
#                    this month vs last month, patterns, fitness, explore, activity mix
#    Weekly report   one report per calendar week (Mon-Sun) vs the week before
#    Monthly report  one report per calendar month vs the month before
#  Both report tabs can download the selected report or ALL weeks / months in one file.
#  Needs: the app file from the Gadgetbridge Lab (it re-uses its reactives and helpers).
# =============================================================================
# make fmt() safe for vectors (single values behave exactly as before)
if (!isTRUE(attr(fmt, "ins_vec"))) fmt <- local({
  f0 <- fmt
  structure(function(x, ...) {
    n <- length(x); if (n <= 1) return(f0(x, ...))
    a <- list(...)
    vapply(seq_len(n), function(i) as.character(do.call(f0, c(list(x[i]),
                                                              lapply(a, function(z) if (length(z) == n) z[i] else z)))), "")
  }, ins_vec = TRUE)
})
# ---------------------------------------------------------------- definitions ----
INS_OUT <- list(
  asleep_h  = list(lab = "sleep duration",            mult = 60, unit = "min",    vunit = "h",   dig = 0, better = "up",      up = "longer",  dn = "shorter"),
  eff       = list(lab = "sleep efficiency",          mult = 1,  unit = "points", vunit = "%",   dig = 1, better = "up",      up = "higher",  dn = "lower"),
  deep_pct  = list(lab = "deep-sleep share",          mult = 1,  unit = "points", vunit = "%",   dig = 1, better = "up",      up = "higher",  dn = "lower"),
  hr_night  = list(lab = "lowest sleeping heart rate", mult = 1, unit = "bpm",    vunit = "bpm", dig = 1, better = "down",    up = "higher",  dn = "lower"),
  hr_sleep  = list(lab = "median sleeping heart rate", mult = 1, unit = "bpm",    vunit = "bpm", dig = 1, better = "down",    up = "higher",  dn = "lower"),
  hrv       = list(lab = "next-morning HRV",          mult = 1,  unit = "ms",     vunit = "ms",  dig = 1, better = "up",      up = "higher",  dn = "lower"),
  dip       = list(lab = "night-time heart-rate dip", mult = 1,  unit = "points", vunit = "%",   dig = 1, better = "up",      up = "bigger",  dn = "smaller"),
  onset_min = list(lab = "sleep-onset time",          mult = 1,  unit = "min",    vunit = "",    dig = 0, better = "neutral", up = "later",   dn = "earlier"))

INS_PRED <- list(
  steps     = list(lab = "daily steps",           scale = 1000, unit = "+1,000 steps"),
  mem       = list(lab = "active minutes",        scale = 30,   unit = "+30 active minutes"),
  wk_min    = list(lab = "workout minutes",       scale = 30,   unit = "+30 workout minutes"),
  wk_trimp  = list(lab = "workout load",          scale = 50,   unit = "+50 TRIMP of workout load"),
  sed_h     = list(lab = "sedentary hours",       scale = 1,    unit = "+1 sedentary hour"),
  stress    = list(lab = "average stress",        scale = 10,   unit = "+10 stress points"),
  nap_len   = list(lab = "nap length",            scale = 30,   unit = "+30 min of nap"),
  end_h     = list(lab = "workout finish time",   scale = 1,    unit = "a workout finishing 1 h later"),
  onset_min = list(lab = "bedtime",               scale = 60,   unit = "going to sleep 1 h later"))

INS_GRP <- list(
  nap      = list(lab = "a nap",                         vs = "days without a nap",          f = function(N) N$nap_min > 0),
  long_nap = list(lab = "a long nap (45+ min)",          vs = "days with a shorter nap",     f = function(N) ifelse(N$nap_min > 0, N$nap_min >= 45, NA)),
  workout  = list(lab = "a workout",                     vs = "rest days",                   f = function(N) N$wk_min > 0),
  hard     = list(lab = "a hard workout",                vs = "other days",                  f = function(N) N$hard),
  late     = list(lab = "a workout ending after 19:30",  vs = "days with an earlier workout", f = function(N) ifelse(N$wk_min > 0, N$late, NA)))

INS_WM <- data.frame(stringsAsFactors = FALSE,
                     id     = c("steps", "mem", "sed_h", "sport_min", "trimp", "asleep_h", "eff", "deep_pct", "hr_night", "hrv", "dip", "stress", "vo2"),
                     lab    = c("Steps per day", "Active minutes per week", "Sedentary hours per day", "Workout minutes per week", "Workout load per week (TRIMP)",
                                "Sleep (h per night)", "Sleep efficiency (%)", "Deep-sleep share (%)", "Lowest sleeping HR (bpm)", "HRV (ms)", "Night HR dip (%)",
                                "Stress score", "Fitness proxy VO2max"),
                     better = c("up", "up", "down", "up", "up", "up", "up", "up", "down", "up", "up", "down", "up"),
                     dom    = c("Activity", "Activity", "Activity", "Training", "Training", "Sleep", "Sleep", "Sleep", "Recovery", "Recovery", "Recovery", "Recovery", "Fitness"),
                     wk     = c(FALSE, TRUE, FALSE, TRUE, TRUE, rep(FALSE, 8)),
                     dig    = c(0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1))

# ---------------------------------------------------------------- small helpers ----
ins_clock <- function(m) { c0 <- round(m + 1080) %% 1440; sprintf("%02d:%02d", as.integer((c0 %/% 60) %% 24), as.integer(c0 %% 60)) }   # minutes after 18:00 -> clock
ins_val <- function(oid, v) {
  o <- INS_OUT[[oid]]
  if (oid == "onset_min") ins_clock(v) else if (oid == "asleep_h") paste0(fmt(v, 1), " h") else paste(fmt(v, o$dig), o$vunit)
}
ins_ts_slope <- function(x, y) {
  n <- length(x); if (n > 400) { i <- (n - 399):n; x <- x[i]; y <- y[i] }
  dx <- outer(x, x, "-"); dy <- outer(y, y, "-"); u <- upper.tri(dx); a <- dx[u]; b <- dy[u]; k <- a != 0
  if (!any(k)) NA_real_ else median(b[k] / a[k])
}
ins_grp_test <- function(y, g, min1 = 4, min0 = 6) {
  ok <- !is.na(y) & !is.na(g); y <- y[ok]; g <- as.logical(g[ok]); n1 <- sum(g); n0 <- sum(!g)
  if (n1 < min1 || n0 < min0 || length(y) < 2 || sd(y) == 0) return(NULL)
  p <- tryCatch(suppressWarnings(wilcox.test(y[g], y[!g], exact = FALSE)$p.value), error = function(e) NA_real_)
  if (is.na(p)) return(NULL)
  m1 <- median(y[g]); m0 <- median(y[!g])
  list(n1 = n1, n0 = n0, m1 = m1, m0 = m0, diff = m1 - m0, eff = (m1 - m0) / sd(y), p = p)
}
ins_strength <- function(p, padj, eff, kind) {
  if (is.na(p) || is.na(eff)) return("none"); a <- abs(eff)
  s <- if (!is.na(padj) && padj < .05 && a >= .3) "strong" else if (p < .05 && a >= .2) "moderate" else if (p < .10 && a >= .2) "hint" else "none"
  if (kind == "sweet" && s == "strong") "moderate" else s      # the best bin is picked after looking, so cap it
}
ins_tone <- function(oid, sgn) {
  b <- INS_OUT[[oid]]$better
  if (b == "neutral" || sgn == 0) "neutral" else if ((b == "up") == (sgn > 0)) "good" else "bad"
}
ins_pct <- function(a, b) ifelse(is.na(a) | is.na(b) | b == 0, "n/a", sprintf("%+.0f%%", 100 * (a - b) / b))

# ---------------------------------------------------------------- pattern search ----
ins_sweet <- function(N) {     # which amount of sport goes with your best night?
  b <- cut(N$wk_min, c(-Inf, 0, 29, 59, 89, Inf), labels = c("rest", "1-29 min", "30-59 min", "60-89 min", "90+ min")); rows <- list()
  for (oid in c("eff", "deep_pct", "asleep_h", "hrv", "hr_night")) {
    y <- N[[oid]]; ok <- !is.na(y) & !is.na(b); if (sum(ok) < 20) next
    tb <- table(b[ok]); if (tb[["rest"]] < 6) next
    keep <- names(tb)[tb >= 5]; if (length(keep) < 3 || !("rest" %in% keep)) next
    z <- ok & (as.character(b) %in% keep); bz <- droplevels(b[z]); meds <- tapply(y[z], bz, median)
    act <- meds[names(meds) != "rest"]; if (!length(act)) next
    bst <- if (INS_OUT[[oid]]$better == "down") names(which.min(act)) else names(which.max(act))
    d <- meds[[bst]] - meds[["rest"]]
    if (!((INS_OUT[[oid]]$better == "up" && d > 0) || (INS_OUT[[oid]]$better == "down" && d < 0))) next
    p <- tryCatch(kruskal.test(y[z], bz)$p.value, error = function(e) NA_real_); if (is.na(p)) next
    rows[[length(rows) + 1]] <- data.frame(kind = "sweet", key = "sweet", out = oid, n = sum(z), n1 = sum(z & as.character(b) == bst), n0 = sum(z & as.character(b) == "rest"),
                                           m1 = meds[[bst]], m0 = meds[["rest"]], est = d, eff = d / sd(y[ok]), p = p, bin = bst, stringsAsFactors = FALSE)
  }
  if (length(rows)) do.call(rbind, rows) else NULL
}
ins_text <- function(r) {
  o <- INS_OUT[[r$out]]; dir <- if (r$eff > 0) o$up else o$dn
  amt <- function(v) paste(fmt(abs(v) * o$mult, o$dig), o$unit)
  if (r$kind == "group") {
    g <- INS_GRP[[r$key]]
    sprintf("On days with %s, your %s is typically %s %s than on %s (median %s vs %s; %d vs %d nights).",
            g$lab, o$lab, amt(r$est), dir, g$vs, ins_val(r$out, r$m1), ins_val(r$out, r$m0), r$n1, r$n0)
  } else if (r$kind == "sweet") {
    sprintf("Your %s is best after %s of sport: median %s vs %s on rest days (%d vs %d nights).", o$lab, r$bin, ins_val(r$out, r$m1), ins_val(r$out, r$m0), r$n1, r$n0)
  } else {
    pr <- INS_PRED[[r$key]]
    sprintf("For every %s, your %s is about %s %s (rank correlation %.2f, %d nights).", pr$unit, o$lab, amt(r$est), dir, r$eff, r$n)
  }
}
ins_advice <- function(key, oid, tone) {
  if (tone == "neutral") return("")
  bad <- tone == "bad"
  if (key %in% c("hard", "wk_trimp", "workout", "wk_min") && bad && oid %in% c("hr_night", "hr_sleep", "hrv", "dip"))
    return("Hard days leave a mark on your night-time heart rate and HRV: follow them with an easier day and avoid late hard sessions.")
  A <- list(
    nap      = c("Keep naps short (about 20-30 min) and early, or skip them on days when you want an early night.", "Naps do not seem to cost you at night, so keep them if they help."),
    long_nap = c("Cap naps at about 20-30 min; longer ones seem to cost you at night.", "Longer naps do not seem to hurt your night."),
    workout  = c("Training days seem to cost you sleep: check the timing (earlier is better) and the intensity.", "Training days go with better nights for you - a good reason to stay consistent."),
    hard     = c("Put hard sessions earlier in the day and plan an easier day afterwards.", "Your body copes well with hard sessions; keep watching resting HR and HRV anyway."),
    late     = c("Finish workouts at least 3 hours before bed when you can.", "Late workouts do not seem to hurt your sleep."),
    sweet    = c("", "Aim for a session of about that size on most training days - it looks like your sweet spot."),
    steps    = c("Unusually many steps go with worse nights: look at whether late-evening activity is the cause.", "More daily movement goes with better nights - protect it on busy days."),
    mem      = c("Many active minutes go with worse nights: allow extra recovery time on those days.", "More active minutes go with better nights."),
    wk_min   = c("Longer workouts go with worse nights: shorten or move them earlier.", "Longer workouts go with better nights."),
    wk_trimp = c("Higher workout load goes with worse nights: spread load more evenly.", "Higher workout load goes with better nights."),
    sed_h    = c("Long sedentary days go with worse nights: break up sitting at least once an hour.", "Sedentary hours do not hurt your nights."),
    stress   = c("Stressful days go with worse nights: a wind-down routine (screens off, light walk) is worth a try.", "Stress seems not to carry into your nights."),
    nap_len  = c("Longer naps go with worse nights: shorten them.", "Nap length does not hurt your night."),
    end_h    = c("Later finishes go with worse nights: move sessions earlier.", "Finish time does not hurt your sleep."),
    onset_min= c("Going to sleep later costs you: your wake time barely moves, so sleep gets shorter. Protect your bedtime.", "A later bedtime goes with better numbers for you."))
  a <- A[[key]]; if (is.null(a)) a <- c("Worth testing: change this for two weeks and watch the same metric.", "Keep doing this.")
  a[if (bad) 1 else 2]
}
ins_build <- function(N) {
  rows <- list()
  for (gk in names(INS_GRP)) {
    g <- INS_GRP[[gk]]$f(N); if (all(is.na(g))) next
    for (oid in names(INS_OUT)) {
      y <- N[[oid]]; if (is.null(y)) next
      t <- ins_grp_test(y, g); if (is.null(t)) next
      rows[[length(rows) + 1]] <- data.frame(kind = "group", key = gk, out = oid, n = t$n1 + t$n0, n1 = t$n1, n0 = t$n0, m1 = t$m1, m0 = t$m0,
                                             est = t$diff, eff = t$eff, p = t$p, bin = NA_character_, stringsAsFactors = FALSE)
    }
  }
  for (pk in names(INS_PRED)) for (oid in names(INS_OUT)) {
    if (oid == pk) next
    x <- N[[pk]]; y <- N[[oid]]; if (is.null(x) || is.null(y)) next
    ok <- !is.na(x) & !is.na(y); if (sum(ok) < 20 || sd(x[ok]) == 0 || sd(y[ok]) == 0) next
    ct <- suppressWarnings(cor.test(x[ok], y[ok], method = "spearman", exact = FALSE))
    sl <- ins_ts_slope(x[ok], y[ok]) * INS_PRED[[pk]]$scale; if (is.na(sl) || is.na(ct$p.value)) next
    rows[[length(rows) + 1]] <- data.frame(kind = "cont", key = pk, out = oid, n = sum(ok), n1 = NA_integer_, n0 = NA_integer_, m1 = NA_real_, m0 = NA_real_,
                                           est = sl, eff = unname(ct$estimate), p = ct$p.value, bin = NA_character_, stringsAsFactors = FALSE)
  }
  sw <- ins_sweet(N); if (!is.null(sw)) rows[[length(rows) + 1]] <- sw
  if (!length(rows)) return(NULL)
  R <- do.call(rbind, rows); rownames(R) <- NULL
  R$padj <- p.adjust(R$p, "BH")
  R$strength <- vapply(seq_len(nrow(R)), function(i) ins_strength(R$p[i], R$padj[i], R$eff[i], R$kind[i]), "")
  R$tone <- vapply(seq_len(nrow(R)), function(i) ins_tone(R$out[i], sign(R$eff[i])), "")
  R$text <- vapply(seq_len(nrow(R)), function(i) ins_text(R[i, ]), "")
  R$advice <- vapply(seq_len(nrow(R)), function(i) ins_advice(R$key[i], R$out[i], R$tone[i]), "")
  R
}
ins_key_lab <- function(k) {
  if (k %in% names(INS_GRP)) INS_GRP[[k]]$lab else if (k %in% names(INS_PRED)) INS_PRED[[k]]$lab else if (k == "sweet") "amount of sport" else k
}

# ---------------------------------------------------------------- period comparison ----
ins_win_range <- function(D, c0, c1, p0, p1) {
  cur <- D[D$day >= c0 & D$day <= c1, ]; prv <- D[D$day >= p0 & D$day <= p1, ]; n <- as.numeric(c1 - c0) + 1
  rows <- lapply(seq_len(nrow(INS_WM)), function(i) {
    id <- INS_WM$id[i]; a <- cur[[id]]; b <- prv[[id]]; f <- if (INS_WM$wk[i]) 7 else 1
    sdv <- sd(D[[id]], na.rm = TRUE); na <- sum(!is.na(a)); nb <- sum(!is.na(b)); mi <- ceiling(n / 3)
    es <- if (na >= mi && nb >= mi && !is.na(sdv) && sdv > 0) (mn(a) - mn(b)) / sdv else NA_real_
    p <- if (na >= 5 && nb >= 5) tryCatch(suppressWarnings(wilcox.test(a, b, exact = FALSE)$p.value), error = function(e) NA_real_) else NA_real_
    data.frame(INS_WM[i, ], cur = mn(a) * f, prv = mn(b) * f, es = es, p = p, stringsAsFactors = FALSE)
  })
  list(tab = do.call(rbind, rows), last = c1, n = n)
}
ins_win <- function(D, n) {
  has <- !is.na(D$steps) | !is.na(D$asleep_h) | !is.na(D$hr_night); last <- max(D$day[has])
  ins_win_range(D, last - n + 1, last, last - 2 * n + 1, last - n)
}
ins_sig <- function(t) ifelse(is.na(t$es) | abs(t$es) < .2, "steady", ifelse((t$better == "up") == (t$es > 0), "better", "worse"))
ins_dom <- function(t) {
  t$sig <- ins_sig(t)
  lapply(unique(t$dom), function(d) {
    z <- t[t$dom == d, ]; nb <- sum(z$sig == "better"); nw <- sum(z$sig == "worse"); has <- sum(!is.na(z$es))
    v <- if (has == 0) "no data" else if (nb > 0 && nw > 0) "mixed" else if (nb > 0) "better" else if (nw > 0) "worse" else "steady"
    list(dom = d, verdict = v, rows = z)
  })
}

# ---------------------------------------------------------------- sports started / stopped ----
ins_sportchg <- function(W, last, n) {
  if (is.null(W) || !nrow(W)) return(NULL)
  nn <- max(n, 14)
  cur <- W[W$day > last - nn & W$day <= last, ]; base <- W[W$day > last - nn - 56 & W$day <= last - nn, ]
  sp <- unique(W$sport[W$day > last - nn - 84 & W$day <= last]); if (!length(sp)) return(NULL)
  tab <- do.call(rbind, lapply(sp, function(s) data.frame(sport = s, cs = sum(cur$sport == s), cm = sum(cur$dur[cur$sport == s], na.rm = TRUE),
                                                          bs = sum(base$sport == s), bm = sum(base$dur[base$sport == s], na.rm = TRUE), stringsAsFactors = FALSE)))
  txt <- character(); tone <- character()
  for (i in which(tab$cs == 0 & tab$bs >= 3)) { txt <- c(txt, sprintf("You stopped <b>%s</b>: %d sessions (%s min) in the 8 weeks before, none in the last %d days.", tab$sport[i], tab$bs[i], fmt(tab$bm[i]), nn)); tone <- c(tone, "bad") }
  for (i in which(tab$cs > 0 & tab$bs == 0)) { txt <- c(txt, sprintf("New: <b>%s</b> - %d session(s), %s min in the last %d days.", tab$sport[i], tab$cs[i], fmt(tab$cm[i]), nn)); tone <- c(tone, "good") }
  for (i in which(tab$cs > 0 & tab$bs >= 3)) {
    rate <- tab$bm[i] * nn / 56; ch <- (tab$cm[i] - rate) / rate
    if (!is.na(ch) && abs(ch) >= .4 && abs(tab$cm[i] - rate) >= 30) {
      txt <- c(txt, sprintf("<b>%s</b>: %s min in the last %d days vs %s min on average per %d days before (%+.0f%%).", tab$sport[i], fmt(tab$cm[i]), nn, fmt(rate), nn, 100 * ch))
      tone <- c(tone, if (ch > 0) "good" else "bad") }
  }
  list(tab = tab, txt = txt, tone = tone, stopped = tab$sport[tab$cs == 0 & tab$bs >= 3], nn = nn)
}

# ---------------------------------------------------------------- fitness verdict ----
ins_fit <- function(D, L, V) {
  has <- !is.na(D$hr_night) | !is.na(D$steps) | !is.na(D$asleep_h)
  span <- as.numeric(max(D$day[has]) - min(D$day[has])) + 1
  k <- if (span >= 120) 28 else if (span >= 56) 21 else 14
  if (span < 2 * k) return(list(ok = FALSE, msg = sprintf("A fitness verdict needs about %d days of data (you have %d in the selected range).", 2 * k, round(span))))
  W <- ins_win(D, k); t <- W$tab[W$tab$id %in% c("hr_night", "hrv", "vo2", "dip"), ]; t$sig <- ins_sig(t)
  nb <- sum(t$sig == "better"); nw <- sum(t$sig == "worse"); lines <- character()
  for (i in seq_len(nrow(t))) if (!is.na(t$es[i])) lines <- c(lines, sprintf("%s: %s vs %s before (%s)", t$lab[i], fmt(t$cur[i], t$dig[i]), fmt(t$prv[i], t$dig[i]), t$sig[i]))
  if (!is.null(L) && nrow(L) > 2 * k) {
    i <- nrow(L); c1 <- L$ctl[i]; c0 <- L$ctl[i - k]
    if (!is.na(c0) && c0 > 5) { r <- (c1 - c0) / c0; sg <- if (r > .1) "better" else if (r < -.1) "worse" else "steady"
    nb <- nb + (sg == "better"); nw <- nw + (sg == "worse")
    lines <- c(lines, sprintf("Training fitness load (28-day EWMA): %.0f vs %.0f, %d days ago (%s)", c1, c0, k, sg)) }
  }
  if (!is.null(V) && nrow(V)) {
    v <- V[!is.na(V$est), ]; last <- W$last
    a <- v$est[v$day > last - k & v$day <= last]; b <- v$est[v$day > last - 2 * k & v$day <= last - k]
    if (length(a) >= 2 && length(b) >= 2) { dm <- median(a) - median(b); sg <- if (dm > 1) "better" else if (dm < -1) "worse" else "steady"
    nb <- nb + (sg == "better"); nw <- nw + (sg == "worse")
    lines <- c(lines, sprintf("VO2max estimate from your runs: %.1f vs %.1f ml/kg/min (%s)", median(a), median(b), sg)) }
  }
  ts <- trend_stats(D$day, D$hr_night)
  if (!is.na(ts$p) && ts$p < .05) lines <- c(lines, sprintf("Long-term sleeping-HR trend: %+.1f bpm per month (p = %.2g)", ts$slope * 30, ts$p))
  verdict <- if (nb > nw) "improving" else if (nw > nb) "declining" else "stable"
  list(ok = TRUE, verdict = verdict, conf = if (max(nb, nw) >= 2) "several independent signs agree" else "only tentative signs", nb = nb, nw = nw, lines = lines, k = k)
}

# ---------------------------------------------------------------- next-week plan ----
ins_plan <- function(D, W, L, S, fit, R, stp, N, need, goal, act_goal) {
  has <- !is.na(D$steps) | !is.na(D$asleep_h); last <- max(D$day[has]); out <- list()
  d7 <- D$day > last - 7 & D$day <= last; d14 <- D$day > last - 14 & D$day <= last; d28 <- D$day > last - 28 & D$day <= last
  l1 <- sum(D$trimp[d7], na.rm = TRUE); b4 <- sum(D$trimp[d28], na.rm = TRUE) / 4
  ac <- if (!is.null(L) && nrow(L) >= 28) L$atl[nrow(L)] / max(L$ctl[nrow(L)], 1e-6) else NA_real_
  rd <- if (!is.null(S)) { x <- S$readiness[S$day <= last]; x <- x[!is.na(x)]; if (length(x)) mean(tail(x, 3)) else NA_real_ } else NA_real_
  wr <- if (!is.null(S)) { x <- S$warn[S$day <= last]; x <- x[!is.na(x)]; if (length(x)) tail(x, 1) else NA_real_ } else NA_real_
  declining <- !is.null(fit) && isTRUE(fit$ok) && fit$verdict == "declining"
  improving <- !is.null(fit) && isTRUE(fit$ok) && fit$verdict == "improving"
  # ---- training
  tr <- character()
  if (is.finite(b4) && b4 > 0) {
    mode <- if (isTRUE(wr >= 2) || isTRUE(rd < 40) || isTRUE(ac > 1.5)) "recover" else if (isTRUE(ac > 1.3)) "hold" else if ((declining && !isTRUE(ac >= 1.1)) || isTRUE(ac < 0.8)) "build" else "maintain"
    tgt <- b4 * c(recover = .6, hold = .95, maintain = 1, build = 1.12)[[mode]]
    if (mode == "build" && l1 > 0) tgt <- min(tgt, l1 * 1.15)
    why <- switch(mode,
                  recover = paste(c(if (isTRUE(wr >= 2)) sprintf("%d exhaustion warning signs", wr), if (isTRUE(rd < 40)) sprintf("readiness %.0f", rd), if (isTRUE(ac > 1.5)) sprintf("recent load %.2f x your usual", ac)), collapse = ", "),
                  hold = sprintf("recent load is %.2f x your usual", ac),
                  build = if (declining) "your fitness indicators are slipping while the load is not high" else sprintf("your recent load is low (%.2f x your usual)", ac),
                  maintain = "load and recovery signs are balanced")
    head <- c(recover = "Recovery week", hold = "Hold steady", build = "Build week", maintain = "Maintain")[[mode]]
    nsess <- if (!is.null(W) && nrow(W)) sum(W$day > last - 28 & W$day <= last) / 4 else NA_real_
    hmax <- c(recover = 0, hold = 1, maintain = 1, build = if (!is.na(nsess) && nsess >= 3.5) 2 else 1)[[mode]]
    tr <- c(sprintf("<b>%s</b> (%s). Aim for a weekly workout load of about <b>%.0f-%.0f TRIMP</b> (last 7 days %.0f, 4-week average %.0f).", head, why, tgt * .9, tgt * 1.1, l1, b4),
            sprintf("About %s sessions, at most <b>%d hard</b> (hard = above your 75th-percentile session load); everything else easy enough to hold a conversation.",
                    if (is.na(nsess)) "3" else sprintf("%.0f-%.0f", max(1, floor(nsess * c(recover = .6, hold = .9, maintain = 1, build = 1)[[mode]])), max(2, ceiling(nsess * c(recover = .8, hold = 1, maintain = 1.1, build = 1.25)[[mode]]))), hmax))
  } else tr <- "No workout load recorded recently: start with 2-3 easy sessions of 30 min and build towards 150 moderate minutes per week."
  if (improving) tr <- c(tr, "Your fitness indicators are improving - keep the current mix and do not raise load by more than about 10% a week.")
  if (declining) tr <- c(tr, "Fitness indicators are slipping: consistency matters more than intensity - add one extra easy session rather than one harder session.")
  if (length(stp$stopped)) tr <- c(tr, sprintf("You have not done %s lately. If it was part of what kept you fit, schedule one session this week.", paste0("<b>", stp$stopped, "</b>", collapse = ", ")))
  out[[1]] <- list(h = "Training", items = tr)
  # ---- activity
  ac_i <- character(); med <- md(D$steps[d28])
  if (!is.na(med)) { tg <- if (med >= goal) round(med / 100) * 100 else min(goal, round(med * 1.1 / 100) * 100)
  ac_i <- c(ac_i, sprintf("Steps: your 4-week median is %s; aim for about <b>%s per day</b>%s.", fmt(med), fmt(tg), if (med >= goal) " (you are at your goal - hold it)" else sprintf(" (goal %s)", fmt(goal)))) }
  m7 <- sum(D$mem[d7], na.rm = TRUE); if (sum(!is.na(D$mem[d7])) >= 4)
    ac_i <- c(ac_i, sprintf("Active minutes: %s moderate-equivalent minutes in the last 7 days vs the WHO minimum of 150 and your own goal of %s a day.", fmt(m7), fmt(act_goal)))
  sd60 <- mn(D$sed60[d7]) / 60; if (!is.na(sd60) && sd60 > 3) ac_i <- c(ac_i, sprintf("You spend about %.1f h a day in sitting stretches of 60+ minutes: stand up or walk 2-3 minutes every hour.", sd60))
  if (length(ac_i)) out[[length(out) + 1]] <- list(h = "Daily activity", items = ac_i)
  # ---- sleep
  sl <- character(); def <- mn(need - D$asleep_h[d14])
  if (!is.na(def) && def > .25) sl <- c(sl, sprintf("You sleep about %.0f min less than your need on average (last 14 nights): go to bed <b>about %.0f min earlier</b>.", def * 60, min(45, round(def * 60 / 5) * 5)))
  if (!is.null(N) && nrow(N) >= 15) {
    sc <- rank(N$asleep_h, na.last = "keep") + rank(N$eff, na.last = "keep"); top <- !is.na(sc) & sc >= quantile(sc, 2 / 3, na.rm = TRUE)
    if (sum(top) >= 5) sl <- c(sl, sprintf("On your best-sleep nights you fell asleep between <b>%s and %s</b> - use that as your target window.", ins_clock(quantile(N$onset_min[top], .25, names = FALSE)), ins_clock(quantile(N$onset_min[top], .75, names = FALSE))))
    rec <- tail(N$onset_min[!is.na(N$onset_min)], 14)
    if (length(rec) >= 7 && sd(rec) > 60) sl <- c(sl, sprintf("Your bedtime varied by about %.0f min (SD) over the last nights; a steadier bedtime improves sleep quality independent of duration.", sd(rec)))
  }
  if (length(sl)) out[[length(out) + 1]] <- list(h = "Sleep", items = sl)
  # ---- from your patterns
  if (!is.null(R) && nrow(R)) {
    r <- R[R$strength %in% c("strong", "moderate") & R$tone != "neutral" & nzchar(R$advice), ]
    if (nrow(r)) {
      r <- r[order(match(r$strength, c("strong", "moderate")), -abs(r$eff)), ]
      bad <- r[r$tone == "bad", ]; bad <- bad[!duplicated(bad$advice), ]; good <- r[r$tone == "good", ]; good <- good[!duplicated(good$advice), ]
      it <- c(if (nrow(bad)) paste0("Watch out: ", head(bad$advice, 3)), if (nrow(good)) paste0("Keep: ", head(good$advice, 2)))
      if (length(it)) out[[length(out) + 1]] <- list(h = "From your own patterns", items = it)
    }
  }
  out
}

# ---------------------------------------------------------------- report helpers ----
ins_html_table <- function(df) {
  if (is.null(df) || !nrow(df)) return("")
  paste0("<table class='rt'><thead><tr>", paste0("<th>", names(df), "</th>", collapse = ""), "</tr></thead><tbody>",
         paste(apply(df, 1, function(r) paste0("<tr>", paste0("<td>", r, "</td>", collapse = ""), "</tr>")), collapse = ""), "</tbody></table>")
}
if_else_html <- function(a) ifelse(nzchar(a), paste0("<br><em>", a, "</em>"), "")
ins_sigspan <- function(s) {
  col <- c(better = "#10b981", worse = "#ef4444", steady = "#9ca3af")[s]
  sprintf("<span style='color:%s;font-weight:600'>%s</span>", col, s)
}
ins_doc <- function(title, body) paste0("<!DOCTYPE html><html><head><meta charset='utf-8'><title>", title, "</title><style>",
                                        "body{font-family:Inter,system-ui,Segoe UI,Arial,sans-serif;max-width:900px;margin:30px auto;padding:0 18px;color:#1f2933;line-height:1.5}",
                                        "h1{font-size:1.6rem;margin-bottom:0}h2{font-size:1.15rem;margin-top:1.6em;border-bottom:1px solid #ddd;padding-bottom:4px}.sub{color:#6b7280;margin-top:2px}",
                                        "table.rt{border-collapse:collapse;width:100%;font-size:.9rem}.rt th,.rt td{text-align:left;padding:4px 8px;border-bottom:1px solid #e5e7eb}.rt th{background:#f3f4f6}",
                                        ".good{border-left:4px solid #10b981;padding-left:10px;margin:6px 0}.bad{border-left:4px solid #ef4444;padding-left:10px;margin:6px 0}",
                                        ".small{color:#6b7280;font-size:.8rem}", INS_CSS, "</style></head><body>", body, "</body></html>")

# ---------------------------------------------------------------- visuals: usual-range bands, period so far vs previous ----
fmtv <- function(x, d) { d <- rep_len(d, length(x)); vapply(seq_along(x), function(i) fmt(x[i], d[i]), "") }

INS_CSS <- paste0(
  ".ins-grid{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:14px}@media(max-width:820px){.ins-grid{grid-template-columns:1fr}}",
  ".ins-card{padding:6px 2px}.ins-card svg{display:block}",
  ".ins-t{font-weight:600;margin-bottom:2px}.ins-s{font-size:.85rem;opacity:.8;margin-top:2px}")

ins_svg_open <- function(w, h)
  sprintf("<svg viewBox='0 0 %d %d' style='width:100%%;height:auto;max-width:%dpx' xmlns='http://www.w3.org/2000/svg' font-family='Inter,system-ui,Arial,sans-serif' font-size='11' fill='currentColor'>", w, h, w)

# ---------------------------------------------------------------- baseline bands ----
# usual range for each day = median +/- k robust SD of the previous 30 days (needs >= 10 values)
ins_baseline <- function(day, x, win = 30, minn = 10, k = 1.28) {
  n <- length(x); lo <- hi <- med <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    j <- which(day < day[i] & day >= day[i] - win & !is.na(x))
    if (length(j) < minn) next
    v <- x[j]; m <- median(v); s <- mad(v)
    if (!is.finite(s) || s == 0) s <- sd(v)
    if (!is.finite(s) || s == 0) next
    med[i] <- m; lo[i] <- m - k * s; hi[i] <- m + k * s
  }
  data.frame(day = day, x = x, med = med, lo = lo, hi = hi)
}

ins_svg_band <- function(B, c0, c1, better, dig, span = 42) {
  s0 <- c1 - span + 1; B <- B[B$day >= s0 & B$day <= c1, ]
  if (sum(is.finite(B$x)) < 3) return(NULL)
  W <- 640; H <- 180; ml <- 46; mr <- 10; mt <- 10; mb <- 24
  v <- c(B$x, B$lo, B$hi); v <- v[is.finite(v)]; lo <- min(v); hi <- max(v); pad <- (hi - lo) * .08; if (pad == 0) pad <- 1; lo <- lo - pad; hi <- hi + pad
  X <- function(d) ml + as.numeric(d - s0) / (span - 1) * (W - ml - mr)
  Y <- function(y) mt + (hi - y) / (hi - lo) * (H - mt - mb)
  o <- ins_svg_open(W, H)
  tk <- pretty(c(lo, hi), 4); tk <- tk[tk >= lo & tk <= hi]
  o <- paste0(o, paste0(sprintf("<line x1='%d' x2='%d' y1='%.1f' y2='%.1f' stroke='currentColor' opacity='.12'/><text x='%d' y='%.1f' text-anchor='end' opacity='.7'>%s</text>",
                                ml, W - mr, Y(tk), Y(tk), ml - 5, Y(tk) + 4, fmtv(tk, dig)), collapse = ""))
  xt <- seq(c1, s0, by = -7)
  o <- paste0(o, paste0(sprintf("<text x='%.1f' y='%d' text-anchor='middle' opacity='.7'>%s</text>", X(xt), H - 6, format(xt, "%d %b")), collapse = ""))
  p0 <- max(c0, s0)                                           # highlight the report period
  o <- paste0(o, sprintf("<rect x='%.1f' y='%d' width='%.1f' height='%d' fill='currentColor' opacity='.06'/>", X(p0) - 3, mt, X(c1) - X(p0) + 6, H - mt - mb))
  bb <- B[is.finite(B$lo) & is.finite(B$hi), ]
  if (nrow(bb) >= 2) {
    pts <- c(sprintf("%.1f,%.1f", X(bb$day), Y(bb$hi)), rev(sprintf("%.1f,%.1f", X(bb$day), Y(bb$lo))))
    o <- paste0(o, "<polygon points='", paste(pts, collapse = " "), "' fill='#9ca3af' opacity='.22'/>")
    o <- paste0(o, "<polyline points='", paste(sprintf("%.1f,%.1f", X(bb$day), Y(bb$med)), collapse = " "), "' fill='none' stroke='#9ca3af' stroke-dasharray='4 3' stroke-width='1.2'/>")
  }
  vv <- B[is.finite(B$x), ]
  o <- paste0(o, "<polyline points='", paste(sprintf("%.1f,%.1f", X(vv$day), Y(vv$x)), collapse = " "), "' fill='none' stroke='currentColor' opacity='.4' stroke-width='1.3'/>")
  st <- ifelse(is.finite(vv$lo) & vv$x < vv$lo, "low", ifelse(is.finite(vv$hi) & vv$x > vv$hi, "high", "in"))
  good <- if (better == "up") "high" else "low"; bad <- if (better == "up") "low" else "high"
  col <- ifelse(st == good, "#34d399", ifelse(st == bad, "#ef4444", "#9ca3af")); rad <- ifelse(vv$day >= c0, 4, 2.6)
  o <- paste0(o, paste0(sprintf("<circle cx='%.1f' cy='%.1f' r='%.1f' fill='%s'><title>%s: %s</title></circle>", X(vv$day), Y(vv$x), rad, col, format(vv$day, "%a %d %b"), fmtv(vv$x, dig)), collapse = ""))
  paste0(o, "</svg>")
}

ins_band_text <- function(B, c0, c1, better, dig) {
  z <- B[B$day >= c0 & B$day <= c1 & is.finite(B$x), ]
  if (!nrow(z)) return("No data in this period.")
  ok <- z[is.finite(z$lo), ]
  if (nrow(ok) < 3) return("Not enough earlier days yet to define your usual range (needs about 10).")
  nl <- sum(ok$x < ok$lo); nh <- sum(ok$x > ok$hi); ni <- nrow(ok) - nl - nh
  nbad <- if (better == "up") nl else nh; ngood <- if (better == "up") nh else nl; thr <- max(2, ceiling(nrow(ok) / 3))
  head <- if (nbad >= thr && nbad > ngood) "<span style='color:#ef4444;font-weight:600'>Mostly worse than usual.</span> "
  else if (ngood >= thr && ngood > nbad) "<span style='color:#10b981;font-weight:600'>Mostly better than usual.</span> "
  else "<span style='font-weight:600'>Within your normal range.</span> "
  paste0(head, sprintf("%d of %d days inside it, %d better, %d worse. Usual range %s to %s (median %s), from the 30 days before each day.",
                       ni, nrow(ok), ngood, nbad, fmt(mean(ok$lo), dig), fmt(mean(ok$hi), dig), fmt(mean(ok$med), dig)))
}

ins_band_html <- function(D, c0, c1, ids = c("hrv", "hr_night", "asleep_h", "stress")) {
  out <- character()
  for (id in ids) {
    m <- INS_WM[INS_WM$id == id, ]; if (!nrow(m) || is.null(D[[id]])) next
    B <- ins_baseline(D$day, D[[id]]); s <- ins_svg_band(B, c0, c1, m$better, m$dig); if (is.null(s)) next
    out <- c(out, sprintf("<div class='ins-card'><div class='ins-t'>%s</div>%s<div class='ins-s'>%s</div></div>", m$lab, s, ins_band_text(B, c0, c1, m$better, m$dig)))
  }
  out
}

# ---------------------------------------------------------------- period so far vs previous period ----
# additive metrics only (cumulative sums make sense for these; averages like HRV are covered by the bands)
INS_CUM <- list(steps = "Steps", mem = "Active minutes", sport_min = "Workout minutes", sessions = "Workout sessions", trimp = "Workout load (TRIMP)")
# to add more (e.g. calories or distance), add  colname = "Label"  here, if D has that daily column

ins_cum_prep <- function(D, W) {          # adds a daily "sessions" column from the workout table
  if (!is.null(W) && nrow(W) && is.null(D$sessions)) { tb <- table(as.character(W$day)); D$sessions <- as.numeric(tb[as.character(D$day)]) }
  D
}

# unit = "month": start = first day of the month, compared with the month before
# unit = "week" : start = the Monday,             compared with the week before
ins_cum_html <- function(D, start, last, unit = "month", ids = names(INS_CUM), W = NULL) {
  D <- ins_cum_prep(D, W); month <- identical(unit, "month"); per <- if (month) "month" else "week"
  mdays <- function(s) as.numeric(seq(s, by = "month", length.out = 2)[2] - s)
  if (month) { pm0 <- seq(start, by = "-1 month", length.out = 2)[2]; nd <- mdays(start); pd <- mdays(pm0) } else { pm0 <- start - 7; nd <- 7; pd <- 7 }
  el <- min(nd, as.numeric(last - start) + 1); out <- character(); if (el < 1) return(out)
  lab1 <- if (month) format(start, "%B") else paste(format(start, "%d %b"), "-", format(start + 6, "%d %b"))
  lab0 <- if (month) format(pm0, "%B") else paste(format(pm0, "%d %b"), "-", format(pm0 + 6, "%d %b"))
  for (id in ids) {
    x <- D[[id]]; if (is.null(x)) next
    cs <- function(s, n) { v <- x[match(s + 0:(n - 1), D$day)]; v[is.na(v)] <- 0; cumsum(v) }
    cur <- cs(start, el); prv <- cs(pm0, pd); if (max(cur, prv) <= 0) next
    W_ <- 640; H <- 190; ml <- 52; mr <- 12; mt <- 12; mb <- 24; xm <- max(nd, pd); ym <- max(cur, prv) * 1.1
    X <- function(d) ml + (d - 1) / (xm - 1) * (W_ - ml - mr); Y <- function(y) mt + (ym - y) / ym * (H - mt - mb)
    o <- ins_svg_open(W_, H)
    tk <- pretty(c(0, ym), 4); tk <- tk[tk <= ym]; dg <- if (all(abs(tk - round(tk)) < 1e-9)) 0 else 1
    o <- paste0(o, paste0(sprintf("<line x1='%d' x2='%d' y1='%.1f' y2='%.1f' stroke='currentColor' opacity='.12'/><text x='%d' y='%.1f' text-anchor='end' opacity='.7'>%s</text>",
                                  ml, W_ - mr, Y(tk), Y(tk), ml - 5, Y(tk) + 4, fmtv(tk, dg)), collapse = ""))
    xt <- if (month) unique(c(1, seq(5, nd, by = 5), nd)) else 1:7
    xl <- if (month) as.character(as.integer(xt)) else format(start + 0:6, "%a")
    o <- paste0(o, paste0(sprintf("<text x='%.1f' y='%d' text-anchor='middle' opacity='.7'>%s</text>", X(xt), H - 6, xl), collapse = ""))
    o <- paste0(o, sprintf("<line x1='%.1f' x2='%.1f' y1='%.1f' y2='%.1f' stroke='currentColor' stroke-dasharray='5 4' opacity='.5'/><text x='%.1f' y='%.1f' text-anchor='end' opacity='.75'>last %s: %s</text>",
                           X(1), X(xm), Y(prv[pd]), Y(prv[pd]), X(xm), Y(prv[pd]) - 4, per, fmt(prv[pd])))
    o <- paste0(o, "<polyline points='", paste(sprintf("%.1f,%.1f", X(seq_len(pd)), Y(prv)), collapse = " "), "' fill='none' stroke='#9ca3af' stroke-width='2.2' stroke-linejoin='round'/>")
    if (!month) o <- paste0(o, paste0(sprintf("<circle cx='%.1f' cy='%.1f' r='2.8' fill='#9ca3af'/>", X(seq_len(pd)), Y(prv)), collapse = ""))
    proj <- if (el >= (if (month) 5 else 3) && el < nd) cur[el] / el * nd else NA_real_
    if (!is.na(proj)) o <- paste0(o, sprintf("<line x1='%.1f' y1='%.1f' x2='%.1f' y2='%.1f' stroke='#f97316' stroke-width='2' stroke-dasharray='2 4' opacity='.7'/>", X(el), Y(cur[el]), X(nd), Y(proj)))
    o <- paste0(o, "<polyline points='", paste(sprintf("%.1f,%.1f", X(seq_len(el)), Y(cur)), collapse = " "), "' fill='none' stroke='#f97316' stroke-width='2.8' stroke-linejoin='round'/>")
    if (!month) o <- paste0(o, paste0(sprintf("<circle cx='%.1f' cy='%.1f' r='2.8' fill='#f97316'/>", X(seq_len(el)), Y(cur)), collapse = ""))
    o <- paste0(o, sprintf("<circle cx='%.1f' cy='%.1f' r='8' fill='#f97316' opacity='.25'/><circle cx='%.1f' cy='%.1f' r='4.2' fill='#f97316'><title>day %d: %s</title></circle></svg>", X(el), Y(cur[el]), X(el), Y(cur[el]), el, fmt(cur[el])))
    same <- prv[min(el, pd)]
    txt <- sprintf("After %d of %d days: <b>%s</b> vs %s at the same point last %s (%s). Last %s ended at %s.%s", el, nd, fmt(cur[el]), fmt(same), per, ins_pct(cur[el], same), per, fmt(prv[pd]),
                   if (!is.na(proj)) sprintf(" On this pace the %s ends at about <b>%s</b>.", per, fmt(proj)) else "")
    out <- c(out, sprintf("<div class='ins-card'><div class='ins-t'>%s - %s vs %s</div>%s<div class='ins-s'>%s</div></div>", INS_CUM[[id]], lab1, lab0, o, txt))
  }
  out
}

# visuals block for the weekly / monthly report (c0 = Monday or first of month)
ins_rep_visuals <- function(D, W, c0, c1, monthly) {
  has <- D$day[!is.na(D$steps) | !is.na(D$asleep_h)]; last <- max(has); e <- min(c1, last); h <- ""
  b <- ins_band_html(D, c0, e)
  if (length(b)) h <- paste0(h, "<h2>Recovery vs your usual range</h2><p class='small'>Grey band = your usual range (median and about 80% of your normal days, from the 30 days before each day). Green = better than usual, red = worse, the shaded strip is this report's period.</p><div class='ins-grid'>", paste(b, collapse = ""), "</div>")
  cm <- ins_cum_html(D, c0, e, if (monthly) "month" else "week", W = W)
  if (length(cm)) h <- paste0(h, "<h2>", if (monthly) "Month" else "Week", if (e < c1) " so far" else "", " vs previous ", if (monthly) "month" else "week", "</h2><div class='ins-grid'>", paste(cm, collapse = ""), "</div>")
  h
}

# ================================================================== UI ===========
# one menu "Insights" with three tabs: Overview, Weekly report, Monthly report
insights_nav <- function() nav_menu("Insights",
                                    nav_panel("Overview",
                                              tags$style(HTML(INS_CSS)),
                                              div(class = "d-flex flex-wrap align-items-end gap-3",
                                                  selectInput("ins_win", "Compare", c("Last 7 days vs the 7 before" = "7", "Last 14 days vs the 14 before" = "14", "Last 30 days vs the 30 before" = "30"), "7", width = "290px"),
                                                  selectInput("ins_min", "Show patterns", c("Strong only" = "strong", "Strong and moderate" = "moderate", "Include early hints" = "hint"), "moderate", width = "230px"),
                                                  tags$span(class = "text-muted small pb-3", "uses the date range from the sidebar - widen it to find more reliable patterns")),
                                              uiOutput("ins_cards"),
                                              grid2(sect("What changed", uiOutput("ins_story")), sect("Plan for next week", uiOutput("ins_plan"))),
                                              sect("Recovery vs your usual range", uiOutput("ins_band"), note = "Grey band = your usual range (median and about 80% of normal days, from the 30 days before each day). Green dots are better than usual, red worse. The shaded strip is the period selected above."),
                                              sect("This month vs last month", uiOutput("ins_cum"), note = "Orange = this month, grey = last month, dashed = last month's total, dotted = projection at the current pace. Only metrics that add up over time are shown."),
                                              sect("Patterns in your own data", uiOutput("ins_top"),
                                                   note = "Observational patterns from your own nights, not proof of cause. 'Strong' means the adjusted p (Benjamini-Hochberg over every pair tested) is below 0.05 and the effect is at least 0.3 SD / rho 0.3; 'moderate' p < 0.05 and 0.2; 'hint' p < 0.10 and 0.2. A night counts as the night after the day on which the nap, workout or activity happened."),
                                              grid2(sect("Change vs the previous period", pH("ins_p_change", 420), note = "Bars are changes in SD of your own daily values; colour says whether that is better or worse for you. Bars between the dotted lines are within normal day-to-day noise."),
                                                    sect("Fitness", uiOutput("ins_fit_txt"), pH("ins_p_fit", 340), note = "Each line is a 14-day mean of one indicator, shown as z-score so that higher always means fitter.")),
                                              sect("Explore a pattern",
                                                   layout_columns(col_widths = breakpoints(sm = 12, md = 6),
                                                                  selectInput("ins_cond", "When you had ...", c("a nap" = "nap", "a long nap (45+ min) vs a short one" = "long_nap", "a workout (vs rest day)" = "workout",
                                                                                                                "a hard workout" = "hard", "a late workout (after 19:30)" = "late"), "nap"),
                                                                  selectInput("ins_out", "... look at", setNames(names(INS_OUT), vapply(INS_OUT, function(o) o$lab, "")), "onset_min")),
                                                   grid2(pH("ins_p_grp", 400), pH("ins_p_dose", 400)),
                                                   note = "Left: the selected comparison. Right: the same outcome against the amount of sport on the day (the boxes show the spread of nights, each dot is one night)."),
                                              sect("Activity mix over time", pH("ins_p_mix", 400), uiOutput("ins_sports")),
                                              sect("Every relationship that was tested", tableOutput("ins_tbl"),
                                                   note = "Effect = median difference (group tests, in the unit of the outcome) or rank correlation rho (continuous). Adjusted p corrects for the number of pairs tested.")),
                                    nav_panel("Weekly report",
                                              tags$style(HTML(INS_CSS)),
                                              sect("Weekly report",
                                                   div(class = "d-flex flex-wrap align-items-end gap-3",
                                                       selectInput("ins_wk_period", "Week (Monday - Sunday)", NULL, width = "300px"),
                                                       div(class = "pb-3 d-flex gap-2",
                                                           downloadButton("ins_wk_dl", "Download this week", class = "btn-sm btn-primary"),
                                                           downloadButton("ins_wk_dl_all", "Download all weeks", class = "btn-sm btn-outline-secondary"))),
                                                   uiOutput("ins_wk_report"),
                                                   note = "A weekly report covers Monday to Sunday and is compared with the week before. 'Download all weeks' builds one file with a report for every week that has at least 3 days of data (newest first, one per printed page). Open the HTML in any browser; Print > Save as PDF.")),
                                    nav_panel("Monthly report",
                                              tags$style(HTML(INS_CSS)),
                                              sect("Monthly report",
                                                   div(class = "d-flex flex-wrap align-items-end gap-3",
                                                       selectInput("ins_mo_period", "Month", NULL, width = "300px"),
                                                       div(class = "pb-3 d-flex gap-2",
                                                           downloadButton("ins_mo_dl", "Download this month", class = "btn-sm btn-primary"),
                                                           downloadButton("ins_mo_dl_all", "Download all months", class = "btn-sm btn-outline-secondary"))),
                                                   uiOutput("ins_mo_report"),
                                                   note = "A monthly report covers the calendar month and is compared with the month before. 'Download all months' builds one file with a report for every month that has at least 3 days of data (newest first, one per printed page).")))

# ================================================================== SERVER =======
insights_server <- function(input, output, session, env) {
  G <- function(n) get(n, envir = env)
  Dt <- G("Dt"); SES <- G("SES"); Wm <- G("Wm"); HRVN <- G("HRVN"); LOAD <- G("LOAD"); SC <- G("SC"); TH <- G("TH"); VO2R <- G("VO2R")
  
  DD <- reactive({
    d <- Dt(); h <- tryCatch(HRVN(), error = function(e) NULL)
    d$hrv <- if (is.null(h) || !nrow(h)) NA_real_ else h$v[match(d$day, h$day)]
    d
  })
  # one row per night; the night belongs to the evening day it started on (so "day d" activity -> "night of d")
  NT <- reactive({
    s <- SES(); shiny::validate(shiny::need(!is.null(s) && any(s$kind == "Night"), "No nights detected yet."))
    D <- DD(); w <- Wm(); tz <- input$tz
    tzt <- function(t) as.POSIXct(t, origin = "1970-01-01", tz = tz)
    clk <- function(t) { x <- tzt(t); (as.numeric(format(x, "%H")) * 60 + as.numeric(format(x, "%M")) - 18 * 60) %% 1440 }
    n <- s[s$kind == "Night", ]; n$kd <- as.Date(tzt(n$start) - 6 * 3600, tz = tz)
    n <- n[order(n$kd, -n$asleep), ]; n <- n[!duplicated(n$kd), ]; stg <- n$deep + n$light + n$rem
    N <- data.frame(kd = n$kd, wake = n$wake_date, onset_min = clk(n$start), asleep_h = n$asleep / 60, eff = n$eff,
                    deep_pct = ifelse(stg > 0, 100 * n$deep / stg, NA_real_), hr_night = n$hr_p5, hr_sleep = n$hr_med)
    N$hrv <- D$hrv[match(N$wake, D$day)]; N$dip <- D$dip[match(N$wake, D$day)]
    N$steps <- D$steps[match(N$kd, D$day)]; N$mem <- D$mem[match(N$kd, D$day)]; N$sed_h <- D$sed_h[match(N$kd, D$day)]; N$stress <- D$stress[match(N$kd, D$day)]
    p <- s[s$kind == "Nap", ]; N$nap_min <- 0
    if (nrow(p)) { a <- tapply(p$asleep, as.character(as.Date(tzt(p$end), tz = tz)), sum); k <- match(as.character(N$kd), names(a)); N$nap_min <- ifelse(is.na(k), 0, as.numeric(a)[k]) }
    N$nap_len <- ifelse(N$nap_min > 0, N$nap_min, NA_real_)
    N$wk_min <- 0; N$wk_trimp <- 0; N$end_h <- NA_real_; N$late <- NA; N$hard <- NA
    if (nrow(w)) {
      st <- tzt(w$start); fin <- tzt(w$start + w$dur * 60)
      sh <- as.numeric(format(st, "%H")) + as.numeric(format(st, "%M")) / 60; eh <- as.numeric(format(fin, "%H")) + as.numeric(format(fin, "%M")) / 60
      w$end_h <- ifelse(eh < sh, eh + 24, eh)
      a <- w %>% dplyr::group_by(day) %>% dplyr::summarise(m = sum(dur, na.rm = TRUE), t = sum(trimp, na.rm = TRUE), e = suppressWarnings(max(end_h, na.rm = TRUE)), .groups = "drop")
      a$e[!is.finite(a$e)] <- NA_real_
      i <- match(N$kd, a$day); N$wk_min <- ifelse(is.na(i), 0, a$m[i]); N$wk_trimp <- ifelse(is.na(i), 0, a$t[i])
      N$end_h <- ifelse(is.na(i) | N$wk_min <= 0, NA_real_, a$e[i])
      N$late <- ifelse(N$wk_min > 0 & !is.na(N$end_h), N$end_h >= 19.5, NA)
      pos <- a$t[a$t > 0]; if (length(pos) >= 8) N$hard <- N$wk_trimp >= quantile(pos, .75, names = FALSE)
    }
    N[order(N$kd), ]
  })
  FIND <- reactive({
    N <- NT(); shiny::validate(shiny::need(nrow(N) >= 15, sprintf("Pattern search needs at least 15 detected nights (found %d). Widen the date range in the sidebar.", nrow(N))))
    R <- ins_build(N); shiny::validate(shiny::need(!is.null(R), "Not enough overlapping data to test relationships yet.")); R
  })
  WIN <- reactive({ D <- DD(); shiny::validate(shiny::need(sum(!is.na(D$steps) | !is.na(D$asleep_h)) >= 6, "Needs a few days of data.")); ins_win(D, as.integer(input$ins_win)) })
  FIT <- reactive({
    L <- tryCatch(LOAD(), error = function(e) NULL); V <- tryCatch(VO2R(), error = function(e) NULL)
    ins_fit(DD(), L, V)
  })
  
  # ---- domain cards ---------------------------------------------------------------
  output$ins_cards <- renderUI({
    th <- TH(); W <- WIN(); doms <- ins_dom(W$tab)
    cols <- c(better = "#34d399", worse = "#ef4444", mixed = "#fbbf24", steady = "#9ca3af", `no data` = "#6b7280")
    txt <- c(better = "Improving", worse = "Slipping", mixed = "Mixed", steady = "Steady", `no data` = "No data")
    f <- FIT()
    cards <- lapply(doms, function(d) {
      v <- d$verdict; z <- d$rows[!is.na(d$rows$es), ]; z <- z[order(-abs(z$es)), ]; z <- head(z, 3)
      if (d$dom == "Fitness" && isTRUE(f$ok)) { v <- c(improving = "better", declining = "worse", stable = "steady")[[f$verdict]]; zz <- NULL } else zz <- z
      ln <- if (d$dom == "Fitness" && isTRUE(f$ok)) sprintf("%s (%s)", f$verdict, f$conf) else if (!is.null(zz) && nrow(zz))
        paste(sprintf("%s: %s <span style='opacity:.65'>(%s)</span>", sub(" \\(.*$", "", zz$lab), fmt(zz$cur, zz$dig), ins_pct(zz$cur, zz$prv)), collapse = "<br>") else "not enough data yet"
      div(class = "tile", style = "cursor:default",
          div(class = "tile-h", style = paste0("color:", cols[[v]]), d$dom),
          div(tags$span(class = "tile-v", style = "font-size:1.7rem", txt[[v]])),
          div(class = "tile-s", style = "min-height:4.2em", HTML(ln)))
    })
    div(class = "tiles", cards)
  })
  
  # ---- what changed ---------------------------------------------------------------------
  output$ins_story <- renderUI({
    W <- WIN(); t <- W$tab; n <- W$n; g <- function(id) t[t$id == id, ]
    a_es <- mn(c(g("steps")$es, g("mem")$es, g("sport_min")$es))
    stp <- ins_sportchg(tryCatch(Wm(), error = function(e) NULL), W$last, n); it <- character()
    dir <- if (is.na(a_es)) NA_character_ else if (a_es > .2) "more active" else if (a_es < -.2) "less active" else "about as active"
    if (!is.na(dir)) {
      s1 <- sprintf("The last %d days were <b>%s</b> than the %d before (steps %s, active minutes %s, workout minutes %s).", n, dir, n, ins_pct(g("steps")$cur, g("steps")$prv), ins_pct(g("mem")$cur, g("mem")$prv), ins_pct(g("sport_min")$cur, g("sport_min")$prv))
      if (length(stp$stopped) && dir == "more active") s1 <- paste0(s1, sprintf(" That is despite dropping %s - the extra activity comes from daily movement, not training.", paste(stp$stopped, collapse = ", ")))
      if (length(stp$stopped) && dir != "more active") s1 <- paste0(s1, sprintf(" A big part of it: you stopped %s.", paste(stp$stopped, collapse = ", ")))
      it <- c(it, s1)
    }
    if (!is.null(stp) && length(stp$txt)) it <- c(it, stp$txt)
    big <- t[!is.na(t$es) & abs(t$es) >= .2, ]; big <- head(big[order(-abs(big$es)), ], 6)
    for (i in seq_len(nrow(big))) {
      sg <- ins_sig(big[i, ])
      it <- c(it, sprintf("<b>%s</b>: %s vs %s (%+.1f, %+.1f SD) - %s", big$lab[i], fmt(big$cur[i], big$dig[i]), fmt(big$prv[i], big$dig[i]), big$cur[i] - big$prv[i], big$es[i], sg))
    }
    if (!length(it)) it <- "Nothing moved by more than normal day-to-day noise."
    f <- FIT(); it <- c(it, if (isTRUE(f$ok)) sprintf("<b>Fitness</b>: %s (%s).", f$verdict, f$conf) else f$msg)
    tags$ul(lapply(it, function(x) tags$li(HTML(x))))
  })
  
  # ---- plan -------------------------------------------------------------------------------
  output$ins_plan <- renderUI({
    D <- DD(); W <- tryCatch(Wm(), error = function(e) NULL); L <- tryCatch(LOAD(), error = function(e) NULL); S <- tryCatch(SC(), error = function(e) NULL)
    R <- tryCatch(FIND(), error = function(e) NULL); N <- tryCatch(NT(), error = function(e) NULL); fit <- FIT()
    last <- WIN()$last; stp <- ins_sportchg(W, last, as.integer(input$ins_win))
    pl <- ins_plan(D, W, L, S, fit, R, stp, N, input$need, input$goal, input$act_goal)
    tagList(lapply(pl, function(s) tagList(tags$h6(class = "mt-2", s$h), tags$ul(lapply(s$items, function(x) tags$li(HTML(x)))))),
            tags$p(class = "text-muted small", "Rule-based guidance from your own numbers (load ratio, readiness, warning signs, fitness trend). It is not medical advice."))
  })
  
  # ---- usual-range bands and month so far ----------------------------------------------------------
  output$ins_band <- renderUI({
    D <- DD(); W <- WIN(); n <- as.integer(input$ins_win)
    b <- ins_band_html(D, W$last - n + 1, W$last)
    if (!length(b)) return(tags$p(class = "text-muted", "Not enough data yet."))
    HTML(paste0("<div class='ins-grid'>", paste(b, collapse = ""), "</div>"))
  })
  output$ins_cum <- renderUI({
    D <- DD(); W <- WIN(); m0 <- as.Date(format(W$last, "%Y-%m-01"))
    b <- ins_cum_html(D, m0, W$last, "month", W = tryCatch(Wm(), error = function(e) NULL))
    if (!length(b)) return(tags$p(class = "text-muted", "No workouts or steps this month yet."))
    HTML(paste0("<div class='ins-grid'>", paste(b, collapse = ""), "</div>"))
  })
  
  # ---- patterns ---------------------------------------------------------------------------------
  output$ins_top <- renderUI({
    R <- FIND(); allow <- switch(input$ins_min, strong = "strong", moderate = c("strong", "moderate"), c("strong", "moderate", "hint"))
    R <- R[R$strength %in% allow, ]
    if (!nrow(R)) return(tags$p("No relationship reaches this evidence level yet. Widen the date range, include early hints, or keep collecting - patterns need roughly 30+ nights."))
    R$rank <- match(R$strength, c("strong", "moderate", "hint")); R <- R[order(R$rank, -abs(R$eff)), ]
    R <- do.call(rbind, lapply(split(R, R$key), function(z) head(z, 3))); R <- R[order(R$rank, -abs(R$eff)), ]; R <- head(R, 14)
    bc <- c(good = "#34d399", bad = "#ef4444", neutral = "#60a5fa"); bg <- c(strong = "bg-success", moderate = "bg-warning text-dark", hint = "bg-secondary")
    tagList(lapply(seq_len(nrow(R)), function(i) { r <- R[i, ]
    div(class = "p-2 mb-2", style = sprintf("border-left:4px solid %s;border-radius:6px;background:rgba(127,127,127,.07)", bc[[r$tone]]),
        tags$span(class = paste("badge me-2", bg[[r$strength]]), r$strength), HTML(r$text),
        if (nzchar(r$advice)) div(class = "mt-1", tags$em(r$advice)),
        div(class = "text-muted small", sprintf("n = %d, p = %.3g, adjusted p = %.3g", r$n, r$p, r$padj))) }))
  })
  
  # ---- fitness ------------------------------------------------------------------------------------
  output$ins_fit_txt <- renderUI({
    f <- FIT(); if (!isTRUE(f$ok)) return(tags$p(f$msg))
    col <- c(improving = "#34d399", declining = "#ef4444", stable = "#9ca3af")[[f$verdict]]
    tagList(tags$h5(style = paste0("color:", col), sprintf("Fitness looks %s", f$verdict), tags$small(class = "text-muted ms-2", sprintf("(%s; last %d days vs the %d before)", f$conf, f$k, f$k))),
            tags$ul(lapply(f$lines, tags$li)))
  })
  output$ins_p_fit <- renderPlotly({
    th <- TH(); D <- DD(); L <- tryCatch(LOAD(), error = function(e) NULL)
    ind <- list(`Sleeping HR (inverted)` = -D$hr_night, HRV = D$hrv, `VO2 proxy` = D$vo2, `Night HR dip` = D$dip)
    if (!is.null(L)) ind[["Training fitness load"]] <- L$ctl[match(D$day, L$day)]
    p <- plot_ly(x = as.character(D$day)); i <- 0; any_ok <- FALSE
    for (nm in names(ind)) { x <- ind[[nm]]; if (sum(!is.na(x)) < 7 || is.na(sd(x, na.rm = TRUE)) || sd(x, na.rm = TRUE) == 0) next
    i <- i + 1; any_ok <- TRUE; z <- (x - mn(x)) / sd(x, na.rm = TRUE)
    p <- p %>% add_lines(y = roll_fun(z, 14, mean, 5), name = nm, line = list(color = th$pal[(i - 1) %% 6 + 1], width = 2.4)) }
    shiny::validate(shiny::need(any_ok, "Not enough data for fitness indicators yet."))
    style_plot(p, th, NULL, "z-score (higher = fitter)")
  })
  
  # ---- period change plot --------------------------------------------------------------------------
  output$ins_p_change <- renderPlotly({
    th <- TH(); t <- WIN()$tab; t <- t[!is.na(t$es), ]; shiny::validate(shiny::need(nrow(t) > 0, "Not enough data in both periods."))
    t$sig <- ins_sig(t); t <- t[order(t$es), ]
    col <- c(better = "#34d399", worse = "#ef4444", steady = "rgba(150,150,150,.55)")[t$sig]
    tx <- sprintf("%s<br>now %s, before %s (%s)", t$lab, fmt(t$cur, t$dig), fmt(t$prv, t$dig), ins_pct(t$cur, t$prv))
    vl <- function(x) list(type = "line", xref = "x", yref = "paper", x0 = x, x1 = x, y0 = 0, y1 = 1, line = list(color = th$fg, dash = "dot", width = 1))
    plot_ly(x = t$es, y = factor(t$lab, levels = t$lab), type = "bar", orientation = "h", marker = list(color = unname(col)), text = tx, hoverinfo = "text") %>%
      style_plot(th, "change in SD of your daily values", NULL) %>%
      plotly::layout(shapes = list(vl(-.2), vl(.2)), hovermode = "closest", margin = list(l = 190, r = 15, t = 10, b = 40))
  })
  
  # ---- explore plots -----------------------------------------------------------------------------------
  box_axis <- function(th, oid) {
    ax <- list(title = INS_OUT[[oid]]$lab, gridcolor = th$grid, zeroline = FALSE)
    if (oid == "onset_min") { tv <- seq(0, 720, by = 60); ax$tickmode <- "array"; ax$tickvals <- tv; ax$ticktext <- ins_clock(tv) }
    ax
  }
  output$ins_p_grp <- renderPlotly({
    th <- TH(); N <- NT(); sp <- INS_GRP[[input$ins_cond]]; gv <- sp$f(N); oid <- input$ins_out; y <- N[[oid]]
    ok <- !is.na(gv) & !is.na(y); shiny::validate(shiny::need(sum(ok) >= 8 && sum(gv[ok]) >= 3 && sum(!gv[ok]) >= 3, "Not enough nights in both groups for this comparison."))
    y <- y[ok]; gv <- as.logical(gv[ok]); t <- ins_grp_test(y, gv, 3, 3)
    p <- plot_ly()
    for (k in c(FALSE, TRUE)) { cl <- if (k) th$pal[2] else th$pal[1]
    p <- p %>% add_trace(y = y[gv == k], type = "box", name = sprintf("%s (n=%d)", if (k) sp$lab else sp$vs, sum(gv == k)), boxpoints = "all", jitter = .4, pointpos = 0,
                         marker = list(color = cl, size = 7, opacity = .7), line = list(color = cl), fillcolor = "rgba(127,127,127,.10)") }
    ann <- if (!is.null(t)) list(list(x = .01, y = .99, xref = "paper", yref = "paper", xanchor = "left", yanchor = "top", showarrow = FALSE, align = "left",
                                      text = sprintf("median %s vs %s<br>p = %.3g (Wilcoxon)", ins_val(oid, t$m1), ins_val(oid, t$m0), t$p), bgcolor = "rgba(0,0,0,.35)", font = list(size = 12, color = "#ffffff"))) else list()
    style_plot(p, th, NULL, NULL) %>% plotly::layout(yaxis = box_axis(th, oid), annotations = ann, hovermode = "closest", showlegend = FALSE)
  })
  output$ins_p_dose <- renderPlotly({
    th <- TH(); N <- NT(); oid <- input$ins_out; y <- N[[oid]]
    b <- cut(N$wk_min, c(-Inf, 0, 29, 59, 89, Inf), labels = c("rest", "1-29 min", "30-59 min", "60-89 min", "90+ min"))
    ok <- !is.na(y) & !is.na(b); shiny::validate(shiny::need(sum(ok) >= 10, "Not enough nights."))
    cols <- c("#9ca3af", th$pal[1], th$pal[3], th$pal[2], th$pal[4]); p <- plot_ly(); k <- 0
    for (lv in levels(b)) { k <- k + 1; z <- ok & b == lv; if (sum(z) < 2) next
    p <- p %>% add_trace(y = y[z], type = "box", name = sprintf("%s (n=%d)", lv, sum(z)), boxpoints = "all", jitter = .4, pointpos = 0,
                         marker = list(color = cols[k], size = 6, opacity = .7), line = list(color = cols[k]), fillcolor = "rgba(127,127,127,.10)") }
    ct <- tryCatch(suppressWarnings(cor.test(N$wk_min[!is.na(y)], y[!is.na(y)], method = "spearman", exact = FALSE)), error = function(e) NULL)
    ann <- if (!is.null(ct) && !is.na(ct$p.value)) list(list(x = .01, y = .99, xref = "paper", yref = "paper", xanchor = "left", yanchor = "top", showarrow = FALSE, align = "left",
                                                             text = sprintf("rank correlation with workout minutes: %.2f<br>p = %.3g", unname(ct$estimate), ct$p.value), bgcolor = "rgba(0,0,0,.35)", font = list(size = 12, color = "#ffffff"))) else list()
    style_plot(p, th, NULL, NULL) %>% plotly::layout(yaxis = box_axis(th, oid), annotations = ann, hovermode = "closest", showlegend = FALSE)
  })
  
  # ---- activity mix --------------------------------------------------------------------------------------
  output$ins_p_mix <- renderPlotly({
    th <- TH(); W <- Wm(); D <- DD(); shiny::validate(shiny::need(nrow(W) > 0, "No workouts in the selected range."))
    wk <- function(x) as.character(as.Date(cut(x, "week")))
    allwk <- tail(as.character(seq(as.Date(cut(min(D$day), "week")), as.Date(cut(max(D$day), "week")), by = "week")), 16)
    W$wk <- wk(W$day); a <- W %>% dplyr::group_by(wk, sport) %>% dplyr::summarise(v = sum(dur, na.rm = TRUE), .groups = "drop"); a <- a[a$wk %in% allwk, ]
    sc <- tapply(D$steps, wk(D$day), function(z) mn(z))
    p <- plot_ly(); i <- 0
    for (s in sort(unique(a$sport))) { i <- i + 1; z <- a[a$sport == s, ]; p <- p %>% add_bars(x = z$wk, y = z$v, name = s, marker = list(color = c(th$pal, "#8899aa")[i])) }
    p <- p %>% add_lines(x = allwk, y = as.numeric(sc[allwk]), yaxis = "y2", name = "steps / day", line = list(color = th$fg, width = 2.2, dash = "dot"))
    p %>% plotly::layout(barmode = "stack", yaxis2 = list(overlaying = "y", side = "right", title = "steps / day", showgrid = FALSE, color = th$fg), xaxis = list(type = "category")) %>%
      style_plot(th, NULL, "workout minutes per week") %>% plotly::layout(xaxis = list(type = "category"))
  })
  output$ins_sports <- renderUI({
    W <- WIN(); stp <- ins_sportchg(tryCatch(Wm(), error = function(e) NULL), W$last, W$n)
    if (is.null(stp) || !length(stp$txt)) return(tags$p(class = "text-muted", "No sport was started, stopped or changed by more than 40% compared with your usual rhythm."))
    tags$ul(lapply(stp$txt, function(x) tags$li(HTML(x))))
  })
  
  # ---- evidence table ------------------------------------------------------------------------------------------
  output$ins_tbl <- renderTable({
    R <- FIND(); R <- R[order(match(R$strength, c("strong", "moderate", "hint", "none")), R$p), ]
    eff <- ifelse(R$kind == "cont", sprintf("rho %+.2f", R$eff), sprintf("%+.1f %s", R$est * vapply(R$out, function(o) INS_OUT[[o]]$mult, 1), vapply(R$out, function(o) INS_OUT[[o]]$unit, "")))
    data.frame(`When / what` = vapply(R$key, ins_key_lab, ""), Outcome = vapply(R$out, function(o) INS_OUT[[o]]$lab, ""), Effect = eff, n = R$n,
               p = formatC(R$p, format = "g", digits = 2), `Adjusted p` = formatC(R$padj, format = "g", digits = 2), Evidence = R$strength, check.names = FALSE, row.names = NULL)
  }, striped = TRUE, spacing = "xs", width = "100%")
  
  # =========================== weekly and monthly reports ===========================
  # the calendar weeks (Mon-Sun) or months that contain data, newest first
  rep_periods <- function(D, type) {
    has <- D$day[!is.na(D$steps) | !is.na(D$asleep_h)]; if (!length(has)) return(NULL)
    last <- max(has); first <- min(has)
    if (identical(type, "Monthly")) {
      m <- rev(seq(as.Date(format(first, "%Y-%m-01")), as.Date(format(last, "%Y-%m-01")), by = "month"))
      setNames(format(m, "%Y-%m"), paste0(format(m, "%B %Y"), ifelse(m == as.Date(format(last, "%Y-%m-01")), " (so far)", "")))
    } else {
      w <- rev(seq(as.Date(cut(first, "week")), last, by = "week"))
      setNames(as.character(w), paste0(format(w, "%d %b"), " - ", format(w + 6, "%d %b %Y"), ifelse(w + 6 > last, " (so far)", "")))
    }
  }
  observe({ ch <- rep_periods(DD(), "Weekly");  req(length(ch) > 0); updateSelectInput(session, "ins_wk_period", choices = ch, selected = ch[[1]]) })
  observe({ ch <- rep_periods(DD(), "Monthly"); req(length(ch) > 0); updateSelectInput(session, "ins_mo_period", choices = ch, selected = ch[[1]]) })
  
  # one report for one calendar week / month; strict = TRUE returns NULL when the period has fewer than 3 days of data
  rep_build <- function(type, per, strict = FALSE) {
    D <- DD(); monthly <- identical(type, "Monthly"); W <- tryCatch(Wm(), error = function(e) NULL)
    if (monthly) { c0 <- as.Date(paste0(per, "-01")); req(!is.na(c0)); c1 <- seq(c0, by = "month", length.out = 2)[2] - 1
    p1 <- c0 - 1; p0 <- seq(c0, by = "-1 month", length.out = 2)[2]; ttl <- paste("Monthly report:", format(c0, "%B %Y")); prvl <- "previous month"
    } else { c0 <- as.Date(per); req(!is.na(c0)); c1 <- c0 + 6; p0 <- c0 - 7; p1 <- c0 - 1
    ttl <- sprintf("Weekly report: %s - %s", format(c0, "%d %b"), format(c1, "%d %b %Y")); prvl <- "previous week" }
    if (strict) { nd <- sum((!is.na(D$steps) | !is.na(D$asleep_h)) & D$day >= c0 & D$day <= c1); if (nd < 3) return(NULL) }
    Wn <- ins_win_range(D, c0, c1, p0, p1); t <- Wn$tab; shiny::validate(shiny::need(sum(!is.na(t$cur)) >= 2, "No data in this period."))
    t$sig <- ins_sig(t); nb <- sum(t$sig == "better"); nw <- sum(t$sig == "worse"); nt <- sum(!is.na(t$es))
    h <- paste0("<h1>", ttl, "</h1><div class='sub'>compared with the ", prvl, " (", format(p0, "%d %b"), " - ", format(p1, "%d %b"), ")</div>")
    h <- paste0(h, sprintf("<h2>Summary</h2><p>Of %d comparable metrics, <b>%d improved</b>, <b>%d got worse</b> and %d stayed within normal day-to-day noise (changes smaller than 0.2 SD of your daily values).</p>", nt, nb, nw, nt - nb - nw))
    big <- t[!is.na(t$es) & abs(t$es) >= .2, ]; big <- head(big[order(-abs(big$es)), ], 6)
    if (nrow(big)) h <- paste0(h, paste0("<div class='", ifelse(big$sig == "worse", "bad", "good"), "'><b>", big$lab, "</b>: ", fmt(big$cur, big$dig), " vs ", fmt(big$prv, big$dig), " (", ins_pct(big$cur, big$prv), ")</div>", collapse = ""))
    h <- paste0(h, ins_rep_visuals(D, W, c0, c1, monthly))
    h <- paste0(h, "<h2>All metrics</h2>", ins_html_table(data.frame(Metric = t$lab, This = fmt(t$cur, t$dig), Previous = fmt(t$prv, t$dig), Change = ins_pct(t$cur, t$prv), Signal = ins_sigspan(t$sig), check.names = FALSE)))
    # sleep
    N <- tryCatch(NT(), error = function(e) NULL)
    if (!is.null(N)) { z <- N[N$wake >= c0 & N$wake <= c1, ]
    if (nrow(z) && any(!is.na(z$eff))) { bi <- which.max(z$eff); wi <- which.min(z$eff)
    h <- paste0(h, "<h2>Sleep</h2><p>", sprintf("%d nights recorded, average %s asleep, efficiency %s%%, typical sleep onset %s (SD %s min), %d nap day(s). Best night: %s (%s%% efficiency); weakest: %s (%s%%).",
                                                nrow(z), paste0(fmt(mn(z$asleep_h), 1), " h"), fmt(mn(z$eff)), ins_clock(md(z$onset_min)), fmt(if (nrow(z) > 1) sd(z$onset_min) else NA_real_), sum(z$nap_min > 0),
                                                format(z$wake[bi], "%a %d %b"), fmt(z$eff[bi]), format(z$wake[wi], "%a %d %b"), fmt(z$eff[wi])), "</p>") } }
    # workouts
    if (!is.null(W) && nrow(W)) {
      sm <- function(x) if (!nrow(x)) data.frame(sport = character(), s = numeric(), m = numeric(), t = numeric(), hr = numeric()) else
        x %>% dplyr::group_by(sport) %>% dplyr::summarise(s = dplyr::n(), m = sum(dur, na.rm = TRUE), t = sum(trimp, na.rm = TRUE), hr = mn(avg_hr), .groups = "drop")
      a <- sm(W[W$day >= c0 & W$day <= c1, ]); b <- sm(W[W$day >= p0 & W$day <= p1, ])
      sp <- union(a$sport, b$sport)
      if (length(sp)) { g <- function(d, s, col) { v <- d[[col]][d$sport == s]; if (length(v)) v else 0 }
      h <- paste0(h, "<h2>Workouts</h2>", ins_html_table(data.frame(Sport = sp, Sessions = vapply(sp, function(s) as.character(g(a, s, "s")), ""), `Sessions before` = vapply(sp, function(s) as.character(g(b, s, "s")), ""),
                                                                    Minutes = vapply(sp, function(s) fmt(g(a, s, "m")), ""), `Minutes before` = vapply(sp, function(s) fmt(g(b, s, "m")), ""), Load = vapply(sp, function(s) fmt(g(a, s, "t")), ""),
                                                                    `Avg HR` = vapply(sp, function(s) fmt(g(a, s, "hr")), ""), check.names = FALSE, row.names = NULL))) }
      stp <- ins_sportchg(W, c1, as.numeric(c1 - c0) + 1)
      if (!is.null(stp) && length(stp$txt)) h <- paste0(h, "<ul>", paste0("<li>", stp$txt, "</li>", collapse = ""), "</ul>")
    }
    # fitness + patterns
    f <- tryCatch(FIT(), error = function(e) NULL)
    if (!is.null(f) && isTRUE(f$ok)) h <- paste0(h, sprintf("<h2>Fitness</h2><p>Fitness looks <b>%s</b> (%s).</p><ul>%s</ul>", f$verdict, f$conf, paste0("<li>", f$lines, "</li>", collapse = "")))
    R <- tryCatch(FIND(), error = function(e) NULL)
    if (!is.null(R)) { r <- R[R$strength %in% c("strong", "moderate"), ]; r <- head(r[order(match(r$strength, c("strong", "moderate")), -abs(r$eff)), ], 4)
    if (nrow(r)) h <- paste0(h, "<h2>Patterns in your data</h2>", paste0("<div class='", ifelse(r$tone == "bad", "bad", "good"), "'>", r$text, if_else_html(r$advice), "</div>", collapse = "")) }
    # plan, only for the most recent period
    has <- D$day[!is.na(D$steps) | !is.na(D$asleep_h)]
    if (c1 >= max(has) - 1) {
      L <- tryCatch(LOAD(), error = function(e) NULL); S <- tryCatch(SC(), error = function(e) NULL)
      pl <- ins_plan(D, W, L, S, f, R, ins_sportchg(W, max(has), 14), N, input$need, input$goal, input$act_goal)
      h <- paste0(h, "<h2>Plan for next week</h2>", paste0("<b>", vapply(pl, function(s) s$h, ""), "</b><ul>", vapply(pl, function(s) paste0("<li>", s$items, "</li>", collapse = ""), ""), "</ul>", collapse = ""))
    }
    h <- paste0(h, "<p class='small'>Generated from your Gadgetbridge data. Patterns are observational; plans are rule-based and not medical advice.</p>")
    list(title = ttl, html = h)
  }
  
  # every week / month with data in one file, newest first, one report per printed page
  rep_all <- function(type) {
    ps <- unname(rep_periods(DD(), type)); rs <- list()
    shiny::withProgress(message = "Building reports", value = 0, {
      for (k in seq_along(ps)) {
        shiny::incProgress(1 / length(ps), detail = sprintf("%d of %d", k, length(ps)))
        r <- tryCatch(rep_build(type, ps[k], strict = TRUE), error = function(e) NULL)
        if (!is.null(r)) rs[[length(rs) + 1]] <- r
      }
    })
    ttl <- if (identical(type, "Monthly")) "All monthly reports" else "All weekly reports"
    if (!length(rs)) return(list(title = ttl, html = "<h1>No report</h1><p>No period with enough data.</p>"))
    toc <- paste0("<ul>", paste0("<li><a href='#r", seq_along(rs), "'>", vapply(rs, function(r) r$title, ""), "</a></li>", collapse = ""), "</ul>")
    sec <- paste0("<section id='r", seq_along(rs), "' style='page-break-before:always'>", vapply(rs, function(r) r$html, ""), "</section>", collapse = "")
    list(title = ttl, html = paste0("<h1>", ttl, "</h1><div class='sub'>", length(rs), " reports, newest first</div>", toc, sec))
  }
  rep_file <- function(title) paste0(gsub("[^A-Za-z0-9]+", "_", title), ".html")
  
  REPW <- reactive({ req(input$ins_wk_period); rep_build("Weekly", input$ins_wk_period) })
  REPM <- reactive({ req(input$ins_mo_period); rep_build("Monthly", input$ins_mo_period) })
  output$ins_wk_report <- renderUI(HTML(REPW()$html))
  output$ins_mo_report <- renderUI(HTML(REPM()$html))
  output$ins_wk_dl <- downloadHandler(filename = function() rep_file(REPW()$title),
                                      content = function(file) writeLines(ins_doc(REPW()$title, REPW()$html), file, useBytes = TRUE))
  output$ins_mo_dl <- downloadHandler(filename = function() rep_file(REPM()$title),
                                      content = function(file) writeLines(ins_doc(REPM()$title, REPM()$html), file, useBytes = TRUE))
  output$ins_wk_dl_all <- downloadHandler(filename = function() "all_weekly_reports.html",
                                          content = function(file) { r <- rep_all("Weekly"); writeLines(ins_doc(r$title, r$html), file, useBytes = TRUE) })
  output$ins_mo_dl_all <- downloadHandler(filename = function() "all_monthly_reports.html",
                                          content = function(file) { r <- rep_all("Monthly"); writeLines(ins_doc(r$title, r$html), file, useBytes = TRUE) })
}