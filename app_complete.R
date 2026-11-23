# =============================================================================
#  Gadgetbridge Lab  -  visualise, interpret and measure your smartwatch data (Huawei, Amazfit/Zepp)
#  Inputs : Gadgetbridge.db (SQLite)  OR  gadgetbridge_*.zip export
#           + optional workout folder / files (.fit)
#  Run    : shiny::runApp("gadgetbridge-lab")      (fully offline)
# =============================================================================
pkgs_needed <- c("shiny", "bslib", "DBI", "RSQLite", "dplyr", "plotly")
miss <- pkgs_needed[!vapply(pkgs_needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss)) stop("Please install first: install.packages(c(",
                       paste0('"', miss, '"', collapse = ", "), "))", call. = FALSE)
suppressPackageStartupMessages({ library(shiny); library(bslib); library(dplyr); library(plotly) })
options(shiny.maxRequestSize = 1024 * 1024^2)

# ---------------------------------------------------------------- helpers ----
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a
mn <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
md <- function(x) if (all(is.na(x))) NA_real_ else median(x, na.rm = TRUE)
mx <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
qq <- function(x, p) if (all(is.na(x))) NA_real_ else quantile(x, p, na.rm = TRUE, names = FALSE)
fmt <- function(x, d = 0) ifelse(is.na(x), "-", formatC(x, format = "f", digits = d, big.mark = ","))
# wall-clock strings for plotly (avoids time-zone shifts on date axes)
ws <- function(x, tz) format(as.POSIXct(as.numeric(x), origin = "1970-01-01", tz = "UTC"),
                             "%Y-%m-%d %H:%M:%S", tz = tz)
hline <- function(y, col) list(type = "line", xref = "paper", x0 = 0, x1 = 1, yref = "y",
                               y0 = y, y1 = y, line = list(color = col, dash = "dot", width = 1))

# Huawei stores heart rate in a SIGNED byte: 128..254 bpm arrive as -128..-2, -1 = missing
fix_hr <- function(x) { x <- ifelse(x < -1, x + 256, x); ifelse(x >= 25 & x <= 230, x, NA_real_) }

SPORT <- c(`16` = "Running", `32` = "Walking", `64` = "Swimming", `128` = "Cycling",
           `256` = "Treadmill", `512` = "Exercise (generic)", `1024` = "Open-water swim",
           `2048` = "Indoor cycling", `1048576` = "Badminton")
sport_label <- function(k) ifelse(is.na(k), "Workout", ifelse(as.character(k) %in% names(SPORT),
                                                              SPORT[as.character(k)], paste("Kind", k)))

ZLAB <- c("<Z1", "Z1", "Z2", "Z3", "Z4", "Z5")
ZCOL <- c("#6b7280", "#60a5fa", "#34d399", "#fbbf24", "#f97316", "#ef4444")
ZRGBA <- c("rgba(96,165,250,.13)", "rgba(52,211,153,.13)", "rgba(251,191,36,.13)",
           "rgba(249,115,22,.13)", "rgba(239,68,68,.13)")
STAGE_COL <- c(Awake = "#f0a35e", REM = "#c792ea", Light = "#6cb6ff", Deep = "#2f5fd0", Unstaged = "#8b95a5")

THEMES <- list(
  Dark  = list(bg = "#0d1117", fg = "#c9d1d9", primary = "#7aa2f7", grid = "rgba(255,255,255,.055)",
               pal = c("#7aa2f7", "#f0a35e", "#6ee7b7", "#f87171", "#c4a7e7", "#f6d28b")),
  Neon  = list(bg = "#07070f", fg = "#e8f0ff", primary = "#00f5d4", grid = "rgba(0,245,212,.09)",
               pal = c("#00f5d4", "#ff2a6d", "#fee440", "#9b5de5", "#00bbf9", "#f15bb5")),
  Light = list(bg = "#f6f7f9", fg = "#1f2933", primary = "#3b6fd4", grid = "rgba(0,0,0,.06)",
               pal = c("#3b6fd4", "#e8833a", "#2fa37a", "#d9534f", "#8a63c7", "#c9a227")))
APP_CSS <- "
  .card { background: color-mix(in srgb, var(--bs-body-color) 5%, var(--bs-body-bg));
          border: 1px solid color-mix(in srgb, var(--bs-body-color) 9%, transparent);
          border-radius: 16px; box-shadow: 0 2px 18px rgba(0,0,0,.14); }
  .card-header { background: transparent; border: 0; font-weight: 600; font-size: 1rem;
                 letter-spacing: .02em; padding-bottom: 0; }
  .nav-tabs { border-bottom: 1px solid color-mix(in srgb, var(--bs-body-color) 10%, transparent); }
  .nav-tabs .nav-link { border: 0; border-bottom: 2px solid transparent; background: transparent; opacity: .7; }
  .nav-tabs .nav-link.active { border-bottom-color: var(--bs-primary); opacity: 1; background: transparent; }
  .form-select, .form-control { border-radius: 10px; }
  .bslib-sidebar-layout > .sidebar { border-right: 0; }
  .tiles { display: grid; grid-template-columns: repeat(auto-fit, minmax(250px, 1fr)); gap: 14px; margin-bottom: 14px; }
  .tile { cursor: pointer; padding: 16px 18px; border-radius: 20px; transition: transform .15s, box-shadow .15s;
          background: color-mix(in srgb, var(--bs-body-color) 5%, var(--bs-body-bg));
          border: 1px solid color-mix(in srgb, var(--bs-body-color) 9%, transparent); }
  .tile:hover { transform: translateY(-3px); box-shadow: 0 8px 26px rgba(0,0,0,.22); }
  .tile-h { font-size: .78rem; font-weight: 700; letter-spacing: .08em; text-transform: uppercase; }
  .tile-v { font-size: 2.4rem; font-weight: 700; line-height: 1.15; }
  .tile-u { font-size: .9rem; opacity: .6; margin-left: 4px; }
  .tile-s { font-size: .82rem; opacity: .7; min-height: 1.3em; }
  .hero { display: flex; flex-wrap: wrap; gap: 36px; align-items: center; justify-content: center; padding: 10px 0; }
  .rleg > div { display: flex; align-items: center; gap: 12px; margin: 10px 0; }
  .rdot { width: 14px; height: 14px; border-radius: 50%; flex: none; }
  .rval { font-size: 1.5rem; font-weight: 700; line-height: 1.1; }
  .rlab { font-size: .8rem; opacity: .65; }
  .sbar { display: flex; height: 8px; border-radius: 6px; overflow: hidden; margin: 6px 0 2px; }

  main { padding: 1.1rem 1.6rem !important; }
  @media (min-width: 1700px) { html { font-size: 17px; } .tiles { grid-template-columns: repeat(6, 1fr); } }
  @media (min-width: 2300px) { html { font-size: 19px; } main { max-width: 2400px; margin: 0 auto; } }
  .rgrid { display: grid; grid-template-columns: repeat(auto-fill, minmax(84px, 1fr)); gap: 12px; flex: 1; min-width: 260px; }
  .rcell { text-align: center; font-size: .78rem; opacity: .95; }
  .arcs { display: grid; grid-template-columns: repeat(2, 1fr); gap: 14px; justify-items: center; padding: 6px 0; }
  @keyframes star-pulse { 0%, 100% { transform: scale(1); } 50% { transform: scale(1.2); } }
  @keyframes tw { 0%, 100% { opacity: .1; } 50% { opacity: 1; } }
  .rcell { cursor: pointer; border-radius: 12px; padding: 2px 0; transition: transform .15s; }
  .rcell:hover { transform: translateY(-2px); }
  .rcell.sel { outline: 2px solid var(--bs-primary); }
  .rcell.perfect svg { filter: drop-shadow(0 0 7px rgba(251,191,36,.8)); }
  .star { transform-origin: 100px 100px; animation: star-pulse 1.8s ease-in-out infinite; }
  .tw { animation: tw 1.6s ease-in-out infinite; } .tw2 { animation-delay: .5s; } .tw3 { animation-delay: 1s; }
  .tl-chip { display: inline-flex; align-items: center; gap: 6px; font-size: .8rem; opacity: .85; }
  .tl-chip i { width: 12px; height: 12px; border-radius: 3px; display: inline-block; }"
mk_theme <- function(nm) {
  t <- THEMES[[nm]]
  bs_theme(version = 5, bg = t$bg, fg = t$fg, primary = t$primary,
           base_font = font_collection("Inter", "system-ui", "-apple-system", "Segoe UI", "Roboto", "sans-serif"),
           "font-size-base" = "0.95rem") %>% bs_add_rules(APP_CSS)
}
style_plot <- function(p, th, xtitle = NULL, ytitle = NULL, ...) {
  ax <- function(t, x = FALSE) list(title = t, gridcolor = if (x) "rgba(0,0,0,0)" else th$grid,
                                    zeroline = FALSE, showline = FALSE, ticks = "",
                                    tickfont = list(color = th$fg, size = 11))
  p %>% plotly::layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
                       font = list(color = th$fg, size = 12), xaxis = ax(xtitle, TRUE), yaxis = ax(ytitle),
                       legend = list(orientation = "h", x = 0, y = 1.12, font = list(size = 11)),
                       margin = list(t = 10, r = 15, l = 50, b = 40), bargap = 0.28,
                       hoverlabel = list(bgcolor = th$bg, bordercolor = th$grid, font = list(color = th$fg)),
                       hovermode = "x unified", ...) %>% plotly::config(displaylogo = FALSE, displayModeBar = FALSE)
}
sniff <- function(path) {
  h <- readBin(path, "raw", 16)
  if (length(h) >= 2 && identical(h[1:2], as.raw(c(0x50, 0x4b)))) return("zip")
  if (length(h) >= 15 && identical(rawToChar(h[1:15]), "SQLite format 3")) return("db")
  "unknown"
}

# ------------------------------------------------------- FIT file parser ----
# Minimal, dependency-free reader (record + session messages, little/big endian)
read_fit <- function(path) {
  b <- readBin(path, "raw", file.size(path))
  if (length(b) < 14 || rawToChar(b[9:12]) != ".FIT") return(NULL)
  rd <- function(pos, sz, big, signed = FALSE) {
    x <- as.numeric(as.integer(b[pos:(pos + sz - 1L)])); if (big) x <- rev(x)
    v <- sum(x * 256^(seq_len(sz) - 1L)); top <- 2^(8 * sz)
    if (v == (if (signed) top / 2 - 1 else top - 1)) return(NA_real_)
    if (signed && v >= top / 2) v <- v - top
    v
  }
  hs <- as.integer(b[1]); end <- min(hs + rd(5L, 4L, FALSE), length(b))
  p <- hs + 1L; defs <- list(); recs <- list(); ses <- NULL
  w20 <- c(ts = 253, hr = 3, lat = 0, lon = 1, alt = 2, dist = 5, spd = 6, espd = 73, ealt = 78, cad = 4)
  w18 <- c(start = 2, elapsed = 7, timer = 8, dist = 9, kcal = 11, avg_hr = 16, max_hr = 17,
           sport = 5, sub = 6, te = 24)
  while (p <= end) {
    h <- as.integer(b[p]); p <- p + 1L
    comp <- bitwAnd(h, 0x80L) != 0L
    if (!comp && bitwAnd(h, 0x40L) != 0L) {                       # definition message
      lt <- bitwAnd(h, 0x0FL); big <- as.integer(b[p + 1L]) == 1L
      gm <- rd(p + 2L, 2L, big); nf <- as.integer(b[p + 4L]); p <- p + 5L
      fn <- integer(nf); fs <- integer(nf)
      for (i in seq_len(nf)) { fn[i] <- as.integer(b[p]); fs[i] <- as.integer(b[p + 1L]); p <- p + 3L }
      dev <- 0L
      if (bitwAnd(h, 0x20L) != 0L) {
        nd <- as.integer(b[p]); p <- p + 1L
        if (nd > 0L) { dev <- sum(as.integer(b[p + 3L * (seq_len(nd) - 1L) + 1L])); p <- p + 3L * nd }
      }
      defs[[as.character(lt)]] <- list(big = big, gm = gm, fn = fn, fs = fs, dev = dev)
      next
    }
    lt <- if (comp) bitwShiftR(bitwAnd(h, 0x60L), 5L) else bitwAnd(h, 0x0FL)
    d <- defs[[as.character(lt)]]; if (is.null(d)) break
    offs <- p + cumsum(c(0L, d$fs))[seq_along(d$fs)]
    getf <- function(wanted, signed = character()) {
      v <- setNames(rep(NA_real_, length(wanted)), names(wanted))
      for (k in names(wanted)) {
        i <- match(wanted[[k]], d$fn)
        if (!is.na(i) && d$fs[i] <= 4L) v[k] <- rd(offs[i], d$fs[i], d$big, k %in% signed)
      }
      v
    }
    if (!is.na(d$gm) && d$gm == 20) {
      recs[[length(recs) + 1L]] <- getf(w20, c("lat", "lon"))
    } else if (!is.na(d$gm) && d$gm == 18 && is.null(ses)) {
      nm <- NA_character_; i <- match(110, d$fn)
      if (!is.na(i)) { s <- b[offs[i]:(offs[i] + d$fs[i] - 1L)]; s <- s[s != as.raw(0)]
      nm <- tryCatch(rawToChar(s), error = function(e) NA_character_) }
      ses <- c(as.list(getf(w18)), list(name = nm))
    }
    p <- p + sum(d$fs) + d$dev
  }
  if (!length(recs)) return(NULL)
  r <- as.data.frame(do.call(rbind, recs)); r <- r[!is.na(r$ts), ]
  if (!nrow(r)) return(NULL)
  rec <- data.frame(
    t = r$ts + 631065600,
    hr = ifelse(!is.na(r$hr) & r$hr >= 25 & r$hr <= 230, r$hr, NA_real_),
    speed = ifelse(!is.na(r$espd), r$espd, r$spd) / 1000,
    alt = ifelse(!is.na(r$ealt), r$ealt, r$alt) / 5 - 500,
    dist = r$dist / 100, cad = ifelse(!is.na(r$cad) & r$cad > 0, r$cad, NA_real_))
  start <- if (!is.null(ses) && !is.na(ses$start)) ses$start + 631065600 else min(rec$t)
  sub <- ses$sub %||% NA; sp <- ses$sport %||% NA
  lbl <- if (!is.na(sub) && sub == 95) "Badminton" else
    c(`1` = "Running", `2` = "Cycling", `5` = "Swimming", `11` = "Walking", `64` = "Racket sport")[as.character(sp)] %||% "Workout"
  list(start = start, name = ses$name %||% NA_character_, sport = unname(lbl),
       dur = (ses$timer %||% ses$elapsed %||% (max(rec$t) - min(rec$t)) * 1000) / 1000,
       kcal = ses$kcal %||% NA_real_, dist = (ses$dist %||% NA_real_) / 100,
       te = (ses$te %||% NA_real_) / 10, rec = rec)
}

# --------------------------------------------------------- calculations -----
# minutes with 'steps' grouped into walking bouts vs isolated movement
classify_steps <- function(m, cad, len, base, margin) {
  m <- m[order(m$t), ]; n <- nrow(m)
  if (n < 2) { m$cls <- "none"; return(m) }
  ok <- m$steps >= cad
  g <- cumsum(c(TRUE, diff(m$t) != 60 | ok[-1] != ok[-n]))
  run <- ave(seq_len(n), g, FUN = length)
  bout <- ok & run >= len
  m$cls <- ifelse(m$steps == 0, "none", ifelse(bout, "bout",
                                               ifelse(!is.na(m$hr) & m$hr <= base + margin, "artefact?", "other")))
  m
}
in_sleep <- function(t, ses) {
  if (is.null(ses) || !nrow(ses)) return(rep(NA_integer_, length(t)))
  o <- order(ses$start); s <- ses$start[o]; e <- ses$end[o]
  i <- findInterval(t, s); j <- pmax(i, 1L)
  ifelse(i > 0 & t <= e[j], o[j], NA_integer_)
}
sleep_sessions <- function(df, light, deep, rem, awake, gap, night_min, tz) {
  df <- df[df$unst | df$code %in% c(light, deep, rem, awake), , drop = FALSE]
  if (!nrow(df)) return(list(seg = NULL, ses = NULL))
  # begin/end-only sleep (source 10) is dropped where staged segments already cover it
  stg <- df[!df$unst & df$code %in% c(light, deep, rem), ]
  if (any(df$unst) && nrow(stg)) {
    u <- which(df$unst)
    drop <- u[vapply(u, function(i) any(stg$start < df$end[i] & stg$end > df$start[i]), logical(1))]
    if (length(drop)) df <- df[-drop, , drop = FALSE]
  }
  df <- df[order(df$start), ]
  df$stage <- ifelse(df$unst, "Unstaged", ifelse(df$code %in% deep, "Deep", ifelse(df$code %in% rem, "REM",
                                                                                   ifelse(df$code %in% light, "Light", "Awake"))))
  df$dur <- (df$end - df$start) / 60
  gp <- c(Inf, (df$start[-1] - df$end[-nrow(df)]) / 60)
  df$sid <- cumsum(gp > gap)
  ses <- df %>% group_by(sid) %>%
    summarise(start = min(start), end = max(end), asleep = sum(dur[stage != "Awake"]),
              deep = sum(dur[stage == "Deep"]), light = sum(dur[stage == "Light"]),
              rem = sum(dur[stage == "REM"]), unst = sum(dur[stage == "Unstaged"]), .groups = "drop") %>% dplyr::filter(asleep >= 20)
  if (!nrow(ses)) return(list(seg = df, ses = NULL))
  ses$window <- (ses$end - ses$start) / 60
  ses$eff <- 100 * ses$asleep / ses$window
  ses$kind <- ifelse(ses$asleep >= night_min, "Night", "Nap")
  ses$wake_date <- as.Date(as.POSIXct(ses$end, origin = "1970-01-01", tz = tz), tz = tz)
  mid <- as.POSIXct((ses$start + ses$end) / 2, origin = "1970-01-01", tz = tz)
  ses$mid_min <- (as.numeric(format(mid, "%H")) * 60 + as.numeric(format(mid, "%M")) - 18 * 60) %% 1440
  list(seg = df, ses = as.data.frame(ses))
}
hr_metrics <- function(t, hr, hrmax, hrrest, male) {
  ok <- !is.na(hr); t <- as.numeric(t)[ok]; hr <- hr[ok]
  if (length(hr) < 3) return(NULL)
  dt <- pmin(c(diff(t), median(diff(t))), 30) / 60                      # minutes
  hrr <- pmin(pmax((hr - hrrest) / (hrmax - hrrest), 0), 1)
  trimp <- sum(dt * hrr * 0.64 * exp((if (male) 1.92 else 1.67) * hrr))   # Banister 1991
  zone <- cut(hr / hrmax, c(0, .5, .6, .7, .8, .9, Inf), labels = ZLAB, right = FALSE)
  zmin <- as.numeric(tapply(dt, zone, sum)); zmin[is.na(zmin)] <- 0; names(zmin) <- ZLAB
  s3 <- as.numeric(tapply(dt, cut(hr / hrmax, c(0, .82, .87, Inf), labels = c("a", "b", "c"), right = FALSE), sum)); s3[is.na(s3)] <- 0
  list(avg = mean(hr), max = max(hr), p99 = quantile(hr, .99, names = FALSE), trimp = trimp, edwards = sum(zmin[2:6] * 1:5), zmin = zmin,
       s3 = s3, minutes = sum(dt))
}
wk_series <- function(row, F, r) {
  if (!is.na(row$fit)) {
    f <- F[[row$fit]]$rec
    return(data.frame(t = f$t, hr = f$hr, speed = f$speed, alt = f$alt, cad = f$cad, src = "FIT file", stringsAsFactors = FALSE))
  }
  if (!is.na(row$db)) {
    x <- r$wd[r$wd$id == row$db, ]
    if (nrow(x)) {
      cad <- ifelse(!is.na(x$cad) & x$cad > 0, x$cad, ifelse(!is.na(x$rcad) & x$rcad > 0, x$rcad, ifelse(!is.na(x$srate) & x$srate > 0, x$srate, NA_real_)))
      return(data.frame(t = x$ts, hr = fix_hr(x$hr), speed = ifelse(!is.na(x$spd) & x$spd >= 0, x$spd / 10, NA_real_),   # raw unit = 0.1 m/s
                        alt = ifelse(is.na(x$alt) | x$alt < 0, NA_real_, x$alt / 10), cad = cad, src = "watch DB", stringsAsFactors = FALSE))
    }
  }
  a <- r$act
  if (!is.null(a) && nrow(a) && !is.na(row$start)) {                       # any brand: reconstruct from minute heart rate
    a <- a[a$src == 11 & a$ts >= row$start & a$ts < row$start + row$dur * 60, ]
    if (nrow(a) >= 3) return(data.frame(t = a$ts, hr = fix_hr(a$hr), speed = NA_real_, alt = NA_real_, cad = NA_real_, src = "minute HR", stringsAsFactors = FALSE))
  }
  NULL
}
# reference bands for the dashboard comparison plots (approximate; sources in 'Data & methods')
ref_defs <- function(age, sex) {
  old <- age >= 60; g <- "rgba(52,211,153,.28)"; n <- "rgba(150,150,150,.17)"
  list(
    steps = list(unit = "steps / day", bands = list(
      list(lo = if (old) 6000 else 8000, hi = if (old) 8000 else 10000, col = g, txt = "benefit plateau (Paluch 2022)"),
      list(lo = if (sex == "f") 1750 else 2250, hi = if (sex == "f") 9820 else 12330, col = n, txt = "US adults, 10th-90th pct (NHANES)")),
      lines = list(list(x = 6564, txt = "US adult mean"))),
    mvpa = list(unit = "moderate+ minutes / week (7-day window)", bands = list(list(lo = 150, hi = 300, col = g, txt = "WHO 2020 guideline")), lines = list()),
    hours = list(unit = "active hours / day (250+ steps)", bands = list(list(lo = 9, hi = 16, col = g, txt = "9+ hours: common wearable goal")), lines = list()),
    sleep = list(unit = "hours asleep", bands = list(list(lo = 7, hi = if (old) 8 else 9, col = g, txt = "recommended for your age (AASM/SRS)")), lines = list()),
    eff = list(unit = "sleep efficiency (%)", bands = list(list(lo = 85, hi = 100, col = g, txt = "good: 85% or more (NSF consensus)")), lines = list()),
    deep = list(unit = "deep sleep share (%)", bands = list(list(lo = 13, hi = 23, col = n, txt = "typical lab range, adults (approx.)")), lines = list()),
    rhr = list(unit = "resting heart rate (bpm)", bands = list(list(lo = 60, hi = 100, col = n, txt = "AHA normal range")), lines = list(list(x = 65, txt = "wearable cohort mean (Quer 2020)"))),
    hrv = list(unit = "HRV RMSSD (ms)", bands = list(list(lo = 27, hi = 57, col = n, txt = "healthy adults, mean +/- 1 SD (Nunan 2010, approx.)")), lines = list()))
}
# ---------------------------------------------------------------- device adapters ----
# Gadgetbridge stores every brand in its own tables. Each adapter converts its tables into one common shape
# (act: minute rows src 11, staged sleep src 13, unstaged sleep src 10; plus stress, workouts, profile),
# so everything downstream is brand independent. Add a new brand by writing one more adapt_*() function.
tsec2 <- function(x) ifelse(x > 1e11, x / 1000, x)
json_val <- function(txt, key) vapply(as.character(txt), function(t) {
  if (is.na(t)) return(NA_real_)
  m <- regmatches(t, regexec(paste0('"', key, '"\\s*:\\s*\\{[^}]*?"value"\\s*:\\s*(-?[0-9.]+)'), t))
  if (length(m[[1]]) >= 2) as.numeric(m[[1]][2]) else NA_real_ }, numeric(1), USE.NAMES = FALSE)
common_tables <- function(q) list(
  user = q("USER", "SELECT BIRTHDAY b, GENDER g FROM USER LIMIT 1"),
  goal = q("USER_ATTRIBUTES", "SELECT STEPS_GOAL_SPD g FROM USER_ATTRIBUTES ORDER BY _id DESC LIMIT 1"),
  device = q("DEVICE", "SELECT NAME name, MODEL model FROM DEVICE"))
adapt_huawei <- function(q) {
  list(
    act = q("HUAWEI_ACTIVITY_SAMPLE", "SELECT TIMESTAMP ts, OTHER_TIMESTAMP ots, SOURCE src, RAW_KIND kind, STEPS steps, CALORIES cal, DISTANCE dist, SPO spo, HEART_RATE hr, RESTING_HEART_RATE rhr FROM HUAWEI_ACTIVITY_SAMPLE WHERE OTHER_TIMESTAMP > TIMESTAMP"),
    stress = q("HUAWEI_STRESS_SAMPLE", "SELECT TIMESTAMP/1000 ts, STRESS stress, LEVEL level FROM HUAWEI_STRESS_SAMPLE"),
    wk = q("HUAWEI_WORKOUT_SUMMARY_SAMPLE", "SELECT * FROM HUAWEI_WORKOUT_SUMMARY_SAMPLE"),
    wd = { x <- q("HUAWEI_WORKOUT_DATA_SAMPLE", "SELECT * FROM HUAWEI_WORKOUT_DATA_SAMPLE"); pk <- function(n) if (n %in% names(x)) x[[n]] else rep(NA_real_, nrow(x))
    if (!nrow(x)) data.frame() else data.frame(id = pk("WORKOUT_ID"), ts = pk("TIMESTAMP"), hr = pk("HEART_RATE"), spd = pk("SPEED"), alt = pk("ALTITUDE"),
                                               cad = pk("CADENCE"), rcad = pk("RIDE_CADENCE"), srate = pk("STEP_RATE")) },
    pace = q("HUAWEI_WORKOUT_PACE_SAMPLE", "SELECT WORKOUT_ID id, PACE_INDEX idx, DISTANCE dist, TYPE type, PACE pace FROM HUAWEI_WORKOUT_PACE_SAMPLE"),
    base = q("BASE_ACTIVITY_SUMMARY", "SELECT NAME name, START_TIME/1000 s, ACTIVITY_KIND kind FROM BASE_ACTIVITY_SUMMARY"),
    user = q("USER", "SELECT BIRTHDAY b, GENDER g FROM USER LIMIT 1"),
    goal = q("USER_ATTRIBUTES", "SELECT STEPS_GOAL_SPD g FROM USER_ATTRIBUTES ORDER BY _id DESC LIMIT 1"),
    device = q("DEVICE", "SELECT NAME name, MODEL model FROM DEVICE"),
    hrv = q("HUAWEI_HRV_VALUE_SAMPLE", "SELECT TIMESTAMP ts, VALUE v FROM HUAWEI_HRV_VALUE_SAMPLE"),
    temp = q("HUAWEI_TEMPERATURE_SAMPLE", "SELECT TIMESTAMP ts, TEMPERATURE t FROM HUAWEI_TEMPERATURE_SAMPLE"),
    sstat = q("HUAWEI_SLEEP_STATS_SAMPLE", "SELECT * FROM HUAWEI_SLEEP_STATS_SAMPLE"),
    apnea = q("HUAWEI_SLEEP_APNEA_SAMPLE", "SELECT TIMESTAMP ts, LAST_TIMESTAMP lts, LEVEL lv FROM HUAWEI_SLEEP_APNEA_SAMPLE"),
    ecg = q("HUAWEI_ECG_SUMMARY_SAMPLE", "SELECT START_TIMESTAMP ts, END_TIMESTAMP ets, AVERAGE_HEART_RATE hr, ARRHYTHMIA_TYPE a FROM HUAWEI_ECG_SUMMARY_SAMPLE"))
}

# Huami: Amazfit, Zepp OS watches, Mi Band up to 7 (tables MI_BAND_ACTIVITY_SAMPLE / HUAMI_*). Minute rows already carry
# steps and heart rate; sleep stages are written by Gadgetbridge as RAW_KIND 120 light, 121 deep, 122 REM, 112 wake marker.
adapt_huami <- function(q) {
  pk <- function(x, nms) { for (k in nms) if (k %in% names(x)) return(x[[k]]); rep(NA_real_, nrow(x)) }
  x <- dplyr::bind_rows(q("MI_BAND_ACTIVITY_SAMPLE", "SELECT TIMESTAMP, DEVICE_ID, STEPS, RAW_KIND, HEART_RATE FROM MI_BAND_ACTIVITY_SAMPLE"),
                        q("HUAMI_EXTENDED_ACTIVITY_SAMPLE", "SELECT TIMESTAMP, DEVICE_ID, STEPS, RAW_KIND, HEART_RATE FROM HUAMI_EXTENDED_ACTIVITY_SAMPLE"))
  dev <- as.numeric(names(which.max(table(x$DEVICE_ID)))); x <- x[x$DEVICE_ID == dev, ]
  x <- x[!duplicated(x$TIMESTAMP), ]; x <- x[order(x$TIMESTAMP), ]
  minute <- data.frame(ts = x$TIMESTAMP, ots = x$TIMESTAMP + 60, src = 11, kind = -1, steps = ifelse(x$STEPS < 0, -1, x$STEPS),
                       cal = -1, dist = -1, spo = -1, hr = x$HEART_RATE, rhr = -1)
  sp <- q("HUAMI_SPO2_SAMPLE", "SELECT * FROM HUAMI_SPO2_SAMPLE")
  if (nrow(sp)) { i <- findInterval(tsec2(pk(sp, "TIMESTAMP")), minute$ts); v <- pk(sp, c("SPO2", "SPO_2", "VALUE")); ok <- i > 0 & !is.na(v) & v > 0 & v <= 100; minute$spo[i[ok]] <- v[ok] }
  rh <- q("HUAMI_HEART_RATE_RESTING_SAMPLE", "SELECT * FROM HUAMI_HEART_RATE_RESTING_SAMPLE")
  if (nrow(rh)) { i <- findInterval(tsec2(pk(rh, "TIMESTAMP")), minute$ts); v <- pk(rh, "HEART_RATE"); ok <- i > 0 & !is.na(v) & v > 0 & v < 255; minute$rhr[i[ok]] <- v[ok] }
  kc <- c(`120` = 6, `121` = 7, `122` = 9, `112` = 8)                                   # canonical codes: 6 light, 7 deep, 9 REM, 8 wake
  sl <- x[x$RAW_KIND %in% as.numeric(names(kc)), ]; seg <- data.frame()
  if (nrow(sl)) {
    code <- unname(kc[as.character(sl$RAW_KIND)]); g <- cumsum(c(TRUE, diff(sl$TIMESTAMP) != 60 | diff(code) != 0))
    seg <- data.frame(ts = as.numeric(tapply(sl$TIMESTAMP, g, min)), ots = as.numeric(tapply(sl$TIMESTAMP, g, max)) + 60, src = 13,
                      kind = as.numeric(tapply(code, g, function(z) z[1])), steps = -1, cal = -1, dist = -1, spo = -1, hr = -1, rhr = -1)
  }
  st <- q("HUAMI_STRESS_SAMPLE", "SELECT * FROM HUAMI_STRESS_SAMPLE"); stress <- data.frame()
  if (nrow(st)) { v <- pk(st, c("STRESS", "VALUE")); stress <- data.frame(ts = tsec2(pk(st, "TIMESTAMP")), stress = v, level = findInterval(v, c(0, 26, 51, 76)))
  stress <- stress[!is.na(stress$stress) & stress$stress > 0, ] }
  bs <- q("BASE_ACTIVITY_SUMMARY", "SELECT _id id, NAME name, START_TIME st, END_TIME en, ACTIVITY_KIND kind, SUMMARY_DATA js, DEVICE_ID dev FROM BASE_ACTIVITY_SUMMARY")
  wk <- data.frame(); base <- data.frame()
  if (nrow(bs)) { bs <- bs[bs$dev == dev, ]; if (nrow(bs)) { s0 <- tsec2(bs$st); e0 <- tsec2(bs$en)
  wk <- data.frame(WORKOUT_ID = 1e6 + bs$id, START_TIMESTAMP = s0, END_TIMESTAMP = e0, TOTAL_TIME = e0 - s0, CALORIES = json_val(bs$js, "caloriesBurnt"), DISTANCE = json_val(bs$js, "distanceMeters"))
  base <- data.frame(name = bs$name, s = s0, kind = bs$kind) } }
  c(list(act = dplyr::bind_rows(minute, seg), stress = stress, wk = wk, wd = data.frame(), pace = data.frame(), base = base,
         hrv = data.frame(), temp = data.frame(), sstat = data.frame(), apnea = data.frame(), ecg = data.frame()), common_tables(q))
}
merge_parts <- function(parts) {
  nm <- c("act", "stress", "wk", "wd", "pace", "base", "user", "goal", "device", "hrv", "temp", "sstat", "apnea", "ecg")
  out <- lapply(nm, function(n) {
    xs <- lapply(parts, function(p) p[[n]]); xs <- xs[vapply(xs, function(x) !is.null(x) && nrow(x) > 0, logical(1))]
    if (!length(xs)) data.frame() else if (length(xs) == 1) xs[[1]] else dplyr::bind_rows(xs) })
  names(out) <- nm; out$user <- head(out$user, 1); out$goal <- head(out$goal, 1)
  if (nrow(out$act)) out$act <- out$act[!duplicated(paste(out$act$src, out$act$ts)), ]
  out
}
complete_days <- function(d) {
  g <- data.frame(day = seq(min(d$day), max(d$day), by = "day"))
  left_join(g, d, by = "day")
}
roll_fun <- function(x, k, f = mean, minobs = ceiling(k / 2)) {
  n <- length(x); out <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    w <- x[max(1L, i - k + 1L):i]
    if (sum(!is.na(w)) >= minobs) out[i] <- f(w, na.rm = TRUE)
  }
  out
}
# Theil-Sen slope (per day) + Kendall p; weekly means when the series is long
trend_stats <- function(day, y) {
  ok <- !is.na(y); x <- as.numeric(day)[ok]; y <- y[ok]; n <- length(y)
  sdv <- if (n > 1) sd(y) else NA_real_
  if (n < 10 || is.na(sdv) || sdv == 0) return(list(n = n, slope = NA_real_, p = NA_real_, sd = sdv, icpt = NA_real_))
  if (n > 700) { wk <- floor(x / 7); a <- tapply(y, wk, mean); x <- as.numeric(names(a)) * 7; y <- as.numeric(a) }
  dx <- outer(x, x, "-"); dy <- outer(y, y, "-"); ut <- upper.tri(dx)
  dxv <- dx[ut]; dyv <- dy[ut]; k <- dxv != 0
  if (!any(k)) return(list(n = n, slope = NA_real_, p = NA_real_, sd = sdv, icpt = NA_real_))
  slope <- median(dyv[k] / dxv[k])
  p <- suppressWarnings(cor.test(x, y, method = "kendall", exact = FALSE)$p.value)
  list(n = n, slope = slope, p = p, sd = sdv, icpt = median(y - slope * x))
}
# "improving" only if p < 0.05 AND total change > 0.2 SD (Hopkins' smallest worthwhile change)
trend_verdict <- function(ts, span, better) {
  if (is.na(ts$slope) || is.na(ts$p)) return("not enough data")
  ch <- ts$slope * span
  if (ts$p >= 0.05 || abs(ch) < 0.2 * ts$sd) return("stable")
  up <- ch > 0
  if (better == "up") return(if (up) "improving" else "worsening")
  if (better == "down") return(if (up) "worsening" else "improving")
  if (up) "rising" else "falling"
}
VERD <- c(improving = "improving (+)", worsening = "worsening (-)", rising = "rising", falling = "falling", stable = "stable")
vtxt <- function(v) ifelse(v %in% names(VERD), unname(VERD[v]), v)

stage_of <- function(t, seg) {
  if (is.null(seg) || !nrow(seg)) return(rep(NA_character_, length(t)))
  seg <- seg[order(seg$start), ]; i <- findInterval(t, seg$start); j <- pmax(i, 1L)
  ifelse(i > 0 & t < seg$end[j], seg$stage[j], NA_character_)
}
# sedentary time inside the waking window: runs of minutes without steps
sed_stats <- function(m, h0, h1) {
  empty <- data.frame(day = as.Date(character()), sed_h = numeric(), sed_long = numeric(), sed60 = numeric())
  m <- m[m$hour >= h0 & m$hour < h1 & is.na(m$sidx), ]
  if (nrow(m) < 2) return(empty)
  m <- m[order(m$t), ]; nn <- nrow(m); s <- m$steps == 0
  g <- cumsum(c(TRUE, diff(m$t) != 60 | s[-1] != s[-nn]))
  r <- data.frame(day = m$day, g = g, s = s) %>% group_by(g) %>%
    summarise(day = dplyr::first(day), s = dplyr::first(s), len = n(), .groups = "drop") %>% dplyr::filter(s)
  if (!nrow(r)) return(empty)
  r %>% group_by(day) %>% summarise(sed_h = sum(len) / 60, sed_long = max(len), sed60 = sum(len[len >= 60]), .groups = "drop")
}
# Sleep Regularity Index (Phillips 2017): chance of the same sleep/wake state 24 h apart
sri_daily <- function(t, asleep, tz) {
  if (length(t) < 2880) return(NULL)
  t0 <- min(t); k <- as.integer(round((t - t0) / 60)) + 1L; nn <- max(k)
  if (nn <= 1440L) return(NULL)
  have <- logical(nn); st <- logical(nn); have[k] <- TRUE; st[k] <- asleep
  i <- seq_len(nn - 1440L); ok <- have[i] & have[i + 1440L]
  if (!any(ok)) return(NULL)
  eq <- st[i] == st[i + 1440L]
  day <- as.Date(as.POSIXct(t0 + (i - 1L) * 60, origin = "1970-01-01", tz = tz), tz = tz)
  a <- tapply(eq[ok], day[ok], mean)
  d <- data.frame(day = as.Date(names(a)), sri = 100 * (2 * as.numeric(a) - 1))
  d <- complete_days(d); d$roll7 <- roll_fun(d$sri, 7, mean, 4); d
}
agg_nights <- function(nd, how) {
  if (how == "Nightly") { nd$x <- as.character(nd$day); return(nd) }
  nd$x <- as.character(if (how == "Weekly mean") as.Date(cut(nd$day, "week")) else as.Date(format(nd$day, "%Y-%m-01")))
  nd %>% group_by(x) %>% summarise(across(c(deep, light, rem, unst, awake, hr_med, hr_p5, hr_deep, hr_light, hr_rem, eff, mid_min, asleep), ~ mn(.x)), .groups = "drop")
}
SUMIDS <- c("sport_min", "trimp", "sessions", "mvpa")
CFAM <- list(act = c("steps", "steps_meas", "core", "arte", "kcal", "cad100", "cad130"),
             hr = c("hr_p5", "hr_med", "hr_night", "rhr_w", "hr_sleep", "hr_awake", "vo2", "dip"),
             tr = c("sport_min", "trimp", "sessions"))
CMET <- data.frame(stringsAsFactors = FALSE,
                   id = c("steps", "steps_meas", "core", "arte", "kcal", "mvpa", "mem", "cad100", "cad130", "sed_h", "sed60", "sed_long",
                          "asleep_h", "eff", "deep_pct", "mid_h",
                          "hr_night", "hr_sleep", "hr_awake", "rhr_w", "hr_p5", "hr_med", "hr_max", "dip", "vo2",
                          "stress", "stress_hi", "spo_mean", "spo_min",
                          "sport_min", "trimp", "sessions",
                          "hrv", "skin_temp", "sleep_score", "rdi", "breath", "spo_min_sleep",
                          "sc_ready", "sc_sleep", "sc_activity", "sc_strain", "sc_warn"),
                   lab = c("Steps / day (selected basis)", "Measured steps / day", "Walking-bout steps / day", "Artefact steps / day", "Active kcal / day", "Moderate+ minutes (HR) / day", "Moderate-equivalent minutes (moderate + 2 x vigorous)",
                           "Minutes at >= 100 steps/min", "Minutes at >= 130 steps/min", "Sedentary hours (waking window)", "Minutes in sedentary bouts >= 60 min", "Longest sedentary stretch (min)",
                           "Sleep (h / night)", "Sleep efficiency (%)", "Deep sleep share (%)", "Sleep midpoint (clock h, 24+ = after midnight)",
                           "Sleeping HR nadir (bpm)", "Median sleeping HR (bpm)", "Median daytime HR (bpm)", "Watch resting HR (bpm)", "Daily 5th percentile HR (bpm)", "Daily median HR (bpm)", "Daily max HR (bpm)",
                           "Nocturnal HR dip (%)", "Fitness proxy: VO2max (Uth)",
                           "Stress score (mean)", "Medium/high stress readings (%)", "SpO2 mean (%)", "SpO2 lowest (%)",
                           "Workout minutes", "Workout load (TRIMP)", "Workout sessions",
                           "HRV (ms, nightly)", "Skin temperature (deg C, nightly)", "Sleep score", "RDI (events/h)", "Breathing rate in sleep", "Lowest SpO2 in sleep (%)",
                           "Readiness score", "Sleep score (own)", "Activity score (PAI-style)", "Training strain score", "Exhaustion warning signs"),
                   grp = c(rep("Activity", 12), rep("Sleep", 4), rep("Heart", 9), rep("Stress and SpO2", 4), rep("Training", 3), rep("Advanced (newer watches)", 6), rep("Scores", 5)))
shift_lead <- function(y, k) {
  n <- length(y); if (k == 0 || n == 0) return(y)
  if (abs(k) >= n) return(rep(NA_real_, n))
  if (k > 0) c(y[(k + 1):n], rep(NA_real_, k)) else c(rep(NA_real_, -k), y[1:(n + k)])
}
cor_agg <- function(d, how, cols) {
  cols <- intersect(unique(cols), names(d))
  if (how == "Day") return(d[, c("day", cols), drop = FALSE])
  d$per <- if (how == "Week") as.Date(cut(d$day, "week")) else as.Date(format(d$day, "%Y-%m-01"))
  need <- if (how == "Week") 4 else 15
  out <- lapply(split(d, d$per), function(z) {
    ok <- sum(!is.na(z$steps) | !is.na(z$asleep_h)) >= need
    r <- vapply(cols, function(cn) { x <- z[[cn]]
    if (!ok || all(is.na(x))) NA_real_ else if (cn %in% SUMIDS || startsWith(cn, "sp_")) sum(x, na.rm = TRUE) else mean(x, na.rm = TRUE) }, numeric(1))
    df <- as.data.frame(as.list(r), check.names = FALSE); df$day <- z$per[1]; df
  })
  out <- do.call(rbind, out); rownames(out) <- NULL; out[, c("day", cols), drop = FALSE]
}
cor_stats <- function(x, y, spear) {
  n <- length(x); res <- list(n = n)
  if (n < 5 || sd(x) == 0 || sd(y) == 0) return(res)
  if (spear) {
    ct <- suppressWarnings(cor.test(x, y, method = "spearman", exact = FALSE)); r <- unname(ct$estimate); r <- max(min(r, .999999), -.999999)
    ci <- tanh(atanh(r) + c(-1, 1) * 1.96 * sqrt((1 + r^2 / 2) / (n - 3))); p <- ct$p.value
  } else { ct <- cor.test(x, y); r <- unname(ct$estimate); ci <- as.numeric(ct$conf.int); p <- ct$p.value }
  fit <- lm(y ~ x); sm <- summary(fit); cf <- confint(fit)
  list(n = n, r = r, ci = ci, p = p, r2 = if (spear) r^2 else sm$r.squared, slope = unname(coef(fit)[2]), slope_ci = as.numeric(cf[2, ]), fit = fit)
}
DAYS <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
TREND <- data.frame(
  id = c("steps", "core", "mvpa", "sport_min", "trimp", "sed_h", "sed60", "asleep_h", "eff", "deep_pct", "mid_h", "hr_night", "rhr_w", "hr_sleep", "dip", "stress", "vo2"),
  lab = c("Steps / day", "Walking-bout steps / day", "Moderate+ minutes (HR) / day", "Workout minutes / day", "Workout load (TRIMP) / day",
          "Sedentary hours (waking window)", "Minutes in sedentary bouts >= 60 min", "Sleep (h / night)", "Sleep efficiency (%)",
          "Deep sleep share (%, device estimate)", "Sleep midpoint (clock h, 24+ = after midnight)", "Sleeping HR nadir (bpm)",
          "Watch resting HR (bpm)", "Median sleeping HR (bpm)", "Nocturnal HR dip (%)", "Stress score", "Fitness proxy: VO2max (Uth)"),
  better = c("up", "up", "up", "up", "up", "down", "down", "up", "up", "up", "neutral", "down", "down", "down", "up", "down", "up"),
  stringsAsFactors = FALSE)

# ================================================================== UI =======
tz0 <- tryCatch(Sys.timezone(), error = function(e) NA_character_)
if (is.null(tz0) || is.na(tz0) || !(tz0 %in% OlsonNames())) tz0 <- "UTC"
sect <- function(title, ..., note = NULL)
  card(class = "mb-3", full_screen = TRUE, card_header(title),
       card_body(fillable = FALSE, fill = FALSE, ...,
                 if (!is.null(note)) tags$p(class = "text-muted small mb-0 mt-2", note)))
gcard <- function(id) card(class = "p-2 mb-3", gOut(id))
# height lives on a wrapper div (plotlyOutput only accepts plain CSS units, not clamp())
pH <- function(id, h = 380) div(style = sprintf("height: clamp(%dpx, %.0fvh, %dpx);", round(h * 0.8), h / 9.5, round(h * 1.8)), plotlyOutput(id, height = "100%"))

gOut <- function(id) div(style = "height: clamp(200px, 26vh, 360px);", plotlyOutput(id, height = "100%"))
grid2 <- function(...) layout_columns(col_widths = breakpoints(sm = 12, xl = 6), ...)
grid3 <- function(...) layout_columns(col_widths = breakpoints(sm = 12, lg = 4), ...)
make_demo <- function(last_day, tz) {      # synthetic values to test the advanced-metric plots
  set.seed(42); days <- seq(last_day - 119, last_day, by = "day"); n <- length(days)
  at <- function(h) as.numeric(as.POSIXct(paste(days, sprintf("%02d:00:00", h)), tz = tz))
  k <- rpois(n, .8)
  list(hrv = data.frame(ts = at(3), v = exp(log(52) + seq(0, .08, length.out = n) + rnorm(n, 0, .13))),
       temp = data.frame(ts = at(3), t = 33.6 + rnorm(n, 0, .18) + ifelse(seq_len(n) %% 28 > 14, .25, 0)),
       sstat = data.frame(TIMESTAMP = at(7), SLEEP_SCORE = round(pmin(98, rnorm(n, 78, 7))), RDI = pmax(0, rnorm(n, 2.5, 1.5)),
                          AVG_BREATH_RATE = rnorm(n, 15, .7), MIN_OXYGEN_SATURATION = round(pmin(99, rnorm(n, 91, 2)))),
       apnea = data.frame(ts = rep(at(2), times = k) + runif(sum(k), 0, 3600), lts = rep(at(2), times = k) + 3600 + runif(sum(k), 0, 60), lv = 1),
       ecg = data.frame(ts = at(10)[c(10, 45, 90)], ets = at(10)[c(10, 45, 90)] + 30, hr = c(68, 72, 65), a = c(0, 0, 0)))
}

ui <- page_sidebar(
  title = "Gadgetbridge Lab", theme = mk_theme("Dark"), fillable = FALSE,
  sidebar = sidebar(width = 340,
                    fileInput("src", "Gadgetbridge .db or export .zip", accept = c(".db", ".sqlite", ".zip")),
                    textInput("fit_dir", "Workout folder with .fit files (searched recursively)",
                              placeholder = "path to a folder with .fit files"),
                    fileInput("fit_up", "...or pick .fit files", multiple = TRUE, accept = ".fit"),
                    selectInput("tz", "Time zone", choices = OlsonNames(), selected = tz0),
                    dateRangeInput("range", "Date range", weekstart = 1),
                    radioButtons("steps_basis", "Steps shown", c("Real steps (typing and arm movement removed)" = "real", "Measured by the watch" = "meas"), "real"),
                    selectInput("theme", "Theme", c("Dark", "Neon", "Light"), "Dark"),
                    checkboxInput("demo", "Demo values for HRV, temperature, sleep stats (layout test)", FALSE),
                    accordion(open = FALSE,
                              accordion_panel("Profile & parameters",
                                              numericInput("age", "Age (years)", 30, 10, 100),
                                              selectInput("sex", "Sex (TRIMP weighting)", c(Male = "m", Female = "f")),
                                              selectInput("hrmax_m", "HRmax estimate", c("Tanaka 208 - 0.7 x age" = "tanaka", "Fox 220 - age" = "fox",
                                                                                         "Observed (99.9th percentile)" = "obs", "Manual" = "man")),
                                              numericInput("hrmax_man", "Manual HRmax", 190),
                                              numericInput("rhr_man", "Resting HR override (0 = auto)", 0),
                                              numericInput("goal", "Daily steps reference line", 8000),
                                              numericInput("act_goal", "Daily active-minutes goal (ring)", 30, 5, 120),
                                              sliderInput("cad", "Walking bout: cadence >= (steps/min)", 30, 120, 60),
                                              sliderInput("blen", "...sustained for >= (min)", 2, 15, 5),
                                              sliderInput("art_margin", "Artefact test: HR within sedentary median + (bpm)", 0, 15, 5),
                                              tags$hr(),
                                              selectizeInput("c_light", "Raw codes = light sleep", as.character(1:12), "6", multiple = TRUE),
                                              selectizeInput("c_deep", "Raw codes = deep sleep", as.character(1:12), "7", multiple = TRUE),
                                              selectizeInput("c_rem", "Raw codes = REM", as.character(1:12), "9", multiple = TRUE),
                                              selectizeInput("c_awake", "Raw codes = awake marker", as.character(1:12), "8", multiple = TRUE),
                                              sliderInput("gap", "Merge sleep segments if gap <= (min)", 15, 240, 120),
                                              sliderInput("nap", "Counts as night if asleep >= (h)", 1, 6, 3, step = 0.5),
                                              tags$hr(),
                                              sliderInput("need", "Your sleep need (h)", 6, 10, 7.5, step = 0.25),
                                              selectizeInput("free_days", "Free days (no alarm clock)", c(Mon = 1, Tue = 2, Wed = 3, Thu = 4, Fri = 5, Sat = 6, Sun = 7),
                                                             selected = c(6, 7), multiple = TRUE),
                                              numericInput("wake0", "Waking window from (h)", 7, 0, 23), numericInput("wake1", "...to (h)", 22, 1, 24),
                                              textAreaInput("sport_map", "Rename sport labels (Old=New, one per line)", rows = 3,
                                                            placeholder = "Exercise (generic)=Strength training")))),
  uiOutput("empty"),
  navset_tab(id = "tab",
             nav_panel("Today",
          card(class = "mb-3",
               div(class = "d-flex flex-wrap align-items-center gap-2 p-2",
                   actionButton("day_prev", "<", class = "btn-sm btn-outline-secondary"),
                   dateInput("home_day", NULL, width = "170px", weekstart = 1),
                   actionButton("day_next", ">", class = "btn-sm btn-outline-secondary"),
                   selectInput("ring_win", NULL, c("Last 7 days", "Last 30 days"), "Last 7 days", width = "150px"),
                   tags$span(class = "text-muted small ms-2", "rings per complete day; tiles and charts show the day picked here")),
               uiOutput("home_rings")),
          uiOutput("home_tiles"),
          sect("Your days at a glance",
               div(class = "d-flex flex-wrap gap-3 align-items-center",
                   selectInput("tl_win", NULL, c("Last 31 days", "Last 90 days", "Whole range"), "Last 90 days", width = "170px"),
                   tags$span(class = "text-muted small", "one column per day, midnight at the top - scroll sideways through the weeks")),
               uiOutput("tl_ui"),
               accordion(open = FALSE, accordion_panel("Daily totals: sleep, awake, workouts, walking, idle", pH("p_tl_tot", 300)))),
          grid2(sect("Heart rate on the picked day", pH("p_home_hr", 320)), sect("Sleep, night before", pH("p_home_sleep", 320))),
          sect("Heart rate, day by day", selectInput("dash_win", NULL, c("Last 7 days", "Last 30 days", "Whole range"), "Last 30 days", width = "180px"),
               pH("p_hr_box", 400))),
nav_panel("Metrics",
          div(class = "d-flex flex-wrap align-items-end gap-3",
              selectInput("gb_agg", "View", c("Day", "Week", "Month"), "Day", width = "120px"),
              numericInput("gb_stride", "Stride (m)", 0.75, 0.4, 1.2, 0.01, width = "110px"),
              numericInput("gb_kg", "Weight (kg)", 75, 30, 200, 1, width = "110px"),
              numericInput("gb_cm", "Height (cm)", 175, 120, 220, 1, width = "110px"),
              tags$span(class = "text-muted small pb-3", "stride, weight and height only feed the rough distance and calorie estimates")),
          uiOutput("gb_tiles"),
          grid2(sect("Distance walked (rough, km)", pH("gb_dist", 320)), sect("Active time (minutes)", pH("gb_active", 320)),
                sect("Stress score", pH("gb_stress", 320)), sect("SpO2 range per day", pH("gb_spo", 320)),
                sect("Calories burned, total (estimated)", pH("gb_kcal_tot", 320)), sect("Calories from activity", pH("gb_kcal_act", 320))),
          sect("Fitness: VO2max, your own estimate", pH("gb_vo2", 380),
               note = "The watch's own VO2max is deliberately not used. Dots = one estimate per steady run (running equation of the ACSM divided by the share of your heart-rate reserve used; only runs at 60-95 % of reserve count). Line = resting-heart-rate proxy by month (Uth). Both are rough, typically 10-15 % off in absolute terms, so trust the direction over months, not the number.")),
             nav_panel("Monthly review",
                       sect("Review month", selectInput("mon", NULL, NULL, width = "200px"), uiOutput("mon_text")),
                       sect("Month vs previous months", tableOutput("tbl_mon"),
                            note = "Signal appears when the change exceeds 0.2 SD of your daily values (smallest worthwhile change)."),
                       grid2(sect("Steps", pH("p_mon_steps", 300)), sect("Sleep", pH("p_mon_sleep", 300))),
                       sect("Sleeping HR nadir", pH("p_mon_rhr", 300)),
                       grid2(sect("Heart rate per sport", pH("p_mon_wk_hr", 380)), sect("Minutes, energy and load per sport", pH("p_mon_wk_eff", 380)))),
             nav_panel("Trends",
                       sect("Is it getting better or worse?", tableOutput("tbl_trend"),
                            note = "Trend = Theil-Sen slope over the selected date range with a Kendall test. A metric only counts as improving/worsening if p < 0.05 AND the total change exceeds 0.2 SD of its daily values (smallest worthwhile change). Partial days are excluded. Set the date range to 'all' for long-term views."),
                       sect("Metric explorer",
                            layout_columns(col_widths = breakpoints(sm = 12, md = 6), selectInput("tr_metric", "Metric", setNames(TREND$id, TREND$lab), "asleep_h"),
                                           selectInput("tr_win", "Smoothing", c("7 days", "14 days", "28 days", "56 days"), "28 days")),
                            pH("p_trend", 440)),
                       grid2( sect("Monthly mean +/- SD", pH("p_tr_month")), sect("Weekday pattern", pH("p_tr_dow")))),
             nav_panel("Correlations",
                       sect("Relate any two measurements",
                            layout_columns(col_widths = breakpoints(sm = 12, md = 4), selectInput("cor_x", "X axis", NULL), selectInput("cor_y", "Y axis", NULL),
                                           selectInput("cor_agg", "Resolution", c("Day", "Week", "Month"), "Day")),
                            layout_columns(col_widths = breakpoints(sm = 12, md = 4),
                                           sliderInput("cor_lag", "Lag: Y is taken N periods after X", -7, 14, 0, step = 1),
                                           selectInput("cor_col", "Colour points by", c("None", "Date (older to newer)", "Weekday", "Month", "Free day vs work day", "Another metric")),
                                           selectInput("cor_col2", "Colour metric (if 'Another metric')", NULL)),
                            checkboxGroupInput("cor_opts", NULL, c("Trend line" = "line", "95% confidence band of the line" = "band", "Show r, R2, CI, p in the plot" = "stats",
                                                                   "Rank correlation (Spearman)" = "spear", "Smooth curve (LOESS)" = "loess", "Hide outliers beyond 3 SD" = "out"), selected = c("line", "band", "stats"), inline = TRUE),
                            pH("p_cor", 480), uiOutput("cor_text"),
                            note = "Week and Month use totals for workout minutes/load/sessions/moderate minutes and averages for everything else (weeks need 4+ valid days). Lag 1 asks, for example, whether last night's sleep relates to the next morning's resting HR. Trend line, band and statistics need 8+ points."),
                       sect("Strongest relationships in your data", actionButton("cor_scan", "Scan all pairs", class = "btn-primary mb-2"),
                            div(class = "text-muted small mb-2", "Spearman correlation for every pair with 15+ shared values at the chosen resolution. Pairs from the same family (for example two step counts) and near-duplicates are skipped. With hundreds of pairs some look strong by chance, so rely on the adjusted p (Benjamini-Hochberg)."),
                            grid2(tableOutput("tbl_cor_top"), pH("p_cor_map", 500)))),
             nav_panel("Sleep",
                       grid2(sect("Night in a ring", pH("p_donut", 340)), sect("Sleep per night", pH("p_sleep", 340))),
                       sect("Night viewer", selectInput("night", NULL, NULL, width = "340px"), pH("p_hypno", 500),
                            note = "Stage codes differ by brand and firmware - check the mapping under 'Profile & parameters' against the code table in 'Data & methods'. Stages from wrist devices are weak; sleep/wake is far better than deep/REM."),
                       grid2(
                         sect("Bed / wake regularity", pH("p_reg")), sect("Sleeping HR vs daytime HR", pH("p_sleephr"))),
                       sect("Sleep sessions", tableOutput("tbl_ses")),
                       sect("Stages with pulse on top", selectInput("sl_agg", NULL, c("Nightly", "Weekly mean", "Monthly mean"), "Nightly", width = "200px"),
                            pH("p_sl_stack", 440), note = "Bars = hours per stage and awake time in bed (left axis). Lines = median sleeping HR and nightly nadir (right axis). Use weekly/monthly for years of data."),
                       grid2( sect("Pulse by sleep stage", pH("p_sl_stagehr")),
                              sect("Stage composition (% of time in bed)", pH("p_sl_comp"))),
                       grid2(
                         sect("Sleep Regularity Index (7-day mean)", pH("p_sri"), note = "100 = identical sleep/wake pattern at the same clock time every day (Phillips 2017). Nights the watch failed to detect count as awake and lower the value."),
                         sect("Sleep debt: hours short of your need (14-day sum)", pH("p_debt"))),
                       sect("Social jetlag by month (free-day vs work-day sleep midpoint)", pH("p_sjl", 300),
                            note = "Simplified Roenneberg measure without oversleep correction. Large positive = you sleep much later on free days."),
                       grid2(sect("Pulse during each stage (all nights)", pH("p_sl_violin", 340)), sect("Stage minutes per night", pH("p_sl_box", 340)))),
             nav_panel("Heart",
          sect("Heart rate timeline", selectInput("hr_res", NULL, c("Auto", "1 min", "5 min", "15 min", "60 min"), "Auto", width = "160px"),
               pH("p_hr", 420), note = "Shaded bands = detected sleep. Coarser resolution shows the median with a min-max ribbon."),
          grid2(sect("Daily rhythm (median, 10-90 %)", pH("p_circ")), sect("Daily distribution", pH("p_hr_violin"))),
          sect("Resting heart rate over time",
               selectInput("rhr_src", NULL, c("Sleeping nadir (5th pct of sleep HR)" = "hr_night", "Watch-reported resting HR" = "rhr_w",
                                              "Daily 5th percentile" = "hr_p5", "Median sleeping HR" = "hr_sleep"), width = "320px"),
               pH("p_rhr", 420), uiOutput("rhr_text"),
               note = "Band = your own normal range (previous 28 days). Red dots = above it; 2+ days in a row can mean illness, heavy training or short sleep."),
          grid2(sect("Stress", pH("p_stress")), sect("Stress by hour of day", pH("p_stresshr")))),
             nav_panel("Steps",
                       grid2(sect("The day in numbers", uiOutput("arcs_day")), sect("The day on a clock", pH("p_clock", 380))),
                       sect("Step quality", pH("p_cls", 400),
                            note = "Walking bout = sustained cadence. 'Likely artefact' = steps below walking cadence while HR shows no response (<= your sedentary median + margin) - typical of typing and hand movement. 'Other' = slow movement that does raise HR."),
                       grid2(
                         sect("Cadence-based intensity (steps/min)", pH("p_cadmin")),
                         sect("Steps by hour of day", pH("p_heat"))),
                       sect("Sedentary time (waking window)", pH("p_sed", 360),
                            note = "Runs of minutes without steps inside your waking window (Profile & parameters). Bars = hours, solid line = hours inside bouts of 60+ min, dotted = longest bout."),
                       grid2(sect("Steps per waking hour by weekday", pH("p_step_dow", 340)), sect("Walking cadence per day (steps/min while moving)", pH("p_cad_violin", 340)))),
             nav_panel("Workouts & training",
                       sect("Training calendar (workout load per day)", pH("p_wk_cal", 260)),
                       sect("Workouts", tableOutput("tbl_wk")),
                       sect("Session viewer", selectInput("wk_sel", NULL, NULL, width = "480px"), uiOutput("wk_info"),
                            checkboxGroupInput("wk_over", "Overlay curves (clicking legend entries also hides or shows them)", choices = character(), inline = TRUE),
                            pH("p_wk", 440)),
                       grid2( sect("Time in zone (min)", pH("p_wkz", 320)),
                              sect("Pace per km (GPS workouts)", pH("p_pace", 320))),
                       sect("Sport volume and composition", selectInput("sp_agg", NULL, c("Week", "Month"), "Week", width = "140px"),
                            grid2( pH("p_sp_time", 400), pH("p_sp_comp", 400)),
                            note = "Dashed line = WHO minimum of 150 min/week moderate activity (workout minutes only). Rename generic labels such as 'Exercise (generic)' in Profile & parameters."),
                       grid2(
                         sect("Which weekday?", selectInput("sp_metric", NULL, c("Sessions", "Minutes", "Load (TRIMP)"), "Sessions", width = "180px"), pH("p_sp_dow", 360)),
                         sect("Start time of day", pH("p_sp_hour", 360))),
                       grid2(
                         sect("Load, fatigue and fitness (EWMA model)", pH("p_load", 400),
                              note = "Needs >= 4 weeks. Acute:chronic load ratios are widely used but their injury-predictive value is disputed (Impellizzeri 2020) - read them as description, not prediction."),
                         sect("Intensity distribution of workouts", pH("p_intens", 400),
                              note = "Share of workout time below 82 %, 82-87 % and above 87 % of HRmax (approximation of Seiler's 3-zone model; thresholds differ per person).")),
                       sect("Weekly training log", tableOutput("tbl_wkly")),
                       sect("Trends within one sport",
                            layout_columns(col_widths = breakpoints(sm = 12, md = 4), selectInput("ps_sport", "Sport", NULL),
                                           selectInput("ps_metric", "Metric", c("Peak HR (99th pct of session)" = "p99_hr", "Max HR" = "max_hr", "Average HR" = "avg_hr",
                                                                                "Average % of HRmax" = "pct_avg", "Duration (min)" = "dur", "Energy (kcal)" = "kcal", "Load (TRIMP)" = "trimp",
                                                                                "Load per minute" = "trimp_min", "Time above 87 % HRmax (%)" = "hi_pct", "Distance (km)" = "dist",
                                                                                "Average speed (km/h)" = "speed", "Pace (min/km)" = "pace", "Metres per heartbeat (efficiency)" = "mpb")),
                                           selectInput("ps_win", "Smoothing (sessions)", c("3", "5", "10"), "5")),
                            pH("p_ps", 420),
                            note = "Dots = sessions, line = rolling mean, dashed = Theil-Sen trend. Peak HR reflects effort, not fitness; lower average HR at equal effort suggests better fitness. Session peaks can also come from sensor spikes - the 99th percentile is more robust than the single max. Needs >= 10 sessions for a trend test."),
                       grid2( sect("Session trend statistics", tableOutput("tbl_ps")),
                              sect("Selected metric by month (box = spread of sessions)", pH("p_ps_month", 340))),
                       grid2(sect("Selected metric: all sports, monthly mean", pH("p_ps_all", 340)), sect("Selected metric by sport (each dot a session)", pH("p_sp_box", 340)))),
             nav_panel("Recovery & advanced",
                       sect("Recovery overview: every metric against your own baseline", pH("p_recov", 340),
                            note = "Green = more favourable than your own recent baseline, red = less favourable (robust z-score of the previous 28-60 days, capped at +/-3, direction adjusted per metric). Rows appear as soon as the watch records that metric."),
                       grid2(sect("HRV: nightly value vs your normal range", pH("p_hrv"), note = "Normal range = mean +/- 0.75 SD of ln(HRV) over the previous 60 nights; the 7-day mean is the line to watch (Plews 2013)."),
                             sect("HRV variability (7-day coefficient of variation)", pH("p_hrv_cv"), note = "Rising day-to-day variability of HRV often accompanies accumulated fatigue; very low CV with a falling mean can too.")),
                       grid2(sect("Skin temperature: nightly deviation from baseline", pH("p_temp"), note = "Deviation from the median of the previous 28 nights. Persistent shifts of a few tenths of a degree can accompany illness or the menstrual cycle; single nights are noise."),
                             sect("Skin temperature: nightly mean", pH("p_temp_abs"))),
                       grid2(sect("Sleep score", pH("p_ss_score", 300)), sect("Breathing rate in sleep", pH("p_ss_breath", 300))),
                       grid2(sect("Respiratory disturbance index (RDI)", pH("p_ss_rdi", 300), note = "Dotted lines at 5 and 15 events/hour mirror the AASM mild/moderate apnea cut-offs for AHI. A watch estimate is a screening hint, not a diagnosis."),
                             sect("Lowest SpO2 per night", pH("p_ss_spo", 300))),
                       grid2(sect("Apnea events per night", pH("p_apnea", 300)), sect("ECG recordings", tableOutput("tbl_ecg"))),
                       sect("Stress during sleep (HRV-derived score)", pH("p_px_stress", 320),
                            note = "The watch's stress score is derived from heart-rate variability; its mean during sleep stands in for nightly HRV on devices that do not record an HRV value.")),
             nav_panel("Data & methods",
                       sect("Detected data source", tableOutput("tbl_src"), note = "Gadgetbridge writes each brand into its own tables; the app converts them into one common shape. Supported: Huawei/Honor and Huami (Amazfit, Zepp OS, Mi Band). If you use another brand, share the list of non-empty tables (shown below) so an adapter can be added."),
                       sect("What the scores are and are not", tableOutput("tbl_scores"), note = "Composite scores from commercial watches are mostly proprietary. These versions use only components with a scientific basis, show their formula, and compare you with yourself."),
                       sect("Reference values used on the Dashboard", tableOutput("tbl_refs"), note = "Rounded orientation values from the cited studies and guidelines; not diagnostic."),
                       sect("What your watch records", tableOutput("tbl_avail"),
                            note = "Gadgetbridge already has tables for HRV, skin temperature, sleep statistics, apnea and ECG, so the 'Recovery & advanced' tab fills automatically once they contain rows. Units are assumed from the table layout (HRV in ms, temperature in deg C) and may need adjusting on first real data."),
                       grid2(
                         sect("Raw stage codes (check the mapping)", tableOutput("tbl_codes"),
                              note = "Daytime codes = activity classes, night codes = sleep. Change the mapping under Profile & parameters."),
                         sect("Database content", tableOutput("tbl_counts"))),
                       sect("Coverage per day", tableOutput("tbl_cov")), uiOutput("dq_notes"),
                       sect("Derived parameters", tableOutput("tbl_zones")), uiOutput("methods"))))

# ============================================================ SERVER =========
server <- function(input, output, session) {
  TH <- reactive(THEMES[[input$theme %||% "Dark"]])
  observeEvent(input$theme, session$setCurrentTheme(mk_theme(input$theme)), ignoreInit = TRUE)
  
  # ---- input handling --------------------------------------------------------
  src <- reactive({
    req(input$src); p <- input$src$datapath; k <- sniff(p)
    if (k == "db") return(list(db = p, fit = character()))
    shiny::validate(shiny::need(k == "zip", "Please provide the Gadgetbridge .db (SQLite) or the export .zip"))
    ex <- file.path(tempdir(), paste0("gb_", as.integer(Sys.time()))); dir.create(ex, TRUE, FALSE)
    nm <- utils::unzip(p, list = TRUE)$Name
    dbn <- nm[grepl("(^|/)database/Gadgetbridge$", nm)]
    shiny::validate(shiny::need(length(dbn) >= 1, "No database/Gadgetbridge file inside this zip"))
    fitn <- nm[grepl("\\.fit$", nm, ignore.case = TRUE)]
    utils::unzip(p, files = c(dbn[1], fitn), exdir = ex)
    list(db = file.path(ex, dbn[1]), fit = file.path(ex, fitn))
  })
  fit_paths <- reactive({
    p <- character()
    if (!is.null(input$src)) p <- c(p, src()$fit)
    if (!is.null(input$fit_up)) p <- c(p, input$fit_up$datapath)
    d <- trimws(input$fit_dir %||% "")
    if (nzchar(d) && dir.exists(d)) p <- c(p, list.files(d, "\\.fit$", recursive = TRUE, ignore.case = TRUE, full.names = TRUE))
    unique(p)
  })
  fits <- reactive({
    p <- fit_paths(); if (!length(p)) return(list())
    res <- vector("list", length(p))
    withProgress(message = "Reading FIT files", value = 0, for (i in seq_along(p)) {
      res[i] <- list(tryCatch(read_fit(p[i]), error = function(e) NULL)); incProgress(1 / length(p)) })
    res <- res[!vapply(res, is.null, logical(1))]
    if (length(res)) res <- res[!duplicated(vapply(res, function(f) round(f$start), numeric(1)))]
    res
  })
  
  raw <- reactive({
    f <- src()$db
    con <- DBI::dbConnect(RSQLite::SQLite(), f, flags = RSQLite::SQLITE_RO); on.exit(DBI::dbDisconnect(con))
    tb <- DBI::dbListTables(con)
    q <- function(t, sql) if (t %in% tb) DBI::dbGetQuery(con, sql) else data.frame()
    cnt <- vapply(tb, function(t) as.numeric(DBI::dbGetQuery(con, paste0('SELECT COUNT(*) n FROM "', t, '"'))$n), numeric(1))
    n <- function(t) { v <- cnt[t]; if (is.na(v)) 0 else v }
    fams <- c(if (n("HUAWEI_ACTIVITY_SAMPLE") > 0) "huawei", if (n("MI_BAND_ACTIVITY_SAMPLE") + n("HUAMI_EXTENDED_ACTIVITY_SAMPLE") > 0) "huami")
    shiny::validate(shiny::need(length(fams) > 0, paste0("No supported activity data found. Supported so far: Huawei/Honor and Huami (Amazfit, Zepp OS, Mi Band). Non-empty tables in this file: ",
                                                         paste(names(cnt)[cnt > 0], collapse = ", "))))
    parts <- list(); if ("huawei" %in% fams) parts$huawei <- adapt_huawei(q); if ("huami" %in% fams) parts$huami <- adapt_huami(q)
    last <- vapply(parts, function(p) { a <- p$act[p$act$src == 11, ]; if (nrow(a)) max(a$ts) else 0 }, numeric(1)); parts <- parts[order(-last)]   # newest watch wins where two overlap
    out <- merge_parts(parts); out$counts <- cnt; out$family <- names(parts); out
  })
  observeEvent(raw(), {
    u <- raw()$user
    if (nrow(u) && !is.na(u$b) && u$b > 0) {
      a <- as.numeric(difftime(Sys.time(), as.POSIXct(u$b / 1000, origin = "1970-01-01", tz = "UTC"), units = "days")) / 365.25
      if (a > 10 && a < 100) updateNumericInput(session, "age", value = round(a))
      updateSelectInput(session, "sex", selected = if (isTRUE(u$g == 0)) "f" else "m")
    }
    g <- raw()$goal; if (nrow(g) && !is.na(g$g) && g$g > 0) updateNumericInput(session, "goal", value = g$g)
  })
  
  # ---- minute-level table ------------------------------------------------------
  M <- reactive({
    a <- raw()$act; a <- a[a$src == 11, ]; tz <- input$tz
    shiny::validate(shiny::need(nrow(a) > 0, "No minute data (source 11) in this database."))
    tm <- as.POSIXct(a$ts, origin = "1970-01-01", tz = tz)
    m <- data.frame(t = a$ts, time = tm, day = as.Date(tm, tz = tz), hour = as.integer(format(tm, "%H")),
                    steps = pmax(a$steps, 0), kcal = pmax(a$cal, 0) / 1000, dist = pmax(a$dist, 0),
                    spo = ifelse(a$spo > 0 & a$spo <= 100, a$spo, NA_real_), hr = fix_hr(a$hr),
                    rhr = ifelse(a$rhr > 0, a$rhr, NA_real_))
    m[order(m$t), ]
  })
  observeEvent(M(), updateDateRangeInput(session, "range", start = min(M()$day), end = max(M()$day),
                                         min = min(M()$day), max = max(M()$day)))
  hrmax <- reactive(switch(input$hrmax_m, tanaka = 208 - 0.7 * input$age, fox = 220 - input$age,
                           obs = qq(M()$hr, .999), man = input$hrmax_man))
  rhr <- reactive(if (isTRUE(input$rhr_man > 0)) input$rhr_man else qq(M()$hr, .05))
  
  # data-derived sedentary baseline: median HR of awake minutes without steps
  sed <- reactive({ m <- M(); md(m$hr[m$steps == 0 & is.na(in_sleep(m$t, SL()$ses))]) })
  SL <- reactive({
    a <- raw()$act; st <- a[a$src == 13, ]; un <- a[a$src == 10, ]     # source 10 = sleep with begin/end only (no stages)
    sleep_sessions(rbind(data.frame(start = st$ts, end = st$ots, code = st$kind, unst = rep(FALSE, nrow(st))),
                         data.frame(start = un$ts, end = un$ots, code = rep(-10, nrow(un)), unst = rep(TRUE, nrow(un)))),
                   as.numeric(input$c_light), as.numeric(input$c_deep), as.numeric(input$c_rem),
                   as.numeric(input$c_awake), input$gap, input$nap * 60, input$tz)
  })
  M2 <- reactive({
    m <- classify_steps(M(), input$cad, input$blen, sed(), input$art_margin)
    m$sidx <- in_sleep(m$t, SL()$ses); m$stage <- stage_of(m$t, SL()$seg)
    m$steps_m <- m$steps; if (input$steps_basis == "real") m$steps <- ifelse(m$cls == "artefact?", 0, m$steps)
    m
  })
  inr <- function(d) { req(input$range); d >= input$range[1] & d <= input$range[2] }
  Mr <- reactive({ m <- M2(); m[inr(m$day), ] })
  SLr <- reactive({ s <- SL()$ses; if (is.null(s)) return(NULL); s[inr(s$wake_date), ] })
  ST <- reactive({
    s <- raw()$stress; if (!nrow(s)) return(s)
    tm <- as.POSIXct(s$ts, origin = "1970-01-01", tz = input$tz)
    s$day <- as.Date(tm, tz = input$tz); s$hour <- as.integer(format(tm, "%H")); s[inr(s$day), ]
  })
  
  # sessions enriched with HR / SpO2 per stage
  SES <- reactive({
    s <- SLr(); if (is.null(s) || !nrow(s)) return(NULL)
    ses_all <- SL()$ses; m <- M2(); x <- m[!is.na(m$sidx), ]
    if (nrow(x)) {
      x$sid <- ses_all$sid[x$sidx]
      agg <- x %>% group_by(sid) %>% summarise(hr_med = md(hr), hr_p5 = qq(hr, .05), spo_mean = mn(spo),
                                               hr_deep = md(hr[stage %in% "Deep"]), hr_light = md(hr[stage %in% "Light"]), hr_rem = md(hr[stage %in% "REM"]), .groups = "drop")
      s <- left_join(s, agg, by = "sid")
    }
    for (cn in c("hr_med", "hr_p5", "spo_mean", "hr_deep", "hr_light", "hr_rem")) if (!cn %in% names(s)) s[[cn]] <- NA_real_
    s
  })
  
  # ---- daily table ---------------------------------------------------------------
  Dly <- reactive({
    m <- Mr(); hm <- hrmax(); h0 <- rhr(); req(nrow(m) > 0)
    d <- m %>% group_by(day) %>% summarise(
      n = n(), cov = 100 * sum(!is.na(hr)) / 1440,
      core = sum(steps_m[cls == "bout"]), arte = sum(steps_m[cls == "artefact?"]), steps_meas = sum(steps_m), kcal = sum(kcal),
      hr_p5 = qq(hr, .05), hr_med = md(hr), hr_max = mx(hr), rhr_w = md(rhr),
      hr_sleep = md(hr[!is.na(sidx)]), hr_awake = md(hr[is.na(sidx)]),
      mvpa = sum(hr >= .64 * hm, na.rm = TRUE), cad100 = sum(steps >= 100), cad130 = sum(steps >= 130),
      mod_min = sum(hr >= h0 + .4 * (hm - h0) & hr < h0 + .6 * (hm - h0), na.rm = TRUE), vig_min = sum(hr >= h0 + .6 * (hm - h0), na.rm = TRUE),
      spo_mean = mn(spo), spo_min = mx(-spo) * -1, steps = sum(steps), .groups = "drop")
    d$mem <- d$mod_min + 2 * d$vig_min
    d$partial <- d$n < 1200
    d <- left_join(d, sed_stats(m, input$wake0, input$wake1), by = "day")
    for (cn in c("sed_h", "sed_long", "sed60")) if (!cn %in% names(d)) d[[cn]] <- NA_real_
    wk <- Wm(); d$sport_min <- 0; d$trimp <- 0
    if (nrow(wk)) {
      a <- wk %>% group_by(day) %>% summarise(sm = sum(dur), tr = sum(trimp, na.rm = TRUE), .groups = "drop")
      i <- match(d$day, a$day); d$sport_min <- ifelse(is.na(i), 0, a$sm[i]); d$trimp <- ifelse(is.na(i), 0, a$tr[i])
    }
    s <- SES()
    if (!is.null(s)) {
      nt <- s[s$kind == "Night", ]
      if (nrow(nt)) d <- left_join(d, nt %>% group_by(wake_date) %>% summarise(
        asleep_h = sum(asleep) / 60, deep_pct = ifelse(sum(deep + light + rem) > 0, 100 * sum(deep) / sum(deep + light + rem), NA_real_), mid_h = mean(mid_min) / 60 + 18,
        hr_night = mn(hr_p5), eff = mean(eff), .groups = "drop"), by = c("day" = "wake_date"))
    }
    for (cn in c("asleep_h", "deep_pct", "eff", "mid_h", "hr_night")) if (!cn %in% names(d)) d[[cn]] <- NA_real_
    d$dip <- 100 * (1 - d$hr_sleep / d$hr_awake)
    d$rhr_day <- ifelse(is.na(d$rhr_w), d$hr_p5, d$rhr_w); d$vo2 <- 15.3 * hm / d$rhr_day
    st <- ST()
    if (nrow(st)) d <- left_join(d, st %>% group_by(day) %>% summarise(stress = mean(stress), stress_hi = 100 * mean(level >= 3), .groups = "drop"), by = "day")
    for (cn in c("stress", "stress_hi")) if (!cn %in% names(d)) d[[cn]] <- NA_real_
    as.data.frame(d)
  })
  
  # ---- empty state + insights ------------------------------------------------------
  output$empty <- renderUI(if (is.null(input$src)) div(class = "alert alert-info",
                                                       "Load your Gadgetbridge database (.db) or the export .zip in the sidebar. Optionally add the folder with your .fit workout files."))
  output$insights <- renderUI({
    d <- Dly(); full <- d[!d$partial, ]; if (!nrow(full)) full <- d
    n <- nrow(full); it <- character()
    tot <- sum(full$steps_meas); core <- sum(full$core); art <- sum(full$arte)
    it <- c(it, sprintf("<b>Steps</b>: %s/day (median %s) over %d full day(s). %.0f%% of steps sit in real walking bouts (>= %d steps/min for >= %d min), %.0f%% occur at low cadence with no heart-rate response (sedentary median %.0f bpm + %d) and are probably arm movement such as typing. A conservative figure is about <b>%s/day</b>.",
                        fmt(tot / n), fmt(median(full$steps_meas)), n, 100 * core / max(tot, 1), input$cad, input$blen, 100 * art / max(tot, 1), sed(), input$art_margin, fmt((tot - art) / n)))
    it <- c(it, sprintf("<b>Intensity</b>: %.0f min/day at >= 64%% of HRmax (%.0f bpm) = about <b>%.0f min/week</b> (WHO: 150-300 min of moderate activity). %.0f min/day at >= 100 steps/min.",
                        mean(full$mvpa), .64 * hrmax(), mean(full$mvpa) * 7, mean(full$cad100)))
    s <- SES()
    if (!is.null(s) && any(s$kind == "Night")) {
      nt <- s[s$kind == "Night", ]; nn <- nrow(nt)
      it <- c(it, sprintf("<b>Sleep</b>: %.1f h asleep on average over %d night(s); %d below 7 h (adult minimum from the AASM/SRS consensus). Deep share %.0f%% (PSG reference ~13-23%% - watch estimate, treat as trend only). Efficiency %.0f%%.%s",
                          mean(nt$asleep) / 60, nn, sum(nt$asleep < 420), 100 * sum(nt$deep) / max(sum(nt$deep + nt$light + nt$rem), 1), mean(nt$eff),
                          paste0(if (nn >= 3) sprintf(" Sleep-midpoint varies by %.0f min (SD) - regularity matters independently of duration.", sd(nt$mid_min)) else "", if (any(nt$unst > 0)) sprintf(" %d of %d night(s) have no stage detail (begin/end only).", sum(nt$unst > 0), nn) else "")))
      hs <- mean(d$hr_sleep, na.rm = TRUE); ha <- mean(d$hr_awake, na.rm = TRUE)
      if (is.finite(hs) && is.finite(ha)) it <- c(it, sprintf("<b>Heart</b>: sleeping HR median %.0f bpm vs %.0f bpm outside sleep (%.0f%% lower). Resting-HR proxy (5th percentile) %.0f bpm; watch-reported resting HR median %s. Trend over weeks is what counts, not a single day.",
                                                              hs, ha, 100 * (1 - hs / ha), rhr(), fmt(md(d$rhr_w))))
    } else it <- c(it, sprintf("<b>Heart</b>: resting-HR proxy (5th percentile of all minutes) %.0f bpm; watch-reported resting HR median %s. No sleep sessions found with the current stage mapping.", rhr(), fmt(md(d$rhr_w))))
    miss <- full$day[is.na(full$asleep_h)]
    if (length(miss)) it <- c(it, sprintf("<b>Sleep gaps</b>: no night detected for %s - the watch recorded no sleep stages there (not worn, or detection failed).", paste(format(miss, "%a %d %b"), collapse = ", ")))
    if (any(!is.na(d$stress))) it <- c(it, sprintf("<b>Stress</b>: mean score %.0f; %.0f%% of readings in the medium/high band.", mean(d$stress, na.rm = TRUE), mean(d$stress_hi, na.rm = TRUE)))
    if (any(!is.na(d$spo_mean))) it <- c(it, sprintf("<b>SpO2</b>: mean %.0f%%, lowest %.0f%% (%d readings). Wrist values below ~90%% need repeating before they mean anything.",
                                                     mean(Mr()$spo, na.rm = TRUE), min(Mr()$spo, na.rm = TRUE), sum(!is.na(Mr()$spo))))
    tags$ul(lapply(it, function(x) tags$li(HTML(x))))
  })
  
  # ---- overview plots ------------------------------------------------------------------
  output$tbl_daily <- renderTable({
    d <- Dly()
    data.frame(Day = paste0(format(d$day, "%a %d %b"), ifelse(d$partial, " (partial)", "")),
               Steps = fmt(d$steps), `Bout steps` = fmt(d$core), `Artefact?` = fmt(d$arte), `Active kcal` = fmt(d$kcal),
               `HR p5` = fmt(d$hr_p5), `HR med` = fmt(d$hr_med), `HR max` = fmt(d$hr_max), `Watch RHR` = fmt(d$rhr_w),
               `MVPA min` = fmt(d$mvpa), `Sleep h` = fmt(d$asleep_h, 1), `Deep %` = fmt(d$deep_pct), `Stress` = fmt(d$stress),
               `SpO2` = fmt(d$spo_mean), `HR cov %` = fmt(d$cov), check.names = FALSE)
  }, striped = TRUE, hover = TRUE, spacing = "xs", width = "100%")
  
  output$p_steps <- renderPlotly({
    th <- TH(); d <- Dly(); x <- as.character(d$day); real <- input$steps_basis == "real"
    p <- plot_ly(x = x) %>%
      add_bars(y = d$core, name = "Walking bouts", marker = list(color = th$pal[1])) %>%
      add_bars(y = d$steps_meas - d$core - d$arte, name = "Other movement", marker = list(color = th$pal[3]))
    if (!real) p <- p %>% add_bars(y = d$arte, name = "Likely artefact", marker = list(color = th$pal[4]))
    p %>% plotly::layout(barmode = "stack", shapes = list(hline(input$goal, th$fg))) %>% style_plot(th, NULL, if (real) "real steps" else "measured steps")
  })
  output$p_sleep <- renderPlotly({
    th <- TH(); s <- SES(); shiny::validate(shiny::need(!is.null(s), "No sleep sessions with the current mapping."))
    a <- s %>% group_by(wake_date) %>% summarise(deep = sum(deep) / 60, light = sum(light) / 60, rem = sum(rem) / 60, unst = sum(unst) / 60, .groups = "drop")
    plot_ly(x = as.character(a$wake_date)) %>%
      add_bars(y = a$deep, name = "Deep", marker = list(color = STAGE_COL[["Deep"]])) %>%
      add_bars(y = a$light, name = "Light", marker = list(color = STAGE_COL[["Light"]])) %>%
      add_bars(y = a$rem, name = "REM", marker = list(color = STAGE_COL[["REM"]])) %>%
      add_bars(y = a$unst, name = "Asleep, no stages", marker = list(color = STAGE_COL[["Unstaged"]])) %>%
      plotly::layout(barmode = "stack") %>% style_plot(th, "wake-up date", "hours asleep")
  })
  output$p_hrd <- renderPlotly({
    th <- TH(); d <- Dly(); x <- as.character(d$day)
    plot_ly(x = x) %>%
      add_lines(y = d$hr_max, name = "max", line = list(color = th$pal[4])) %>%
      add_lines(y = d$hr_med, name = "median", line = list(color = th$pal[1])) %>%
      add_lines(y = d$hr_p5, name = "5th pct (rest proxy)", line = list(color = th$pal[3])) %>%
      add_lines(y = d$hr_sleep, name = "median asleep", line = list(color = th$pal[5], dash = "dot")) %>%
      add_markers(y = d$rhr_w, name = "watch RHR", marker = list(color = th$pal[2])) %>% style_plot(th, NULL, "bpm")
  })
  output$p_mvpa <- renderPlotly({
    th <- TH(); d <- Dly(); x <- as.character(d$day)
    plot_ly(x = x) %>% add_bars(y = d$mvpa, name = "HR >= 64% HRmax", marker = list(color = th$pal[1])) %>%
      add_bars(y = d$cad100, name = "cadence >= 100/min", marker = list(color = th$pal[2])) %>%
      plotly::layout(barmode = "group", shapes = list(hline(150 / 7, th$fg))) %>% style_plot(th, NULL, "minutes / day")
  })
  
  # ---- heart ---------------------------------------------------------------------------
  output$p_hr <- renderPlotly({
    th <- TH(); m <- Mr(); req(nrow(m) > 0); m <- m[!is.na(m$hr), ]; req(nrow(m) > 0)
    span <- (max(m$t) - min(m$t)) / 86400
    res <- switch(input$hr_res, "1 min" = 1, "5 min" = 5, "15 min" = 15, "60 min" = 60,
                  if (span <= 2) 1 else if (span <= 14) 5 else 15)
    m$b <- floor(m$t / (res * 60)) * res * 60
    a <- m %>% group_by(b) %>% summarise(lo = min(hr), hi = max(hr), hr = median(hr), .groups = "drop")
    sh <- list(); s <- SLr()
    if (!is.null(s)) sh <- lapply(seq_len(nrow(s)), function(i) list(type = "rect", xref = "x", yref = "paper",
                                                                     x0 = ws(s$start[i], input$tz), x1 = ws(s$end[i], input$tz), y0 = 0, y1 = 1,
                                                                     fillcolor = "rgba(111,150,255,.16)", line = list(width = 0), layer = "below"))
    p <- plot_ly(x = ws(a$b, input$tz))
    if (res > 1) p <- p %>% add_ribbons(ymin = a$lo, ymax = a$hi, name = "min-max", line = list(width = 0),
                                        fillcolor = "rgba(95,179,246,.18)")
    p %>% add_lines(y = a$hr, name = "HR", line = list(color = th$pal[1], width = 1.2)) %>%
      plotly::layout(shapes = c(sh, list(hline(hrmax(), th$pal[4])))) %>% style_plot(th, NULL, "bpm")
  })
  output$p_zone <- renderPlotly({
    th <- TH(); m <- Mr(); m <- m[!is.na(m$hr), ]; req(nrow(m) > 0)
    m$z <- cut(m$hr / hrmax(), c(0, .5, .6, .7, .8, .9, Inf), labels = ZLAB, right = FALSE)
    a <- as.data.frame(table(day = as.character(m$day), zone = m$z), stringsAsFactors = FALSE)
    p <- plot_ly()
    for (i in seq_along(ZLAB)) { z <- a[a$zone == ZLAB[i], ]
    p <- p %>% add_bars(x = z$day, y = z$Freq, name = ZLAB[i], marker = list(color = ZCOL[i])) }
    p %>% plotly::layout(barmode = "stack") %>% style_plot(th, NULL, "minutes")
  })
  
  # ---- steps ---------------------------------------------------------------------------------
  output$p_cls <- renderPlotly({
    th <- TH(); m <- Mr(); req(nrow(m) > 0); m$b <- floor(m$t / 3600) * 3600
    a <- m %>% group_by(b) %>% summarise(core = sum(steps_m[cls == "bout"]), other = sum(steps_m[cls == "other"]), art = sum(steps_m[cls == "artefact?"]), .groups = "drop")
    plot_ly(x = ws(a$b, input$tz)) %>% add_bars(y = a$core, name = "Walking bouts", marker = list(color = th$pal[1])) %>%
      add_bars(y = a$other, name = "Other", marker = list(color = th$pal[3])) %>%
      add_bars(y = a$art, name = "Likely artefact", marker = list(color = th$pal[4])) %>%
      plotly::layout(barmode = "stack") %>% style_plot(th, NULL, "steps / hour")
  })
  output$p_cadmin <- renderPlotly({
    th <- TH(); d <- Dly(); x <- as.character(d$day)
    plot_ly(x = x) %>% add_bars(y = d$cad100 - d$cad130, name = "100-129 /min (moderate)", marker = list(color = th$pal[3])) %>%
      add_bars(y = d$cad130, name = ">= 130 /min (vigorous)", marker = list(color = th$pal[4])) %>%
      plotly::layout(barmode = "stack") %>% style_plot(th, NULL, "minutes")
  })
  output$p_heat <- renderPlotly({
    th <- TH(); m <- Mr(); req(nrow(m) > 0)
    h <- m %>% group_by(day, hour) %>% summarise(s = sum(steps), .groups = "drop")
    plot_ly(x = h$hour, y = as.character(h$day), z = h$s, type = "heatmap",
            colorscale = list(c(0, th$bg), c(1, th$pal[1]))) %>% style_plot(th, "hour of day", NULL) %>% plotly::layout(hovermode = "closest")
  })
  
  # ---- sleep ----------------------------------------------------------------------------------------
  observe({
    s <- SES(); if (is.null(s)) { updateSelectInput(session, "night", choices = character()); return() }
    s <- s[order(-s$start), ]
    lab <- paste0(format(as.POSIXct(s$start, origin = "1970-01-01", tz = input$tz), "%a %d %b %H:%M"), " -> ",
                  format(as.POSIXct(s$end, origin = "1970-01-01", tz = input$tz), "%H:%M"), "  (", sprintf("%.1f", s$asleep / 60), " h, ", s$kind, ")")
    updateSelectInput(session, "night", choices = setNames(s$sid, lab), selected = s$sid[1])
  })
  output$p_hypno <- renderPlotly({
    th <- TH(); req(input$night); sg <- SL()$seg; s <- SL()$ses
    ss <- s[s$sid == as.numeric(input$night), ]; req(nrow(ss) == 1)
    g <- sg[sg$start >= ss$start - 1 & sg$end <= ss$end + 1 & sg$sid == as.numeric(input$night), ]; req(nrow(g) > 0)
    lv <- c(Awake = 4, REM = 3, Light = 2, Deep = 1, Unstaged = 0); g$y <- lv[g$stage]
    p <- plot_ly()
    for (st in names(lv)) { z <- g[g$stage == st, ]; if (!nrow(z)) next
    p <- p %>% add_segments(x = ws(z$start, input$tz), xend = ws(z$end, input$tz), y = z$y, yend = z$y, name = st,
                            line = list(color = STAGE_COL[[st]], width = 16)) }
    m <- M2(); m <- m[m$t >= ss$start & m$t <= ss$end & !is.na(m$hr), ]
    if (nrow(m)) p <- p %>% add_lines(x = ws(m$t, input$tz), y = m$hr, yaxis = "y2", name = "HR", line = list(color = th$fg, width = 1), opacity = .7)
    p %>% plotly::layout(yaxis2 = list(overlaying = "y", side = "right", title = "bpm", showgrid = FALSE, color = th$fg)) %>%
      style_plot(th, NULL, NULL) %>% plotly::layout(yaxis = list(tickvals = unname(lv), ticktext = names(lv), range = c(-.6, 4.6), gridcolor = th$grid, tickfont = list(color = th$fg)))
  })
  output$p_reg <- renderPlotly({
    th <- TH(); s <- SES(); shiny::validate(shiny::need(!is.null(s) && any(s$kind == "Night"), "Needs at least one night."))
    s <- s[s$kind == "Night", ]
    clk <- function(t) { x <- as.POSIXct(t, origin = "1970-01-01", tz = input$tz)
    (as.numeric(format(x, "%H")) * 60 + as.numeric(format(x, "%M")) - 18 * 60) %% 1440 / 60 + 18 }
    hm <- function(t) format(as.POSIXct(t, origin = "1970-01-01", tz = input$tz), "%H:%M")
    tv <- seq(18, 42, by = 2); tt <- sprintf("%02d:00", tv %% 24)
    plot_ly(x = as.character(s$wake_date)) %>%
      add_markers(y = clk(s$start), name = "fell asleep", text = hm(s$start), hoverinfo = "text", marker = list(color = th$pal[5], size = 10)) %>%
      add_markers(y = clk(s$end), name = "woke up", text = hm(s$end), hoverinfo = "text", marker = list(color = th$pal[2], size = 10)) %>%
      style_plot(th, NULL, "time of day") %>% plotly::layout(yaxis = list(tickmode = "array", tickvals = tv, ticktext = tt))
  })
  output$p_sleephr <- renderPlotly({
    th <- TH(); d <- Dly(); x <- as.character(d$day)
    plot_ly(x = x) %>% add_bars(y = d$hr_awake, name = "outside sleep", marker = list(color = th$pal[2])) %>%
      add_bars(y = d$hr_sleep, name = "asleep", marker = list(color = th$pal[5])) %>% plotly::layout(barmode = "group") %>% style_plot(th, NULL, "median bpm")
  })
  output$tbl_ses <- renderTable({
    s <- SES(); shiny::validate(shiny::need(!is.null(s), "No sessions."))
    tz <- input$tz; f <- function(t) format(as.POSIXct(t, origin = "1970-01-01", tz = tz), "%a %d %b %H:%M"); stg <- s$deep + s$light + s$rem
    data.frame(Start = f(s$start), End = f(s$end), Type = s$kind, `Asleep h` = fmt(s$asleep / 60, 1), `Deep %` = fmt(ifelse(stg > 0, 100 * s$deep / stg, NA)),
               `Light %` = fmt(ifelse(stg > 0, 100 * s$light / stg, NA)), `REM %` = fmt(ifelse(stg > 0, 100 * s$rem / stg, NA)), `No-stage min` = fmt(s$unst),
               `Efficiency %` = fmt(s$eff), `HR med` = fmt(s$hr_med), `HR p5` = fmt(s$hr_p5), `SpO2` = fmt(s$spo_mean), check.names = FALSE)
  }, striped = TRUE, spacing = "xs", width = "100%")
  
  # ---- stress / SpO2 -----------------------------------------------------------------------------------
  output$p_stress <- renderPlotly({
    th <- TH(); s <- ST(); shiny::validate(shiny::need(nrow(s) > 0, "No stress samples in range."))
    cols <- c("#34d399", "#60a5fa", "#fbbf24", "#ef4444")
    plot_ly(x = ws(s$ts, input$tz), y = s$stress, type = "scatter", mode = "markers", color = factor(s$level),
            colors = cols[sort(unique(s$level))], marker = list(size = 6)) %>%
      plotly::layout(shapes = list(hline(30, th$grid), hline(60, th$grid), hline(80, th$grid))) %>% style_plot(th, NULL, "stress score")
  })
  output$p_spo <- renderPlotly({
    th <- TH(); m <- Mr(); m <- m[!is.na(m$spo), ]; shiny::validate(shiny::need(nrow(m) > 0, "No SpO2 samples in range."))
    plot_ly(x = ws(m$t, input$tz), y = m$spo, type = "scatter", mode = "markers", marker = list(color = th$pal[1], size = 6), name = "SpO2") %>%
      plotly::layout(shapes = list(hline(95, th$pal[3]), hline(90, th$pal[4]))) %>% style_plot(th, NULL, "%")
  })
  
  # ---- workouts -------------------------------------------------------------------------------------------
  sport_map <- reactive({
    l <- trimws(strsplit(input$sport_map %||% "", "\n")[[1]]); l <- l[grepl("=", l)]
    if (!length(l)) return(character())
    setNames(trimws(sub("^[^=]*=", "", l)), trimws(sub("=.*$", "", l)))
  })
  W <- reactive({
    r <- raw(); F <- fits(); w <- r$wk
    col <- function(n) if (n %in% names(w)) w[[n]] else rep(NA_real_, nrow(w))
    out <- data.frame(key = character(), start = numeric(), name = character(), sport = character(), dur = numeric(),
                      kcal = numeric(), dist = numeric(), ae = numeric(), ane = numeric(), rec = numeric(), db = numeric(), fit = integer(), stringsAsFactors = FALSE)
    if (nrow(w)) {
      s <- col("START_TIMESTAMP"); bi <- match(s, r$base$s)
      bn <- if (nrow(r$base)) r$base$name[bi] else rep(NA_character_, length(s))
      bk <- if (nrow(r$base)) r$base$kind[bi] else rep(NA_real_, length(s))
      out <- data.frame(key = paste0("db", col("WORKOUT_ID")), start = s,
                        name = ifelse(is.na(bn), paste("Workout", col("WORKOUT_ID")), bn), sport = sport_label(bk),
                        dur = col("TOTAL_TIME") / 60, kcal = col("CALORIES"), dist = col("DISTANCE") / 1000, ae = col("WORKOUT_AEROBIC_EFFECT") / 10,
                        ane = col("WORKOUT_ANAEROBIC_EFFECT") / 10, rec = col("RECOVERY_TIME") / 60, db = col("WORKOUT_ID"), fit = NA_integer_, stringsAsFactors = FALSE)
    }
    for (j in seq_along(F)) { f <- F[[j]]
    k <- which(abs(out$start - f$start) <= 180 & is.na(out$fit))
    if (length(k)) { k <- k[which.min(abs(out$start[k] - f$start))]; out$fit[k] <- j
    } else out <- rbind(out, data.frame(key = paste0("fit", j), start = f$start, name = f$name %||% "FIT workout", sport = f$sport,
                                        dur = f$dur / 60, kcal = f$kcal, dist = f$dist / 1000, ae = f$te, ane = NA_real_, rec = NA_real_, db = NA_real_, fit = j, stringsAsFactors = FALSE)) }
    mp <- sport_map(); if (length(mp) && nrow(out)) out$sport <- ifelse(out$sport %in% names(mp), unname(mp[out$sport]), out$sport)
    if (nrow(out)) { out$day <- as.Date(as.POSIXct(out$start, origin = "1970-01-01", tz = input$tz), tz = input$tz); out <- out[order(-out$start), ] }
    out
  })
  Wm <- reactive({
    w <- W(); if (!nrow(w)) return(w); w <- w[inr(w$day), ]; if (!nrow(w)) return(w)
    r <- raw(); F <- fits(); male <- input$sex == "m"; hm <- hrmax(); h0 <- rhr()
    m <- lapply(seq_len(nrow(w)), function(i) {
      s <- wk_series(w[i, ], F, r); k <- if (is.null(s)) NULL else hr_metrics(s$t, s$hr, hm, h0, male)
      if (is.null(k)) return(data.frame(avg_hr = NA_real_, max_hr = NA_real_, p99_hr = NA_real_, trimp = NA_real_, edwards = NA_real_,
                                        s3_1 = NA_real_, s3_2 = NA_real_, s3_3 = NA_real_, hr_src = NA_character_, stringsAsFactors = FALSE))
      data.frame(avg_hr = k$avg, max_hr = k$max, p99_hr = k$p99, trimp = k$trimp, edwards = k$edwards,
                 s3_1 = k$s3[1], s3_2 = k$s3[2], s3_3 = k$s3[3], hr_src = s$src[1], stringsAsFactors = FALSE)
    })
    w <- cbind(w, do.call(rbind, m))
    w$pct_avg <- 100 * w$avg_hr / hm; w$trimp_min <- w$trimp / w$dur
    w$hi_pct <- 100 * w$s3_3 / pmax(w$s3_1 + w$s3_2 + w$s3_3, 1e-9)
    ok <- !is.na(w$dist) & w$dist > 0.2 & w$dur > 0
    w$speed <- ifelse(ok, w$dist / (w$dur / 60), NA_real_)               # km/h
    w$pace <- ifelse(ok, w$dur / w$dist, NA_real_)                       # min/km
    w$mpb <- ifelse(ok & !is.na(w$avg_hr), w$dist * 1000 / (w$avg_hr * w$dur), NA_real_)  # metres per heartbeat
    w
  })
  observe({ w <- Wm(); if (!nrow(w)) { updateSelectInput(session, "wk_sel", choices = character()); return() }
  lab <- paste0(format(as.POSIXct(w$start, origin = "1970-01-01", tz = input$tz), "%a %d %b %H:%M"), "  ", w$sport, "  ", round(w$dur), " min")
  updateSelectInput(session, "wk_sel", choices = setNames(w$key, lab), selected = w$key[1]) })
  output$tbl_wk <- renderTable({
    w <- Wm(); shiny::validate(shiny::need(nrow(w) > 0, "No workouts in range."))
    data.frame(Start = format(as.POSIXct(w$start, origin = "1970-01-01", tz = input$tz), "%a %d %b %H:%M"), Name = w$name, Sport = w$sport,
               `Min` = fmt(w$dur), kcal = fmt(w$kcal), `km` = fmt(w$dist, 2), `Avg HR` = fmt(w$avg_hr), `Max HR` = fmt(w$max_hr),
               TRIMP = fmt(w$trimp), Edwards = fmt(w$edwards), `TE aer.` = fmt(w$ae, 1), `TE anaer.` = fmt(w$ane, 1), `Recovery h` = fmt(w$rec), `HR from` = w$hr_src, check.names = FALSE)
  }, striped = TRUE, spacing = "xs", width = "100%")
  sel <- reactive({ req(input$wk_sel); w <- Wm(); w <- w[w$key == input$wk_sel, ]; req(nrow(w) == 1); w })
  output$wk_info <- renderUI({
    w <- sel(); s <- wk_series(w, fits(), raw()); k <- if (!is.null(s)) hr_metrics(s$t, s$hr, hrmax(), rhr(), input$sex == "m")
    if (is.null(k)) return(tags$p("No heart-rate series for this workout."))
    tags$ul(tags$li(sprintf("Avg %.0f bpm (%.0f%% HRmax), max %.0f bpm (%.0f%% HRmax); source: %s", k$avg, 100 * k$avg / hrmax(), k$max, 100 * k$max / hrmax(), s$src[1])),
            tags$li(sprintf("Banister TRIMP %.0f, Edwards TRIMP %.0f; %.0f of %.0f min in zone 3 or higher", k$trimp, k$edwards, sum(k$zmin[4:6]), k$minutes)))
  })
  observe({
    w <- tryCatch(sel(), error = function(e) NULL); if (is.null(w)) return()
    s <- wk_series(w, fits(), raw()); av <- character()
    if (!is.null(s)) {
      if (any(!is.na(s$speed))) av <- c(av, "Speed (km/h)")
      if (any(!is.na(s$alt))) av <- c(av, "Elevation (m)")
      if (any(!is.na(s$cad))) av <- c(av, "Cadence")
    }
    keep <- intersect(isolate(input$wk_over), av); if (!length(keep) && "Speed (km/h)" %in% av) keep <- "Speed (km/h)"
    updateCheckboxGroupInput(session, "wk_over", choices = av, selected = keep, inline = TRUE)
  })
  output$p_wk <- renderPlotly({
    th <- TH(); w <- sel(); s <- wk_series(w, fits(), raw()); shiny::validate(shiny::need(!is.null(s) && sum(!is.na(s$hr)) > 1, "No heart-rate series for this workout."))
    tryCatch({
      s <- s[!is.na(s$t), ]; s <- s[order(s$t), ]; x <- (s$t - s$t[1]) / 60; zb <- c(.5, .6, .7, .8, .9, 1) * hrmax()
      sh <- lapply(1:5, function(i) list(type = "rect", xref = "paper", x0 = 0, x1 = 1, yref = "y", y0 = zb[i], y1 = zb[i + 1], fillcolor = ZRGBA[i], line = list(width = 0), layer = "below"))
      al <- s$alt - s$alt[which(!is.na(s$alt))[1]]
      defs <- list(`Speed (km/h)` = list(y = s$speed * 3.6, col = th$pal[3]), `Elevation (m)` = list(y = al, col = th$pal[2]), Cadence = list(y = s$cad, col = th$pal[5]))
      ov <- input$wk_over; if (is.null(ov)) ov <- character(); ov <- ov[ov %in% names(defs)]; ov <- ov[vapply(ov, function(o) any(!is.na(defs[[o]]$y)), logical(1))]
      dom <- 1 - 0.07 * length(ov)
      p <- plot_ly(x = x, y = s$hr, type = "scatter", mode = "lines", name = "HR (bpm)", connectgaps = TRUE, line = list(color = th$pal[1], width = 1.6))
      lay <- list(shapes = sh, xaxis = list(domain = c(0, dom)))
      for (k in seq_along(ov)) {
        d <- defs[[ov[k]]]; ax <- paste0("y", k + 1)
        p <- p %>% add_trace(x = x, y = d$y, type = "scatter", mode = "lines", name = ov[k], yaxis = ax, connectgaps = TRUE, line = list(color = d$col, width = 1.4))
        lay[[paste0("yaxis", k + 1)]] <- list(overlaying = "y", side = "right", anchor = "free", position = dom + 0.07 * (k - 1), title = ov[k], showgrid = FALSE, color = d$col)
      }
      p <- style_plot(p, th, "minutes", "bpm")
      do.call(plotly::layout, c(list(p), lay))
    }, error = function(e) shiny::validate(shiny::need(FALSE, paste("Could not draw this workout:", conditionMessage(e)))))
  })
  output$p_wkz <- renderPlotly({
    th <- TH(); w <- sel(); s <- wk_series(w, fits(), raw()); req(!is.null(s)); k <- hr_metrics(s$t, s$hr, hrmax(), rhr(), input$sex == "m"); req(!is.null(k))
    plot_ly(x = ZLAB, y = k$zmin, type = "bar", marker = list(color = ZCOL)) %>% style_plot(th, NULL, "minutes")
  })
  output$p_pace <- renderPlotly({
    th <- TH(); w <- sel(); shiny::validate(shiny::need(!is.na(w$db), "Pace splits come from the watch database.")); p <- raw()$pace
    p <- p[p$id == w$db & p$type == 0 & p$pace > 0, ]; shiny::validate(shiny::need(nrow(p) > 0, "No pace splits for this workout (no GPS)."))
    plot_ly(x = p$dist, y = p$pace / 60, type = "bar", marker = list(color = th$pal[2])) %>% style_plot(th, "km", "min / km") %>%
      plotly::layout(yaxis = list(autorange = "reversed", gridcolor = th$grid))
  })
  
  # ================= LONG-TERM TRENDS ==============================================
  Dt <- reactive({
    d <- Dly(); cc <- c("steps", "steps_meas", "mod_min", "vig_min", "mem", "core", "arte", "kcal", "mvpa", "cad100", "cad130", "sed_h", "sed_long", "sed60")
    d[d$partial, cc] <- NA_real_
    complete_days(d)
  })
  TL <- setNames(TREND$lab, TREND$id)
  output$tbl_trend <- renderTable({
    d <- Dt(); span <- as.numeric(max(d$day) - min(d$day)); last <- max(d$day)
    shiny::validate(shiny::need(span >= 9, "Need about 10 days of data for trend statistics - this table fills as your history grows."))
    do.call(rbind, lapply(seq_len(nrow(TREND)), function(i) {
      y <- d[[TREND$id[i]]]; ts <- trend_stats(d$day, y); v <- trend_verdict(ts, span, TREND$better[i])
      data.frame(Metric = TREND$lab[i], `Last 30 d` = fmt(mn(y[d$day > last - 30]), 1), `Prior 30 d` = fmt(mn(y[d$day > last - 60 & d$day <= last - 30]), 1),
                 `Trend / month` = fmt(ts$slope * 30, 2), p = ifelse(is.na(ts$p), "-", formatC(ts$p, format = "g", digits = 2)),
                 Days = ts$n, Verdict = vtxt(v), check.names = FALSE, stringsAsFactors = FALSE)
    }))
  }, striped = TRUE, spacing = "xs", width = "100%")
  output$p_trend <- renderPlotly({
    th <- TH(); d <- Dt(); id <- input$tr_metric; req(id %in% TREND$id); y <- d[[id]]; req(sum(!is.na(y)) > 2)
    k <- as.integer(sub(" .*", "", input$tr_win)); ts <- trend_stats(d$day, y)
    p <- plot_ly(x = as.character(d$day)) %>%
      add_markers(y = y, name = "daily", marker = list(color = th$pal[1], size = 5, opacity = .45)) %>%
      add_lines(y = roll_fun(y, k), name = paste0(k, "-day mean"), line = list(color = th$pal[2], width = 2.5))
    if (!is.na(ts$slope)) p <- p %>% add_lines(y = ts$icpt + ts$slope * as.numeric(d$day),
                                               name = sprintf("trend %+.2f / month (p=%.2g)", ts$slope * 30, ts$p), line = list(color = th$pal[4], dash = "dash"))
    style_plot(p, th, NULL, TL[[id]])
  })
  
  # ================= MONTHLY REVIEW =================================================
  observe({
    d <- Dly(); m <- sort(unique(format(d$day, "%Y-%m")), decreasing = TRUE)
    sel <- isolate(input$mon); if (is.null(sel) || !(sel %in% m)) sel <- m[1]
    updateSelectInput(session, "mon", choices = m, selected = sel)
  })
  MON <- reactive({
    d <- Dt(); req(input$mon); d$mon <- format(d$day, "%Y-%m"); mons <- sort(unique(d$mon)); i <- match(input$mon, mons); req(!is.na(i))
    cur <- d[d$mon == input$mon, ]
    prv <- if (i > 1) d[d$mon == mons[i - 1], ] else d[0, ]
    ref <- if (i > 1) d[d$mon %in% mons[max(1, i - 3):(i - 1)], ] else d[0, ]
    r <- lapply(seq_len(nrow(TREND)), function(k) {
      id <- TREND$id[k]; a <- mn(cur[[id]]); b <- mn(prv[[id]]); sdv <- sd(d[[id]], na.rm = TRUE)
      data.frame(id = id, lab = TREND$lab[k], better = TREND$better[k], cur = a, prev = b, ref = mn(ref[[id]]),
                 es = if (!is.na(a) && !is.na(b) && !is.na(sdv) && sdv > 0) (a - b) / sdv else NA_real_, stringsAsFactors = FALSE)
    })
    list(tab = do.call(rbind, r), n_days = sum(!is.na(cur$steps)), n_nights = sum(!is.na(cur$asleep_h)))
  })
  signal <- function(t) ifelse(is.na(t$es) | abs(t$es) < 0.2, "-", ifelse(t$better == "neutral", "changed",
                                                                          ifelse((t$better == "up") == (t$cur > t$prev), "better", "worse")))
  output$mon_text <- renderUI({
    r <- MON(); t <- r$tab; t$sig <- signal(t); big <- t[t$sig != "-", ]; big <- big[order(-abs(big$es)), ]; big <- head(big, 6)
    it <- sprintf("Tracked %d full day(s) and %d night(s) in %s.", r$n_days, r$n_nights, input$mon)
    if (all(is.na(t$es))) it <- c(it, "No previous month to compare with yet.")
    else if (!nrow(big)) it <- c(it, "No metric moved by more than the smallest worthwhile change (0.2 SD) versus last month.")
    else it <- c(it, vapply(seq_len(nrow(big)), function(i) sprintf("<b>%s</b>: %s vs %s last month - %s (%+.1f SD)",
                                                                    big$lab[i], fmt(big$cur[i], 1), fmt(big$prev[i], 1), big$sig[i], big$es[i]), character(1)))
    tags$ul(lapply(it, function(x) tags$li(HTML(x))))
  })
  output$tbl_mon <- renderTable({
    t <- MON()$tab
    data.frame(Metric = t$lab, `This month` = fmt(t$cur, 1), `Previous month` = fmt(t$prev, 1), Change = fmt(t$cur - t$prev, 1),
               `3-month avg before` = fmt(t$ref, 1), Signal = signal(t), check.names = FALSE)
  }, striped = TRUE, spacing = "xs", width = "100%")
  mon_plot <- function(id, ytitle, th, bars = TRUE) {
    d <- Dt(); req(input$mon); d$y <- d[[id]]; d$roll <- roll_fun(d$y, 7); d <- d[format(d$day, "%Y-%m") == input$mon, ]; req(nrow(d) > 0)
    p <- plot_ly(x = as.character(d$day))
    p <- if (bars) add_bars(p, y = d$y, name = ytitle, marker = list(color = th$pal[1])) else add_markers(p, y = d$y, name = ytitle, marker = list(color = th$pal[1]))
    p %>% add_lines(y = d$roll, name = "7-day mean", line = list(color = th$pal[2], width = 2)) %>% style_plot(th, NULL, ytitle)
  }
  output$p_mon_steps <- renderPlotly(mon_plot("steps", "steps", TH()))
  output$p_mon_sleep <- renderPlotly(mon_plot("asleep_h", "hours asleep", TH()))
  output$p_mon_rhr <- renderPlotly(mon_plot("hr_night", "bpm", TH(), FALSE))
  output$tbl_mon_wk <- renderTable({
    w <- Wm(); shiny::validate(shiny::need(nrow(w) > 0, "No workouts.")); req(input$mon); w <- w[format(w$day, "%Y-%m") == input$mon, ]
    shiny::validate(shiny::need(nrow(w) > 0, "No workouts this month."))
    data.frame(Start = format(as.POSIXct(w$start, origin = "1970-01-01", tz = input$tz), "%a %d %b %H:%M"), Sport = w$sport, Min = fmt(w$dur), kcal = fmt(w$kcal),
               `Avg HR` = fmt(w$avg_hr), `Peak HR` = fmt(w$p99_hr), TRIMP = fmt(w$trimp), check.names = FALSE)
  }, striped = TRUE, spacing = "xs", width = "100%")
  
  # ================= RESTING HR =====================================================
  RH <- reactive({
    d <- Dt(); y <- d[[input$rhr_src]]; req(sum(!is.na(y)) >= 3)
    n <- length(y); base <- rep(NA_real_, n); sdv <- base
    for (i in seq_len(n)) if (i > 1) {
      w <- y[max(1, i - 28):(i - 1)]; w <- w[!is.na(w)]
      if (length(w) >= 7) { base[i] <- median(w); sdv[i] <- max(mad(w), 1) }
    }
    z <- (y - base) / sdv; fl <- !is.na(z) & z > 2
    data.frame(day = d$day, y = y, base = base, sd = sdv, z = z, flag = fl, flag2 = fl & c(FALSE, fl[-n]), roll = roll_fun(y, 7, mean, 3))
  })
  output$p_rhr <- renderPlotly({
    th <- TH(); r <- RH(); x <- as.character(r$day); fl <- r$flag; ts <- trend_stats(r$day, r$y)
    p <- plot_ly(x = x)
    if (any(!is.na(r$base))) p <- p %>% add_ribbons(ymin = r$base - 2 * r$sd, ymax = r$base + 2 * r$sd, name = "personal range", line = list(width = 0), fillcolor = "rgba(95,179,246,.18)")
    p <- p %>% add_markers(y = r$y, name = "daily", marker = list(color = th$pal[1], size = 6, opacity = .6)) %>%
      add_lines(y = r$roll, name = "7-day mean", line = list(color = th$pal[2], width = 2.5))
    if (any(fl)) p <- p %>% add_markers(x = x[fl], y = r$y[fl], name = "above range", marker = list(color = th$pal[4], size = 10))
    if (!is.na(ts$slope)) p <- p %>% add_lines(x = x, y = ts$icpt + ts$slope * as.numeric(r$day), name = sprintf("trend %+.2f bpm / month", ts$slope * 30), line = list(color = th$pal[3], dash = "dash"))
    style_plot(p, th, NULL, "bpm")
  })
  output$p_rhrz <- renderPlotly({
    th <- TH(); r <- RH(); col <- ifelse(is.na(r$z), th$grid, ifelse(r$z > 2, th$pal[4], ifelse(r$z < -2, th$pal[1], th$pal[3])))
    plot_ly(x = as.character(r$day), y = r$z, type = "bar", marker = list(color = col), name = "z") %>%
      plotly::layout(shapes = list(hline(2, th$pal[4]), hline(-2, th$pal[1]))) %>% style_plot(th, NULL, "robust z-score")
  })
  output$rhr_text <- renderUI({
    r <- RH(); ts <- trend_stats(r$day, r$y); span <- as.numeric(max(r$day) - min(r$day)); it <- character()
    if (!is.na(ts$slope)) it <- c(it, sprintf("Trend: %+.2f bpm per month over %d days (Kendall p = %.2g) - %s.", ts$slope * 30, ts$n, ts$p, vtxt(trend_verdict(ts, span, "down"))))
    yy <- r$y[!is.na(r$y)]
    it <- c(it, sprintf("Last 7 days average %.1f bpm vs %.1f bpm median of the last 28 observations.", mn(tail(r$y, 7)), md(tail(yy, 28))))
    f2 <- which(r$flag2)
    it <- c(it, if (length(f2)) sprintf("Sustained elevation (2+ days above range) on: %s.", paste(format(tail(r$day[f2], 6), "%d %b"), collapse = ", ")) else "No sustained elevation above your personal range.")
    tags$ul(lapply(it, tags$li))
  })
  output$p_vo2 <- renderPlotly({
    th <- TH(); d <- Dt(); d$mon <- format(d$day, "%Y-%m"); a <- d %>% group_by(mon) %>% summarise(v = mn(vo2), .groups = "drop")
    plot_ly(x = a$mon, y = a$v, type = "scatter", mode = "lines+markers", line = list(color = th$pal[3]), marker = list(color = th$pal[3])) %>% style_plot(th, NULL, "ml/kg/min (estimate)")
  })
  
  # ================= SLEEP TRENDS ===================================================
  NI <- reactive({
    s <- SES(); shiny::validate(shiny::need(!is.null(s) && any(s$kind == "Night"), "No nights detected with the current stage mapping.")); nt <- s[s$kind == "Night", ]
    nt %>% group_by(day = wake_date) %>% summarise(deep = sum(deep) / 60, light = sum(light) / 60, rem = sum(rem) / 60, unst = sum(unst) / 60, awake = sum(window - asleep) / 60,
                                                   hr_med = mn(hr_med), hr_p5 = mn(hr_p5), hr_deep = mn(hr_deep), hr_light = mn(hr_light), hr_rem = mn(hr_rem),
                                                   eff = mean(eff), mid_min = mean(mid_min), asleep = sum(asleep) / 60, .groups = "drop")
  })
  output$p_sl_stack <- renderPlotly({
    th <- TH(); a <- agg_nights(NI(), input$sl_agg)
    plot_ly(x = a$x) %>% add_bars(y = a$deep, name = "Deep", marker = list(color = STAGE_COL[["Deep"]])) %>%
      add_bars(y = a$light, name = "Light", marker = list(color = STAGE_COL[["Light"]])) %>%
      add_bars(y = a$rem, name = "REM", marker = list(color = STAGE_COL[["REM"]])) %>%
      add_bars(y = a$unst, name = "Asleep, no stages", marker = list(color = STAGE_COL[["Unstaged"]])) %>%
      add_bars(y = a$awake, name = "Awake in bed", marker = list(color = STAGE_COL[["Awake"]])) %>%
      add_lines(y = a$hr_med, yaxis = "y2", name = "HR median", line = list(color = th$fg, width = 2)) %>%
      add_lines(y = a$hr_p5, yaxis = "y2", name = "HR nadir", line = list(color = th$pal[4], width = 2)) %>%
      plotly::layout(barmode = "stack", yaxis2 = list(overlaying = "y", side = "right", title = "bpm", showgrid = FALSE, color = th$fg)) %>% style_plot(th, NULL, "hours")
  })
  output$p_sl_stagehr <- renderPlotly({
    th <- TH(); a <- agg_nights(NI(), input$sl_agg)
    plot_ly(x = a$x) %>% add_lines(y = a$hr_deep, name = "Deep", line = list(color = STAGE_COL[["Deep"]], width = 2)) %>%
      add_lines(y = a$hr_light, name = "Light", line = list(color = STAGE_COL[["Light"]], width = 2)) %>%
      add_lines(y = a$hr_rem, name = "REM", line = list(color = STAGE_COL[["REM"]], width = 2)) %>% style_plot(th, NULL, "median HR (bpm)")
  })
  output$p_sl_comp <- renderPlotly({
    th <- TH(); a <- agg_nights(NI(), input$sl_agg); tot <- a$deep + a$light + a$rem + a$unst + a$awake
    plot_ly(x = a$x) %>% add_bars(y = 100 * a$deep / tot, name = "Deep", marker = list(color = STAGE_COL[["Deep"]])) %>%
      add_bars(y = 100 * a$light / tot, name = "Light", marker = list(color = STAGE_COL[["Light"]])) %>%
      add_bars(y = 100 * a$rem / tot, name = "REM", marker = list(color = STAGE_COL[["REM"]])) %>%
      add_bars(y = 100 * a$unst / tot, name = "Asleep, no stages", marker = list(color = STAGE_COL[["Unstaged"]])) %>%
      add_bars(y = 100 * a$awake / tot, name = "Awake in bed", marker = list(color = STAGE_COL[["Awake"]])) %>%
      plotly::layout(barmode = "stack") %>% style_plot(th, NULL, "% of time in bed")
  })
  SRI <- reactive({ m <- M2(); sri_daily(m$t, !is.na(m$stage) & m$stage != "Awake", input$tz) })
  output$p_sri <- renderPlotly({
    th <- TH(); r <- SRI(); shiny::validate(shiny::need(!is.null(r), "Needs at least 3 days of data with detected sleep.")); r <- r[inr(r$day), ]; shiny::validate(shiny::need(nrow(r) > 0, "No days in range."))
    plot_ly(x = as.character(r$day)) %>% add_markers(y = r$sri, name = "daily", marker = list(color = th$pal[1], size = 6, opacity = .5)) %>%
      add_lines(y = r$roll7, name = "7-day mean", line = list(color = th$pal[2], width = 2.5)) %>% style_plot(th, NULL, "SRI")
  })
  output$p_debt <- renderPlotly({
    th <- TH(); d <- Dt(); def <- input$need - d$asleep_h; shiny::validate(shiny::need(any(!is.na(def)), "No nights detected."))
    cum <- roll_fun(def, 14, function(z, na.rm = TRUE) sum(z, na.rm = na.rm), 7)
    plot_ly(x = as.character(d$day)) %>% add_bars(y = def, name = "short per night (h)", marker = list(color = th$pal[5])) %>%
      add_lines(y = cum, yaxis = "y2", name = "14-day sum (h)", line = list(color = th$pal[4], width = 2.5)) %>%
      plotly::layout(yaxis2 = list(overlaying = "y", side = "right", title = "14-day sum", showgrid = FALSE, color = th$fg)) %>% style_plot(th, NULL, "hours")
  })
  output$p_sjl <- renderPlotly({
    th <- TH(); s <- SES(); shiny::validate(shiny::need(!is.null(s) && any(s$kind == "Night"), "No nights detected.")); nt <- s[s$kind == "Night", ]
    nt$free <- as.integer(format(nt$wake_date, "%u")) %in% as.integer(input$free_days); nt$mon <- format(nt$wake_date, "%Y-%m")
    a <- nt %>% group_by(mon) %>% summarise(nf = sum(free), nw = sum(!free), msf = mn(mid_min[free]), msw = mn(mid_min[!free]), .groups = "drop")
    a$sjl <- ifelse(a$nf >= 2 & a$nw >= 3, a$msf - a$msw, NA_real_)
    shiny::validate(shiny::need(any(!is.na(a$sjl)), "Needs a month with >= 2 free-day and >= 3 work-day nights."))
    plot_ly(x = a$mon, y = a$sjl, type = "bar", marker = list(color = th$pal[2])) %>% style_plot(th, NULL, "minutes later on free days")
  })
  
  # ================= TRAINING =======================================================
  SPCOL <- reactive({ s <- sort(unique(Wm()$sport)); setNames(rep(c(TH()$pal, "#8899aa"), length.out = length(s)), s) })
  WS <- reactive({
    w <- Wm(); shiny::validate(shiny::need(nrow(w) > 0, "No workouts in the selected range."))
    w$period <- if (input$sp_agg == "Month") format(w$day, "%Y-%m-01") else as.character(as.Date(cut(w$day, "week"))); w
  })
  stack_by_sport <- function(a, x, y, cl, th, ytitle) {
    p <- plot_ly()
    for (s in names(cl)) { z <- a[a$sport == s, ]; if (nrow(z)) p <- p %>% add_bars(x = z[[x]], y = z[[y]], name = s, marker = list(color = cl[[s]])) }
    p %>% plotly::layout(barmode = "stack")
  }
  output$p_sp_time <- renderPlotly({
    th <- TH(); w <- WS(); a <- w %>% group_by(period, sport) %>% summarise(v = sum(dur), .groups = "drop")
    stack_by_sport(a, "period", "v", SPCOL(), th) %>% plotly::layout(shapes = if (input$sp_agg == "Week") list(hline(150, th$fg)) else list()) %>% style_plot(th, NULL, "minutes")
  })
  output$p_sp_comp <- renderPlotly({
    th <- TH(); w <- WS()
    a <- w %>% group_by(period, sport) %>% summarise(v = sum(dur), .groups = "drop") %>% group_by(period) %>% mutate(sh = 100 * v / sum(v)) %>% ungroup()
    stack_by_sport(a, "period", "sh", SPCOL(), th) %>% style_plot(th, NULL, "% of minutes")
  })
  output$p_sp_dow <- renderPlotly({
    th <- TH(); w <- WS(); w$wd <- as.integer(format(w$day, "%u"))
    w$y <- switch(input$sp_metric, Sessions = 1, Minutes = w$dur, `Load (TRIMP)` = ifelse(is.na(w$trimp), 0, w$trimp))
    a <- w %>% group_by(wd, sport) %>% summarise(v = sum(y), .groups = "drop")
    stack_by_sport(a, "wd", "v", SPCOL(), th) %>% style_plot(th, NULL, input$sp_metric) %>%
      plotly::layout(xaxis = list(tickmode = "array", tickvals = 1:7, ticktext = DAYS))
  })
  output$p_sp_hour <- renderPlotly({
    th <- TH(); w <- WS(); w$h <- as.integer(format(as.POSIXct(w$start, origin = "1970-01-01", tz = input$tz), "%H"))
    a <- w %>% group_by(h, sport) %>% summarise(v = n(), .groups = "drop")
    stack_by_sport(a, "h", "v", SPCOL(), th) %>% style_plot(th, "hour of day", "sessions")
  })
  LOAD <- reactive({
    w <- Wm(); shiny::validate(shiny::need(nrow(w) > 0, "No workouts.")); w <- w[!is.na(w$trimp), ]; shiny::validate(shiny::need(nrow(w) > 0, "No workouts with heart-rate data."))
    days <- seq(min(w$day), max(M()$day), by = "day"); ld <- tapply(w$trimp, as.character(w$day), sum); l <- as.numeric(ld[as.character(days)]); l[is.na(l)] <- 0
    ewma <- function(x, n) { lam <- 2 / (n + 1); out <- numeric(length(x)); prev <- x[1]
    for (i in seq_along(x)) { prev <- if (i == 1) x[1] else lam * x[i] + (1 - lam) * prev; out[i] <- prev }; out }
    data.frame(day = days, load = l, atl = ewma(l, 7), ctl = ewma(l, 28))
  })
  output$p_load <- renderPlotly({
    th <- TH(); l <- LOAD(); x <- as.character(l$day)
    plot_ly(x = x) %>% add_bars(y = l$load, name = "daily TRIMP", marker = list(color = th$grid)) %>%
      add_lines(y = l$ctl, name = "fitness (28 d EWMA)", line = list(color = th$pal[1], width = 2.5)) %>%
      add_lines(y = l$atl, name = "fatigue (7 d EWMA)", line = list(color = th$pal[4], width = 2)) %>%
      add_lines(y = l$ctl - l$atl, name = "form (fitness - fatigue)", line = list(color = th$pal[3], dash = "dot")) %>% style_plot(th, NULL, "TRIMP")
  })
  output$p_intens <- renderPlotly({
    th <- TH(); w <- WS(); a <- w %>% group_by(period) %>% summarise(a = sum(s3_1, na.rm = TRUE), b = sum(s3_2, na.rm = TRUE), c3 = sum(s3_3, na.rm = TRUE), .groups = "drop")
    tot <- pmax(a$a + a$b + a$c3, 1e-9)
    plot_ly(x = a$period) %>% add_bars(y = 100 * a$a / tot, name = "< 82 % HRmax", marker = list(color = "#34d399")) %>%
      add_bars(y = 100 * a$b / tot, name = "82-87 %", marker = list(color = "#fbbf24")) %>%
      add_bars(y = 100 * a$c3 / tot, name = "> 87 %", marker = list(color = "#ef4444")) %>% plotly::layout(barmode = "stack") %>% style_plot(th, NULL, "% of workout time")
  })
  output$tbl_wkly <- renderTable({
    w <- Wm(); shiny::validate(shiny::need(nrow(w) > 0, "No workouts.")); l <- LOAD(); w$wk <- as.Date(cut(w$day, "week"))
    a <- w %>% group_by(wk) %>% summarise(sessions = n(), hours = sum(dur) / 60, trimp = sum(trimp, na.rm = TRUE),
                                          hi = 100 * sum(s3_3, na.rm = TRUE) / max(sum(s3_1 + s3_2 + s3_3, na.rm = TRUE), 1e-9), .groups = "drop")
    mo <- vapply(a$wk, function(k) { x <- l$load[l$day >= k & l$day < k + 7]; if (length(x) < 7 || sd(x) == 0) NA_real_ else mean(x) / sd(x) }, numeric(1))
    ac <- vapply(a$wk, function(k) { i <- match(k + 6, l$day); if (!is.na(i) && i >= 28 && l$ctl[i] > 0) l$atl[i] / l$ctl[i] else NA_real_ }, numeric(1))
    a <- a[order(-as.numeric(a$wk)), ]; mo <- mo[order(-as.numeric(a$wk))]; ac <- ac[order(-as.numeric(a$wk))]
    data.frame(Week = format(a$wk, "%Y-%m-%d"), Sessions = a$sessions, Hours = fmt(a$hours, 1), TRIMP = fmt(a$trimp), Monotony = fmt(mo, 2),
               Strain = fmt(a$trimp * mo), `ACWR (EWMA)` = fmt(ac, 2), `% time > 87% HRmax` = fmt(a$hi), check.names = FALSE)
  }, striped = TRUE, spacing = "xs", width = "100%")
  
  # ---- trends within one sport (peak HR, speed, distance ...) ---------------------------
  PSM <- c(p99_hr = "Peak HR (99th pct, bpm)", max_hr = "Max HR (bpm)", avg_hr = "Average HR (bpm)", pct_avg = "Average % of HRmax", dur = "Duration (min)",
           kcal = "Energy (kcal)", trimp = "Load (TRIMP)", trimp_min = "Load per minute", hi_pct = "Time above 87 % HRmax (%)",
           dist = "Distance (km)", speed = "Average speed (km/h)", pace = "Pace (min/km)", mpb = "Metres per heartbeat (efficiency)")
  observe({
    s <- sort(unique(Wm()$sport)); sel <- isolate(input$ps_sport); if (is.null(sel) || !(sel %in% s)) sel <- s[1]
    updateSelectInput(session, "ps_sport", choices = s, selected = sel)
  })
  PS <- reactive({ req(input$ps_sport); w <- Wm(); w <- w[w$sport == input$ps_sport, ]; shiny::validate(shiny::need(nrow(w) > 0, "No sessions for this sport in range.")); w[order(w$start), ] })
  output$p_ps <- renderPlotly({
    th <- TH(); w <- PS(); id <- input$ps_metric; y <- w[[id]]; shiny::validate(shiny::need(sum(!is.na(y)) >= 2, "Not enough sessions with this metric (distance/speed need GPS or distance data)."))
    ts <- trend_stats(w$day, y); k <- as.integer(input$ps_win); x <- ws(w$start, input$tz)
    p <- plot_ly(x = x) %>% add_markers(y = y, name = "session", marker = list(color = th$pal[1], size = 8, opacity = .7)) %>%
      add_lines(y = roll_fun(y, k, mean, 1), name = paste0(k, "-session mean"), line = list(color = th$pal[2], width = 2.5))
    if (!is.na(ts$slope)) p <- p %>% add_lines(x = x, y = ts$icpt + ts$slope * as.numeric(w$day), name = sprintf("trend %+.2f / month (p=%.2g)", ts$slope * 30, ts$p), line = list(color = th$pal[4], dash = "dash"))
    style_plot(p, th, NULL, PSM[[id]])
  })
  output$tbl_ps <- renderTable({
    w <- PS(); span <- as.numeric(max(w$day) - min(w$day)); n <- nrow(w); third <- max(1, floor(n / 3))
    do.call(rbind, lapply(names(PSM), function(id) {
      y <- w[[id]]; if (sum(!is.na(y)) < 2) return(NULL); ts <- trend_stats(w$day, y)
      data.frame(Metric = PSM[[id]], Sessions = sum(!is.na(y)), `First third` = fmt(mn(head(y, third)), 1), `Last third` = fmt(mn(tail(y, third)), 1),
                 `Trend / month` = fmt(ts$slope * 30, 2), p = ifelse(is.na(ts$p), "-", formatC(ts$p, format = "g", digits = 2)),
                 Verdict = vtxt(trend_verdict(ts, span, "neutral")), check.names = FALSE, stringsAsFactors = FALSE)
    }))
  }, striped = TRUE, spacing = "xs", width = "100%")
  output$p_ps_month <- renderPlotly({
    th <- TH(); w <- PS(); id <- input$ps_metric; y <- w[[id]]; shiny::validate(shiny::need(sum(!is.na(y)) >= 2, "Not enough sessions."))
    plot_ly(x = format(w$day, "%Y-%m"), y = y, type = "box", boxpoints = "all", jitter = .4, marker = list(color = th$pal[1]), line = list(color = th$pal[1])) %>% style_plot(th, NULL, PSM[[id]])
  })
  output$p_ps_all <- renderPlotly({
    th <- TH(); w <- Wm(); id <- input$ps_metric; shiny::validate(shiny::need(nrow(w) > 0, "No workouts.")); w$y <- w[[id]]; w <- w[!is.na(w$y), ]; shiny::validate(shiny::need(nrow(w) > 0, "No data for this metric."))
    w$mon <- format(w$day, "%Y-%m"); a <- w %>% group_by(mon, sport) %>% summarise(v = mean(y), .groups = "drop"); cl <- SPCOL(); p <- plot_ly()
    for (s in names(cl)) { z <- a[a$sport == s, ]; if (nrow(z)) p <- p %>% add_lines(x = z$mon, y = z$v, name = s, line = list(color = cl[[s]], width = 2), marker = list(color = cl[[s]]), mode = "lines+markers") }
    style_plot(p, th, NULL, PSM[[id]])
  })
  
  # ================= RECOVERY & ADVANCED (HRV, skin temperature, sleep breathing, ECG) ============
  tsec <- function(x) ifelse(x > 1e11, x / 1000, x)
  nightly <- function(ts, v) {
    ts <- tsec(ts); tm <- as.POSIXct(ts, origin = "1970-01-01", tz = input$tz); h <- as.integer(format(tm, "%H")); k <- h >= 22 | h < 10
    if (sum(k) >= 3) { day <- as.Date(tm + 12 * 3600, tz = input$tz); ok <- k } else { day <- as.Date(tm, tz = input$tz); ok <- rep(TRUE, length(ts)) }
    a <- tapply(v[ok], day[ok], mean); data.frame(day = as.Date(names(a)), v = as.numeric(a))
  }
  in_rng <- function(day) if (isTRUE(input$demo)) rep(TRUE, length(day)) else inr(day)
  adv_in <- function(d) d[in_rng(d$day), ]
  ADV <- reactive({
    if (isTRUE(input$demo)) return(make_demo(max(M()$day), input$tz))
    r <- raw(); list(hrv = r$hrv, temp = r$temp, sstat = r$sstat, apnea = r$apnea, ecg = r$ecg)
  })
  HRVN <- reactive({ h <- ADV()$hrv; if (is.null(h) || !nrow(h)) NULL else complete_days(nightly(h$ts, h$v)) })
  TEMPN <- reactive({ t <- ADV()$temp; if (is.null(t) || !nrow(t)) NULL else complete_days(nightly(t$ts, t$t)) })
  SS <- reactive({
    s <- ADV()$sstat; shiny::validate(shiny::need(!is.null(s) && nrow(s) > 0, "No nightly sleep statistics yet - they appear here as soon as the watch records them."))
    col <- function(n) if (n %in% names(s)) s[[n]] else rep(NA_real_, nrow(s))
    data.frame(day = as.Date(as.POSIXct(tsec(col("TIMESTAMP")), origin = "1970-01-01", tz = input$tz), tz = input$tz), score = col("SLEEP_SCORE"), rdi = col("RDI"),
               breath = col("AVG_BREATH_RATE"), spo_min = col("MIN_OXYGEN_SATURATION"))
  })
  output$tbl_avail <- renderTable({
    cnt <- raw()$counts
    d <- data.frame(Metric = c("Heart rate, steps, basic sleep stages, stress score, SpO2 spot checks", "HRV (ms)", "Skin temperature", "Sleep apnea events",
                               "Nightly stats: score, RDI, breathing rate, SpO2 range", "ECG recordings", "Emotion / mood tracking", "Arterial stiffness / PPG rhythm screening"),
                    `Newer Huawei watches (e.g. Fit 5 Pro)` = c("yes", "yes", "yes", "yes (market dependent)", "yes", "yes (market dependent)", "yes", "yes (market dependent)"),
                    `Gadgetbridge table` = c("HUAWEI_ACTIVITY_SAMPLE", "HUAWEI_HRV_VALUE_SAMPLE", "HUAWEI_TEMPERATURE_SAMPLE", "HUAWEI_SLEEP_APNEA_SAMPLE",
                                             "HUAWEI_SLEEP_STATS_SAMPLE", "HUAWEI_ECG_SUMMARY_SAMPLE", "HUAWEI_EMOTIONS_SAMPLE", "-"), check.names = FALSE, stringsAsFactors = FALSE)
    d$`Rows in this export` <- fmt(vapply(d$`Gadgetbridge table`, function(t) if (t == "-") NA_real_ else { v <- cnt[t]; if (is.na(v)) 0 else as.numeric(v) }, numeric(1)))
    d
  }, striped = TRUE, spacing = "xs", width = "100%")
  zfun <- function(y, win, floor_) {
    n <- length(y); z <- rep(NA_real_, n)
    for (i in seq_len(n)) if (i > 1) { w <- y[max(1, i - win):(i - 1)]; w <- w[!is.na(w)]
    if (length(w) >= 7 && !is.na(y[i])) z[i] <- (y[i] - median(w)) / max(mad(w), floor_) }
    z
  }
  output$p_recov <- renderPlotly({
    th <- TH(); d <- Dt(); rows <- list()
    add <- function(lab, day, z) { k <- in_rng(day)
    if (any(!is.na(z[k]))) rows[[length(rows) + 1L]] <<- data.frame(metric = lab, day = as.character(day[k]), z = pmax(pmin(z[k], 3), -3), stringsAsFactors = FALSE) }
    y <- if (sum(!is.na(d$hr_night)) >= 7) d$hr_night else d$rhr_w
    add("Resting HR (lower = better)", d$day, -zfun(y, 28, 1))
    add("Sleep duration", d$day, zfun(d$asleep_h, 28, .5))
    h <- HRVN(); if (!is.null(h)) add("HRV (higher = better)", h$day, zfun(log(h$v), 60, .05))
    t <- TEMPN(); if (!is.null(t)) add("Skin temperature (stable = better)", t$day, -abs(zfun(t$v, 28, .1)))
    s <- tryCatch(SS(), error = function(e) NULL)
    if (!is.null(s)) { add("Sleep score", s$day, zfun(s$score, 28, 3)); add("Sleep apnea index RDI (lower = better)", s$day, -zfun(s$rdi, 28, .5)); add("Lowest SpO2 in sleep", s$day, zfun(s$spo_min, 28, 1)) }
    df <- if (length(rows)) do.call(rbind, rows) else NULL
    shiny::validate(shiny::need(!is.null(df) && nrow(df) > 0, "Needs about 8 nights of data for at least one metric."))
    plot_ly(df, x = ~day, y = ~metric, z = ~z, type = "heatmap", zmin = -3, zmax = 3, colorscale = list(c(0, "#ef4444"), c(.5, "rgba(127,127,127,.25)"), c(1, "#34d399")),
            colorbar = list(title = "vs baseline", tickfont = list(color = th$fg))) %>% style_plot(th, NULL, NULL) %>% plotly::layout(hovermode = "closest")
  })
  output$p_hrv <- renderPlotly({
    th <- TH(); h <- HRVN(); shiny::validate(shiny::need(!is.null(h), "No HRV values yet - they appear here as soon as the watch records them."))
    l <- log(h$v); base <- rep(NA_real_, nrow(h)); sdv <- base
    for (i in seq_len(nrow(h))) if (i > 1) { w <- l[max(1, i - 60):(i - 1)]; w <- w[!is.na(w)]; if (length(w) >= 14) { base[i] <- mean(w); sdv[i] <- sd(w) } }
    k <- in_rng(h$day); shiny::validate(shiny::need(any(k & !is.na(h$v)), "No HRV values in the selected date range."))
    x <- as.character(h$day[k]); p <- plot_ly(x = x)
    if (any(!is.na(base[k]))) p <- p %>% add_ribbons(ymin = exp(base[k] - .75 * sdv[k]), ymax = exp(base[k] + .75 * sdv[k]), name = "normal range", line = list(width = 0), fillcolor = "rgba(95,179,246,.18)")
    p %>% add_markers(y = h$v[k], name = "nightly", marker = list(color = th$pal[1], size = 6, opacity = .6)) %>%
      add_lines(y = exp(roll_fun(l, 7, mean, 3))[k], name = "7-day mean", line = list(color = th$pal[2], width = 2.5)) %>% style_plot(th, NULL, "HRV (ms)")
  })
  output$p_hrv_cv <- renderPlotly({
    th <- TH(); h <- HRVN(); shiny::validate(shiny::need(!is.null(h), "No HRV values yet - they appear here as soon as the watch records them."))
    cv <- roll_fun(h$v, 7, function(z, na.rm = TRUE) 100 * sd(z, na.rm = na.rm) / mean(z, na.rm = na.rm), 5); k <- in_rng(h$day)
    plot_ly(x = as.character(h$day[k]), y = cv[k], type = "scatter", mode = "lines", line = list(color = th$pal[5], width = 2)) %>% style_plot(th, NULL, "7-day CV of HRV (%)")
  })
  output$p_temp <- renderPlotly({
    th <- TH(); t <- TEMPN(); shiny::validate(shiny::need(!is.null(t), "No skin-temperature values yet - they appear here as soon as the watch records them."))
    y <- t$v; base <- rep(NA_real_, nrow(t)); for (i in seq_len(nrow(t))) if (i > 1) { w <- y[max(1, i - 28):(i - 1)]; w <- w[!is.na(w)]; if (length(w) >= 7) base[i] <- median(w) }
    dev <- (y - base)[in_rng(t$day)]; x <- as.character(t$day[in_rng(t$day)]); shiny::validate(shiny::need(length(x) > 0, "No temperature values in the selected date range."))
    plot_ly(x = x, y = dev, type = "bar", marker = list(color = ifelse(is.na(dev), th$grid, ifelse(dev > 0, th$pal[2], th$pal[1])))) %>% style_plot(th, NULL, "deg C vs baseline")
  })
  output$p_temp_abs <- renderPlotly({
    th <- TH(); t <- TEMPN(); shiny::validate(shiny::need(!is.null(t), "No skin-temperature values yet - they appear here as soon as the watch records them.")); k <- in_rng(t$day)
    plot_ly(x = as.character(t$day[k]), y = t$v[k], type = "scatter", mode = "lines+markers", line = list(color = th$pal[2]), marker = list(color = th$pal[2])) %>% style_plot(th, NULL, "deg C")
  })
  ss_plot <- function(id, ytitle, ref = NULL) {
    th <- TH(); s <- adv_in(SS()); y <- s[[id]]; shiny::validate(shiny::need(sum(!is.na(y)) > 0, "No values for this metric yet - they appear here as soon as the watch records them."))
    p <- plot_ly(x = as.character(s$day), y = y, type = "scatter", mode = "lines+markers", line = list(color = th$pal[1]), marker = list(color = th$pal[1]))
    if (!is.null(ref)) p <- p %>% plotly::layout(shapes = lapply(ref, hline, col = th$fg))
    style_plot(p, th, NULL, ytitle)
  }
  output$p_ss_score <- renderPlotly(ss_plot("score", "score"))
  output$p_ss_breath <- renderPlotly(ss_plot("breath", "breaths / min"))
  output$p_ss_rdi <- renderPlotly(ss_plot("rdi", "events / hour", c(5, 15)))
  output$p_ss_spo <- renderPlotly(ss_plot("spo_min", "%", c(90)))
  output$p_apnea <- renderPlotly({
    th <- TH(); a <- ADV()$apnea; shiny::validate(shiny::need(!is.null(a) && nrow(a) > 0, "No apnea events recorded yet."))
    day <- as.Date(as.POSIXct(tsec(a$ts) + 12 * 3600, origin = "1970-01-01", tz = input$tz), tz = input$tz)
    g <- data.frame(day = day) %>% group_by(day) %>% summarise(n = n(), .groups = "drop"); g <- adv_in(g)
    plot_ly(x = as.character(g$day), y = g$n, type = "bar", marker = list(color = th$pal[4])) %>% style_plot(th, NULL, "events / night")
  })
  output$tbl_ecg <- renderTable({
    e <- ADV()$ecg; shiny::validate(shiny::need(!is.null(e) && nrow(e) > 0, "No ECG recordings yet."))
    data.frame(Start = format(as.POSIXct(tsec(e$ts), origin = "1970-01-01", tz = input$tz), "%Y-%m-%d %H:%M"), `Avg HR` = e$hr, `Rhythm code` = e$a, check.names = FALSE)
  }, striped = TRUE, spacing = "xs")
  output$p_px_stress <- renderPlotly({
    th <- TH(); s <- ST(); ses <- SL()$ses; shiny::validate(shiny::need(nrow(s) > 0 && !is.null(ses), "No stress readings or sleep sessions."))
    i <- in_sleep(s$ts, ses); k <- !is.na(i); shiny::validate(shiny::need(any(k), "No stress readings fall inside detected sleep."))
    g <- data.frame(day = ses$wake_date[i[k]], v = s$stress[k]) %>% group_by(day) %>% summarise(v = mean(v), .groups = "drop")
    plot_ly(x = as.character(g$day), y = g$v, type = "scatter", mode = "lines+markers", line = list(color = th$pal[5]), marker = list(color = th$pal[5])) %>% style_plot(th, NULL, "mean stress during sleep")
  })
  
  # ================= HEALTH-APP HOME (added by merge_app.R) =================
last_full <- function(d) { x <- d$day[!d$partial]; if (length(x)) max(x) else max(d$day) }
home_day <- reactive({ d <- Dly(); x <- input$home_day; if (is.null(x) || !(x %in% d$day)) last_full(d) else x })
observe({ d <- Dly(); updateDateInput(session, "home_day", value = last_full(d), min = min(d$day), max = max(d$day)) })
observeEvent(input$day_prev, updateDateInput(session, "home_day", value = home_day() - 1))
observeEvent(input$day_next, updateDateInput(session, "home_day", value = home_day() + 1))
observeEvent(input$goto, nav_select("tab", input$goto))
hm <- function(h) if (is.na(h)) "-" else { m <- round(h * 60); sprintf("%d h %02d", m %/% 60, m %% 60) }
spark <- function(y, col, w = 220, h = 38) {
  y <- y[!is.na(y)]; if (length(y) < 2) return("")
  r <- range(y); s <- if (diff(r) == 0) 1 else diff(r)
  xs <- seq(2, w - 2, length.out = length(y)); ys <- h - 4 - (y - r[1]) / s * (h - 8)
  HTML(sprintf('<svg width="100%%" height="%d" viewBox="0 0 %d %d" preserveAspectRatio="none"><polyline fill="none" stroke="%s" stroke-width="2.4" stroke-linecap="round" stroke-linejoin="round" points="%s"/><circle cx="%.1f" cy="%.1f" r="3.5" fill="%s"/></svg>',
               h, w, h, col, paste(sprintf("%.1f,%.1f", xs, ys), collapse = " "), tail(xs, 1), tail(ys, 1), col))
}
ring_svg <- function(f, cols) {
  rs <- c(88, 68, 48); out <- ""
  for (i in 1:3) { c0 <- 2 * pi * rs[i]; fr <- if (is.na(f[i])) 0 else min(max(f[i], 0), 1)
  out <- paste0(out, sprintf('<circle cx="100" cy="100" r="%d" fill="none" stroke="%s" stroke-opacity=".18" stroke-width="15"/>', rs[i], cols[i]),
                if (fr > 0) sprintf('<circle cx="100" cy="100" r="%d" fill="none" stroke="%s" stroke-width="15" stroke-linecap="round" stroke-dasharray="%.1f %.1f" transform="rotate(-90 100 100)"/>', rs[i], cols[i], fr * c0, c0) else "") }
  HTML(sprintf('<svg viewBox="0 0 200 200" width="230" height="230">%s</svg>', out))
}
HOME <- reactive({
  d <- Dly(); day <- home_day(); i <- match(day, d$day); req(!is.na(i))
  list(r = d[i, ], w = d[max(1, i - 6):i, ], day = day, S = tryCatch(SC(), error = function(e) NULL))
})
tile <- function(goto, label, val, unit, sub, col, y, extra = NULL)
  tags$div(class = "tile", onclick = sprintf("Shiny.setInputValue('goto','%s',{priority:'event'})", goto),
           tags$div(class = "tile-h", style = paste0("color:", col), label),
           tags$div(tags$span(class = "tile-v", val), tags$span(class = "tile-u", unit)),
           tags$div(class = "tile-s", sub), extra, spark(y, col))
output$home_tiles <- renderUI({
  th <- TH(); H <- HOME(); r <- H$r; w <- H$w; p <- th$pal
  m <- Mr(); m <- m[m$day == H$day & !is.na(m$hr), ]
  rhr_v <- if (!is.na(r$rhr_w)) r$rhr_w else r$hr_p5
  s <- SES(); s <- if (is.null(s)) s else s[s$wake_date == H$day & s$kind == "Night", ]
  sbar <- NULL
  if (!is.null(s) && nrow(s)) { s <- s[which.max(s$asleep), ]; v <- c(Deep = s$deep, Light = s$light, REM = s$rem, Awake = s$window - s$asleep); v <- v / max(sum(v), 1) * 100
  sbar <- div(class = "sbar", lapply(names(v), function(n) div(style = sprintf("width:%.1f%%;background:%s", v[[n]], STAGE_COL[[n]])))) }
  rd <- if (is.null(H$S)) NA_real_ else H$S$readiness[match(H$day, H$S$day)]
  rd7 <- if (is.null(H$S)) NA_real_ else tail(H$S$readiness[H$S$day <= H$day], 7)
  div(class = "tiles",
      tile("Steps", "Steps", fmt(r$steps), "", sprintf("7-day average %s", fmt(mn(w$steps))), p[3], w$steps),
      tile("Heart", "Resting heart rate", fmt(rhr_v), "bpm",
           if (nrow(m)) sprintf("today %.0f - %.0f bpm", min(m$hr), max(m$hr)) else "no heart rate today", p[4], if (!is.na(r$rhr_w)) w$rhr_w else w$hr_p5),
      tile("Sleep", "Sleep", hm(r$asleep_h), "", sprintf("efficiency %s %%", fmt(r$eff)), p[5], w$asleep_h, sbar),
      tile("Heart", "Stress", fmt(r$stress), "", sprintf("%s %% of the day medium or high", fmt(r$stress_hi)), p[2], w$stress),
      tile("Workouts & training", "Active energy", fmt(r$kcal), "kcal", sprintf("%s active minutes", fmt(r$mem)), p[6], w$kcal),
      tile("Recovery & advanced", "Readiness", fmt(rd), "/ 100", "against your own normal", p[1], rd7))
})
output$p_home_hr <- renderPlotly({
  th <- TH(); m <- Mr(); m <- m[m$day == home_day() & !is.na(m$hr), ]; req(nrow(m) > 0)
  m$b <- floor(m$t / 300) * 300; a <- m %>% group_by(b) %>% summarise(lo = min(hr), hi = max(hr), hr = median(hr), .groups = "drop")
  plot_ly(x = ws(a$b, input$tz)) %>%
    add_ribbons(ymin = a$lo, ymax = a$hi, line = list(width = 0), fillcolor = "rgba(248,113,113,.18)", name = "range") %>%
    add_lines(y = a$hr, name = "heart rate", line = list(color = th$pal[4], width = 2.2, shape = "spline")) %>% style_plot(th, NULL, "bpm")
})
sleep_chart <- function(th, ss, g) {
  tz <- input$tz; g <- g[order(g$start), ]
  have <- intersect(c("Awake", "REM", "Light", "Deep", "Unstaged"), unique(g$stage))
  lab <- c(Awake = "Awake", REM = "REM", Light = "Light", Deep = "Deep", Unstaged = "Asleep (no stages)")[have]
  lv <- setNames(rev(seq_along(have)), have)
  clk <- function(t) format(as.POSIXct(t, origin = "1970-01-01", tz = tz), "%H:%M")
  p1 <- plot_ly() %>% add_lines(x = ws(c(rbind(g$start, g$end)), tz), y = unname(c(rbind(lv[g$stage], lv[g$stage]))),
                                line = list(color = "rgba(150,160,175,.5)", width = 1.5), hoverinfo = "none", showlegend = FALSE)
  for (st in have) { z <- g[g$stage == st, ]
  p1 <- p1 %>% add_segments(x = ws(z$start, tz), xend = ws(z$end, tz), y = lv[[st]], yend = lv[[st]], showlegend = FALSE,
                            line = list(color = STAGE_COL[[st]], width = 22), hoverinfo = "text",
                            text = sprintf("%s: %s - %s (%.0f min)", lab[[st]], clk(z$start), clk(z$end), z$dur)) }
  tot <- ss$deep + ss$light + ss$rem
  head <- sprintf("<b>%s asleep</b>  |  in bed %s  |  efficiency %.0f%%", hm(ss$asleep / 60), hm(ss$window / 60), ss$eff)
  sub <- if (tot > 0) sprintf("Deep %.0f%%  |  Light %.0f%%  |  REM %.0f%% of sleep", 100 * ss$deep / tot, 100 * ss$light / tot, 100 * ss$rem / tot) else
    "The watch recorded when you slept but no sleep stages for this night"
  ax0 <- list(gridcolor = th$grid, zeroline = FALSE, showline = FALSE, ticks = "")
  m <- M2(); m <- m[m$t >= ss$start & m$t <= ss$end & !is.na(m$hr), ]
  if (nrow(m) > 3) {
    p2 <- plot_ly(x = ws(m$t, tz), y = m$hr, type = "scatter", mode = "lines", fill = "tozeroy", fillcolor = "rgba(248,113,113,.12)",
                  line = list(color = th$pal[4], width = 1.6), hoverinfo = "y", showlegend = FALSE) %>%
      add_markers(x = ws(m$t[which.min(m$hr)], tz), y = min(m$hr), marker = list(color = th$pal[4], size = 9), hoverinfo = "text",
                  text = sprintf("lowest %.0f bpm", min(m$hr)), showlegend = FALSE)
    p <- plotly::subplot(p1, p2, nrows = 2, heights = c(.7, .3), shareX = TRUE, titleY = FALSE) %>%
      plotly::layout(yaxis2 = c(ax0, list(title = "bpm", range = c(min(m$hr) - 5, max(m$hr) + 5))),
                     xaxis2 = list(tickformat = "%H:%M", gridcolor = th$grid, zeroline = FALSE))
  } else p <- p1
  p %>% plotly::layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", font = list(color = th$fg, size = 12),
                       title = list(text = paste0(head, "<br><sup>", sub, "</sup>"), x = 0, xanchor = "left", font = list(size = 14)),
                       margin = list(t = 80, l = 120, r = 15, b = 40), hovermode = "closest", showlegend = FALSE,
                       xaxis = list(tickformat = "%H:%M", gridcolor = th$grid, zeroline = FALSE),
                       yaxis = c(ax0, list(tickvals = unname(lv), ticktext = unname(lab), range = c(.4, length(have) + .6)))) %>%
    plotly::config(displayModeBar = FALSE)
}
output$p_hypno <- renderPlotly({
  th <- TH(); req(input$night); s <- SL()$ses; sg <- SL()$seg
  ss <- s[s$sid == as.numeric(input$night), ]; req(nrow(ss) == 1); g <- sg[sg$sid == ss$sid, ]; req(nrow(g) > 0)
  sleep_chart(th, ss, g)
})
output$p_home_sleep <- renderPlotly({
  th <- TH(); s <- SL()$ses; sg <- SL()$seg; shiny::validate(shiny::need(!is.null(s), "No night recorded."))
  s <- s[s$wake_date == home_day() & s$kind == "Night", ]; shiny::validate(shiny::need(nrow(s) > 0, "No night recorded for this day."))
  s <- s[which.max(s$asleep), ]; g <- sg[sg$sid == s$sid, ]; req(nrow(g) > 0)
  sleep_chart(th, s, g)
})
output$p_hr_box <- renderPlotly({
  th <- TH(); D <- DASH(); m <- M2(); m <- m[m$day %in% D$cur$day & !is.na(m$hr), ]; req(nrow(m) > 0)
  m$g <- grp_days(m$day); m$state <- ifelse(is.na(m$sidx), "Awake", "Asleep"); cols <- c(Awake = th$pal[2], Asleep = th$pal[5]); p <- plot_ly()
  for (s in names(cols)) {
    z <- m[m$state == s, ]; if (!nrow(z)) next
    p <- p %>% add_trace(data = thin(z), x = ~g, y = ~hr, type = "box", boxpoints = FALSE, name = s, legendgroup = s,
                         marker = list(color = cols[[s]]), line = list(color = cols[[s]], width = 1.4))
    a <- z %>% group_by(g) %>% summarise(med = median(hr), d = min(day), .groups = "drop") %>% arrange(d)
    if (nrow(a) >= 3) {
      p <- p %>% add_lines(x = a$g, y = roll_fun(a$med, min(7, nrow(a)), mean, 2), name = paste(s, "median, rolling"), legendgroup = s,
                           line = list(color = cols[[s]], width = 3, shape = "spline"))
      ts <- trend_stats(a$d, a$med)
      if (!is.na(ts$slope)) p <- p %>% add_lines(x = a$g, y = ts$icpt + ts$slope * as.numeric(a$d), legendgroup = s,
                                                 name = sprintf("%s trend %+.1f bpm / month", s, ts$slope * 30), line = list(color = cols[[s]], dash = "dash", width = 2))
    }
  }
  p %>% plotly::layout(boxmode = "group") %>% style_plot(th, NULL, "heart rate (bpm)") %>% plotly::layout(xaxis = list(type = "category"), hovermode = "closest")
})
WKI <- c("#9ca3af", "#fde68a", "#fbbf24", "#f97316", "#ef4444", "#991b1b")
tl_cols <- function(light) c(if (light) "#e5e7eb" else "#1c222b", "#586272", "#a7f3d0", "#22c55e", "#f0a35e", "#6cb6ff", "#c792ea", "#2f5fd0", "#4f7cf0", WKI)
TLCODE <- c("No data", "Idle", "Light movement", "Walking", "Awake in sleep", "Light sleep", "REM sleep", "Deep sleep", "Asleep (no stages)",
            "Workout (no heart rate)", "Workout: very light (< 60% HRmax)", "Workout: light (60-70%)", "Workout: moderate (70-80%)", "Workout: hard (80-90%)", "Workout: maximal (90%+)")
TLH <- 560
build_Z <- function(days) {       # 288 five-minute cells x days, integer activity codes; highest code wins inside a cell
  tz <- input$tz; nd <- length(days); Z <- matrix(0, 288, nd); m <- M2(); m <- m[m$day %in% days, ]
  st <- as.numeric(as.POSIXct(as.character(days), tz = tz)); d <- NULL
  if (nrow(m)) {
    col <- match(m$day, days); mm <- pmin(pmax(round((m$t - st[col]) / 60), 0), 1439)
    code <- ifelse(m$cls == "bout", 3, ifelse(m$cls == "other", 2, 1))
    code <- ifelse(!is.na(m$stage), unname(c(Awake = 4, Light = 5, REM = 6, Deep = 7, Unstaged = 8)[m$stage]), code)
    d <- data.frame(b = mm %/% 5 + 1, col = col, code = code)
  }
  w <- Wm()
  if (nrow(w)) {
    bin <- ifelse(is.na(w$pct_avg), 0, 1 + findInterval(w$pct_avg, c(60, 70, 80, 90)))
    wd <- do.call(rbind, lapply(seq_len(nrow(w)), function(i) {
      t <- w$start[i] + 60 * (0:max(0, ceiling(w$dur[i]) - 1)); dy <- as.Date(as.POSIXct(t, origin = "1970-01-01", tz = tz), tz = tz)
      cl <- match(dy, days); ok <- !is.na(cl); if (!any(ok)) return(NULL)
      data.frame(b = pmin(pmax(round((t[ok] - st[cl[ok]]) / 60), 0), 1439) %/% 5 + 1, col = cl[ok], code = 9 + bin[i]) }))
    d <- rbind(d, wd)
  }
  if (is.null(d) || !nrow(d)) return(Z)
  d <- d[order(d$code), ]; Z[cbind(d$b, d$col)] <- d$code; Z
}
TLM <- reactive({
  m <- M2(); m <- m[inr(m$day), ]; req(nrow(m) > 0)
  n <- switch(input$tl_win %||% "Last 90 days", "Last 31 days" = 31, "Last 90 days" = 90, 1e5)
  last <- max(m$day); days <- seq(max(min(m$day), last - n + 1), last, by = "day")
  list(Z = build_Z(days), days = days)
})
output$tl_ui <- renderUI({
  T <- TLM(); nd <- length(T$days); cols <- tl_cols(input$theme == "Light"); hrs <- seq(0, 24, 3)
  labs <- lapply(hrs, function(h) tags$div(style = sprintf("position:absolute;right:6px;top:calc(46px + (%dpx - 72px) * %f - 8px);font-size:.72rem;opacity:.65", TLH, h / 24), sprintf("%02d:00", as.integer(h))))
  ln <- c("Idle" = 2, "Light movement" = 3, "Walking" = 4, "Light sleep" = 6, "Deep sleep" = 8, "REM" = 7, "Awake in sleep" = 5, "No data" = 1,
          "Workout < 60% HRmax" = 11, "60-70%" = 12, "70-80%" = 13, "80-90%" = 14, "90%+" = 15, "Workout, no HR" = 10)
  chips <- lapply(names(ln), function(n) tags$span(class = "tl-chip", tags$i(style = paste0("background:", cols[ln[[n]]])), n))
  tagList(
    tags$div(style = sprintf("display:flex;height:%dpx", TLH),
             tags$div(style = sprintf("position:relative;flex:none;width:52px;height:%dpx", TLH), labs),
             tags$div(id = "tl_scroll", style = "overflow-x:auto;flex:1;min-width:0;",
                      tags$div(style = sprintf("min-width:%dpx;", nd * 28), plotlyOutput("p_tl", width = "100%", height = sprintf("%dpx", TLH))))),
    tags$div(class = "d-flex flex-wrap gap-3 mt-2", chips),
    tags$script(HTML("setTimeout(function(){var e=document.getElementById('tl_scroll'); if(e){e.scrollLeft=e.scrollWidth;}}, 700);")))
})
output$p_tl <- renderPlotly({
  th <- TH(); T <- TLM(); nd <- length(T$days); cols <- tl_cols(input$theme == "Light"); n <- length(cols)
  cs <- unlist(lapply(seq_len(n), function(i) list(c((i - 1) / n, cols[i]), c(i / n, cols[i]))), recursive = FALSE)
  hh <- ((0:287) * 5) %/% 60; mi <- ((0:287) * 5) %% 60
  tx <- matrix(paste0(rep(format(T$days, "%a %d %b"), each = 288), "  ", sprintf("%02d:%02d", rep(hh, nd), rep(mi, nd)), "<br>", TLCODE[T$Z + 1]), 288, nd)
  d0 <- as.character(T$days); dd <- format(T$days, "%d"); pf <- T$days %in% perfect_days(); tt <- paste0(dd, "<br>", ifelse(pf, "<span style='color:#fbbf24'>\u2605</span>", substr(format(T$days, "%a"), 1, 2)))
  first <- which(dd == "01"); mk <- unique(c(1, first))
  shapes <- lapply(first[first > 1], function(i) list(type = "line", xref = "x", x0 = i - 1.5, x1 = i - 1.5, yref = "paper", y0 = 0, y1 = 1, line = list(color = th$fg, width = 1, dash = "dot")))
  ann <- lapply(mk, function(i) list(x = i - 1.4, y = 0, xref = "x", yref = "paper", xanchor = "left", yanchor = "top", yshift = -4, showarrow = FALSE,
                                     text = paste0("<b>", format(T$days[i], "%b %Y"), "</b>"), font = list(size = 11, color = th$fg)))
  plot_ly(source = "tl", x = d0, y = (0:287) * 5 / 60 + 2.5 / 60, z = T$Z, type = "heatmap", zmin = -.5, zmax = n - .5, colorscale = cs, showscale = FALSE,
          xgap = 3, ygap = 0, text = tx, hoverinfo = "text") %>%
    plotly::layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", font = list(color = th$fg, size = 11), dragmode = FALSE,
                   margin = list(l = 0, r = 0, t = 46, b = 26), shapes = shapes, annotations = ann,
                   xaxis = list(type = "category", side = "top", tickmode = "array", tickvals = d0, ticktext = tt, tickfont = list(size = 10), showgrid = FALSE, ticks = "", fixedrange = TRUE),
                   yaxis = list(range = c(24, 0), showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE, fixedrange = TRUE)) %>%
    plotly::config(displayModeBar = FALSE) %>% plotly::event_register("plotly_click")
})
output$p_tl_tot <- renderPlotly({       # daily totals behind the timeline (hidden in an accordion by default)
  th <- TH(); T <- TLM(); Z <- T$Z; cl <- tl_cols(input$theme == "Light"); hrs <- function(lo, hi) 5 * colSums(Z >= lo & Z <= hi) / 60
  p <- plot_ly(x = as.character(T$days))
  for (s in list(list("Sleep", 5, 8, cl[6]), list("Awake in sleep", 4, 4, cl[5]), list("Workout", 9, 14, cl[14]),
                 list("Walking", 3, 3, cl[4]), list("Light movement", 2, 2, cl[3]), list("Idle", 1, 1, cl[2])))
    p <- p %>% add_bars(y = hrs(s[[2]], s[[3]]), name = s[[1]], marker = list(color = s[[4]]))
  p %>% plotly::layout(barmode = "stack") %>% style_plot(th, NULL, "hours recorded")
})
MWK <- reactive({
  w <- Wm(); shiny::validate(shiny::need(nrow(w) > 0, "No workouts.")); req(input$mon); w <- w[format(w$day, "%Y-%m") == input$mon, ]
  shiny::validate(shiny::need(nrow(w) > 0, "No workouts this month."))
  w$lab <- paste0(format(as.POSIXct(w$start, origin = "1970-01-01", tz = input$tz), "%a %d %b %H:%M"), "<br>", w$sport, ", ", round(w$dur), " min"); w
})
output$p_mon_wk_hr <- renderPlotly({
  th <- TH(); w <- MWK()
  d <- rbind(data.frame(sport = w$sport, v = w$avg_hr, k = "Average HR", lab = w$lab), data.frame(sport = w$sport, v = w$p99_hr, k = "Peak HR", lab = w$lab))
  d <- d[!is.na(d$v), ]; shiny::validate(shiny::need(nrow(d) > 0, "No heart rate for these workouts."))
  plot_ly(d, x = ~sport, y = ~v, color = ~k, colors = c(`Average HR` = th$pal[1], `Peak HR` = th$pal[4]), type = "box", boxpoints = "all", jitter = .5, pointpos = 0,
          text = ~lab, hoverinfo = "text+y", marker = list(size = 8, opacity = .85)) %>%
    plotly::layout(boxmode = "group") %>% style_plot(th, NULL, "bpm") %>% plotly::layout(hovermode = "closest")
})
output$p_mon_wk_eff <- renderPlotly({
  th <- TH(); w <- MWK()
  mk <- function(v, nm, col) plot_ly(x = w$sport, y = v, type = "box", name = nm, boxpoints = "all", jitter = .5, pointpos = 0, text = w$lab, hoverinfo = "text+y",
                                     fillcolor = "rgba(127,127,127,.10)", line = list(color = col), marker = list(color = col, size = 8))
  ax <- function(t) list(title = t, gridcolor = th$grid, zeroline = FALSE)
  plotly::subplot(mk(w$dur, "minutes", th$pal[3]), mk(w$kcal, "kcal", th$pal[2]), mk(w$trimp, "TRIMP", th$pal[5]), nrows = 1, margin = 0.05, titleY = FALSE) %>%
    plotly::layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", font = list(color = th$fg), showlegend = FALSE, hovermode = "closest",
                   margin = list(t = 10, l = 55, r = 15, b = 40), yaxis = ax("minutes"), yaxis2 = ax("kcal"), yaxis3 = ax("TRIMP")) %>%
    plotly::config(displayModeBar = FALSE)
})
ring_svg2 <- function(f, cols, size = 230) {
  rs <- c(88, 68, 48); out <- ""
  for (i in 1:3) { c0 <- 2 * pi * rs[i]; fr <- if (is.na(f[i])) 0 else min(max(f[i], 0), 1)
  out <- paste0(out, sprintf('<circle cx="100" cy="100" r="%d" fill="none" stroke="%s" stroke-opacity=".18" stroke-width="15"/>', rs[i], cols[i]),
                if (fr > 0) sprintf('<circle cx="100" cy="100" r="%d" fill="none" stroke="%s" stroke-width="15" stroke-linecap="round" stroke-dasharray="%.1f %.1f" transform="rotate(-90 100 100)"/>', rs[i], cols[i], fr * c0, c0) else "") }
  HTML(sprintf('<svg viewBox="0 0 200 200" width="%d" height="%d">%s</svg>', size, size, out))
}
arc_svg <- function(frac, col, val, lab) {   # semicircle gauge like the Gadgetbridge app
  f <- if (is.na(frac)) 0 else min(max(frac, 0), .999); r <- 60; cx <- 80; cy <- 82
  pt <- function(a) sprintf("%.1f %.1f", cx - r * cos(a), cy - r * sin(a))
  fg <- if (f > 0.002) sprintf('<path d="M %s A %d %d 0 0 1 %s" fill="none" stroke="%s" stroke-width="14" stroke-linecap="round"/>', pt(0), r, r, pt(pi * f), col) else ""
  HTML(sprintf('<svg viewBox="0 0 160 112" width="100%%" style="max-width:210px"><path d="M %s A %d %d 0 0 1 %s" fill="none" stroke="%s" stroke-opacity=".18" stroke-width="14" stroke-linecap="round"/>%s<text x="80" y="80" text-anchor="middle" font-size="22" font-weight="700" fill="currentColor">%s</text><text x="80" y="102" text-anchor="middle" font-size="11" fill="currentColor" opacity=".65">%s</text></svg>',
               pt(0), r, r, pt(pi), col, fg, val, lab))
}
output$p_clock <- renderPlotly({            # the circular 24 h widget for the selected day
  th <- TH(); day <- home_day(); z <- build_Z(day)[, 1]; cl <- tl_cols(input$theme == "Light"); hh <- ((0:287) * 5) %/% 60; mi <- ((0:287) * 5) %% 60
  plot_ly(type = "barpolar", r = rep(1, 288), theta = (0:287) * 1.25 + .625, width = rep(1.25, 288), marker = list(color = cl[z + 1], line = list(width = 0)),
          text = sprintf("%02d:%02d  %s", hh, mi, TLCODE[z + 1]), hoverinfo = "text") %>%
    plotly::layout(paper_bgcolor = "rgba(0,0,0,0)", font = list(color = th$fg), showlegend = FALSE, margin = list(t = 40, b = 20, l = 40, r = 40),
                   title = list(text = format(day, "%A %d %B"), x = .5, font = list(size = 13)),
                   polar = list(bgcolor = "rgba(0,0,0,0)", hole = .55, radialaxis = list(visible = FALSE, range = c(0, 1)),
                                angularaxis = list(rotation = 90, direction = "clockwise", tickmode = "array", tickvals = seq(0, 315, 45),
                                                   ticktext = c("0", "3", "6", "9", "12", "15", "18", "21"), gridcolor = th$grid, linecolor = "rgba(0,0,0,0)"))) %>%
    plotly::config(displayModeBar = FALSE)
})
output$p_donut <- renderPlotly({            # sleep stages of the selected night as a ring
  th <- TH(); s <- SL()$ses; shiny::validate(shiny::need(!is.null(s), "No night recorded."))
  s <- s[s$wake_date == home_day() & s$kind == "Night", ]; shiny::validate(shiny::need(nrow(s) > 0, "No night recorded for this day.")); s <- s[which.max(s$asleep), ]
  v <- c(Deep = s$deep, Light = s$light, REM = s$rem, `Asleep (no stages)` = s$unst, Awake = s$window - s$asleep); v <- v[v > 0.5]
  cl <- c(Deep = STAGE_COL[["Deep"]], Light = STAGE_COL[["Light"]], REM = STAGE_COL[["REM"]], `Asleep (no stages)` = STAGE_COL[["Unstaged"]], Awake = STAGE_COL[["Awake"]])
  plot_ly(labels = names(v), values = as.numeric(v), type = "pie", hole = .68, sort = FALSE, direction = "clockwise", textinfo = "percent",
          marker = list(colors = unname(cl[names(v)]), line = list(color = th$bg, width = 2)), hovertemplate = "%{label}: %{value:.0f} min<extra></extra>") %>%
    plotly::layout(paper_bgcolor = "rgba(0,0,0,0)", font = list(color = th$fg), margin = list(t = 10, b = 10), legend = list(orientation = "h", x = .1, y = -.05),
                   annotations = list(list(text = sprintf("<b>%s</b><br>asleep", hm(s$asleep / 60)), showarrow = FALSE, x = .5, y = .5, xref = "paper", yref = "paper", font = list(size = 22, color = th$fg)))) %>%
    plotly::config(displayModeBar = FALSE)
})
output$p_wk_cal <- renderPlotly({           # training calendar: one cell per day, weeks left to right, colour = workout load
  th <- TH(); w <- Wm(); shiny::validate(shiny::need(nrow(w) > 0, "No workouts in range.")); d <- Dly()
  days <- seq(min(d$day), max(d$day), by = "day"); a <- tapply(w$trimp, as.character(w$day), sum, na.rm = TRUE)
  ld <- as.numeric(a[as.character(days)]); ld[is.na(ld)] <- 0; wk <- as.Date(cut(days, "week")); wd <- as.integer(format(days, "%u")); xs <- sort(unique(wk))
  z <- matrix(NA_real_, 7, length(xs)); z[cbind(wd, match(wk, xs))] <- ld
  tx <- matrix("", 7, length(xs)); tx[cbind(wd, match(wk, xs))] <- sprintf("%s<br>load %.0f", format(days, "%a %d %b"), ld)
  plot_ly(x = as.character(xs), y = DAYS, z = z, type = "heatmap", zmin = 0, zmax = max(ld, 1), xgap = 3, ygap = 3, showscale = FALSE, text = tx, hoverinfo = "text",
          colorscale = list(c(0, "rgba(127,127,127,.15)"), c(.0009, "rgba(127,127,127,.15)"), c(.001, "#fde68a"), c(.5, "#f97316"), c(1, "#991b1b"))) %>%
    style_plot(th, NULL, NULL) %>% plotly::layout(hovermode = "closest", yaxis = list(type = "category", categoryorder = "array", categoryarray = DAYS, autorange = "reversed"))
})
gb_agg <- function(day, y, how, f = mean) {
  if (how == "Day") return(data.frame(x = as.character(day), v = y, stringsAsFactors = FALSE))
  k <- if (how == "Week") as.character(as.Date(cut(day, "week"))) else format(day, "%Y-%m")
  a <- tapply(y, k, function(z) if (all(is.na(z))) NA_real_ else f(z, na.rm = TRUE)); data.frame(x = names(a), v = as.numeric(a), stringsAsFactors = FALSE)
}
gb_plot <- function(y, ytitle, col, type = "bar", colfun = NULL, goal = NULL) {
  th <- TH(); d <- Dt(); a <- gb_agg(d$day, y, input$gb_agg); a <- a[!is.na(a$v), ]
  shiny::validate(shiny::need(nrow(a) > 0, "No data for this metric in the selected range."))
  cl <- if (is.null(colfun)) col else colfun(a$v); p <- plot_ly(x = a$x)
  p <- if (type == "bar") add_bars(p, y = a$v, marker = list(color = cl)) else
    add_trace(p, y = a$v, type = "scatter", mode = "lines+markers", line = list(color = col, width = 2.5, shape = "spline"), marker = list(color = col, size = 6))
  if (!is.null(goal)) p <- p %>% plotly::layout(shapes = list(hline(goal, th$fg)))
  style_plot(p, th, NULL, ytitle) %>% plotly::layout(xaxis = list(type = "category"))
}
bmr_est <- function() 10 * input$gb_kg + 6.25 * input$gb_cm - 5 * input$age + (if (input$sex == "m") 5 else -161)
output$gb_dist <- renderPlotly(gb_plot(Dt()$steps * input$gb_stride / 1000, "km", TH()$pal[3]))
output$gb_active <- renderPlotly(gb_plot(Dt()$mem, "moderate-equivalent minutes", TH()$pal[2], goal = input$act_goal))
output$gb_stress <- renderPlotly(gb_plot(Dt()$stress, "stress score", TH()$pal[2],
                                         colfun = function(v) ifelse(v < 30, "#34d399", ifelse(v < 60, "#60a5fa", ifelse(v < 80, "#fbbf24", "#ef4444")))))
output$gb_kcal_tot <- renderPlotly({
  th <- TH(); d <- Dt(); A <- gb_agg(d$day, d$kcal, input$gb_agg); A <- A[!is.na(A$v), ]; shiny::validate(shiny::need(nrow(A) > 0, "No calorie data in range."))
  plot_ly(x = A$x) %>% add_bars(y = rep(bmr_est(), nrow(A)), name = "resting (estimated)", marker = list(color = th$pal[1])) %>%
    add_bars(y = A$v, name = "active (watch)", marker = list(color = th$pal[2])) %>%
    plotly::layout(barmode = "stack") %>% style_plot(th, NULL, "kcal") %>% plotly::layout(xaxis = list(type = "category"))
})
output$gb_kcal_act <- renderPlotly({
  th <- TH(); d <- Dt(); w <- Wm(); wk <- rep(0, nrow(d))
  if (nrow(w)) { a <- tapply(w$kcal, as.character(w$day), sum, na.rm = TRUE); wk <- as.numeric(a[as.character(d$day)]); wk[is.na(wk)] <- 0 }
  A <- gb_agg(d$day, d$kcal, input$gb_agg); B <- gb_agg(d$day, wk, input$gb_agg); shiny::validate(shiny::need(any(!is.na(A$v)), "No calorie data in range."))
  plot_ly(x = A$x) %>% add_bars(y = A$v, name = "active kcal (watch)", marker = list(color = th$pal[2])) %>%
    add_trace(x = B$x, y = B$v, type = "scatter", mode = "lines+markers", name = "workouts (summaries)", line = list(color = th$pal[4], width = 2), marker = list(color = th$pal[4])) %>%
    style_plot(th, NULL, "kcal") %>% plotly::layout(xaxis = list(type = "category"))
})
output$gb_spo <- renderPlotly({
  th <- TH(); m <- M2(); m <- m[inr(m$day) & !is.na(m$spo), ]; shiny::validate(shiny::need(nrow(m) > 0, "No SpO2 readings in range."))
  k <- switch(input$gb_agg, Day = as.character(m$day), Week = as.character(as.Date(cut(m$day, "week"))), Month = format(m$day, "%Y-%m"))
  a <- data.frame(k = k, spo = m$spo) %>% group_by(k) %>% summarise(lo = min(spo), hi = max(spo), avg = mean(spo), .groups = "drop")
  plot_ly(a) %>% add_segments(x = ~k, xend = ~k, y = ~lo, yend = ~hi, line = list(color = th$pal[1], width = 10), name = "range", hoverinfo = "none") %>%
    add_markers(x = ~k, y = ~avg, marker = list(color = th$pal[3], size = 8), name = "mean", text = ~sprintf("%.0f %% mean, %.0f-%.0f", avg, lo, hi), hoverinfo = "text") %>%
    style_plot(th, NULL, "SpO2 (%)") %>% plotly::layout(xaxis = list(type = "category"), hovermode = "closest")
})
VO2R <- reactive({
  w <- Wm(); w <- w[w$sport %in% c("Running", "Treadmill") & !is.na(w$speed) & !is.na(w$avg_hr) & w$dur >= 10, ]
  if (!nrow(w)) return(NULL); w <- w[order(w$start), ]
  hmx <- hrmax(); h0 <- rhr(); vo2 <- 0.2 * w$speed * 1000 / 60 + 3.5; frac <- (w$avg_hr - h0) / (hmx - h0)
  w$est <- ifelse(frac >= .6 & frac <= .95, (vo2 - 3.5) / frac + 3.5, NA_real_); w
})
output$gb_vo2 <- renderPlotly({
  th <- TH(); d <- Dt(); w <- VO2R(); p <- plot_ly()
  mo <- d %>% group_by(mon = format(day, "%Y-%m")) %>% summarise(u = mn(vo2), .groups = "drop"); mo <- mo[!is.na(mo$u), ]
  shiny::validate(shiny::need((!is.null(w) && any(!is.na(w$est))) || nrow(mo) > 0, "Needs heart rate data, or runs with distance and heart rate."))
  if (nrow(mo)) p <- p %>% add_trace(x = paste0(mo$mon, "-15"), y = mo$u, type = "scatter", mode = "lines+markers", name = "resting-HR proxy (monthly)", line = list(color = th$pal[5], width = 2.5), marker = list(color = th$pal[5]))
  if (!is.null(w) && any(!is.na(w$est))) {
    w <- w[!is.na(w$est), ]
    p <- p %>% add_markers(x = as.character(w$day), y = w$est, name = "run estimate", marker = list(color = th$pal[2], size = 9, opacity = .8),
                           text = sprintf("%s: %.0f ml/kg/min<br>%.1f km/h, %.0f bpm", format(w$day, "%d %b"), w$est, w$speed, w$avg_hr), hoverinfo = "text")
    if (nrow(w) >= 3) p <- p %>% add_lines(x = as.character(w$day), y = roll_fun(w$est, 3, mean, 1), name = "3-run mean", line = list(color = th$pal[2], width = 2.5))
    ts <- trend_stats(w$day, w$est)
    if (!is.na(ts$slope)) p <- p %>% add_lines(x = as.character(w$day), y = ts$icpt + ts$slope * as.numeric(w$day), name = sprintf("trend %+.1f / month", ts$slope * 30), line = list(color = th$pal[4], dash = "dash"))
  }
  style_plot(p, th, NULL, "ml/kg/min (estimate)") %>% plotly::layout(hovermode = "closest")
})
output$gb_tiles <- renderUI({
  th <- TH(); d <- Dt(); p <- th$pal; l14 <- function(x) tail(x, 14); dk <- d$steps * input$gb_stride / 1000
  spo <- M2(); spo <- spo[inr(spo$day) & !is.na(spo$spo), ]
  v <- tryCatch(VO2R(), error = function(e) NULL); vlast <- if (is.null(v) || !any(!is.na(v$est))) NA_real_ else tail(v$est[!is.na(v$est)], 1)
  div(class = "tiles",
      tile("Metrics", "Distance walked", fmt(mn(dk), 1), "km / day", "rough: steps x stride", p[3], l14(dk)),
      tile("Metrics", "Active time", fmt(mn(d$mem)), "min / day", sprintf("goal %d min", input$act_goal), p[2], l14(d$mem)),
      tile("Metrics", "Stress", fmt(mn(d$stress)), "", "mean score", p[6], l14(d$stress)),
      tile("Metrics", "SpO2", if (nrow(spo)) fmt(mean(spo$spo)) else "-", "%", if (nrow(spo)) sprintf("lowest %s %%", fmt(min(spo$spo))) else "no readings", p[1], NULL),
      tile("Metrics", "Energy, estimated", fmt(bmr_est() + mn(d$kcal)), "kcal / day", sprintf("resting %s + active %s", fmt(bmr_est()), fmt(mn(d$kcal))), p[4], l14(d$kcal)),
      tile("Metrics", "VO2max, own estimate", fmt(vlast, 1), "ml/kg/min", "from your runs, not the watch", p[5], NULL))
})
perfect_days <- reactive({
  d <- Dly(); ok <- !d$partial & !is.na(d$steps) & !is.na(d$mem) & !is.na(d$asleep_h) & d$steps >= input$goal & d$mem >= input$act_goal & d$asleep_h >= input$need
  d$day[ok]
})
star_pts <- function(cx = 100, cy = 100, ro = 26, ri = 11) {
  a <- seq(-pi / 2, by = pi / 5, length.out = 10); r <- rep(c(ro, ri), 5); paste(sprintf("%.1f,%.1f", cx + r * cos(a), cy + r * sin(a)), collapse = " ")
}
ring_svg3 <- function(f, cols, size = 74, perfect = FALSE) {
  s <- as.character(ring_svg2(f, cols, size))
  if (perfect) {
    extra <- sprintf('<polygon class="star" points="%s" fill="#fbbf24" stroke="#f59e0b" stroke-width="2" stroke-linejoin="round"/><circle class="tw" cx="76" cy="76" r="3" fill="#fde68a"/><circle class="tw tw2" cx="126" cy="80" r="2.5" fill="#fde68a"/><circle class="tw tw3" cx="118" cy="128" r="3" fill="#fde68a"/>', star_pts())
    s <- sub("</svg>", paste0(extra, "</svg>"), s, fixed = TRUE)
  }
  HTML(s)
}
output$home_rings <- renderUI({
  th <- TH(); d <- Dly(); n <- if (identical(input$ring_win, "Last 30 days")) 30 else 7
  d <- d[!d$partial & d$day <= home_day(), ]; d <- tail(d, n); req(nrow(d) > 0); cols <- c(th$pal[3], th$pal[2], th$pal[5])
  fl <- function(r) c(r$steps / input$goal, r$mem / input$act_goal, r$asleep_h / input$need)
  perfect <- vapply(seq_len(nrow(d)), function(i) { f <- fl(d[i, ]); all(!is.na(f) & f >= 1) }, logical(1))
  streak <- 0; for (j in rev(seq_along(perfect))) { if (perfect[j]) streak <- streak + 1 else break }
  cell <- function(i) { r <- d[i, ]
  div(class = paste("rcell", if (perfect[i]) "perfect", if (r$day == home_day()) "sel"),
      title = sprintf("%s: %s steps, %s active min, sleep %s%s - click to show this day", format(r$day, "%a %d %b"), fmt(r$steps), fmt(r$mem), hm(r$asleep_h), if (perfect[i]) " - all rings closed!" else ""),
      onclick = sprintf("Shiny.setInputValue('ring_day','%s',{priority:'event'})", format(r$day)),
      format(r$day, if (n <= 7) "%a" else "%d"), ring_svg3(fl(r), cols, 74, perfect[i])) }
  hit <- function(x, g) sum(!is.na(x) & x >= g)
  leg <- function(col, txt) tags$div(tags$span(class = "rdot", style = paste0("background:", col)), tags$span(txt))
  div(class = "d-flex flex-wrap gap-4 align-items-center p-2",
      div(class = "rgrid", lapply(seq_len(nrow(d)), cell)),
      div(class = "rleg", tags$h6(sprintf("%s - %s", format(min(d$day), "%d %b"), format(max(d$day), "%d %b"))),
          leg(cols[1], sprintf("steps goal reached on %d of %d days (average %s)", hit(d$steps, input$goal), nrow(d), fmt(mn(d$steps)))),
          leg(cols[2], sprintf("active-minutes goal on %d of %d days (average %s min)", hit(d$mem, input$act_goal), nrow(d), fmt(mn(d$mem)))),
          leg(cols[3], sprintf("sleep need met on %d of %d nights (average %s)", hit(d$asleep_h, input$need), nrow(d), hm(mn(d$asleep_h)))),
          if (any(perfect)) tags$div(class = "mt-2", tags$b(sprintf("\u2605 %d perfect day%s - all three rings closed", sum(perfect), if (sum(perfect) == 1) "" else "s")),
                                     if (streak >= 2) tags$span(sprintf("  |  current streak: %d%s", streak, if (streak >= 3) " \U0001F525" else ""))) else NULL))
})
observeEvent(input$ring_day, { d <- tryCatch(as.Date(input$ring_day), error = function(e) NA); if (!is.na(d)) updateDateInput(session, "home_day", value = d) })
observeEvent(event_data("plotly_click", source = "tl"), {
  cl <- event_data("plotly_click", source = "tl"); d <- tryCatch(as.Date(as.character(cl$x[1])), error = function(e) NA)
  if (!is.na(d)) { updateDateInput(session, "home_day", value = d)
    showNotification(sprintf("Showing %s: the tiles, heart rate and sleep on the Today tab now follow this day.", format(d, "%A %d %B")), duration = 4, type = "message") }
})
output$arcs_day <- renderUI({
  th <- TH(); r <- HOME()$r; rhr_v <- if (!is.na(r$rhr_w)) r$rhr_w else r$hr_p5
  go <- function(tab, x) div(style = "cursor:pointer;width:100%;display:flex;justify-content:center", title = paste("open:", tab),
                             onclick = sprintf("Shiny.setInputValue('goto','%s',{priority:'event'})", tab), x)
  div(class = "arcs", go("Metrics", arc_svg(r$steps / input$goal, th$pal[3], fmt(r$steps), "Steps")),
      go("Metrics", arc_svg(r$mem / input$act_goal, th$pal[2], sprintf("%s min", fmt(r$mem)), "Active time")),
      go("Sleep", arc_svg(r$asleep_h / input$need, th$pal[5], hm(r$asleep_h), "Sleep")),
      go("Heart", arc_svg((rhr_v - 40) / 60, th$pal[4], fmt(rhr_v), "Resting HR")))
})

  # ================= DASHBOARD ======================================================
  AHD <- reactive({
    m <- M2(); m <- m[m$hour >= input$wake0 & m$hour < input$wake1, ]
    h <- m %>% group_by(day, hour) %>% summarise(s = sum(steps), .groups = "drop"); h$act <- h$s >= 250; h
  })
  DASH <- reactive({
    d <- Dt(); req(nrow(d) > 0); last <- max(d$day[!is.na(d$steps) | !is.na(d$asleep_h)])
    n <- switch(input$dash_win, "Last 7 days" = 7, "Last 30 days" = 30, 1e5)
    ah <- AHD() %>% group_by(day) %>% summarise(act_h = sum(act), .groups = "drop")
    d$act_h <- ah$act_h[match(d$day, ah$day)]; d$act_h[is.na(d$steps)] <- NA_real_
    cur <- d[d$day > last - n & d$day <= last, ]
    prv <- if (n < 1e4) d[d$day > last - 2 * n & d$day <= last - n, ] else d[0, ]
    list(d = d, cur = cur, prv = prv, last = last, n = n)
  })
  output$dash_period <- renderUI({
    D <- DASH(); lo <- if (D$n < 1e4) D$last - D$n + 1 else min(D$d$day)
    tags$span(class = "text-muted small", sprintf("%s - %s | %d full day(s) | arrow = change vs the previous %s", format(lo, "%d %b"), format(D$last, "%d %b %Y"),
                                                  sum(!is.na(D$cur$steps)), if (D$n < 1e4) paste(D$n, "days") else "period (not available)"))
  })
  RGB <- list(g = "rgba(52,211,153,.55)", a = "rgba(251,191,36,.55)", r = "rgba(239,68,68,.55)", b = "rgba(96,165,250,.45)")
  mk_gauge <- function(title, v, rng, bands, thr = NULL, ref = NA_real_, suffix = "", better = "up", dig = 0) {
    th <- TH(); nodata <- is.na(v)
    if (nodata) { v <- rng[1]; title <- paste0(title, " (no data yet)") }
    gauge <- list(axis = list(range = rng, tickcolor = th$fg, tickfont = list(color = th$fg)), bar = list(color = th$pal[1], thickness = .28),
                  bgcolor = "rgba(0,0,0,0)", borderwidth = 0, steps = lapply(bands, function(b) list(range = c(b[[1]], b[[2]]), color = RGB[[b[[3]]]])))
    if (!is.null(thr)) gauge$threshold <- list(line = list(color = th$fg, width = 3), thickness = .85, value = thr)
    up <- if (better == "up") "#34d399" else if (better == "down") "#ef4444" else th$fg
    dn <- if (better == "up") "#ef4444" else if (better == "down") "#34d399" else th$fg
    hasref <- !nodata && !is.na(ref)
    plot_ly(type = "indicator", mode = if (nodata) "gauge" else if (hasref) "gauge+number+delta" else "gauge+number", value = v,
            number = list(suffix = suffix, valueformat = paste0(".", dig, "f"), font = list(size = 34, color = th$fg)),
            delta = list(reference = if (hasref) ref else v, valueformat = paste0(".", dig, "f"), increasing = list(color = up), decreasing = list(color = dn)),
            title = list(text = title, font = list(size = 15, color = th$fg)), gauge = gauge) %>%
      plotly::layout(paper_bgcolor = "rgba(0,0,0,0)", margin = list(t = 70, b = 15, l = 30, r = 30), font = list(color = th$fg)) %>% plotly::config(displaylogo = FALSE)
  }
  output$g_steps <- renderPlotly({ D <- DASH()
  mk_gauge(if (input$steps_basis == "real") "Steps / day (real)" else "Steps / day (measured)", mn(D$cur$steps), c(0, 15000),
           list(list(0, 5000, "r"), list(5000, 8000, "a"), list(8000, 15000, "g")), thr = input$goal, ref = mn(D$prv$steps)) })
  output$g_who <- renderPlotly({ D <- DASH()
  mk_gauge("Moderate+ activity, min/week (WHO)", mn(D$cur$mvpa) * 7, c(0, 450), list(list(0, 150, "a"), list(150, 300, "g"), list(300, 450, "b")), thr = 150, ref = mn(D$prv$mvpa) * 7, suffix = " min") })
  output$g_hours <- renderPlotly({ D <- DASH(); top <- max(12, input$wake1 - input$wake0)
  mk_gauge("Active hours / day (250+ steps)", mn(D$cur$act_h), c(0, top), list(list(0, 6, "r"), list(6, 9, "a"), list(9, top, "g")), thr = 9, ref = mn(D$prv$act_h), dig = 1) })
  output$g_sleep <- renderPlotly({ D <- DASH()
  mk_gauge("Sleep (h / night)", mn(D$cur$asleep_h), c(0, 12), list(list(0, 6, "r"), list(6, 7, "a"), list(7, 9, "g"), list(9, 12, "a")), thr = 7, ref = mn(D$prv$asleep_h), suffix = " h", dig = 1) })
  output$g_rhr <- renderPlotly({
    D <- DASH(); d <- D$d; y <- if (sum(!is.na(d$hr_night)) >= 7) d$hr_night else d$rhr_w
    cur <- mn(y[d$day %in% D$cur$day]); hist <- y[d$day <= D$last - D$n]; hist <- hist[!is.na(hist)]; allv <- y[!is.na(y)]
    if (is.na(cur) || length(allv) < 3) return(mk_gauge("Resting HR", NA_real_, c(40, 70), list()))
    base <- if (length(hist) >= 7) median(hist) else median(allv); s <- max(mad(if (length(hist) >= 7) hist else allv), if (length(hist) >= 7) 1 else 1.5)
    lo <- floor(min(base - 8, cur - 2)); hi <- ceiling(max(base + 8, cur + 2, base + 2 * s + 2))
    mk_gauge("Resting HR vs your baseline", cur, c(lo, hi), list(list(lo, base + s, "g"), list(base + s, base + 2 * s, "a"), list(base + 2 * s, hi, "r")),
             thr = base, ref = mn(y[d$day %in% D$prv$day]), suffix = " bpm", better = "down", dig = 1)
  })
  output$g_hrv <- renderPlotly({
    D <- DASH(); h <- HRVN(); if (is.null(h)) return(mk_gauge("HRV", NA_real_, c(0, 100), list()))
    w <- function(a, b) mn(h$v[h$day > a & h$day <= b]); cur <- w(D$last - D$n, D$last); prv <- if (D$n < 1e4) w(D$last - 2 * D$n, D$last - D$n) else NA_real_
    hist <- log(h$v[h$day <= D$last - D$n & !is.na(h$v)]); if (length(hist) < 14) hist <- log(h$v[!is.na(h$v)])
    if (is.na(cur) || length(hist) < 3) return(mk_gauge("HRV", NA_real_, c(0, 100), list()))
    mu <- exp(mean(hist)); sg <- sd(hist); top <- ceiling(max(2 * mu, cur * 1.2))
    bands <- if (length(hist) >= 14) list(list(0, mu * exp(-1.5 * sg), "r"), list(mu * exp(-1.5 * sg), mu * exp(-.75 * sg), "a"), list(mu * exp(-.75 * sg), top, "g")) else list()
    mk_gauge("HRV vs your range (ms)", cur, c(0, top), bands, thr = mu, ref = prv, suffix = " ms")
  })
  output$p_dash_hours <- renderPlotly({
    th <- TH(); D <- DASH(); a <- AHD(); a <- a[a$day %in% D$cur$day[!is.na(D$cur$steps)], ]; shiny::validate(shiny::need(nrow(a) > 0, "No full days in this window."))
    g <- a %>% group_by(hour) %>% summarise(pct = 100 * mean(act), st = mean(s), .groups = "drop")
    plot_ly(x = g$hour, y = g$pct, type = "bar", marker = list(color = ifelse(g$pct >= 50, th$pal[3], th$pal[1])), hoverinfo = "text",
            text = sprintf("%02d:00 - active on %.0f%% of days, %.0f steps on average", g$hour, g$pct, g$st)) %>%
      style_plot(th, "hour of day", "% of days active") %>% plotly::layout(xaxis = list(tickmode = "linear", dtick = 1), yaxis = list(range = c(0, 100)))
  })
  # how you compare: your daily values (violin + box + points) against population or guideline bands
  CMP <- reactive({
    d <- Dt(); req(nrow(d) > 0); ah <- AHD() %>% group_by(day) %>% summarise(v = sum(act), .groups = "drop"); h <- HRVN()
    list(steps = d$steps, mvpa = roll_fun(d$mvpa, 7, function(z, na.rm = TRUE) 7 * mean(z, na.rm = na.rm), 6), hours = ah$v[ah$day %in% d$day[!is.na(d$steps)]],
         sleep = d$asleep_h, eff = d$eff, deep = d$deep_pct, rhr = d$rhr_w, hrv = if (is.null(h)) numeric() else h$v)
  })
  cmp_plot <- function(id) {
    th <- TH(); ref <- ref_defs(input$age, input$sex)[[id]]; v <- CMP()[[id]]; v <- v[!is.na(v)]
    shiny::validate(shiny::need(length(v) >= 2, "Not enough data yet - this fills in as your history grows."))
    shapes <- list(); ann <- list(); k <- 0
    for (b in ref$bands) { k <- k + 1
    shapes[[length(shapes) + 1]] <- list(type = "rect", xref = "x", yref = "paper", x0 = b$lo, x1 = b$hi, y0 = 0, y1 = 1, fillcolor = b$col, line = list(width = 0), layer = "below")
    ann[[length(ann) + 1]] <- list(x = (b$lo + b$hi) / 2, y = 1 - 0.1 * (k - 1), xref = "x", yref = "paper", text = b$txt, showarrow = FALSE, yanchor = "top", font = list(size = 11, color = th$fg)) }
    for (l in ref$lines) {
      shapes[[length(shapes) + 1]] <- list(type = "line", xref = "x", yref = "paper", x0 = l$x, x1 = l$x, y0 = 0, y1 = 1, line = list(color = th$fg, dash = "dot", width = 1))
      ann[[length(ann) + 1]] <- list(x = l$x, y = 0.02, xref = "x", yref = "paper", text = l$txt, showarrow = FALSE, yanchor = "bottom", font = list(size = 11, color = th$fg)) }
    xs <- c(v, unlist(lapply(ref$bands, function(b) c(b$lo, b$hi))), unlist(lapply(ref$lines, function(l) l$x))); pad <- 0.04 * diff(range(xs))
    plot_ly(x = v, y = rep("you", length(v)), type = "violin", orientation = "h", name = "you", box = list(visible = TRUE), meanline = list(visible = TRUE), points = "all", jitter = .4, pointpos = 0,
            marker = list(color = th$pal[1], opacity = .6, size = 5), line = list(color = th$pal[1], width = 1), fillcolor = "rgba(95,179,246,.22)", hoverinfo = "x") %>%
      style_plot(th, ref$unit, NULL) %>% plotly::layout(shapes = shapes, annotations = ann, showlegend = FALSE, yaxis = list(showticklabels = FALSE), xaxis = list(range = c(min(xs) - pad, max(xs) + pad)))
  }
  for (.id in c("steps", "mvpa", "hours", "sleep", "eff", "deep", "rhr", "hrv")) local({ i <- .id; output[[paste0("c_", i)]] <- renderPlotly(cmp_plot(i)) })
  output$tbl_refs <- renderTable(data.frame(
    Metric = c("Steps per day", "Moderate+ minutes", "Active hours", "Sleep duration", "Sleep efficiency", "Deep sleep share", "Resting HR", "HRV (RMSSD)"),
    `Reference shown` = c("US adults 10th-90th percentile (men 2,250-12,330; women 1,750-9,820); mean 6,564; green = benefit plateau 8-10k (under 60) or 6-8k (60+)", "150-300 min/week (WHO 2020)",
                          "9+ active hours (250+ steps/hour)", "7-9 h (under 65), 7-8 h (65+)", "85% or more", "13-23% (typical lab range, approximate)", "60-100 bpm (AHA); wearable cohort mean 65 bpm", "27-57 ms (mean +/- 1 SD, approximate)"),
    Source = c("Tudor-Locke 2010/2016 on NHANES 2005-06 accelerometry; Paluch 2022 meta-analysis", "WHO guidelines on physical activity and sedentary behaviour 2020", "common wearable convention (not a guideline)",
               "AASM/SRS consensus (Watson 2015); NSF recommendations", "National Sleep Foundation consensus (Ohayon 2017)", "Ohayon 2004 meta-analysis (stage shares fall with age)", "Quer et al. 2020, PLoS ONE (92,457 Fitbit users)", "Nunan 2010 review of short-term HRV norms"),
    Caveat = c("Pedometer-scaled NHANES counts run lower than modern wrist counts; middle quartiles not used", "HR-based estimate from a wrist sensor", "arbitrary threshold", "individual need varies", "watch efficiency is not lab efficiency",
               "wrist stage estimates are unreliable; use as trend", "athletes sit well below the range; individual spread up to 70 bpm", "sleep HRV from watches is higher than short seated readings; compare with your own history"),
    check.names = FALSE), striped = TRUE, spacing = "xs", width = "100%")
  
  output$p_sed <- renderPlotly({
    th <- TH(); d <- Dt(); x <- as.character(d$day); shiny::validate(shiny::need(any(!is.na(d$sed_h)), "No sedentary data in range."))
    plot_ly(x = x) %>% add_bars(y = d$sed_h, name = "sedentary h", marker = list(color = th$pal[1])) %>%
      add_lines(y = d$sed60 / 60, name = "h inside bouts >= 60 min", line = list(color = th$pal[4], width = 2)) %>%
      add_lines(y = d$sed_long, yaxis = "y2", name = "longest bout (min)", line = list(color = th$pal[2], dash = "dot")) %>%
      plotly::layout(yaxis2 = list(overlaying = "y", side = "right", title = "min", showgrid = FALSE, color = th$fg)) %>% style_plot(th, NULL, "hours")
  })
  
  # ================= DISTRIBUTIONS (violin / box) ====================================
  vcol <- function(th) list(line = list(color = th$pal[1], width = 1), fillcolor = "rgba(95,179,246,.25)")
  grp_days <- function(day) { span <- as.numeric(max(day) - min(day)); if (span <= 35) as.character(day) else as.character(as.Date(cut(day, "week"))) }
  thin <- function(m, n = 60000) if (nrow(m) > n) m[sample(nrow(m), n), ] else m
  output$p_circ <- renderPlotly({
    th <- TH(); m <- Mr(); m <- m[!is.na(m$hr), ]; req(nrow(m) > 0); m <- thin(m); m$h <- sprintf("%02d", m$hour)
    plot_ly(m, x = ~h, y = ~hr, type = "box", boxpoints = FALSE, marker = list(color = th$pal[1]), line = list(color = th$pal[1])) %>%
      style_plot(th, "hour of day", "minute HR (bpm)") %>% plotly::layout(xaxis = list(type = "category"))
  })
  output$p_hr_violin <- renderPlotly({
    th <- TH(); m <- Mr(); m <- m[!is.na(m$hr), ]; req(nrow(m) > 0); m <- thin(m); m$g <- grp_days(m$day)
    plot_ly(m, x = ~g, y = ~hr, type = "violin", box = list(visible = TRUE), meanline = list(visible = TRUE), points = FALSE, line = list(color = th$pal[1], width = 1), fillcolor = "rgba(95,179,246,.25)") %>%
      style_plot(th, NULL, "minute HR (bpm)") %>% plotly::layout(xaxis = list(type = "category"))
  })
  output$p_step_dow <- renderPlotly({
    th <- TH(); a <- AHD(); a <- a[a$day %in% Dt()$day[!is.na(Dt()$steps)] & inr(a$day), ]; shiny::validate(shiny::need(nrow(a) > 0, "No full days in range."))
    a$lab <- DAYS[as.integer(format(a$day, "%u"))]
    plot_ly(a, x = ~lab, y = ~s, type = "violin", box = list(visible = TRUE), meanline = list(visible = TRUE), points = FALSE, line = list(color = th$pal[3], width = 1), fillcolor = "rgba(123,216,143,.25)") %>%
      style_plot(th, NULL, "steps per waking hour") %>% plotly::layout(xaxis = list(categoryorder = "array", categoryarray = DAYS))
  })
  output$p_cad_violin <- renderPlotly({
    th <- TH(); m <- Mr(); m <- m[m$steps > 0, ]; shiny::validate(shiny::need(nrow(m) > 0, "No walking minutes in range.")); m$g <- grp_days(m$day)
    plot_ly(m, x = ~g, y = ~steps, type = "violin", box = list(visible = TRUE), meanline = list(visible = TRUE), points = FALSE, line = list(color = th$pal[2], width = 1), fillcolor = "rgba(246,160,95,.25)") %>%
      style_plot(th, NULL, "steps per minute while moving") %>% plotly::layout(xaxis = list(type = "category"))
  })
  output$p_sl_violin <- renderPlotly({
    th <- TH(); m <- M2(); m <- m[inr(m$day) & !is.na(m$stage) & !is.na(m$hr), ]; shiny::validate(shiny::need(nrow(m) > 0, "No heart rate inside detected sleep."))
    plot_ly(m, x = ~stage, y = ~hr, color = ~stage, colors = STAGE_COL, type = "violin", box = list(visible = TRUE), meanline = list(visible = TRUE), points = FALSE) %>%
      style_plot(th, NULL, "minute HR in sleep (bpm)") %>% plotly::layout(showlegend = FALSE)
  })
  output$p_sl_box <- renderPlotly({
    th <- TH(); n <- NI(); p <- plot_ly()
    for (st in c("deep", "light", "rem", "unst", "awake")) { y <- n[[st]] * 60; if (sum(y > 0, na.rm = TRUE) < 1) next
    nm <- c(deep = "Deep", light = "Light", rem = "REM", unst = "No stages", awake = "Awake in bed")[[st]]; col <- STAGE_COL[[c(deep = "Deep", light = "Light", rem = "REM", unst = "Unstaged", awake = "Awake")[[st]]]]
    p <- p %>% add_trace(y = y, name = nm, type = "box", boxpoints = "all", jitter = .4, pointpos = 0, marker = list(color = col), line = list(color = col)) }
    style_plot(p, th, NULL, "minutes per night")
  })
  output$p_stresshr <- renderPlotly({
    th <- TH(); s <- ST(); shiny::validate(shiny::need(nrow(s) > 0, "No stress samples in range.")); s$h <- sprintf("%02d", s$hour)
    plot_ly(s, x = ~h, y = ~stress, type = "box", boxpoints = "outliers", marker = list(color = th$pal[2]), line = list(color = th$pal[2])) %>%
      style_plot(th, "hour of day", "stress score") %>% plotly::layout(xaxis = list(type = "category"))
  })
  output$p_tr_month <- renderPlotly({
    th <- TH(); d <- Dt(); id <- input$tr_metric; req(id %in% TREND$id); d$y <- d[[id]]; d <- d[!is.na(d$y), ]; shiny::validate(shiny::need(nrow(d) > 2, "Not enough data.")); d$mon <- format(d$day, "%Y-%m")
    plot_ly(d, x = ~mon, y = ~y, type = "box", boxpoints = "all", jitter = .3, pointpos = 0, marker = list(color = th$pal[1], opacity = .5), line = list(color = th$pal[1])) %>%
      style_plot(th, NULL, TL[[id]]) %>% plotly::layout(xaxis = list(type = "category"))
  })
  output$p_tr_dow <- renderPlotly({
    th <- TH(); d <- Dt(); id <- input$tr_metric; req(id %in% TREND$id); d$y <- d[[id]]; d <- d[!is.na(d$y), ]; shiny::validate(shiny::need(nrow(d) > 2, "Not enough data."))
    d$lab <- DAYS[as.integer(format(d$day, "%u"))]
    plot_ly(d, x = ~lab, y = ~y, type = "violin", box = list(visible = TRUE), meanline = list(visible = TRUE), points = "all", jitter = .3, pointpos = 0, marker = list(color = th$pal[3], opacity = .5), line = list(color = th$pal[3], width = 1), fillcolor = "rgba(123,216,143,.22)") %>%
      style_plot(th, NULL, TL[[id]]) %>% plotly::layout(xaxis = list(categoryorder = "array", categoryarray = DAYS))
  })
  output$p_sp_box <- renderPlotly({
    th <- TH(); w <- Wm(); id <- input$ps_metric; shiny::validate(shiny::need(nrow(w) > 0, "No workouts.")); w$y <- w[[id]]; w <- w[!is.na(w$y), ]; shiny::validate(shiny::need(nrow(w) > 0, "No data for this metric."))
    plot_ly(w, x = ~sport, y = ~y, color = ~sport, colors = SPCOL(), type = "box", boxpoints = "all", jitter = .4, pointpos = 0) %>%
      style_plot(th, NULL, PSM[[id]]) %>% plotly::layout(showlegend = FALSE)
  })
  
  # ================= CORRELATIONS ====================================================
  CORD <- reactive({
    d <- Dt(); w <- Wm(); i <- as.character(d$day); d$sessions <- 0; splab <- character()
    if (nrow(w)) {
      tb <- table(as.character(w$day)); d$sessions <- ifelse(i %in% names(tb), as.numeric(tb[i]), 0)
      for (s in unique(w$sport)) { ww <- w[w$sport == s, ]; a <- tapply(ww$dur, as.character(ww$day), sum); id <- paste0("sp_", make.names(s))
      d[[id]] <- ifelse(i %in% names(a), as.numeric(a[i]), 0); splab[id] <- paste("Workout minutes:", s) }
    }
    h <- HRVN(); if (!is.null(h)) d$hrv <- h$v[match(d$day, h$day)]
    t <- TEMPN(); if (!is.null(t)) d$skin_temp <- t$v[match(d$day, t$day)]
    s <- tryCatch(SS(), error = function(e) NULL)
    if (!is.null(s)) { k <- match(d$day, s$day); d$sleep_score <- s$score[k]; d$rdi <- s$rdi[k]; d$breath <- s$breath[k]; d$spo_min_sleep <- s$spo_min[k] }
    sc <- tryCatch(SC(), error = function(e) NULL)
    if (!is.null(sc)) { k <- match(d$day, sc$day); d$sc_ready <- sc$readiness[k]; d$sc_sleep <- sc$sleep_sc[k]; d$sc_activity <- sc$activity_sc[k]; d$sc_strain <- sc$strain_sc[k]; d$sc_warn <- sc$warn[k] }
    list(d = d, splab = splab)
  })
  cor_meta <- function(C) {
    d <- C$d; m <- CMET[CMET$id %in% names(d), ]
    if (length(C$splab)) m <- rbind(m, data.frame(id = names(C$splab), lab = unname(C$splab), grp = "Training", stringsAsFactors = FALSE))
    m[vapply(m$id, function(i) sum(!is.na(d[[i]])) >= 5, logical(1)), ]
  }
  observe({
    m <- cor_meta(CORD()); req(nrow(m) > 1)
    ch <- split(setNames(m$id, m$lab), factor(m$grp, levels = unique(m$grp)))
    keep <- function(cur, def) if (!is.null(cur) && cur %in% m$id) cur else if (def %in% m$id) def else m$id[1]
    updateSelectInput(session, "cor_x", choices = ch, selected = keep(isolate(input$cor_x), "asleep_h"))
    updateSelectInput(session, "cor_y", choices = ch, selected = keep(isolate(input$cor_y), if ("hr_night" %in% m$id) "hr_night" else "rhr_w"))
    updateSelectInput(session, "cor_col2", choices = ch, selected = keep(isolate(input$cor_col2), "sport_min"))
  })
  CORP <- reactive({
    C <- CORD(); d <- C$d; m <- cor_meta(C); req(input$cor_x, input$cor_y)
    shiny::validate(shiny::need(all(c(input$cor_x, input$cor_y) %in% m$id), "Choose two metrics that have data."))
    cm <- input$cor_col; if (cm == "Weekday" && input$cor_agg != "Day") cm <- "Date (older to newer)"
    c2 <- if (cm == "Another metric" && !is.null(input$cor_col2) && input$cor_col2 %in% m$id) input$cor_col2 else NULL
    if (cm == "Another metric" && is.null(c2)) cm <- "None"
    a <- cor_agg(d, input$cor_agg, c(input$cor_x, input$cor_y, c2)); lab <- setNames(m$lab, m$id)
    wd <- as.integer(format(a$day, "%u"))
    cv <- switch(cm, "None" = rep(NA_real_, nrow(a)), "Date (older to newer)" = as.numeric(a$day), "Weekday" = DAYS[wd], "Month" = format(a$day, "%Y-%m"),
                 "Free day vs work day" = ifelse(wd %in% as.integer(input$free_days), "free day", "work day"), "Another metric" = a[[c2]])
    dd <- data.frame(day = a$day, x = a[[input$cor_x]], y = shift_lead(a[[input$cor_y]], input$cor_lag), cv = cv, stringsAsFactors = FALSE)
    dd <- dd[!is.na(dd$x) & !is.na(dd$y), ]
    if ("out" %in% input$cor_opts && nrow(dd) > 8) {
      zx <- as.numeric(abs(scale(dd$x))); zy <- as.numeric(abs(scale(dd$y))); dd <- dd[(is.na(zx) | zx <= 3) & (is.na(zy) | zy <= 3), ]
    }
    unit <- c(Day = "day(s)", Week = "week(s)", Month = "month(s)")[[input$cor_agg]]
    list(dd = dd, cm = cm, cname = if (cm == "Another metric") lab[[c2]] else cm, xlab = lab[[input$cor_x]],
         ylab = if (input$cor_lag != 0) sprintf("%s (%d %s %s X)", lab[[input$cor_y]], abs(input$cor_lag), unit, if (input$cor_lag > 0) "after" else "before") else lab[[input$cor_y]])
  }) 
  output$p_cor <- renderPlotly({
    th <- TH(); P <- CORP(); dd <- P$dd; opts <- input$cor_opts
    shiny::validate(shiny::need(nrow(dd) >= 3, "Too few overlapping values for this pair (3 minimum, 10+ recommended)."))
    st <- cor_stats(dd$x, dd$y, "spear" %in% opts); hov <- sprintf("%s<br>%s: %.3g<br>%s: %.3g", format(dd$day), P$xlab, dd$x, P$ylab, dd$y)
    p <- plot_ly()
    if (P$cm == "None" || all(is.na(dd$cv))) {
      p <- p %>% add_markers(x = dd$x, y = dd$y, name = "observations", text = hov, hoverinfo = "text", marker = list(color = th$pal[1], size = 9, opacity = .75))
    } else if (is.numeric(dd$cv)) {
      rg <- range(dd$cv, na.rm = TRUE); tv <- seq(rg[1], rg[2], length.out = 4)
      tt <- if (P$cm == "Date (older to newer)") format(as.Date(tv, origin = "1970-01-01"), "%d %b %Y") else format(round(tv, 1))
      p <- p %>% add_markers(x = dd$x, y = dd$y, name = "observations", text = hov, hoverinfo = "text",
                             marker = list(color = dd$cv, colorscale = "Viridis", showscale = TRUE, size = 9, opacity = .85,
                                           colorbar = list(title = list(text = P$cname), tickvals = tv, ticktext = tt, tickfont = list(color = th$fg))))
    } else {
      lv <- sort(unique(dd$cv))
      for (k in seq_along(lv)) { z <- dd$cv == lv[k]
      p <- p %>% add_markers(x = dd$x[z], y = dd$y[z], name = lv[k], text = hov[z], hoverinfo = "text", marker = list(color = th$pal[(k - 1) %% length(th$pal) + 1], size = 9, opacity = .8)) }
    }
    enough <- !is.null(st$fit) && nrow(dd) >= 8
    if (enough && any(c("line", "band") %in% opts)) {
      xs <- seq(min(dd$x), max(dd$x), length.out = 100); pr <- predict(st$fit, data.frame(x = xs), interval = "confidence")
      if ("band" %in% opts) p <- p %>% add_ribbons(x = xs, ymin = pr[, "lwr"], ymax = pr[, "upr"], name = "95% CI of the line", line = list(width = 0), fillcolor = "rgba(150,150,150,.25)", hoverinfo = "none")
      if ("line" %in% opts) p <- p %>% add_lines(x = xs, y = pr[, "fit"], name = "linear fit", line = list(color = th$fg, width = 2), hoverinfo = "none")
    }
    if (enough && "loess" %in% opts && nrow(dd) >= 10 && length(unique(dd$x)) >= 6) {
      lo <- tryCatch(loess(y ~ x, data = dd, span = .9), error = function(e) NULL)
      if (!is.null(lo)) { xs <- seq(min(dd$x), max(dd$x), length.out = 100); yl <- suppressWarnings(predict(lo, data.frame(x = xs)))
      p <- p %>% add_lines(x = xs, y = yl, name = "LOESS", line = list(color = th$pal[2], width = 2, dash = "dot"), hoverinfo = "none") }
    }
    ann <- list()
    if ("stats" %in% opts && !is.null(st$r)) {
      txt <- sprintf("%s = %.2f [95%% CI %.2f, %.2f]<br>R<sup>2</sup> = %.2f | p = %.3g | n = %d<br>slope = %.3g per unit [%.3g, %.3g]",
                     if ("spear" %in% opts) "Spearman rho" else "Pearson r", st$r, st$ci[1], st$ci[2], st$r2, st$p, st$n, st$slope, st$slope_ci[1], st$slope_ci[2])
      if (st$n < 10) txt <- paste0(txt, "<br>few points: treat as unstable")
      ann <- list(list(x = 0.01, y = 0.99, xref = "paper", yref = "paper", xanchor = "left", yanchor = "top", showarrow = FALSE, align = "left", text = txt,
                       bgcolor = "rgba(0,0,0,.35)", font = list(size = 12, color = "#ffffff")))
    }
    p %>% style_plot(th, P$xlab, P$ylab) %>% plotly::layout(hovermode = "closest", annotations = ann)
  })
  output$cor_text <- renderUI({
    P <- CORP(); dd <- P$dd; req(nrow(dd) >= 5); st <- cor_stats(dd$x, dd$y, "spear" %in% input$cor_opts); req(!is.null(st$r))
    a <- abs(st$r); strength <- if (a < .1) "negligible" else if (a < .3) "weak" else if (a < .5) "moderate" else if (a < .7) "strong" else "very strong"
    dirn <- if (st$r > 0) "higher values of X go with higher values of Y" else "higher values of X go with lower values of Y"
    it <- c(sprintf("A %s %s relationship (r = %.2f, R2 = %.2f): %s. %s", strength, if (st$r > 0) "positive" else "negative", st$r, st$r2, dirn,
                    if (st$p < 0.05) sprintf("The 95%% interval for r is %.2f to %.2f, so a zero relationship is unlikely (p = %.3g).", st$ci[1], st$ci[2], st$p) else
                      sprintf("The 95%% interval for r is %.2f to %.2f and includes zero (p = %.3g), so this could be chance.", st$ci[1], st$ci[2], st$p)),
            "Correlation is not causation, and day-to-day values are not independent (a good week follows a good week), which makes p-values look better than they are. Weekly resolution is safer for slow-moving measures.")
    tags$ul(lapply(it, tags$li))
  })
  SCAN <- eventReactive(input$cor_scan, {
    C <- CORD(); d <- C$d; m <- cor_meta(C); ids <- m$id[vapply(m$id, function(i) sum(!is.na(d[[i]])) >= 15, logical(1))]
    shiny::validate(shiny::need(length(ids) >= 3, "Not enough metrics with 15+ values yet."))
    a <- cor_agg(d, input$cor_agg, ids)
    fam <- function(i) if (i %in% CFAM$act) "a" else if (i %in% CFAM$hr) "h" else if (i %in% CFAM$tr || startsWith(i, "sp_")) "t" else i
    k <- length(ids); Rm <- matrix(NA_real_, k, k, dimnames = list(ids, ids)); rows <- list()
    for (i in seq_len(k - 1)) for (j in (i + 1):k) {
      x <- a[[ids[i]]]; y <- a[[ids[j]]]; ok <- !is.na(x) & !is.na(y)
      if (sum(ok) < 15 || sd(x[ok]) == 0 || sd(y[ok]) == 0) next
      ct <- suppressWarnings(cor.test(x[ok], y[ok], method = "spearman", exact = FALSE)); r <- unname(ct$estimate); Rm[i, j] <- r; Rm[j, i] <- r
      if (fam(ids[i]) == fam(ids[j]) || abs(r) > 0.97) next
      rows[[length(rows) + 1]] <- data.frame(x = ids[i], y = ids[j], rho = r, n = sum(ok), p = ct$p.value, stringsAsFactors = FALSE)
    }
    tb <- if (length(rows)) do.call(rbind, rows) else NULL
    if (!is.null(tb)) { tb$padj <- p.adjust(tb$p, "BH"); tb <- tb[order(-abs(tb$rho)), ] }
    list(tb = tb, R = Rm, lab = setNames(m$lab, m$id))
  })
  output$tbl_cor_top <- renderTable({
    shiny::validate(shiny::need(input$cor_scan > 0, "Press 'Scan all pairs'.")); S <- SCAN(); tb <- S$tb; shiny::validate(shiny::need(!is.null(tb) && nrow(tb) > 0, "No pairs with 15+ shared values yet."))
    tb <- head(tb, 15)
    data.frame(`X` = S$lab[tb$x], `Y` = S$lab[tb$y], rho = fmt(tb$rho, 2), n = tb$n, p = formatC(tb$p, format = "g", digits = 2), `adjusted p` = formatC(tb$padj, format = "g", digits = 2), check.names = FALSE, row.names = NULL)
  }, striped = TRUE, spacing = "xs", width = "100%")
  output$p_cor_map <- renderPlotly({
    shiny::validate(shiny::need(input$cor_scan > 0, "Press 'Scan all pairs'.")); th <- TH(); S <- SCAN(); lb <- unname(S$lab[colnames(S$R)])
    plot_ly(z = S$R, x = lb, y = lb, type = "heatmap", zmin = -1, zmax = 1, colorscale = list(c(0, "#ef4444"), c(.5, "rgba(127,127,127,.2)"), c(1, "#5fb3f6")),
            colorbar = list(title = "rho", tickfont = list(color = th$fg))) %>% style_plot(th, NULL, NULL) %>%
      plotly::layout(hovermode = "closest", xaxis = list(tickangle = -50, tickfont = list(size = 9)), yaxis = list(autorange = "reversed", tickfont = list(size = 9)))
  })
  
  # ================= SCORES (transparent, data-derived) ==============================
  SC <- reactive({
    d <- Dt(); req(nrow(d) > 0); n <- nrow(d)
    y <- if (sum(!is.na(d$hr_night)) >= 7) d$hr_night else d$rhr_w
    zr <- zfun(y, 28, 1)                                                       # + = resting HR above own baseline
    h <- HRVN(); zh <- rep(NA_real_, n); if (!is.null(h)) zh <- zfun(log(h$v), 60, .05)[match(d$day, h$day)]
    zs <- pmin(pmax((d$asleep_h - input$need) / 0.75, -3), 1)                  # sleep vs your need
    ze <- pmin(pmax((d$eff - 85) / 5, -3), 1.5)                                # efficiency vs 85 %
    L <- tryCatch(LOAD(), error = function(e) NULL); acwr <- rep(NA_real_, n); atl <- rep(NA_real_, n); strain <- rep(NA_real_, n)
    if (!is.null(L) && nrow(L) >= 28) {
      ac <- L$atl / pmax(L$ctl, 1e-6); ac[seq_len(27)] <- NA; k <- match(d$day, L$day); acwr <- ac[k]; atl <- L$atl[k]
      ref <- max(quantile(L$atl[28:nrow(L)], .9, names = FALSE), 1); strain <- 100 * pmin(atl / ref, 1)
    }
    zl <- pmin(pmax((1 - acwr) / .25, -3), 1.5)
    comb <- function(M, w) { W <- matrix(w, nrow(M), ncol(M), byrow = TRUE); W[is.na(M)] <- 0; Z <- M; Z[is.na(Z)] <- 0; r <- rowSums(Z * W) / rowSums(W); r[rowSums(!is.na(M)) < 2] <- NA_real_; r }
    ready <- 100 * pnorm(comb(cbind(-zr, zh, zs, ze, zl), c(.25, .25, .20, .10, .20)))
    dur <- ifelse(d$asleep_h <= 9, 100 * pmin(pmax((d$asleep_h - 4) / (input$need - 4), 0), 1), pmax(0, 100 - 20 * (d$asleep_h - 9)))
    eff <- 100 * pmin(pmax((d$eff - 65) / 20, 0), 1)
    sdm <- roll_fun(d$mid_h * 60, 7, function(z, na.rm = TRUE) sd(z, na.rm = na.rm), 3); reg <- 100 * pmin(pmax(1 - (sdm - 30) / 60, 0), 1)
    sleep_sc <- comb(cbind(dur, eff, reg), c(.5, .25, .25)); sleep_sc[is.na(dur)] <- NA_real_
    act <- 100 * roll_fun(d$mem, 7, mean, 5) * 7 / 150
    fl1 <- as.numeric(!is.na(zr) & zr > 2); fl1 <- as.numeric(fl1 > 0 & c(0, fl1[-n]) > 0); f1 <- roll_fun(fl1, 7, function(z, na.rm = TRUE) as.numeric(any(z > 0)), 1); f1[is.na(zr)] <- NA_real_
    f2 <- as.numeric(roll_fun(zh, 7, mean, 4) < -0.75)
    f3 <- as.numeric(roll_fun(input$need - d$asleep_h, 14, mean, 7) * 14 > 5)
    f4 <- as.numeric(acwr > 1.5)
    F <- cbind(f1, f2, f3, f4)
    data.frame(day = d$day, readiness = ready, sleep_sc = sleep_sc, activity_sc = act, strain_sc = strain,
               warn = ifelse(rowSums(!is.na(F)) == 0, NA_real_, rowSums(F, na.rm = TRUE)), warn_n = rowSums(!is.na(F)))
  })
  sc_last <- function(S, col, last) { x <- S[S$day <= last & !is.na(S[[col]]), ]; if (nrow(x)) x[nrow(x), ] else NULL }
  output$g_ready <- renderPlotly({ D <- DASH(); S <- SC()
  cur <- mn(S$readiness[S$day > D$last - D$n & S$day <= D$last]); prv <- if (D$n < 1e4) mn(S$readiness[S$day > D$last - 2 * D$n & S$day <= D$last - D$n]) else NA_real_
  mk_gauge("Readiness (0-100)", cur, c(0, 100), list(list(0, 40, "r"), list(40, 70, "a"), list(70, 100, "g")), ref = prv) })
  output$g_pai <- renderPlotly({ D <- DASH(); S <- SC(); r <- sc_last(S, "activity_sc", D$last); o <- sc_last(S, "activity_sc", D$last - 7)
  mk_gauge("Activity, PAI-style (7 days, 100 = goal)", if (is.null(r)) NA_real_ else r$activity_sc, c(0, 200), list(list(0, 50, "r"), list(50, 100, "a"), list(100, 200, "g")), thr = 100, ref = if (is.null(o)) NA_real_ else o$activity_sc) })
  output$g_strain <- renderPlotly({ D <- DASH(); S <- SC(); r <- sc_last(S, "strain_sc", D$last); o <- sc_last(S, "strain_sc", D$last - 7)
  mk_gauge("Training strain (acute load vs your usual)", if (is.null(r)) NA_real_ else r$strain_sc, c(0, 100), list(list(0, 60, "g"), list(60, 85, "a"), list(85, 100, "r")), ref = if (is.null(o)) NA_real_ else o$strain_sc, better = "neutral") })
  output$g_warn <- renderPlotly({ D <- DASH(); S <- SC(); r <- sc_last(S, "warn", D$last)
  mk_gauge(if (is.null(r)) "Exhaustion warning signs" else sprintf("Exhaustion warning signs (of %d checkable)", r$warn_n), if (is.null(r)) NA_real_ else r$warn, c(0, 4),
           list(list(0, 1, "g"), list(1, 2, "a"), list(2, 4, "r")), better = "down") })
  sc_def <- c(readiness = "Readiness", sleep_sc = "Sleep score", activity_sc = "Activity (PAI-style)", strain_sc = "Training strain", warn = "Warning signs")
  output$p_scores <- renderPlotly({
    th <- TH(); S <- SC(); sel <- input$sc_sel; shiny::validate(shiny::need(length(sel) > 0, "Tick at least one score."))
    k <- switch(input$sc_smooth, "Daily" = 1, "7-day mean" = 7, "28-day mean" = 28); x <- as.character(S$day); p <- plot_ly(x = x); cols <- th$pal; i <- 0
    for (id in sel) { i <- i + 1; y <- S[[id]]; if (id == "warn") y <- y * 25; if (id == "activity_sc") y <- pmin(y, 100)
    if (k > 1) y <- roll_fun(y, k, mean, ceiling(k / 3))
    nm <- c(readiness = "Readiness", sleep_sc = "Sleep score", activity_sc = "Activity (100 = weekly goal reached)", strain_sc = "Training strain", warn = "Warning signs (x25)")[[id]]
    p <- p %>% add_trace(y = y, type = "scatter", mode = if (k == 1) "lines+markers" else "lines", name = nm, connectgaps = FALSE, line = list(color = cols[(i - 1) %% length(cols) + 1], width = 2.2), marker = list(color = cols[(i - 1) %% length(cols) + 1], size = 5)) }
    p %>% plotly::layout(yaxis = list(range = c(0, 105))) %>% style_plot(th, NULL, "score (0-100)")
  })
  output$p_sc_month <- renderPlotly({
    th <- TH(); S <- SC(); id <- input$sc_focus; y <- S[[id]]; if (id == "warn") y <- y * 25; ok <- !is.na(y); shiny::validate(shiny::need(sum(ok) >= 3, "Not enough values yet."))
    plot_ly(x = format(S$day[ok], "%Y-%m"), y = y[ok], type = "box", boxpoints = "all", jitter = .3, pointpos = 0, marker = list(color = th$pal[1], opacity = .5), line = list(color = th$pal[1])) %>%
      style_plot(th, NULL, if (id == "warn") "warning signs x25" else sc_def[[id]]) %>% plotly::layout(xaxis = list(type = "category"))
  })
  output$tbl_sc <- renderTable({
    S <- SC(); span <- as.numeric(max(S$day) - min(S$day)); last <- max(S$day); bet <- c(readiness = "up", sleep_sc = "up", activity_sc = "up", strain_sc = "neutral", warn = "down")
    do.call(rbind, lapply(names(sc_def), function(id) { y <- S[[id]]; ts <- trend_stats(S$day, y)
    data.frame(Score = sc_def[[id]], `Last 30 d` = fmt(mn(y[S$day > last - 30]), 1), `Prior 30 d` = fmt(mn(y[S$day > last - 60 & S$day <= last - 30]), 1),
               `Trend / month` = fmt(ts$slope * 30, 2), p = ifelse(is.na(ts$p), "-", formatC(ts$p, format = "g", digits = 2)), Verdict = vtxt(trend_verdict(ts, span, bet[[id]])), check.names = FALSE, stringsAsFactors = FALSE) }))
  }, striped = TRUE, spacing = "xs", width = "100%")
  output$tbl_scores <- renderTable(data.frame(
    Score = c("Readiness (this app)", "Body Battery / energy meters (Garmin, Huawei)", "Activity, PAI-style (this app)", "Training strain (this app)", "Exhaustion warning signs (this app)", "Sleep score (this app)"),
    `Built from` = c("Resting-HR and HRV deviation from your own baseline, sleep vs your need, sleep efficiency, acute/chronic load ratio (weights .25/.25/.20/.10/.20, missing parts re-weighted), mapped to 0-100 (50 = your normal)",
                     "Proprietary mix of HRV, stress, sleep and activity", "Moderate-equivalent minutes (moderate + 2 x vigorous, heart-rate-reserve zones) over 7 days, scaled so 100 = 150 min (WHO minimum)",
                     "7-day EWMA of Banister TRIMP relative to the 90th percentile of your own history", "Four checks: resting HR 2+ days above range, 7-day HRV below range, 14-day sleep debt over 5 h, acute:chronic load above 1.5",
                     "Duration vs your need (50%), efficiency (25%), 7-night timing regularity (25%)"),
    Evidence = c("Components are sound (HRV and resting-HR monitoring; HRV-guided training shows a small and uncertain benefit over fixed plans in meta-analyses, Duking 2021). The weights are not validated.",
                 "No independent validation found.", "Official PAI is peer-reviewed: derived in the HUNT Fitness Study (n = 4,631), validated in 39,298 adults; 100+ PAI per week went with 17% (men) and 23% (women) lower cardiovascular mortality (Nes 2017).",
                 "Banister impulse-response model is established; injury prediction from load ratios is disputed (Impellizzeri 2020).", "Signs follow the overreaching monitoring consensus (Meeusen 2013, Halson 2014).",
                 "Dimensions from sleep-health research (Buysse 2014: duration, efficiency, regularity); cut-offs from AASM/SRS and NSF."),
    Verdict = c("Useful as a trend, not a verdict", "Not implemented", "Similar in spirit; the official PAI formula is not public in what I found", "Descriptive only", "Prompts to look closer, not a diagnosis", "Transparent composite"),
    check.names = FALSE), striped = TRUE, spacing = "xs", width = "100%")
  
  # ---- data quality + methods -----------------------------------------------------------------------------------
  output$tbl_codes <- renderTable({
    a <- raw()$act; a <- a[a$src == 13, ]; shiny::validate(shiny::need(nrow(a) > 0, "No stage rows (source 13)."))
    h <- as.integer(format(as.POSIXct(a$ts, origin = "1970-01-01", tz = input$tz), "%H"))
    a$night <- h >= 22 | h < 8; a$min <- (a$ots - a$ts) / 60
    z <- a %>% group_by(kind) %>% summarise(Segments = n(), Minutes = sum(min), `Median segment (min)` = median(min), `% at 22-08 h` = 100 * mean(night), .groups = "drop")
    z$Mapped <- ifelse(z$kind %in% as.numeric(input$c_deep), "Deep", ifelse(z$kind %in% as.numeric(input$c_light), "Light",
                                                                            ifelse(z$kind %in% as.numeric(input$c_rem), "REM", ifelse(z$kind %in% as.numeric(input$c_awake), "Awake marker", "activity / ignored"))))
    z$Segments <- fmt(z$Segments); z$Minutes <- fmt(z$Minutes); z$`% at 22-08 h` <- fmt(z$`% at 22-08 h`); z$`Median segment (min)` <- fmt(z$`Median segment (min)`, 1)
    names(z)[1] <- "Raw code"; z <- as.data.frame(z); z$`Raw code` <- as.character(z$`Raw code`)
    u <- raw()$act; u <- u[u$src == 10, ]
    if (nrow(u)) { hu <- as.integer(format(as.POSIXct(u$ts, origin = "1970-01-01", tz = input$tz), "%H")); mu <- (u$ots - u$ts) / 60
    z <- rbind(z, data.frame(`Raw code` = "source 10 (begin/end only)", Segments = fmt(nrow(u)), Minutes = fmt(sum(mu)), `Median segment (min)` = fmt(median(mu), 1),
                             `% at 22-08 h` = fmt(100 * mean(hu >= 22 | hu < 8)), Mapped = "Sleep, no stages", check.names = FALSE, stringsAsFactors = FALSE)) }
    z
  }, striped = TRUE, spacing = "xs", width = "100%")
  output$tbl_src <- renderTable({
    r <- raw(); cnt <- r$counts; n <- function(t) { v <- cnt[t]; if (is.na(v)) 0 else v }; a <- r$act[r$act$src == 11, ]
    fam <- c(huawei = "Huawei / Honor (HUAWEI_* tables)", huami = "Huami: Amazfit, Zepp OS, Mi Band (MI_BAND_ / HUAMI_* tables)")
    rows <- vapply(r$family, function(f) if (f == "huawei") n("HUAWEI_ACTIVITY_SAMPLE") else n("MI_BAND_ACTIVITY_SAMPLE") + n("HUAMI_EXTENDED_ACTIVITY_SAMPLE"), numeric(1))
    data.frame(`Detected data source` = unname(fam[r$family]), `Activity rows` = fmt(rows), check.names = FALSE) %>%
      rbind(data.frame(`Detected data source` = "Minute data covers", `Activity rows` = if (nrow(a)) paste(format(as.Date(as.POSIXct(min(a$ts), origin = "1970-01-01", tz = "UTC"))), "to", format(as.Date(as.POSIXct(max(a$ts), origin = "1970-01-01", tz = "UTC")))) else "-", check.names = FALSE))
  }, striped = TRUE, spacing = "xs")
  output$tbl_counts <- renderTable({ c <- raw()$counts; c <- c[c > 0]; data.frame(Table = names(c), Rows = fmt(as.numeric(c))) },
                                   striped = TRUE, spacing = "xs", width = "100%")
  output$tbl_cov <- renderTable({
    d <- Dly(); data.frame(Day = format(d$day, "%a %d %b"), `Minutes with data` = fmt(d$n), `HR coverage %` = fmt(d$cov), `Partial` = ifelse(d$partial, "yes", ""), check.names = FALSE)
  }, striped = TRUE, spacing = "xs")
  output$dq_notes <- renderUI({
    a <- raw()$act; dev <- raw()$device
    tags$ul(tags$li(sprintf("Heart-rate values wrapped by the signed-byte storage were repaired: %d samples (128-254 bpm).", sum(a$src == 11 & a$hr < -1))),
            tags$li(sprintf("Device(s) in this file: %s. Metrics a device does not record (for example HRV, skin temperature, or REM on some models) cannot be analysed here.", paste(dev$name, collapse = ", "))),
            tags$li("Minute rows use TIMESTAMP as the start of the minute; the mirrored duplicate rows Gadgetbridge writes are dropped."),
            tags$li("Sleep nights: staged rows (source 13) and begin/end-only rows (source 10, older sync without stages) are both read; the latter show as 'Asleep, no stages'."),
            tags$li("Calories are the watch's active calories (stored in cal, shown in kcal)."),
            tags$li(sprintf("FIT files read: %d. Gadgetbridge's GPS .bin files inside the zip are not read, so routes are not shown.", length(fits()))))
  })
  output$tbl_zones <- renderTable({
    hm <- hrmax(); data.frame(Parameter = c("HRmax", "Resting HR", "Zone 1 (50-60%)", "Zone 2 (60-70%)", "Zone 3 (70-80%)", "Zone 4 (80-90%)", "Zone 5 (90-100%)", "MVPA threshold (64% HRmax)"),
                              Value = c(sprintf("%.0f bpm", hm), sprintf("%.0f bpm", rhr()), sprintf("%.0f-%.0f bpm", .5 * hm, .6 * hm), sprintf("%.0f-%.0f bpm", .6 * hm, .7 * hm), sprintf("%.0f-%.0f bpm", .7 * hm, .8 * hm),
                                        sprintf("%.0f-%.0f bpm", .8 * hm, .9 * hm), sprintf("%.0f-%.0f bpm", .9 * hm, hm), sprintf("%.0f bpm", .64 * hm)))
  }, striped = TRUE, spacing = "xs")
  output$methods <- renderUI(tags$div(class = "small",
                                      tags$h5("How things are measured"),
                                      tags$ul(
                                        tags$li("HRmax: Tanaka et al. 2001 (208 - 0.7 x age); Fox 220 - age as alternative; or the observed 99.9th percentile. Zones in % of HRmax."),
                                        tags$li("Moderate-or-harder activity: HR >= 64% HRmax (ACSM moderate threshold) compared with the WHO 2020 target of 150-300 min/week."),
                                        tags$li("Cadence: >= 100 steps/min ~ moderate, >= 130 ~ vigorous (Tudor-Locke et al. 2019, CADENCE-Adults)."),
                                        tags$li("Step quality: a minute counts as a walking bout when cadence stays above your threshold for the chosen duration. Minutes below that cadence in which HR stays within the sedentary median (median HR of awake minutes without steps) + margin are flagged 'likely artefact' - an estimate, not ground truth. Other minutes with steps and an HR response are kept as real light movement."),
                                        tags$li("TRIMP: Banister 1991 (heart-rate-reserve weighted, sex-specific exponent); Edwards 1993 (minutes x zone weight 1-5)."),
                                        tags$li("Sleep: duration/efficiency use the watch's sleep-wake segmentation; stage shares are device estimates. Consumer wearables track sleep vs wake reasonably but stages poorly against polysomnography (Chinoy et al. 2021). Reference: >= 7 h for adults (Watson et al. 2015, AASM/SRS); typical PSG deep-sleep share 13-23% (Ohayon et al. 2004)."),
                                        tags$li("Resting-HR proxy: 5th percentile of all minute HR values (or your override); sleeping HR = median of minutes inside detected sleep sessions."),
                                        tags$li("Daily steps context: mortality benefit plateaus around 6-8k (age >= 60) and 8-10k (younger) steps/day (Paluch et al. 2022)."),
                                        tags$li("Trends: Theil-Sen slope (robust to outliers) with a Kendall test; a change is reported only if p < 0.05 and the total change exceeds 0.2 SD of the daily values, the smallest worthwhile change of Hopkins 2009."),
                                        tags$li("Sleep Regularity Index: Phillips et al. 2017. Social jetlag: Roenneberg et al. 2012 (simplified). Sleep debt vs your own need: chronic short sleep accumulates performance deficits (Van Dongen et al. 2003)."),
                                        tags$li("Resting HR range: median +/- 2 robust SD (MAD) of the previous 28 days; elevations over several days flag strain or illness. Fitness proxy: Uth et al. 2004 (15.3 x HRmax/HRrest)."),
                                        tags$li("Training load: EWMA acute (7 d) and chronic (28 d) load of Banister TRIMP (Williams et al. 2017); monotony and strain after Foster 1998; ACWR as description only (Gabbett 2016; criticism: Impellizzeri 2020). Intensity distribution after Seiler 2010."),
                                        tags$li("Sedentary time: runs of waking-window minutes without steps; high sedentary time is associated with mortality, largely offset by ample daily activity (Ekelund et al. 2016). Efficiency = metres per heartbeat (distance / total beats); a rise at similar effort suggests better aerobic fitness."),
                                        tags$li("HRV normal range: ln-transformed nightly values, 7-day mean against a 60-night baseline (Plews et al. 2013)."),
                                        tags$li("Dashboard gauges: steps (Paluch 2022), WHO 2020 activity minutes, 250 steps per hour as an active hour, sleep 7-9 h and efficiency >= 85 %, timing variability (SD of sleep midpoint), resting HR and HRV against your own history, Huawei stress bands, SpO2 bands, training load ratio. Colour bands are conventions or population guidance, not diagnoses."),
                                        tags$li("Real steps: a data-derived rule, not a trained model. Minutes with steps below walking cadence (not part of a sustained bout) are flagged as artefacts when heart rate shows no response, meaning HR at or below your median on awake zero-step minutes plus a margin. 'Real steps' sets those minutes to zero; gauges, trends, hourly activity and sedentary time all follow the Steps setting in the sidebar."),
                                        tags$li("Dashboard comparison bands: NHANES 2005-06 step distribution (Tudor-Locke et al.), Quer et al. 2020 resting HR, WHO 2020, AASM/SRS and NSF sleep consensus, Ohayon 2004. HRV and deep-sleep bands are approximate; see the reference table above."),
                                        tags$li("Scores: Readiness = weighted robust z-scores (resting HR and HRV against your previous 28-60 days, sleep against your stated need, efficiency against 85 %, acute:chronic load), mapped with the normal CDF so 50 means your normal. Activity (PAI-style) = 7-day mean of moderate + 2 x vigorous minutes (40-59 % and 60 % or more of heart-rate reserve, ACSM) x 7 / 150 x 100. It is not the official PAI algorithm. Strain = 7-day EWMA TRIMP against your own 90th percentile. Warning signs follow overreaching monitoring signs (Meeusen 2013, Halson 2014)."),
                                        tags$li("Correlations: Pearson r with Fisher-z 95% interval and R2 from the linear fit (the grey band is the confidence interval of the fitted mean), or Spearman rho with a Bonett-Wright interval; lag shifts Y by whole periods; the all-pairs scan applies Benjamini-Hochberg correction. Daily values are autocorrelated, so p-values are optimistic; trust effect sizes and intervals more than p."),
                                        tags$li("Recovery overview: robust z-scores (median and MAD of the previous 28-60 days), sign-adjusted so green always means more favourable than your own baseline."))))
}

shinyApp(ui, server)
