# -----------------------------------------------------------------------------
# Prescient Coding Challenge 2026 -- your submission (R).
#
# THIS IS THE ONLY FILE YOU MAY CHANGE.
#
# You implement one function. The harness calls it once per trading day and
# hands you a `hist` list holding every observation STRICTLY BEFORE that day.
# You return the weights you want to hold for that day.
#
#     generate_weights(hist, prev_weights, params) -> named numeric vector
#
# What you get
# ------------
# hist$date                 the day you are allocating for (no data for it yet)
# hist$returns              matrix [date x asset] of daily returns, decimals
# hist$prices               matrix [date x asset] of total-return index levels
# hist$macro                matrix [date x macro feature]
# hist$assets               the six asset codes, in order
# hist$benchmark            named vector of benchmark weights
# hist$active_weight(w)     total active weight of w -- the number rule 3 tests
#
# prev_weights              what you held yesterday. Trading away from it costs
#                           money, so look at it.
# params                    the PARAMS list below, passed straight through
#
# Optional extras, in case you want them: hist$cov() gives an EWMA covariance
# matrix and hist$te(w) an ex-ante tracking error. No rule depends on either.
#
# What you must return
# --------------------
# Six weights (named numeric vector) that sum to 1, are all non-negative, sit
# within 10% of their benchmark weight, have a total active weight of no more
# than 40%, keep total equity at or below 75% and gold at or below 10%.
# make_legal() below already does all
# of that -- you can leave it alone.
#
# Declare every tuneable number in PARAMS. Parameter count is part of the score.
#
# Run `Rscript harness.R` to test on the practice window (calendar 2025), then
# `Rscript validate.R` before you submit.
# -----------------------------------------------------------------------------

# ---- Every tuneable number lives here. Fewer is better. ---------------------

PARAMS <- list(
  reversion_days = 120,
  z_window       = 500,
  z_cap          = 2.5,
  carry_weight   = 0.6,
  target_active  = 0.175,
  trade_speed    = 0.07,
  deadband       = 0.010
)

# The rules, restated locally so this file reads on its own.
ACTIVE_BAND   <- 0.10    # per asset, distance from benchmark
ACTIVE_BUDGET <- 0.40    # total, summed over assets
EQUITY        <- c("SA_EQUITY", "GLOBAL_EQUITY")
EQUITY_CAP    <- 0.75    # total equity, whatever the bands allow
GOLD_CAP      <- 0.10


# -----------------------------------------------------------------------------
# YOUR CODE GOES BELOW THIS LINE ----------------------------------------------


REVERSION_ASSETS <- c("SA_EQUITY", "GLOBAL_EQUITY", "SA_BONDS", "GOLD")

trailing_cum_return <- function(x, L) {
  n <- length(x)
  if (n <= L) return(rep(NA_real_, n))
  logx <- log1p(x)
  missing <- is.na(logx)
  logx[missing] <- 0
  csum      <- c(0, cumsum(logx))
  miss_csum <- c(0, cumsum(as.numeric(missing)))
  out <- rep(NA_real_, n)
  idx <- (L + 1):n
  clean <- (miss_csum[idx + 1] - miss_csum[idx - L + 1]) == 0
  out[idx[clean]] <- exp(csum[idx[clean] + 1] - csum[idx[clean] - L + 1]) - 1
  out
}


trailing_z <- function(x, z_window, cap) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 20) return(0)
  window <- utils::tail(x, as.integer(z_window))
  mu  <- mean(window)
  sdv <- stats::sd(window)
  if (!is.finite(sdv) || sdv <= 0) return(0)
  z <- (x[n] - mu) / sdv
  max(min(z, cap), -cap)
}



build_signal <- function(hist, params) {
  score <- setNames(rep(0, length(hist$assets)), hist$assets)

  L  <- as.integer(params$reversion_days)
  zw <- as.integer(params$z_window)
  zc <- as.numeric(params$z_cap)

  for (a in REVERSION_ASSETS) {
    trail <- trailing_cum_return(hist$returns[, a], L)
    score[[a]] <- score[[a]] - trailing_z(trail, zw, zc)
  }

  macro_cols <- colnames(hist$macro)
  if (all(c("sa_10y", "jibar_3m") %in% macro_cols) && nrow(hist$macro) > 0) {
    spread  <- hist$macro[, "sa_10y"] - hist$macro[, "jibar_3m"]
    z_carry <- trailing_z(spread, zw, zc)
    w_carry <- as.numeric(params$carry_weight)
    score[["SA_BONDS"]] <- score[["SA_BONDS"]] + w_carry * z_carry
    score[["SA_CASH"]]  <- score[["SA_CASH"]]  - w_carry * z_carry
  }



  active_assets <- setdiff(hist$assets, "SA_PROPERTY")
  score[active_assets] <- score[active_assets] - mean(score[active_assets])

  score
}


scale_to_active_weight <- function(signal, target_active) {
  total <- sum(abs(signal))
  if (!is.finite(total) || total <= 0) return(signal)
  signal * (target_active / total)
}



make_legal <- function(weights, hist) {
  bm <- hist$benchmark
  active <- weights[hist$assets] - bm

  for (i in 1:50) {
    active <- pmin(pmax(active, -ACTIVE_BAND), ACTIVE_BAND)   # rule 2
    active <- pmax(active, -bm)                               # keeps weights >= 0
    # rule 4: total equity cap. Trim the equity block back, sharing the cut
    # over whichever equity assets still have room to come down.
    eq_excess <- sum(bm[EQUITY] + active[EQUITY]) - EQUITY_CAP
    eq_full   <- eq_excess > -1e-12
    if (eq_excess > 0) {
      floor <- pmax(-ACTIVE_BAND, -bm[EQUITY])
      down  <- pmax(active[EQUITY] - floor, 0)
      if (sum(down) > 1e-15)
        active[EQUITY] <- active[EQUITY] - eq_excess * down / sum(down)
    }

    active[["GOLD"]] <- min(active[["GOLD"]], GOLD_CAP - bm[["GOLD"]])  # rule 5

    excess <- sum(active)        # must be zero for weights to sum to 1
    if (abs(excess) < 1e-12) break
    # give the correction to the assets that have room to absorb it
    room <- if (excess < 0) ACTIVE_BAND - active else active + bm
    room <- pmax(room, 0)
    if (excess < 0 && eq_full) room[EQUITY] <- 0  # equity full -- top up elsewhere
    if (sum(room) <= 1e-15) break
    active <- active - excess * room / sum(room)
  }

  total <- sum(abs(active))                                   # rule 3
  if (total > ACTIVE_BUDGET) active <- active * (ACTIVE_BUDGET / total)

  bm + active
}


#' Return the six portfolio weights to hold on hist$date.
generate_weights <- function(hist, prev_weights, params) {
  bm <- hist$benchmark

  # not enough history to estimate anything: sit on the benchmark
  if (nrow(hist$returns) < 260) return(bm)

  signal <- build_signal(hist, params)
  sized  <- scale_to_active_weight(signal, as.numeric(params$target_active))
  target <- make_legal(bm + sized, hist)

  prev <- prev_weights[hist$assets]
  gap  <- target - prev
  gap[abs(gap) < as.numeric(params$deadband)] <- 0
  w <- prev + as.numeric(params$trade_speed) * gap

  make_legal(w, hist)
}

# AI tools used: Claude code, for research tooling, data exploration, and
# iterating on candidate signals under our direction.

# YOUR CODE GOES ABOVE THIS LINE ----------------------------------------------
