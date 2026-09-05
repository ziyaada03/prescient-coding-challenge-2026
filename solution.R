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
  reversion_days = 120,   # lookback for the price-reversal signal (see build_signal) --
                          # chosen for year-to-year SIGN STABILITY (~90% of years agree),
                          # not for the best-looking backtest number
  z_window       = 500,   # ~2 years: one shared "what counts as unusual right now" lookback,
                          # used to normalise both the reversal signal and the carry spread
  z_cap          = 2.5,   # cap on any z-score before use, so one extreme day cannot dominate
  carry_weight   = 0.6,   # weight of the bonds-vs-cash carry signal relative to reversion (=1)
  target_active  = 0.175, # fixed L1 size (sum of |active weight|) the raw signal is scaled to
                          # before legality/cost control -- direction comes from the signal,
                          # size does not, which keeps the realised active weight away from
                          # rule 6's 5% floor regardless of how strong today's z-scores are
  trade_speed    = 0.07,  # fraction of the remaining gap to target closed per day -- kept
                          # slow: cost drag scales directly with this, and a sweep showed
                          # excess was flat-to-better at slower speeds, not just cheaper
  deadband       = 0.010  # ignore target moves smaller than this -- not worth the trading cost
)

# The rules, restated locally so this file reads on its own.
ACTIVE_BAND   <- 0.10    # per asset, distance from benchmark
ACTIVE_BUDGET <- 0.40    # total, summed over assets
EQUITY        <- c("SA_EQUITY", "GLOBAL_EQUITY")
EQUITY_CAP    <- 0.75    # total equity, whatever the bands allow
GOLD_CAP      <- 0.10


# -----------------------------------------------------------------------------
# YOUR CODE GOES BELOW THIS LINE ----------------------------------------------
#
# Three signals, chosen only after checking each one on real numbers across
# the full 2004-2025 sample (see research notes -- correlations, quartile
# spreads and hit rates, year-by-year sign stability). Two ideas that looked
# plausible going in were tested and dropped rather than kept:
#
#   - Trailing-return MOMENTUM does not work at this horizon. What is stable
#     (80-95% of years agree on the sign) is the opposite: a large move over
#     the last ~6 months tends to partially reverse over the following month,
#     for SA_EQUITY, GLOBAL_EQUITY, SA_BONDS and GOLD. That is signal 1.
#   - A currency-trend signal for GLOBAL_EQUITY/GOLD (rand weakness helps
#     both, since both are rand-denominated) was tested directly: GOLD and
#     GLOBAL_EQUITY do move with the rand SAME DAY (beta 0.7 and 0.5), but a
#     rand TREND has ~zero correlation with either asset's FORWARD return.
#     That same-day co-movement is a mechanical translation effect, not a
#     tradeable timing signal, and a static "rand always weakens" tilt is
#     exactly the fixed-tilt-earns-nothing trap the brief warns about. Dropped.
#
# What survived:
#   1. Mean reversion (SA_EQUITY, GLOBAL_EQUITY, SA_BONDS, GOLD) -- fade an
#      unusually large 120-day move, sized by how unusual it is relative to
#      that asset's own recent history.
#   2. Carry (SA_BONDS vs SA_CASH) -- a steep SA 10y-vs-3m-Jibar curve has
#      historically paid to hold duration over cash (clean, monotonic
#      relationship, strengthening with horizon); a flat/inverted curve has
#      not. Symmetric: bonds up, cash down, funded against each other.
#
# A third idea, a VIX-based volatility dampener that shrinks the whole tilt
# (not a directional bet) when vol is elevated, was built and tested the same
# way as everything else here -- and cut. A parameter sweep showed it made the
# honest out-of-sample years worse as it got stronger (mean excess fell from
# +0.124 bps/day with no dampener to +0.065 at the tested setting), for no
# measurable improvement in the worst-case window. It did not earn its
# parameter, so it is not in this file.
#
# SA_PROPERTY gets no directional signal at all: its own momentum/reversion
# sign is close to a coin flip year to year (14-68% stability, against 80%+
# for everything else), and it is the most expensive asset to trade (35bp).
# Its score stays exactly 0 and is kept OUT of the centring step below, so
# nothing ever assigns it a deliberate view.
#
# Both signals are combined as capped z-scores with FIXED weights (reversion
# = 1 implicitly, carry = carry_weight, both explainable on their own terms)
# -- the weights were not fit to make any scored window look good. The five
# assets that carry a view are then centred to net to ~0 among themselves
# (property excluded from that group entirely, not just left at 0): without
# this, make_legal()'s own sum-to-1 correction would trade property to make
# up whatever the other five did not net out to on their own -- an
# accidental property bet, not a deliberate one.
#
# Sizing is a fixed L1 budget (target_active), not a linear multiplier on
# the raw z-scores: the direction of the tilt comes from the signal, but its
# SIZE does not, so a run of quiet-signal days cannot drift the realised
# average active weight toward rule 6's 5% floor. Cost control (deadband +
# partial adjustment) is applied on top of the sized signal, in
# generate_weights(), not folded into it. A parameter sweep on trade_speed
# showed slower was flat-to-better on expected excess and meaningfully
# better on worst-case loss, so it was set toward the cautious end of what
# was tested rather than the value that looked best on any single window.
# -----------------------------------------------------------------------------

REVERSION_ASSETS <- c("SA_EQUITY", "GLOBAL_EQUITY", "SA_BONDS", "GOLD")

#' Trailing L-day cumulative return ending at each row (vectorised via a
#' cumulative sum of log returns). NA for the first L rows, where there is
#' not yet enough history to form the window, AND for any row whose window
#' contains a missing return -- a plain cumsum() would otherwise let one NA
#' poison every window for the rest of the asset's history, not just the
#' windows that actually touch it.
trailing_cum_return <- function(x, L) {
  n <- length(x)
  if (n <= L) return(rep(NA_real_, n))
  logx <- log1p(x)
  missing <- is.na(logx)
  logx[missing] <- 0                              # neutral placeholder, tracked separately below
  csum      <- c(0, cumsum(logx))
  miss_csum <- c(0, cumsum(as.numeric(missing)))  # count of missing days up to and including i
  out <- rep(NA_real_, n)
  idx <- (L + 1):n
  clean <- (miss_csum[idx + 1] - miss_csum[idx - L + 1]) == 0
  out[idx[clean]] <- exp(csum[idx[clean] + 1] - csum[idx[clean] - L + 1]) - 1
  out
}

#' Capped z-score of the LAST valid value of `x` against its own trailing
#' history. Returns 0 (neutral) if there is not enough history yet, or if the
#' recent history has no variation to compare against -- handles both short
#' history at the start of a window and gappy/missing macro rows defensively.
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

#' Score per asset. Positive means overweight, negative means underweight.
#' See the header comment above for the economic thesis behind each piece.
build_signal <- function(hist, params) {
  score <- setNames(rep(0, length(hist$assets)), hist$assets)

  L  <- as.integer(params$reversion_days)
  zw <- as.integer(params$z_window)
  zc <- as.numeric(params$z_cap)

  # 1. mean reversion: fade a stretched move, sized by how stretched it is.
  for (a in REVERSION_ASSETS) {
    trail <- trailing_cum_return(hist$returns[, a], L)
    score[[a]] <- score[[a]] - trailing_z(trail, zw, zc)
  }

  # 2. carry: bonds vs cash, symmetric.
  macro_cols <- colnames(hist$macro)
  if (all(c("sa_10y", "jibar_3m") %in% macro_cols) && nrow(hist$macro) > 0) {
    spread  <- hist$macro[, "sa_10y"] - hist$macro[, "jibar_3m"]
    z_carry <- trailing_z(spread, zw, zc)
    w_carry <- as.numeric(params$carry_weight)
    score[["SA_BONDS"]] <- score[["SA_BONDS"]] + w_carry * z_carry
    score[["SA_CASH"]]  <- score[["SA_CASH"]]  - w_carry * z_carry
  }

  # Centre the five assets that actually carry a view so they net to ~0
  # among themselves -- SA_PROPERTY is deliberately excluded from the
  # centring group, not just left at 0, so it never absorbs the other
  # five's netting-to-zero adjustment. Without this, make_legal()'s own
  # sum-to-1 correction would end up trading property to make up whatever
  # the other five's raw scores did not net out to on their own -- an
  # accidental property bet, not a deliberate one.
  active_assets <- setdiff(hist$assets, "SA_PROPERTY")
  score[active_assets] <- score[active_assets] - mean(score[active_assets])

  score
}

#' Scale `signal` so the resulting active-weight vector has an L1 size (sum
#' of |active weight|, pre-legality) equal to `target_active`, rather than a
#' fixed linear multiplier on the raw z-scores. A multiplier makes the day's
#' active-weight SIZE a function of how large that day's z-scores happen to
#' be -- a run of quiet-signal days can drift the realised average toward
#' (or under) rule 6's 5% floor. Fixing the L1 size instead means direction
#' still comes from the signal, but size does not depend on conviction; the
#' trade-off is deliberate, in exchange for removing that gate risk and
#' using more of the risk budget consistently. Returns `signal` unscaled
#' (all zero) if there is no view at all today, rather than dividing by 0.
scale_to_active_weight <- function(signal, target_active) {
  total <- sum(abs(signal))
  if (!is.finite(total) || total <= 0) return(signal)
  signal * (target_active / total)
}


#' Force `weights` to satisfy every rule. You can leave this alone.
#'
#' Everything happens in active space -- how far each asset sits from its
#' benchmark weight -- because that is how the rules are written.
#'
#' The loop is there because the steps interfere: forcing the active weights to
#' net to zero (so the portfolio sums to 1) can push an asset back outside its
#' band. A few passes settles it. The budget scaling goes last and is safe
#' there: shrinking every active weight toward zero cannot breach a band, a
#' cap, or non-negativity.
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

  # 1. signal -> fixed-size tilt -> target weights around the benchmark
  signal <- build_signal(hist, params)
  sized  <- scale_to_active_weight(signal, as.numeric(params$target_active))
  target <- make_legal(bm + sized, hist)

  # 2. cost control, applied on top of the signal rather than inside it.
  # No-trade band first: a target move too small to be worth its trading
  # cost is treated as no move at all, per asset. Then partial adjustment:
  # close only part of whatever gap survives the deadband, so a signal that
  # flips does not cost a full round-trip in one day.
  prev <- prev_weights[hist$assets]
  gap  <- target - prev
  gap[abs(gap) < as.numeric(params$deadband)] <- 0
  w <- prev + as.numeric(params$trade_speed) * gap

  make_legal(w, hist)
}

# YOUR CODE GOES ABOVE THIS LINE ----------------------------------------------
