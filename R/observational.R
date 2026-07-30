# observational.R — estimation when assignment is not random.
#
# Two distinct jobs live here:
#   1. make_confounded_sample() manufactures confounding for the benchmark.
#      It is a demonstration device, NOT part of the client-facing path.
#   2. Everything else is the real estimator: propensity modelling, overlap
#      diagnostics, IPW, and doubly robust AIPW.

# ---- 1. manufactured confounding (benchmark only) -------------------------

# Selects a biased subsample so that treatment status becomes correlated with
# covariates that also drive the outcome. Because the true effect is known from
# the full randomised data, this gives ground truth to validate against.
make_confounded_sample <- function(cd, cfg, arm,
                                   confound_weights = c(recency = -1, history = 1),
                                   strength = 1.5, seed = 42) {

  dat     <- cd$data
  control <- cd$meta$control
  d <- dat[dat$treatment %in% c(control, arm), , drop = FALSE]
  d$treatment <- droplevels(d$treatment)

  # Standardise each driver; log-transform non-negative skewed variables so a
  # single extreme value cannot dominate the selection score.
  score <- rep(0, nrow(d))
  for (nm in names(confound_weights)) {
    v <- d[[nm]]
    if (is.null(v)) stop("confound variable not found: ", nm, call. = FALSE)
    v <- if (all(v >= 0, na.rm = TRUE)) log1p(v) else v
    score <- score + confound_weights[[nm]] * as.numeric(scale(v))
  }
  score <- score / length(confound_weights)

  is_treated <- d$treatment == arm
  p_keep <- ifelse(is_treated,
                   stats::plogis( strength * score),
                   stats::plogis(-strength * score))

  set.seed(seed)
  keep <- stats::runif(nrow(d)) < p_keep
  d2 <- d[keep, , drop = FALSE]
  d2$treatment <- droplevels(d2$treatment)

  list(
    data = d2,
    meta = modifyList(cd$meta, list(
      n        = nrow(d2),
      arms     = levels(d2$treatment),
      control  = control,
      n_by_arm = as.integer(table(d2$treatment)),
      synthetic = list(
        constructed = TRUE,
        arm         = arm,
        drivers     = confound_weights,
        strength    = strength,
        seed        = seed,
        note = "Randomisation deliberately broken by biased subsampling. Not observational data found in the wild."
      )
    ))
  )
}

# ---- 2. propensity model and overlap --------------------------------------

fit_propensity <- function(cd, arm, trim = 0.01) {
  d     <- cd$data
  covs  <- cd$meta$covariates
  X     <- d[, covs, drop = FALSE]
  X[]   <- lapply(X, function(v) if (is.character(v)) factor(v) else v)
  A     <- as.integer(d$treatment == arm)

  fml <- stats::as.formula(paste("A ~", paste(covs, collapse = " + ")))
  fit <- stats::glm(fml, data = cbind(A = A, X), family = stats::binomial())

  e_raw <- stats::fitted(fit)
  e     <- pmin(pmax(e_raw, trim), 1 - trim)

  list(fit = fit, e = e, e_raw = e_raw, A = A, X = X,
       n_trimmed = sum(e_raw != e), trim = trim)
}

check_overlap <- function(ps) {
  e <- ps$e_raw; A <- ps$A
  r1 <- range(e[A == 1]); r0 <- range(e[A == 0])
  lo <- max(r1[1], r0[1]); hi <- min(r1[2], r0[2])
  outside <- mean(e < lo | e > hi)

  w <- ifelse(A == 1, mean(A) / ps$e, mean(1 - A) / (1 - ps$e))
  max_w <- max(w)

  if (outside < 0.01 && max_w < 10) {
    new_check("overlap", "Common support", "pass", sprintf(
      "Propensity scores overlap well between groups. %.1f%% of units fall outside the shared range and the largest stabilised weight is %.1f. Every treated unit has comparable controls, so adjustment is interpolating rather than extrapolating.",
      100 * outside, max_w))
  } else {
    new_check("overlap", "Common support", "fail", sprintf(
      "Propensity overlap is poor: %.1f%% of units fall outside the shared range and the largest stabilised weight is %.1f. Some units have no comparable counterpart in the other group, so the adjusted estimate depends on extrapolation the data does not support.",
      100 * outside, max_w))
  }
}

# ---- 3. estimators --------------------------------------------------------

estimate_naive <- function(cd, arm, outcome, type, alpha = 0.05) {
  d  <- cd$data
  y1 <- d[[outcome]][d$treatment == arm]
  y0 <- d[[outcome]][d$treatment == cd$meta$control]
  est <- mean(y1) - mean(y0)
  se  <- sqrt(stats::var(y1) / length(y1) + stats::var(y0) / length(y0))
  crit <- stats::qnorm(1 - alpha / 2)
  data.frame(estimator = "Naive comparison", estimate = est, se = se,
             ci_low = est - crit * se, ci_high = est + crit * se,
             stringsAsFactors = FALSE)
}

# Hájek IPW. Standard errors treat the propensity score as known, which is
# conservative — estimating it typically reduces variance rather than inflating it.
estimate_ipw <- function(cd, ps, arm, outcome, alpha = 0.05) {
  d <- cd$data; Y <- d[[outcome]]; A <- ps$A; e <- ps$e
  w1 <- A / e; w0 <- (1 - A) / (1 - e)
  mu1 <- sum(w1 * Y) / sum(w1)
  mu0 <- sum(w0 * Y) / sum(w0)
  est <- mu1 - mu0
  psi <- w1 * (Y - mu1) - w0 * (Y - mu0)
  se  <- stats::sd(psi) / sqrt(length(psi))
  crit <- stats::qnorm(1 - alpha / 2)
  data.frame(estimator = "IPW", estimate = est, se = se,
             ci_low = est - crit * se, ci_high = est + crit * se,
             stringsAsFactors = FALSE)
}

# Augmented IPW: doubly robust. Consistent if EITHER the propensity model or
# the outcome model is correct — you get two chances to be right instead of one.
estimate_aipw <- function(cd, ps, arm, outcome, type, alpha = 0.05) {
  d <- cd$data; Y <- d[[outcome]]; A <- ps$A; e <- ps$e; X <- ps$X
  covs <- cd$meta$covariates
  fam  <- if (identical(type, "binary")) stats::binomial() else stats::gaussian()
  fml  <- stats::as.formula(paste("Y ~", paste(covs, collapse = " + ")))

  dm <- cbind(Y = Y, X)
  m1 <- stats::predict(stats::glm(fml, data = dm[A == 1, , drop = FALSE], family = fam),
                       newdata = X, type = "response")
  m0 <- stats::predict(stats::glm(fml, data = dm[A == 0, , drop = FALSE], family = fam),
                       newdata = X, type = "response")

  psi <- (A * (Y - m1) / e + m1) - ((1 - A) * (Y - m0) / (1 - e) + m0)
  est <- mean(psi)
  se  <- stats::sd(psi) / sqrt(length(psi))
  crit <- stats::qnorm(1 - alpha / 2)
  data.frame(estimator = "AIPW (doubly robust)", estimate = est, se = se,
             ci_low = est - crit * se, ci_high = est + crit * se,
             stringsAsFactors = FALSE)
}

# ---- 4. orchestration -----------------------------------------------------

run_observational <- function(cd, cfg, arm, outcome = NULL, trim = 0.01) {
  outcome <- outcome %||% cfg$outcomes$default
  type    <- cd$meta$outcome_types[[outcome]]
  alpha   <- cfg$design$alpha

  ps <- fit_propensity(cd, arm, trim = trim)
  est <- rbind(
    estimate_naive(cd, arm, outcome, type, alpha),
    estimate_ipw(cd, ps, arm, outcome, alpha),
    estimate_aipw(cd, ps, arm, outcome, type, alpha)
  )

  list(estimates = est,
       overlap   = check_overlap(ps),
       meta = list(design = "propensity", outcome = outcome, arm = arm,
                   control = cd$meta$control, n = cd$meta$n,
                   n_trimmed = ps$n_trimmed, trim = trim, alpha = alpha))
}

# The within-study comparison: experimental truth vs. what each observational
# estimator recovers on the confounded subsample.
run_benchmark <- function(cd, cfg, arm, outcome = NULL, strength = 1.5, seed = 42) {
  outcome <- outcome %||% cfg$outcomes$default

  truth_all <- run_experiment(cd, cfg, outcome = outcome)$estimates
  truth <- truth_all[truth_all$arm == arm, ]

  conf <- make_confounded_sample(cd, cfg, arm, strength = strength, seed = seed)
  obs  <- run_observational(conf, cfg, arm, outcome = outcome)

  tbl <- obs$estimates
  tbl$truth      <- truth$estimate
  tbl$bias       <- tbl$estimate - truth$estimate
  tbl$pct_error  <- 100 * tbl$bias / truth$estimate
  tbl$covers_truth <- truth$estimate >= tbl$ci_low & truth$estimate <= tbl$ci_high

  list(benchmark = tbl,
       overlap   = obs$overlap,
       truth     = truth,
       meta = list(outcome = outcome, arm = arm,
                   n_full = cd$meta$n, n_confounded = conf$meta$n,
                   construction = conf$meta$synthetic))
}