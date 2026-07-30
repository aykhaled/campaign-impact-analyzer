# experiment.R — analysis for randomised designs.
# Each treatment arm is compared against the control arm. Estimates are
# accompanied by a minimum detectable effect so that a null result can be
# distinguished from an underpowered one.

run_experiment <- function(cd, cfg, outcome = NULL, power = 0.8) {

  outcome <- outcome %||% cfg$outcomes$default
  type    <- cd$meta$outcome_types[[outcome]]
  if (is.null(type)) {
    stop("Outcome '", outcome, "' is not declared in config outcomes$available.",
         call. = FALSE)
  }

  dat     <- cd$data
  control <- cd$meta$control
  arms    <- setdiff(cd$meta$arms, control)
  alpha   <- cfg$design$alpha

  y0 <- dat[[outcome]][dat$treatment == control]
  n0 <- length(y0)
  b0 <- if (type == "binary") mean(y0) else mean(y0)

  rows <- lapply(arms, function(a) {
    y1 <- dat[[outcome]][dat$treatment == a]
    n1 <- length(y1)

    if (type == "binary") {
      p1 <- mean(y1); p0 <- mean(y0)
      est <- p1 - p0
      se  <- sqrt(p1 * (1 - p1) / n1 + p0 * (1 - p0) / n0)
      crit <- stats::qnorm(1 - alpha / 2)
      pval <- 2 * stats::pnorm(-abs(est / se))
    } else {
      m1 <- mean(y1); m0 <- mean(y0)
      v1 <- stats::var(y1); v0 <- stats::var(y0)
      est <- m1 - m0
      se  <- sqrt(v1 / n1 + v0 / n0)
      df  <- (v1/n1 + v0/n0)^2 /
             ((v1/n1)^2 / (n1 - 1) + (v0/n0)^2 / (n0 - 1))   # Welch
      crit <- stats::qt(1 - alpha / 2, df)
      pval <- 2 * stats::pt(-abs(est / se), df)
    }

    data.frame(
      arm        = a,
      n_treated  = n1,
      n_control  = n0,
      mean_treated = if (type == "binary") mean(y1) else mean(y1),
      mean_control = b0,
      estimate   = est,
      se         = se,
      ci_low     = est - crit * se,
      ci_high    = est + crit * se,
      rel_lift   = if (b0 != 0) est / b0 else NA_real_,
      p_value    = pval,
      mde        = (stats::qnorm(1 - alpha / 2) + stats::qnorm(power)) * se,
      stringsAsFactors = FALSE
    )
  })

  est <- do.call(rbind, rows)

  method <- cfg$design$multiple_comparison %||% "none"
  est$p_adjusted <- if (identical(method, "none")) est$p_value
                    else stats::p.adjust(est$p_value, method = method)
  est$significant <- est$p_adjusted < alpha

  list(
    estimates = est,
    meta = list(
      design            = "randomised",
      outcome           = outcome,
      outcome_type      = type,
      control           = control,
      alpha             = alpha,
      power_target      = power,
      correction        = method,
      n_comparisons     = nrow(est),
      control_baseline  = b0
    )
  )
}

# ---- Bayesian readout (binary outcomes only) ------------------------------
# Uniform Beta(1,1) prior; conjugate update gives a Beta posterior per arm.
# No sampler, no compilation — this deploys anywhere.

posterior_binary <- function(cd, cfg, outcome, draws = 2e5, seed = 42) {

  type <- cd$meta$outcome_types[[outcome]]
  if (!identical(type, "binary")) {
    return(NULL)
  }

  dat     <- cd$data
  control <- cd$meta$control
  arms    <- setdiff(cd$meta$arms, control)

  y0 <- dat[[outcome]][dat$treatment == control]
  post0 <- c(1 + sum(y0), 1 + length(y0) - sum(y0))

  set.seed(seed)
  d0 <- stats::rbeta(draws, post0[1], post0[2])

  out <- lapply(arms, function(a) {
    y1 <- dat[[outcome]][dat$treatment == a]
    d1 <- stats::rbeta(draws, 1 + sum(y1), 1 + length(y1) - sum(y1))
    diff <- d1 - d0
    data.frame(
      arm            = a,
      prob_beats_control = mean(d1 > d0),
      diff_median    = stats::median(diff),
      diff_low       = stats::quantile(diff, 0.025, names = FALSE),
      diff_high      = stats::quantile(diff, 0.975, names = FALSE),
      stringsAsFactors = FALSE
    )
  })

  list(
    posteriors = do.call(rbind, out),
    meta = list(prior = "Beta(1, 1)", draws = draws, seed = seed,
                outcome = outcome, control = control)
  )
}