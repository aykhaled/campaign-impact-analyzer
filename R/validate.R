# validate.R — assumption checks. Each returns a structured result carrying a
# status and a plain-language explanation. Pure functions: no printing, no Shiny.
# The explanation travels with the result so the app and the report cannot
# describe the same check differently.

new_check <- function(id, label, status, message, detail = NULL) {
  stopifnot(status %in% c("pass", "fail", "not_applicable"))
  structure(
    list(id = id, label = label, status = status,
         message = message, detail = detail),
    class = "validity_check"
  )
}

# ---- standardised mean differences ---------------------------------------

smd_numeric <- function(x1, x0) {
  s <- sqrt((stats::var(x1) + stats::var(x0)) / 2)
  if (!is.finite(s) || s == 0) return(0)
  (mean(x1) - mean(x0)) / s
}

smd_prop <- function(p1, p0) {
  s <- sqrt((p1 * (1 - p1) + p0 * (1 - p0)) / 2)
  if (!is.finite(s) || s == 0) return(0)
  (p1 - p0) / s
}

# For a categorical covariate, report the level with the largest imbalance.
covariate_smd <- function(x, in_arm, in_control) {
  if (is.numeric(x)) {
    return(smd_numeric(x[in_arm], x[in_control]))
  }
  lv <- unique(stats::na.omit(x))
  d <- vapply(lv, function(l) {
    smd_prop(mean(x[in_arm] == l), mean(x[in_control] == l))
  }, numeric(1))
  d[which.max(abs(d))]
}

check_balance <- function(cd, cfg) {
  covs <- cd$meta$covariates
  if (!length(covs)) {
    return(new_check(
      "balance", "Covariate balance", "not_applicable",
      "No covariates are declared in the configuration, so balance between groups cannot be assessed."))
  }

  dat     <- cd$data
  control <- cd$meta$control
  arms    <- setdiff(cd$meta$arms, control)
  thr     <- cfg$validity$smd_threshold
  in_control <- dat$treatment == control

  rows <- list()
  for (a in arms) {
    in_arm <- dat$treatment == a
    for (cv in covs) {
      rows[[length(rows) + 1L]] <- data.frame(
        arm = a, covariate = cv,
        smd = covariate_smd(dat[[cv]], in_arm, in_control),
        stringsAsFactors = FALSE
      )
    }
  }
  tbl <- do.call(rbind, rows)
  tbl$imbalanced <- abs(tbl$smd) > thr

  worst <- tbl[which.max(abs(tbl$smd)), ]
  n_bad <- sum(tbl$imbalanced)

  if (n_bad == 0L) {
    msg <- sprintf(
      "All %d covariate comparisons are balanced. The largest standardised difference is %.3f (%s, %s) against a threshold of %.2f. Groups are comparable before treatment, which is what random assignment should produce.",
      nrow(tbl), abs(worst$smd), worst$covariate, worst$arm, thr)
    new_check("balance", "Covariate balance", "pass", msg, tbl)
  } else {
    msg <- sprintf(
      "%d of %d covariate comparisons exceed the balance threshold of %.2f. The worst is %s in the %s group, at %.3f. The groups differed before treatment, so any raw difference in outcomes mixes the treatment effect with those pre-existing differences.",
      n_bad, nrow(tbl), thr, worst$covariate, worst$arm, worst$smd)
    new_check("balance", "Covariate balance", "fail", msg, tbl)
  }
}

# ---- group sizes ----------------------------------------------------------

check_group_size <- function(cd, cfg) {
  min_n <- cfg$validity$min_group_n
  n <- cd$meta$n_by_arm
  names(n) <- cd$meta$arms
  tbl <- data.frame(arm = names(n), n = as.integer(n), stringsAsFactors = FALSE)
  small <- n[n < min_n]

  if (!length(small)) {
    new_check("group_size", "Group sizes", "pass",
      sprintf("All %d groups meet the minimum of %d observations. The smallest is %s at %d.",
              length(n), min_n, names(n)[which.min(n)], min(n)), tbl)
  } else {
    new_check("group_size", "Group sizes", "fail",
      sprintf("%d group(s) fall below the minimum of %d: %s. Estimates for these arms would be too imprecise to act on.",
              length(small), min_n,
              paste(sprintf("%s (n = %d)", names(small), small), collapse = ", ")), tbl)
  }
}

# ---- parallel pre-trends (difference-in-differences only) -----------------

check_pretrend <- function(cd, cfg) {
  if (!cd$meta$has_period) {
    return(new_check(
      "pretrend", "Parallel pre-trends", "not_applicable",
      "No period column is configured, so there are no pre-treatment periods to compare. Difference-in-differences requires the same units observed before and after the intervention."))
  }
  new_check("pretrend", "Parallel pre-trends", "not_applicable",
            "Pre-trend testing is implemented with the difference-in-differences estimator.")
}

# ---- runner ---------------------------------------------------------------

run_validity_checks <- function(cd, cfg) {
  checks <- list(
    check_balance(cd, cfg),
    check_group_size(cd, cfg),
    check_pretrend(cd, cfg)
  )
  names(checks) <- vapply(checks, `[[`, character(1), "id")
  checks
}

validity_summary <- function(checks) {
  data.frame(
    check   = vapply(checks, `[[`, character(1), "label"),
    status  = vapply(checks, `[[`, character(1), "status"),
    message = vapply(checks, `[[`, character(1), "message"),
    stringsAsFactors = FALSE, row.names = NULL
  )
}