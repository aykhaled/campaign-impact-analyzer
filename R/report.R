# report.R — assembles a single results object for the Quarto report.
# All computation happens here. The .qmd formats and nothing else, so the
# report and the app cannot disagree about a number.

build_results <- function(cfg, outcome = NULL, arm = NULL, path = NULL) {

  cd      <- load_campaign_data(cfg, path = path)
  outcome <- outcome %||% cfg$outcomes$default
  arm     <- arm %||% setdiff(cd$meta$arms, cd$meta$control)[1]

  checks <- run_validity_checks(cd, cfg)
  dx     <- diagnose_design(cd, cfg, checks)
  exper  <- run_experiment(cd, cfg, outcome = outcome)
  bayes  <- posterior_binary(cd, cfg, outcome)
  bench  <- run_benchmark(cd, cfg, arm = arm, outcome = outcome)

  list(
    meta = list(
      generated   = format(Sys.time(), "%Y-%m-%d %H:%M %Z"),
      source      = cd$meta$source,
      n           = cd$meta$n,
      arms        = cd$meta$arms,
      n_by_arm    = cd$meta$n_by_arm,
      control     = cd$meta$control,
      covariates  = cd$meta$covariates,
      outcome     = outcome,
      outcome_type= cd$meta$outcome_types[[outcome]],
      focus_arm   = arm,
      audience    = cfg$report$audience,
      alpha       = cfg$design$alpha,
      correction  = cfg$design$multiple_comparison
    ),
    diagnosis  = dx,
    checks     = checks,
    experiment = exper,
    bayes      = bayes,
    benchmark  = bench
  )
}

# Headline sentences, generated from the numbers rather than written by hand.
# Used at the top of the report so the lead cannot drift from the results.
results_headlines <- function(res) {
  e <- res$experiment$estimates
  a <- e[e$arm == res$meta$focus_arm, ]
  b <- res$benchmark$benchmark
  naive <- b[b$estimator == "Naive comparison", ]
  adj   <- b[b$estimator != "Naive comparison", ]

  pretty_status <- function(s) {
    switch(s, pass = "passed", fail = "FAILED", "not applicable")
  }

  c(
    sprintf("The %s campaign changed %s by %+.4f (95%% CI %.4f to %.4f, adjusted p = %s).",
            a$arm, res$meta$outcome, a$estimate, a$ci_low, a$ci_high,
            ifelse(a$p_adjusted < 1e-4, "<0.0001", sprintf("%.4f", a$p_adjusted))),
    sprintf("The design could reliably detect effects of %.4f or larger; the observed effect is %s that floor.",
            a$mde, ifelse(abs(a$estimate) > 2 * a$mde, "comfortably above",
                          "close to")),
    sprintf("On a confounded sample with known truth, an unadjusted comparison erred by %+.1f%% while adjusted estimators fell within %.1f%%.",
            naive$pct_error, max(abs(adj$pct_error))),
    sprintf("Validity checks: %s.",
            paste(sprintf("%s %s",
                          vapply(res$checks, `[[`, character(1), "label"),
                          vapply(res$checks, function(ch) pretty_status(ch$status), character(1))),
                  collapse = "; "))
  )
}