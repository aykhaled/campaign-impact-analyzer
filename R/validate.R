# diagnose.R — method routing. Given data, config, and validity checks,
# determine which designs the data supports, which it rules out, and why.
# Availability and appropriateness are separate judgements: a method can be
# computable on this data and still be the wrong one to use.

new_design <- function(id, label, supported, reason) {
  structure(list(id = id, label = label, supported = supported, reason = reason),
            class = "design_option")
}

# ---- randomised experiment ------------------------------------------------

assess_randomised <- function(cd, cfg, checks) {
  id <- "randomised"; label <- "Randomised experiment"

  if (length(cd$meta$arms) < 2) {
    return(new_design(id, label, FALSE,
      "Fewer than two treatment groups are present, so there is nothing to compare."))
  }
  if (identical(checks$group_size$status, "fail")) {
    return(new_design(id, label, FALSE,
      paste("At least one group is too small to estimate an effect.",
            checks$group_size$message)))
  }
  if (identical(checks$balance$status, "fail")) {
    return(new_design(id, label, FALSE,
      paste("Covariate balance fails, so the groups cannot be treated as randomly assigned.",
            "A simple comparison of group means would confound the treatment effect with pre-existing differences.")))
  }
  if (identical(checks$balance$status, "not_applicable")) {
    return(new_design(id, label, TRUE,
      "No covariates are declared, so randomisation is assumed rather than verified. The estimate is only as trustworthy as that assumption."))
  }
  new_design(id, label, TRUE,
    "Treatment groups are balanced on all declared covariates, consistent with random assignment. Differences in outcomes can be attributed to the treatment.")
}

# ---- propensity-based adjustment (cross-sectional observational) ----------

assess_propensity <- function(cd, cfg, checks) {
  id <- "propensity"; label <- "Propensity-adjusted comparison"

  if (length(cd$meta$arms) < 2) {
    return(new_design(id, label, FALSE, "Fewer than two treatment groups are present."))
  }
  if (!length(cd$meta$covariates)) {
    return(new_design(id, label, FALSE,
      "No covariates are declared. Adjustment requires the variables that drive group membership."))
  }
  if (identical(checks$group_size$status, "fail")) {
    return(new_design(id, label, FALSE,
      "At least one group is too small to fit a propensity model reliably."))
  }
  new_design(id, label, TRUE,
    "Covariates are available to model group membership and adjust for measured differences. This corrects for imbalance in the variables you observed; it cannot correct for anything you did not measure.")
}

# ---- difference-in-differences --------------------------------------------

assess_did <- function(cd, cfg, checks) {
  id <- "did"; label <- "Difference-in-differences"

  if (!cd$meta$has_period) {
    return(new_design(id, label, FALSE,
      "No period column is configured. Difference-in-differences compares change over time between groups, which requires observations from before and after the intervention."))
  }
  per_arm <- tapply(cd$data$period, cd$data$treatment,
                    function(p) length(unique(p)))
  if (any(per_arm < 2)) {
    return(new_design(id, label, FALSE,
      "At least one group is observed in only one period, so its change over time cannot be measured."))
  }
  new_design(id, label, TRUE,
    "Both groups are observed across multiple periods, so their trends can be compared. Validity additionally depends on the pre-treatment trends being parallel.")
}

# ---- routing --------------------------------------------------------------

diagnose_design <- function(cd, cfg, checks) {
  designs <- list(
    randomised = assess_randomised(cd, cfg, checks),
    did        = assess_did(cd, cfg, checks),
    propensity = assess_propensity(cd, cfg, checks)
  )
  supported <- names(designs)[vapply(designs, `[[`, logical(1), "supported")]

  # Preference order reflects the strength of the identifying assumption,
  # not what is computable. Randomisation is known by design; parallel trends
  # is testable; conditional ignorability is neither.
  recommended <- if ("randomised" %in% supported) "randomised"
                 else if ("did" %in% supported)   "did"
                 else if ("propensity" %in% supported) "propensity"
                 else NA_character_

  rationale <- if (is.na(recommended)) {
    "No design is supported by this data. See the reasons against each method below."
  } else if (recommended == "randomised" && "propensity" %in% supported) {
    paste("Randomised analysis is recommended. Propensity adjustment is also computable here,",
          "but adjusting a balanced experiment adds variance without removing bias — there is no",
          "confounding left for it to correct.")
  } else {
    designs[[recommended]]$reason
  }

  list(
    designs     = designs,
    supported   = supported,
    recommended = recommended,
    refused     = is.na(recommended),
    rationale   = rationale
  )
}

design_summary <- function(dx) {
  data.frame(
    design    = vapply(dx$designs, `[[`, character(1), "label"),
    supported = vapply(dx$designs, `[[`, logical(1), "supported"),
    reason    = vapply(dx$designs, `[[`, character(1), "reason"),
    stringsAsFactors = FALSE, row.names = NULL
  )
}