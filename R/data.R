# data.R — load and validate campaign data against the config schema.
# The ONLY module aware of client column names. Everything downstream
# uses the canonical vocabulary: treatment / period / unit_id.

# Resolves the data source. Prefers the configured path (the real CSV in
# development); falls back to a committed .rds so the deployed app is
# self-contained without shipping raw data.
resolve_source <- function(cfg, path = NULL) {
  candidate <- path %||% cfg$data$path
  if (!is.null(candidate) && file.exists(candidate)) return(candidate)

  fallback <- tryCatch({
    root <- rprojroot::find_root(rprojroot::has_file("config.yml"))
    file.path(root, "inst", "extdata", "hillstrom.rds")
  }, error = function(e) file.path("inst", "extdata", "hillstrom.rds"))

  if (file.exists(fallback)) return(fallback)

  stop("No data source found. Looked for '", candidate, "' and '", fallback,
       "'. See the README for the dataset fetch step.", call. = FALSE)
}

read_source <- function(path) {
  if (grepl("[.]rds$", path, ignore.case = TRUE)) {
    readRDS(path)
  } else {
    utils::read.csv(path, stringsAsFactors = FALSE)
  }
}

load_campaign_data <- function(cfg, path = NULL) {

  path <- resolve_source(cfg, path)
  raw  <- read_source(path)

  # ---- structural columns -------------------------------------------------
  treat_col <- cfg$columns$treatment
  if (is.null(treat_col)) {
    stop("config: columns$treatment is required.", call. = FALSE)
  }
  require_cols(raw, treat_col, "treatment")

  period_col  <- cfg$columns$period
  unit_col    <- cfg$columns$unit_id
  if (!is.null(period_col)) require_cols(raw, period_col, "period")
  if (!is.null(unit_col))   require_cols(raw, unit_col,   "unit_id")

  # ---- covariates and outcomes -------------------------------------------
  covariates <- cfg$columns$covariates %||% character(0)
  require_cols(raw, covariates, "covariate")

  outcome_types <- cfg$outcomes$available
  require_cols(raw, names(outcome_types), "outcome")

  for (nm in names(outcome_types)) {
    check_outcome_type(raw[[nm]], nm, outcome_types[[nm]])
  }

  # ---- canonical renaming -------------------------------------------------
  dat <- raw
  names(dat)[names(dat) == treat_col] <- "treatment"
  if (!is.null(period_col)) names(dat)[names(dat) == period_col] <- "period"
  if (!is.null(unit_col))   names(dat)[names(dat) == unit_col]   <- "unit_id"

  # Control becomes the reference level so every model contrast is
  # "arm vs control" without further specification.
  control <- cfg$design$control_label
  arms <- unique(dat$treatment)
  if (!control %in% arms) {
    stop("control_label '", control, "' not found in ", treat_col,
         ". Observed: ", paste(arms, collapse = ", "), call. = FALSE)
  }
  dat$treatment <- factor(dat$treatment, levels = c(control, setdiff(arms, control)))

  list(
    data = dat,
    meta = list(
      source        = basename(path),
      n             = nrow(dat),
      arms          = levels(dat$treatment),
      control       = control,
      n_by_arm      = as.integer(table(dat$treatment)),
      covariates    = covariates,
      outcome_types = outcome_types,
      has_period    = !is.null(period_col),
      has_unit_id   = !is.null(unit_col)
    )
  )
}

# ---- helpers --------------------------------------------------------------

`%||%` <- function(x, y) if (is.null(x)) y else x

require_cols <- function(df, cols, role) {
  missing <- setdiff(cols, names(df))
  if (length(missing)) {
    stop("Missing ", role, " column(s): ", paste(missing, collapse = ", "),
         "\nAvailable: ", paste(names(df), collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

check_outcome_type <- function(x, nm, declared) {
  if (identical(declared, "binary")) {
    vals <- unique(stats::na.omit(x))
    if (!all(vals %in% c(0, 1))) {
      stop("Outcome '", nm, "' is declared binary but contains: ",
           paste(utils::head(sort(vals), 5), collapse = ", "), call. = FALSE)
    }
  } else if (identical(declared, "continuous")) {
    if (!is.numeric(x)) {
      stop("Outcome '", nm, "' is declared continuous but is ", class(x)[1],
           ".", call. = FALSE)
    }
  } else {
    stop("Unknown outcome type '", declared, "' for '", nm,
         "'. Use 'binary' or 'continuous'.", call. = FALSE)
  }
  invisible(TRUE)
}