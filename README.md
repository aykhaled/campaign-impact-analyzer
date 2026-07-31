# Campaign Impact Analyzer

An R/Shiny tool that takes campaign or before/after data, determines which analysis method the data actually supports, estimates the effect with honest uncertainty, and states which assumptions it checked.

**Live app:** https://aykhaled-campaign-impact-analyzer.share.connect.posit.cloud/

The design constraint that drives everything: the tool refuses to produce an answer the data can't support, and raises validity warnings even when the estimate happens to look fine.

---

## Why this exists

Most campaign analysis stops at a p-value. That leaves three questions unanswered:

1. **Was this design capable of detecting the effect?** A null result from an underpowered test is not evidence of no effect.
2. **Which identifying assumption is this estimate resting on?** Randomisation, parallel trends, and conditional ignorability are not interchangeable.
3. **How wrong could an unadjusted comparison be?** Usually unknowable — unless you construct a case where the truth is known.

This tool answers all three.

---

## What it does

| Tab | Purpose |
|---|---|
| **Diagnose** | Reports which designs the data supports and which are ruled out, with reasons. Deliberately the first thing the user sees: method selection precedes results. |
| **Effect** | Point estimate, confidence interval, relative lift, multiplicity-adjusted p-value, and the minimum detectable effect. A Bayesian probability readout for binary outcomes. |
| **Validity** | Covariate balance, group sizes, parallel pre-trends, and common support — each pass/fail with a plain-language finding. |
| **Benchmark** | Naive, IPW, and AIPW estimates on a deliberately confounded sample, scored against the effect known from the underlying randomised experiment. |

---

## Data

### Primary: Hillstrom MineThatData E-Mail Analytics Challenge

64,000 customers randomly assigned in thirds to a men's-merchandise email campaign, a women's-merchandise campaign, or no email. Visit, conversion, and spend tracked over the following two weeks.

Released publicly by Kevin Hillstrom (MineThatData) for open analysis:
https://blog.minethatdata.com/2008/03/minethatdata-e-mail-analytics-and-data.html

Attribution is expected; no formal licence grant is attached to the dataset. Cited here accordingly.

**Fetching it** (the raw CSV is not committed):

```r
dir.create("data/raw", recursive = TRUE)
download.file(
  "http://www.minethatdata.com/Kevin_Hillstrom_MineThatData_E-MailAnalytics_DataMiningChallenge_2008.03.20.csv",
  "data/raw/hillstrom.csv"
)
```

A compressed copy ships in `inst/extdata/hillstrom.rds` so that the deployed app and a fresh clone both work without the fetch step. `R/data.R` prefers the configured CSV path and falls back to the `.rds`.

**Two deliberate choices about this data:**

- **`history_segment` is excluded from the covariate set.** It is a binned version of `history` — the same variable twice. Including both would double-count that dimension in the balance check and in any adjustment model.
- **`zip_code` contains the value "Surburban", misspelled in the source.** Left uncorrected. Quietly editing source data is a habit worth not having; the quirk is documented instead.

### Secondary: a constructed confounded sample

`make_confounded_sample()` in `R/observational.R` deliberately breaks the randomisation. A biased subsample is drawn where retention probability depends on `recency` (negative weight) and `log1p(history)` (positive weight), so treatment status becomes correlated with covariates that also drive the outcome. At the default strength it retains roughly half the two-arm data.

**This sample is manufactured, not observed.** It is stated as such in the app UI, in the generated report, and in the function's own metadata, which travels with the results.

It exists for one reason: because the true effect is known from the full randomised data, an observational estimator run on the confounded subsample can be *scored* rather than merely computed. That makes it a within-study comparison rather than a methods demonstration.

---

## Results

### Randomised estimates reproduce the published benchmark

| Arm | Outcome | Estimate | 95% CI | p (Holm) | MDE |
|---|---|---|---|---|---|
| Men's | visit | +7.66pp | 6.99 – 8.32 | <0.0001 | 0.95pp |
| Women's | visit | +4.52pp | — | <0.0001 | 0.91pp |
| Men's | spend | +0.770 | 0.485 – 1.054 | <0.0001 | 0.407 |
| Women's | spend | +0.424 | 0.169 – 0.680 | 0.0011 | 0.365 |

These match the figures published by Stochastic Solutions in 2008 (7.66pp / 4.52pp visit uplift; 77¢ / 42¢ spend uplift). That comparison is asserted in `tests/testthat/test-experiment.R` — the estimator is validated against a public benchmark, not against itself.

**Worth noting:** women's spend has an estimate of 0.424 against a detection floor of 0.365. Statistically significant, but close enough to the floor that the magnitude is imprecisely measured — the interval spans a fourfold range. The men's campaign has no such problem. This distinction is visible only in the MDE column.

### Observational estimators against known truth

Men's arm, confounded subsample (n = 21,333):

| Estimator | `visit` estimate | Error | CI covers truth |
|---|---|---|---|
| Naive comparison | 0.1118 | +46.0% | **No** |
| IPW | 0.0811 | +5.9% | Yes |
| AIPW (doubly robust) | 0.0859 | +12.1% | Yes |

True effect: 0.0766.

**The naive comparison is not merely wrong — it is confidently wrong.** Its confidence interval excludes the true value entirely. Nothing in that output signals a problem to the reader.

On `spend`, all three intervals cover the truth, including the naive one. That is not the estimator behaving well: `spend` is extremely noisy because most customers spend nothing, and wide intervals hide bias. The binary outcome is the sharper test.

IPW outperformed AIPW on `visit`; AIPW outperformed IPW on `spend`. Two outcomes is not evidence that either dominates — the gap is sampling variation. The defensible claim is that both adjusted estimators substantially corrected the bias while the naive one did not.

### Validity checks

On the clean randomised data: balance passes with a maximum standardised mean difference of 0.014 across 14 comparisons.

On the confounded subsample: overlap **fails** — 0.4% of units fall outside common support and the largest stabilised weight is 50.1.

That failure is the point. The adjustment worked well anyway, and the tool warned regardless. A tool tuned to demonstrate well would have suppressed it.

---

## Architecture

```
campaign-impact-analyzer/
├── R/
│   ├── data.R              load + validate; the ONLY module aware of client column names
│   ├── validate.R          balance, group size, pre-trend checks
│   ├── diagnose.R          method routing
│   ├── experiment.R        randomised analysis + conjugate Bayesian readout
│   ├── observational.R     confounding construction, propensity, IPW, AIPW, benchmark
│   └── report.R            assembles the results object for Quarto
├── app.R                   Shiny entry point (project root, not app/)
├── app/modules/            one Shiny module per tab
├── reports/                Quarto report source and a rendered sample
├── tests/testthat/
├── inst/extdata/           committed .rds fallback
├── config.yml
├── manifest.json           Connect Cloud dependency file
└── renv.lock
```

**Everything in `R/` is a pure function.** No Shiny dependency, no printing — takes data, returns a result object. The statistical engine is unit-testable without launching the app, and the same functions serve both the app and the Quarto report, so the two cannot disagree about a number.

**`app.R` lives at the project root, not in `app/`.** Shiny sets the working directory to the app's folder; keeping the entry point at the root means `config.yml` and `R/` resolve identically in development and on the deploy target.

**Validity explanations are generated inside each check** and travel with the result. The app and the report read the same string rather than each writing their own, so a caveat cannot drift from the finding it describes.

**`R/data.R` is the only schema-aware module.** It renames client columns to a canonical vocabulary (`treatment`, `period`, `unit_id`) once; everything downstream speaks only that vocabulary. This is what makes the configuration-driven claim true rather than aspirational.

---

## Configuration

Onboarding a different dataset is an edit to `config.yml`, not a code change:

```yaml
default:
  columns:
    treatment: segment
    period: null                  # absent — no pre/post structure
    covariates: [recency, history, mens, womens, newbie, zip_code, channel]
  outcomes:
    default: spend
    available:
      spend: continuous
      visit: binary
  design:
    control_label: "No E-Mail"
    alpha: 0.05
    multiple_comparison: holm
```

`period: null` is not a placeholder. It is the fact that causes `diagnose.R` to rule out difference-in-differences and explain why. The configuration is an input to the routing decision, not just to the analysis.

**Outcome types are declared and verified, never inferred.** Inferring "binary" from a 0/1 column is how a continuous variable that happens to be all zeros in a small sample ends up with a beta-binomial posterior fitted to it.

---

## Method notes

**Preference order among designs** reflects the strength of the identifying assumption rather than what is computable:

1. **Randomised** — known by design
2. **Difference-in-differences** — parallel trends is testable
3. **Propensity adjustment** — conditional ignorability is neither known nor testable; it is asserted

Propensity adjustment is perfectly computable on the clean randomised data, and running it would be a mistake — there is no confounding to remove, so adjustment adds variance and researcher degrees of freedom without reducing bias. The router says so explicitly rather than silently preferring one method.

**Difference-in-differences is implemented but not exercised by this dataset.** Hillstrom is cross-sectional: assignment, then outcomes over two weeks. There is no "before." DiD becomes available when a period column is configured.

**Minimum detectable effect, not post-hoc power.** Power computed from the effect you found is a restatement of the p-value. MDE derived from the standard error answers the question that matters: how large would an effect have needed to be for this design to detect it?

**Holm correction across three arms.** Uncorrected, the family-wise false-positive rate is nearer 14% than 5%.

**Conjugate Beta posteriors with Monte Carlo draws.** No Stan, no `brms`, no compiler — deploys anywhere. Seeded for reproducibility. The readout exists because "probability this beats control" is actionable for a decision-maker in a way that *p* < 0.05 is not.

**AIPW alongside IPW.** IPW is consistent only if the propensity model is correct; outcome regression only if the outcome model is. AIPW is consistent if either is — two chances rather than one.

---

## Running locally

```r
renv::restore()          # install the locked dependencies
shiny::runApp()          # launch the app
```

Render the report:

```r
quarto::quarto_render("reports/campaign-report.qmd")
```

The report is parameterised, so a different outcome produces a different document from the same source:

```bash
quarto render reports/campaign-report.qmd -P outcome:spend -P arm:"Womens E-Mail"
```

Run the tests:

```r
testthat::test_dir("tests/testthat")
```

---

## Deployment

Deployed to Posit Connect Cloud from this repository, with automatic republishing on push to `main`.

Connect Cloud reads `manifest.json` for the R version and package set — it does not use `renv` to build the environment, though `rsconnect::writeManifest()` captures dependencies from `renv.lock`. **Regenerate the manifest whenever files or dependencies change:**

```r
rsconnect::writeManifest()
```

`.rscignore` excludes `data`, `reports`, `tests`, and `renv` from the bundle. Note that `rsconnect` honours `.rscignore` and not `.gitignore`, and that its entries match names rather than paths — `data/raw` does not work as an entry, `data` does.

Free-tier content sleeps when idle. Cold-start the app before sharing the link.

---

## Environment

| | |
|---|---|
| R | 4.6.1 (managed with `rig`) |
| Dependencies | `renv` |
| Packages | shiny, bslib, ggplot2, config, rprojroot, knitr, quarto, testthat |
| IDE | Positron |
| Deployment | Posit Connect Cloud |

No Stan, no compiled Bayesian toolchain, and no interactive table widgets — every dependency is one the deploy target has to resolve, and the tables here are small enough that plain rendering is better.
