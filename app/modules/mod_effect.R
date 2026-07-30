# mod_effect.R — effect estimates for whichever design the router recommended.
# Randomised → per-arm contrasts vs control, with MDE and (for binary
# outcomes) a Bayesian readout. Propensity → naive / IPW / AIPW side by side.

mod_effect_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("banner")),
    layout_columns(
      col_widths = c(7, 5),
      card(card_header("Estimates"), tableOutput(ns("tbl"))),
      card(card_header("Effect and 95% interval"), plotOutput(ns("plot"), height = "260px"))
    ),
    uiOutput(ns("bayes"))
  )
}

mod_effect_server <- function(id, cd, cfg, dx, outcome, arm) {
  moduleServer(id, function(input, output, session) {

    fit <- reactive({
      d <- dx()
      if (d$refused) return(NULL)
      if (identical(d$recommended, "randomised")) {
        list(kind = "randomised", res = run_experiment(cd(), cfg, outcome = outcome()))
      } else {
        list(kind = "propensity",
             res = run_observational(cd(), cfg, arm = arm(), outcome = outcome()))
      }
    })

    output$banner <- renderUI({
      d <- dx()
      if (d$refused) {
        return(div(class = "alert alert-danger", strong("No estimate produced. "), d$rationale))
      }
      div(class = "alert alert-info",
          strong(paste0(d$designs[[d$recommended]]$label, ". ")),
          sprintf("Outcome: %s. Control: %s.", outcome(), cd()$meta$control))
    })

    plot_df <- reactive({
      f <- fit(); if (is.null(f)) return(NULL)
      e <- f$res$estimates
      data.frame(
        label = if (f$kind == "randomised") e$arm else e$estimator,
        estimate = e$estimate, ci_low = e$ci_low, ci_high = e$ci_high,
        stringsAsFactors = FALSE
      )
    })

    output$tbl <- renderTable({
      f <- fit(); if (is.null(f)) return(NULL)
      e <- f$res$estimates
      if (f$kind == "randomised") {
        data.frame(
          Arm       = e$arm,
          Estimate  = sprintf("%.4f", e$estimate),
          `95% CI`  = sprintf("%.4f – %.4f", e$ci_low, e$ci_high),
          `Rel lift`= ifelse(is.na(e$rel_lift), "—", sprintf("%+.1f%%", 100 * e$rel_lift)),
          `p (adj)` = ifelse(e$p_adjusted < 1e-4, "<0.0001", sprintf("%.4f", e$p_adjusted)),
          MDE       = sprintf("%.4f", e$mde),
          check.names = FALSE, stringsAsFactors = FALSE
        )
      } else {
        data.frame(
          Estimator = e$estimator,
          Estimate  = sprintf("%.4f", e$estimate),
          `95% CI`  = sprintf("%.4f – %.4f", e$ci_low, e$ci_high),
          SE        = sprintf("%.4f", e$se),
          check.names = FALSE, stringsAsFactors = FALSE
        )
      }
    }, width = "100%")

    output$plot <- renderPlot({
      df <- plot_df(); if (is.null(df)) return(NULL)
      ggplot(df, aes(x = estimate, y = label)) +
        geom_vline(xintercept = 0, linetype = "dashed", colour = "grey55") +
        geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.12, linewidth = 0.6) +
        geom_point(size = 3, colour = "#2c7fb8") +
        labs(x = paste("Effect on", outcome()), y = NULL) +
        theme_minimal(base_size = 13) +
        theme(panel.grid.major.y = element_blank())
    })

    output$bayes <- renderUI({
      f <- fit(); if (is.null(f) || f$kind != "randomised") return(NULL)
      pb <- posterior_binary(cd(), cfg, outcome())
      if (is.null(pb)) {
        return(div(class = "text-muted small mt-2",
                   sprintf("Bayesian readout applies to binary outcomes; '%s' is continuous.", outcome())))
      }
      p <- pb$posteriors
      card(
        card_header("Bayesian readout"),
        renderTable({
          data.frame(
            Arm = p$arm,
            `P(beats control)` = sprintf("%.3f", p$prob_beats_control),
            `Median difference` = sprintf("%.4f", p$diff_median),
            `95% credible interval` = sprintf("%.4f – %.4f", p$diff_low, p$diff_high),
            check.names = FALSE, stringsAsFactors = FALSE
          )
        }, width = "100%"),
        div(class = "text-muted small",
            sprintf("%s prior, %s draws, seed %d. Reported as a probability because that is what a decision-maker can act on.",
                    pb$meta$prior, format(pb$meta$draws, big.mark = ","), pb$meta$seed))
      )
    })
  })
}