# mod_benchmark.R — within-study comparison.
# Always uses the full randomised data for ground truth and constructs the
# confounded subsample internally, so it does not follow the dataset selector.

mod_benchmark_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "alert alert-warning",
        strong("How this test is built. "),
        uiOutput(ns("construction"), inline = TRUE)),
    layout_columns(
      col_widths = c(7, 5),
      card(card_header("Estimator accuracy against known truth"),
           tableOutput(ns("tbl"))),
      card(card_header("Estimates and 95% intervals"),
           plotOutput(ns("plot"), height = "260px"))
    ),
    uiOutput(ns("overlap")),
    card(
      card_body(
        class = "text-muted small",
        "The naive comparison is what an analyst produces by comparing group means without adjustment. Because the true effect is known here, its error is measurable rather than hypothetical."
      )
    )
  )
}

mod_benchmark_server <- function(id, cd_full, cfg, arm, outcome) {
  moduleServer(id, function(input, output, session) {

    bm <- reactive({
      run_benchmark(cd_full, cfg, arm = arm(), outcome = outcome())
    })

    output$construction <- renderUI({
      m <- bm()$meta; s <- m$construction
      HTML(sprintf(
        "The full randomised sample (n = %s) gives the true effect. A biased subsample (n = %s) is then drawn where retention depends on %s, breaking randomisation on purpose. Each estimator is run on that subsample and scored against the truth. Seed %d, strength %.1f.",
        format(m$n_full, big.mark = ","), format(m$n_confounded, big.mark = ","),
        paste(names(s$drivers), collapse = " and "), s$seed, s$strength))
    })

    output$tbl <- renderTable({
      b <- bm()$benchmark
      data.frame(
        Estimator   = b$estimator,
        Estimate    = sprintf("%.4f", b$estimate),
        Truth       = sprintf("%.4f", b$truth),
        Error       = sprintf("%+.1f%%", b$pct_error),
        `95% CI`    = sprintf("%.4f – %.4f", b$ci_low, b$ci_high),
        `Covers truth` = ifelse(b$covers_truth, "yes", "NO"),
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }, width = "100%")

    output$plot <- renderPlot({
      b <- bm()
      df <- b$benchmark
      df$estimator <- factor(df$estimator, levels = rev(df$estimator))
      ggplot(df, aes(x = estimate, y = estimator)) +
        geom_vline(xintercept = b$truth$estimate, linetype = "dashed",
                   colour = "#d95f02", linewidth = 0.7) +
        geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.12, linewidth = 0.6) +
        geom_point(aes(colour = covers_truth), size = 3) +
        scale_colour_manual(values = c("TRUE" = "#2c7fb8", "FALSE" = "#d7301f"),
                            guide = "none") +
        labs(x = paste("Effect on", outcome()), y = NULL,
             caption = "Dashed line: true effect from the randomised data") +
        theme_minimal(base_size = 13) +
        theme(panel.grid.major.y = element_blank())
    })

    output$overlap <- renderUI({
      ch <- bm()$overlap
      div(class = if (identical(ch$status, "fail")) "alert alert-danger" else "alert alert-success",
          status_badge(ch$status), strong(paste0("  ", ch$label)),
          p(ch$message, class = "mb-0 mt-2 small"))
    })
  })
}