# mod_diagnose.R — data shape and method routing.
# Deliberately the first tab: the user sees which methods the data supports
# before they see any estimate.

mod_diagnose_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Data"),
        tableOutput(ns("shape")),
        uiOutput(ns("construction"))
      ),
      card(
        card_header("Recommended analysis"),
        uiOutput(ns("recommendation"))
      )
    ),
    card(
      card_header("Designs considered"),
      uiOutput(ns("designs"))
    )
  )
}

mod_diagnose_server <- function(id, cd, dx) {
  moduleServer(id, function(input, output, session) {

    output$shape <- renderTable({
      m <- cd()$meta
      data.frame(
        Field = c("Observations", "Groups", "Control", "Covariates",
                  "Period column", "Unit ID column"),
        Value = c(format(m$n, big.mark = ","),
                  paste(sprintf("%s (%s)", m$arms, format(m$n_by_arm, big.mark = ",")),
                        collapse = "; "),
                  m$control,
                  paste(m$covariates, collapse = ", "),
                  if (m$has_period) "present" else "absent",
                  if (m$has_unit_id) "present" else "absent"),
        stringsAsFactors = FALSE
      )
    }, colnames = FALSE, width = "100%")

    output$construction <- renderUI({
      syn <- cd()$meta$synthetic
      if (is.null(syn)) return(NULL)
      div(
        class = "alert alert-warning mt-2 mb-0",
        strong("Constructed sample. "),
        sprintf(
          "Randomisation was deliberately broken by biased subsampling on %s (strength %.1f, seed %d) so that an observational estimator can be tested against a known truth. This is not observational data found in the wild.",
          paste(names(syn$drivers), collapse = " and "), syn$strength, syn$seed)
      )
    })

    output$recommendation <- renderUI({
      d <- dx()
      if (d$refused) {
        return(div(class = "alert alert-danger mb-0",
                   strong("No supported design. "), d$rationale))
      }
      div(
        h4(d$designs[[d$recommended]]$label, class = "mb-2"),
        p(d$rationale, class = "mb-0 text-muted")
      )
    })

    output$designs <- renderUI({
      d <- dx()
      rows <- lapply(d$designs, function(x) {
        div(
          class = "mb-3",
          span(class = if (x$supported) "badge bg-success" else "badge bg-secondary",
               if (x$supported) "supported" else "ruled out"),
          strong(paste0("  ", x$label)),
          p(x$reason, class = "mb-0 mt-1 text-muted small")
        )
      })
      do.call(tagList, unname(rows))
    })
  })
}