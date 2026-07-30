# mod_validity.R — assumption checks, each with the explanation generated
# alongside it in R/. The app renders those strings and never writes its own,
# so the caveat cannot drift from the finding.

status_badge <- function(status) {
  cls <- switch(status,
                pass = "bg-success",
                fail = "bg-danger",
                "bg-secondary")
  lbl <- switch(status,
                pass = "pass",
                fail = "fail",
                "not applicable")
  span(class = paste("badge", cls), lbl)
}

mod_validity_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("cards")),
    card(
      card_header("Covariate balance detail"),
      p(class = "text-muted small",
        "Standardised mean difference against the control group. Values beyond the configured threshold indicate the groups differed before treatment."),
      tableOutput(ns("balance_tbl"))
    )
  )
}

mod_validity_server <- function(id, cd, cfg, checks, dx, arm, outcome) {
  moduleServer(id, function(input, output, session) {

    # Overlap requires a fitted propensity model, so it is only computed
    # when the propensity path is the recommended one.
    all_checks <- reactive({
      base <- checks()
      if (identical(dx()$recommended, "propensity")) {
        obs <- run_observational(cd(), cfg, arm = arm(), outcome = outcome())
        base$overlap <- obs$overlap
      }
      base
    })

    output$cards <- renderUI({
      items <- lapply(all_checks(), function(ch) {
        card(
          class = "mb-2",
          card_body(
            div(status_badge(ch$status), strong(paste0("  ", ch$label))),
            p(ch$message, class = "mb-0 mt-2 small")
          )
        )
      })
      do.call(tagList, unname(items))
    })

    output$balance_tbl <- renderTable({
      d <- all_checks()$balance$detail
      if (is.null(d)) return(NULL)
      d <- d[order(-abs(d$smd)), ]
      data.frame(
        Arm = d$arm,
        Covariate = d$covariate,
        SMD = sprintf("%+.4f", d$smd),
        Status = ifelse(d$imbalanced, "imbalanced", "balanced"),
        stringsAsFactors = FALSE
      )
    }, width = "100%")
  })
}