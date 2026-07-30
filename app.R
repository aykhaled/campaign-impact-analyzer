# app.R — Campaign Impact Analyzer
# Lives at the project root so that config.yml and R/ resolve identically
# in development and on shinyapps.io.

library(shiny)
library(bslib)
library(ggplot2)

root <- rprojroot::find_root(rprojroot::has_file("config.yml"))

for (f in list.files(file.path(root, "R"), pattern = "[.][Rr]$", full.names = TRUE)) {
  source(f)
}
for (f in list.files(file.path(root, "app", "modules"), pattern = "[.][Rr]$", full.names = TRUE)) {
  source(f)
}

cfg <- config::get(file = file.path(root, "config.yml"))
cfg$data$path <- file.path(root, cfg$data$path)

cd_full  <- load_campaign_data(cfg)
arm_opts <- setdiff(cd_full$meta$arms, cd_full$meta$control)

ui <- page_navbar(
  title = "Campaign Impact Analyzer",
  theme = bs_theme(version = 5, preset = "flatly"),
  sidebar = sidebar(
    width = 280,
    selectInput("dataset", "Dataset",
                c("Full randomised" = "full",
                  "Confounded subsample" = "confounded")),
    selectInput("arm", "Treatment arm", arm_opts),
    selectInput("outcome", "Outcome", names(cfg$outcomes$available),
                selected = cfg$outcomes$default),
    hr(),
    p(class = "small text-muted",
      "The confounded subsample breaks randomisation on purpose so the observational estimator can be tested against a known effect."),
    p(class = "small text-muted",
      "The Benchmark tab uses both datasets and does not follow this selector.")
  ),
  nav_panel("Diagnose",  mod_diagnose_ui("diagnose")),
  nav_panel("Effect",    mod_effect_ui("effect")),
  nav_panel("Validity",  mod_validity_ui("validity")),
  nav_panel("Benchmark", mod_benchmark_ui("benchmark"))
)

server <- function(input, output, session) {

  cd_active <- reactive({
    if (identical(input$dataset, "full")) {
      cd_full
    } else {
      make_confounded_sample(cd_full, cfg, arm = input$arm)
    }
  })

  checks <- reactive(run_validity_checks(cd_active(), cfg))
  dx     <- reactive(diagnose_design(cd_active(), cfg, checks()))

  mod_diagnose_server("diagnose", cd = cd_active, dx = dx)

  mod_effect_server("effect", cd = cd_active, cfg = cfg, dx = dx,
                    outcome = reactive(input$outcome),
                    arm     = reactive(input$arm))

  mod_validity_server("validity", cd = cd_active, cfg = cfg,
                      checks = checks, dx = dx,
                      arm = reactive(input$arm), outcome = reactive(input$outcome))

  mod_benchmark_server("benchmark", cd_full = cd_full, cfg = cfg,
                       arm = reactive(input$arm), outcome = reactive(input$outcome))
}

shinyApp(ui, server)