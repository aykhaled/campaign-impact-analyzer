# Non-package project: load R/ explicitly before tests run.
# Paths in config.yml are relative to the project root, but testthat sets the
# working directory to tests/testthat/ — so resolve them here.

test_root <- rprojroot::find_root(rprojroot::has_file("config.yml"))

for (f in list.files(file.path(test_root, "R"),
                     pattern = "[.][Rr]$", full.names = TRUE)) {
  source(f)
}

cfg_test <- config::get(file = file.path(test_root, "config.yml"))
cfg_test$data$path <- file.path(test_root, cfg_test$data$path)