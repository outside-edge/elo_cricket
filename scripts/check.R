source("R/paths.R")
source("R/acquisition.R")
source("R/elo.R")
source("R/visualization.R")
source("R/pipeline.R")
for (module in list.files("R/models", pattern = "\\.R$", full.names = TRUE)) source(module)
files <- c(
  list.files("R", pattern = "\\.R$", full.names = TRUE, recursive = TRUE),
  list.files("tests/testthat", pattern = "\\.R$", full.names = TRUE),
  "scripts/pipeline.R", "scripts/prepare_matches.R", "scripts/check.R", "scripts/check_site.R",
  "scripts/models.R", "scripts/check_models.R"
)
lints <- unlist(lapply(files, lintr::lint), recursive = FALSE)
if (length(lints)) {
  print(lints)
  stop("Lint checks failed")
}
