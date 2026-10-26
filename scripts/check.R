source("R/paths.R")
source("R/acquisition.R")
source("R/elo.R")
source("R/visualization.R")
source("R/pipeline.R")
files <- c(
  list.files("R", pattern = "\\.R$", full.names = TRUE),
  list.files("tests/testthat", pattern = "\\.R$", full.names = TRUE),
  "scripts/pipeline.R", "scripts/prepare_matches.R", "scripts/check.R", "scripts/check_site.R"
)
lints <- unlist(lapply(files, lintr::lint), recursive = FALSE)
if (length(lints)) {
  print(lints)
  stop("Lint checks failed")
}
