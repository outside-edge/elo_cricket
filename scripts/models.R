source("R/acquisition.R")
source("R/elo.R")
source("R/pipeline.R")
for (module in list.files("R/models", pattern = "\\.R$", full.names = TRUE)) source(module)

args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) {
  stop("Usage: models.R prepare | compare [format] [origin] [until] [iterations] | visualize <output-dir>")
}
if (args[1] == "prepare") {
  prepare_model_context()
} else if (args[1] == "compare") {
  format <- if (length(args) >= 2L) args[2] else "Test"
  origin <- as.Date(if (length(args) >= 3L) args[3] else "2024-01-01")
  until <- as.Date(if (length(args) >= 4L) args[4] else "2025-01-01")
  iterations <- if (length(args) >= 5L) as.integer(args[5]) else 1000L
  run_model_comparison(format, origin, until, iterations)
} else if (args[1] == "visualize" && length(args) == 2L) {
  plot_model_strengths(args[2])
} else {
  stop("Unknown model command: ", args[1])
}
