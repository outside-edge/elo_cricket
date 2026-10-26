sha256 <- function(path) digest::digest(file = path, algo = "sha256")

write_json <- function(value, path) {
  jsonlite::write_json(value, path, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)
}

code_fingerprint <- function() {
  paths <- sort(c(list.files("R", pattern = "\\.R$", full.names = TRUE), "scripts/pipeline.R", "renv.lock"))
  if (length(paths) < 3L || !all(file.exists(paths))) stop("Run from the repository root to record code provenance")
  as.list(setNames(vapply(paths, sha256, character(1)), paths))
}

replace_directory <- function(output, write) {
  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  backup <- paste0(output, ".previous")
  if (dir.exists(backup)) stop("Previous output needs recovery: ", backup)
  staging <- tempfile("stage-", tmpdir = dirname(output))
  dir.create(staging)
  on.exit(unlink(staging, recursive = TRUE), add = TRUE)
  value <- write(staging)
  if (dir.exists(output) && !file.rename(output, backup)) stop("Cannot preserve previous output")
  if (!file.rename(staging, output)) {
    if (dir.exists(backup)) file.rename(backup, output)
    stop("Cannot replace output directory")
  }
  unlink(backup, recursive = TRUE)
  invisible(value)
}

prepare_data <- function(raw_dir, output, as_of, require_fresh = FALSE, coverage = formats) {
  data <- load_matches(raw_dir, as_of, require_fresh, coverage)
  if (!setequal(unique(data$matches$format), coverage$format)) stop("Missing a format in prepared data")
  replace_directory(output, function(directory) {
    readr::write_csv(data$matches, file.path(directory, "matches.csv"), na = "")
    manifest <- list(
      schema_version = 1L, as_of = as.character(as_of),
      match_counts = as.list(table(data$matches$format)),
      last_result = lapply(split(data$matches$end_date, data$matches$format), function(dates) as.character(max(dates))),
      outcome_counts = as.list(table(data$matches$result)), sources = data$sources,
      matches_sha256 = sha256(file.path(directory, "matches.csv")), code_sha256 = code_fingerprint()
    )
    write_json(manifest, file.path(directory, "manifest.json"))
    manifest
  })
}

read_prepared <- function(directory) {
  manifest <- jsonlite::read_json(file.path(directory, "manifest.json"), simplifyVector = FALSE)
  path <- file.path(directory, "matches.csv")
  if (!identical(sha256(path), manifest$matches_sha256)) stop("Prepared data checksum mismatch")
  matches <- readr::read_csv(path, col_types = readr::cols(
    match_id = readr::col_integer(), format = readr::col_character(),
    start_date = readr::col_date(), end_date = readr::col_date(),
    team1_id = readr::col_integer(), team1 = readr::col_character(),
    team2_id = readr::col_integer(), team2 = readr::col_character(),
    result = readr::col_character(), score = readr::col_double(), source_url = readr::col_character()
  ), show_col_types = FALSE)
  if (nrow(readr::problems(matches))) stop("Invalid prepared data types")
  if (anyDuplicated(matches$match_id)) stop("Duplicate prepared match IDs")
  if (any(matches$end_date > as.Date(manifest$as_of))) stop("Prepared data exceed cutoff")
  list(matches = as.data.frame(matches), manifest = manifest)
}

analyze_data <- function(input, output, model = "elo") {
  if (model != "elo") stop("No verified pipeline adapter for model: ", model)
  data <- read_prepared(input)
  result <- calculate_elo(data$matches, as.Date(data$manifest$as_of))
  replace_directory(output, function(directory) {
    for (name in c("latest", "monthly", "updates")) {
      readr::write_csv(result[[name]], file.path(directory, paste0(name, ".csv")), na = "")
    }
    saveRDS(result$models, file.path(directory, "fits.rds"))
    metadata <- list(
      model = model, as_of = data$manifest$as_of,
      input_sha256 = data$manifest$matches_sha256, specification = result$specification,
      last_result = lapply(split(data$matches$end_date, data$matches$format), function(dates) as.character(max(dates))),
      r_version = as.character(getRversion()),
      code_sha256 = code_fingerprint(),
      files = as.list(setNames(vapply(
        file.path(directory, paste0(c("latest", "monthly", "updates"), ".csv")),
        sha256, character(1)
      ), paste0(c("latest", "monthly", "updates"), ".csv")))
    )
    write_json(metadata, file.path(directory, "manifest.json"))
    metadata
  })
}

read_analysis <- function(directory) {
  manifest <- jsonlite::read_json(file.path(directory, "manifest.json"), simplifyVector = FALSE)
  for (name in names(manifest$files)) {
    if (!identical(sha256(file.path(directory, name)), manifest$files[[name]])) {
      stop("Analysis checksum mismatch: ", name)
    }
  }
  read <- function(name) {
    readr::read_csv(file.path(directory, paste0(name, ".csv")),
      col_types = readr::cols(), show_col_types = FALSE
    )
  }
  list(manifest = manifest, latest = read("latest"), monthly = read("monthly"), updates = read("updates"))
}

chart_data <- function(latest, monthly, format_name) {
  leaders <- head(latest[latest$format == format_name & latest$is_active, ], 10)
  history <- monthly[monthly$format == format_name & monthly$team_id %in% leaders$team_id, ]
  history$team <- leaders$team[match(history$team_id, leaders$team_id)]
  history
}

visualize_data <- function(input, output) {
  data <- read_analysis(input)
  .data <- rlang::.data
  replace_directory(output, function(directory) {
    for (format in unique(data$latest$format)) {
      history <- chart_data(data$latest, data$monthly, format)
      chart <- ggplot2::ggplot(history, ggplot2::aes(x = .data$date, y = .data$rating, colour = .data$team)) +
        ggplot2::geom_line(linewidth = 0.45) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::labs(
          title = paste(format, toupper(data$manifest$model), "ratings"),
          subtitle = paste("Month-end history of the current top ten active teams · through", data$manifest$as_of),
          x = NULL, y = "Rating", colour = NULL,
          caption = "Completed matches only; no smoothing or interpolation. Independent of ICC rankings."
        ) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
      ggplot2::ggsave(file.path(directory, paste0(tolower(format), ".png")), chart, width = 11, height = 6, dpi = 150)
    }
    manifest <- list(
      analysis_sha256 = sha256(file.path(input, "manifest.json")),
      code_sha256 = code_fingerprint(),
      files = as.list(setNames(
        vapply(list.files(directory, full.names = TRUE), sha256, character(1)),
        list.files(directory)
      ))
    )
    write_json(manifest, file.path(directory, "manifest.json"))
    manifest
  })
}

write_report <- function(data, path) {
  lines <- c(
    paste0("# ", toupper(data$manifest$model), " cricket ratings through ", data$manifest$as_of), "",
    "Independent ratings of men's international teams. Results enter on their completion date.", "",
    "[Latest ratings](latest.csv) · [Monthly history](monthly.csv) · [Match updates](updates.csv)", "",
    "[Methods and limitations](../../../README.md#elo-specification)", ""
  )
  for (format in unique(data$latest$format)) {
    rows <- data$latest[data$latest$format == format & data$latest$is_active, ]
    label <- paste0(rows$team, ifelse(rows$is_provisional, " (provisional)", ""))
    lines <- c(
      lines, paste0("## ", format), "",
      paste("Most recent recorded result:", data$manifest$last_result[[format]]), "",
      "| Rank | Team | Rating | Rated matches | Last rated match |",
      "| ---: | --- | ---: | ---: | --- |",
      sprintf("| %s | %s | %s | %s | %s |", rows$rank, label, rows$rating, rows$rated_matches, rows$last_rated_match),
      "", paste0("![", format, " rating history](", tolower(format), ".png)"), ""
    )
  }
  writeLines(head(lines, -1L), path, useBytes = TRUE)
}

publish_data <- function(prepared, analysis, figures, output, require_fresh = TRUE, now = Sys.time()) {
  source <- read_prepared(prepared)
  data <- read_analysis(analysis)
  if (
    !identical(source$manifest$matches_sha256, data$manifest$input_sha256) ||
      !identical(source$manifest$as_of, data$manifest$as_of)
  ) {
    stop("Analysis uses different prepared data")
  }
  visual <- jsonlite::read_json(file.path(figures, "manifest.json"), simplifyVector = FALSE)
  code <- code_fingerprint()
  for (manifest in list(source$manifest, data$manifest, visual)) {
    if (!identical(manifest$code_sha256, code)) stop("Stage code differs; rebuild all stages before publication")
  }
  if (!identical(visual$analysis_sha256, sha256(file.path(analysis, "manifest.json")))) {
    stop("Figures use different analysis")
  }
  for (name in names(visual$files)) {
    if (!identical(sha256(file.path(figures, name)), visual$files[[name]])) stop("Figure checksum mismatch")
  }
  if (require_fresh) {
    age <- as.integer(as.Date(now, tz = "UTC") - as.Date(data$manifest$as_of))
    if (is.na(age) || age < 1L || age > 8L) stop("Publication cutoff is stale or incomplete")
    for (receipt in source$manifest$sources) {
      validate_source_freshness(receipt, as.Date(data$manifest$as_of), now)
    }
  }
  replace_directory(output, function(directory) {
    inputs <- c(
      file.path(analysis, c("latest.csv", "monthly.csv", "updates.csv")),
      file.path(figures, names(visual$files))
    )
    if (!all(file.copy(inputs, directory))) stop("Cannot stage publication")
    write_report(data, file.path(directory, "index.md"))
    manifest <- list(
      analysis = data$manifest, source = source$manifest, visualization = visual,
      code_sha256 = code_fingerprint(),
      files = as.list(setNames(
        vapply(list.files(directory, full.names = TRUE), sha256, character(1)),
        list.files(directory)
      ))
    )
    write_json(manifest, file.path(directory, "manifest.json"))
    manifest
  })
}
