# Cricket team ratings

A shared data pipeline for developing and publishing separate Elo, Glickman,
and Stan-based ratings of international cricket teams. Acquisition, match
preparation, model fitting, visualization, and publication run independently.
R orchestrates the pipeline; Stan remains the language for its model definitions.

The maintained adapter currently calculates **Elo** for men's Tests, ODIs, and
T20Is. The existing Glickman and Stan analyses remain research code. They are not
silently replaced with Elo, and they are not yet scheduled for publication.

[Latest Elo tables](out/release/elo/index.md) ·
[Latest ratings](out/release/elo/latest.csv) ·
[Monthly history](out/release/elo/monthly.csv) ·
[Prepared matches](data/build/matches.csv) ·
[Correctness audit](docs/correctness.md)

## Run

Install R 4.6.0, then run from the repository root:

```sh
make sync
make check
make update
```

`renv.lock` pins the R dependencies. The first R invocation bootstraps `renv`;
`make sync` restores the project library. `make update` acquires missing historical
years, refreshes the current and previous year, and builds a publication through
yesterday. All subsequent analysis and rendering use saved data.

```sh
make fetch
make prepare AS_OF=2025-12-31
make analyze
make visualize
make publish
```

The date belongs to acquisition/preparation. Analysis, figures, and publication
inherit the cutoff from their input manifests. `make publish` requires a recent
cutoff and source receipts; use the R functions with `require_fresh = FALSE` only
for an explicitly historical local publication. `make update FLAGS=--refresh`
also revisits older source years to incorporate historical corrections.

| Stage | Output | Purpose |
| --- | --- | --- |
| Fetch | `data/raw/<format>/<year>.html.gz` and JSON receipts | Preserve source responses and retrieval metadata |
| Prepare | `data/build/matches.csv`, `manifest.json` | Validate identities, dates, outcomes, coverage, and duplicate agreement |
| Analyze | `out/analysis/<model>/` | Fit each model independently; save ratings, diagnostics, and specification |
| Visualize | `figs/<model>/` | Render saved model outputs without fetching or refitting |
| Publish | `out/release/<model>/` | Assemble verified tables, charts, and provenance |

The implemented adapter writes under `elo/`. Future Glickman and Stan adapters
should use their own directories and model identifiers. They can have different
rating scales, fitting periods, covariates, and uncertainty summaries. The shared
match table preserves `drawn`, `tied`, and no-result outcomes separately; its
`score` is a convenient 1/0.5/0 encoding, not a prescribed likelihood for every
model. Additional covariates need their own documented acquisition and joins by
match/team ID. The annual results source does not supply toss or umpire covariates.

## Data

The source is ESPNcricinfo's annual [match-results tables](https://stats.espncricinfo.com/ci/engine/records/team/match_results.html?class=1;id=2025;type=year).
Collection starts in 1877 for Tests, 1971 for ODIs, and 2005 for T20Is. Coverage is
the set of matches listed there; it has not been independently certified as a census.

Each receipt records the requested URL, retrieval time in UTC, SHA-256 of the
uncompressed response, format, year, and row count. `httr2` provides HTTP retries
and throttling, including `Retry-After`; `rvest` and `xml2` parse saved pages.
Verified historical downloads are reused. A stopped download can resume.

Missing years, unexpected tables, unknown outcomes, invalid winners, pagination,
conflicting duplicate IDs, and checksum failures stop preparation. An explicit
“No records found” response is required for an empty year. Cross-year matches
are deduplicated only when their records agree. A refreshed page that drops a
previously recorded match stops before overwriting that page.

Matches retain both start and completion dates. Only completed matches through
the requested cutoff enter analysis. Stable team IDs connect name changes.
No-result matches remain in the shared table, with a missing score.

## Elo specification

The implementation calls the documented `EloRating::e.single()` update function,
the same function used internally by `elo.seq()`. It keeps the original defaults:
initial rating 1,000, K = 100, normal-CDF expectation, and integer rounding after
every match. All 100 final team ratings in the recorded 2025 Test/T20I fixtures
match an independent `elo.seq()` reference exactly.

The normal-CDF probability is
`Φ((rating1 − rating2) / (200 × √2))`. This is distinct from the logistic link in
Bradley–Terry. A logistic variant should have a separate specification and
numerical checks. These parameter choices are a baseline, not a claim of optimal
predictive performance.

- Wins score 1 and losses 0. Draws and ties score 0.5 each. Matches recorded as
  tied are treated as ties even if a bowl-out or Super Over subsequently decided
  a winner.
- Abandoned, cancelled, and no-result matches cause no rating update.
- Each format has a separate pool. Results are ordered by completion date,
  start date, and match ID. The latter is a reproducible convention for same-day
  finishes, not a claim about their exact finish times.
- Month-end ratings use the last completed update on or before each calendar
  month end. There is no interpolation, future-day averaging, or inclusion of
  teams before their first rated match.
- Current rankings include teams with a rated match within 730 days. Inactive
  teams remain in the CSV without a current rank. Fewer than ten rated matches
  is marked provisional; equal ratings share a competition rank. These are our
  display rules, not ICC eligibility rules.

The update ledger records both teams' ratings before and after every rated match.
Charts show month-end histories of the current top ten active teams, without
smoothing. Histories are reconstructed from the saved source version; they are
not evidence of what could have been published using records available at the time.
Git history preserves successive actual publications.

Elo currently has no venue or margin adjustment, inactivity decay, or standard
errors. Glickman uncertainty and Stan posterior summaries belong to their own
models and should not be presented as interchangeable Elo standard errors.
Before comparing methods, use common chronological forecast cutoffs and document
each model's treatment of draws, ties, inactivity, and missing covariates.

## Publication and checks

The workflow is scheduled for Monday at 06:17 UTC, with monthly snapshots included
in each update. Scheduling starts when the workflow is on the default branch.
It runs `make check`, updates all stages, and commits the data/output directories
only after successful validation. GitHub schedules can be delayed or disabled;
the publication manifest records the actual cutoff.

Every stage writes to a temporary directory before replacing its previous output.
Input/output hashes prevent mixing different data, model, or figure versions.
A failed stage leaves the previous publication intact. If interrupted between
directory renames, recover `<output>.previous` before rerunning; the pipeline
refuses to discard that recovery directory automatically.

```sh
make check
make ci-docker
```

`make check` runs `lintr` and `testthat` against the maintained pipeline, including
real source fixtures, the legacy BOM and outcome regressions, exact numerical
reference checks, absence of future-data effects, and publication failure cases.
`make ci-docker` uses the standard `rocker/r-ver:4.6.0` image and requires a running
Docker daemon. The GitHub checks run the same R tests on Ubuntu.

## Research and historical outputs

Obsolete rating CSVs are available in Git history. The original inputs, figures, `scripts/elo.R`, `scripts/glickman.R`, and
`scripts/stan/` remain for provenance and continued model development. They are
outside the scheduled pipeline and its dependency environment. Their historical
outputs are not current validated ratings; see the [audit](docs/correctness.md)
for known defects and unresolved reproduction differences. The shared legacy
importer in `scripts/prepare_matches.R` fixes BOM handling and outcome coding.

Authors: Gaurav Sood and Derek Willis.
