# Model review and validation

The experimental implementation was independently reviewed through agy and
Claude Code on 2026-10-25, followed by focused re-reviews of the revised code.
Both checked the statistical construction and installed package behavior.
The final agy review reported no remaining merge blockers. Claude's final
review accepted the forecasting logic and requested preserving failed model
runs; that change has a regression test that checks both retained diagnostics
and preservation of the preceding successful result.

Consequential changes from review:

* Add home-adjusted Elo/Glicko and home-conditioned outcome-frequency baselines,
  retaining unadjusted comparators. This distinguishes additional context from
  changes in the strength model.
* Calibrate on historical forecasts frozen for the same horizon as evaluation,
  rather than mixing sequential historical predictions with annual forecasts.
* Hold GAM strength at its training boundary; expose its plug-in prediction and
  constant-threshold limitations. Include a recent-frequency benchmark because
  Test draw rates have changed.
* Use centered, noncentered random-walk states, scaled draw trend, and explicit
  Stan initialization. Require successful four-chain diagnostics, simulation
  recovery, and swap checks through Stan's generated quantities.
* Audit jurisdiction aliases and dated Dhaka mappings, guard single-country
  GAM fits, and report missing-country and new-team exclusions explicitly.
* Give fractional accuracy credit for tied maximum probabilities, preserve
  failed-run evidence, and replace successful comparison/figure sets together.

Two initial concerns were checked rather than accepted mechanically. mgcv's
matrix factor-smooth construction does implement the paired difference; the
installed implementation, a recovery simulation, and swapped-team predictions
agree. The Glicko adapter follows the package's rating-period convention:
`gdevs` is calculated after inactive-period inflation and before the current
period's variance update. Predictions use the package's public `predict.rating`
method with `gamma=0`, `tng=0`, and explicit new-team defaults. Empty calendar
months are represented by incrementing the package's lag counter. A result
completed during a month becomes available at the following month's vintage.

Local checks cover input identity/uniqueness, outcome scoring, no-result and
ongoing-match exclusion, changing held-out results, frozen calibration histories,
new levels, team-swap symmetry, single-venue fits, simulated strength ordering,
and atomic failure behavior. `make check-models` additionally compiles and runs
the actual Stan model. CI runs both suites and renders the existing website.
The new CmdStanR dependencies were restored into a separate local library to
verify their lockfile download sources.

This review supports merging an experimental implementation. It does not select
a production rating model. Remaining research includes rolling origins, prior
sensitivity, debut-state modeling, richer recent venue coverage, separate
post-toss forecasts, and player lineups. The exact Lewis spline reference still
needs identification; no replication claim is made.
