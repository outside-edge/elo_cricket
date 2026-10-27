# What should a cricket rating estimate?

The production Elo series is a reproducible baseline. Alternative models should
make different assumptions explicit and earn their place through prediction and
model checks. A smoother-looking curve alone is not evidence of a better model.

The [implemented experiment](https://github.com/outside-edge/side_order/blob/main/docs/model_experiment.md) now fits the GAM, Glicko-2,
and dynamic ordinal-probit Stan models, with shared Test-match forecasts,
simulation checks, and saved diagnostics. The conceptual distinctions below
remain relevant when extending them to lineups or regular publication.

## Three useful targets

1. **An institution's current strength:** how a national side is expected to
   perform under a specified reference schedule and playing conditions. Its
   players, selection policy, coaching, and resources can change over time.
2. **A particular XI's match performance:** a prediction conditional on selected
   players, opponent, venue, and information available at the prediction time.
   This needs lineup and player data beyond the present results table.
3. **Retrospective historical strength:** what all subsequently observed matches
   suggest about a side at an earlier date. This can revise history and should
   be published separately from the ratings actually available at that date.

Keep formats separate initially. A team's Test, ODI, and T20I selections and
skills differ. Partial pooling across formats is a later modeling decision.

## Elo already makes modeling choices

The current adapter starts teams at 1,000, uses a normal-CDF expectation, fixes
K at 100, and treats draws and ties as half-results. K determines how much a
result changes a rating. The curve's responsiveness is therefore a choice;
it is not a direct observation of underlying ability. The common logistic
Bradley–Terry likelihood is a different link from this normal-CDF baseline.

The model has no explicit lineup, home, ground, or country effects. Its rating
mixes those influences with team strength. Calendar gaps leave the point rating
unchanged, and it provides no uncertainty distribution. Smaller K values may
reduce volatility but do not supply the missing context or uncertainty.

## Model candidates

| Candidate | What it adds | Main choices to resolve |
| --- | --- | --- |
| Elo baseline | Simple sequential updates and an auditable match ledger | Select K using past-only validation; assess sensitivity to the expectation link |
| Glickman-style dynamic model | Evolving strength and uncertainty | Calendar-time evolution, inactivity, context effects, and the specific observation model |
| TrueSkill-style model in Stan | Separate latent skill, match performance noise, and outcome thresholds | Team versus player latent variables, process variance, draw mechanism, and lineup aggregation |
| Paired-comparison GAM in mgcv | Penalized team trajectories and structured context effects | Shared team basis, identifiability, penalties, and a draw-aware likelihood |

[TrueSkill](https://www.microsoft.com/en-us/research/wp-content/uploads/2006/01/TR-2006-80.pdf)
separates latent skill from noisy match performance and can aggregate players
into teams. A cricket extension could use an evolving country component plus
lineup contributions, with shrinkage to avoid assigning an entire result to
every player independently. Player identification depends on lineup variation
and the match network, not just on having an inference engine.

For a GAM, the central predictor would contain the same team-strength function
evaluated for each opponent: `strength(A, time) - strength(B, time)`. mgcv supports
[weighted sums of shared smooth evaluations](https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/linear.functional.terms.html)
and [team-specific smooths](https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/factor.smooth.html).
Those are implementation ingredients, not a complete validated cricket model.
The precise Lewis reference remains to be identified before attributing a
particular specification to it.

Match outcomes identify relative strength. Adding the same time-varying function
to every team's strength leaves paired comparisons unchanged, so the model needs
an explicit location constraint. Results alone cannot establish that all teams
became better across eras. A dynamic model should also specify how uncertainty
grows with elapsed calendar time, including long gaps without matches.

## Context needs a symmetric interpretation

For decisive outcomes, a proposed linear predictor is

```text
strength(A, time) - strength(B, time)
+ home advantage(A, venue, time) - home advantage(B, venue, time)
+ suitability(A, conditions) - suitability(B, conditions)
+ signed toss / batting-order effects
```

Swapping A and B must reverse the directional predictor. A bare ground intercept
in an arbitrarily ordered team-1 win equation would favor whichever team happens
to be listed first. Ground effects can instead modify home or batting-order
advantages, team-specific suitability, scoring conditions, or draw propensity.
Country and ground effects also need hierarchical structure or constraints:
grounds are nested geographically and some teams rarely visit particular places.

Test draws deserve an explicit outcome model. Rain, available playing time, and
conditions can produce a draw between unequally matched teams. Treating every
draw as evidence of equal ability is a substantive simplification. No-results
remain distinct from draws and ties.

## Data work before fitting

The sibling [fairly_random preparation](https://github.com/outside-edge/fairly_random/blob/master/scripts/01_prep.R)
joins grounds to country, continent, latitude, and longitude, and constructs
home status. It is a useful starting inventory. The ratings pipeline should
extend its canonical table with stable ground IDs and a documented venue-to-
country mapping, then join additional context by stable match ID.

Audit the mappings and coverage rather than copying the old name-based join.
Distinguish neutral venues, overseas home fixtures, and unknown home status.
National-side names do not always equal geographic country names; West Indies
and England require explicit treatment. Preserve missing context as missing.
Check older team-ID namespaces before joining historical covariates.

Record which information was available before the toss, after the toss, and
after play. These define different prediction tasks. Outcome-dependent fields
such as whether a rain adjustment was ultimately used cannot enter an ex-ante
forecast without modeling their availability or uncertainty.

## Filtering, smoothing, and validation

[TrueSkill Through Time](https://www.microsoft.com/en-us/research/wp-content/uploads/2008/01/NIPS2007_0931.pdf)
uses later evidence to revise earlier skill estimates. That is useful for a
retrospective history. A GAM fitted over the full time span can do the same.
For weekly ratings and forecast evaluation, fit only observations available at
the cutoff and retain those historical vintages. Later information must not
silently enter an earlier forecast.

Compare models on rolling future-match holdouts, with hyperparameters selected
inside the training period. Report predictive log loss, Brier scores, and
calibration; use the full outcome distribution when modeling draws. Check
performance by format, home/away/neutral status, match frequency, and established
versus new teams. Examine weakly connected opponent groups before interpreting
cross-group ranks. Account for series-level dependence when comparing predictive
performance, and inspect posterior predictive checks for generative models.

Publish model-specific strengths and uncertainty, plus predictions under a
common, declared reference context. Do not average incompatible rating scales
or choose a model because its trajectories are smoother. Retain Elo as a
benchmark even if a richer model becomes the main publication.

## Visualizations

The default comparison uses recent history, endpoint team names and current
ratings, and both color and line type. Full histories use one panel per team
with common axes. These views show current top-ten teams, not every historical
leader. Steps come from end-of-day match updates and carry forward to the cutoff.

For future probabilistic models, use per-team strength curves with uncertainty
bands and ordered current-strength dot-and-interval plots. A selected-team
interactive comparison could complement the static publication. Keep the
displayed uncertainty tied to a defined model quantity; Elo's current
deterministic ratings do not justify invented confidence bands.
