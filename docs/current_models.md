# Current ratings from other models

Current ratings describe national teams using completed matches available through
an explicit data cutoff. They are separate from forecast evaluation: a current fit
uses all eligible observations through that cutoff and needs no held-out matches.
Each format has its own fit. The first publication covers Tests from 2000 onward.
No-results and invitational XIs are excluded. Unknown venue countries stop the
publication rather than silently removing matches from contextual models.

Four methods are published separately:

- GAM: the existing paired team splines, with country and ground home effects.
  The final-year internal validation chooses the draw threshold; all eligible
  data then enter the fit. Current strength is the fitted boundary value and
  its approximate 90% interval, including smoothing uncertainty.
- Stan: the existing dynamic TrueSkill-style ordinal-probit model, with quarterly
  strengths and contextual home/draw effects. The current quarter's posterior
  mean and 90% interval describe latent strength. Four-chain sampling diagnostics
  must pass before publication. Empty quarters increase uncertainty.
- Glicko-2: `PlayerRatings` with calendar-month completion periods. The current
  month's results enter a partial period rebuilt from scratch at each update;
  they are never applied twice. Ratings are accompanied by rating deviations
  and approximate normal 90% intervals. Inactivity inflates rating deviation
  for elapsed empty periods, capped at 350.
- TrueSkill: the forward filtering algorithm in `TrueSkillThroughTime` 1.0.0.
  Each national side is one competitor. Match completion dates supply timestamps
  in days; initial mean is 0, initial standard deviation 6, performance standard
  deviation 1, and dynamic standard deviation 0.03 per square-root day. Draw
  probability is the fitted sample's draw/tie frequency with a Beta(1,1) adjustment.
  These are baseline settings, not tuned claims of optimal prediction. The
  backward smoothing procedure is not called. No venue effects are included.

Higher estimates mean stronger teams within a method. The methods' scales differ
and should not be averaged. Rankings use point estimates among teams with a
rated match in the preceding 730 days; fewer than ten matches is provisional.
Rankings do not measure the probability that one team is better than another.
Intervals describe model-based uncertainty about strength, not match outcomes.

All outputs record their cutoff, training IDs, coverage, model settings, package
versions, and input/code hashes. Historical curves from a full GAM or Stan fit
are retrospective. TrueSkill's draw setting uses the full fitted sample, so its
saved historical filtering curves also must not be treated as archived forecasts.
Git history preserves actual successive publications. Changing settings or
selecting methods by performance belongs in the separate evaluation pipeline.

Implementation references: [TrueSkillThroughTime manual](https://cran.r-project.org/web/packages/TrueSkillThroughTime/TrueSkillThroughTime.pdf)
and [contextual model specification](model_experiment.md).

## Run and publish

Restore the pinned R dependencies with `make sync` and install CmdStan as described
in the [experiment specification](model_experiment.md#run-and-inspect). Then:

```sh
make update
make model-data
make current-models
make visualize-models
make publish-models
make site
```

`make update` acquires and prepares the latest match data and updates Elo. The
current-model fit inherits the prepared cutoff; it does not fetch data or run a
holdout evaluation. `MODEL_ARGS='Test 1000'` specifies the format and warmup and
retained iterations per chain; four chains run. The first supported publication
is Tests. Other formats must have complete venue coverage and their own checks.

Analysis writes to `out/analysis/current/test/`, charts to
`figs/models/current/test/`, and publication to `out/release/models/test/`.
The site reads the saved publication and verifies its hashes. Failed fits preserve
the previous output and retain their diagnostic files. The weekly ratings workflow
runs these stages after the Elo update; a failed model update retains the previous model publication while Elo can still
update. The model page flags any difference between their cutoffs. Historical evaluation remains `make models`.

## Venue additions

The current Test sample adds two audited ground mappings missing from the older
`fairly_random` map: New Chandigarh to India, and Tarouba to the West Indies
cricket jurisdiction. Ground profile IDs come from the saved match-result pages.
The [Punjab Cricket Association](https://cricketpunjab.in/pdf/Terms-condition--2024444.pdf)
identifies the New Chandigarh venue; [Cricket West Indies](https://www.windiescricket.com/news/cpl-venue-preview-brian-lara-cricket-academy/)
locates the Brian Lara venue in Tarouba, Trinidad. Sources and validity periods
are recorded in `data/raw/context/jurisdiction_periods.csv`.
