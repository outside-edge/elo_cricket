# Current rating publication review

Agy and Claude Code independently reviewed the current-rating implementation on
2026-10-26. The review covered cutoff handling, package update semantics, current
versus retrospective estimates, uncertainty, and publication checks.
Both follow-up reviews found no remaining substantive blockers. Claude's follow-up
was a code review; local execution and artifact validation were performed separately.

Changes made in response:

- Keep current fits independent of historical forecast evaluation and label
  current GAM and Stan states separately from retrospective history.
- Preserve Elo updates if a current-model fit fails. Retain the previous model
  publication and flag a different cutoff on the website.
- Test Glicko inactivity both when nobody plays and when other teams keep playing.
  Rating deviation grows once per empty period; repeating a partial-month fit
  from scratch does not apply a result twice.
- Compare TrueSkill against direct package updates for a win, a draw, and a
  two-match sequence, including elapsed-time uncertainty and opponent swapping.
- Handle leap-day tuning dates and ground mappings supplied entirely by explicit
  jurisdiction records.
- Verify model input/code hashes at publication, require the complete set of
  ratings and figures, and check copied website downloads against the originals.
- Rebuild Elo provenance after adding the TrueSkill dependency and verify that
  its numerical CSVs remain byte-identical.

The suggestion that fit objects would be committed was checked: the existing
`out/analysis/**/fits.rds` ignore rule already excludes them. Rank distributions
are not added in this change; ranks explicitly order point estimates, and the
page distinguishes strength intervals from uncertainty about ranks.

Validation includes 205 passing local test assertions, lint, Stan simulation
recovery and generated-quantities opponent-swap checks, and the actual four-chain
current Test fit. The website build verifies nine pages and both publications.
This review concerns implementation and publication, not predictive superiority
or validation of the new methods for ODIs or T20Is.
