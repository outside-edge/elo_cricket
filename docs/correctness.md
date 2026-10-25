# Correctness audit

The audit separates errors in result coding from changes in sample, chronology,
model specification, and output extraction. Previously committed ratings remain in Git history; the maintained pipeline writes separate model outputs.

## Verified legacy defects

The prepared CSV contains 62,591 unique match IDs. Its core fields agree with the
raw legacy CSV after matching by ID and normalizing missing-value representation.
The file begins with a UTF-8 byte-order mark. The legacy importer now explicitly
uses `fileEncoding = "UTF-8-BOM"`; regression tests check that `team1` exists and
has the expected length.

The original winner assignment treated every result that was not a team-1 win
as a team-2 win, except outcomes containing “drawn”. Across the men's international
subsets, this affected **415 matches**:

| Format | No result / abandoned / cancelled | Tied | Total |
| --- | ---: | ---: | ---: |
| Test | 12 | 2 | 14 |
| ODI | 273 | 39 | 312 |
| T20I | 70 | 19 | 89 |
| Total | 355 | 60 | 415 |

The corrected legacy sample has 7,735 rated matches: 2,416 Tests, 4,162 ODIs, and
1,157 T20Is. All 60 ties score 0.5. The 355 matches without a result do not update
ratings. These are executable regression checks, not documentation assertions.

Using the same legacy sample, original date ordering, and EloRating defaults,
changing only outcome handling changes final ratings materially. Examples:
Pakistan's Test rating 1,079 → 1,108; Sri Lanka's ODI rating 1,268 → 1,157;
South Africa's ODI rating 1,214 → 1,319; West Indies' T20I rating 1,043 → 1,158.
These are controlled coding comparisons, not the newly expanded pipeline's ratings.
An uncertainty calculation must use the corrected outcomes; consistency with a
known coding error is not a reason to retain it.

## Chronology and numerical reproduction

The legacy monthly extraction requests `NA.interpolate = TRUE` and a 30-day
range. This can use future matches and average dates after the stated snapshot.
A two-team example gives 1,050 by carrying forward the rating on January 15,
versus 1,021 with interpolation using a later match. The maintained pipeline
extracts from its completed-match update ledger instead.

Running the current EloRating version against the legacy extraction does not
exactly reproduce the committed latest monthly values. Maximum absolute
mismatches are 17 Test points, 25 ODI points, and 53 T20I points. The old package
version and exact generation provenance were not pinned. This discrepancy is
unresolved and is not attributed solely to the result-coding defect.

The new engine is checked against an independent `EloRating::elo.seq()` reference
for all 100 teams across the full saved 2025 Test and T20I tables (597 rated
matches). Additional tests truncate the data at historical cutoffs and confirm
that adding later matches cannot change earlier snapshots. A single-date dataset
exposes a dimension-dropping failure in EloRating 0.46.18's sequence wrapper;
the maintained pipeline uses its documented `e.single()` primitive, preserving
the package's update arithmetic without relying on that wrapper's date matrices.

The expanded results cannot be expected to equal the old published series. It
starts at the source's earliest international years, adds subsequent results,
uses completion dates, joins team aliases by ID, fixes outcomes, and changes
monthly extraction. Each of those choices is explicit in the manifest/README.

## Other model work

The historical Glickman script uses `PlayerRatings::glicko2()` but repeats the
old outcome coding. Its period-to-date join can duplicate team-date rows, and
its purported Elo comparison reads the Glicko CSV again. The Stan driver also
uses the old outcome recoding and refers to model files not present in this
repository. These scripts are exploratory, not verified publication adapters.

Their eventual adapters should consume the shared validated match identities
and outcomes, with model-specific treatment of rating periods, ties/draws,
inactivity, and covariates. Glickman and Stan outputs must have independent
specifications, checks, diagnostics, and publication directories. No claim of
numerical equivalence between these different methods is implied.

The historical all-matches request list contains 63,360 unique IDs; 810 requested
IDs are absent from the 62,591 loaded records, and 41 loaded IDs were not in that
list. That is a further reason to document coverage of the new source explicitly,
rather than assume the legacy extraction was complete.
