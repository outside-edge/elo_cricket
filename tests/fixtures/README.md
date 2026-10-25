# Recorded source fixtures

The HTML files retain the title, source description, and unedited results table
from ESPNcricinfo's annual results pages, fetched on 2026-10-25. They contain
42 Test results for 2025, 579 T20I results for 2025, and the explicit no-records
response for Tests in 1878. Non-table page navigation and advertising are omitted.

`elo_reference.csv` was calculated independently with R 4.6.0 and EloRating
0.46.18. It covers the 42 Tests and 555 T20Is with rated outcomes in these
fixtures. The inputs were sorted by completion date, start date, and match ID;
team IDs were passed as character identities, ties and draws as `draw=TRUE`, and
the package defaults were used. The R test compares every final team rating
with this reference, including the 88 T20I and 12 Test teams.
