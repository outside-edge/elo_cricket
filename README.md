## Over Time ELO Rankings of International Cricket Teams By Format

The ICC only lists the [current ranking of cricket teams](https://www.icc-cricket.com/rankings/mens/team-rankings/odi). To address the paucity of historical ranking data, we generate ELO rankings.

### Data, Scripts, and Outputs

To build the [dataset of matches](data/), we used the [python wrapper to espncricinfo](https://github.com/outside-edge/python-espncricinfo).

* [ELO](scripts/elo.R)

**Test Ratings as of 6/18/2021**

![Test ratings as of 6/18/2021](figs/test_ratings_2021-06-18.png)

**Smoothed Test Ratings Over Time (1881--2021)**

![Test ratings over time](figs/test_ratings_1881_2021.png.png)

Download the complete monthly time series dataset [here](data/)

### Authors

Gaurav Sood and Derek Willis

