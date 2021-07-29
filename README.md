# Sequential movement pattern-mining (SMP) Framework

## Introduction
The SMP framework is a series of logical stepwise calculations that utilise data mining techniques to identify mathematical descriptions of patterns and
regularities in a data set (Fayyad et al., 1996). The framework makes use of spatiotemporal data, in the form of geospatial coordinates (i.e., latitude and
longitude) in combination with locomotive data (i.e., instantaneous velocity (m·s-1) and acceleration (m·s-2)). The framework attempts to identify sequential 
movement sequences using GPS-derived data from field-based team-sport athletes. The `SMP` package offers functions for the preparation and analysis of the GPS signal:

- `remove_string_elements` : Removes chosen string element or pattern from a list of file names.
- `read_gps_files` : Reads in Catapult 10Hz GPS files from a source directory and removes file type extensions to create a list of player/ position names.
- `create_movement_unit_dictionary` : Creates a standardised movement unit dictionary for use in the SMP framework.
- `compute_bearing` : Computes the bearing or heading angle between a pair of decimal degree geospatial coordinates.
- `compute_turning_angle` : Computes the turning angle of an object.
- `identify_index_ranges` : Identifies the movement unit subsequence index's by delineating the velocity vector time-series using the user selected threshold value.
- `hierarchical_cluster_reassignment` : Reprocesses hierarchical cluster results by identifying single element clusters and reassigning the single elements to the 
next most similar cluster.
- `identify_frequent_SMP` : Applies the longest common subsequence algorithm to the movement unit subsequences to identify the frequent SMP within each cluster
group.
- `SMP_framework_player_level` : Applies the SMP framework on an individual basis.
- `SMP_framework_team_level` : Applies the SMP framework to a list of Catapult 10Hz GPS dataframes.

## Required packages
The `SMP` package requires a number of supplementary packages to support the frameworks functions:

- [`ade4`](https://cran.r-project.org/web/packages/ade4/ade4.pdf)
- [`PTXQC`](https://cran.r-project.org/web/packages/PTXQC/PTXQC.pdf)
- [`stringdist`](https://cran.r-project.org/web/packages/stringdist/index.html)
- [`tidyverse`](https://rdocumentation.org/packages/tidyverse/versions/1.3.1)
- [`utils`](https://stat.ethz.ch/R-manual/R-devel/library/utils/html/00Index.html)
- [`zoo`](https://cran.r-project.org/web/packages/zoo/index.html)
