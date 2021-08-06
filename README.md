# Sequential movement pattern-mining (SMP) Framework

## Introduction
The SMP framework is a series of logical stepwise calculations that builds upon the work of [Sweeting et al. 2017](https://www.tandfonline.com/doi/full/10.1080/02640414.2016.1273536) and utilises data mining techniques to identify mathematical descriptions of patterns and 
regularities in a data set [(Fayyad et al., 1996)](https://ojs.aaai.org//index.php/aimagazine/article/view/1230). The framework makes use of spatiotemporal data, in the form of geospatial coordinates (i.e., latitude and 
longitude) in combination with locomotive data (i.e., instantaneous velocity (m·s-1) and acceleration (m·s-2)). The framework attempts to identify sequential 
movement sequences using GPS-derived data from field-based team-sport athletes. The `SMP` package offers functions for the 
preparation and analysis of the GPS signal:

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
- `SMP_framework_player_level` : Applies the SMP framework on an individual Catapult 10Hz GPS dataframe.
- `SMP_framework_team_level` : Applies the SMP framework to a list of Catapult 10Hz GPS dataframes.

## Installation insturctions
R enables downloading and installing packages directly from GitHub by using the function `install_github` of the `devtools` package. See the code required below:

```r
library(devtools)
install_github("Ryan-Colin-White/smp-framework-in-r")
```

## Required packages
The `SMP` package requires a number of supplementary packages to support the frameworks functions:

- [`ade4`](https://cran.r-project.org/web/packages/ade4/ade4.pdf)
- [`PTXQC`](https://cran.r-project.org/web/packages/PTXQC/PTXQC.pdf)
- [`stringdist`](https://cran.r-project.org/web/packages/stringdist/index.html)
- [`tidyverse`](https://rdocumentation.org/packages/tidyverse/versions/1.3.1)
- [`utils`](https://stat.ethz.ch/R-manual/R-devel/library/utils/html/00Index.html)
- [`zoo`](https://cran.r-project.org/web/packages/zoo/index.html)

## Example: Player level application
First, we load the `SMP` package and create our standardised movement unit dictionary for use in the analysis using the `create_movement_unit_dictionary` function. This ensures uniformity and prevents variability in results.

```r
# Load the SMP library
library(SMP)

# Create the standardised movement unit dictionary 
my_dictionary <- create_movement_unit_dictionary(default = TRUE)
```

Next, we read in the players Catapult 10Hz GPS dataset using the `read_gps_files` function. Remember to have the required dataset located in your working directory.

```r
# Read in individual dataset
gps_data <- read_gps_files(gps_file_names = list.files(pattern = "*.csv"),
                           pattern_to_remove = "")

# Select individual dataframe
my_df <- as.data.frame(gps_data[["John Hancock"]])
```

```r
'data.frame':	59900 obs. of  15 variables:
 $ Timestamp             : chr  "03/05/2019 19:47:21" "03/05/2019 19:47:21" "03/05/2019 19:47:21" "03/05/2019 19:47:21" ...
 $ Seconds               : num  0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 ...
 $ Velocity              : num  0 0 0 0 0 0 0 0 0 0 ...
 $ Acceleration          : num  0 0 0 0 0 0 0 0 0 0 ...
 $ Odometer              : num  0 0 0 0 0 0 0 0 0 0 ...
 $ Latitude              : num  53.5 53.5 53.5 53.5 53.5 ...
 $ Longitude             : num  -2.38 -2.38 -2.38 -2.38 -2.38 ...
 $ Heart.Rate            : int  0 0 0 0 0 0 0 0 0 0 ...
 $ Player.Load           : num  0 0 0 0 0 0 0 0 0 0 ...
 $ Positional.Quality....: num  79.1 79.1 79.1 79.1 79.1 79.1 79.1 79.1 79.1 79.7 ...
 $ HDOP                  : num  1.5 1.5 1.5 1.5 1.5 1.5 1.5 1.5 1.5 1.4 ...
 $ X.Sats                : int  9 9 9 9 9 9 9 9 9 10 ...
 $ z5                    : chr  "John" "John" "John" "John" ...
 $ z6                    : chr  "Hancock" "Hancock" "Hancock" "Hancock" ...
 $ Player_name           : chr  "John Hancock" "John Hancock" "John Hancock" "John Hancock" ...
 ```
 
Finally, we apply the SMP framework using the `SMP_framework_player_level` function.
 
 ```r
 smp_data <- SMP_framework_player_level(player_df = my_df,
                                        dictionary_df = my_dictionary,
                                        velocity_thresholds = c(0.00, 1.69, 3.90, 4.99),
                                        acceleration_thresholds = c(min(my_df$Acceleration), -0.20, 0.20),
                                        threshold_value = 1.20,
                                        number_of_clusters = 25,
                                        movement_unit_subsequence_duration = 50,
                                        movement_unit_subsequence_length = "long")
 ```

The `SMP_framework_player_level` function analyses the dataset and returns a list of dataframes:
- `Alpha_data` : The original dataset supplied for the analysis.
- `Movement_dictionary` : A movement unit dictionary for the supplied dataset containing summary stats.
- `Frequent_SMP` : A dataframe containing the identified frequent SMP and associated summary stats.
- `Movement_unit_subsequences` : A dataframe containing all the movement unit subsequences identified during the analysis.

## Example: Team level application
The team level application of the SMP framework uses the `SMP_framework_team_level` function which is a rapper function for applying the SMP framework to a list of Catapult 10Hz GPS datasets. First, we load the `SMP` package and read in our list of datasets. Remember to have the required datasets located in your working directory.

```r
# Load the SMP library
library(SMP)

# Read in the datasets
my_gps_data <- read_gps_files(gps_file_names = list.files(pattern = "*.csv"),
                              pattern_to_remove = "")
```

Finally, we apply the SMP framework using the `SMP_framework_team_level` function.

```r
SMP_data <- SMP_framework_team_level(my_list = gps_data_list, 
                                     velocity_thresholds = c(0.00, 1.69, 3.90, 4.99),
                                     threshold_value = 1.20, 
                                     movement_unit_subsequence_duration = 50, 
                                     movement_unit_subsequence_length = "long", 
                                     number_of_clusters = 25)
```

The `SMP_framework_team_level` function analyses the datasets and returns a list of dataframes, one for each player with the same output as the `SMP_framework_player_level` function for each.

## SMP framework contributors
- [`Ryan White`](https://twitter.com/RCWhite93)
- [`Dr Anna Palczewska`](https://www.leedsbeckett.ac.uk/staff/dr-anna-palczewska/)
- [`Dr Daniel Weaving`](https://twitter.com/DanWeaving)
- [`Neil Collins`](https://twitter.com/_NeilC_)
- [`Prof Ben Jones`](https://twitter.com/23Benjones)
