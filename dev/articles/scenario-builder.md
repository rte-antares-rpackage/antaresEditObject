# Scenario Builder

``` r
 # CRAN limite CPU usage
data.table::setDTthreads(2)
library(antaresEditObject)
#> Loading required package: antaresRead
```

First let’s create a new study with some areas and clusters:

``` r
path <- tempdir()
createStudy(path = path, study_name = "my-study")
#> Warning: Parameter 'horizon' is missing or inconsistent with 'january.1st' and 'leapyear'. Assume correct year is 2018.
#> To avoid this warning message in future simulations, open the study with Antares and go to the simulation tab, put a valid year number in the cell 'horizon' and use consistent values for parameters 'Leap year' and '1st january'.

# Set number of Monte-Carlo scenarios
updateGeneralSettings(nbyears = 10)

# First area
createArea("earth")
createCluster(area = "earth", cluster_name = "america", add_prefix = FALSE)
createCluster(area = "earth", cluster_name = "africa", add_prefix = FALSE)
createCluster(area = "earth", cluster_name = "europe", add_prefix = FALSE)

# Second one
createArea("moon")
createCluster(area = "moon", cluster_name = "tranquility", add_prefix = FALSE)
createCluster(area = "moon", cluster_name = "serenety", add_prefix = FALSE)

# More areas
createArea("titan")
createArea("ceres")

# Some links
createLink("earth", "moon")
createLink("moon", "titan")
createLink("moon", "ceres")

# Check what we have created
getAreas()
#> [1] "ceres" "earth" "moon"  "titan"
readClusterDesc()
#>      area     cluster  group   co2 enabled fixed.cost     gen.ts law.forced
#>    <char>      <fctr> <char> <num>  <lgcl>      <num>     <char>     <char>
#> 1:  earth     america  Other     0    TRUE          0 Use Global    Uniform
#> 2:  earth      africa  Other     0    TRUE          0 Use Global    Uniform
#> 3:  earth      europe  Other     0    TRUE          0 Use Global    Uniform
#> 4:   moon tranquility  Other     0    TRUE          0 Use Global    Uniform
#> 5:   moon    serenety  Other     0    TRUE          0 Use Global    Uniform
#>    law.planned marginal.cost market.bid.cost min.down.time min.stable.power
#>         <char>         <num>           <num>         <num>            <num>
#> 1:     Uniform             0               0             1                0
#> 2:     Uniform             0               0             1                0
#> 3:     Uniform             0               0             1                0
#> 4:     Uniform             0               0             1                0
#> 5:     Uniform             0               0             1                0
#>    min.up.time must.run nominalcapacity spinning spread.cost startup.cost
#>          <num>   <lgcl>           <num>    <num>       <num>        <num>
#> 1:           1    FALSE               0        0           0            0
#> 2:           1    FALSE               0        0           0            0
#> 3:           1    FALSE               0        0           0            0
#> 4:           1    FALSE               0        0           0            0
#> 5:           1    FALSE               0        0           0            0
#>    unitcount volatility.forced volatility.planned
#>        <num>             <num>              <num>
#> 1:         1                 0                  0
#> 2:         1                 0                  0
#> 3:         1                 0                  0
#> 4:         1                 0                  0
#> 5:         1                 0                  0
```

We can read scenario builder data with:

``` r
readScenarioBuilder()
#> list()
```

Currently it’s empty. We need to create rules before updating data:

``` r
# All areas
scenarioBuilder(n_scenario = 3)
#>       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#> ceres "1"  "2"  "3"  "1"  "2"  "3"  "1"  "2"  "3"  "1"  
#> earth "1"  "2"  "3"  "1"  "2"  "3"  "1"  "2"  "3"  "1"  
#> moon  "1"  "2"  "3"  "1"  "2"  "3"  "1"  "2"  "3"  "1"  
#> titan "1"  "2"  "3"  "1"  "2"  "3"  "1"  "2"  "3"  "1"
scenarioBuilder(n_scenario = 5)
#>       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#> ceres "1"  "2"  "3"  "4"  "5"  "1"  "2"  "3"  "4"  "5"  
#> earth "1"  "2"  "3"  "4"  "5"  "1"  "2"  "3"  "4"  "5"  
#> moon  "1"  "2"  "3"  "4"  "5"  "1"  "2"  "3"  "4"  "5"  
#> titan "1"  "2"  "3"  "4"  "5"  "1"  "2"  "3"  "4"  "5"
# Specific area
scenarioBuilder(n_scenario = 3, areas = "earth")
#>       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#> earth "1"  "2"  "3"  "1"  "2"  "3"  "1"  "2"  "3"  "1"
# Specify an area for which to use random values
scenarioBuilder(n_scenario = 3, areas_rand = "earth")
#>       [,1]   [,2]   [,3]   [,4]   [,5]   [,6]   [,7]   [,8]   [,9]   [,10] 
#> ceres "1"    "2"    "3"    "1"    "2"    "3"    "1"    "2"    "3"    "1"   
#> earth "rand" "rand" "rand" "rand" "rand" "rand" "rand" "rand" "rand" "rand"
#> moon  "1"    "2"    "3"    "1"    "2"    "3"    "1"    "2"    "3"    "1"   
#> titan "1"    "2"    "3"    "1"    "2"    "3"    "1"    "2"    "3"    "1"
```

Now we can update the scenario builder data:

``` r
my_scenario <- scenarioBuilder(n_scenario = 3)

# for load serie
updateScenarioBuilder(ldata = my_scenario, series = "load")

# equivalent as
updateScenarioBuilder(ldata = list(l = my_scenario))
```

Here we update data for serie `load` only. To update several series at
once you can do:

- with same scenario data:

``` r
my_scenario <- scenarioBuilder(n_scenario = 3)

updateScenarioBuilder(
  ldata = my_scenario, 
  series = c("load", "hydro", "solar")
)
```

- with differents scenario:

``` r
load_scenario <- scenarioBuilder(n_scenario = 3)
hydro_scenario <- scenarioBuilder(n_scenario = 4)
solar_scenario <- scenarioBuilder(n_scenario = 5)

updateScenarioBuilder(ldata = list(
  l = load_scenario,
  h = hydro_scenario,
  s = solar_scenario
))
```

If you read scenario builder now, wet got:

``` r
readScenarioBuilder()
#> $h
#>       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#> ceres    1    2    3    4    1    2    3    4    1     2
#> earth    1    2    3    4    1    2    3    4    1     2
#> moon     1    2    3    4    1    2    3    4    1     2
#> titan    1    2    3    4    1    2    3    4    1     2
#> 
#> $l
#>       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#> ceres    1    2    3    1    2    3    1    2    3     1
#> earth    1    2    3    1    2    3    1    2    3     1
#> moon     1    2    3    1    2    3    1    2    3     1
#> titan    1    2    3    1    2    3    1    2    3     1
```

For thermal and renewables series, default behavior is to set rules to
each clusters in the area :

``` r
my_scenario <- scenarioBuilder(n_scenario = 3)

updateScenarioBuilder(
  ldata = my_scenario, 
  series = "thermal"
)

readScenarioBuilder()$t
#>                  [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#> earth_africa        1    2    3    1    2    3    1    2    3     1
#> earth_america       1    2    3    1    2    3    1    2    3     1
#> earth_europe        1    2    3    1    2    3    1    2    3     1
#> moon_serenety       1    2    3    1    2    3    1    2    3     1
#> moon_tranquility    1    2    3    1    2    3    1    2    3     1
```

We can specify specific clusters with:

``` r
updateScenarioBuilder(
  ldata = my_scenario, 
  series = "thermal",
  clusters_areas = data.table::data.table(
    area = c("earth", "earth"),
    cluster = c("africa", "europe")
  )
)
readScenarioBuilder()$t
#>                  [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#> earth_africa        1    2    3    1    2    3    1    2    3     1
#> earth_america      NA   NA   NA   NA   NA   NA   NA   NA   NA    NA
#> earth_europe        1    2    3    1    2    3    1    2    3     1
#> moon_serenety      NA   NA   NA   NA   NA   NA   NA   NA   NA    NA
#> moon_tranquility   NA   NA   NA   NA   NA   NA   NA   NA   NA    NA
```

For NTC serie (Antares \>= 8.2.0), it writes the scenario for all links
:

``` r
updateScenarioBuilder(
  ldata = my_scenario, 
  series = "ntc"
)
readScenarioBuilder()$ntc
#>            [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#> ceres%moon    1    2    3    1    2    3    1    2    3     1
#> earth%moon    1    2    3    1    2    3    1    2    3     1
#> moon%titan    1    2    3    1    2    3    1    2    3     1
```

For writing scenario for a specific link you can do:

``` r
updateScenarioBuilder(
  ldata = my_scenario, 
  series = "ntc",
  links = "moon%ceres"
)
readScenarioBuilder()$ntc
#>            [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#> ceres%moon    1    2    3    1    2    3    1    2    3     1
```

Finally, you can remove all scenarios from a ruleset with:

``` r
clearScenarioBuilder()
```
