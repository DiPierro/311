replicating-311
================
Amy DiPierro
3/4/2020

``` r
# Libraries
library(dcl)
library(tidyverse)
```

    ## ── Attaching packages ───────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.2.1     ✓ purrr   0.3.3
    ## ✓ tibble  2.1.3     ✓ dplyr   0.8.4
    ## ✓ tidyr   1.0.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ──────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
library(vroom)
library(stringr)
library(sf)
```

    ## Linking to GEOS 3.7.2, GDAL 2.4.2, PROJ 5.2.0

``` r
# Parameters

file_in <- ("/Users/amydipierro/Downloads/311_Cases-4.csv")

df <-
  read_csv(file_in)
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   Opened = col_character(),
    ##   Closed = col_character(),
    ##   Updated = col_character(),
    ##   Status = col_character(),
    ##   `Status Notes` = col_character(),
    ##   `Responsible Agency` = col_character(),
    ##   Category = col_character(),
    ##   `Request Type` = col_character(),
    ##   `Request Details` = col_character(),
    ##   Address = col_character(),
    ##   Street = col_character(),
    ##   Neighborhood = col_character(),
    ##   `Police District` = col_character(),
    ##   Point = col_character(),
    ##   Source = col_character(),
    ##   `Media URL` = col_character()
    ## )

    ## See spec(...) for full column specifications.

## Prepare the data for analysis

``` r
av <- 
  df %>% 
  mutate(
    opened = mdy_hms(Opened),
    request_details = str_to_lower(`Request Details`),
    plate = str_extract(request_details, "\\d{1}[:alpha:]{3}\\d{3}"),
    status_notes = str_to_lower(`Status Notes`),
    occupied = str_detect(status_notes, "occupied")
  ) %>% 
  filter(
    Category == "Abandoned Vehicle",
    opened <= mdy("02/06/2020")
  )
```

## Total number of abandoned vehicle cases

``` r
total_av <- nrow(av)
total_av
```

    ## [1] 237902

Irena also filtered out the cases where we don’t have `Request Details`:

``` r
av %>% 
  filter(!is.na(request_details)) %>% 
  nrow()
```

    ## [1] 233077

We should decide which of these two numbers to use.

## Number of abandoned vehicle cases that record a license plate

``` r
has_plate <-
  av %>% 
  filter(!is.na(plate)) %>% 
  count()

has_plate
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1 92139

This is what Irena found.

## Number of unique license places recorded in the AV cases

``` r
unique_plates <- n_distinct(av$plate, na.rm = TRUE)

unique_plates
```

    ## [1] 58393

This is what Irena found.

## Percent of cases with a license plate

``` r
has_plate / total_av
```

    ##           n
    ## 1 0.3872981

## Number of “hot team” cases

``` r
av %>% 
  filter(
    str_detect(status_notes, "sfhot")|
    str_detect(status_notes, "hot team")|
    str_detect(status_notes, "hott team")|
    str_detect(status_notes, "persons living inside")|
    str_detect(status_notes, "sf hot")
  ) %>% 
  nrow()
```

    ## [1] 231

Vehicles transferred to hot team for which we have plates:

``` r
hot_plates <-
  av %>% 
  filter(
    str_detect(status_notes, "sfhot")|
    str_detect(status_notes, "hot team")|
    str_detect(status_notes, "hott team")|
    str_detect(status_notes, "persons living inside")|
    str_detect(status_notes, "sf hot")
  ) %>% 
  drop_na(plate) %>% 
  count(plate) %>% 
  pull(plate)
```

Number of times those vehicles were reported in the data:

``` r
av %>% 
  filter(plate %in% hot_plates) %>% 
  count(plate, sort = TRUE)
```

    ## # A tibble: 20 x 2
    ##    plate       n
    ##    <chr>   <int>
    ##  1 2axp264     9
    ##  2 3brh445     7
    ##  3 5gpe380     7
    ##  4 6tnd078     7
    ##  5 6mzn931     6
    ##  6 3vxn321     5
    ##  7 4ebl958     4
    ##  8 2sxc091     3
    ##  9 3twv967     3
    ## 10 6ksg965     3
    ## 11 2cbw689     2
    ## 12 2pcn842     2
    ## 13 3dux026     2
    ## 14 4anp608     2
    ## 15 7uav007     2
    ## 16 8cqm151     2
    ## 17 3bfr541     1
    ## 18 5hrw494     1
    ## 19 6eej516     1
    ## 20 8aaw298     1

## Number of cases in which vehicle was “occupied”

``` r
av %>% 
  count(occupied, sort = TRUE)
```

    ## # A tibble: 3 x 2
    ##   occupied      n
    ##   <lgl>     <int>
    ## 1 FALSE    237708
    ## 2 TRUE        140
    ## 3 NA           54

Occupied cars for which we have plates:

``` r
occupied_plates <-
  av %>% 
  drop_na(plate) %>% 
  filter(occupied == TRUE) %>% 
  count(plate) %>% 
  pull(plate)
```

Number of times those occupied cars appear in the data:

``` r
av %>% 
  filter(plate %in% occupied_plates) %>% 
  count(plate, sort = TRUE)
```

    ## # A tibble: 16 x 2
    ##    plate       n
    ##    <chr>   <int>
    ##  1 2axp264     9
    ##  2 6mzn931     6
    ##  3 3vxn321     5
    ##  4 4ebl958     4
    ##  5 1cku239     3
    ##  6 3twv967     3
    ##  7 4ndy812     3
    ##  8 4wcr925     3
    ##  9 6ksg965     3
    ## 10 7jur939     3
    ## 11 7tpv483     2
    ## 12 7uav007     2
    ## 13 7uvz064     2
    ## 14 5ywz838     1
    ## 15 6mzp498     1
    ## 16 8brp545     1

## Number of HSOC

``` r
av %>% 
  filter(str_detect(status_notes, "living"))
```

    ## # A tibble: 40 x 52
    ##    CaseID Opened Closed Updated Status `Status Notes` `Responsible Ag… Category
    ##     <dbl> <chr>  <chr>  <chr>   <chr>  <chr>          <chr>            <chr>   
    ##  1 4.65e6 04/08… 04/09… 04/09/… Closed Case Transfer… DPT Abandoned V… Abandon…
    ##  2 9.79e6 11/10… 11/15… 11/15/… Closed Case Transfer… DPT Abandoned V… Abandon…
    ##  3 9.62e6 10/03… 10/10… 10/10/… Closed Case Transfer… DPT Abandoned V… Abandon…
    ##  4 9.57e6 09/22… 09/28… 09/28/… Closed Case Transfer… DPT Abandoned V… Abandon…
    ##  5 9.56e6 09/18… 09/20… 09/20/… Closed Case Transfer… DPT Abandoned V… Abandon…
    ##  6 9.55e6 09/17… 09/20… 09/20/… Closed Case Transfer… DPT Abandoned V… Abandon…
    ##  7 9.55e6 09/17… 10/02… 10/02/… Closed Case Transfer… DPT Abandoned V… Abandon…
    ##  8 9.52e6 09/10… 09/19… 09/19/… Closed Case Transfer… DPT Abandoned V… Abandon…
    ##  9 9.49e6 09/03… 09/10… 09/10/… Closed Case Transfer… DPT Abandoned V… Abandon…
    ## 10 9.27e6 07/16… 08/15… 08/15/… Closed DPT Abandoned… DPT Abandoned V… Abandon…
    ## # … with 30 more rows, and 44 more variables: `Request Type` <chr>, `Request
    ## #   Details` <chr>, Address <chr>, Street <chr>, `Supervisor District` <dbl>,
    ## #   Neighborhood <chr>, `Police District` <chr>, Latitude <dbl>,
    ## #   Longitude <dbl>, Point <chr>, Source <chr>, `Media URL` <chr>, `SF Find
    ## #   Neighborhoods` <dbl>, `Current Police Districts` <dbl>, `Current Supervisor
    ## #   Districts` <dbl>, `Analysis Neighborhoods` <dbl>,
    ## #   `:@computed_region_rxqg_mtj9` <dbl>, `:@computed_region_yftq_j783` <dbl>,
    ## #   `:@computed_region_jx4q_fizf` <dbl>, `:@computed_region_bh8s_q3mv` <dbl>,
    ## #   `:@computed_region_p5aj_wyqh` <dbl>, `:@computed_region_fyvs_ahh9` <dbl>,
    ## #   `:@computed_region_f58d_8dbm` <dbl>, `:@computed_region_9dfj_4gjx` <dbl>,
    ## #   `:@computed_region_vtsz_7cme` <dbl>, `:@computed_region_n4xg_c4py` <dbl>,
    ## #   `:@computed_region_sruu_94in` <dbl>, `:@computed_region_4isq_27mq` <dbl>,
    ## #   `:@computed_region_viu7_rrfi` <dbl>, `:@computed_region_fcz8_est8` <dbl>,
    ## #   `:@computed_region_pigm_ib2e` <dbl>, `:@computed_region_9jxd_iqea` <dbl>,
    ## #   `:@computed_region_6ezc_tdp2` <dbl>, `:@computed_region_6pnf_4xz7` <dbl>,
    ## #   `:@computed_region_h4ep_8xdi` <dbl>, `:@computed_region_nqbw_i6c3` <dbl>,
    ## #   `:@computed_region_2dwj_jsy4` <dbl>, `:@computed_region_y6ts_4iup` <dbl>,
    ## #   Neighborhoods <dbl>, opened <dttm>, request_details <chr>, plate <chr>,
    ## #   status_notes <chr>, occupied <lgl>

## Repeat RVs, trailers, campers, etc.

``` r
av %>% 
  filter(
    str_detect(request_details, "rv")|
    str_detect(request_details, "trailer")|
    str_detect(request_details, "camper")|
    str_detect(request_details, "van")
  ) %>% 
  drop_na(plate) %>% 
  count(plate, sort = TRUE)
```

    ## # A tibble: 3,470 x 2
    ##    plate       n
    ##    <chr>   <int>
    ##  1 3lxy721   113
    ##  2 4ahl026   110
    ##  3 5szu022    40
    ##  4 6puj756    35
    ##  5 6dea826    33
    ##  6 1dyu930    27
    ##  7 5nyl790    26
    ##  8 6emy618    26
    ##  9 2jpu300    25
    ## 10 5flr337    23
    ## # … with 3,460 more rows

## Count by neighborhood

``` r
av %>% 
  count(Neighborhood, sort = TRUE)
```

    ## # A tibble: 118 x 2
    ##    Neighborhood            n
    ##    <chr>               <int>
    ##  1 Bernal Heights      19984
    ##  2 Outer Sunset        18506
    ##  3 Outer Richmond      10097
    ##  4 Parkside             7753
    ##  5 Mission              6786
    ##  6 Bayview              6615
    ##  7 Golden Gate Heights  5989
    ##  8 Buena Vista          5781
    ##  9 Peralta Heights      5506
    ## 10 Potrero Hill         5440
    ## # … with 108 more rows
