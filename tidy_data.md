tidy_data
================
Hanrui Li
2024-09-24

``` r
library(tidyverse)
library(readxl)
library(haven)
```

# `pivot_longer`

``` r
pulse_df = 
  read_sas("data/public_pulse_data.sas7bdat") |>
  janitor::clean_names()

pulse_tidy_df = 
  pulse_df |>
  pivot_longer(
    cols = bdi_score_bl:bdi_score_12m,
    names_to = "visit",
    values_to = "bdi_score",
    names_prefix = "bdi_score_"
  ) |>
  mutate(
    visit = replace(visit, visit == "bl", "00m")
  ) |>
  relocate(id, visit)

pulse_tidy_df
```

    ## # A tibble: 4,348 × 5
    ##       id visit   age sex   bdi_score
    ##    <dbl> <chr> <dbl> <chr>     <dbl>
    ##  1 10003 00m    48.0 male          7
    ##  2 10003 01m    48.0 male          1
    ##  3 10003 06m    48.0 male          2
    ##  4 10003 12m    48.0 male          0
    ##  5 10015 00m    72.5 male          6
    ##  6 10015 01m    72.5 male         NA
    ##  7 10015 06m    72.5 male         NA
    ##  8 10015 12m    72.5 male         NA
    ##  9 10022 00m    58.5 male         14
    ## 10 10022 01m    58.5 male          3
    ## # ℹ 4,338 more rows

## Example:

``` r
litters_df = 
  read_csv("data/FAS_litters.csv", na = c("NA", "", ".")) |>
  janitor::clean_names()
```

    ## Rows: 49 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): Group, Litter Number
    ## dbl (6): GD0 weight, GD18 weight, GD of Birth, Pups born alive, Pups dead @ ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
litters_tidy_df = 
  litters_df |>
  pivot_longer(
    cols = gd0_weight:gd18_weight,
    names_to = "gd_time",
    values_to = "weight"
  ) |>
  mutate(
    gd_time = case_match(
      gd_time,
      "gd0_weight" ~ 0,
      "gd18_weight" ~ 18
    ))

litters_tidy_df
```

    ## # A tibble: 98 × 8
    ##    group litter_number gd_of_birth pups_born_alive pups_dead_birth pups_survive
    ##    <chr> <chr>               <dbl>           <dbl>           <dbl>        <dbl>
    ##  1 Con7  #85                    20               3               4            3
    ##  2 Con7  #85                    20               3               4            3
    ##  3 Con7  #1/2/95/2              19               8               0            7
    ##  4 Con7  #1/2/95/2              19               8               0            7
    ##  5 Con7  #5/5/3/83/3-3          19               6               0            5
    ##  6 Con7  #5/5/3/83/3-3          19               6               0            5
    ##  7 Con7  #5/4/2/95/2            19               5               1            4
    ##  8 Con7  #5/4/2/95/2            19               5               1            4
    ##  9 Con7  #4/2/95/3-3            20               6               0            6
    ## 10 Con7  #4/2/95/3-3            20               6               0            6
    ## # ℹ 88 more rows
    ## # ℹ 2 more variables: gd_time <dbl>, weight <dbl>

# `pivot_wider`

Make up an analysis result table.

``` r
analysis_df = 
  tibble(
    group = c("treatment", "treatment", "control", "control"),
    time = c("pre", "post", "pre", "post"),
    mean = c(4, 10, 4.2, 5)
  )
```

`pivot_wider` for human readability.

``` r
analysis_df |>
  pivot_wider(
    names_from = time,
    values_from = mean
  ) |>
  knitr::kable() # For reading
```

| group     | pre | post |
|:----------|----:|-----:|
| treatment | 4.0 |   10 |
| control   | 4.2 |    5 |

# Bind tables

``` r
fellowship_ring = 
  read_excel("data/LotR_Words.xlsx", range = "B3:D6") |>
  mutate(movie = "fellowship_ring")

two_towers = 
  read_excel("data/LotR_Words.xlsx", range = "F3:H6") |>
  mutate(movie = "two_towers")

return_king = 
  read_excel("data/LotR_Words.xlsx", range = "J3:L6") |>
  mutate(movie = "return_king")

lotr_df = 
  bind_rows(fellowship_ring, two_towers, return_king) |>
  janitor::clean_names() |>
  pivot_longer(
    cols = female:male,
    names_to = "sex",
    values_to = "words"
  ) |>
  relocate(movie) |>
  mutate(race = str_to_lower(race))

lotr_df
```

    ## # A tibble: 18 × 4
    ##    movie           race   sex    words
    ##    <chr>           <chr>  <chr>  <dbl>
    ##  1 fellowship_ring elf    female  1229
    ##  2 fellowship_ring elf    male     971
    ##  3 fellowship_ring hobbit female    14
    ##  4 fellowship_ring hobbit male    3644
    ##  5 fellowship_ring man    female     0
    ##  6 fellowship_ring man    male    1995
    ##  7 two_towers      elf    female   331
    ##  8 two_towers      elf    male     513
    ##  9 two_towers      hobbit female     0
    ## 10 two_towers      hobbit male    2463
    ## 11 two_towers      man    female   401
    ## 12 two_towers      man    male    3589
    ## 13 return_king     elf    female   183
    ## 14 return_king     elf    male     510
    ## 15 return_king     hobbit female     2
    ## 16 return_king     hobbit male    2673
    ## 17 return_king     man    female   268
    ## 18 return_king     man    male    2459
