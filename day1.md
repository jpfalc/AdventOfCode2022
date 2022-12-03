AOC2022 Day 1
================

# Data Load and Clean

``` r
data <- read_csv('data/day1.txt', 
                 col_names = c('calories'), 
                 skip_empty_rows = FALSE,
                 col_types = cols(calories = col_double())) %>%
  filter(!is.na(calories) | (is.na(calories) & is.na(lead(calories)))) %>%
  mutate(elf = cumsum(is.na(calories)) + 1) %>%
  filter(!is.na(calories)) %>%
  group_by(elf) %>% 
  summarise_all(list(sum)) %>%
  arrange(-calories)
data
```

    ## # A tibble: 256 x 2
    ##      elf calories
    ##    <dbl>    <dbl>
    ##  1   113    71506
    ##  2   192    69368
    ##  3   134    68729
    ##  4   255    68342
    ##  5   149    67818
    ##  6   183    67613
    ##  7   120    66821
    ##  8   167    66684
    ##  9   228    65762
    ## 10    38    65579
    ## # ... with 246 more rows

# Part 1

``` r
data %>% head(1)
```

    ## # A tibble: 1 x 2
    ##     elf calories
    ##   <dbl>    <dbl>
    ## 1   113    71506

# Part 2

``` r
data %>% 
  head(3) %>%
  select(calories) %>%
  summarise_all(list(sum))
```

    ## # A tibble: 1 x 1
    ##   calories
    ##      <dbl>
    ## 1   209603
