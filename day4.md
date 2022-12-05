AOC2022 Day 4
================

# Data Load and Clean

``` r
data <- read_delim('data/day4.txt',
                   delim = ',',
                   col_names = c('group1', 'group2'),
                   col_types = cols(group1 = col_character(), group2 = col_character())) %>%
  mutate(pair = 1:n()) %>%
  mutate(group1 = str_split(group1, '-'),
         group2 = str_split(group2, '-')) %>%
  unnest(group1) %>% mutate(group1 = as.integer(group1)) %>% 
  unnest(group2) %>% mutate(group2 = as.integer(group2)) %>%
  group_by(pair) %>%
  summarise(min1 = min(group1), max1 = max(group1),
            min2 = min(group2), max2 = max(group2),
            .groups = 'drop')
data
```

    ## # A tibble: 1,000 x 5
    ##     pair  min1  max1  min2  max2
    ##    <int> <int> <int> <int> <int>
    ##  1     1     8    17    16    49
    ##  2     2    17    38    18    36
    ##  3     3    17    43    43    43
    ##  4     4    86    94     7    87
    ##  5     5    23    97    22    85
    ##  6     6     8    50     7    50
    ##  7     7    82    84     1    83
    ##  8     8    43    95    51    94
    ##  9     9     7    89     8    90
    ## 10    10    85    90    21    70
    ## # ... with 990 more rows

# Part 1

``` r
data %>%
  mutate(fully_contained = 0,
         fully_contained = ifelse(min1 <= min2 & max1 >= max2, 1, fully_contained),
         fully_contained = ifelse(min2 <= min1 & max2 >= max1, 1, fully_contained)) %>%
  select(pair, fully_contained) %>%
  adorn_totals() %>%
  filter(pair == 'Total')
```

    ##   pair fully_contained
    ##  Total             413

# Part 2

``` r
data %>%
  mutate(overlap = 1,
         overlap = ifelse(max1 < min2 | max2 < min1, 0, overlap)) %>%
  select(pair, overlap) %>%
  adorn_totals() %>%
  filter(pair == 'Total')
```

    ##   pair overlap
    ##  Total     806
