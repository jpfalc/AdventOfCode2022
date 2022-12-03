AOC2022 Day 2
================

# Data Load and Clean

``` r
choices <- c('rock', 'paper', 'scissors')

choice_map <- tibble(code = c('A', 'B', 'C', 'X', 'Y', 'Z'),
                     choice = c(choices, choices))

shape_scores <- tibble(myself = choices, shape_score =  c(1, 2, 3))

outcome_scores <- tibble(opponent = choices) %>%
  crossing(tibble(myself = choices)) %>%
  arrange(opponent, myself) %>%
  mutate(outcome_score = c(3,0,6,6,3,0,0,6,3))

data <- read_delim('data/day2.txt',
                   delim = ' ',
                   col_names = c('opponent', 'myself'),
                   col_types = cols(opponent = col_character(), myself = col_character())) %>%
  inner_join(choice_map %>% rename(opponent = code), by="opponent") %>%
  select(-opponent) %>% rename(opponent = choice) %>%
  inner_join(choice_map %>% rename(myself = code), by="myself") %>%
  select(-myself) %>% rename(myself = choice) %>%
  mutate(round = row_number())
data
```

    ## # A tibble: 2,500 x 3
    ##    opponent myself   round
    ##    <chr>    <chr>    <int>
    ##  1 rock     paper        1
    ##  2 paper    paper        2
    ##  3 paper    scissors     3
    ##  4 paper    scissors     4
    ##  5 paper    rock         5
    ##  6 paper    scissors     6
    ##  7 scissors paper        7
    ##  8 rock     scissors     8
    ##  9 scissors rock         9
    ## 10 scissors rock        10
    ## # ... with 2,490 more rows

# Part 1

``` r
data %>%
  inner_join(shape_scores, by = "myself") %>%
  inner_join(outcome_scores, by = c("opponent", "myself")) %>%
  mutate(score = shape_score + outcome_score) %>%
  select(opponent, score) %>%
  adorn_totals() %>% as_tibble() %>%
  filter(opponent == 'Total')
```

    ## # A tibble: 1 x 2
    ##   opponent score
    ##   <chr>    <dbl>
    ## 1 Total    15422

# Part 2

``` r
instructions = tibble(myself = c('rock', 'paper', 'scissors'),
                      outcome_score = c(0, 3, 6))
data %>%
  inner_join(instructions, by = "myself") %>%
  select(-myself) %>%
  inner_join(outcome_scores, by = c("opponent", "outcome_score")) %>%
  inner_join(shape_scores, by = "myself") %>%
  mutate(score = shape_score + outcome_score) %>%
  select(opponent, score) %>%
  adorn_totals() %>% as_tibble() %>%
  filter(opponent == 'Total')
```

    ## # A tibble: 1 x 2
    ##   opponent score
    ##   <chr>    <dbl>
    ## 1 Total    15442
