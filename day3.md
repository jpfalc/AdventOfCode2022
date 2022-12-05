AOC2022 Day 3
================

# Data Load and Clean

``` r
data <- read_csv('data/day3.txt',
                 col_names = c('contents'),
                 col_types = cols(contents = col_character())) %>%
  mutate(rucksack = 1:n(), 
         group = ceiling(rucksack / 3),
         elf = paste0('elf', (rucksack-1) %% 3 + 1),
         comp1 = str_sub(contents, 1, str_length(contents)/2),
         comp2 = str_sub(contents, str_length(contents)/2 + 1)) %>%
  rowwise() %>%
  mutate(comp1 = pmap(list(comp1), ~strtoi(charToRaw(..1), 16L)),
         comp2 = pmap(list(comp2), ~strtoi(charToRaw(..1), 16L)))

priority_lower <- tibble(priority = 1:26) %>% mutate(ascii = strtoi(charToRaw('a'), 16L) + priority - 1)
priority_upper <- tibble(priority = 27:52) %>% mutate(ascii = strtoi(charToRaw('A'), 16L) + priority - 27)
priority <- priority_lower %>% bind_rows(priority_upper)
data
```

    ## # A tibble: 300 x 6
    ## # Rowwise: 
    ##    contents                              rucksack group elf   comp1     comp2   
    ##    <chr>                                    <int> <dbl> <chr> <list>    <list>  
    ##  1 gvNbShZZgQfWdQhdPQmggLTFLwmwjFqjVVgM         1     1 elf1  <int [18~ <int [1~
    ##  2 CsJnHllcsnnnnJrGRnRwPPLVmFLHLBjFFVHm~        2     1 elf2  <int [19~ <int [1~
    ##  3 JlnCtctJnJDcJlDCRpPrSSQWfphzWZfbZSvf~        3     1 elf3  <int [20~ <int [2~
    ##  4 WjvRSdSQjvpjWzNlnZlNZqCCMzZZ                 4     2 elf1  <int [14~ <int [1~
    ##  5 nJtJsbctPBPwLNcDZNNGLClC                     5     2 elf2  <int [12~ <int [1~
    ##  6 tsFJHBgJwgJbnvSHHWVWHhVhpQ                   6     2 elf3  <int [13~ <int [1~
    ##  7 zRzPhCCSHVZzfGHZ                             7     3 elf1  <int [8]> <int [8~
    ##  8 qBsWBpqBwBcvqqWgdfZrprdggPHHVZ               8     3 elf2  <int [15~ <int [1~
    ##  9 WWmvwvBbnWmnwvWcbmWWnqNCRSDRRSSjjSDb~        9     3 elf3  <int [23~ <int [2~
    ## 10 rQrznfHHhrHzllzlzTGcJgtJ                    10     4 elf1  <int [12~ <int [1~
    ## # ... with 290 more rows

# Part 1

``` r
data %>%
  mutate(ascii = intersect(comp1, comp2)) %>%
  inner_join(priority, by = 'ascii') %>%
  select(rucksack, priority) %>%
  adorn_totals() %>% 
  filter(rucksack == 'Total')
```

    ## # A tibble: 1 x 2
    ## # Rowwise: 
    ##   rucksack priority
    ##   <chr>       <int>
    ## 1 Total        7553

# Part 2

``` r
data %>%
  select(group, elf, contents) %>%
  mutate(x = pmap(list(contents), ~strtoi(charToRaw(..1), 16L))) %>%
  select(-contents) %>%
  pivot_wider(names_from=elf, values_from = x) %>%
  rowwise() %>%
  mutate(ascii = list(intersect(intersect(elf1, elf2), elf3))) %>%
  unnest(ascii) %>%
  inner_join(priority, by = 'ascii') %>%
  select(group, priority) %>%
  adorn_totals() %>% 
  filter(group == 'Total')
```

    ##  group priority
    ##  Total     2758
