Data Exploration
================
Margaret Reed, Yi Chen, Caroline Maloney, Evelyn Cupil-Garcia
Nov 2, 2019

``` r
training %>%
  names()
```

    ## [1] "userID"      "inAudience"  "topic_id"    "ltiFeatures" "stiFeatures"

``` r
interest_topics %>%
  names()
```

    ## [1] "topic_id"   "topic_name"

``` r
user1 <- training %>%
  filter(userID == 1) 


user1Interest <- full_join(user1, interest_topics)
```

    ## Joining, by = "topic_id"

``` r
user1Interest$userID <- 1
user1Interest <- user1Interest %>%
  mutate_all(~replace(., is.na(.), 0))

joined <- full_join(training, interest_topics)
```

    ## Joining, by = "topic_id"

``` r
joined %>%
  group_by(topic_name) %>%
  count(inAudience) %>%
  filter(inAudience == TRUE) 
```

    ## # A tibble: 1,393 x 3
    ## # Groups:   topic_name [1,393]
    ##    topic_name                                              inAudience     n
    ##    <chr>                                                   <lgl>      <int>
    ##  1 /Arts & Entertainment                                   TRUE         923
    ##  2 /Arts & Entertainment/Celebrities & Entertainment News  TRUE        1185
    ##  3 /Arts & Entertainment/Comics & Animation                TRUE          75
    ##  4 /Arts & Entertainment/Comics & Animation/Anime & Manga  TRUE         167
    ##  5 /Arts & Entertainment/Comics & Animation/Cartoons       TRUE         183
    ##  6 /Arts & Entertainment/Comics & Animation/Comics         TRUE         279
    ##  7 /Arts & Entertainment/Entertainment Industry            TRUE         154
    ##  8 /Arts & Entertainment/Entertainment Industry/Film & TV… TRUE         148
    ##  9 /Arts & Entertainment/Entertainment Industry/Film & TV… TRUE         102
    ## 10 /Arts & Entertainment/Entertainment Industry/Film & TV… TRUE          85
    ## # … with 1,383 more rows

## Including Plots

You can also embed plots, for example:

![](exploration_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
