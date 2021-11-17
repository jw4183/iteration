Iteration\_listcols
================
Jiacheng Wu
11/16/2021

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.4     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)
theme_set(theme_minimal() + theme(legend.position = "bottom"))
options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)
scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

## Define function

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("x needs to be numeric")
  }
  
  if (length(x) < 3) {
    stop("x should have at least 3 numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  output_df = 
    tibble(
      mean = mean_x,
      sd = sd_x
    )
  
  return(output_df)
  
}
```

## Lists

``` r
l = 
  list(
    vec_numeric = 5:8,
    vec_logical = c(TRUE, FALSE),
    summary = summary(rnorm(1000, mean = 5, sd = 3))
  )
l[[3]]
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -6.174   2.961   4.907   4.971   6.833  14.816

``` r
l[["summary"]]
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -6.174   2.961   4.907   4.971   6.833  14.816

``` r
l$summary
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -6.174   2.961   4.907   4.971   6.833  14.816

## List of normals

``` r
list_norms = 
  list(
    a = rnorm(50, mean = 2, sd = 1),
    b = rnorm(50, mean = 5, sd = 3),
    c = rnorm(50, mean = 20, sd = 1.2),
    d = rnorm(50, mean = -12, sd = 0.5)
  )
mean_and_sd(list_norms[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.97  1.00

## for loop

Let’s use a for loop to iterate over my list of normals.

``` r
output = vector("list", length = 4)
for (i in 1:4){
  
  output[[i]] = mean_and_sd(list_norms[[i]])
  
}
```

let’s use map instead …

``` r
output = map(list_norms, mean_and_sd)
output = map(list_norms, summary)
output = map_dbl(list_norms, median)
```

## LIST COLUMNS!!!!!

``` r
listcol_df =
  tibble(
    name = c("a", "b", "c", "d"),
    norms = list_norms
  )
listcol_df %>% 
  filter(name == "a")
```

    ## # A tibble: 1 × 2
    ##   name  norms       
    ##   <chr> <named list>
    ## 1 a     <dbl [50]>

``` r
listcol_df %>% pull(name)
```

    ## [1] "a" "b" "c" "d"

``` r
listcol_df %>% pull(norms)
```

    ## $a
    ##  [1]  2.7953122  0.7502458  2.1732550  2.5805610  1.5616296  0.2067721
    ##  [7]  2.1476611  0.5524433  0.4225732 -0.1414475  2.5840060  1.5737411
    ## [13]  1.4022573  2.9182611  2.6846089  2.7291091  2.0790103  3.5882603
    ## [19]  2.9117104  0.5647352  1.9994171  3.0531064  0.9468646  3.1279631
    ## [25]  2.6134617  2.5706575  3.2864144  2.6109807  2.2467038  2.3103433
    ## [31]  0.1893910  2.9566840  3.3610120  3.0070410  1.4597269  1.7835879
    ## [37]  1.1973357 -0.4149460  2.2838929  2.9029308  2.6883675  1.7712955
    ## [43]  1.7437545  0.6080148  2.1233062  2.0314272  1.8165615  1.9069312
    ## [49]  3.2955127  1.1470842
    ## 
    ## $b
    ##  [1]  3.964957  8.071466  4.574953  6.683401  4.932737  9.492089  4.884541
    ##  [8]  2.383652  7.199855  5.816115 11.469502  6.384929  6.620704  3.516508
    ## [15]  7.050965  8.698785 10.469775  1.525450  6.773695  5.708092  4.445844
    ## [22]  3.803841  3.792249  2.006111  7.755598 -1.698062  7.287935  2.798608
    ## [29]  5.811378  2.041893  5.818751  5.161681  3.181019  3.508137  2.660530
    ## [36]  8.350009  9.615827  6.428805  4.904382  3.231652  4.587634  3.441645
    ## [43]  7.786527  1.345970  5.038959  3.565920  4.715022  1.792421  6.337620
    ## [50]  6.917926
    ## 
    ## $c
    ##  [1] 20.21180 20.47672 20.11337 19.81489 20.31743 20.55273 20.04484 18.96613
    ##  [9] 19.55315 20.14662 20.15365 17.85018 20.52732 19.01346 20.56731 18.33290
    ## [17] 17.39598 19.65636 19.03308 20.69809 21.16016 20.68428 20.69329 20.29725
    ## [25] 20.52297 19.88677 19.19253 20.03340 21.74870 19.53095 19.93391 20.20502
    ## [33] 20.61349 22.28465 19.37694 19.05635 18.85020 21.65447 20.47958 20.24897
    ## [41] 20.12980 19.24485 18.13377 19.70405 19.05052 21.53338 20.53428 19.86414
    ## [49] 19.50521 18.61904
    ## 
    ## $d
    ##  [1] -12.21282 -12.52977 -11.74949 -11.61447 -11.20022 -11.37673 -11.58973
    ##  [8] -11.77312 -12.60487 -11.81505 -12.13339 -13.04241 -11.70912 -12.27819
    ## [15] -11.77069 -12.31404 -12.52283 -11.60907 -11.71141 -12.41105 -11.86884
    ## [22] -11.95808 -12.56368 -12.40083 -11.72199 -10.73769 -11.43230 -12.29565
    ## [29] -12.37007 -11.49403 -12.36754 -12.72076 -11.53616 -11.97704 -12.19434
    ## [36] -11.40945 -12.24012 -12.53513 -11.99523 -11.37099 -12.55157 -12.08159
    ## [43] -12.43209 -11.69706 -11.37151 -12.20441 -11.84543 -11.84679 -11.83125
    ## [50] -12.66908

``` r
listcol_df %>% 
  mutate(summaries = map(norms, mean_and_sd))
```

    ## # A tibble: 4 × 3
    ##   name  norms        summaries       
    ##   <chr> <named list> <named list>    
    ## 1 a     <dbl [50]>   <tibble [1 × 2]>
    ## 2 b     <dbl [50]>   <tibble [1 × 2]>
    ## 3 c     <dbl [50]>   <tibble [1 × 2]>
    ## 4 d     <dbl [50]>   <tibble [1 × 2]>

## Nested data

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USC00519397", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>%
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USC00519397 = "Waikiki_HA",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2021-10-09 16:25:09 (7.604)

    ## file min/max dates: 1869-01-01 / 2021-10-31

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USC00519397.dly

    ## date created (size, mb): 2021-10-09 16:25:13 (1.697)

    ## file min/max dates: 1965-01-01 / 2020-02-29

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2021-10-09 16:25:15 (0.913)

    ## file min/max dates: 1999-09-01 / 2021-10-31

Nest data within location

``` r
weather_nested = 
  weather_df %>% 
  nest(data = date:tmin)
weather_nested %>% 
  unnest(data)
```

    ## # A tibble: 1,095 × 6
    ##    name           id          date        prcp  tmax  tmin
    ##    <chr>          <chr>       <date>     <dbl> <dbl> <dbl>
    ##  1 CentralPark_NY USW00094728 2017-01-01     0   8.9   4.4
    ##  2 CentralPark_NY USW00094728 2017-01-02    53   5     2.8
    ##  3 CentralPark_NY USW00094728 2017-01-03   147   6.1   3.9
    ##  4 CentralPark_NY USW00094728 2017-01-04     0  11.1   1.1
    ##  5 CentralPark_NY USW00094728 2017-01-05     0   1.1  -2.7
    ##  6 CentralPark_NY USW00094728 2017-01-06    13   0.6  -3.8
    ##  7 CentralPark_NY USW00094728 2017-01-07    81  -3.2  -6.6
    ##  8 CentralPark_NY USW00094728 2017-01-08     0  -3.8  -8.8
    ##  9 CentralPark_NY USW00094728 2017-01-09     0  -4.9  -9.9
    ## 10 CentralPark_NY USW00094728 2017-01-10     0   7.8  -6  
    ## # … with 1,085 more rows

``` r
weather_lm = function(df) {
  
  lm(tmax ~ tmin, data = df)
  
}
weather_lm(weather_nested$data[[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039

``` r
map(weather_nested$data, weather_lm)
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221

``` r
weather_nested %>% 
  mutate(lm_results = map(data, weather_lm))
```

    ## # A tibble: 3 × 4
    ##   name           id          data               lm_results
    ##   <chr>          <chr>       <list>             <list>    
    ## 1 CentralPark_NY USW00094728 <tibble [365 × 4]> <lm>      
    ## 2 Waikiki_HA     USC00519397 <tibble [365 × 4]> <lm>      
    ## 3 Waterhole_WA   USS0023B17S <tibble [365 × 4]> <lm>

## Napoleon!!!

Function to get reviews / stars

``` r
get_page_reviews = function(page_url) {
  
  page_html = read_html(page_url)
  review_titles = 
    page_html %>%
    html_elements(".a-text-bold span") %>%
    html_text()
  
  review_stars = 
    page_html %>%
    html_elements("#cm_cr-review_list .review-rating") %>%
    html_text()
  
  review_text = 
    page_html %>%
    html_elements(".review-text-content span") %>%
    html_text()
  
  reviews = 
    tibble(
      title = review_titles,
      stars = review_stars,
      text = review_text
    )
  
  return(reviews)
  
  
}
base_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="
urls = str_c(base_url, 1:5)
map(urls, get_page_reviews)
```

    ## [[1]]
    ## # A tibble: 10 × 3
    ##    title                                                 stars   text           
    ##    <chr>                                                 <chr>   <chr>          
    ##  1 Yeah., it was pretty good.                            5.0 ou… "\n  Yeah, it …
    ##  2 Love it                                               5.0 ou… "\n  Didn't li…
    ##  3 it was                                                5.0 ou… "\n  mad good …
    ##  4 Fun!                                                  4.0 ou… "\n  Fun and e…
    ##  5 Vintage                                               5.0 ou… "\n  Easy to o…
    ##  6 too many commercials                                  1.0 ou… "\n  5 minutes…
    ##  7 this film is so good!                                 5.0 ou… "\n  VOTE FOR …
    ##  8 Good movie                                            5.0 ou… "\n  Weird sto…
    ##  9 I Just everyone to know this....                      5.0 ou… "\n  VOTE FOR …
    ## 10 the cobweb in his hair during the bike ramp scene lol 5.0 ou… "\n  5 stars f…
    ## 
    ## [[2]]
    ## # A tibble: 10 × 3
    ##    title                             stars              text                    
    ##    <chr>                             <chr>              <chr>                   
    ##  1 Best quirky movie ever            5.0 out of 5 stars "\n  You all know the a…
    ##  2 Classic Film                      5.0 out of 5 stars "\n  Had to order this …
    ##  3 hehehehe                          5.0 out of 5 stars "\n  goodjobboys\n"     
    ##  4 Painful                           1.0 out of 5 stars "\n  I think I sneezed …
    ##  5 GRAND                             5.0 out of 5 stars "\n  GRAND\n"           
    ##  6 Hello, 90s                        5.0 out of 5 stars "\n  So nostalgic movie…
    ##  7 Cult Classic                      5.0 out of 5 stars "\n  Watched it with my…
    ##  8 Format was inaccurate             4.0 out of 5 stars "\n  There was an optio…
    ##  9 Good funny                        3.0 out of 5 stars "\n  Would recommend\n" 
    ## 10 Not available w/in 48 hour window 1.0 out of 5 stars "\n  I couldn't watch i…
    ## 
    ## [[3]]
    ## # A tibble: 10 × 3
    ##    title                                       stars              text          
    ##    <chr>                                       <chr>              <chr>         
    ##  1 Your mom went to college.                   5.0 out of 5 stars "\n  Classic …
    ##  2 Very funny movie                            5.0 out of 5 stars "\n  I watch …
    ##  3 Watch it twice! Trust me!                   5.0 out of 5 stars "\n  Nothing …
    ##  4 A classic                                   5.0 out of 5 stars "\n  If you d…
    ##  5 Can't say how many times I've seen          5.0 out of 5 stars "\n  Such a g…
    ##  6 I pity the fool who doesn’t own this movie. 5.0 out of 5 stars "\n  I love t…
    ##  7 I don’t know why it’s so popular!           2.0 out of 5 stars "\n  My girlf…
    ##  8 Okay                                        3.0 out of 5 stars "\n  Okay\n"  
    ##  9 A WHOLESOME comedic journey                 5.0 out of 5 stars "\n  Not a mo…
    ## 10 Hilarious                                   5.0 out of 5 stars "\n  Funny\n" 
    ## 
    ## [[4]]
    ## # A tibble: 10 × 3
    ##    title                                         stars              text        
    ##    <chr>                                         <chr>              <chr>       
    ##  1 Love it                                       5.0 out of 5 stars "\n  What o…
    ##  2 WORTH IT!                                     5.0 out of 5 stars "\n  It's t…
    ##  3 Funny movie.                                  5.0 out of 5 stars "\n  Great …
    ##  4 Best movie ever!                              5.0 out of 5 stars "\n  Got th…
    ##  5 I was stuck in the oil patch back in the day. 5.0 out of 5 stars "\n  I watc…
    ##  6 Funny Dork humor                              5.0 out of 5 stars "\n  Humor …
    ##  7 Still funny!                                  5.0 out of 5 stars "\n  Still …
    ##  8 Love it!! 💜                                  5.0 out of 5 stars "\n  Love i…
    ##  9 LOVE it                                       5.0 out of 5 stars "\n  cult c…
    ## 10 Perfect                                       5.0 out of 5 stars "\n  Exactl…
    ## 
    ## [[5]]
    ## # A tibble: 10 × 3
    ##    title                             stars              text                    
    ##    <chr>                             <chr>              <chr>                   
    ##  1 Love this movie!                  5.0 out of 5 stars "\n  Great movie and se…
    ##  2 Love it                           5.0 out of 5 stars "\n  Love this movie. H…
    ##  3 As described                      3.0 out of 5 stars "\n  Book is as describ…
    ##  4 GOSH!!!                           5.0 out of 5 stars "\n  Just watch the mov…
    ##  5 Watch it right now                5.0 out of 5 stars "\n  You need to watch …
    ##  6 At this point it’s an addiction   5.0 out of 5 stars "\n  I watch this movie…
    ##  7 💕                                5.0 out of 5 stars "\n  Hands down, one of…
    ##  8 Good dumb movie                   5.0 out of 5 stars "\n  I really wanted to…
    ##  9 funny                             5.0 out of 5 stars "\n  so funny and inven…
    ## 10 Best Movie- Try to prove me wrong 5.0 out of 5 stars "\n  Best movie ever\n"

``` r
napoleon_df = 
  tibble(
    urls = urls
  )
napoleon_df %>% 
  mutate(reviews = map(urls, get_page_reviews)) %>% 
  select(reviews) %>% 
  unnest()
```

    ## Warning: `cols` is now required when using unnest().
    ## Please use `cols = c(reviews)`

    ## # A tibble: 50 × 3
    ##    title                                                 stars   text           
    ##    <chr>                                                 <chr>   <chr>          
    ##  1 Yeah., it was pretty good.                            5.0 ou… "\n  Yeah, it …
    ##  2 Love it                                               5.0 ou… "\n  Didn't li…
    ##  3 it was                                                5.0 ou… "\n  mad good …
    ##  4 Fun!                                                  4.0 ou… "\n  Fun and e…
    ##  5 Vintage                                               5.0 ou… "\n  Easy to o…
    ##  6 too many commercials                                  1.0 ou… "\n  5 minutes…
    ##  7 this film is so good!                                 5.0 ou… "\n  VOTE FOR …
    ##  8 Good movie                                            5.0 ou… "\n  Weird sto…
    ##  9 I Just everyone to know this....                      5.0 ou… "\n  VOTE FOR …
    ## 10 the cobweb in his hair during the bike ramp scene lol 5.0 ou… "\n  5 stars f…
    ## # … with 40 more rows
