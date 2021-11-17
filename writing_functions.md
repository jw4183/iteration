Writing functions
================
Jiacheng Wu
11/13/2021

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

## Z scores

``` r
x_vec = rnorm(25, mean = 5, sd = 4)
(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -1.63142579  0.89851656 -0.85579410  0.53593871  1.20493815  0.52691631
    ##  [7]  0.96481167  1.22734170 -0.80882840  0.16041941 -0.70264672 -1.74472276
    ## [13]  0.27198528 -0.86998306 -0.83525912 -1.90689265 -0.01383329 -0.45456098
    ## [19] -0.23591007  1.54355561  1.48263347  0.57774173 -0.42533245  0.47483666
    ## [25]  0.61555415

``` r
z_scores = function(x) {
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}
z_scores(x = x_vec)
```

    ##  [1] -1.63142579  0.89851656 -0.85579410  0.53593871  1.20493815  0.52691631
    ##  [7]  0.96481167  1.22734170 -0.80882840  0.16041941 -0.70264672 -1.74472276
    ## [13]  0.27198528 -0.86998306 -0.83525912 -1.90689265 -0.01383329 -0.45456098
    ## [19] -0.23591007  1.54355561  1.48263347  0.57774173 -0.42533245  0.47483666
    ## [25]  0.61555415

``` r
y_vec = rnorm(40, mean = 12, sd = .3)
z_scores(x = y_vec)
```

    ##  [1] -0.68963775 -1.69318971 -2.38422017 -0.64868389  0.34813144  2.11755440
    ##  [7] -1.09520903 -0.02679555 -0.48954060  0.20539360  0.02715749 -0.41755547
    ## [13] -1.06809000  0.63837556 -0.73075700  1.44296935  1.98944823  0.73698038
    ## [19]  0.57326130  0.32427960 -1.03782430  1.28881951  1.52165159 -0.53248568
    ## [25]  0.98026088 -1.28424700  0.02136427 -0.04114489  0.11202308 -0.73819333
    ## [31]  0.70568933 -1.30299678 -0.33894257 -0.37445662 -0.51290765 -0.14138668
    ## [37]  0.06818113  0.90232724  1.45710746  0.08728884

How great is this??

Only kinda great.

Let’s try again.

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("x needs to be numeric")
  }
  
  if (length(x) < 3) {
    stop("x should have at least 3 numbers")
  }
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}
```

``` r
z_scores(3)
```

    ## Error in z_scores(3): x should have at least 3 numbers

``` r
z_scores(c("my", "name", "is", "jeff"))
```

    ## Error in z_scores(c("my", "name", "is", "jeff")): x needs to be numeric

``` r
z_scores(mtcars)
```

    ## Error in z_scores(mtcars): x needs to be numeric

``` r
z_scores(x = y_vec)
```

    ##  [1] -0.68963775 -1.69318971 -2.38422017 -0.64868389  0.34813144  2.11755440
    ##  [7] -1.09520903 -0.02679555 -0.48954060  0.20539360  0.02715749 -0.41755547
    ## [13] -1.06809000  0.63837556 -0.73075700  1.44296935  1.98944823  0.73698038
    ## [19]  0.57326130  0.32427960 -1.03782430  1.28881951  1.52165159 -0.53248568
    ## [25]  0.98026088 -1.28424700  0.02136427 -0.04114489  0.11202308 -0.73819333
    ## [31]  0.70568933 -1.30299678 -0.33894257 -0.37445662 -0.51290765 -0.14138668
    ## [37]  0.06818113  0.90232724  1.45710746  0.08728884

## Multiple outputs

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
mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.69  5.30

``` r
mean_and_sd(y_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  12.1 0.304

## Different sample sizes, means, sds

``` r
sim_data = 
  tibble(
    x = rnorm(30, mean = 2, sd = 3)
  )
sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.18  3.37

Let’s write a function that simulates data, computes the mean and sd.

``` r
sim_mean_sd = function(n, mu = 3, sigma = 4) {
  
  # do checks on inputs
  
  sim_data = 
    tibble(
      x = rnorm(n, mean = mu, sd = sigma)
    )
  
  sim_data %>% 
    summarize(
      mean = mean(x),
      sd = sd(x)
    )
  
}
sim_mean_sd(n = 30, sigma = 3, mu = 40)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  39.8  3.54

``` r
sim_mean_sd(30)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.39  4.02

## Napoleon Dynamite

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"
dynamite_html = read_html(url)
review_titles = 
  dynamite_html %>%
  html_elements(".a-text-bold span") %>%
  html_text()
review_stars = 
  dynamite_html %>%
  html_elements("#cm_cr-review_list .review-rating") %>%
  html_text()
review_text = 
  dynamite_html %>%
  html_elements(".review-text-content span") %>%
  html_text()
reviews = 
  tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  )
```

Okay but there are a lot of pages of reviews.

Write a function that gets reviews based on page url

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
bind_rows(
  get_page_reviews(urls[1]),
  get_page_reviews(urls[2]),
  get_page_reviews(urls[3]),
  get_page_reviews(urls[4]),
  get_page_reviews(urls[5]))
```

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

``` r
f = function(x) {
  z = x + y
  z
}
x = 1
y = 2
f(x = y)
```

    ## [1] 4
