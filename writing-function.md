writing function
================
Huili Zheng
11/4/2021

## z score

``` r
x_vec= rnorm(25, mean = 5, sd = 4)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -0.5593127  1.3502822 -0.7720532 -0.4347886 -0.1523862 -0.5718339
    ##  [7] -0.7252347  2.5647659  1.8202417 -1.0429417  0.1560149 -0.5838205
    ## [13] -0.1402121 -0.4975567 -0.6682741 -0.1962355 -0.1149187 -0.6137456
    ## [19]  2.2845206  0.3150105  0.5779636  0.1665217 -1.2109141 -0.7179827
    ## [25] -0.2331102

``` r
z_scores = function(x){
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
}

z_scores(x = x_vec)
```

    ##  [1] -0.5593127  1.3502822 -0.7720532 -0.4347886 -0.1523862 -0.5718339
    ##  [7] -0.7252347  2.5647659  1.8202417 -1.0429417  0.1560149 -0.5838205
    ## [13] -0.1402121 -0.4975567 -0.6682741 -0.1962355 -0.1149187 -0.6137456
    ## [19]  2.2845206  0.3150105  0.5779636  0.1665217 -1.2109141 -0.7179827
    ## [25] -0.2331102

``` r
y_vec = rnorm(40, mean = 12, sd = .3)

z_scores(x = y_vec)
```

    ##  [1] -0.575661318 -1.423431208 -0.192550656  0.769136824 -0.640136107
    ##  [6]  0.823634408 -0.902297268  0.608508892  1.260041409  1.378365884
    ## [11]  1.969685362  1.086300907  0.464286987  2.122582922 -1.521297271
    ## [16] -1.418603140  0.470141074 -1.231937490  0.668284153 -1.026002869
    ## [21]  1.071508549  0.730824523 -0.135820091  1.497299714 -0.213273062
    ## [26] -0.568729683 -0.369588463 -0.218689455  0.641423826 -1.194620608
    ## [31] -0.097218367 -1.128041108  0.001398179  0.741740754 -0.665975194
    ## [36] -1.935752594  0.019240782  0.137594955 -0.380145927 -0.622228223

How great is this ?

only kinda great.

let’s try again.

``` r
z_scores = function(x){
  
  if (!is.numeric(x)){
    stop("x needs to be numeric")
  }
  
  if (length(x)<3){
    stop("x should be at least 3 numbers")
  }
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
}


z_scores(3)
```

    ## Error in z_scores(3): x should be at least 3 numbers

``` r
z_scores(c("my","name","is","hu"))
```

    ## Error in z_scores(c("my", "name", "is", "hu")): x needs to be numeric

``` r
z_scores(mtcars)
```

    ## Error in z_scores(mtcars): x needs to be numeric

``` r
z_scores(x = y_vec)
```

    ##  [1] -0.575661318 -1.423431208 -0.192550656  0.769136824 -0.640136107
    ##  [6]  0.823634408 -0.902297268  0.608508892  1.260041409  1.378365884
    ## [11]  1.969685362  1.086300907  0.464286987  2.122582922 -1.521297271
    ## [16] -1.418603140  0.470141074 -1.231937490  0.668284153 -1.026002869
    ## [21]  1.071508549  0.730824523 -0.135820091  1.497299714 -0.213273062
    ## [26] -0.568729683 -0.369588463 -0.218689455  0.641423826 -1.194620608
    ## [31] -0.097218367 -1.128041108  0.001398179  0.741740754 -0.665975194
    ## [36] -1.935752594  0.019240782  0.137594955 -0.380145927 -0.622228223

## multiple outputs

``` r
mean_and_sd = function(x){
  
  if (!is.numeric(x)){
    stop("x needs to be numeric")
  }
  
  if (length(x)<3){
    stop("x should be at least 3 numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  #create an output tibble
  output_df = 
    tibble(
      mean = mean_x,
      sd = sd_x
    )
  
  return(output_df)
}

mean_and_sd(y_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  12.0 0.258

## different sample sizes, means, sds

``` r
sim_data = 
  tibble(
    x = rnorm(30, mean = 2, sd = 3)
  )

sim_data %>%
  summarise(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.28  2.73

let’s write a function that simulates data, computes the mean and sd.

``` r
sim_mean_sd = function(n, mu, sigma){
  
  # do checks on inputs
  
sim_data = 
  tibble(
    x = rnorm(n, mean = mu, sd = sigma)
  )

sim_data %>%
  summarise(
    mean = mean(x),
    sd = sd(x)
  )
}

sim_mean_sd(30,4,3)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.95  3.12

## napoleon dynamite

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

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

okay but there are a lot of pages of reviews

write a function that gets reviews based on page url

``` r
get_page_reviews = function(page_url){
  dynamite_html = read_html(page_url)
  
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
  
  reviews = tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  ) 
  
  return(reviews)
}

url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

get_page_reviews(url)
```

    ## # A tibble: 10 × 3
    ##    title                                                 stars   text           
    ##    <chr>                                                 <chr>   <chr>          
    ##  1 I Just everyone to know this....                      5.0 ou… "\n  VOTE FOR …
    ##  2 the cobweb in his hair during the bike ramp scene lol 5.0 ou… "\n  5 stars f…
    ##  3 Best quirky movie ever                                5.0 ou… "\n  You all k…
    ##  4 Classic Film                                          5.0 ou… "\n  Had to or…
    ##  5 hehehehe                                              5.0 ou… "\n  goodjobbo…
    ##  6 Painful                                               1.0 ou… "\n  I think I…
    ##  7 GRAND                                                 5.0 ou… "\n  GRAND\n"  
    ##  8 Hello, 90s                                            5.0 ou… "\n  So nostal…
    ##  9 Cult Classic                                          5.0 ou… "\n  Watched i…
    ## 10 Format was inaccurate                                 4.0 ou… "\n  There was…

``` r
base_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

urls = str_c(base_url,1:5)

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
    ##  1 I Just everyone to know this....                      5.0 ou… "\n  VOTE FOR …
    ##  2 the cobweb in his hair during the bike ramp scene lol 5.0 ou… "\n  5 stars f…
    ##  3 Best quirky movie ever                                5.0 ou… "\n  You all k…
    ##  4 Classic Film                                          5.0 ou… "\n  Had to or…
    ##  5 hehehehe                                              5.0 ou… "\n  goodjobbo…
    ##  6 Painful                                               1.0 ou… "\n  I think I…
    ##  7 GRAND                                                 5.0 ou… "\n  GRAND\n"  
    ##  8 Hello, 90s                                            5.0 ou… "\n  So nostal…
    ##  9 Cult Classic                                          5.0 ou… "\n  Watched i…
    ## 10 Format was inaccurate                                 4.0 ou… "\n  There was…
    ## # … with 40 more rows

``` r
get_page_reviews(url)
```

    ## # A tibble: 10 × 3
    ##    title                                                 stars   text           
    ##    <chr>                                                 <chr>   <chr>          
    ##  1 I Just everyone to know this....                      5.0 ou… "\n  VOTE FOR …
    ##  2 the cobweb in his hair during the bike ramp scene lol 5.0 ou… "\n  5 stars f…
    ##  3 Best quirky movie ever                                5.0 ou… "\n  You all k…
    ##  4 Classic Film                                          5.0 ou… "\n  Had to or…
    ##  5 hehehehe                                              5.0 ou… "\n  goodjobbo…
    ##  6 Painful                                               1.0 ou… "\n  I think I…
    ##  7 GRAND                                                 5.0 ou… "\n  GRAND\n"  
    ##  8 Hello, 90s                                            5.0 ou… "\n  So nostal…
    ##  9 Cult Classic                                          5.0 ou… "\n  Watched i…
    ## 10 Format was inaccurate                                 4.0 ou… "\n  There was…
