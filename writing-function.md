writing function
================
Huili Zheng
11/4/2021

## z score

``` r
x_vec= rnorm(25, mean = 5, sd = 4)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.13234080 -0.09477344 -0.43895581  1.29135224 -0.21094486 -0.41465018
    ##  [7]  0.83228759 -0.47076867 -0.30261148 -0.71714318  1.23303235  0.99616273
    ## [13] -1.28479777  1.31459693 -0.60083209  0.84797560 -0.67887460 -0.93763847
    ## [19]  1.11194899 -0.44503436 -1.07158562  0.59287821 -2.58123001  1.52659653
    ## [25]  0.37066856

``` r
z_scores = function(x){
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
}

z_scores(x = x_vec)
```

    ##  [1]  0.13234080 -0.09477344 -0.43895581  1.29135224 -0.21094486 -0.41465018
    ##  [7]  0.83228759 -0.47076867 -0.30261148 -0.71714318  1.23303235  0.99616273
    ## [13] -1.28479777  1.31459693 -0.60083209  0.84797560 -0.67887460 -0.93763847
    ## [19]  1.11194899 -0.44503436 -1.07158562  0.59287821 -2.58123001  1.52659653
    ## [25]  0.37066856

``` r
y_vec = rnorm(40, mean = 12, sd = .3)

z_scores(x = y_vec)
```

    ##  [1]  0.06105031  0.13436250 -0.40034042  0.14265544  0.81346395 -0.31563978
    ##  [7] -1.33021007 -0.55918166  0.65613184 -0.75434476  0.71385244 -0.68554636
    ## [13] -0.94812551  1.46373290  0.91555232 -0.35315986  1.21020672 -0.28966403
    ## [19]  1.88105727 -0.51744341  2.09140355  1.47167359 -0.79723886  0.11895651
    ## [25] -0.91412557  0.17790459  2.07711786 -0.29672667 -0.67014570 -1.69231458
    ## [31]  1.33202519  0.28823908  0.33284023 -0.40897488 -1.08911059  0.56595638
    ## [37] -0.77632694 -1.03355935 -1.07443583 -1.54156780

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

    ##  [1]  0.06105031  0.13436250 -0.40034042  0.14265544  0.81346395 -0.31563978
    ##  [7] -1.33021007 -0.55918166  0.65613184 -0.75434476  0.71385244 -0.68554636
    ## [13] -0.94812551  1.46373290  0.91555232 -0.35315986  1.21020672 -0.28966403
    ## [19]  1.88105727 -0.51744341  2.09140355  1.47167359 -0.79723886  0.11895651
    ## [25] -0.91412557  0.17790459  2.07711786 -0.29672667 -0.67014570 -1.69231458
    ## [31]  1.33202519  0.28823908  0.33284023 -0.40897488 -1.08911059  0.56595638
    ## [37] -0.77632694 -1.03355935 -1.07443583 -1.54156780

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
    ## 1  12.0 0.236

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
    ## 1  2.02  3.42

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
    ## 1  3.46  3.34
