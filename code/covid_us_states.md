``` r
library(tidyverse)
library(MASS, exclude = "select")
library(here)
library(zoo)
library(vars)
library(tseries)
```

The data is from the New York Times Covid 19 dataset [available on
Github](https://github.com/nytimes/covid-19-data).

``` r
df <- read.csv(here("data/us-states.csv"))
head(df) 
```

    ##         date      state fips cases deaths
    ## 1 2020-01-21 Washington   53     1      0
    ## 2 2020-01-22 Washington   53     1      0
    ## 3 2020-01-23 Washington   53     1      0
    ## 4 2020-01-24   Illinois   17     1      0
    ## 5 2020-01-24 Washington   53     1      0
    ## 6 2020-01-25 California    6     1      0

For simplicity, start with a subset of northeastern states, with counts
by month starting in March 2020. We will look at Covid cases instead of
deaths.

``` r
# make table of states/regions
states <- data.frame(state.name, state.region)
northeast_states <- states %>%
  filter(state.region == "Northeast") %>%
           pull(state.name)
northeast_states
```

    ## [1] "Connecticut"   "Maine"         "Massachusetts" "New Hampshire"
    ## [5] "New Jersey"    "New York"      "Pennsylvania"  "Rhode Island" 
    ## [9] "Vermont"

``` r
# make northeast df
northeast_df <- df %>%
  # select northeast states only
  filter(state %in% northeast_states) %>%
  mutate(date_ym = as.yearmon(substr(date,1, 7), 
                              format = "%Y-%m")) %>%
  select(state, cases, date_ym) %>%
  group_by(date_ym, state) %>%
  # calculate cases by month
  summarize(cases = sum(cases), .groups = "drop") %>%
  pivot_wider(names_from = state, values_from = cases) %>%
  # massachusets is only state to record in feb 2020, 
  # so start in March 2020
  drop_na()
```

Next we convert to a time series object.

``` r
# make each individual state a time series
ts_list <- lapply(X = setNames(northeast_states, northeast_states), FUN =  function(x) {
  state_ts <- ts(northeast_df[[x]],
        start = c(2020, 3), 
        end = c(2022, 4), 
        frequency = 12)
  state_ts
})

# merge time series together into one time series object/matrix
var_data <- do.call(ts.union, ts_list)
```

Next we calculate a correlation between states. I get errors later about
singularity and positive definite. So, I calculate the correlation
matrix and take the determinant, and find that it is treated as zero by
machine precision.

``` r
cor_matrix <- round(cor(var_data), 3)
cor_matrix
```

    ##               Connecticut Maine Massachusetts New Hampshire New Jersey New York
    ## Connecticut         1.000 0.972         0.997         0.984      0.999    0.995
    ## Maine               0.972 1.000         0.984         0.996      0.977    0.985
    ## Massachusetts       0.997 0.984         1.000         0.994      0.998    0.999
    ## New Hampshire       0.984 0.996         0.994         1.000      0.988    0.994
    ## New Jersey          0.999 0.977         0.998         0.988      1.000    0.998
    ## New York            0.995 0.985         0.999         0.994      0.998    1.000
    ## Pennsylvania        0.997 0.981         0.997         0.990      0.997    0.996
    ## Rhode Island        0.998 0.980         0.999         0.991      0.998    0.997
    ## Vermont             0.943 0.988         0.965         0.986      0.952    0.968
    ##               Pennsylvania Rhode Island Vermont
    ## Connecticut          0.997        0.998   0.943
    ## Maine                0.981        0.980   0.988
    ## Massachusetts        0.997        0.999   0.965
    ## New Hampshire        0.990        0.991   0.986
    ## New Jersey           0.997        0.998   0.952
    ## New York             0.996        0.997   0.968
    ## Pennsylvania         1.000        0.996   0.954
    ## Rhode Island         0.996        1.000   0.960
    ## Vermont              0.954        0.960   1.000

``` r
# I think this is the problem -> 0 with machine precision
print(paste("The determinant is:", det(cor_matrix)))
```

    ## [1] "The determinant is: 1.14427799999977e-21"

I use `VARselect` to see the information criteria for different lags.
All agree that the lag should be 2.

``` r
VARselect(var_data)
```

    ## $selection
    ## AIC(n)  HQ(n)  SC(n) FPE(n) 
    ##      2      2      2      2 
    ## 
    ## $criteria
    ##                   1    2    3    4    5    6    7    8    9   10
    ## AIC(n) 1.123237e+02 -Inf -Inf -Inf -Inf -Inf -Inf -Inf -Inf -Inf
    ## HQ(n)  1.125463e+02 -Inf -Inf -Inf -Inf -Inf -Inf -Inf -Inf -Inf
    ## SC(n)  1.166695e+02 -Inf -Inf -Inf -Inf -Inf -Inf -Inf -Inf -Inf
    ## FPE(n) 4.237992e+49    0    0    0    0    0    0    0    0    0

VAR does the estimation of VAR with OLS.

``` r
var_est <- VAR(y = var_data, p = 2)

# coefficients for each equation
coefficients <- coef(var_est) %>%
  lapply(as.data.frame) 

# Connecticut example
summary(var_est$varresult$Connecticut)
```

    ## 
    ## Call:
    ## lm(formula = y ~ -1 + ., data = datamat)
    ## 
    ## Residuals:
    ##         1         2         3         4         5         6         7         8 
    ##  108171.3  -28467.4 -601083.1  222907.8  214054.9 -178097.5  370249.5  -52997.5 
    ##         9        10        11        12        13        14        15        16 
    ##    4231.5  -80468.5   28048.4  -33810.1    8099.2  -91992.1 -296267.8  220559.2 
    ##        17        18        19        20        21        22        23        24 
    ##  -28306.2  428988.6    -799.6 -414066.1  238905.2  -66067.5   55272.6  -27064.8 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)  
    ## Connecticut.l1   -3.203e+00  1.036e+01  -0.309   0.7697  
    ## Maine.l1          3.093e+00  5.012e+00   0.617   0.5642  
    ## Massachusetts.l1 -2.920e+00  2.762e+00  -1.057   0.3389  
    ## New.Hampshire.l1  1.855e+01  1.850e+01   1.003   0.3620  
    ## New.Jersey.l1     4.197e-01  1.336e+00   0.314   0.7660  
    ## New.York.l1      -3.585e-01  7.900e-01  -0.454   0.6690  
    ## Pennsylvania.l1  -1.341e+00  1.300e+00  -1.031   0.3497  
    ## Rhode.Island.l1   2.025e+01  2.075e+01   0.976   0.3740  
    ## Vermont.l1       -1.311e+00  2.004e+01  -0.065   0.9504  
    ## Connecticut.l2    2.120e+00  6.125e+00   0.346   0.7434  
    ## Maine.l2          7.272e-01  8.345e+00   0.087   0.9339  
    ## Massachusetts.l2 -1.833e+00  3.433e+00  -0.534   0.6162  
    ## New.Hampshire.l2  2.956e+00  1.273e+01   0.232   0.8256  
    ## New.Jersey.l2    -1.816e-01  1.711e+00  -0.106   0.9196  
    ## New.York.l2       7.713e-01  4.578e-01   1.685   0.1528  
    ## Pennsylvania.l2   4.689e-01  1.181e+00   0.397   0.7077  
    ## Rhode.Island.l2  -4.649e+00  1.032e+01  -0.451   0.6711  
    ## Vermont.l2       -3.392e+01  2.307e+01  -1.470   0.2014  
    ## const             4.421e+06  1.642e+06   2.693   0.0431 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 493100 on 5 degrees of freedom
    ## Multiple R-squared:  0.9988, Adjusted R-squared:  0.9943 
    ## F-statistic: 225.7 on 18 and 5 DF,  p-value: 4.669e-06

I should be able to run the following to get a summary for the entire
VAR model, but in this dataset, I get an error that says the system in
singular:

*Error in solve.default(Sigma) : system is computationally singular:
reciprocal condition number = 1.44378e-19*

It should give output like this:

*VAR Estimation Results:*

*=========================*

*Endogenous variables: real_gdp_growth, psei, bsp_rrp, unem*

*Deterministic variables: const*

*Sample size: 78*

*Log Likelihood: -876.81*

*Roots of the characteristic polynomial:*

*0.9776 0.8784 0.56 0.501 0.501 0.1641 0.1525 0.1525*

*Call:*

*VAR(y = v1, p = 2, type = “const”, exogen = NULL)*

``` r
#can't run this due to singuilarity
summary(var_est)
```

Next we run Phillips-Perron Unit Root Test, which tests the stationarity
assumption. The results of the test suggest that the data is
non-stationary, and there is a trend (this makes sense because of the
upward covid cases trend).

``` r
pp_test <- lapply(ts_list, pp.test)

lapply(pp_test, "[[", "p.value") %>%
  as.data.frame() %>%
  pivot_longer(everything(), names_to = c("state")) %>%
  mutate(value = round(value, 3))
```

    ## # A tibble: 9 × 2
    ##   state         value
    ##   <chr>         <dbl>
    ## 1 Connecticut   0.436
    ## 2 Maine         0.9  
    ## 3 Massachusetts 0.575
    ## 4 New.Hampshire 0.769
    ## 5 New.Jersey    0.451
    ## 6 New.York      0.57 
    ## 7 Pennsylvania  0.53 
    ## 8 Rhode.Island  0.533
    ## 9 Vermont       0.926

The stability function checks for structural breaks. Structural breaks
may impact the estimation. The line in the middle of the plot should not
go outside of the red bounds.

``` r
stability <- stability(var_est, type = "OLS-CUSUM")

# connecticut for example
stability$stability$Connecticut
```

    ## 
    ## Empirical Fluctuation Process: OLS-based CUSUM test 
    ## 
    ## Call: efp(formula = formula, data = data, type = type, h = h, dynamic = dynamic, 
    ##     rescale = rescale)

``` r
plot(stability$stability$Connecticut)
```

![](covid_us_states_files/figure-markdown_github/unnamed-chunk-10-1.png)

There are additional functions we cannot run due to singularity.

`serial.test()` computes the multivariate Portmanteau- and
Breusch-Godfrey test for serially correlated errors. This checks the
assumption that the residuals should be non-autocorrelated.

`arch.test()` this computes the ARCH(autoregressive conditionally
heteroscedastic)-LM test, which analyzes volatility variance.

`causality()` computes Granger- and Instantaneous causality. Granger
causality tests if one time series is useful for forecasting another.

``` r
serial.test(var_est, type = "PT.asymptotic")

arch.test(var_est, 
          lags.multi = 15, 
          multivariate.only = TRUE)

causality(var_est, cause = "Connecticut")
```

There are additional functions we cannot run due to not positive
definite. This is the error:

*the leading minor of order 6 is not positive definite for normality
test*

`normality.test()` checks for normality of the distribution of the
residuals.

`irf()` computes the impulse response coefficients. It is not clear to
my why we would need this.

`fevd()` computes the forecast error variance decomposition. It tells
which states influence the variance the most over time.

``` r
normality.test(var_est, multivariate.only = TRUE)

irf(var_est, impulse = "Connecticut", response = "Maine", n.ahead = 20, boot = TRUE)

fevd(var_est)
```
