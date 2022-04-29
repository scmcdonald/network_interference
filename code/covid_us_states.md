``` r
library(tidyverse)
library(MASS, exclude = "select")
library(here)
library(zoo)
library(vars)
library(tseries)
library(ggplot2)
library(lubridate)
```

The data is from the
[CDC](https://data.cdc.gov/Case-Surveillance/United-States-COVID-19-Cases-and-Deaths-by-State-o/9mfq-cb36/data).

``` r
df <- read.csv(here("data/United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv")) %>%
  select(submission_date, state, new_case)
head(df) 
```

    ##   submission_date state new_case
    ## 1      12/01/2021    ND      589
    ## 2      12/10/2020    WA     3018
    ## 3      08/17/2020    MD      503
    ## 4      03/28/2022    VT      467
    ## 5      03/18/2020    ME       12
    ## 6      02/06/2020    NE        0

For simplicity, start with a subset of northeastern states, with counts
by month starting in March 2020. We will look at Covid cases instead of
deaths.

``` r
# make table of states/regions
states <- data.frame(state.name, state.region, state.abb)
northeast_states_df <- states %>%
  filter(state.region == "Northeast")
northeast_states <- northeast_states_df %>%
  select(state.name) %>%
  rename("state" = "state.name") %>%
  pull(state)

# make northeast df
northeast_df <- df %>%
  # select northeast states only
  merge(northeast_states_df, by.x = "state", by.y = "state.abb", all.x = F) %>%
  select(submission_date, new_case, state.name) %>%
  rename("state" = "state.name",  "cases" = "new_case") %>%
  mutate(date = mdy(submission_date)) %>%
  select(state, cases, date) %>%
  group_by(date, state) %>%
  # calculate cases by month
  summarize(cases = sum(cases), .groups = "drop") %>%
  pivot_wider(names_from = state, values_from = cases) 
```

Next we convert to a time series object.

``` r
# make each individual state a time series
ts_list <- lapply(X = setNames(northeast_states, northeast_states), FUN =  function(x) {
  state_ts <- ts(northeast_df[[x]], 
        frequency = 1)
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
    ## Connecticut         1.000 0.571         0.763         0.650      0.548    0.516
    ## Maine               0.571 1.000         0.702         0.637      0.696    0.737
    ## Massachusetts       0.763 0.702         1.000         0.874      0.654    0.619
    ## New Hampshire       0.650 0.637         0.874         1.000      0.424    0.436
    ## New Jersey          0.548 0.696         0.654         0.424      1.000    0.943
    ## New York            0.516 0.737         0.619         0.436      0.943    1.000
    ## Pennsylvania        0.561 0.830         0.658         0.514      0.881    0.909
    ## Rhode Island        0.563 0.827         0.675         0.494      0.905    0.904
    ## Vermont             0.330 0.580         0.674         0.730      0.481    0.503
    ##               Pennsylvania Rhode Island Vermont
    ## Connecticut          0.561        0.563   0.330
    ## Maine                0.830        0.827   0.580
    ## Massachusetts        0.658        0.675   0.674
    ## New Hampshire        0.514        0.494   0.730
    ## New Jersey           0.881        0.905   0.481
    ## New York             0.909        0.904   0.503
    ## Pennsylvania         1.000        0.933   0.496
    ## Rhode Island         0.933        1.000   0.531
    ## Vermont              0.496        0.531   1.000

``` r
# I think this is the problem -> 0 with machine precision
print(paste("The determinant is:", det(cor_matrix)))
```

    ## [1] "The determinant is: 4.8520786529286e-06"

I use `VARselect` to see the information criteria for different lags.
All agree that the lag should be 2.

``` r
VARselect(var_data)
```

    ## $selection
    ## AIC(n)  HQ(n)  SC(n) FPE(n) 
    ##     10     10      9     10 
    ## 
    ## $criteria
    ##                   1            2            3            4            5
    ## AIC(n) 1.151310e+02 1.136756e+02 1.125915e+02 1.116755e+02 1.108474e+02
    ## HQ(n)  1.153300e+02 1.140536e+02 1.131485e+02 1.124116e+02 1.117625e+02
    ## SC(n)  1.156494e+02 1.146605e+02 1.140429e+02 1.135935e+02 1.132319e+02
    ## FPE(n) 1.001803e+50 2.337432e+49 7.905931e+48 3.164546e+48 1.383180e+48
    ##                   6            7            8            9           10
    ## AIC(n) 1.097734e+02 1.088017e+02 1.078232e+02 1.071806e+02 1.067846e+02
    ## HQ(n)  1.108676e+02 1.100749e+02 1.092754e+02 1.088119e+02 1.085949e+02
    ## SC(n)  1.126245e+02 1.121192e+02 1.116072e+02 1.114313e+02 1.115018e+02
    ## FPE(n) 4.729302e+47 1.791539e+47 6.743225e+46 3.553109e+46 2.396552e+46

VAR does the estimation of VAR with OLS.

``` r
var_est <- VAR(y = var_data, p = 10)


# coefficients for each equation, CA example
summary <- summary(var_est)
summary$varresult$Pennsylvania
```

    ## 
    ## Call:
    ## lm(formula = y ~ -1 + ., data = datamat)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3627.7  -281.7   -39.5   262.0  6237.2 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## Connecticut.l1     0.296047   0.051481   5.751 1.31e-08 ***
    ## Maine.l1           1.169644   0.330838   3.535 0.000433 ***
    ## Massachusetts.l1  -0.121707   0.032949  -3.694 0.000237 ***
    ## New.Hampshire.l1   0.406527   0.132683   3.064 0.002266 ** 
    ## New.Jersey.l1      0.204669   0.047863   4.276 2.16e-05 ***
    ## New.York.l1        0.239553   0.032728   7.319 6.62e-13 ***
    ## Pennsylvania.l1    0.090579   0.039163   2.313 0.021008 *  
    ## Rhode.Island.l1    1.727276   0.323349   5.342 1.23e-07 ***
    ## Vermont.l1        -0.215786   0.199848  -1.080 0.280611    
    ## Connecticut.l2     0.133198   0.057117   2.332 0.019973 *  
    ## Maine.l2           0.154486   0.345267   0.447 0.654690    
    ## Massachusetts.l2   0.011799   0.034053   0.346 0.729086    
    ## New.Hampshire.l2   0.110153   0.133736   0.824 0.410400    
    ## New.Jersey.l2     -0.091638   0.053382  -1.717 0.086467 .  
    ## New.York.l2        0.002185   0.036618   0.060 0.952432    
    ## Pennsylvania.l2    0.069858   0.040451   1.727 0.084598 .  
    ## Rhode.Island.l2    0.009702   0.353196   0.027 0.978092    
    ## Vermont.l2        -0.933437   0.195929  -4.764 2.29e-06 ***
    ## Connecticut.l3     0.064345   0.060445   1.065 0.287451    
    ## Maine.l3          -0.706339   0.343355  -2.057 0.040027 *  
    ## Massachusetts.l3  -0.064772   0.035427  -1.828 0.067908 .  
    ## New.Hampshire.l3   0.145480   0.130646   1.114 0.265845    
    ## New.Jersey.l3      0.238565   0.054185   4.403 1.23e-05 ***
    ## New.York.l3       -0.102970   0.039412  -2.613 0.009171 ** 
    ## Pennsylvania.l3    0.114527   0.038676   2.961 0.003164 ** 
    ## Rhode.Island.l3    0.200719   0.352010   0.570 0.568713    
    ## Vermont.l3         0.362027   0.194857   1.858 0.063586 .  
    ## Connecticut.l4    -0.017403   0.051887  -0.335 0.737416    
    ## Maine.l4           0.452635   0.327082   1.384 0.166828    
    ## Massachusetts.l4  -0.083690   0.031795  -2.632 0.008664 ** 
    ## New.Hampshire.l4   0.213156   0.130727   1.631 0.103420    
    ## New.Jersey.l4     -0.147929   0.055035  -2.688 0.007355 ** 
    ## New.York.l4       -0.146846   0.041847  -3.509 0.000477 ***
    ## Pennsylvania.l4    0.083759   0.038689   2.165 0.030718 *  
    ## Rhode.Island.l4    0.015141   0.352488   0.043 0.965749    
    ## Vermont.l4         0.496694   0.209205   2.374 0.017846 *  
    ## Connecticut.l5    -0.052078   0.051506  -1.011 0.312303    
    ## Maine.l5          -0.640986   0.319863  -2.004 0.045448 *  
    ## Massachusetts.l5   0.008851   0.031679   0.279 0.780022    
    ## New.Hampshire.l5   0.007117   0.124099   0.057 0.954283    
    ## New.Jersey.l5      0.101095   0.055631   1.817 0.069592 .  
    ## New.York.l5        0.015581   0.040557   0.384 0.700952    
    ## Pennsylvania.l5   -0.057704   0.039370  -1.466 0.143167    
    ## Rhode.Island.l5   -0.078084   0.355864  -0.219 0.826384    
    ## Vermont.l5        -0.468359   0.200790  -2.333 0.019943 *  
    ## Connecticut.l6    -0.108055   0.052790  -2.047 0.041030 *  
    ## Maine.l6           0.853585   0.311827   2.737 0.006345 ** 
    ## Massachusetts.l6   0.081175   0.031855   2.548 0.011031 *  
    ## New.Hampshire.l6  -0.232474   0.124188  -1.872 0.061615 .  
    ## New.Jersey.l6     -0.013634   0.055891  -0.244 0.807351    
    ## New.York.l6        0.063321   0.040934   1.547 0.122317    
    ## Pennsylvania.l6    0.200360   0.040967   4.891 1.24e-06 ***
    ## Rhode.Island.l6    0.108264   0.360771   0.300 0.764193    
    ## Vermont.l6        -0.504012   0.196993  -2.559 0.010713 *  
    ## Connecticut.l7    -0.114339   0.055017  -2.078 0.038036 *  
    ## Maine.l7          -0.109100   0.323978  -0.337 0.736402    
    ## Massachusetts.l7  -0.089169   0.032343  -2.757 0.005980 ** 
    ## New.Hampshire.l7   0.156038   0.125907   1.239 0.215631    
    ## New.Jersey.l7     -0.173450   0.055552  -3.122 0.001866 ** 
    ## New.York.l7       -0.048192   0.043072  -1.119 0.263563    
    ## Pennsylvania.l7    0.199332   0.040989   4.863 1.42e-06 ***
    ## Rhode.Island.l7    1.651381   0.361520   4.568 5.79e-06 ***
    ## Vermont.l7         0.622181   0.193600   3.214 0.001368 ** 
    ## Connecticut.l8    -0.154895   0.061470  -2.520 0.011953 *  
    ## Maine.l8          -0.631550   0.333378  -1.894 0.058570 .  
    ## Massachusetts.l8  -0.064901   0.033080  -1.962 0.050148 .  
    ## New.Hampshire.l8  -0.221023   0.127006  -1.740 0.082239 .  
    ## New.Jersey.l8     -0.186361   0.056175  -3.318 0.000953 ***
    ## New.York.l8       -0.053101   0.041008  -1.295 0.195771    
    ## Pennsylvania.l8    0.190983   0.042889   4.453 9.80e-06 ***
    ## Rhode.Island.l8   -1.865133   0.366157  -5.094 4.48e-07 ***
    ## Vermont.l8         0.883847   0.198156   4.460 9.48e-06 ***
    ## Connecticut.l9     0.035583   0.059008   0.603 0.546685    
    ## Maine.l9           0.242284   0.342451   0.708 0.479482    
    ## Massachusetts.l9   0.024689   0.035806   0.690 0.490702    
    ## New.Hampshire.l9  -0.106212   0.131144  -0.810 0.418269    
    ## New.Jersey.l9      0.107674   0.055558   1.938 0.053004 .  
    ## New.York.l9        0.053199   0.040398   1.317 0.188293    
    ## Pennsylvania.l9   -0.064763   0.044829  -1.445 0.148982    
    ## Rhode.Island.l9   -0.899602   0.343404  -2.620 0.008986 ** 
    ## Vermont.l9        -0.087608   0.213208  -0.411 0.681263    
    ## Connecticut.l10   -0.024152   0.055289  -0.437 0.662358    
    ## Maine.l10         -0.571751   0.310104  -1.844 0.065629 .  
    ## Massachusetts.l10  0.090035   0.033954   2.652 0.008184 ** 
    ## New.Hampshire.l10 -0.244684   0.125944  -1.943 0.052426 .  
    ## New.Jersey.l10     0.043487   0.054945   0.791 0.428936    
    ## New.York.l10      -0.067744   0.039069  -1.734 0.083353 .  
    ## Pennsylvania.l10   0.104929   0.041658   2.519 0.011988 *  
    ## Rhode.Island.l10  -0.034739   0.311161  -0.112 0.911137    
    ## Vermont.l10       -0.223221   0.225144  -0.991 0.321792    
    ## const             39.473310  42.154487   0.936 0.349381    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 812.6 on 726 degrees of freedom
    ## Multiple R-squared:  0.9728, Adjusted R-squared:  0.9695 
    ## F-statistic: 288.9 on 90 and 726 DF,  p-value: < 2.2e-16

Next we run Phillips-Perron Unit Root Test, which tests the stationarity
assumption. The results of the test suggest that the data is stationary,
if we hold significance at the 0.1 level.

``` r
pp_test <- lapply(ts_list, pp.test)

lapply(pp_test, "[[", "p.value") %>%
  as.data.frame() %>%
  pivot_longer(everything(), names_to = c("state"), values_to = "p.value") %>%
  mutate(p.value = round(p.value, 3))
```

    ## # A tibble: 9 × 2
    ##   state         p.value
    ##   <chr>           <dbl>
    ## 1 Connecticut     0.01 
    ## 2 Maine           0.01 
    ## 3 Massachusetts   0.01 
    ## 4 New.Hampshire   0.01 
    ## 5 New.Jersey      0.057
    ## 6 New.York        0.01 
    ## 7 Pennsylvania    0.017
    ## 8 Rhode.Island    0.04 
    ## 9 Vermont         0.01

The stability function checks for structural breaks. Structural breaks
may impact the estimation. The line in the middle of the plot should not
go outside of the red bounds.

``` r
stability <- stability(var_est, type = "OLS-CUSUM")

# connecticut for example
stability$stability$Pennsylvania
```

    ## 
    ## Empirical Fluctuation Process: OLS-based CUSUM test 
    ## 
    ## Call: efp(formula = formula, data = data, type = type, h = h, dynamic = dynamic, 
    ##     rescale = rescale)

``` r
plot(stability$stability$Pennsylvania)
```

![](covid_us_states_files/figure-markdown_github/unnamed-chunk-9-1.png)

`serial.test()` computes the multivariate Portmanteau- and
Breusch-Godfrey test for serially correlated errors. This checks the
assumption that the residuals should be non-autocorrelated. The null
hypothesis is that there is no serial correlation (need to check this).
So we conclude, that there is serial correlation in this data.

``` r
serial.test(var_est, type = "PT.asymptotic")
```

    ## 
    ##  Portmanteau Test (asymptotic)
    ## 
    ## data:  Residuals of VAR object var_est
    ## Chi-squared = 2056.8, df = 486, p-value < 2.2e-16

`arch.test()` this computes the ARCH(autoregressive conditionally
heteroscedastic)-LM test, which analyzes volatility variance. Need to
check what the null is for this test.

``` r
arch.test(var_est, 
          multivariate.only = TRUE)
```

    ## 
    ##  ARCH (multivariate)
    ## 
    ## data:  Residuals of VAR object var_est
    ## Chi-squared = 23998, df = 10125, p-value < 2.2e-16

`causality()` computes Granger- and Instantaneous causality. Granger
causality tests if one time series is useful for forecasting another. We
would say that Pensylvaina does Granger-cause COVID cases in other
states.

``` r
causality(var_est, cause = "Pennsylvania")
```

    ## $Granger
    ## 
    ##  Granger causality H0: Pennsylvania do not Granger-cause Connecticut
    ##  Maine Massachusetts New.Hampshire New.Jersey New.York Rhode.Island
    ##  Vermont
    ## 
    ## data:  VAR object var_est
    ## F-Test = 8.7864, df1 = 80, df2 = 6534, p-value < 2.2e-16
    ## 
    ## 
    ## $Instant
    ## 
    ##  H0: No instantaneous causality between: Pennsylvania and Connecticut
    ##  Maine Massachusetts New.Hampshire New.Jersey New.York Rhode.Island
    ##  Vermont
    ## 
    ## data:  VAR object var_est
    ## Chi-squared = 81.91, df = 8, p-value = 2.021e-14

`normality.test()` checks for normality of the distribution of the
residuals. It’s not clear what the null/alternative hypothesis is. I
think it is that the null hypothesis is that the distribution is normal,
so we would conclude that the residuals are not normal.

``` r
normality.test(var_est, multivariate.only = TRUE)
```

    ## $JB
    ## 
    ##  JB-Test (multivariate)
    ## 
    ## data:  Residuals of VAR object var_est
    ## Chi-squared = 271657, df = 18, p-value < 2.2e-16
    ## 
    ## 
    ## $Skewness
    ## 
    ##  Skewness only (multivariate)
    ## 
    ## data:  Residuals of VAR object var_est
    ## Chi-squared = 2080.1, df = 9, p-value < 2.2e-16
    ## 
    ## 
    ## $Kurtosis
    ## 
    ##  Kurtosis only (multivariate)
    ## 
    ## data:  Residuals of VAR object var_est
    ## Chi-squared = 269577, df = 9, p-value < 2.2e-16

`irf()` computes the impulse response coefficients. It is not clear to
my why we would need this.

``` r
irf(var_est, impulse = "Connecticut", response = "Maine", n.ahead = 20, boot = TRUE)
```

`fevd()` computes the forecast error variance decomposition. It tells
which states influence the variance the most over time.

``` r
fevd(var_est)
```

    ## $Connecticut
    ##       Connecticut        Maine Massachusetts New.Hampshire   New.Jersey
    ##  [1,]   1.0000000 0.0000000000    0.00000000    0.00000000 0.000000e+00
    ##  [2,]   0.8326356 0.0003369803    0.04787033    0.02017401 4.675195e-06
    ##  [3,]   0.7986690 0.0005379655    0.04540249    0.01959152 1.222629e-03
    ##  [4,]   0.7489220 0.0047986557    0.07915509    0.01846743 5.694976e-03
    ##  [5,]   0.7179808 0.0074044503    0.07580451    0.01752104 1.900860e-02
    ##  [6,]   0.6701044 0.0091162511    0.07614394    0.01992043 6.856454e-02
    ##  [7,]   0.5875341 0.0250167886    0.06735484    0.02134379 1.352092e-01
    ##  [8,]   0.6453224 0.0183905034    0.05064522    0.02830319 9.833446e-02
    ##  [9,]   0.5610947 0.0161911839    0.05619365    0.03329526 9.079761e-02
    ## [10,]   0.5406264 0.0167328522    0.05622410    0.03735906 9.359043e-02
    ##         New.York Pennsylvania Rhode.Island     Vermont
    ##  [1,] 0.00000000   0.00000000   0.00000000 0.000000000
    ##  [2,] 0.02795964   0.01892838   0.05007170 0.002018724
    ##  [3,] 0.05219839   0.03312218   0.04697734 0.002278468
    ##  [4,] 0.05320642   0.03135704   0.04448642 0.013912022
    ##  [5,] 0.05583974   0.03610485   0.04837293 0.021963070
    ##  [6,] 0.05209560   0.03831314   0.04521811 0.020523537
    ##  [7,] 0.05625176   0.03972013   0.04040498 0.027164375
    ##  [8,] 0.05832024   0.04065952   0.03190772 0.028116760
    ##  [9,] 0.10531046   0.06045031   0.04045295 0.036213910
    ## [10,] 0.12178376   0.05801529   0.03907807 0.036590066
    ## 
    ## $Maine
    ##       Connecticut     Maine Massachusetts New.Hampshire  New.Jersey    New.York
    ##  [1,] 0.003946538 0.9960535   0.000000000   0.000000000 0.000000000 0.000000000
    ##  [2,] 0.011935198 0.9540652   0.001429056   0.001030286 0.004754670 0.002870939
    ##  [3,] 0.011789890 0.9358585   0.010118003   0.001671005 0.005655942 0.004410649
    ##  [4,] 0.011595984 0.9043806   0.009605622   0.004475907 0.005290187 0.004549170
    ##  [5,] 0.023641438 0.8759052   0.009083709   0.011277204 0.004981521 0.010572327
    ##  [6,] 0.031178223 0.8378091   0.008699600   0.023447366 0.023161546 0.013174378
    ##  [7,] 0.032293638 0.7546914   0.010464225   0.041836122 0.059697202 0.041027751
    ##  [8,] 0.031241292 0.7067778   0.009908034   0.044196535 0.092096317 0.052324712
    ##  [9,] 0.028789592 0.6705085   0.008642840   0.039364329 0.094925602 0.082140850
    ## [10,] 0.027765936 0.6527270   0.012857510   0.037651205 0.096845437 0.093729156
    ##       Pennsylvania Rhode.Island     Vermont
    ##  [1,]  0.000000000   0.00000000 0.000000000
    ##  [2,]  0.007608063   0.01338279 0.002923773
    ##  [3,]  0.007274707   0.02042940 0.002791914
    ##  [4,]  0.007463544   0.03575343 0.016885508
    ##  [5,]  0.008315437   0.03394228 0.022280871
    ##  [6,]  0.008276868   0.03263615 0.021616769
    ##  [7,]  0.007172564   0.02798638 0.024830725
    ##  [8,]  0.006897972   0.03601938 0.020537960
    ##  [9,]  0.011947727   0.04539211 0.018288460
    ## [10,]  0.011761357   0.04824073 0.018421650
    ## 
    ## $Massachusetts
    ##       Connecticut      Maine Massachusetts New.Hampshire New.Jersey   New.York
    ##  [1,]   0.1870990 0.04678495     0.7661160  0.0000000000 0.00000000 0.00000000
    ##  [2,]   0.1443316 0.03176998     0.6780040  0.0007219012 0.01131362 0.01768274
    ##  [3,]   0.1315707 0.02805734     0.6019259  0.0006382502 0.01050553 0.02240099
    ##  [4,]   0.1245706 0.03716606     0.5582580  0.0067879514 0.02375703 0.02384453
    ##  [5,]   0.1150478 0.03515680     0.5150841  0.0093851219 0.02962945 0.04024872
    ##  [6,]   0.1118805 0.03942626     0.4696595  0.0087047393 0.08048674 0.03691580
    ##  [7,]   0.1009831 0.06725180     0.4213260  0.0094498617 0.09831674 0.04150478
    ##  [8,]   0.1051513 0.07045746     0.3697882  0.0108779553 0.09894315 0.07632965
    ##  [9,]   0.1109386 0.06105827     0.3202599  0.0211241768 0.09964591 0.07776087
    ## [10,]   0.1035659 0.05652287     0.3398562  0.0353648785 0.09132520 0.07101578
    ##       Pennsylvania Rhode.Island    Vermont
    ##  [1,]   0.00000000   0.00000000 0.00000000
    ##  [2,]   0.04396909   0.04811307 0.02409402
    ##  [3,]   0.11712283   0.04973434 0.03804409
    ##  [4,]   0.12063626   0.05747715 0.04750244
    ##  [5,]   0.12286697   0.05670845 0.07587263
    ##  [6,]   0.12915314   0.05173030 0.07204300
    ##  [7,]   0.11652625   0.06567254 0.07896888
    ##  [8,]   0.11682493   0.05887774 0.09274961
    ##  [9,]   0.10245229   0.05222457 0.15453552
    ## [10,]   0.10332173   0.05481922 0.14420827
    ## 
    ## $New.Hampshire
    ##       Connecticut      Maine Massachusetts New.Hampshire  New.Jersey   New.York
    ##  [1,]  0.10159167 0.04504037     0.3292665     0.5241014 0.000000000 0.00000000
    ##  [2,]  0.10810655 0.03242194     0.4229602     0.3607495 0.001242303 0.01471686
    ##  [3,]  0.09891974 0.03330017     0.3848372     0.3274874 0.007466572 0.02196709
    ##  [4,]  0.09192469 0.04204737     0.3873125     0.3059208 0.020707212 0.02194390
    ##  [5,]  0.08991406 0.04097532     0.3846591     0.2974361 0.022976109 0.02295319
    ##  [6,]  0.09137854 0.04489140     0.3743454     0.2917561 0.029064644 0.02294148
    ##  [7,]  0.08430950 0.07113088     0.3447103     0.2687458 0.026953736 0.02310285
    ##  [8,]  0.08778291 0.08535887     0.2980070     0.2556632 0.023514714 0.02902302
    ##  [9,]  0.08368693 0.08128456     0.2753145     0.2346667 0.022953974 0.02602612
    ## [10,]  0.10115419 0.07310382     0.2895073     0.2139750 0.020079364 0.02368551
    ##       Pennsylvania Rhode.Island    Vermont
    ##  [1,]   0.00000000  0.000000000 0.00000000
    ##  [2,]   0.03122231  0.009656865 0.01892349
    ##  [3,]   0.08738385  0.012861868 0.02577607
    ##  [4,]   0.08634716  0.019718747 0.02407759
    ##  [5,]   0.08371635  0.019356377 0.03801341
    ##  [6,]   0.08967980  0.018937882 0.03700470
    ##  [7,]   0.10197193  0.030581703 0.04849330
    ##  [8,]   0.13515727  0.026466035 0.05902701
    ##  [9,]   0.16323451  0.023473071 0.08935964
    ## [10,]   0.15314323  0.045551532 0.07980011
    ## 
    ## $New.Jersey
    ##        Connecticut       Maine Massachusetts New.Hampshire New.Jersey
    ##  [1,] 0.0001203289 0.013586633   0.026714085    0.02867801  0.9309009
    ##  [2,] 0.0475251826 0.020285540   0.022103037    0.02489718  0.8266064
    ##  [3,] 0.0965320108 0.020720474   0.015912487    0.01684985  0.7001311
    ##  [4,] 0.1170501523 0.017055522   0.014274362    0.02011343  0.6110734
    ##  [5,] 0.1146292541 0.016994764   0.016230278    0.02217385  0.5945795
    ##  [6,] 0.0971340034 0.014442468   0.014266707    0.02499404  0.6003129
    ##  [7,] 0.0757144294 0.012789401   0.011541725    0.01931236  0.6505176
    ##  [8,] 0.0609325744 0.012345440   0.009894333    0.01555355  0.6872912
    ##  [9,] 0.0755788445 0.010409733   0.008066506    0.01335190  0.6761953
    ## [10,] 0.0913289156 0.008521428   0.009232674    0.01394546  0.6341563
    ##         New.York Pennsylvania Rhode.Island     Vermont
    ##  [1,] 0.00000000  0.000000000   0.00000000 0.000000000
    ##  [2,] 0.02218366  0.002726934   0.02640852 0.007263579
    ##  [3,] 0.11469874  0.007690827   0.01985797 0.007606541
    ##  [4,] 0.16235302  0.021325442   0.02610464 0.010649987
    ##  [5,] 0.17612156  0.023571115   0.02446639 0.011233272
    ##  [6,] 0.15564042  0.047755840   0.03097857 0.014475099
    ##  [7,] 0.13460361  0.051732122   0.02531444 0.018474315
    ##  [8,] 0.11408875  0.054016969   0.01984993 0.026027257
    ##  [9,] 0.11398861  0.052079907   0.01657980 0.033749358
    ## [10,] 0.12978860  0.053952506   0.01341068 0.045663471
    ## 
    ## $New.York
    ##        Connecticut       Maine Massachusetts New.Hampshire New.Jersey  New.York
    ##  [1,] 0.0001415754 0.009664858  0.0004898873   0.003624111 0.04962928 0.9364503
    ##  [2,] 0.0347624215 0.040281018  0.0043453363   0.008794337 0.11531483 0.7654939
    ##  [3,] 0.0496507386 0.059020938  0.0250341025   0.007229317 0.11254435 0.6943624
    ##  [4,] 0.0535653979 0.066529943  0.0304571061   0.009685913 0.14457681 0.6172697
    ##  [5,] 0.0546519225 0.062566918  0.0306303794   0.014231022 0.15100952 0.5824501
    ##  [6,] 0.0511502261 0.058173172  0.0301328168   0.013136348 0.17727999 0.5715018
    ##  [7,] 0.0423014256 0.057180839  0.0274429807   0.012945604 0.22492708 0.5147049
    ##  [8,] 0.0366461737 0.049697081  0.0237778489   0.011345920 0.30283361 0.4622780
    ##  [9,] 0.0357226904 0.046973552  0.0200096715   0.009936108 0.35758046 0.4290596
    ## [10,] 0.0590520969 0.041950636  0.0159402403   0.012271666 0.37517481 0.3963078
    ##       Pennsylvania Rhode.Island     Vermont
    ##  [1,]  0.000000000   0.00000000 0.000000000
    ##  [2,]  0.002722900   0.01957816 0.008707108
    ##  [3,]  0.004096371   0.03547743 0.012584389
    ##  [4,]  0.030509467   0.02780319 0.019602487
    ##  [5,]  0.048036203   0.03803586 0.018388092
    ##  [6,]  0.044314774   0.03697801 0.017332840
    ##  [7,]  0.051453322   0.03049509 0.038548780
    ##  [8,]  0.050731026   0.02643337 0.036256996
    ##  [9,]  0.044587906   0.02280304 0.033326973
    ## [10,]  0.045156461   0.01903255 0.035113740
    ## 
    ## $Pennsylvania
    ##       Connecticut      Maine Massachusetts New.Hampshire New.Jersey    New.York
    ##  [1,]  0.03334586 0.02107849   0.009862845  0.0003159543 0.02766503 0.005568692
    ##  [2,]  0.06988051 0.06063272   0.011243824  0.0036859399 0.07176112 0.065345243
    ##  [3,]  0.12174803 0.07879724   0.012404441  0.0063241221 0.06996450 0.069139023
    ##  [4,]  0.12079330 0.07720716   0.011103645  0.0092227431 0.10693240 0.065817833
    ##  [5,]  0.11544763 0.08855317   0.016777667  0.0254581542 0.10250384 0.069071035
    ##  [6,]  0.11098844 0.08341902   0.015683542  0.0246604578 0.13389025 0.080072531
    ##  [7,]  0.10046799 0.08216416   0.021560673  0.0224320650 0.17121955 0.077273245
    ##  [8,]  0.08006386 0.09031256   0.017236510  0.0196548183 0.24895739 0.090195154
    ##  [9,]  0.08378179 0.08201202   0.017602445  0.0175315458 0.28583869 0.120490944
    ## [10,]  0.11008794 0.07851486   0.014742265  0.0201582112 0.31696115 0.138585881
    ##       Pennsylvania Rhode.Island     Vermont
    ##  [1,]    0.9021631   0.00000000 0.000000000
    ##  [2,]    0.6896945   0.02664241 0.001113758
    ##  [3,]    0.5872109   0.04643667 0.007975065
    ##  [4,]    0.5178970   0.08224965 0.008776261
    ##  [5,]    0.4753694   0.09808115 0.008737902
    ##  [6,]    0.4449112   0.09348464 0.012889930
    ##  [7,]    0.4042568   0.09943023 0.021195316
    ##  [8,]    0.3298773   0.10680955 0.016892887
    ##  [9,]    0.2867397   0.09048943 0.015513398
    ## [10,]    0.2305762   0.07398636 0.016387188
    ## 
    ## $Rhode.Island
    ##       Connecticut      Maine Massachusetts New.Hampshire  New.Jersey
    ##  [1,] 0.002134956 0.08135268   0.003849300   0.001156081 0.016035041
    ##  [2,] 0.119278760 0.11239268   0.002574101   0.002096886 0.012034746
    ##  [3,] 0.142618843 0.11772053   0.013241582   0.009693438 0.009215628
    ##  [4,] 0.122262581 0.11322822   0.021971766   0.027696659 0.013915332
    ##  [5,] 0.118036667 0.11743680   0.020868175   0.026853430 0.026622061
    ##  [6,] 0.102863631 0.11053135   0.019733383   0.022902435 0.102290965
    ##  [7,] 0.087256047 0.10057845   0.026600365   0.024776440 0.191139284
    ##  [8,] 0.066852963 0.08957530   0.022687236   0.019579289 0.287966727
    ##  [9,] 0.075646704 0.08200591   0.017229270   0.021961465 0.339501449
    ## [10,] 0.086312345 0.06743511   0.019948359   0.020201588 0.326697902
    ##          New.York Pennsylvania Rhode.Island     Vermont
    ##  [1,] 0.002519092 5.215858e-05    0.8929007 0.000000000
    ##  [2,] 0.092940937 1.158484e-03    0.6533991 0.004124272
    ##  [3,] 0.097904467 7.310905e-03    0.5972412 0.005053431
    ##  [4,] 0.088539299 1.150464e-02    0.5923767 0.008504853
    ##  [5,] 0.089874562 1.359986e-02    0.5779616 0.008746872
    ##  [6,] 0.080869622 1.792855e-02    0.5351859 0.007694161
    ##  [7,] 0.074252221 1.959605e-02    0.4675931 0.008208036
    ##  [8,] 0.105103375 1.652884e-02    0.3850353 0.006670932
    ##  [9,] 0.130907825 2.119610e-02    0.3063568 0.005194482
    ## [10,] 0.177489394 3.188643e-02    0.2633811 0.006647774
    ## 
    ## $Vermont
    ##        Connecticut      Maine Massachusetts New.Hampshire  New.Jersey
    ##  [1,] 0.0008798163 0.01127104     0.1547894    0.01894688 0.009881452
    ##  [2,] 0.0022669510 0.02662967     0.1911013    0.01709382 0.015614822
    ##  [3,] 0.0052355375 0.02711962     0.2103242    0.01615315 0.014950728
    ##  [4,] 0.0072459163 0.03341658     0.1804257    0.01750799 0.016264898
    ##  [5,] 0.0309797599 0.03129654     0.2054534    0.01631316 0.015391058
    ##  [6,] 0.0379431535 0.02970623     0.1977841    0.01702172 0.034909655
    ##  [7,] 0.0373107970 0.05871074     0.1919747    0.01896489 0.035301188
    ##  [8,] 0.0417950135 0.06104379     0.1934812    0.01894305 0.033600739
    ##  [9,] 0.0550575757 0.05062158     0.1999179    0.04720644 0.035979722
    ## [10,] 0.0667566947 0.04389823     0.1942104    0.04300982 0.035313083
    ##          New.York Pennsylvania Rhode.Island   Vermont
    ##  [1,] 0.003890545   0.01194252  0.001066178 0.7873322
    ##  [2,] 0.015637418   0.01528549  0.002260197 0.7141104
    ##  [3,] 0.014740841   0.03416401  0.003490298 0.6738216
    ##  [4,] 0.017913547   0.10498098  0.017054529 0.6051898
    ##  [5,] 0.018330091   0.09605946  0.021166500 0.5650100
    ##  [6,] 0.017734942   0.09267953  0.036413098 0.5358075
    ##  [7,] 0.025946596   0.08783612  0.035770601 0.5081843
    ##  [8,] 0.025019658   0.09303381  0.040360815 0.4927219
    ##  [9,] 0.020859208   0.10456917  0.038506656 0.4472818
    ## [10,] 0.053296100   0.11792980  0.063242289 0.3823435
