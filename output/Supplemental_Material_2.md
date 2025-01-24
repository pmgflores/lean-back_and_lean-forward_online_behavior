### 1) Import data

Importing the data for the analysis

``` r
#library here uses the base of the R-project as initial directory
#JB stands for Judge Barrett
case_JB <- read.csv(here::here("data", "JudgeBarrettRTQT_R.csv"), header = TRUE)

#JJ stands for Judge Jackson
case_JJ <- read.csv(here::here("data", "JudgeJacksonRTQT_R.csv"), header = TRUE)

#LA stands for LA earthquake
case_LA <- read.csv(here::here("data", "LAearthquakeRTQT_R.csv"), header = TRUE)

#NZ stands for New Zealand earthquake
case_NZ <- read.csv(here::here("data", "NZearthquakeRTQT_R.csv"), header = TRUE)

#case_N20 <- read.csv("NBAFinals20RTQT_R.csv", header = TRUE)
case_N20 <- read.csv(here::here("data", "NBAFinals20RTQT_R.csv"), header = TRUE)

#case_N21 <- read.csv("NBAFinals21RTQT_R.csv", header = TRUE)
case_N21 <- read.csv(here::here("data", "NBAFinals21RTQT_R.csv"), header = TRUE)
```

### 2) Functions for the analysis

Import the set of functions used for the analysis from R script
“functions_2.R”

``` r
sys.source(here::here("scripts", "functions", "functions_2.R"), envir = knitr::knit_global())
```

### 3) Model for analysis

#### 3.1) Model definition

Model for the analysis of one-step information sharing process using all
five emotions.

``` r
#########################
#Model for the analysis #
#########################

library(lavaan)

#Model definition
model <- 'QT ~ a*fear 
          QT ~ b*anger
          QT ~ c*disgust
          QT ~ d*sadness
          QT ~ e*joy
          fear_qt ~ aa*QT
          anger_qt ~ bb*QT
          disgust_qt ~ cc*QT
          sadness_qt ~ dd*QT
          joy_qt ~ ee*QT
          
          #effect
          aaa := a*aa
          abb := a*bb
          acc := a*cc
          add := a*dd
          aee := a*ee
          baa := b*aa
          bbb := b*bb
          bcc := b*cc
          bdd := b*dd
          bee := b*ee
          caa := c*aa
          cbb := c*bb
          ccc := c*cc
          cdd := c*dd
          cee := c*ee
          daa := d*aa
          dbb := d*bb
          dcc := d*cc
          ddd := d*dd
          dee := d*ee
          eaa := e*aa
          ebb := e*bb
          ecc := e*cc
          edd := e*dd
          eee := e*ee'
```

#### 3.2) Global variables for analysis.

Definition of the variables used in the rest of the analysis for the
size of the sample, number of repetitions (bootstrapping), and
significance level.

``` r
# Size of the sample for each case
sample <- 100

# Number of times the sample is repeated
repetitions <- 30

# Significance level
sig_lev <- 0.008
```

### 4) Emotions dynamic analysis

#### 4.1) Judge Barrett case

Analysis for the Judge Barret case.

``` r
#QT_JB is the database obtained from retrieving quoted tweets
QT_JB <- read.csv(here::here("data", "JudgeBarrett_quoted_R.csv"), header = TRUE)

# Databases with number of samples equal to repetitions
db_JB <- create.ddbb(case_JB, QT_JB, repetitions, sample)

# Plot fot the QT pre, QT post, and RT levels (normalized)
QTRT_plot_JB <- create.QT_RT.plot(db_JB, repetitions, title="Figure SM 3: Average level of emotions JB case")

QTRT_plot_JB$QT_RT.plot
```

![](C:\Users\pmflo\OneDrive\Pablo%20Flores%20Bautista\UCDavis\PhD\Papers\2020%20Lean_github\lean-back_and_lean-forward_online_behavior\output\Supplemental_Material_2_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
QTRT_plot_JB$dif_pre_post_Table[,1:5]
```

    ##             pre LF    post LF         LB |pre LF - LB| |post LF - LB|
    ## sadness 0.23012745 0.24525124 0.22669911   0.003428334     0.01855213
    ## joy     0.24189963 0.26948200 0.28784679   0.045947154     0.01836479
    ## fear    0.06752696 0.07940161 0.05916902   0.008357941     0.02023259
    ## disgust 0.10719520 0.10033124 0.17402493   0.066829729     0.07369369
    ## anger   0.09070009 0.10408386 0.06712325   0.023576834     0.03696061

``` r
# List of model fit for each sample
model_fit_JB <- modelfit.list(db_JB, model, repetitions)

# Distribution of p-values and standard estimates
dist_pval_est.std_JB <- distribution_p_est.std(model_fit_JB, repetitions)

# Plot for the distribution of p-values and standard estimates
dist_pval_est.std_plot_JB <- distribution_p_est.std.plot(dist_pval_est.std_JB$dist_est.std,
                                                         dist_pval_est.std_JB$dist_pvalue)
```

    ## No id variables; using all as measure variables
    ## No id variables; using all as measure variables

``` r
# dist_pval_est.std_plot_JB$dist_est.std.plot
# dist_pval_est.std_plot_JB$dist_pvalue.plot

# Plot for model estimates
model_estimates_plot_JB <- create.model_estimates.plot(dist_pval_est.std_JB$dist_est.std,
                                                       dist_pval_est.std_JB$dist_pvalue,
                                                       significant = FALSE,
                                                       sig_level = sig_lev,
                                                       title="Figure SM 4: Effects JB case")

model_estimates_plot_JB
```

![](C:\Users\pmflo\OneDrive\Pablo%20Flores%20Bautista\UCDavis\PhD\Papers\2020%20Lean_github\lean-back_and_lean-forward_online_behavior\output\Supplemental_Material_2_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

#### 4.2) Judge Jackson case

Analysis for the Judge Jackson case.

``` r
#QT_JJ is the database obtained from retrieving quoted tweets
QT_JJ <- read.csv(here::here("data", "JudgeJackson_quoted_R.csv"), header = TRUE)

# Databases with number of samples equal to repetitions
db_JJ <- create.ddbb(case_JJ, QT_JJ, repetitions, sample)

# Plot fot the QT pre, QT post, and RT levels (normalized)
QTRT_plot_JJ <- create.QT_RT.plot(db_JJ, repetitions, title="Figure SM 5: Average level of emotions JJ case")

QTRT_plot_JJ$QT_RT.plot
```

![](C:\Users\pmflo\OneDrive\Pablo%20Flores%20Bautista\UCDavis\PhD\Papers\2020%20Lean_github\lean-back_and_lean-forward_online_behavior\output\Supplemental_Material_2_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
QTRT_plot_JJ$dif_pre_post_Table[,1:5]
```

    ##             pre LF   post LF         LB |pre LF - LB| |post LF - LB|
    ## sadness 0.19458323 0.2119187 0.18055545    0.01402778     0.03136327
    ## joy     0.25426942 0.2520281 0.28537029    0.03110087     0.03334222
    ## fear    0.07998543 0.0745483 0.05442823    0.02555721     0.02012007
    ## disgust 0.15753695 0.1773625 0.31254950    0.15501255     0.13518699
    ## anger   0.08501786 0.1091679 0.06540240    0.01961545     0.04376554

``` r
# List of model fit for each sample
model_fit_JJ <- modelfit.list(db_JJ, model, repetitions)

# Distribution of p-values and standard estimates
dist_pval_est.std_JJ <- distribution_p_est.std(model_fit_JJ, repetitions)

# Plot for the distribution of p-values and standard estimates
dist_pval_est.std_plot_JJ <- distribution_p_est.std.plot(dist_pval_est.std_JJ$dist_est.std,
                                                         dist_pval_est.std_JJ$dist_pvalue)
```

    ## No id variables; using all as measure variables
    ## No id variables; using all as measure variables

``` r
# dist_pval_est.std_plot_JJ$dist_est.std.plot
# dist_pval_est.std_plot_JJ$dist_pvalue.plot

# Plot for model estimates
model_estimates_plot_JJ <- create.model_estimates.plot(dist_pval_est.std_JJ$dist_est.std,
                                                       dist_pval_est.std_JJ$dist_pvalue,
                                                       significant = FALSE,
                                                       sig_level = sig_lev,
                                                       title="Figure SM 6: Effects JJ case")
model_estimates_plot_JJ
```

![](C:\Users\pmflo\OneDrive\Pablo%20Flores%20Bautista\UCDavis\PhD\Papers\2020%20Lean_github\lean-back_and_lean-forward_online_behavior\output\Supplemental_Material_2_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

#### 4.3) LA case

Analysis for the Los Angeles earthquake case.

``` r
#QT_LA is the database obtained from retrieving quoted tweets
QT_LA <- read.csv(here::here("data", "LAearthquake_quoted_R.csv"), header = TRUE)

# Databases with number of samples equal to repetitions
db_LA <- create.ddbb(case_LA, QT_LA, repetitions, n_sample = 150)

# Plot fot the QT pre, QT post, and RT levels (normalized)
QTRT_plot_LA <- create.QT_RT.plot(db_LA, repetitions, title="Figure SM 7: Average level of emotions LA case")

QTRT_plot_LA$QT_RT.plot
```

![](C:\Users\pmflo\OneDrive\Pablo%20Flores%20Bautista\UCDavis\PhD\Papers\2020%20Lean_github\lean-back_and_lean-forward_online_behavior\output\Supplemental_Material_2_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
QTRT_plot_LA$dif_pre_post_Table[,1:5]
```

    ##             pre LF    post LF         LB |pre LF - LB| |post LF - LB|
    ## sadness 0.22303578 0.20515151 0.23638021   0.013344434     0.03122870
    ## joy     0.29207715 0.39302391 0.34145548   0.049378331     0.05156842
    ## fear    0.20260893 0.15660563 0.17750635   0.025102571     0.02090073
    ## disgust 0.03992236 0.05199059 0.08417221   0.044249858     0.03218163
    ## anger   0.06301939 0.07445366 0.05823267   0.004786719     0.01622099

``` r
# List of model fit for each sample
model_fit_LA <- modelfit.list(db_LA, model, repetitions)

# Distribution of p-values and standard estimates
dist_pval_est.std_LA <- distribution_p_est.std(model_fit_LA, repetitions)

# Plot for the distribution of p-values and standard estimates
dist_pval_est.std_plot_LA <- distribution_p_est.std.plot(dist_pval_est.std_LA$dist_est.std,
                                                         dist_pval_est.std_LA$dist_pvalue)
```

    ## No id variables; using all as measure variables
    ## No id variables; using all as measure variables

``` r
# dist_pval_est.std_plot_LA$dist_est.std.plot
# dist_pval_est.std_plot_LA$dist_pvalue.plot

# Plot for model estimates
model_estimates_plot_LA <- create.model_estimates.plot(dist_pval_est.std_LA$dist_est.std,
                                                       dist_pval_est.std_LA$dist_pvalue,
                                                       significant = FALSE,
                                                       sig_level = sig_lev,
                                                       title="Figure SM 8: Effects LA case")
model_estimates_plot_LA
```

![](C:\Users\pmflo\OneDrive\Pablo%20Flores%20Bautista\UCDavis\PhD\Papers\2020%20Lean_github\lean-back_and_lean-forward_online_behavior\output\Supplemental_Material_2_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

#### 4.4) NZ case

Analysis for New Zealand earthquake case

``` r
#QT_NZ is the database obtained from retrieving quoted tweets
QT_NZ <- read.csv(here::here("data", "NZearthquake_quoted_R.csv"), header = TRUE)

# Databases with number of samples equal to repetitions
db_NZ <- create.ddbb(case_NZ, QT_NZ, repetitions, n_sample = 50)
#(case_ddbb, ddbb_QT, n_repetitions, n_sample)

# Plot fot the QT pre, QT post, and RT levels (normalized)
QTRT_plot_NZ <- create.QT_RT.plot(db_NZ, repetitions, title="Figure SM 9: Average level of emotions NZ case")

QTRT_plot_NZ$QT_RT.plot
```

![](C:\Users\pmflo\OneDrive\Pablo%20Flores%20Bautista\UCDavis\PhD\Papers\2020%20Lean_github\lean-back_and_lean-forward_online_behavior\output\Supplemental_Material_2_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
QTRT_plot_NZ$dif_pre_post_Table[,1:5]
```

    ##             pre LF    post LF         LB |pre LF - LB| |post LF - LB|
    ## sadness 0.30374923 0.23008699 0.26585933    0.03788990    0.035772333
    ## joy     0.20616120 0.32022878 0.31216907    0.10600787    0.008059711
    ## fear    0.26268125 0.20882774 0.21326835    0.04941290    0.004440605
    ## disgust 0.02272944 0.02969426 0.05242035    0.02969091    0.022726089
    ## anger   0.04465996 0.05370419 0.03302107    0.01163888    0.020683115

``` r
# List of model fit for each sample
model_fit_NZ <- modelfit.list(db_NZ, model, repetitions)

# Distribution of p-values and standard estimates
dist_pval_est.std_NZ <- distribution_p_est.std(model_fit_NZ, repetitions)

# Plot for the distribution of p-values and standard estimates
dist_pval_est.std_plot_NZ <- distribution_p_est.std.plot(dist_pval_est.std_NZ$dist_est.std,
                                                         dist_pval_est.std_NZ$dist_pvalue)
```

    ## No id variables; using all as measure variables
    ## No id variables; using all as measure variables

``` r
# dist_pval_est.std_plot_NZ$dist_est.std.plot
# dist_pval_est.std_plot_NZ$dist_pvalue.plot

# Plot for model estimates
model_estimates_plot_NZ <- create.model_estimates.plot(dist_pval_est.std_NZ$dist_est.std,
                                                       dist_pval_est.std_NZ$dist_pvalue,
                                                       significant = FALSE,
                                                       sig_level = sig_lev,
                                                       title="Figure SM 10: Effects NZ case")
model_estimates_plot_NZ
```

![](C:\Users\pmflo\OneDrive\Pablo%20Flores%20Bautista\UCDavis\PhD\Papers\2020%20Lean_github\lean-back_and_lean-forward_online_behavior\output\Supplemental_Material_2_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

#### 4.5) N20 case

Analysis for the NBA finals 2020 case

``` r
#QT_N20 is the database obtained from retrieving quoted tweets
QT_N20 <- read.csv(here::here("data", "NBAFinals20_quoted_R.csv"), header = TRUE)

# Databases with number of samples equal to repetitions
db_N20 <- create.ddbb(case_N20, QT_N20, repetitions, sample)

# Plot fot the QT pre, QT post, and RT levels (normalized)
QTRT_plot_N20 <- create.QT_RT.plot(db_N20, repetitions, title="Figure SM 11: Average level of emotions N20 case")

QTRT_plot_N20$QT_RT.plot
```

![](C:\Users\pmflo\OneDrive\Pablo%20Flores%20Bautista\UCDavis\PhD\Papers\2020%20Lean_github\lean-back_and_lean-forward_online_behavior\output\Supplemental_Material_2_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
QTRT_plot_N20$dif_pre_post_Table[,1:5]
```

    ##             pre LF    post LF         LB |pre LF - LB| |post LF - LB|
    ## sadness 0.23114957 0.20548713 0.24732271   0.016173136    0.041835574
    ## joy     0.37734385 0.46595259 0.45252704   0.075183193    0.013425547
    ## fear    0.07064106 0.06010257 0.06066363   0.009977438    0.000561058
    ## disgust 0.06783479 0.08858663 0.11988402   0.052049231    0.031297385
    ## anger   0.08611224 0.07620835 0.05751991   0.028592327    0.018688437

``` r
# List of model fit for each sample
model_fit_N20 <- modelfit.list(db_N20, model, repetitions)

# Distribution of p-values and standard estimates
dist_pval_est.std_N20 <- distribution_p_est.std(model_fit_N20, repetitions)

# Plot for the distribution of p-values and standard estimates
dist_pval_est.std_plot_N20 <- distribution_p_est.std.plot(dist_pval_est.std_N20$dist_est.std,
                                                          dist_pval_est.std_N20$dist_pvalue)
```

    ## No id variables; using all as measure variables
    ## No id variables; using all as measure variables

``` r
# dist_pval_est.std_plot_N20$dist_est.std.plot
# dist_pval_est.std_plot_N20$dist_pvalue.plot

# Plot for model estimates
model_estimates_plot_N20 <- create.model_estimates.plot(dist_pval_est.std_N20$dist_est.std,
                                                       dist_pval_est.std_N20$dist_pvalue,
                                                       significant = FALSE,
                                                       sig_level = sig_lev,
                                                       title="Figure SM 12: Effects N20 case")
model_estimates_plot_N20
```

![](C:\Users\pmflo\OneDrive\Pablo%20Flores%20Bautista\UCDavis\PhD\Papers\2020%20Lean_github\lean-back_and_lean-forward_online_behavior\output\Supplemental_Material_2_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

#### 4.6) N21 case

Analisys for the NBA finals 2021 case

``` r
#QT_N21 is the database obtained from retrieving quoted tweets
QT_N21 <- read.csv(here::here("data", "NBAFinals21_quoted_R.csv"), header = TRUE)

# Databases with number of samples equal to repetitions
db_N21 <- create.ddbb(case_N21, QT_N21, repetitions, sample)

# Plot fot the QT pre, QT post, and RT levels (normalized)
QTRT_plot_N21 <- create.QT_RT.plot(db_N21, repetitions, title="Figure SM 13: Average level of emotions N21 case")

QTRT_plot_N21$QT_RT.plot
```

![](C:\Users\pmflo\OneDrive\Pablo%20Flores%20Bautista\UCDavis\PhD\Papers\2020%20Lean_github\lean-back_and_lean-forward_online_behavior\output\Supplemental_Material_2_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
QTRT_plot_N21$dif_pre_post_Table[,1:5]
```

    ##             pre LF    post LF         LB |pre LF - LB| |post LF - LB|
    ## sadness 0.20954191 0.19265588 0.22383340   0.014291490    0.031177523
    ## joy     0.44569561 0.50120922 0.45376158   0.008065970    0.047447633
    ## fear    0.07222278 0.06184752 0.07093575   0.001287032    0.009088227
    ## disgust 0.05655119 0.10232485 0.12758969   0.071038496    0.025264837
    ## anger   0.06048724 0.06519472 0.05927692   0.001210328    0.005917801

``` r
# List of model fit for each sample
model_fit_N21 <- modelfit.list(db_N21, model, repetitions)

# Distribution of p-values and standard estimates
dist_pval_est.std_N21 <- distribution_p_est.std(model_fit_N21, repetitions)

# Plot for the distribution of p-values and standard estimates
dist_pval_est.std_plot_N21 <- distribution_p_est.std.plot(dist_pval_est.std_N21$dist_est.std,
                                                          dist_pval_est.std_N21$dist_pvalue)
```

    ## No id variables; using all as measure variables
    ## No id variables; using all as measure variables

``` r
# dist_pval_est.std_plot_N21$dist_est.std.plot
# dist_pval_est.std_plot_N21$dist_pvalue.plot

# Plot for model estimates
model_estimates_plot_N21 <- create.model_estimates.plot(dist_pval_est.std_N21$dist_est.std,
                                                       dist_pval_est.std_N21$dist_pvalue,
                                                       significant = FALSE,
                                                       sig_level = sig_lev,
                                                       title="Figure SM 14: Effects N21 case")
model_estimates_plot_N21
```

![](C:\Users\pmflo\OneDrive\Pablo%20Flores%20Bautista\UCDavis\PhD\Papers\2020%20Lean_github\lean-back_and_lean-forward_online_behavior\output\Supplemental_Material_2_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

### 5) Analysis by topic

#### 5.1) Political cases

Joint analysis for the group Judge Barret and Judge Jackson.

``` r
#Database with two hearings
db_pol <- list()

for (i in 1:repetitions){
  
  db_pol[[i]] <- rbind(db_JB[[i]], db_JJ[[i]])
}


# Plot fot the QT pre, QT post, and RT levels (normalized)
QTRT_plot_pol <- create.QT_RT.plot(db_pol, repetitions, title = "(a) Politics")

#QTRT_plot_pol$QT_RT.plot
QTRT_plot_pol$dif_pre_post_Table[,1:5]
```

    ##             pre LF    post LF         LB |pre LF - LB| |post LF - LB|
    ## sadness 0.21235534 0.22858498 0.20362728   0.008728056     0.02495770
    ## joy     0.24808453 0.26075503 0.28660854   0.038524012     0.02585351
    ## fear    0.07375619 0.07697495 0.05679862   0.016957573     0.02017633
    ## disgust 0.13236608 0.13884687 0.24328721   0.110921137     0.10444034
    ## anger   0.08785897 0.10662590 0.06626283   0.021596143     0.04036307

``` r
# List of model fit for each sample
model_fit_pol <- modelfit.list(db_pol, model, repetitions)

# Distribution of p-values and standard estimates
dist_pval_est.std_pol <- distribution_p_est.std(model_fit_pol, repetitions)

# Plot for the distribution of p-values and standard estimates
dist_pval_est.std_plot_pol <- distribution_p_est.std.plot(dist_pval_est.std_pol$dist_est.std,
                                                          dist_pval_est.std_pol$dist_pvalue)
```

    ## No id variables; using all as measure variables
    ## No id variables; using all as measure variables

``` r
# dist_pval_est.std_plot_pol$dist_est.std.plot
# dist_pval_est.std_plot_pol$dist_pvalue.plot

# Plot for model estimates
model_estimates_plot_pol <- create.model_estimates.plot(dist_pval_est.std_pol$dist_est.std,
                                                       dist_pval_est.std_pol$dist_pvalue,
                                                       significant = TRUE,
                                                       sig_level = sig_lev,
                                                       title = "(a) Politics")
# model_estimates_plot_pol

# table with model standard estimates and p value
std_estimates_pol <- data.frame(cbind(colMeans(dist_pval_est.std_pol$dist_est.std),
                                      colMeans(dist_pval_est.std_pol$dist_pvalue)))

colnames(std_estimates_pol) <- c('est.std.pol', 'p.value.pol')
```

#### 5.2) Earthquake cases

Joint analysis for the group LA earthquake and NZ earthquake.

``` r
#Database with two earthquakes
db_etq <- list()

for (i in 1:repetitions){
  
  db_etq[[i]] <- rbind(db_LA[[i]], db_NZ[[i]])
}


# Plot fot the QT pre, QT post, and RT levels (normalized)
QTRT_plot_etq <- create.QT_RT.plot(db_etq, repetitions, title = "(b) Earthquakes")

# QTRT_plot_etq$QT_RT.plot
QTRT_plot_etq$dif_pre_post_Table[,1:5]
```

    ##             pre LF    post LF         LB |pre LF - LB| |post LF - LB|
    ## sadness 0.24321414 0.21138538 0.24374999  0.0005358498     0.03236461
    ## joy     0.27059816 0.37482513 0.33413388  0.0635357162     0.04069125
    ## fear    0.21762701 0.16966116 0.18644685  0.0311801547     0.01678570
    ## disgust 0.03562413 0.04641650 0.07623425  0.0406101200     0.02981774
    ## anger   0.05842953 0.06926629 0.05192977  0.0064997598     0.01733652

``` r
# List of model fit for each sample
model_fit_etq <- modelfit.list(db_etq, model, repetitions)

# Distribution of p-values and standard estimates
dist_pval_est.std_etq <- distribution_p_est.std(model_fit_etq, repetitions)

# Plot for the distribution of p-values and standard estimates
dist_pval_est.std_plot_etq <- distribution_p_est.std.plot(dist_pval_est.std_etq$dist_est.std,
                                                          dist_pval_est.std_etq$dist_pvalue)
```

    ## No id variables; using all as measure variables
    ## No id variables; using all as measure variables

``` r
# dist_pval_est.std_plot_etq$dist_est.std.plot
# dist_pval_est.std_plot_etq$dist_pvalue.plot

# Plot for model estimates
model_estimates_plot_etq <- create.model_estimates.plot(dist_pval_est.std_etq$dist_est.std,
                                                       dist_pval_est.std_etq$dist_pvalue,
                                                       significant = TRUE,
                                                       sig_level = sig_lev,
                                                       title = "(b) Earthquakes")
# model_estimates_plot_etq

# table with model standard estimates and p value
std_estimates_etq <- data.frame(cbind(colMeans(dist_pval_est.std_etq$dist_est.std),
                                      colMeans(dist_pval_est.std_etq$dist_pvalue)))

colnames(std_estimates_etq) <- c('est.std.etq', 'p.value.etq')
```

#### 5.3) Sport cases

Joint analysis for the group NBA finals 2020 and NBA finals 2021

``` r
#Database with two NBA finals
db_spo <- list()

for (i in 1:repetitions){
  
  db_spo[[i]] <- rbind(db_N20[[i]], db_N21[[i]])
}


# Plot fot the QT pre, QT post, and RT levels (normalized)
QTRT_plot_spo <- create.QT_RT.plot(db_spo, repetitions, title = "(c) Sports")

# QTRT_plot_spo$QT_RT.plot
QTRT_plot_spo$dif_pre_post_Table[,1:5]
```

    ##             pre LF    post LF         LB |pre LF - LB| |post LF - LB|
    ## sadness 0.22034574 0.19907150 0.23557805   0.015232313    0.036506548
    ## joy     0.41151973 0.48358090 0.45314431   0.041624581    0.030436590
    ## fear    0.07143192 0.06097504 0.06579969   0.005632235    0.004824643
    ## disgust 0.06219299 0.09545574 0.12373685   0.061543863    0.028281111
    ## anger   0.07329974 0.07070153 0.05839841   0.014901328    0.012303119

``` r
# List of model fit for each sample
model_fit_spo <- modelfit.list(db_spo, model, repetitions)

# Distribution of p-values and standard estimates
dist_pval_est.std_spo <- distribution_p_est.std(model_fit_spo, repetitions)

# Plot for the distribution of p-values and standard estimates
dist_pval_est.std_plot_spo <- distribution_p_est.std.plot(dist_pval_est.std_spo$dist_est.std,
                                                          dist_pval_est.std_spo$dist_pvalue)
```

    ## No id variables; using all as measure variables
    ## No id variables; using all as measure variables

``` r
# dist_pval_est.std_plot_spo$dist_est.std.plot
# dist_pval_est.std_plot_spo$dist_pvalue.plot

# Plot for model estimates
model_estimates_plot_spo <- create.model_estimates.plot(dist_pval_est.std_spo$dist_est.std,
                                                       dist_pval_est.std_spo$dist_pvalue,
                                                       significant = TRUE,
                                                       sig_level = sig_lev,
                                                       title = "(c) Sports")
# model_estimates_plot_spo

# table with model standard estimates and p value
std_estimates_spo <- data.frame(cbind(colMeans(dist_pval_est.std_spo$dist_est.std),
                                      colMeans(dist_pval_est.std_spo$dist_pvalue)))

colnames(std_estimates_spo) <- c('est.std.spo', 'p.value.spo')
```

#### 5.4) Manuscript Figure 4: Significant effects of lean-forward behavior using front-door criterion.

``` r
library(ggpubr)

ggarrange(model_estimates_plot_pol,
          model_estimates_plot_etq,
          model_estimates_plot_spo,
          ncol = 2, nrow = 2, common.legend = TRUE, legend="bottom")
```

![](C:\Users\pmflo\OneDrive\Pablo%20Flores%20Bautista\UCDavis\PhD\Papers\2020%20Lean_github\lean-back_and_lean-forward_online_behavior\output\Supplemental_Material_2_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
#Standard estimates for SCM
SCM_table <- cbind(std_estimates_pol,std_estimates_etq,std_estimates_spo)
SCM_table
```

    ##                est.std.pol  p.value.pol   est.std.etq  p.value.etq  est.std.spo
    ## fear -> M     0.1060311580 1.483946e-01 -0.0782054067 2.811310e-01 -0.085366329
    ## ang -> M      0.2131490182 2.388888e-03  0.1253116188 9.674588e-02  0.072683915
    ## dis -> M     -0.3140621892 1.527207e-04 -0.2771642232 3.671927e-07 -0.216794981
    ## sad -> M     -0.0082119550 5.336768e-01 -0.1101556916 1.601931e-01 -0.183794396
    ## joy -> M     -0.1016808094 2.190167e-01  0.0160463327 5.340319e-01 -0.083419335
    ## M -> fear     0.1434765205 3.355863e-02  0.0955597297 1.662011e-01  0.042062719
    ## M -> ang      0.1491442742 3.259098e-02  0.0510113842 3.139722e-01  0.111154909
    ## M -> dis     -0.3161981069 6.414920e-09 -0.3490755116 6.763806e-11 -0.369896485
    ## M -> sad      0.0336956457 4.681323e-01 -0.0016039961 5.545732e-01 -0.053007188
    ## M -> joy     -0.1008640456 1.287220e-01 -0.1698568551 5.676677e-03 -0.095069288
    ## fear -> fear  0.0176852358 1.920504e-01 -0.0061060650 4.504549e-01 -0.001511136
    ## fear -> ang   0.0164339865 1.987949e-01 -0.0041002702 5.103271e-01 -0.010180865
    ## fear -> dis  -0.0334117952 1.534849e-01  0.0270286599 2.858880e-01  0.031794959
    ## fear -> sad   0.0036967504 5.290893e-01  0.0004571732 6.407210e-01  0.004669068
    ## fear -> joy  -0.0118328455 2.610975e-01  0.0129619397 3.098061e-01  0.007525461
    ## ang -> fear   0.0306810168 5.211806e-02  0.0122637741 2.664148e-01  0.003665577
    ## ang -> ang    0.0336675500 5.083872e-02  0.0077172571 3.627119e-01  0.009192064
    ## ang -> dis   -0.0675943069 3.906750e-03 -0.0434607234 1.008025e-01 -0.026586289
    ## ang -> sad    0.0067309051 4.809526e-01  0.0005972599 5.894538e-01 -0.004062189
    ## ang -> joy   -0.0213983472 1.507226e-01 -0.0215934435 1.275690e-01 -0.006875316
    ## dis -> fear  -0.0443450472 4.094143e-02 -0.0263361279 1.754349e-01 -0.008278541
    ## dis -> ang   -0.0458728236 3.867832e-02 -0.0141043977 3.227275e-01 -0.023964135
    ## dis -> dis    0.1004900776 4.783928e-04  0.0972420910 2.509850e-05  0.081154760
    ## dis -> sad   -0.0105099653 4.737184e-01  0.0009260419 5.587474e-01  0.011883011
    ## dis -> joy    0.0312205429 1.390732e-01  0.0468576388 9.620157e-03  0.019909781
    ## sad -> fear  -0.0010272782 5.488161e-01 -0.0102050221 3.442519e-01 -0.007685657
    ## sad -> ang   -0.0013758740 5.516712e-01 -0.0054631507 4.414169e-01 -0.020791632
    ## sad -> dis    0.0029519422 5.369061e-01  0.0385268914 1.653286e-01  0.068031332
    ## sad -> sad    0.0001642593 6.852448e-01  0.0015874821 6.213250e-01  0.010741403
    ## sad -> joy    0.0012812761 5.703005e-01  0.0182247830 1.938519e-01  0.017789209
    ## joy -> fear  -0.0147435710 2.698251e-01  0.0015662556 6.137204e-01 -0.002843178
    ## joy -> ang   -0.0145618426 2.756339e-01  0.0011513568 6.590819e-01 -0.010033637
    ## joy -> dis    0.0323791175 2.250803e-01 -0.0055656541 5.365781e-01  0.031354679
    ## joy -> sad   -0.0042610616 5.559012e-01  0.0003702263 7.171721e-01  0.004572463
    ## joy -> joy    0.0115141501 3.197663e-01 -0.0020187968 5.480129e-01  0.009327147
    ## M~~M          0.8055836078 0.000000e+00  0.8831371277 0.000000e+00  0.921074342
    ##               p.value.spo
    ## fear -> M    2.761428e-01
    ## ang -> M     3.362436e-01
    ## dis -> M     5.431747e-03
    ## sad -> M     5.784444e-02
    ## joy -> M     3.126863e-01
    ## M -> fear    3.307580e-01
    ## M -> ang     1.070649e-01
    ## M -> dis     1.068903e-11
    ## M -> sad     3.393295e-01
    ## M -> joy     1.546488e-01
    ## fear -> fear 5.532660e-01
    ## fear -> ang  3.497747e-01
    ## fear -> dis  2.795082e-01
    ## fear -> sad  5.145026e-01
    ## fear -> joy  4.266833e-01
    ## ang -> fear  5.123581e-01
    ## ang -> ang   3.898639e-01
    ## ang -> dis   3.398208e-01
    ## ang -> sad   5.476968e-01
    ## ang -> joy   4.356953e-01
    ## dis -> fear  3.511052e-01
    ## dis -> ang   1.315907e-01
    ## dis -> dis   6.673670e-03
    ## dis -> sad   3.562501e-01
    ## dis -> joy   1.807130e-01
    ## sad -> fear  3.882506e-01
    ## sad -> ang   1.785714e-01
    ## sad -> dis   6.064589e-02
    ## sad -> sad   3.854069e-01
    ## sad -> joy   2.362506e-01
    ## joy -> fear  5.434169e-01
    ## joy -> ang   3.944525e-01
    ## joy -> dis   3.162502e-01
    ## joy -> sad   5.453226e-01
    ## joy -> joy   4.126150e-01
    ## M~~M         0.000000e+00

#### 5.5) Manuscript Figure A1: Average level of emotions for lean-back and lean-forward behaviors.

``` r
ggarrange(QTRT_plot_pol$QT_RT.plot,
          QTRT_plot_etq$QT_RT.plot,
          QTRT_plot_spo$QT_RT.plot,
          ncol = 2, nrow = 2, common.legend = TRUE, legend="bottom")
```

![](C:\Users\pmflo\OneDrive\Pablo%20Flores%20Bautista\UCDavis\PhD\Papers\2020%20Lean_github\lean-back_and_lean-forward_online_behavior\output\Supplemental_Material_2_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->
