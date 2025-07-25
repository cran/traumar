
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![R-CMD-check](https://github.com/bemts-hhs/traumar/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bemts-hhs/traumar/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/traumar)](https://CRAN.R-project.org/package=traumar)
[![Codecov test
coverage](https://codecov.io/gh/bemts-hhs/traumar/graph/badge.svg)](https://app.codecov.io/gh/bemts-hhs/traumar)
<!-- badges: end -->

# traumar <a href="https://bemts-hhs.github.io/traumar/"><img src="man/figures/logo.png" align="right" height="137" alt="traumar website" /></a>

Continuous Quality Improvement (CQI) and Process Improvement (PI) are
essential pillars of healthcare, particularly in the care of injured
patients. However, hospitals, trauma systems, and their trauma program
managers (TPMs) often lack access to standardized quality measures
derived from academic literature. The {traumar} package addresses this
gap by providing tools to calculate quality measures from trauma center
or trauma system effectiveness and efficiency, to relative mortality
efficiently and accurately. By automating these calculations, {traumar}
empowers hospital systems, trauma networks, and TPMs to focus their
efforts on analyzing outcomes and driving meaningful improvements in
patient care. Whether you’re seeking to enhance PI initiatives or
streamline CQI processes, {traumar} serves as a valuable resource for
advancing trauma care quality.

## Installation

You can install the development version of `traumar` from
[GitHub](https://github.com/bemts-hhs/traumar) with:

``` r
# install.packages("remotes")
remotes::install_github("bemts-hhs/traumar")
```

Additionally, you can install the CRAN version of `traumar` via:

``` r
install.packages("traumar")
```

## Helper Functions

{traumar} has many functions to help you in your data analysis journey!
In particular, if you do not presently have access to probability of
survival data, {traumar} provides the `probability_of_survival()`
function to do just that using the TRISS method. Check out the
additional package documentation at
<https://bemts-hhs.github.io/traumar/> where you can find examples of
each function the package has to offer.

## Trauma System Evaluation and Quality Improvement

{traumar} includes functions to allow users to calculate the efficiency
and effectiveness of one or more trauma centers, or even a trauma
system. The `seqic_indicator_*` family of functions affords users the
logic and formulas to assess their center or system on topics of
importance such as:

- Surgeon/physician/mid-level provider response times
- Data Quality
- Rate of autopsy among deceased trauma patients
- Blood alcohol measurements / drug testing
- Analysis of mortality among risk groups
- Time from patient arrival to a) physical discharge, b) decision, c)
  time from decision to physical discharge
- Trauma team activations
- Under and over triage
- …among many others. Please check the {traumar}
  <https://bemts-hhs.github.io/traumar/> documentation to explore all
  these analytical opportunities

## Calculating the W-Score

The W-Score tells us how many survivals (or deaths) on average out of
every 100 cases seen in a trauma center. Using R, we can do this with
the {traumar} package.

### First, we will create the data for these examples

``` r

# Generate example data
set.seed(123)

# Parameters
n_patients <- 5000  # Total number of patients

groups <- sample(x = LETTERS[1:2], size = n_patients, replace = TRUE) # Arbitrary group labels

trauma_type_values <- sample(x = c("Blunt", "Penetrating"), size = n_patients, replace = TRUE) # Trauma types

rts_values <- sample(x = seq(from = 0, to = 7.8408, by = 0.005), size = n_patients, replace = TRUE) # RTS values

ages <- sample(x = seq(from = 0, to = 100, by = 1), size = n_patients, replace = TRUE) # patient ages

iss_scores <- sample(x = seq(from = 0, to = 75, by = 1), size = n_patients, replace = TRUE) # ISS scores

# Generate survival probabilities (Ps)
Ps <- traumar::probability_of_survival(trauma_type = trauma_type_values, age = ages, rts = rts_values, iss = iss_scores)

# Simulate survival outcomes based on Ps
survival_outcomes <- rbinom(n_patients, size = 1, prob = Ps)

# Create data frame
data <- data.frame(Ps = Ps, survival = survival_outcomes, groups = groups) |>
dplyr::mutate(death = dplyr::if_else(survival == 1, 0, 1))
```

### The W-Score!

``` r

# Calculate trauma performance (W, M, Z scores)
trauma_performance(data, Ps_col = Ps, outcome_col = death)
#> # A tibble: 1 × 9
#>   N_Patients N_Survivors N_Deaths Predicted_Survivors Predicted_Deaths
#>        <int>       <int>    <int>               <dbl>            <dbl>
#> 1       5000        1701     3299               1711.            3289.
#> # ℹ 4 more variables: Patient_Estimate <dbl>, W_Score <dbl>, M_Score <dbl>,
#> #   Z_Score <dbl>
```

## Comparing the Probability of Survival Distribution of your Patient Mix to the [Major Trauma Outcomes Study](https://journals.lww.com/jtrauma/Abstract/1990/11000/The_Major_Trauma_Outcome_Study__Establishing.8.aspx)

The M and Z scores are calculated using methods defined in the
[literature](https://journals.lww.com/jtrauma/abstract/1978/10000/a_method_for_comparing_survival_of_burn_patients.3.aspx)
may not be meaningful if your the distribution of the probability of
survival measure is not similar enough to the Major Trauma Outcomes
Study distribution. {traumar} provides a way to check this in your data
analysis script, or even from the console. The `trauma_performance()`
function does this under the hood for you, so you can get a read out of
how much confidence you can put into the Z score.

``` r

# Compare the current case mix with the MTOS case mix
trauma_case_mix(data, Ps_col = Ps, outcome_col = death)
#>      Ps_range current_fraction MTOS_distribution survivals predicted_survivals
#> 1 0.00 - 0.25           0.5414             0.010      2534            187.9191
#> 2 0.26 - 0.50           0.1432             0.043       468            269.5871
#> 3 0.51 - 0.75           0.1278             0.000       217            405.8877
#> 4 0.76 - 0.90           0.0870             0.052        58            366.1312
#> 5 0.91 - 0.95           0.0508             0.053        18            237.6390
#> 6 0.96 - 1.00           0.0498             0.842         4            243.4833
#>   deaths predicted_deaths count
#> 1    173      2519.080869  2707
#> 2    248       446.412896   716
#> 3    422       233.112251   639
#> 4    377        68.868790   435
#> 5    236        16.361033   254
#> 6    245         5.516716   249
```

## The Relative Mortality Metric

Napoli et al.(2017) published methods for calculating a measure of
trauma center (or system) performance while overcoming a problem with
the W-Score and the TRISS methodology. Given that the majority of
patients seen at trauma centers will have a probability of survival over
90%, estimating performance based on the W-Score may only indicate how
well a center performed with lower acuity patients. Using Napoli et
al. (2017), it is possible to calculate a score that is similar to the
W-Score in its interpretability, but deals with the negatively skewed
probability of survival problem by creating non-linear bins of score
ranges, and then weighting a score based on the nature of those bins.
The Relative Mortality Metric (RMM) has a scale from -1 to 1.

- An RMM of 0 indicates that the observed mortality aligns with the
  expected national benchmark across all acuity levels.
- An RMM greater than 0 indicates better-than-expected performance,
  where the center is outperforming the national benchmark.
- An RMM less than 0 indicates under-performance, where the center’s
  observed mortality is higher than the expected benchmark.

## Non-Linear Binning Algorithm

An important part of the approach Napoli et al. (2017) took was to
modify the M-Score approach of looking at linear bins of the probability
of survival distribution, and make it non-linear. The {traumar} package
does this for you using Dr. Napoli’s method:

``` r

# Apply the nonlinear_bins function
results <- nonlinear_bins(data = data,
                         Ps_col = Ps,
                         outcome_col = survival,
                         divisor1 = 4,
                         divisor2 = 4,
                         threshold_1 = 0.9,
                         threshold_2 = 0.99)

# View intervals created by the algorithm
results$intervals
#> [1] 0.0002015449 0.0256191282 0.1455317587 0.4842820556 0.9003870455
#> [6] 0.9285354475 0.9518925450 0.9722272703 0.9968989233

# View the bin statistics
results$bin_stats
#> # A tibble: 8 × 13
#>   bin_number bin_start bin_end    mean      sd Pred_Survivors_b Pred_Deaths_b
#>        <int>     <dbl>   <dbl>   <dbl>   <dbl>            <dbl>         <dbl>
#> 1          1  0.000202  0.0256 0.00935 0.00722             10.4       1106.  
#> 2          2  0.0256    0.146  0.0732  0.0345              81.7       1033.  
#> 3          3  0.146     0.484  0.293   0.0959             327.         788.  
#> 4          4  0.484     0.900  0.697   0.124              777.         337.  
#> 5          5  0.900     0.929  0.916   0.00790            114.          10.5 
#> 6          6  0.929     0.952  0.940   0.00680            117.           7.50
#> 7          7  0.952     0.972  0.963   0.00564            120.           4.65
#> 8          8  0.972     0.997  0.984   0.00686            162.           2.67
#> # ℹ 6 more variables: AntiS_b <dbl>, AntiM_b <dbl>, alive <int>, dead <int>,
#> #   count <int>, percent <dbl>
```

## The RMM function

The RMM is sensitive to higher acuity patients, meaning that if a trauma
center struggles with these patients, it will be reflected in the RMM.
In contrast, the W-Score may mask declines in performance due to the
influence of lower acuity patients via the MTOS Distribution. The
{traumar} package automates RMM calculation as a single score using the
nonlinear binning method from Napoli et al. (2017). The `rmm()` and
`rm_bin_summary()` functions internally call `nonlinear_bins()` to
generate the non-linear binning process. The function uses bootstrap
sampling with `n_samples` to simulate an RMM distribution and estimate
95% confidence intervals. The RMM, along with corresponding confidence
intervals, are provided for the population in `data`, as well.

``` r

# Example usage of the `rmm()` function
rmm(data = data,
    Ps_col = Ps,
    outcome_col = survival,
    n_samples = 250,
    Divisor1 = 4,
    Divisor2 = 4
    )
#> # A tibble: 1 × 8
#>   population_RMM_LL population_RMM population_RMM_UL population_CI
#>               <dbl>          <dbl>             <dbl>         <dbl>
#> 1            -0.106        0.00247             0.111         0.108
#> # ℹ 4 more variables: bootstrap_RMM_LL <dbl>, bootstrap_RMM <dbl>,
#> #   bootstrap_RMM_UL <dbl>, bootstrap_CI <dbl>

# Pivoting can be helpful at times
rmm(
  data = data,
  Ps_col = Ps,
  outcome_col = survival,
  n_samples = 250,
  Divisor1 = 4,
  Divisor2 = 4,
  pivot = TRUE
)
#> # A tibble: 8 × 2
#>   stat                  value
#>   <chr>                 <dbl>
#> 1 population_RMM_LL -0.106   
#> 2 population_RMM     0.00247 
#> 3 population_RMM_UL  0.111   
#> 4 population_CI      0.108   
#> 5 bootstrap_RMM_LL   0.000492
#> 6 bootstrap_RMM      0.00218 
#> 7 bootstrap_RMM_UL   0.00386 
#> 8 bootstrap_CI       0.00168

# RMM calculated by non-linear bin range
# `rm_bin_summary()` function
rm_bin_summary(data = data,
               Ps_col = Ps,
               outcome_col = survival,
               Divisor1 = 4,
               Divisor2 = 4,
               n_samples = 250
               )
#> # A tibble: 8 × 19
#>   bin_number  TA_b  TD_b   N_b  EM_b AntiS_b AntiM_b bin_start bin_end midpoint
#>        <int> <int> <int> <int> <dbl>   <dbl>   <dbl>     <dbl>   <dbl>    <dbl>
#> 1          1     9  1107  1116 0.992 0.00935  0.991   0.000202  0.0256   0.0129
#> 2          2    72  1043  1115 0.935 0.0732   0.927   0.0256    0.146    0.0856
#> 3          3   300   815  1115 0.731 0.293    0.707   0.146     0.484    0.315 
#> 4          4   805   309  1114 0.277 0.697    0.303   0.484     0.900    0.692 
#> 5          5   115    10   125 0.08  0.916    0.0844  0.900     0.929    0.914 
#> 6          6   116     9   125 0.072 0.940    0.0600  0.929     0.952    0.940 
#> 7          7   119     6   125 0.048 0.963    0.0372  0.952     0.972    0.962 
#> 8          8   165     0   165 0     0.984    0.0162  0.972     0.997    0.985 
#> # ℹ 9 more variables: R_b <dbl>, population_RMM_LL <dbl>, population_RMM <dbl>,
#> #   population_RMM_UL <dbl>, population_CI <dbl>, bootstrap_RMM_LL <dbl>,
#> #   bootstrap_RMM <dbl>, bootstrap_RMM_UL <dbl>, bootstrap_CI <dbl>
```

## Code of Conduct

Please note that the traumar project is released with a [Contributor
Code of
Conduct](https://bemts-hhs.github.io/traumar/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
