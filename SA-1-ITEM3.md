SA ITEM_3
================

3.  By generating 10,000 searches in R, carry out a simulation
    experiment for a search engine going through a list of sites for a
    given key phrase, until the key phrase is found. You should allow
    your program to input the probability p that any site will contain
    the key phrase

4.  Plot the simulated pdf and calculate its mean and variance, and

5.  Obtain the simulated conditional distribution of searches when three
    searches have been carried out without success. Calculate its mean
    and variance, and satisfy yourself that they are equivalent to the
    simulated distribution of the complete set.

As test data assume each site has a 60% chance of containing the key
phrase. To satisfy yourself that the Markov memoryless property holds,
obtain estimates of a) P ( X = 4 \| X \> 3) and P ( X = 1 ) b) P ( X = 5
\| X \> 3) and P ( X = 2 )

where X is the number of searches to the first success

``` r
set.seed(123) # this will help us later for documenation to keep data avoid from repetition

p <- 0.6

simulate_search <- function(p, n_searches) {
  successes <- numeric()
  for (i in 1:10000) {
    search_count <- 0
    while (TRUE) {
      search_count <- search_count + 1
      if (runif(1) <= p) break 
    }
    successes[i] <- search_count
  }
  successes
}

search_results <- simulate_search(p, 10000)

hist(search_results, breaks = 30, freq = FALSE, main = "Simulated PDF of Searches", xlab = "Number of Searches")
```

![](SA-1-ITEM3_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
mean_search <- mean(search_results)
var_search <- var(search_results)
cat("Mean of the simulated pdf:", mean_search, "\n")
```

    ## Mean of the simulated pdf: 1.6497

``` r
cat("Variance of the simulated pdf:", var_search, "\n","\n")
```

    ## Variance of the simulated pdf: 1.029493 
    ## 

``` r
search_results_conditional <- search_results[search_results > 3]

mean_conditional <- mean(search_results_conditional)
var_conditional <- var(search_results_conditional)
cat("Mean of the simulated pdf when 3 searches were unsuccesful:", mean_conditional, "\n")
```

    ## Mean of the simulated pdf when 3 searches were unsuccesful: 4.559738

``` r
cat("Variance of the simulated pdf when 3 searches were unsuccesful:", var_conditional, "\n","\n")
```

    ## Variance of the simulated pdf when 3 searches were unsuccesful: 0.9222452 
    ## 

``` r
prob_X4_given_X3 <- sum(search_results == 4) / sum(search_results > 3)
prob_X1 <- sum(search_results == 1) / length(search_results)

prob_X5_given_X3 <- sum(search_results == 5) / sum(search_results > 3)
prob_X2 <- sum(search_results == 2) / length(search_results)
cat("Estimate of P(X=4 | X>3):", prob_X4_given_X3, "\n")
```

    ## Estimate of P(X=4 | X>3): 0.6579378

``` r
cat("Estimate of P(X=1):", prob_X1, "\n")
```

    ## Estimate of P(X=1): 0.6011

``` r
cat("Estimate of P(X=5 | X>3):", prob_X5_given_X3, "\n")
```

    ## Estimate of P(X=5 | X>3): 0.2045827

``` r
cat("Estimate of P(X=2):", prob_X2, "\n")
```

    ## Estimate of P(X=2): 0.2434

Explanation:

The average number of searches required to discover the key term is
1.65, according to the mean value of 1.6497. This result is in line with
what was anticipated given the success probability, which is 60% of the
time to discover the key term in a single search. The variance indicates
a substantial range or scattering of the data around the mean, at around
1.029493. This suggests that although there is some variation in the
quantity of searches needed, it is not overly broad.

Now for the part where we asked to find the simulated conditional
distribution of searches when three searches have been carried out
without success, this is basically searches with 3 errors or no success.
With a mean value of about 4.559738, it takes, on average, 4.56 more
searches to identify the key word after three failed attempts, this
average rises relative to the total mean when more searches are required
following first failures. In comparison to the total variation, the
variance of around 0.9222452 indicates that there is less fluctuation in
the number of searches needed following three failed efforts. This
decline in variance might point to a more regular pattern in the
quantity of searches required following first failures.

For part a and part b, With over three unsuccessful searches, the
estimate of around 0.6579378 indicates that there is approximately a
65.79% probability that the key word will be located on the fourth
search. This likelihood corresponds with the higher average number of
searches needed following three failed tries. With an estimate of around
0.6011, there is roughly a 60.11% chance of discovering the key term in
the initial search. This likelihood is consistent with the success rate
that is specified for every search. Given that more than three searches
were performed without result, there is around a 20.46% probability of
discovering the key word on the fifth search, based on an estimate of
0.2045827. This likelihood is consistent with the need for more searches
following early failures. With an estimate of around 0.2434, there is
about a 24.34% probability of discovering the crucial term in the
subsequent search. This likelihood is also consistent with the success
rate that is specified for every single search.

Since we use set.seed(123) to keep the data and make sure that every
time we run this program, we can keep the data.

Lets run some test trials:

Trial one:

``` r
p <- 0.6

simulate_search <- function(p, n_searches) {
  successes <- numeric()
  for (i in 1:10000) {
    search_count <- 0
    while (TRUE) {
      search_count <- search_count + 1
      if (runif(1) <= p) break 
    }
    successes[i] <- search_count
  }
  successes
}

search_results <- simulate_search(p, 10000)

hist(search_results, breaks = 30, freq = FALSE, main = "Simulated PDF of Searches", xlab = "Number of Searches")
```

![](SA-1-ITEM3_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
mean_search <- mean(search_results)
var_search <- var(search_results)
cat("Mean of the simulated pdf:", mean_search, "\n")
```

    ## Mean of the simulated pdf: 1.6633

``` r
cat("Variance of the simulated pdf:", var_search, "\n","\n")
```

    ## Variance of the simulated pdf: 1.097243 
    ## 

``` r
search_results_conditional <- search_results[search_results > 3]

mean_conditional <- mean(search_results_conditional)
var_conditional <- var(search_results_conditional)
cat("Mean of the simulated pdf when 3 searches were unsuccesful:", mean_conditional, "\n")
```

    ## Mean of the simulated pdf when 3 searches were unsuccesful: 4.64031

``` r
cat("Variance of the simulated pdf when 3 searches were unsuccesful:", var_conditional, "\n","\n")
```

    ## Variance of the simulated pdf when 3 searches were unsuccesful: 0.9822235 
    ## 

``` r
prob_X4_given_X3 <- sum(search_results == 4) / sum(search_results > 3)
prob_X1 <- sum(search_results == 1) / length(search_results)

prob_X5_given_X3 <- sum(search_results == 5) / sum(search_results > 3)
prob_X2 <- sum(search_results == 2) / length(search_results)
cat("Estimate of P(X=4 | X>3):", prob_X4_given_X3, "\n")
```

    ## Estimate of P(X=4 | X>3): 0.6046512

``` r
cat("Estimate of P(X=1):", prob_X1, "\n")
```

    ## Estimate of P(X=1): 0.6023

``` r
cat("Estimate of P(X=5 | X>3):", prob_X5_given_X3, "\n")
```

    ## Estimate of P(X=5 | X>3): 0.2372093

``` r
cat("Estimate of P(X=2):", prob_X2, "\n")
```

    ## Estimate of P(X=2): 0.2379

Trial two:

``` r
p <- 0.6

simulate_search <- function(p, n_searches) {
  successes <- numeric()
  for (i in 1:10000) {
    search_count <- 0
    while (TRUE) {
      search_count <- search_count + 1
      if (runif(1) <= p) break 
    }
    successes[i] <- search_count
  }
  successes
}

search_results <- simulate_search(p, 10000)

hist(search_results, breaks = 30, freq = FALSE, main = "Simulated PDF of Searches", xlab = "Number of Searches")
```

![](SA-1-ITEM3_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
mean_search <- mean(search_results)
var_search <- var(search_results)
cat("Mean of the simulated pdf:", mean_search, "\n")
```

    ## Mean of the simulated pdf: 1.6551

``` r
cat("Variance of the simulated pdf:", var_search, "\n","\n")
```

    ## Variance of the simulated pdf: 1.125857 
    ## 

``` r
search_results_conditional <- search_results[search_results > 3]

mean_conditional <- mean(search_results_conditional)
var_conditional <- var(search_results_conditional)
cat("Mean of the simulated pdf when 3 searches were unsuccesful:", mean_conditional, "\n")
```

    ## Mean of the simulated pdf when 3 searches were unsuccesful: 4.736508

``` r
cat("Variance of the simulated pdf when 3 searches were unsuccesful:", var_conditional, "\n","\n")
```

    ## Variance of the simulated pdf when 3 searches were unsuccesful: 1.21504 
    ## 

``` r
prob_X4_given_X3 <- sum(search_results == 4) / sum(search_results > 3)
prob_X1 <- sum(search_results == 1) / length(search_results)

prob_X5_given_X3 <- sum(search_results == 5) / sum(search_results > 3)
prob_X2 <- sum(search_results == 2) / length(search_results)
cat("Estimate of P(X=4 | X>3):", prob_X4_given_X3, "\n")
```

    ## Estimate of P(X=4 | X>3): 0.5761905

``` r
cat("Estimate of P(X=1):", prob_X1, "\n")
```

    ## Estimate of P(X=1): 0.6069

``` r
cat("Estimate of P(X=5 | X>3):", prob_X5_given_X3, "\n")
```

    ## Estimate of P(X=5 | X>3): 0.2380952

``` r
cat("Estimate of P(X=2):", prob_X2, "\n")
```

    ## Estimate of P(X=2): 0.2405

Trial three:

``` r
p <- 0.6

simulate_search <- function(p, n_searches) {
  successes <- numeric()
  for (i in 1:10000) {
    search_count <- 0
    while (TRUE) {
      search_count <- search_count + 1
      if (runif(1) <= p) break 
    }
    successes[i] <- search_count
  }
  successes
}

search_results <- simulate_search(p, 10000)

hist(search_results, breaks = 30, freq = FALSE, main = "Simulated PDF of Searches", xlab = "Number of Searches")
```

![](SA-1-ITEM3_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
mean_search <- mean(search_results)
var_search <- var(search_results)
cat("Mean of the simulated pdf:", mean_search, "\n")
```

    ## Mean of the simulated pdf: 1.6661

``` r
cat("Variance of the simulated pdf:", var_search, "\n","\n")
```

    ## Variance of the simulated pdf: 1.076918 
    ## 

``` r
search_results_conditional <- search_results[search_results > 3]

mean_conditional <- mean(search_results_conditional)
var_conditional <- var(search_results_conditional)
cat("Mean of the simulated pdf when 3 searches were unsuccesful:", mean_conditional, "\n")
```

    ## Mean of the simulated pdf when 3 searches were unsuccesful: 4.626817

``` r
cat("Variance of the simulated pdf when 3 searches were unsuccesful:", var_conditional, "\n","\n")
```

    ## Variance of the simulated pdf when 3 searches were unsuccesful: 1.004522 
    ## 

``` r
prob_X4_given_X3 <- sum(search_results == 4) / sum(search_results > 3)
prob_X1 <- sum(search_results == 1) / length(search_results)

prob_X5_given_X3 <- sum(search_results == 5) / sum(search_results > 3)
prob_X2 <- sum(search_results == 2) / length(search_results)
cat("Estimate of P(X=4 | X>3):", prob_X4_given_X3, "\n")
```

    ## Estimate of P(X=4 | X>3): 0.6058158

``` r
cat("Estimate of P(X=1):", prob_X1, "\n")
```

    ## Estimate of P(X=1): 0.5978

``` r
cat("Estimate of P(X=5 | X>3):", prob_X5_given_X3, "\n")
```

    ## Estimate of P(X=5 | X>3): 0.2536349

``` r
cat("Estimate of P(X=2):", prob_X2, "\n")
```

    ## Estimate of P(X=2): 0.239

Every time the code is executed, a different random number would be
generated if set.seed(123) were removed.As we can see from each trials,
we have different kinds of results which implies of not accurate
representation of data. This indicates that since random number creation
is inherently unpredictable, the simulation results would change with
each iteration if there was no set seed.

Should you repeat the simulation without initializing the seed, you
should probably see distinct sets of outcomes. Because there were so
many trials (10,000 searches), the distribution’s overall statistical
characteristics, such the mean and variance, could hold true for all
runs.

The effect of randomization increases if you run just three trials
without setting the seed. Due to the short number of trials, there would
likely be greater variation in the results across runs, which might lead
to different conclusions about how the search engine behaves. This
emphasizes how crucial it is to provide the foundation for
repeatability, particularly when working with random process
simulations.
