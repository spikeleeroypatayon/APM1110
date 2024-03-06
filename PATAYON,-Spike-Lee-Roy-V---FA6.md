FA 6 IN R LANGUAGE
================
PATAYON, SPIKE LEE-ROY V
2024-02-23

I. Geometric Distribution. Provide an R code for the geometric
distribution. The geometric distribution is a probability distribution
that models the number of trials required to achieve the first success
in a sequence of Bernoulli trials, where each trial has a constant
probability of success.

1.  Set the probability of success: p \<- 0.2

2.  Generate 1000 random variables from the geometric distribution.

3.  Calculate some basic statistics:

mean_x \<- mean(x)

var_x \<- var(x)

sd_x \<- sd(x)

4.  Print the results in item 3 with the following output (string):

Number of trials required to achieve first success:

Mean (in 2 decimal places):

Variance (in 2 decimal places):

Sandard deviation ( in 2 decimal places):

5.  Plot the histogram of the results.

``` r
p <- 0.2
expected_trials <- 1 / p

x <- rgeom(1000, prob = p)

mean_x <- mean(x)
var_x <- var(x)
sd_x <- sd(x)

print("Raw data of mean, variance, and standard deviation of x:")
```

    ## [1] "Raw data of mean, variance, and standard deviation of x:"

``` r
cat("Mean of x:",mean_x,"\n")
```

    ## Mean of x: 4.179

``` r
cat("Variance of x:",var_x,"\n")
```

    ## Variance of x: 22.60156

``` r
cat("Standard Deviation of x:",sd_x,"\n")
```

    ## Standard Deviation of x: 4.75411

``` r
mean_x = round(mean_x, digits = 2)
var_x = round(var_x, digits = 2)
sd_x = round(sd_x, digits = 2)

print("-------------------------------")
```

    ## [1] "-------------------------------"

``` r
cat("Number of trials required to achieve first success: ", expected_trials, "\n")
```

    ## Number of trials required to achieve first success:  5

``` r
cat("Mean (in 2 decimal places):", mean_x, "\n")
```

    ## Mean (in 2 decimal places): 4.18

``` r
cat("Variance (in 2 decimal places):", var_x, "\n")
```

    ## Variance (in 2 decimal places): 22.6

``` r
cat("Standard deviation (in 2 decimal places):",sd_x, "\n")
```

    ## Standard deviation (in 2 decimal places): 4.75

5.  Plot the histogram of the results.

``` r
hist(x, breaks = 20, main = "Histogram of Geometric Distribution", xlab = "Number of Trials to First Success", ylab = "Frequency")
```

![](PATAYON,-Spike-Lee-Roy-V---FA6_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

2.  Hypergeometric Distribution. Consider a plant manufacturing IC chips
    of which 10% are expected to be defective. The chips are packed in
    boxes for export. Before transportation, a sample is drawn from each
    box. Estimate the probability that the sample contains more than 10%
    defectives, when:

A sample of 10 is selected from a box of 40; A sample of 10 is selected
from a box of 5000.

``` r
m <- 0.10 * 40  
n <- 10        
k <- 40       

prob_10_40 <- 1 - phyper(0.10 * n - 1, m, k - m, n)

m <- 0.10 * 5000  
n <- 10            
k <- 5000      

prob_10_5000 <- 1 - phyper(0.10 * n - 1, m, k - m, n)

prob_10_40 = round(prob_10_40, digits = 4)
prob_10_5000 = round(prob_10_5000, digits = 2)

cat("A sample of 10 is selected from a box of 40: ", prob_10_40, "\n")
```

    ## A sample of 10 is selected from a box of 40:  0.7001

``` r
cat("A sample of 10 is selected from a box of 5000: ",prob_10_5000, "\n")
```

    ## A sample of 10 is selected from a box of 5000:  0.65
