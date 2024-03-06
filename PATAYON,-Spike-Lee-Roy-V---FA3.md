FA3 in R language
================
PATAYON, SPIKE LEE-ROY V
2024-02-19

1.  A binary communication channel carries data as one of two sets of
    signals denoted by 0 and 1. Owing to noise, a transmitted 0 is
    sometimes received as a 1, and a transmitted 1 is sometimes received
    as a 0. For a given channel, it can be assumed that a transmitted 0
    is correctly received with probability 0.95, and a transmitted 1 is
    correctly received with probability 0.75. Also, 70% of all messages
    are transmitted as a 0. If a signal is sent, determine the
    probability that:

<!-- -->

1)  a 1 was received;
2)  a 1 was transmitted given than a 1 was received.

``` r
P_1_received_given_1_transmitted <- 0.75
P_0_received_given_0_transmitted <- 1 - 0.95
P_1_transmitted <- 0.30
P_0_transmitted <- 1 - P_1_transmitted

P_1_received <- P_1_received_given_1_transmitted * P_1_transmitted +
                P_0_received_given_0_transmitted * P_0_transmitted

P_1_transmitted_given_1_received <- (P_1_received_given_1_transmitted * P_1_transmitted) / P_1_received

cat("Probability that 1 was received:",P_1_received,"\n")
```

    ## Probability that 1 was received: 0.26

``` r
cat("Probability that 1 was transmitted given than a 1 was received:", P_1_transmitted_given_1_received,"\n")
```

    ## Probability that 1 was transmitted given than a 1 was received: 0.8653846

2.  There are three employees working at an IT company: Jane, Amy, and
    Ava, doing 10%, 30%, and 60% of the programming, respectively. 8% of
    Jane’s work, 5% of Amy’s work, and just 1% of Ava‘s work is in
    error. What is the overall percentage of error? If a program is
    found with an error, who is the most likely person to have written
    it?

``` r
percentages <- c(0.10, 0.30, 0.60)

error <- c(0.08, 0.05, 0.01)

total_work <- percentages * 100

total_errors <- error * total_work

overall_error_percentage <- sum(total_errors) / sum(total_work) * 100

most_likely_person <- which.max(total_errors)

cat("Overall percentage of error:", overall_error_percentage, "%\n")
```

    ## Overall percentage of error: 2.9 %

``` r
cat("Most likely person to have written a program with an error:", c("Jane", "Amy", "Ava")[most_likely_person], "\n")
```

    ## Most likely person to have written a program with an error: Amy

To better visualize this:

``` r
library(ggplot2)

df <- data.frame(Employee = c("Jane", "Amy", "Ava"),
                 Total_Work = total_work,
                 Total_Errors = total_errors)

ggplot(df, aes(x = Employee, y = Total_Errors, fill = Employee)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Errors by Employee",
       x = "Employee",
       y = "Total Errors") +
  theme_minimal()
```

![](PATAYON,-Spike-Lee-Roy-V---FA3_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->
