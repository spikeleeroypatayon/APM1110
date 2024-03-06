FA 5 MONFERO & PATAYON
================

1.  An email message can travel through one of three server routes. The
    percentage of errors in each of the servers and the percentage of
    messages that travel through each route are shown in the table.
    Assume that the servers are independent.

``` r
server <- c(1, 2, 3)
percent_messages <- c(40, 25, 35)
percent_errors <- c(1, 2, 1.5)

data <- data.frame(server, percent_messages, percent_errors)

data
```

    ##   server percent_messages percent_errors
    ## 1      1               40            1.0
    ## 2      2               25            2.0
    ## 3      3               35            1.5

using r and probability methods:

1.  what is the probability of receiving an email containing an error?
2.  what is the probability that a message will arrive without error?
3.  If a message arrives without error, what is the probability that it
    was sent through server 1?

``` r
percent_messages <- c(0.4, 0.25, 0.35)
percent_errors <- c(0.01, 0.02, 0.015)

# a. using the Law of Total Probability
p_error <- sum(percent_messages * percent_errors)

# b. probability that a message will arrive without error (using complement)
p_no_error <- 1 - p_error

# c. Using Bayes' theorem
p_no_error_given_server1 <- 1 - 0.01
p_server1 <- 0.4
p_server1_given_no_error <- p_no_error_given_server1 * p_server1 / p_no_error

cat("a. Probability of receiving an email containing an error:", p_error, "\n")
```

    ## a. Probability of receiving an email containing an error: 0.01425

``` r
cat("b. Probability that a message will arrive without error:", p_no_error, "\n")
```

    ## b. Probability that a message will arrive without error: 0.98575

``` r
cat("c. Probability that a message was sent through server 1 given that it arrived without error:", p_server1_given_no_error, "\n")
```

    ## c. Probability that a message was sent through server 1 given that it arrived without error: 0.4017246

Explanation:

let;

E = email contains error R = message travels that through the server

1.  Probability of Receiving an Email Containing an Error

using the law of total probability:

$$
P(E) = P(E∣R_1)×P(R_1)+P(E∣R_2)×P(R_2)+P(E∣R_3)×P(R_3)
$$ =(0.01×0.40)+(0.02×0.25)+(0.015×0.35) =0.004+0.005+0.00525 =0.01425

therefore,the Probability of receiving an email containing an error is
0.014.

B. the probability that a message will arrive without error

Using complement of P(E) we can solve this:

$$
P(¬E)=1−P(E)
$$ 1−0.01425 = 0.98575

therefore, Probability that a message will arrive without error is 0.98.

C. the probability that it was sent through server 1

We can use the Bayes’ theorem to solve this:

P(R1∣¬E)=P(¬E∣R_1)×P(R_1)/P(¬E)

= (1-0.01) x 0.40 / 0.98 = 0.40

therefore, the Probability that a message was sent through server 1
given that it arrived without error is 0.40 or 40%

3.  A malicious spyware can infect a computer system through the
    internet or through email. The spyware comes through the internet
    70% of the time and 30% of the time, it gets in through email. If it
    enters via the internet, the anti-virus detector will detect it with
    probability 0.6, and via email, it is detected with probability 0.8

<!-- -->

1.  what is the probability that this spyware infects the system?
2.  if the spyware is detected, what is the probability that it came
    through the internet?

``` r
P_internet <- 0.7 # spyware comes through the internet
P_email <- 0.3 # spyware comes through the email
P_antivirus_I <- 0.6 # enters via internet
P_antivirus_E <- 0.8 # enters via email

# using the Law of Total Probability
P_infection <- P_internet * P_antivirus_I + P_email * P_antivirus_E

# using Bayes' Theorem
P_I_given_D <- (P_internet *P_antivirus_I) / P_infection

cat("a. Probability that the spyware infects the system:", P_infection, "\n")
```

    ## a. Probability that the spyware infects the system: 0.66

``` r
cat("b. Probability that the spyware came through the internet given it's detected:", P_I_given_D, "\n")
```

    ## b. Probability that the spyware came through the internet given it's detected: 0.6363636

Explanation:

A. the probability that this spyware infects the system

Let; I = internet E = email AV_I = anti-virus detected that enters via
the internet AV_E = anti-virus detected that enters via the email

Using the law of total probability: $$
P(I∪E)=P(I)×P(AV_I ∣ I)+P(E)×P(AV_E∣E)
$$ = 0.7×0.6+0.3×0.8 = 0.42+0.24 = 0.66

Therefore, the probability that this spyware infects the system is 0.66
or 66%

B. if the spyware is detected, what is the probability that it came
through the internet?

using Bayes’ theorem:

$$
P(I|AV_I)= \frac{ P(I)×P(AV_I∣I)}{P(I∪E) } 
$$ = 0.7 x 0.6 / 0.66 = 0.42 / 0.66 = 0.6363…

therefore, the probability that it came through the internet if a anti
virus is detected is 0.63 or 63%
