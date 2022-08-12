Salmon code challenge
================

# Introduction

For an average weight, we assume normal distributed size of the fish
around this mean and the standard deviations given in Table 1 in the
attached Excel file. For each month in a year, we have the number of
individuals and the total biomass in a cage (Table 2).

1.  How would you go about calculating the total harvestable biomass
    (fish larger than 4kg) for these months?

2.  What is the average weight of these (harvestable) fish?

3.  Assuming you only know the biomass and number of individuals at the
    start of the first month (from table 2). Assume a growth rate of
    11,2%. How much will be harvested during the next 12 months, if we
    assume that all fish over 4kg will be harvested at the end of each
    month?

4.  Do you see any other interesting questions we could try to answer
    with these or similar kind of data?

# Load data

Here we load the library that are used for the analysis

``` r
library(data.table)
library(readxl)
```

Let’s load the dataset from the excel file.

-   The first table (**Table 1**) is stored in `weight` object
-   The second table (**Table 2**) is the `time_series` object

``` r
data_path = "C:/Users/nicol/Downloads/Data Scientist assigment - Backround data.xlsx"
weight = data.table(read_xlsx(data_path,sheet = 1))
time_series = data.table(read_xlsx(data_path,sheet = 2,skip = 1,
                                   col_names = c("Month","Number of individuals",
                                                 "Biomass")))

# head(weight)
# time_series
```

# Analysis

## Question 1

In order to answer to the first question we need to know the
distribution of the weight for the overall population of fish. We know
for each month the number of fish and the total biomass in a cage. We
assume that the distribution of the population follows a *Poisson
distribution*: we picked this distribution because we need a discrete
probability distribution, skewed and non-negative. We have to define the
*λ* parameter of the distribution according to the data, each month will
have a different distribution with a different *λ* parameter.

Based on the data we have *61* categories of weight and each category is
normally distributed with mean and standard deviation given as input.
The following function is the objective function that will be optimized
(finding the optimum *λ*) according to the data. The function work as
follow:

-   Fixed a seed to have reproducibility (`set.seed(90)`)
-   Use the poisson distribution to define the number of fish per
    category of weight (here the parameter *λ* is the goal of the
    optimization that will give us the proportion of fish for each
    category of weight)
-   Assign one of the category to each simulated fish (according to the
    population proportion)
-   

``` r
opt_function = function(weight,par,n_pop,biomass){
  set.seed(90)
  weight[, prop_p := dpois(0:60,par)]
  tmp = sample(61,n_pop, replace = T,prob = weight$prop_p)
  pop_group = rep(0,61)
  pop_group[as.numeric(names(table(tmp)))] = table(tmp)
  weight[, pop_p := pop_group]
  return(abs(round(sum(gen_weight(dt = weight)),0) - biomass))
}
```
