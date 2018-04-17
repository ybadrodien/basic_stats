
# Day 3 -------------------------------------------------------------------

# 17th April 2018

# Normal Distribution - mean same as median
# Most measurements are clustered around the mean - central limit theorem
# normal distribution: the cocnept of the mean and median carries weight as distribution deviate
# T distribution is representative
# Poisson



#Running the Cullen Grey Test: Establishing the distribution of data

#loading libraries

library(fitdistrplus)
library(logspline)

# generating normally distributed data

r_norm <- rnorm(n = 1000, mean = 13, sd = 1)

# generating a histogram of the data

hist(r_norm)
descdist(r_norm, discrete = FALSE, boot = 100)

# generating uniform data

y <- runif(100)
par(mfrow = c(1,1))
plot(x = c(1:100), y = y)

# generating a histogram of the data
hist(y)
descdist(y)


# Chapter 6: T-tests ------------------------------------------------------

# loading libraries

library(tidyverse)
library(plotly)

# T-tests are used to compare the differences in the mean between 2 samples
# ANOVA is used to compare the differences in the mean between 3 or more samples


# Assumptions -------------------------------------------------------------
# Assumptions are made WRT the variance, the distribution etc of the data being used

r_dat <- data.frame(dat = c(rnorm(n = 1000, mean = 10, sd = 3),
                            rnorm(n = 1000, mean = 8, sd = 2)),
                    sample = c(rep("A", 1000), rep("B", 1000)))


# check assumptions --------------------------------------------------------

# Normality
# For this we must use the shapiro-wilk test

shapiro.test(r_dat$dat)

# But that is testing all the data together
# We must be abit more clever about how we make this test

r_dat %>% 
  group_by(sample) %>% 
  summarise(r_norm_dist =as.numeric(shapiro.test(dat)[2]))

# Remember the data are normal when p > 0.05
# The data are non-normal when p >= 0.05


# Check homoscedastisity --------------------------------------------------

# There are many ways to check for homoscedastisity
# Which is the similiarity in variance between sample sets
# For now we will simply say that the assumptions are met when
# the variance of the sample are not more than 3-4 times greater
# Than one another

# Check everything at once
# WRONG
var(r_dat)

# Correct way - separate the data
r_dat %>% 
  group_by(sample) %>% 
  summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2]),
            r_norm_var = var(dat))
# this will produce a tibble
# the first column shows normality of the distribution
# second column shows the normality of the variance
# compare the top and bottom values from each column to establish homscedastisity

# t-test cannot test categorical data
# it has to be conducted using continuous data


# One sampled t-Test ------------------------------------------------------

r_one <- data.frame(dat = rnorm(n = 20, mean = 20, sd = 5),
                    sample = "A")
# Perhaps a visualisation?
# Running the test
# use the t.test function
# specify the data within the dataset you want to use using $ function
# include the population mean = mu

t.test(r_one$dat, mu = 20)

# Run a test we know will have a significant result 

t.test(r_one$dat, mu = 30)


# Pick a side -------------------------------------------------------------

# are these data smaller/ less than the population mean
t.test(r_one$dat, mu = 20, alternative = "less")

# Or greater

t.test(r_one$dat, mu = 20, alternative = "greater")

# but what about for a larger population mean?

# are the samples less than the population of 30?

t.test(r_one$dat, mu = 30, alternative = "less")

# what about samples greater than the population of 30?

t.test(r_one$dat, mu = 30, alternative = "greater")


# Two-Sampled t-Tests -----------------------------------------------------

# subtract the mean of one sample from the mean of another sample and / by grouped SD

# create a new dataframe

r_two <- data.frame(dat = c(rnorm(n = 20, mean = 4, sd = 1),
                            rnorm(n = 20, mean = 5, sd = 1)),
                    sample = c(rep("A", 20), rep("B", 20)))

# run a default/basic
# indicate the column = dat, the group = sample, and indicate the df = r_two
# we should check the homoscedastisity of the data before we conduct such a test
# if the assumption is that the variances are similar then var = TRUE
# if the assumption is that the variances are different then var = FALSE
# NB the DF = N-2 for a 2 sample t-Test

t.test(dat ~ sample, data = r_two, var.equal = TRUE)

# pick a side: One sided 2 sample t-Test
# is "A" less than "B"
# add the alternative command to the end of the code: "less", "greater"
# depending on the application

t.test(dat ~ sample, data = r_two, var.equal = TRUE, alternative = "less")

# Exercises

# Loading the data

ecklonia <- read_csv("ecklonia.csv") %>% 
  gather(key = "variable", value = "value", -species, -site, -ID)


# Visualising the data ----------------------------------------------------

ggplot(data = ecklonia, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip()


# Hypothesis development --------------------------------------------------

# Having visualised the data, we observe that primary blade length is similar
# For samples collected at Batsata Rock vs Boulders Beach
# A hypotheses can be devised regarding this

# Filtering based on the variable of interest

ecklonia_one <- ecklonia %>% 
  filter(variable == "primary_blade_width")

# Visualising the filtered data: Primary blade length ---------------------

ggplot(data = ecklonia_one, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "Primary Blade Length (cm)", x = "") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# Developing the Hypothesis
 
# H0: The primary blade length of kelp at Bastata Beach is not greater than Boulders Beach
# H1: The primary blade length of kelp at BAstata Beach is greater than Boulders Beach


# CHoosing a test: Evaluating the data for homoscedastisity -------

ecklonia_one %>% 
  group_by(site) %>% 
  summarise(prim_blade_len_norm = as.numeric(shapiro.tesT(ecklonia_one$site)[2],
            prim_blade_len_norm = two_assum(shapiro.test(ecklonia_one)[2])
            
# TRIED USING THE CODE IN THE MANUAL TO PRODUCE TIBBLE FOR NORMALITY: DIS VS VAR
# FIRSTLY IT DIDNT RECOGNIZE THE TWO_ASSUM COMMMAND
# I THEN TRIED TO REWRITE THE CODE BASED ON WHAT WE DID IN CLASS
# NOT UNDERSTANDING WHAT THE NUMBER FOLLOWING THE CODE VIZ. [1] OR [2]
# ALSO IN THE DAT PART OF THE CODE I USED THE $ TO DENOTE WHAT COLUMN NEEDS TO BE USED

  