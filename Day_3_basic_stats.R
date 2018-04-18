
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
library(ggpubr)

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
  summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2]))

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
var(r_dat$dat)

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
 
# H0: The primary blade length of kelp at Batsata Beach is not greater than Boulders Beach
# H1: The primary blade length of kelp at Batsata Beach is greater than Boulders Beach


# Choosing a test: Evaluating the data for assumptions --------------------

ecklonia_one %>% 
  group_by(site) %>% 
  summarise(prim_blade_len_norm = as.numeric(shapiro.test(value)[2]),
            prim_blade_len_var = var(value))
            
# TRIED USING THE CODE IN THE MANUAL TO PRODUCE TIBBLE FOR NORMALITY: DIS VS VAR
  # Wonderful :)
# FIRSTLY IT DIDNT RECOGNIZE THE TWO_ASSUM COMMMAND
  # It didn't recognise this command because it is a function that we are making "by hand"
  # In order for R to recognise it we must first run the code on our computer
  # THis then creates the new function that we may use in our pipes etc.
# I THEN TRIED TO REWRITE THE CODE BASED ON WHAT WE DID IN CLASS
  # Very good
# NOT UNDERSTANDING WHAT THE NUMBER FOLLOWING THE CODE VIZ. [1] OR [2]
  # Square brackets are used to 'subset' data
  # In this case, [1] takes the first value in a vector
  # Therefore, [2] takes the second value
  # shapiro.test() spits out two values
  # Every value we want to create with summarise() may only be one value long
  # So because shapiro.test() produces two values we have to pick only one
# ALSO IN THE DAT PART OF THE CODE I USED THE $ TO DENOTE WHAT COLUMN NEEDS TO BE USED
  # That's fine


# Running the analysis ----------------------------------------------------

t.test(value ~ site, data = ecklonia_one, var.equal = TRUE, alternative = "greater")  

# DF output
compare_means(value ~ site, data = ecklonia_one, var.equal = TRUE, alternative = "greater")


# Interpreting results and drawing conclusions ----------------------------

# t-Test results:

# t = 0.71902, df = 24, p-value = 0.2395 

# P > 0.05, therefore no significant difference

# The primary blade length (cm) of the kelp Ecklonia maxima was found to be significantly greater at Batsata Rock than at Boulders Beach (p = 0.2395, t = 0.71902, df = 24)


# Exercise 6.7.1 ----------------------------------------------------------

# load data

stud <- read_csv("stud.csv")

# Visualizing data --------------------------------------------------------

ggplot(data = stud, aes(x = sex, y = height)) +
  geom_boxplot(aes(fill = sex))

# Formulating a hypothesis
# given the fact that the data is already cleaned and filtered, there is no need to filter

# The hypothesis: Randomly made data

# H0: Female height is less than male height.
# H1: Female height is greater than male height.

# Checking assumptions: Comparing the variance male height vs. female height

stud %>% 
  group_by(sex) %>% 
  summarise(height_norm = as.numeric(shapiro.test(height)[2]),
            height_var = var(height))


# Running the analysis ----------------------------------------------------

t.test(height ~ sex, data = stud, var.equal = TRUE, alternative = "greater")


# Conclusion --------------------------------------------------------------

Female height is less than male height (p = 1, t = -5.1079, df = 28)


