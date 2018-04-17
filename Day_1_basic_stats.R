
# Yameen Badrodien --------------------------------------------------------
# 12 April 2018
# Day 1_basic_stats.R
# Day 1 stats class
# Purpose: Practice some of the concepts we will encounter

#load library
library(tidyverse)


# Integers: discrete data ----------------------------------------------------------------
# how to generate a sequence of integers
# "by" represents the incriments 
# "seq" refers to the sequence of values which will be created
integer_r <- as.integer(seq(5, 14, by = 1))
# run the object in the console to view the string of data


# Continuous data ---------------------------------------------------------

numeric_r <- seq(23,43, length.out = 10)
# check manual for which graphical repr suited to the type of data


# Dates -------------------------------------------------------------------

dates_r <- as.Date("2005-12-31") - as.Date("2005-12-12")
# The function above allows us to tell the difference in days between the given dates
# the as.date function informs the software that the values provided are dates
# the way that date is written is specific to countries
# United states- month, day, year
# Other countries - year, month, day - unambiguous
dates_r <- seq(as.Date("2005-12-16"), as.Date("2005-12-25"), by = "day")
summary(dates_r)
# the above command will summarise the dates_r dataframe


# Tibble/Dataframe --------------------------------------------------------
# create the base dataframe and assign to object
df_r <- data.frame(integers = integer_r,
                  numeric = numeric_r,
                  dates = dates_r)
#Upgrade to tibble
df_r <- as.tibble(df_r)       #assigns to object
summary(df_r)                 #populates table of the objects data


# Qualitative data ------------------------------------------------------
#electronics 
elec_r <- as.factor(c("laptops",
                      "desktops",
                      "cell phones"))

#People
people_r <- as.factor(c("funny",
                      "beautiful",
                      "beanies"))

#Colours
colour_r <- as.factor(c("red", "blue"))

#the as.factor encodes a vector (object) as a factor


# Ordinal Data ------------------------------------------------------------

# Here we still have qualitative data
# but with some sort of order
# ordered function is a logical flag to determine if the levels should be regarded as ordered( in the order given)
# the levels argument specifies how the various groups differ- the ranking

colour_qual <- ordered(c("blue","green",
                             "yellow", "orange",
                             "red"),
                           levels = c("blue","green",
                                      "yellow", "orange",
                                      "red"))

# Binary ------------------------------------------------------------------

# These are generally represented as either TRUE or FALSE
binary_r <- c(TRUE,FALSE,TRUE,FALSE)
summary(binary_r)


# Characters --------------------------------------------------------------

sites_r <- c("Yzervarkpunt", "Betty's Bay",
             "Gansbaai", "Sea Point")

# Missing values ----------------------------------------------------------
# special type of data which is denoted by N.A meaning it is not available
#it isnt applicable where a value is zero, but rather where data for a sample is unavailable

chicks_nest <- c(3, 2, 0, 10, 5, 6, 8, 2, 4, NA)
#summary
summary(chicks_nest)

#mean
mean(chicks_nest)

#standard deviation
sd(chicks_nest)


# Viewing the data --------------------------------------------------------
# this may be achieved in a number of ways

#type the name of the dataframe and run it
ChickWeight

#alternatively the dataset may be viewed as a summary
summary(ChickWeight)

#to view either the top or bottom of the datset use head or tail commands
# a comma after these commands indicates how many values from top or bottom

#head shows the top of the data set
head(ChickWeight, 15)

#tail shows the bottom of the dataset
tail(ChickWeight, 15)



# Descriptive Stats -------------------------------------------------------

#creating a dataframe

chicks <- as_tibble(ChickWeight)

#count the data
chicks %>% 
  summarise(chicken_count = n())

#alternatively
nrow(chicks)


# Measures of central tendency --------------------------------------------

#Calculate the mean weight
chicks %>% 
  summarise( mean_wt = mean(weight))

#we need to be more specific
chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mean_wt = mean(weight))

#the mean becomes less accurate with a higher number of outliers
#we can use other measures of central tendency eg. mode, median
chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mean_wt = mean(weight),
          median_wt = median(weight))
central_chicks <- chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mean_wt = mean(weight),
            median_wt = median(weight))

#Visualising the density of the data
ggplot(data = filter(chicks, Time == 21 ),
       aes(x= weight, fill = Diet))+
  geom_density(alpha = 0.4)

# Skewness ----------------------------------------------------------------

#data that are normally distributed should not be skewed i.e the mean and the median should be similar
#WRT chickweight
# if it lies to the left, it is right skewed
#diet 1 = right skewed
#diet 2 = right skewed
#diet 3 = left skewed
#diet 4 = normally distributed

#calculate the numeric value
#load library for skewness
library(e1071)

chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mean_wt = mean(weight),
            median_wt = median(weight),
            skew_wt = skewness(weight))

ggplot(data = filter(chicks, Time == 21),
       aes(x = weight, fill = Diet))+
  geom_density(alpha = 0.4)+
  geom_vline(data = central_chicks, aes(xintercept = mean_wt, colour = Diet)) +
  geom_vline(data = central_chicks, aes(xintercept = median_wt, colour = Diet),
             linetype = "dashed")
          

# Kurtosis ----------------------------------------------------------------

#calculate the kurtosis of the tails of the distribution 
chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mean_wt = mean(weight),
            median_wt = median(weight),
            skew_wt = skewness(weight),
            kurt_wt = kurtosis(weight))


# Variance ----------------------------------------------------------------
#std dev is the square root of the variance- its just weight
# in write ups we generally give the mean +/- the std deviation

#measures of variability

#below is a summary of many different stats properties
wt_summary <- chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(wt_mean = mean(weight),
            wt_median = median(weight),
            wt_var = var(weight),
            wt_sd = sd(weight),
            wt_min = min(weight),
            wt_quartl = quantile(weight, 0.25),
            wt_quart2 = quantile(weight, 0.5),
            wt_quart3 = quantile(weight, 0.75))
    

#run for tibble of weight summary 
wt_summary            



