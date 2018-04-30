# Day 6
# Yameen Badrodien
# 26 April 2018
# Confidence intervals and data transformation

# Confidence interval is the range of estimates of the mean, that would represent
# 95% of the means that have been derived from multiple samples

# There a number of R packages which are available for confence interval analysis
# package <-  Rcompanion

library(rcompanion)

#loading the df
Input <- ("
Student  Sex     Teacher  Steps  Rating
          a        female  Jacob    8000   7
          b        female  Jacob    9000  10
          c        female  Jacob   10000   9
          d        female  Jacob    7000   5
          e        female  Jacob    6000   4
          f        female  Jacob    8000   8
          g        male    Jacob    7000   6
          h        male    Jacob    5000   5
          i        male    Jacob    9000  10
          j        male    Jacob    7000   8
          k        female  Sadam    8000   7
          l        female  Sadam    9000   8
          m        female  Sadam    9000   8
          n        female  Sadam    8000   9
          o        male    Sadam    6000   5
          p        male    Sadam    8000   9
          q        male    Sadam    7000   6
          r        female  Donald   10000  10
          s        female  Donald    9000  10
          t        female  Donald    8000   8
          u        female  Donald    8000   7
          v        female  Donald    6000   7
          w        male    Donald    6000   8
          x        male    Donald    8000  10
          y        male    Donald    7000   7
          z        male    Donald    7000   7
          ")

data <- read.table(textConnection(Input),header = TRUE)
summary(data)


# one way
groupwiseMean(Steps ~ 1,data = data, conf = 0.95, digits = 3)

# two way
groupwiseMean(Steps ~ Teacher + Sex, data = data, conf = 0.95,digits = 3)

# assign the summary to an object

dat1 <- groupwiseMean(Steps ~ Teacher + Sex, data = data, conf = 0.95,digits = 3)

# plotting a graph with the mean and the conf intervals

ggplot(data = dat1, aes(y = Mean, x = Sex)) +
  geom_point(aes(colour = Teacher)) +
  geom_errorbar(aes(ymin = Mean-(Trad.lower), ymax = Mean +(Trad.upper), colour = Teacher))+
  facet_wrap(~Teacher)



# bootsrapping takes multiple samples from a pile of ddata
# in this case it takes the mean, 10 000 repeat samples
# based on repeat samples it takes confidence intervals around the means

groupwiseMean(Steps ~ Sex,
              data = data,
              conf = 0.95,
              digits = 3,
              R = 10000,
              boot = TRUE,
              traditional = FALSE,
              normal = FALSE,
              basic = FALSE,
              percentile = FALSE,
              bca = TRUE)


# Testing assumptions or: How I learned to stop worrying and test data  --------


# Assumptions: ------------------------------------------------------------
# These need to be met before deciding which stat analsysis is selected for applic.

# Data is normally distributed
# Data is homoscedastic
# Data is continuous
# Data  must be independant of one another

# Continuous data :we cant really establish if data is continuous in nature
# it will be revealed as the analysis is conducted


# How to check if the data meets the assumptions --------------------------

# perform anoy of the diagnostic plots we covered in the earlier chapters
# compare the variances to see if they differ by a factor of 4
# do a levenes's test to test for equal variances
# do a shapiro-wilk test to for normality


# Checking for normality --------------------------------------------------

chicks <-(ChickWeight)
shapiro.test(chicks$weight)

# if the p-value is less than 0.05 then the data is considered non-normal

# in the case of the above data: Non-normal

# this data could be analysed using either the Wilcox (one sample)
# or Mann Whitney (2 sample)


# Assumptions for anova ---------------------------------------------------

# read corresponding section in 



# Transforming data -------------------------------------------------------

library(dplyr)
library(tidyverse)
library(ggpubr)

# calculating and assigning transformations to individual columns

logdat <- data %>% 
  mutate(log10 = log10(Steps)) %>% 
  mutate(log = log(Steps)) %>% 
  mutate(cube.rt = (Steps^1/3)) %>% 
  mutate(sq.rt = sqrt(Steps))
  
#Plotting various transformations as histograms and assigning to indep vectors

hist1 <- ggplot(data = logdat, aes(x = log10, fill = Teacher )) + 
  geom_histogram(aes(fill = Teacher), position = "dodge")

hist2 <- ggplot(data = logdat, aes(x = log, fill = Teacher)) +
  geom_histogram(aes(fill = Teacher), position = "dodge")

hist3 <-  ggplot(data = logdat, aes(x = cube.rt, fill = Teacher)) + 
  geom_histogram(aes(fill = Teacher), position = "dodge")

hist4 <- ggplot(data = logdat, aes(x = sq.rt, fill = Teacher))+
  geom_histogram(aes(fill = Teacher), position = "dodge")

# arranging the histograms into a multipanelled graph and assigning to vector

fin.hist <- ggarrange(hist1, hist2, hist3, hist4)

# running the final plot

fin.hist

# Smitties way

logdat1 <- data %>% 
  mutate(log10 = log10(Steps)) %>% 
  mutate(log = log(Steps)) %>% 
  mutate(cube.rt = (Steps^1/3)) %>% 
  mutate(sq.rt = sqrt(Steps)) %>% 
  select(- Student, - Rating) %>% 
  gather(key = "data.type", value = "trans.data",- Sex, -Teacher)

#select is to select certain columns which are then to be excluded
# using the - symbol
# the various transformation data types are then assembled into a sinbgle column
# using the gather function using the data.type as the key, and placing the values
# in a single column ~ value as trans.data

# plotting the data as histograms

hist <- ggplot(data = logdat, aes(x = trans.data))+
  geom_histogram(fill = data.type)+
  facet_grid(Sex ~ Teacher)

# running the final plot

hist

# Revision: ---------------------------------------------------------------

# How to do an anova ~ checking assumptions to running the analysis

iris.dat <-  as.tibble(iris)

# View the data: see what can be analysed

# Develop the hypothesis
# h0: there is no signoficant difference in petal width between three iris species
# H1: There is a significnat difference in petal width between three iris species

# checking the assumptions

# normality
shapiro.test(iris$Petal.Width)

# This tells us that the data is non-normal

# running a comprehensive test regarding the normality of a dataset
iris %>% 
  group_by(Species) %>% 
  summarise(norm_dat = as.numeric(shapiro.test(Petal.Width)[2]))

# This gives a table displaying the normality of the distribution

# notice that the data for virginica is greater than 0.05 therefore data non norma
# refer to the table in the tranforming data chapter
# the data should be analysed using a anova - but because its non normal
# we use the Kruskal test

kruskal.test(Petal.Width~Species, data = iris)
# the p value derived from this test, informs us that:
# p is less than 0.05 therefore:
# we fail to accept the null hypothesis, and we accept the alternate hypothesis
# therefore, There is a signifant diff between the petal width of 3 iris species

