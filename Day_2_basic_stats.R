# Yameen Badrodien --------------------------------------------------------
# 13 April 2018
# Day 2_basic_stats.R
# Day 2 stats class
# Purpose: Data visualizations and distribution


# load libraries ----------------------------------------------------------

library(tidyverse)


# Manual calculations -----------------------------------------------------

<<<<<<< HEAD
# the mean
# rnorm is used to generate a dataset of random normally distributed numbers
# rpois is used to produced a dataset of random poisson distributed numbers
# rbinom is used to produce a dataset of random binomialy distributed numbers
=======
#the mean
#rnorm is used to generate a dataset of random normally distributed numbers
#rpois is used to produced a dataset of random poisson distributed numbers
#rbinom is used to produce a dataset of random binomialy distributed numbers
>>>>>>> e5402a161e06f2dfcd79ad11252fc30098dcfecb

r_dat <- data.frame(dat = rnorm(n = 600, mean = 372, sd = 50),
                    sample = "A")
                    


<<<<<<< HEAD
# quick visualisation

=======
#quick visualisation
>>>>>>> e5402a161e06f2dfcd79ad11252fc30098dcfecb
ggplot(data = r_dat, aes(x = dat)) +
  geom_density()


# the mean ----------------------------------------------------------------


<<<<<<< HEAD
# what is the mean?
# sum of points 
# divided by 
# the number of points
=======
#what is the mean?
#sum of points 
#divided by 
#the number of points
>>>>>>> e5402a161e06f2dfcd79ad11252fc30098dcfecb

r_dat %>% 
  summarise(r_sum = sum(dat),
            r_n = n(),
            r_mean = r_sum/r_n,
            r_mean_func = mean(dat))
<<<<<<< HEAD
# refrain from using actaul numbers in these types of calculations
# use the data, which even if updated will maintain the integrity of the code
# we can double check the mean which was manually calculated
# using the r_mean_func, pictured in the last line of code
=======
#refrain from using actaul numbers in these types of calculations
#use the data, which even if updated will maintain the integrity of the code
#we can double check the mean which was manually calculated
#using the r_mean_func, pictured in the last line of code
>>>>>>> e5402a161e06f2dfcd79ad11252fc30098dcfecb



# the median --------------------------------------------------------------
<<<<<<< HEAD
# find what the middle value is by using subsetting
# this can be achieved using brute force using basic R

r_dat$dat[(length(r_dat$dat)+1)/2]

# or manually using tidy
=======
#find what the middle value is by using subsetting
#this can be achieved using brute force using basic R

r_dat$dat[(length(r_dat$dat)+1)/2]

#or manually using tidy
>>>>>>> e5402a161e06f2dfcd79ad11252fc30098dcfecb

r_dat %>% 
  arrange(dat) %>% 
  slice(n()/2)

# it can also be achieved using the tidymagic method
r_dat %>% 
  summarise(r_median = median(dat))


# variance ----------------------------------------------------------------

# the sum of 
  # each value minus the mean
    # squared
# divided by 
  # the count of samples minus one

r_dat %>%
  mutate(r_error = dat-mean(dat),
         r_error_square = r_error * r_error) %>% 
  summarise(r_squared_sum = sum(r_error_square),
            r_var = r_squared_sum/ (n()-1),
            r_var_func = var(dat))

# std deviation -----------------------------------------------------------

r_dat %>% 
  summarise(r_var = var(dat),
            r_sd = sqrt(r_var),
            r_sd_func = sd(dat))


# exercise ----------------------------------------------------------------
summary(ChickWeight$weight)
# refer to the outputs of summary

#obtain the outputs of the summary using tidy code
ChickWeight %>% 
  summarise(min_weight = min(weight),
            quart_1 = quantile(weight, 0.25),
            med_weight = median(weight),
            mn_weight = mean(weight),
            quart_3 = quantile(weight, 0.75),
            max_weight = max(weight))


# Visualisations ----------------------------------------------------------

# load libraries
library(viridis)
library(ggpubr)
library(RColorBrewer)
library(tidyverse)


# qualitative data --------------------------------------------------------

# load our SA time and replacing the raw data in the global environment
# except with the column which we have added
sa_time <- read_csv("sa_time.csv")

# adding the human column
sa_time <- sa_time %>% 
<<<<<<< HEAD
  mutate(human = seq(1, n(),1),
         geo = c(rep(c("Cape Town", "George", "PE"), times = 6),
                     rep("Joburg", 2)))
=======
  mutate(human = seq(1, n(),1))
>>>>>>> e5402a161e06f2dfcd79ad11252fc30098dcfecb

# edit our data
sa_long <- sa_time %>% 
  gather(key = "time_type", value ="minutes", -human)
  


# create a count of qualitative values

# stacked bar graphs

sa_count <- sa_long %>% 
  count(time_type) %>% 
  mutate(prop = n/sum(n))
  
ggplot(data = sa_count, aes(x = "", y = n, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +
  labs(title = "Stacked bar graph", subtitle = "cumulative sum",
       x = NULL, y = "Count") +
  theme_minimal()

# stacked proportion bar graph

ggplot(data = sa_count, aes(x = "", y = prop, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +
  scale_y_continuous(breaks = c(0.00, 0.33, 0.66, 1.00)) +
  labs(title = "Stacked bar graph", subtitle = "relative proportions",
       x = NULL, y = "Proportion") +
  theme_minimal()

# pie chart NB refrain from using this type of graph for representations

<<<<<<< HEAD
ggplot(data = sa_count, aes(x = "", y = prop, fill = time_type)) +
=======
(data = sa_count, aes(x = "", y = prop, fill = time_type)) +
>>>>>>> e5402a161e06f2dfcd79ad11252fc30098dcfecb
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  labs(title = "Friends don't let...", subtitle = "...friends make pie charts",
       x = NULL, y = NULL) +
  scale_fill_brewer(palette = "blues") +
  theme_minimal()


# continuous data ---------------------------------------------------------

# histograms

# faceted histogram

# the code below will lump all of the data into a single bin

ggplot(data = sa_long, aes(x = minutes))+
  geom_histogram()

# to avoid this from happening, we clean the data

sa_clean <- sa_long %>% 
<<<<<<< HEAD
  filter(minutes < 300)
=======
  filter(minutes < 10000)
>>>>>>> e5402a161e06f2dfcd79ad11252fc30098dcfecb

# run again with cleaned data

ggplot(data = sa_clean, aes(x = minutes))+
  geom_histogram()

ggplot(data = sa_clean, aes(x = minutes)) +
  geom_histogram(aes(fill = time_type),
                 position = "dodge") +
  facet_wrap(~time_type, ncol = 1, scales = "free_x")

# relative proportion histogram

ggplot(data = sa_clean, aes (x = minutes)) +
  geom_histogram(aes( y = ..density.., fill = time_type),
                 position = "dodge", binwidth = 1) +
  facet_wrap(~time_type, ncol = 1, scales = "free_x")

# Boxplots

ggplot(data = sa_clean, aes(x = time_type, y = minutes)) +
  geom_boxplot(aes(fill=time_type))

# the thick line in the middle of the plot is the median
# the  coloured box represents the interquartile range
# the whiskers of the boxplot express the variation of the groups
# longer whiskers indicate greater variation ~ no outliers
# shorter whiskers indicate less variation ~ therefore outliers are conclusive
# box plots can indicate whether or not a sig diff exists between groups
# simply see if the boxes overlap in relation to one another

# Notched Boxplots
ggplot(data = sa_clean, aes(x = time_type, y = minutes)) +
<<<<<<< HEAD
  geom_boxplot(aes(fill = time_type), notch =TRUE)

# calculate summary stats for plotting over the boxplots
sa_summary_stats <- sa_clean %>% 
  group_by(time_type) %>% 
  summarise(time_type_mean = mean(minutes))

ggplot(data = sa_clean, aes(x = time_type, y = minutes)) +
  geom_boxplot(aes(fill = time_type), notch = TRUE) +
  geom_point(data = sa_summary_stats, size = 6, shape = 18,
             aes(y = time_type_mean), colour = "goldenrod")

#relationships

# a basic scatterplot

ggplot(data = sa_time, aes(y = now_now, x = just_now)) +
         geom_point() +
         coord_equal(xlim = c(0, 60), ylim = c(0, 60))

# adding trendlines ~ geom_smooth function 

ggplot(data = sa_time, aes(y = now_now, x = just_now)) +
  geom_point(aes(colour = geo)) +
  geom_smooth(aes(colour = geo), method = "lm") +
  coord_equal(xlim = c(0, 60), ylim = c(0, 60))

#grey areas denoted in the graph = standard error around the mean
# the standard error can be removed by setting SE= FALSE
=======
  geom_boxplot(aes(fill=time_type), notch =TRUE)
>>>>>>> e5402a161e06f2dfcd79ad11252fc30098dcfecb
