# Day 5: Basic Stats
# Yameen Badrodien 
# 20 April 2018


# Exercises: Snakes

# loading the dataset

snakes <- read_csv("snakes.csv.txt") %>% 
  mutate(day = as.factor(day))

# summarise the data

snakes_summary <-  snakes %>% 
  group_by(day) %>% 
  summarise(snakes_mean = mean(openings),
            snakes_sd = sd(openings))

# Fomulate a hypothesis

# h0: There is no difference in the number of openings from day to day
# H1: There is a difference in the number of openings from day to to day


# Test a hypothesis -------------------------------------------------------
# library(Rmisc) # Unfortunately this overrides many dplyr functions

# first calculate SE and CI 

snakes.summary2 <-  Rmisc::summarySE(data = snakes,
                              measurevar = "openings",
                              groupvar = c("day"))

# Visualise the data

ggplot(data = snakes, aes( x = day, y = openings)) + 
  geom_segment(data = snakes.summary2, aes(x = day, xend = day,
                                           y = openings - ci, yend = openings + ci,
                                           colour = day),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = day), alpha = 0.6, show.legend = F) +
  geom_jitter(width = 0.05)

# But, wait we have two factors so we need two sets of hypotheses

# H0 : There is no difference between snakes with respect to
  # the number of openenings at which they habituate
# H0 : There is no difference between days in terms of
  # the number of openings at which the snakes habituate

# Test just the days hypothesis
snakes.day.aov <- aov(openings ~ day, data = snakes)
summary(snakes.day.aov)

# Test both hypotheses
snakes.all.aov <- aov(openings ~ day + snake, data = snakes)
summary(snakes.all.aov)

# There is a functyion which may be used to extract the residuals (numbers)
# Residuals = unexplained variation

# Testing assumptions afterwards ------------------------------------------
# First visualise normality of data

snakes.residuals <-  residuals(snakes.all.aov)
hist(snakes.residuals)

# Then visualise homoscedastisity

plot(fitted(snakes.all.aov), residuals(snakes.all.aov))

# Check Tukey results
snakes.tukey <-  TukeyHSD(snakes.all.aov, which = "snake")
plot(snakes.tukey)



# Moth ANOVA Exercise --------------------------------------------------------------

moth <- read_csv("moth_data.txt") %>% 
  gather(key = "trap", value = "count", -Location) 

# Summarising the data
moth_loc_summary <-  moth %>% 
  group_by(Location) %>% 
  summarise(moth_mean = mean(count),
            moth_sd = sd(count))

moth_trap_summary <-  moth %>% 
  group_by(trap) %>% 
  summarise(moth_mean = mean(count),
            moth_sd = sd(count))

# Calculating SE and CI for Location and Trap
moth_loc_summary_2 <- Rmisc::summarySE(data = moth, 
                                measurevar = "count",
                                groupvars = c("Location"))

moth_trap_summary_2 <- Rmisc::summarySE(data = moth, 
                                measurevar = "count",
                                groupvars = c("trap"))





# Visualising the data:

# Plot for Location
location <- ggplot(data = moth, aes( x = Location, y = count)) + 
  geom_segment(data = moth_loc_summary_2, aes(x = Location, xend = Location,
                                           y = count - ci, yend = count + ci,
                                           colour = Location),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = Location), alpha = 0.6, show.legend = F) +
  geom_jitter(width = 0.05)

# Plot for Trap
trap <- ggplot(data = moth, aes( x = trap, y = count)) + 
  geom_segment(data = moth_trap_summary_2, aes(x = trap, xend = trap,
                                              y = count - ci, yend = count + ci,
                                              colour = trap),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = trap), alpha = 0.6, show.legend = F) +
  geom_jitter(width = 0.05)

# Arranging the plots side by side

Final <- ggarrange(location, trap, 
                   ncol = 2, nrow = 1,
                   labels = c("Location"," Trap"),
                   common.legend = TRUE)

# Run the final plot

Final
# RWS: Very nice!

# Linear Regression -------------------------------------------------------

# Contrasting a variable to an independant variable such as
  # time, temp etc

# residual is the diff between the predicted value and the observed value
# a regression is described by y= mx +c 
# mx is the gradient (rise over run)
# c is the y-intercept - distancxe along the y axis


# Regressions -------------------------------------------------------------

# for the explanation o fthe statistical analysis
# we are going to use eruption data from ol' Faithful

#Viewing the head of the dataset

head(faithful)

# Plotting the data as a scatterplot

ggplot(data = faithful, aes(x = waiting, y = eruptions)) +
  geom_point()+
  geom_smooth(method = "lm", se = F, colour = "hotpink")

# H0 : Waiting time does NOT influence the duration of eruption
# H1: Waiting time DOES influence the duration of the eruption

# Test the hypothesis

faithful_lm <- lm( eruptions ~ waiting, data = faithful)
summary(faithful_lm)


# Correlations ------------------------------------------------------------

# coreelations looks at the relationship between 2 variables

# assumptions
# data are not independant of one another
# pairwise comparison of each individual value 
# no outliers
# linearity- just because a linear line can be fitted, doe snot infer linearity
# normality 
# homoscedastisity
# level of measurement :
  # Pearson for continuous data
  # Spearman for ordinal data

#Load libraries
library(tidyverse)
library(ggpubr)
library(corrplot)

# Load the data: Ecklonia

ecklonia <-  read_csv("ecklonia.csv")


# Formulate a hypothesis --------------------------------------------------

# H0: There is no relationship between stipe mass and primary blade length
  # for the kelp Ecklonia maxima
# H1: There is a relatiosnhip between stipe mass and primary blade length
  # for the kelp Ecklonia maxima


# Test the hypothesis -----------------------------------------------------

cor.test(ecklonia$primary_blade_length, ecklonia$stipe_mass)

# visualise the data 
ggplot(data = ecklonia, aes(primary_blade_length, y = stipe_mass)) +
  geom_point() +
  geom_smooth(method = "lm", se= T)

# run hecka tests at once: Provides a comparative table between variables

ecklonia_sub <- ecklonia %>%
  select(primary_blade_length:epiphyte_length)

ecklonia_cor <-  cor(ecklonia_sub)

ecklonia_cor

#  Other types of correlation tests ---------------------------------------

# Spearman rank test: Ordinal data 
# constructing size classes for length
ecklonia$length <- as.numeric(cut((ecklonia$stipe_length + ecklonia$frond_length),3))

# then run Spearman
cor.test(ecklonia$length, ecklonia$primary_blade_length, method = "spearman")

# Kendall rank correlation: Non-normal data
cor.test(ecklonia$primary_blade_length, ecklonia$primary_blade_width, method = "kendall")

# Visualise all the things!

ecklonia_pearson <-  cor(ecklonia_sub)
corrplot(ecklonia_cor, method = "circle")

<<<<<<< HEAD
# Heat map
# Load Library 
library(reshape2)

melted_weeds <- melt(ecklonia_pearson)
ggplot(melted_weeds, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1)) +
  labs(x = "Variable 1", y = "Variable 2")

# Exercise 1

# enter the mass at the end of the experiment

feed_1 <- c(60.8, 57.0, 65.0, 58.6, 61.7)
feed_2 <- c(68.7, 67.7, 74.0, 66.3, 69.8)
feed_3 <- c(102.6, 102.1, 100.2, 96.5)
feed_4 <- c(87.9, 84.2, 83.1, 85.7, 90.3)

#making the dataframe
chitlins <- as.tibble(data.frame(
  feed = c(
    rep("Feed 1", length(feed_1)),
    rep("Feed 2", length(feed_2)),
    rep("Feed 3", length(feed_3)),
    rep("Feed 4", length(feed_4))),
  mass = c(feed_1, feed_2, feed_3, feed_4)))

#Running the anova

chits.aov1 <- aov(mass ~ feed, data = chitlins)

# summarising the resulta of the ANOVA

summary(chits.aov1)


# Exercise 2: Developing a hypothesis

# h0: There is no difference in tooth length with regard to supplement application
# h1: There is a difference in tooth length with regard to supplement application


# Assigning the df to a vector

teeth <- ToothGrowth

# Running the ANOVA
teeth.aov <- aov(len ~ supp, data = teeth)

# Summarising the ANOVA results
summary(teeth.aov)

