# Day 4
# Yameen Badrodien
# 19 April 2018
# Basic Stats: ANOVA

# ANOVA is used to establish differences between means of more than 2 samples
# Multiple t-tests increases the likelihood of error
# ANOVA decreases this error 

# ANOVA Assumptions

# 1. Normally distributed data
# 2. Homogeneity of variance
# 3. Independance of data
# 4. Data should be balanced: same number of subjects in samples etc

# ANOVA avoids commiting a type 1 error: Rejecting a H:0 when it should not

# load libraries
library(tidyverse)
library(ggpubr)

# assigning data set to a vector

chicks <- as_tibble(ChickWeight)

# filtering out diet 1 and 2 at day 21: Excludes other diets

chicks_sub <- chicks %>% 
  filter(Diet %in% c(1, 2), Time == 21)

# Running the t-test: This produces a tibble with sig values

compare_means(weight ~ Diet, data = chicks_sub, method = "t.test")

# we fail to reject the null hypothesis: DIet has no significant influence on chicken growth


# ANOVA: Multiple diets ---------------------------------------------------

# We use a single factor test because only one factor is being compared: diet

# Research Q: Is there a diff in chick mass at day 21 for 4 diff diets

# Hypothesis formulation
# Null Hypothesis:There is no difference in chicken mass at 21 days after having been fed 4 different diets

# filter the data to isolate day 21 
chicks_21 <- chicks %>% 
  filter(Time == 21)

# Run tha anova, and assign to vector

chicks.aov1 <- aov(weight ~ Diet, data = chicks_21)

# Perform a summary on the vector: Produces an F table
# Check the Pr value.

summary(chicks.aov1) 

# we do not accept H:0 and we accept the H:1.
# There is a significant difference in chick mass 
# after 21 days, having been fed diff diets

# Visualising the data to identify diff: Notched boxplots

ggplot(data = chicks_21, aes(x = Diet, y = weight)) +
  geom_boxplot(aes(fill = Diet), notch = TRUE)

# We look at the notches of the plots, if the notches overlap, there means between
# the plots are similar
# if the notches do not overlap, diet 1 and diet 3, then a sig diff exists between sets


# Tukey Posthoc ----------------------------------------------------------

TukeyHSD(chicks.aov1)

# This compares each diet to one another
# diff in the means
# lower and upper intervals
# padj- used as p value for diff

# Plotting this information
# Boxplots display distribution
# Use the geom-segment function

ggplot(data = chicks_21, aes(x = Diet, y = weight)) +
  geom_boxplot(aes(fill = Diet), notch = TRUE) +
  geom_segment(aes(x = Diet, xend = Diet, y = weight, yend = weight+2))

# Segments showing confidence intervals
# first extract the data provided by the Tukey HSD analysis

# make a DF of segments
chicks_tukey <- as.data.frame(TukeyHSD(aov(weight ~ Diet, data = chicks_21))$Diet)

#adding pairs as a factor of the data set
chicks_tukey$pairs <- as.factor(row.names(chicks_tukey))

#(the above will be your x-axis)

# Plotting the family wise confidence level graph
ggplot(data = chicks_tukey, aes(x = pairs, y = diff)) + 
  geom_segment(aes(x = lwr, xend = upr, y = pairs, yend = pairs))

# adding the 0 line using geom_vline
  
ggplot(data = chicks_tukey, aes(x = pairs, y = diff)) + 
  geom_segment(aes(x = lwr, xend = upr, y = pairs, yend = pairs)) +  
  geom_vline(xintercept= 0, linetype= "dotted")

# The automagic way

plot(TukeyHSD(chicks.aov1))


# ANOVA: Multiple Factor --------------------------------------------------

# H:0 There is non change in chicken mass (kg) from day 1 to 21

chicks_0_21 <- ChickWeight %>% 
  filter(Time %in% c(0,2,21))

ggplot(data = chicks_0_21, aes(x = Time, y = weight)) +
  geom_boxplot(notch = TRUE, aes(fill = as.factor(Time)))

# Run ANOVA
summary(aov(weight ~ as.factor(Time), data = chicks_0_21))

# Perform a Tukey post-hoc test
TukeyHSD(aov(weight ~ as.factor(Time), data = chicks_0_21))

# Look at the confidence intervals
plot(TukeyHSD((aov(weight ~ as.factor(Time), data = chicks_0_21))))

# look at day 0 and day 21 for both time and diet
(summary(aov(weight ~ Diet + as.factor(Time), data = filter(ChickWeight, Time %in% c(0,21)))))
     
# Or simply look at ALL of the time
# this isnt the hypothesis

summary(aov(weight ~ Diet + as.factor(Time), data = ChickWeight))
# Note the increase in the DF for time
# But no corresponding increase for diet

# Now look at interactions BETWEEN factors

summary(aov(weight ~ Diet + as.factor(Time), data = filter(ChickWeight, Time %in% c (0,21))))

# lets look at the Tukey results

TukeyHSD(aov(weight ~ Diet + as.factor(Time), data = filter(ChickWeight, Time %in% c (0, 3, 4 , 17, 21))))
plot(TukeyHSD(aov(weight ~ Diet + as.factor(Time), data = filter(ChickWeight, Time %in% c (0, 3, 4 , 17, 21)))))

# Create a line graph to help explain this concept

# First create mean values by Time and Diet

chicks_mean <- ChickWeight %>% 
  group_by(Diet, Time) %>% 
  summarise(weight_mean = mean(weight, na.rm = TRUE))

#Visualise the data so we can notice the relationship between factors
ggplot(data = chicks_mean, aes( x = Time, y = weight_mean, colour = Diet)) + 
  geom_line(size = 2) + 
  geom_point(shape = 15, size = 5)


# Non-parametric tests ----------------------------------------------------

# But what if....
# we dont have normal data?

# for a t-Test we rather use a wilcox rank sum test
wilcox.test(weight ~ Diet, data = chicks_0_21) 
# and then one fills this in same as we do for  t-test, apparently- but it didnt work :\

# and now for the Kruskall-Wallis
kruskal.test(weight ~ Diet, data = chicks_0_21)

# Load this for non-paramteric post-hoc test
library(pgirmess)
kruskalmc(weight ~ Diet, data = chicks_0_21)
