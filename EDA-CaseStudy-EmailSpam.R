#Exploratory Data Analysis - Case Study
#Email dataset
#Association between spam and length of an email?
# Load packages
library(ggplot2)
library(dplyr)
library(openintro)

# summary statistics
email %>%
  group_by(spam) %>%
  summarize(IQR(num_char), median(num_char))

# plot
email %>%
  mutate(log_num_char = log(num_char)) %>%
  ggplot(aes(x = spam, y = log_num_char)) +
  geom_boxplot()

#Median length of non-spam emails > median length of spam emails

#More obvious indicator of spam - exclamation marks?
#center and spread for exclaim_mess by spam
email %>%
  group_by(spam) %>%
  summarize(median(exclaim_mess),
            IQR(exclaim_mess))

# plot for spam and exclaim_mess
email %>%
  mutate(log_exclaim_mess = log(exclaim_mess + .01)) %>%
  ggplot(aes(x = log_exclaim_mess)) +
  geom_histogram() +
  facet_wrap(~ spam)

# side-by-side box plots
email %>%
  mutate(log_exclaim_mess = log(exclaim_mess + .01)) %>%
  ggplot(aes(x = 1, y = log_exclaim_mess)) +
  geom_boxplot() +
  facet_wrap(~ spam)

# Overlaid density plots
email %>%
  mutate(log_exclaim_mess = log(exclaim_mess + .01)) %>%
  ggplot(aes(x = log_exclaim_mess, fill = spam)) +
  geom_density(alpha = 0.3)

#Not spam actually has more exclamation marks; zero inflation

#Collapsing levels
# plot of proportion of spam by image
email %>%
  mutate(has_image = image > 0) %>%
  ggplot(aes(x = has_image, fill = spam)) +
  geom_bar(position = "fill")
#Email without image more likely to be not spam than spam

#Data Integrity - do images count as attachments?
sum(email$image > email$attach)
#Yes - since image is never greater than attach

#For emails containing the word "dollar", does the typical spam email contain a greater number of occurrences of the word than the typical non-spam email? 
email %>%
  filter(dollar > 0) %>%
  group_by(spam) %>%
  summarize(median(dollar))

#If you encounter an email with greater than 10 occurrences of the word "dollar", is it more likely to be spam or not-spam? 
email %>%
  filter(dollar > 10) %>%
  ggplot(aes(x = spam)) +
  geom_bar()

#explore association between number and spam
# Reorder levels
email$number <- factor(email$number, levels = c("none", "small", "big"))

# plot of number
ggplot(email, aes(x = number)) +
  geom_bar() +
  facet_wrap(~ spam)
