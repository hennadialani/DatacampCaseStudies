#Part 1 of Statistics in R track
#Case Study - Beauty in the Classroom
#Based off course evaluations in UT Austin to determine whether course evaluation
#scores depend on the perceived attractiveness of an instructor

library(dplyr)
library(ggplot2)
#loading evals since it is an RData file
load("~/Documents/DatacampCaseStudies/evals.RData")

#Inspecting the data
nrow(evals)
ncol(evals)
summary(evals)
str(evals)

#It is an observational study. The data was collected by randomly sampling classes.

#Identify variable types
glimpse(evals)
#factors - categorical 
#ratings - numerical - discrete

# Categorical variables
cat_vars <- c("rank", "ethnicity", "gender", "language", "cls_level", "cls_profs", "cls_credits",
              "pic_outfit", "pic_color")

#Recoding cls_type as a categorical variable
evals <- evals %>%
# Create new variable
mutate(cls_type = ifelse(18 >= cls_students, "small", 
                         ifelse(cls_students >= 19 & 59 >= cls_students , "midsize", "large")))

#could have also wrapped nested ifelse statements into factor

#relationship between average beauty rating and average prof evaluation score using ggplot2
# Scatterplot of score vs. bty_avg
ggplot(evals, aes(x = bty_avg, y = score)) +
  geom_point()

#seeing this over different class types (cls_type)
# Scatterplot of score vs. bty_avg colored by cls_type
ggplot(evals, aes(x = bty_avg, y = score, color = cls_type)) +
  geom_point()
