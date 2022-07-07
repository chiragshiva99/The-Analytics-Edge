
# Please report your name and ID in the two lines below
# Name: Chirag Shivakumar
# ID: 1004996

# Load all packages
rm(list=ls())      # Remove all files from current directory
library(ggplot2)   # For data visualization
library(readxl)    # For reading excel files


## Question 1 [1 point]###############################################################

## Task 1 ###################################################################

# Which of the following (see question paper) is correct when comparing LASSO to an ordinary least squares linear regression?
# Select one of (a) - (d). You do not need to justify your answer.


# WRITE YOUR ANSWER HERE:
# Ans C

# Question 2 [4 points] ###################################################################

## Task 1 ###################################################################

# In this task we read and visualize the data. 

# Remove all variables from the R environment to create a fresh start
rm(list=ls())

# Create dataframe
auto <- read.csv("Auto.csv")
auto$horsepower <- as.numeric(as.character(auto$horsepower)) # convert the `horsepower `variable to numeric.

### Task 1.1 ###################################################################

# Find out the correlation between  mpg and horsepower.

cor(auto$mpg, auto$horsepower, use = "pairwise.complete.obs")

### Task 1.2 ###################################################################

# Plot `mpg` versus `horsepower` and draw a line of best fit through the data
ggplot(auto,aes(horsepower,mpg))+geom_point(na.rm=T)+geom_smooth(method="lm",na.rm=T,se=F)

## Task 2  #####################################################################

# Perform a simple linear regression with `mpg` as the response and all other variables except `names` as predictor.
model1<- lm(mpg~.-name, data=auto)
x<- summary(model1)

### Task 2.1 ###################################################################

# Which variables are significant at 0.001 level? What is the adjusted-$R^2$ for this model?

# WRITE YOUR ANSWER HERE:
# weight, year, origin
x$adj.r.squared 

############################################ END OF EXAM ###################################################