##### FIRST EXERCISE IN WEEK 4 PROGRAMMING ASSIGNMENT #####

#loads the outcome data
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character"); head (outcome)
str(outcome)

class(outcome[,11]); head(outcome[,11]) #prints the initial class and overhead of the 30-day death rates

outcome[,11] <- as.numeric(outcome[,11]) #changes the outcome measure from a string to numbers

class(outcome[,11]); head(outcome[,11]) #prints the class and overhead of the 30-day death rates after attempting to swith class

hist(outcome[,11], main = "30-day death rates", xlab = "30-day death rate") #prints a histogram of the 30-day death rates

##### SECOND EXERCISE IN WEEK 4 PROGRAMMING ASSIGNMENT #####

#solution for best hospital
source("best.R")

#alternative solution, where outcome is not given as an argument but prompted
#source("best_prompt.R")


##### THIRD EXERCISE IN WEEK 4 PROGRAMMING ASSIGNMENT #####
source("rankhospital.R")


##### FOURTH EXERCISE IN WEEK 4 PROGRAMMING ASSIGNMENT #####
source("rankall.R")