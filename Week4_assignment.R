##### FIRST EXERCISE IN WEEK 4 PROGRAMMING ASSIGNMENT #####

#loads the outcome data
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character"); head (outcome)
str(outcome)

class(outcome[,11]); head(outcome[,11]) #prints the initial class and overhead of the 30-day death rates

outcome[,11] <- as.numeric(outcome[,11]) #changes the outcome measure from a string to numbers

class(outcome[,11]); head(outcome[,11]) #prints the class and overhead of the 30-day death rates after attempting to swith class

hist(outcome[,11], main = "30-day death rates", xlab = "30-day death rate") #prints a histogram of the 30-day death rates

##### SECOND EXERCISE IN WEEK 4 PROGRAMMING ASSIGNMENT #####

#creates the function to find the best hospital in a state
#I removed outcome as an argument - instead the function prompts you for either heart attack, heart failure or pneumonia
#I added an optional argument (with "outcome-of-care-measures.csv" as default value), so I may change the .csv-file to be read
best <- function(state, data = "outcome-of-care-measures.csv"){
        df <- read.csv(data, colClasses = "character")
        if(!state %in% df$State){print("invalid state")}
        print("worked so far")
        subs <- subset(df, df$State == state)
        oc <- select.list(c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"), graphics = T, title = "Choose between heart attack, heart failure or pneumonia")
        low <- subset(subs, subs[[oc]]==min(na.omit(subs[[oc]])))[, "Hospital.Name"]
        min(low)
}
best("NY")
select.list(c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"), graphics = T, title = "Choose between heart attack, heart failure or pneumonia")

min(na.omit(outcome[,11]))

subset(outcome, outcome[,11]==min(na.omit(outcome[,11])))[, "Hospital.Name"]
test <- subset(outcome, outcome$State == "NY")
head(test)
test$State