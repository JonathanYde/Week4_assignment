##### FIRST EXERCISE IN WEEK 4 PROGRAMMING ASSIGNMENT #####

#loads the outcome data
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character"); head (outcome)
str(outcome)

class(outcome[,11]); head(outcome[,11]) #prints the initial class and overhead of the 30-day death rates

outcome[,11] <- as.numeric(outcome[,11]) #changes the outcome measure from a string to numbers

class(outcome[,11]); head(outcome[,11]) #prints the class and overhead of the 30-day death rates after attempting to swith class

hist(outcome[,11], main = "30-day death rates", xlab = "30-day death rate") #prints a histogram of the 30-day death rates

##### SECOND EXERCISE IN WEEK 4 PROGRAMMING ASSIGNMENT #####


#### CREATIVE SOLUTION TO THE SECOND EXERCISE #####
#### SKIP THIS IS YOU WANT TO READ A SCRIPT MORE IN LINE WITH THE ASSIGNMENT #####
#creates the function to find the best hospital in a state
#I removed outcome as an argument - instead the function prompts you for either heart attack, heart failure or pneumonia
#I added an optional argument (with "outcome-of-care-measures.csv" as default value), so I may change the .csv-file to be read
best <- function(state, data = "outcome-of-care-measures.csv"){
        df <- read.csv(data,  colClasses = "character") #reads the .csv-file
        suppressWarnings({ #removes the warnings produced by coercing strings into numbers
        df[,11] <- as.numeric(df[,11]) #changes heart attacks into numbers
        df[,17] <- as.numeric(df[,17]) #changes heart failures into numbers
        df[,23] <- as.numeric(df[,23]) #changes pneumonias into numbers
        })
        if(!state %in% df$State){print("invalid state")} else {#tests whether the given state is in the dataset
        subs <- subset(df, df$State == state) #subsets the dataframe by the given state
        #the next line prompts the user to select the outcome measure from a list
        oc <- select.list(c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"), graphics = T, title = "Choose between heart attack, heart failure or pneumonia")
        low <- subset(subs, subs[[oc]]==min(na.omit(subs[[oc]])))[, "Hospital.Name"] #selects the hospitals with the lowest mortality
        min(low) #selects the hospital with the lowest mortality that is first alphabetically
        }
}
best(state = "MD")

#this is a second attempt at solving the same exercise, which is probably more in alignment with how the assignment was written

best2 <- function(state, outcome){
        df <- read.csv("outcome-of-care-measures.csv",  colClasses = "character") #reads the .csv-file
        suppressWarnings({ #removes the warnings produced by coercing strings into numbers
                df[,11] <- as.numeric(df[,11]) #changes heart attacks into numbers
                df[,17] <- as.numeric(df[,17]) #changes heart failures into numbers
                df[,23] <- as.numeric(df[,23]) #changes pneumonias into numbers
        })
        if(!state %in% df$State){print("invalid state")} else {#tests whether the given state is in the dataset
                subs <- subset(df, df$State == state) #subsets the dataframe by the given state
                if(outcome == "heart attack") {
                        oc <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
                        low <- subset(subs, subs[[oc]]==min(na.omit(subs[[oc]])))[, "Hospital.Name"] #selects the hospitals with the lowest mortality
                        min(low) #selects the hospital with the lowest mortality that is first alphabetically
                        
                } else if (outcome == "heart failure") {
                        oc <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
                        low <- subset(subs, subs[[oc]]==min(na.omit(subs[[oc]])))[, "Hospital.Name"] #selects the hospitals with the lowest mortality
                        min(low) #selects the hospital with the lowest mortality that is first alphabetically
                        
                } else if (outcome == "pneumonia") {
                        oc <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
                        low <- subset(subs, subs[[oc]]==min(na.omit(subs[[oc]])))[, "Hospital.Name"] #selects the hospitals with the lowest mortality
                        min(low) #selects the hospital with the lowest mortality that is first alphabetically
                        
                } else {
                        print("invalid outcome")
                }
                #low <- subset(subs, subs[[oc]]==min(na.omit(subs[[oc]])))[, "Hospital.Name"] #selects the hospitals with the lowest mortality
                #min(low) #selects the hospital with the lowest mortality that is first alphabetically
        }
}

best2(state = "MD", outcome = "heart attack")


##### THIRD EXERCISE IN WEEK 4 PROGRAMMING ASSIGNMENT #####
rankhospital <- function(state, outcome, num = "best"){
        df <- read.csv("outcome-of-care-measures.csv",  colClasses = "character") #reads the .csv-file
        suppressWarnings({ #removes the warnings produced by coercing strings into numbers
                df[,11] <- as.numeric(df[,11]) #changes heart attacks into numbers
                df[,17] <- as.numeric(df[,17]) #changes heart failures into numbers
                df[,23] <- as.numeric(df[,23]) #changes pneumonias into numbers
        })
        if(!state %in% df$State){print("invalid state")} else {#tests whether the given state is in the dataset
                subs <- subset(df, df$State == state) #subsets the dataframe by the given state
                if(!outcome %in% c("heart attack","heart failure", "pneumonia")){print("invalid outcome")} else {
                #tests whether the given outcome measure is either 'heart attack', 'heart failure' or 'pneumonia'
                        if(outcome == "heart attack") {
                                oc <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
                                
                        } else if (outcome == "heart failure") {
                                oc <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
                                
                        } else if (outcome == "pneumonia") {
                                oc <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
                        }
                        if(num == "best"){
                                low <- subset(subs, subs[[oc]]==min(na.omit(subs[[oc]])))[, "Hospital.Name"] #selects the hospitals with the lowest mortality
                                min(low) #selects the hospital with the lowest mortality that is first alphabetically
                        } else if (num == "worst"){
                                high <- subset(subs, subs[[oc]]==max(na.omit(subs[[oc]])))[, "Hospital.Name"] #selects the hospitals with the lowest mortality
                                max(high) #selects the hospital with the lowest mortality that is first alphabetically
                        } else if (num > nrow(subs)) {
                                return(NA) #returns "NA" when the number given exceeds the number of hospitals
                        } else {
                                ranked <- subs[order(subs[[oc]],subs$Hospital.Name, na.last = T),] #sorts the hospitals in the given state by outcome and by hospital name
                                ranked[[num,2]] #returns the 'num'th best hospital
                        }
                }
        }
}

