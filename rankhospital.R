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