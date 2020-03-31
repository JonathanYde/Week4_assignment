best <- function(state, outcome){
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