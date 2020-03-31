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