
rankall <- function(outcome, num = "best"){
        if (num=="best"){num <-1}
        df <- read.csv("outcome-of-care-measures.csv",  colClasses = "character") #reads the .csv-file
        suppressWarnings({ #removes the warnings produced by coercing strings into numbers
                df[,11] <- as.numeric(df[,11]) #changes heart attacks into numbers
                df[,17] <- as.numeric(df[,17]) #changes heart failures into numbers
                df[,23] <- as.numeric(df[,23]) #changes pneumonias into numbers
        })
        if(!outcome %in% c("heart attack","heart failure", "pneumonia")){stop("invalid outcome")}
        if(outcome == "heart attack") {
                oc <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
                
        } else if (outcome == "heart failure") {
                oc <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
                
        } else if (outcome == "pneumonia") {
                oc <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        }
        concise <- df[,c(2,7,11,17,23)] #stores a new dataframe with only the relevant columns
        ranked <- concise[order(concise$State,concise[[oc]],concise$Hospital.Name, na.last = T),] #sorts the hospitals in the given state by state, outcome and by hospital name
        short <- ranked[,1:2] #stores a new dataframe with only the hospital name and state
        names <- split(short[,1], short$State) #splits the dataframe by state into a list of vectors 
        if(num=="worst"){
                lapply(names, tail, n=1)
        } else{
                rdf <- data.frame(lapply(names, "length<-", max(lengths(names)))) #reforms the list of vectors into a new dataframe with each state as a column
                trdf <- t(rdf) #transposes the dataframe, so each state becomes a row with column 1 showing the best hospital, column 2 showing the second best hospital etc.
                as.list(trdf[,num]) #returns the 'num'th column with the 'num'th best hospital in each state
        }
}

