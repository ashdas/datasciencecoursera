pollutantMean <- function(directory, pollutant, id =1:332){
        ## directory is a character vector of length 1, indicates the directory where
        ## the data will be read from.
        ## Pollutant is a character vector of length 1, values can be "sulfate" or "nitrate"
        ## id is a numeric vector. indicates the monitor IDs
        ## This function should return the mean of the pollutant level across the monitor Ids.
        ## It ignores NA values
        
        old.dir <- getwd()
        #setwd(directory)
        
        charid <- character()
        
        tempArray <- numeric()
        sumTemp <- 0
        lengthTemp <- 0
        
        ## loop through each of the ids
        for(i in 1:length(id)){
                
                ## start convert Id to char
                ## convert the Id to char, append "00" for id < 10 & "0" for 10<= id < 100 
                if(id[i] < 100){
                        if(id[i] < 10){
                                charid <- paste("00",as.character(id[i]),sep="")
                        }
                        else{
                                charid <- paste("0",as.character(id[i]),sep="")   
                        }
                }
                else{
                        charid <- as.character(id[i])
                }  
                ## end convert Id to char
                
                ## read the ith file and calculate the mean of the pollutant value in the Id file.
                ## Store the mean for the Id file in a numeric vector
                readThis <- read.csv(paste(charid,"csv",sep = "."), header = TRUE, na.strings = "NA")
                tempArray <- readThis[,pollutant][!(is.na(readThis[,pollutant]))]
                
                sumTemp <- sumTemp + sum(tempArray)
                lengthTemp <- lengthTemp + length(tempArray)
        }
        
        finalMean <- sumTemp/lengthTemp
        print(finalMean)
        setwd(old.dir)
}