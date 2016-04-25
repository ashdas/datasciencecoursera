corr <- function(directory, threshold = 0){
        ## directory is a character vector of length 1, indicates the directory where
        ## the data will be read from.
        ## id is a numeric vector. indicates the monitor IDs
        ## This function returns the filename and the number of observations in that file.
        
        #old.dir <- getwd()
        
        ## if directory in the input is the full path, use setwd()
        #setwd(directory) 
        
        charid <- character()
        finalop <- numeric()

        ## loop through each of the ids
        for(i in 1:332){
                
                ## start convert Id to char
                ## convert the Id to char, append "00" for id < 10 & "0" for 10<= id < 100 
                if(i < 100){
                        if(i < 10){
                                charid <- paste("00",as.character(i),sep="")
                        }
                        else{
                                charid <- paste("0",as.character(i),sep="")   
                        }
                }
                else{
                        charid <- as.character(i)
                }  
                ## end convert Id to char
                
                readThis <- read.csv(paste(charid,"csv",sep = "."), header = TRUE, na.strings = "NA")
                a1 <- data.frame("sulfate" = readThis$sulfate, "nitrate" =readThis$nitrate)
                a1 <- na.omit(a1)
              
                if(nrow(a1) > threshold){
                        co <- cor(a1$sulfate, a1$nitrate)
                        finalop[i] <- co
                }
        }
        
        finalop <- na.omit(finalop)
}