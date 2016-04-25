complete <- function(directory, id =1:332){
        ## directory is a character vector of length 1, indicates the directory where
        ## the data will be read from.
        ## id is a numeric vector. indicates the monitor IDs
        ## This function returns the filename and the number of observations in that file.
        
        old.dir <- getwd()
        
        ## if directory in the input is the full path, use setwd()
        #setwd(directory) 
        
        charid <- character()
        nobs <- character()
        idName <- numeric()
        
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
                
                readThis <- read.csv(paste(charid,"csv",sep = "."), header = TRUE, na.strings = "NA")
                t1 <- is.na(readThis["nitrate"]) | is.na(readThis["sulfate"])
                t2 <- length(readThis[!t1])/ncol(readThis)
                idName[i] <- id[i]
                nobs[i] <- t2
        }
        
        finalOP <- data.frame(id=idName,nobs=nobs)
        #print(finalOP)
       # setwd(old.dir)
}