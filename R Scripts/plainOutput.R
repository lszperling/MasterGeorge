unifyFilesPlain <- function(folder){
    library(tools)
    library(dplyr)
    library(tidyr)
    file_list <- list.files(folder)
    file_list <- file_list[- grep("Total|Explanations of the table|Sheet1", file_list)]
    for (file in file_list){
        
        fileName <- paste(folder,file, sep = "")
        # if the merged dataset doesn't exist, create it
        
        if (!exists("dataset")){
            dataset <- read.csv(fileName, header=TRUE)
            dataset$company <- basename(file_path_sans_ext(file))
        }else{
            
            # if the merged dataset does exist, append to it
            
            temp_dataset <-read.csv(fileName, header=TRUE)
            temp_dataset$company <- basename(file_path_sans_ext(file))
            dataset<-rbind(dataset, temp_dataset)
            rm(temp_dataset)
        }
    }
    completeDataSet <- tbl_df(dataset)
    completeDataSet <- mutate(completeDataSet,X = company)
    newNames <- c("Data_point","Y.2006","Y.2007","Y.2008","Y.2009","Y.2010","Y.2011","Y.2012","Y.2013","company")
    firstPart <- select(completeDataSet, Year.indexes:X)
    secondPart <- select(completeDataSet, X.1:company)
    names(firstPart) <- newNames
    names(secondPart) <- newNames
    result <- rbind(firstPart, secondPart)
    
    #result <- gather(result, year, value, -c(Data_point,company))
    #gather(result, year, 2006:2013)
    #result <- filter(result, !is.na(value), !Data_point == "")
    #result$value <- as.numeric(result$value) 
    result <- filter(result, !is.na(Y.2007), !Data_point == "")
    
    result
    
}