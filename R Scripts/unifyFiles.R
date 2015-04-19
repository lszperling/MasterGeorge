unifyFiles <- function(folder){
    library(tools)
    library(dplyr)
    file_list <- list.files(folder)
    file_list <- file_list[- grep("Total|Explanations of the table", file_list)]
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
    newNames <- c("Data point","Y2006","Y2007","Y2008","Y2009","Y2010","Y2011","Y2012","Y2013","company")
    firstPart <- select(completeDataSet, Year.indexes:X)
    secondPart <- select(completeDataSet, X.1:company)
    names(firstPart) <- newNames
    names(secondPart) <- newNames
    result <- rbind(firstPart, secondPart)
    result <- filter(result, !is.na(Y2010), !is.na(Y2006))
    
}