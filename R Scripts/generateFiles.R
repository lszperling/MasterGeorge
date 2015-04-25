generateStatFiles <- function(unfilteredData){
    
    withFXD <- (filter(unfilteredData, Data_point == "FXD", value == 1))
    withoutFXD <- (filter(unfilteredData, Data_point == "FXD", value == 0))
    
    
    mergedWithFXD <- tbl_df(merge(unfilteredData,withFXD,by=c("company","year")))
    mergedWithoutFXD <- tbl_df(merge(unfilteredData,withoutFXD,by=c("company","year")))
    
    colnames(mergedWithFXD) <- c("company","year","Data_point","value","Data_point.y","value.y")
    colnames(mergedWithoutFXD) <- c("company","year","Data_point","value","Data_point.y","value.y")
    
    outputWithFXD <- generateStats(mergedWithFXD)
    outputWithoutFXD <- generateStats(mergedWithoutFXD)
    outputAll <- generateStats(unfilteredData)
    
    write.csv(outputWithFXD, "outputs/outputWithFXD.csv", row.names = FALSE)
    write.csv(outputWithoutFXD, "outputs/outputWithoutFXD.csv", row.names = FALSE)
    write.csv(outputAll, "outputs/outputAll.csv", row.names = FALSE)
}


generateStats <- function(dataToProcess){
    
    fields <- c("SIZE","Sales","FSD","FSTS","Tobin's Q","FXD", "Gross FXD","DERIV","R&D/sales","Leverage","Quick ratio","Capex/sales","ROA ","DD","ID","DE","DDE")
    
    outputWithFXD <- data.frame()
    #titles <- c("Measure", "Mean", "Median", "SD", "Observations")
    measures <- c()
    means <- c()
    medians <- c()
    sds <- c()
    observations <- c()
    
    for (dataPoint in fields){
        
        
        info <- filter(dataToProcess, Data_point == dataPoint)$value
        
        measures <- c(measures, dataPoint)
        means <- c(means, mean(info, na.rm = TRUE))
        medians <- c(medians, median(info, na.rm = TRUE))
        sds <- c(sds, sd(info, na.rm = TRUE))
        observations <- c(observations,length(info))
        # outputWithFXD <- rbind(outputWithFXD, c(dataPoint, mean(info, na.rm = TRUE), median(info, na.rm = TRUE), sd(info, na.rm = TRUE)))
        
        
    }
    
    info <- log(filter(dataToProcess, Data_point == "Tobin's Q")$value)
    measures <- c(measures, "Tobin's Q (ln)")
    means <- c(means, mean(info, na.rm = TRUE))
    medians <- c(medians, median(info, na.rm = TRUE))
    sds <- c(sds, sd(info, na.rm = TRUE))
    observations <- c(observations,length(info))
    
    titles <- c("Measure", "Mean", "Median", "SD", "Observations")
    outputWithFXD <- data.frame(measures, means, medians, sds, observations)
    colnames(outputWithFXD) <- titles
    outputWithFXD
}