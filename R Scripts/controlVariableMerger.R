mergeAll <- function(inputData, extraField){
    
    fields <- extraField
    
    fields <- c(fields, "SIZE","FSTS","R&D/sales","Leverage","Quick ratio","Capex/sales","ROA ","DD")

    info <- filter(inputData, Data_point == "Tobin's Q")
    
    info <- select(info, -Data_point)
    
    info$value <- log(info$value)

    redoneNames <- c("company","year","TQ")
    names(info) <- redoneNames    
    
    
    for(field in fields){
    
        toMerge <- filter(inputData, Data_point == field)
        
        redoneNames <- c(redoneNames, field)
        info <- tbl_df(merge(info,toMerge,by=c("company","year")))
        info <- select(info, -Data_point)
        names(info) <- redoneNames    
    }
    
    names(info) <- c("company", "year", "TQ" ,extraField, "SIZE","FSTS","RD","Leverage","Qratio","Capex","ROA","DD")
    
    info
}
