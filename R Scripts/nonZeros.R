nonZeros <- function(inputData){
    names <- c("DDE","DD","ID","Capex/sales","R&D/sales","Leverage")
    for (name in names){
        filtered <- dim(filter(inputData, Data_point == name, value != 0))[1]
        print(name)
        print(filtered)
    }
}