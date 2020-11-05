#' Sort ZIP Codes
#'
#' Sorts pet registration data by ZIP code, provides total number of registered pets and dog:cat ratio by ZIP code
#' @param pet_licenses cleaned dataframe of pet registration data with columns of ZIPCode, Species, Primary.Breed
#' @keywords sort, ratio, pet
#' @export
#' @examples 
#' SortZIPCodes(pet_licenses)

SortZIPCodes <- function(pet_licenses){
  zip_codes <- split(pet_licenses, pet_licenses$ZIPCode)
  ratio_table <- data.frame(matrix(data=NA, nrow=length(unique(pet_licenses[["ZIPCode"]])), ncol=3))
  colnames(ratio_table)[1] <- "ZIPCode"
  colnames(ratio_table)[2] <- "Registered.Pets"
  colnames(ratio_table)[3] <- "Dog.to.Cat.Ratio"
  ratio_table$ZIPCode <- sapply(zip_codes,"[[",1,1)
  ratio_table$Registered.Pets <- sapply(zip_codes, nrow)
  library(stringr)
  ratio_table$Dog.to.Cat.Ratio <- sapply(1:nrow(ratio_table), function(x){
  sum(str_count((zip_codes[[x]][,2]), "Dog"))/sum(str_count((zip_codes[[x]][,2]), "Cat"))
    })
  ratio_table[ratio_table=="Inf"] <- "All Dogs"
  ratio_table <- ratio_table[order(-ratio_table$Registered.Pets),]
  return(ratio_table)
}