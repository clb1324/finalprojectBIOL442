#' Clean Pet Data
#'
#' Cleans Pet Registration Data
#' @param example_data pet registration data to clean, should include columns of ZIP.Code, Species, Primary.Breed
#' @keywords clean, pet
#' @export
#' @examples 
#' CleanPetData(example_data)

CleanPetData <- function(example_data){
  pet_licenses <- data.frame(example_data$ZIP.Code, example_data$Species, example_data$Primary.Breed)
  colnames(pet_licenses)[1] <- "ZIPCode"
  colnames(pet_licenses)[2] <- "Species"
  colnames(pet_licenses)[3] <- "Primary.Breed"
  list.1 <- strsplit(pet_licenses$Primary.Breed, ",")
  pet_licenses$Primary.Breed <- sapply(list.1, "[[", 1)
  pet_licenses$Primary.Breed[grep("mix", pet_licenses$Primary.Breed, ignore.case=TRUE)] <- "Mix"
  pet_licenses$Primary.Breed[grep("shepherd", pet_licenses$Primary.Breed, ignore.case=TRUE)] <- "Shepherd Dog"
  pet_licenses$Primary.Breed[grep("hound", pet_licenses$Primary.Breed, ignore.case=TRUE)] <- "Hound Dog"
  pet_licenses$Primary.Breed[grep("sheep", pet_licenses$Primary.Breed, ignore.case=TRUE)] <- "Sheepdog"
  pet_licenses[pet_licenses==""] <- NA
  pet_licenses <- na.omit(pet_licenses)
  list.2 <- strsplit(pet_licenses$ZIPCode, "-")
  pet_licenses$ZIPCode <- sapply(list.2, "[[", 1)
  pet_licenses$ZIPCode <- as.numeric(pet_licenses$ZIPCode)
  pet_licenses <- subset(pet_licenses, ZIPCode>=98000)
  pet_licenses <- subset(pet_licenses, ZIPCode<=99499)
  return(pet_licenses)
}