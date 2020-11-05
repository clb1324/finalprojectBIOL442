#' Pet Pie Chart
#'
#' Provides pie chart breakdown of breeds of dogs or cats in specific ZIP code
#' @param pet_licenses cleaned dataframe of pet registration data with columns of ZIPCode, Species, Primary.Breed
#' @param zipcode the ZIP code of interest, should be in format "00000"
#' @param species_name "Dog" or "Cat", depending on which pet species want to examine
#' @keywords pie, chart, breakdown, pet
#' @export
#' @examples 
#' PetPieChart(pet_licenses,"98004","Dog")

PetPieChart <- function(pet_licenses,zipcode,species_name){
  zip_codes <- split(pet_licenses, pet_licenses$ZIPCode)
  species.specified <- subset(zip_codes[[zipcode]],Species==species_name)
  breed.breakdown <- table(species.specified$Primary.Breed)
  breed.breakdown <- as.data.frame(breed.breakdown)
  colnames(breed.breakdown)[1]<-"breed"
  colnames(breed.breakdown)[2]<-"frequency"
  pie(breed.breakdown$frequency, labels=breed.breakdown$breed, main="Pie Chart of Breeds", radius=1, cex=0.4)
}