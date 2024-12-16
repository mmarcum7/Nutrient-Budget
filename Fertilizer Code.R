#' Title:Calculate N Released from Fertilizers
#'
#' @param Nitrogen_in_Fertilizer - amount of nitrogen in the fertilizer in lb provided by Loyola University Chicago for the year of 2024
#' @param Fertilizer_Uptake_Factor - fertilizer uptake factor for the associated vegetation/plant in a percentage provided by UVA
#' @param 0.453592 - to convert lbs into kgs
#'
#' @return Total NOx released in kgs
#' @export
#'
#' @examples


#divided by four to account for application at different sites, 860 is the total amount applied
calc_N <- function(plant_type){
  nitrogeninfertilizer <-c(
    syntheticfertilizer = 860 / 4
  )
  
  uptake_factor <-c(
    trees = 0.349,
    shrubs = 0.450,
    turfgrass = 0.500,
    generallandscaping = 0.450
  )
  nitrogen_released <- nitrogeninfertilizer *(1 - uptake_factor[plant_type]) * 0.453593
  return(nitrogen_released)
}


Trees <- calc_N("trees")
Shrubs <- calc_N("shrubs")
Turfgrass <- calc_N("turfgrass")
General_landscaping <- calc_N("generallandscaping")

print(Trees)
print(Shrubs)
print(Turfgrass)
print(General_landscaping)

Total_N_released <- Trees + Shrubs + Turfgrass + General_landscaping
print(Total_N_released)
