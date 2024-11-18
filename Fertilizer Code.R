#' Title:Calculate N Released from Fertilizers
#'
#' @param Nitrogen_in_Fertilizer - amount of nitrogen in the fertilizer in lb
#' @param Fertilizer_Uptake_Factor - fertilizer uptake factor for the associated vegetation/plant in a percantage
#' @param 0.453592 - to convert lbs into kgs
#'
#' @return Total NOx released in kgs
#' @export
#'
#' @examples

calc_N <- function(plant_type, Nitrogen_in_fertilizer){
  if (plant_type == "trees"){
    #0.349 is the Fertilizer Uptake Factor from University of Virginia
    nitrogen_released <- Nitrogen_in_fertilizer * (1-0.349) *0.453592
    
  }else if (plant_type == "shrubs"){
    #0.450 is the Fertilizer Uptake Factor from University of Virginia
    nitrogen_released <- Nitrogen_in_fertilizer * (1-0.450) * 0.453592
    
  }else if (plant_type == "turfgrass"){
    #0.500 is the Fertilizer Uptake Factor from University of Virginia
    nitrogen_released <- Nitrogen_in_fertilizer * (1-0.500) * 0.453592
    
  }else if (plant_type == "general landscaping"){
    #0.450 is the Fertilizer Uptake Factor from University of Virginia
    nitrogen_released <- Nitrogen_in_fertilizer * (1-0.450) * 0.453592
  }
  return(nitrogen_released)
}

Trees <- calc_N("trees",.500)
Shrubs <- calc_N("shrubs",.500)
Turfgrass <- calc_N("turfgrass", .500)
General_landscaping <- calc_N("general landscaping",.500)

print(Trees)
print(Shrubs)
print(Turfgrass)
print(General_landscaping)
