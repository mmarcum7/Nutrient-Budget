#' Institution_Population
#'
#' @param Residential_Employees -Number of Residential Employees
#' @param In_patient_hospital_beds -Number of in-patient hospital beds
#' @param FTE_students - Full-time equivalent enrollment (annualized FTE)
#' @param FTE_employees - Full-time equivalent of employees (annualized FTE)
#' @param FTE_distance_educated_students-Full-time equivalent of distance education students (annualized FTE)
#'
#'
#' @return  
#' @export 
#'
#' @examples
Institution_Population <- function(Residential_Employees, In_patient_hospital_beds, FTE_students, FTE_employees, FTE_distance_educated_students){
    return(Residential_Employees + In_patient_hospital_beds + FTE_students + FTE_employees - 0.75 * FTE_distance_educated_students)
}


#' Institution - Total Food Mass
#'
#' @param Food_Category - kind of food
#' @param Food_Mass - UVA Total Food Mass in kg
#' @param Institution_Population- population value for Loyola 
#' @param 
#'
#'
#' @return  
#' @export 
#'
#' @examples
#' 

Total_Food_Mass <-function(Food_Category, Food_Mass, UVA_Population, Institution_Population) {
  UVA_Population <- 34056  # UVA population is a constant 
  if(Food_Category == "Poultry") {
   mass<-(254697 * Institution_Population/UVA_Population) # 254697= UVA Total food mass for poultry
  }
  else if(Food_Category == "Bovine") {
   mass<-(193361  * Institution_Population/UVA_Population) #  193361 = UVA Total food mass for Bovine
  }
  else if(Food_Category == "Pigmeat") {
    mass<-(153006 * Institution_Population/UVA_Population) #  153,006 = UVA Total food mass for Bovine
  }
  else if(Food_Category == "Milk") {
    mass<-(438138 * Institution_Population/UVA_Population) #   438,138  = UVA Total food mass for Bovine
  }
  else if(Food_Category == "Cheese") {
    mass<-(204446 * Institution_Population/UVA_Population) # 204,446   = UVA Total food mass for Bovine
  }
  else if(Food_Category == "Eggs") {
    mass<-(200567  * Institution_Population/UVA_Population) # 200,567    = UVA Total food mass for Bovine
  }
  else if(Food_Category == "Fish") {
    mass<-(28033   * Institution_Population/UVA_Population) #  28,033= UVA Total food mass for Bovine
  }
  else if(Food_Category == "Cereals") {
    mass<-(422315    * Institution_Population/UVA_Population) # 422,315  = UVA Total food mass for Bovine
  }
  else if(Food_Category == "Fruits") {
    mass<-(393925 * Institution_Population/UVA_Population) # 393,925  = UVA Total food mass for Bovine
  }
  else if(Food_Category == "Pulses") {
    mass<-(77077  * Institution_Population/UVA_Population) #  77,077   = UVA Total food mass for Bovine
  }
  else if(Food_Category == "Starchy Roots") {
    mass<-(213022  * Institution_Population/UVA_Population) # 213,022 = UVA Total food mass for Bovine
  }
  else if(Food_Category == "Vegetables") {
    mass<-( 195840   * Institution_Population/UVA_Population) # 195,840  = UVA Total food mass for Bovine
  }
  else if(Food_Category == "Stimulants") {
    mass<-( 133601    * Institution_Population/UVA_Population) #  133,601   = UVA Total food mass for Bovine
  }
  else if(Food_Category == "Oilcrops") {
    mass<-( 115328    * Institution_Population/UVA_Population) # 115,328    = UVA Total food mass for Bovine
  }
  else if(Food_Category == "Sugarcrops") {
    mass<-(145231     * Institution_Population/UVA_Population) # 145,231   = UVA Total food mass for Bovine
  }
  else if(Food_Category == "Nuts") {
    mass<-(11071   * Institution_Population/UVA_Population) # 11,071   = UVA Total food mass for Bovine
  }
  else if(Food_Category == "Spices") {
    mass<-(5521 * Institution_Population/UVA_Population) # 5,521   = UVA Total food mass for Bovine
  }
  else if(Food_Category == "Beverages") {
    mass<-(517861 * Institution_Population/UVA_Population) # 517,861   = UVA Total food mass for Bovine
  }
   
  return(mass)
}


#OR 
#' Institution - Total Food Mass
#'
#' @param Food_Category Kind of food 
#' @param Institution_Population Population value for Loyola
#'
#' @return Estimated total food mass for the specified category at the institution
#' @export
#'
#' @examples
#' Total_Food_Mass("Poultry", Institution_Population)



Total_Food_Mass <- function(Food_Category, Institution_Population) {
  UVA_Population <- 34056  # UVA population is a constant 
  
  food_masses <- c(
    Poultry = 254697,
    Bovine = 193361,
    Pigmeat = 153006,
    Milk = 438138,
    Cheese = 204446,
    Eggs = 200567,
    Fish = 28033,
    Cereals = 422315,
    Fruits = 393925,
    Pulses = 77077,
    Starchy_Roots = 213022,
    Vegetables = 195840,
    Stimulants = 133601,
    Oilcrops = 115328,
    Sugarcrops = 145231,
    Nuts = 11071,
    Spices = 5521,
    Beverages = 517861
  )
 
    mass <- (food_masses[Food_Category] * Institution_Population / UVA_Population)

return(mass)
}



Total_Food_Mass("Poultry", Institution_Population)





  







