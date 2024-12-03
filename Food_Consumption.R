#calcInstitution_Population####
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
calcInstitution_Population <- function(Residential_Employees, In_patient_hospital_beds, FTE_students, FTE_employees, FTE_distance_educated_students){
  Institution_Population <-(Residential_Employees + In_patient_hospital_beds + FTE_students + FTE_employees - 0.75 * FTE_distance_educated_students)
  return(Institution_Population)
}

#Example
calcInstitution_Population <- calcInstitution_Population(Residential_Employees = 100, 50, 10000, 2000, 5000)
print(Institution_Population)



###################################################################################################################################################################################################################################



#Institution - Mass All Food Sent to Composting ####
#' 
#' @param Weight_Composted total weight of food composted from university, will need to change depending on University
#' @param Weight_Loss UVA weight loss factor through pulper waster removal kg water / kg food
#' @param Mass_Sent_Composting total mass of food sent to composting in kg
#' 
#' @return Mass_Composted total amount of food composted in kg
#' @export 
#' 
#' @examples 
#'  
#'  
#'  
calcMass_AllFood_Composted <- function(){
  Weight_Composted <- 1000 
  Weight_Loss <- 0.25
  Mass_AllFood_Composted <- Weight_Composted * Weight_Loss
  
  return(Mass_AllFood_Composted)
}
Mass_AllFood_Composted <-calcMass_AllFood_Composted()
print(Mass_AllFood_Composted)
###################################################################################################################################################################################################################################



#Institution - Mass All Food Sent to Donating####
Mass_AllFood_Donated <- 5000 #mass of all food donated by university in kg
###################################################################################################################################################################################################################################



#Institution - Total Food Mass/Food Ordered for each Food Category####
#'
#' @param Food_Category Kind of food 
#' @param calcInstitution_Population Population value for Loyola
#'
#' @return Total_Food_Mass Estimated total food mass/food that was ordered for the food category at the institution in kg
#' @export
#'
#' @examples
#' Total_Food_Mass("Poultry", calcInstitution_Population)


calcTotal_Food_Mass <- function(Food_Category) {
  
  UVA_Population <- 34056   #constant, University of Virginia population 
  
  # Define the food masses as a named vector
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
  
  
  mass <- food_masses[Food_Category] * (calcInstitution_Population / UVA_Population)
  
  
  return(mass)
}

# Example usage:

Poultry_Total_Food_Mass <- calcTotal_Food_Mass("Poultry")
Bovine_Total_Food_Mass <- calcTotal_Food_Mass("Bovine")
Pigmeat_Total_Food_Mass <- calcTotal_Food_Mass("Pigmeat")
Milk_Total_Food_Mass <- calcTotal_Food_Mass("Milk")
Cheese_Total_Food_Mass <- calcTotal_Food_Mass("Cheese")
Eggs_Total_Food_Mass <- calcTotal_Food_Mass("Eggs")
Fish_Total_Food_Mass <- calcTotal_Food_Mass("Fish")
Cereals_Total_Food_Mass <- calcTotal_Food_Mass("Cereals")
Fruits_Total_Food_Mass <- calcTotal_Food_Mass("Fruits")
Pulses_Total_Food_Mass <- calcTotal_Food_Mass("Pulses")
Starchy_Roots_Total_Food_Mass <- calcTotal_Food_Mass("Starchy_Roots")
Vegetables_Total_Food_Mass <- calcTotal_Food_Mass("Vegetables")
Stimulants_Total_Food_Mass <- calcTotal_Food_Mass("Stimulants")
Oilcrops_Total_Food_Mass <- calcTotal_Food_Mass("Oilcrops")
Sugarcrops_Total_Food_Mass <- calcTotal_Food_Mass("Sugarcrops")
Nuts_Total_Food_Mass <- calcTotal_Food_Mass("Nuts")
Spices_Total_Food_Mass <- calcTotal_Food_Mass("Spices")
Beverages_Total_Food_Mass <- calcTotal_Food_Mass("Beverages")

#Print 
print(Poultry_Total_Food_Mass)
print(Bovine_Total_Food_Mass)
print(Pigmeat_Total_Food_Mass)
print(Milk_Total_Food_Mass)
print(Cheese_Total_Food_Mass)
print(Eggs_Total_Food_Mass)
print(Fish_Total_Food_Mass)
print(Cereals_Total_Food_Mass)
print(Fruits_Total_Food_Mass)
print(Pulses_Total_Food_Mass)
print(Starchy_Roots_Total_Food_Mass)
print(Vegetables_Total_Food_Mass)
print(Stimulants_Total_Food_Mass)
print(Oilcrops_Total_Food_Mass)
print(Sugarcrops_Total_Food_Mass)
print(Nuts_Total_Food_Mass)
print(Spices_Total_Food_Mass)
print(Beverages_Total_Food_Mass)
#####################################################################################################################################################################################################################################################



#Institution - Total Virtual N for each Food Category####
#'
#' @param Food_Category Kind of food 
#' @param calcInstitution_Population Population value for Loyola
#'
#' @return Estimated total virtual N for the food category at the institution in kg
#' @export
#'
#' @examples


calcTotal_Virtual_N <- function(Food_Category) {
  UVA_Population <- 34056 #constant, University of Virginia population 
  
  # Define the Virtual N values as a named vector
  Virtual_N <- c(
    Poultry = 20546,
    Bovine = 40257,
    Pigmeat = 16600,
    Milk = 9654,
    Cheese = 15710,
    Eggs = 11010,
    Fish = 1688,
    Cereals = 4338,
    Fruits = 2247,
    Pulses = 202,
    Starchy_Roots = 418,
    Vegetables = 2405,
    Stimulants = 35,
    Oilcrops = 306,
    Sugarcrops = 739,
    Nuts = 69,
    Spices = 481,
    Beverages = 340
  )
  
  
  virtual <- Virtual_N[Food_Category] * (calcInstitution_Population / UVA_Population)
  
  return(virtual)
}

# Example:
Poultry_Virtual_N <- calcTotal_Virtual_N("Poultry")
Bovine_Virtual_N <- calcTotal_Virtual_N("Bovine")
Pigmeat_Virtual_N <- calcTotal_Virtual_N("Pigmeat")
Milk_Virtual_N <- calcTotal_Virtual_N("Milk")
Cheese_Virtual_N <- calcTotal_Virtual_N("Cheese")
Eggs_Virtual_N <- calcTotal_Virtual_N("Eggs")
Fish_Virtual_N <- calcTotal_Virtual_N("Fish")
Cereals_Virtual_N <- calcTotal_Virtual_N("Cereals")
Fruits_Virtual_N <- calcTotal_Virtual_N("Fruits")
Pulses_Virtual_N <- calcTotal_Virtual_N("Pulses")
Starchy_Roots_Virtual_N <- calcTotal_Virtual_N("Starchy_Roots")
Vegetables_Virtual_N <- calcTotal_Virtual_N("Vegetables")
Stimulants_Virtual_N <- calcTotal_Virtual_N("Stimulants")
Oilcrops_Virtual_N <- calcTotal_Virtual_N("Oilcrops")
Sugarcrops_Virtual_N <- calcTotal_Virtual_N("Sugarcrops")
Nuts_Virtual_N <- calcTotal_Virtual_N("Nuts")
Spices_Virtual_N <- calcTotal_Virtual_N("Spices")
Beverages_Virtual_N <- calcTotal_Virtual_N("Beverages")

# Print all examples
print(Poultry_Virtual_N)
print(Bovine_Virtual_N)
print(Pigmeat_Virtual_N)
print(Milk_Virtual_N)
print(Cheese_Virtual_N)
print(Eggs_Virtual_N)
print(Fish_Virtual_N)
print(Cereals_Virtual_N)
print(Fruits_Virtual_N)
print(Pulses_Virtual_N)
print(Starchy_Roots_Virtual_N)
print(Vegetables_Virtual_N)
print(Stimulants_Virtual_N)
print(Oilcrops_Virtual_N)
print(Sugarcrops_Virtual_N)
print(Nuts_Virtual_N)
print(Spices_Virtual_N)
print(Beverages_Virtual_N)


#####################################################################################################################################################################################################################################################  



#Institution - Total Transport N for each Food Category####
#'
#' @param Food_Category Kind of food 
#' @param calcInstitution_Population Population value for Loyola
#'
#' @return Estimated Total Transport N for the food category at the institution in kg
#' @export
#'
#' @examples
#' 


calcTotal_Transport_N <- function(Food_Category, calcInstitution_Population) {
  UVA_Population <- 34056 #constant, University of Virginia population 
  
  Transport_N <- c(
    Poultry = 2,
    Bovine = 2,
    Pigmeat = 1,
    Milk = 3,
    Cheese = 1,
    Eggs = 1,
    Fish = 1,
    Cereals = 4,
    Fruits = 35,
    Pulses = 1,
    Starchy_Roots = 2,
    Vegetables = 2,
    Stimulants = 1,
    Oilcrops = 1,
    Sugarcrops = 1,
    Nuts = 0,
    Spices = 0,
    Beverages = 5
  )
  
  transport <- Transport_N[Food_Category] * (calcInstitution_Population / UVA_Population)
  
  return(transport)
}


#Result
Poultry_Transport_N <- calcTotal_Transport_N("Poultry", calcInstitution_Population)
Bovine_Transport_N <- calcTotal_Transport_N("Bovine", calcInstitution_Population)
Pigmeat_Transport_N <- calcTotal_Transport_N("Pigmeat", calcInstitution_Population)
Milk_Transport_N <- calcTotal_Transport_N("Milk", calcInstitution_Population)
Cheese_Transport_N <- calcTotal_Transport_N("Cheese", calcInstitution_Population)
Eggs_Transport_N <- calcTotal_Transport_N("Eggs", calcInstitution_Population)
Fish_Transport_N <- calcTotal_Transport_N("Fish", calcInstitution_Population)
Cereals_Transport_N <- calcTotal_Transport_N("Cereals", calcInstitution_Population)
Fruits_Transport_N <- calcTotal_Transport_N("Fruits", calcInstitution_Population)
Pulses_Transport_N <- calcTotal_Transport_N("Pulses", calcInstitution_Population)
Starchy_Roots_Transport_N <- calcTotal_Transport_N("Starchy_Roots", calcInstitution_Population)
Vegetables_Transport_N <- calcTotal_Transport_N("Vegetables", calcInstitution_Population)
Stimulants_Transport_N <- calcTotal_Transport_N("Stimulants", calcInstitution_Population)
Oilcrops_Transport_N <- calcTotal_Transport_N("Oilcrops", calcInstitution_Population)
Sugarcrops_Transport_N <- calcTotal_Transport_N("Sugarcrops", calcInstitution_Population)
Nuts_Transport_N <- calcTotal_Transport_N("Nuts", calcInstitution_Population)
Spices_Transport_N <- calcTotal_Transport_N("Spices", calcInstitution_Population)
Beverages_Transport_N <- calcTotal_Transport_N("Beverages", calcInstitution_Population)

# Print all values
print(Poultry_Transport_N)
print(Bovine_Transport_N)
print(Pigmeat_Transport_N)
print(Milk_Transport_N)
print(Cheese_Transport_N)
print(Eggs_Transport_N)
print(Fish_Transport_N)
print(Cereals_Transport_N)
print(Fruits_Transport_N)
print(Pulses_Transport_N)
print(Starchy_Roots_Transport_N)
print(Vegetables_Transport_N)
print(Stimulants_Transport_N)
print(Oilcrops_Transport_N)
print(Sugarcrops_Transport_N)
print(Nuts_Transport_N)
print(Spices_Transport_N)
print(Beverages_Transport_N)

#####################################################################################################################################################################################################################################################  



#Institution - Total N in Food for each Food Category ####
#'
#' @param Food_Category Kind of food 
#' @param calcInstitution_Population Population value for Loyola
#'
#' @return Total_Food_N Estimated Total N in food for the food category at the institution in kg
#' @export
#'
#' @examples
#' 
calcTotal_Food_N <- function(Food_Category) {
  UVA_Population <- 34056 # constant, University of Virginia population 
  
  # Define the Total N values for each food category as a named vector
  Food_N <- c(
    Poultry = 10273,
    Bovine = 8752,
    Pigmeat = 6148,
    Milk = 3713,
    Cheese = 6042,
    Eggs = 4078,
    Fish = 938,
    Cereals = 4759,
    Fruits = 548,
    Pulses = 673,
    Starchy_Roots = 837,
    Vegetables = 586,
    Stimulants = 9,
    Oilcrops = 1019,
    Sugarcrops = 110,
    Nuts = 230,
    Spices = 117,
    Beverages = 83
  )
  
  # Calculate the food N based on Institution Population
  total_n <- Food_N[Food_Category] * (calcInstitution_Population / UVA_Population)
  
  return(total_n)
}

#Results
Poultry_Total_Food_N <- calcTotal_Food_N("Poultry")
Bovine_Total_Food_N <- calcTotal_Food_N("Bovine")
Pigmeat_Total_Food_N <- calcTotal_Food_N("Pigmeat")
Milk_Total_Food_N <- calcTotal_Food_N("Milk")
Cheese_Total_Food_N <- calcTotal_Food_N("Cheese")
Eggs_Total_Food_N <- calcTotal_Food_N("Eggs")
Fish_Total_Food_N <- calcTotal_Food_N("Fish")
Cereals_Total_Food_N <- calcTotal_Food_N("Cereals")
Fruits_Total_Food_N <- calcTotal_Food_N("Fruits")
Pulses_Total_Food_N <- calcTotal_Food_N("Pulses")
Starchy_Roots_Total_Food_N <- calcTotal_Food_N("Starchy_Roots")
Vegetables_Total_Food_N <- calcTotal_Food_N("Vegetables")
Stimulants_Total_Food_N <- calcTotal_Food_N("Stimulants")
Oilcrops_Total_Food_N <- calcTotal_Food_N("Oilcrops")
Sugarcrops_Total_Food_N <- calcTotal_Food_N("Sugarcrops")
Nuts_Total_Food_N <- calcTotal_Food_N("Nuts")
Spices_Total_Food_N <- calcTotal_Food_N("Spices")
Beverages_Total_Food_N <- calcTotal_Food_N("Beverages")

# Print each result
print(Poultry_Total_Food_N)
print(Bovine_Total_Food_N)
print(Pigmeat_Total_Food_N)
print(Milk_Total_Food_N)
print(Cheese_Total_Food_N)
print(Eggs_Total_Food_N)
print(Fish_Total_Food_N)
print(Cereals_Total_Food_N)
print(Fruits_Total_Food_N)
print(Pulses_Total_Food_N)
print(Starchy_Roots_Total_Food_N)
print(Vegetables_Total_Food_N)
print(Stimulants_Total_Food_N)
print(Oilcrops_Total_Food_N)
print(Sugarcrops_Total_Food_N)
print(Nuts_Total_Food_N)
print(Spices_Total_Food_N)
print(Beverages_Total_Food_N)

#####################################################################################################################################################################################################################################################  



#Institution - Total N Released by production#### 
#'
#' @param Total_Transport_N total Transport N for the food category at the institution in kg
#' @param Total_Virtual_N total Transport N for the food category at the institution in kg
#'
#' @return 
#' @export
#'
#' @examples
#' 

calcTotal_N_Released <- function(Food_Category) {
  total_virtual_N <- c(
    Poultry = "Poultry_Virtual_N",
    Bovine = "Bovine_Virtual_N", 
    Pigmeat = "Pigmeat_Virtual_N",
    Milk = "Milk_Virtual_N", 
    Cheese = "Cheese_Virtual_N",
    Eggs = "Eggs_Virtual_N",
    Fish = "Fish_Virtual_N", 
    Cereals = "Cereals_Virtual_N", 
    Fruits = "Fruits_Virtual_N", 
    Pulses = "Pulses_Virtual_N", 
    Starchy_Roots = "Starchy_Roots_Virtual_N", 
    Vegetables = "Vegetables_Virtual_N",
    Stimulants = "Stimulants_Virtual_N", 
    Oilcrops = "Oilcrops_Virtual_N", 
    Sugarcrops = "Sugarcrops_Virtual_N",
    Nuts = "Nuts_Virtual_N" ,
    Spices = "Spices_Virtual_N",
    Beverages = "Beverages_Virtual_N"
  )
  total_transport_N <- c(
    Poultry = "Poultry_Transport_N",
    Bovine = "Bovine_Transport_N", 
    Pigmeat = "Pigmeat_Transport_N",
    Milk = "Milk_Transport_N", 
    Cheese = "Cheese_Transport_N",
    Eggs = "Eggs_Transport_N",
    Fish = "Fish_Transport_N", 
    Cereals = "Cereals_Transport_N", 
    Fruits = "Fruits_Transport_N", 
    Pulses = "Pulses_Transport_N", 
    Starchy_Roots = "Starchy_Roots_Transport_N", 
    Vegetables = "Vegetables_Transport_N",
    Stimulants = "Stimulants_Transport_N", 
    Oilcrops = "Oilcrops_Transport_N", 
    Sugarcrops = "Sugarcrops_Transport_N",
    Nuts = "Nuts_Transport_N" ,
    Spices = "Spices_Transport_N",
    Beverages = "Beverages_Transport_N"
  )
  
  total_n_released <- get(total_virtual_N[Food_Category])+get(total_transport_N[Food_Category])
  
  return(total_n_released)
}

#Results
Poultry_Total_N_Released <- calcTotal_N_Released("Poultry")
Bovine_Total_N_Released <- calcTotal_N_Released("Bovine")
Pigmeat_Total_N_Released <- calcTotal_N_Released("Pigmeat")
Milk_Total_N_Released <- calcTotal_N_Released("Milk")
Cheese_Total_N_Released <- calcTotal_N_Released("Cheese")
Eggs_Total_N_Released <- calcTotal_N_Released("Eggs")
Fish_Total_N_Released <- calcTotal_N_Released("Fish")
Cereals_Total_N_Released <- calcTotal_N_Released("Cereals")
Fruits_Total_N_Released <- calcTotal_N_Released("Fruits")
Pulses_Total_N_Released <- calcTotal_N_Released("Pulses")
Starchy_Roots_Total_N_Released <- calcTotal_N_Released("Starchy_Roots")
Vegetables_Total_N_Released <- calcTotal_N_Released("Vegetables")
Stimulants_Total_N_Released <- calcTotal_N_Released("Stimulants")
Oilcrops_Total_N_Released <- calcTotal_N_Released("Oilcrops")
Sugarcrops_Total_N_Released <- calcTotal_N_Released("Sugarcrops")
Nuts_Total_N_Released <- calcTotal_N_Released("Nuts")
Spices_Total_N_Released <- calcTotal_N_Released("Spices")
Beverages_Total_N_Released <- calcTotal_N_Released("Beverages")

# Print the results
print(Poultry_Total_N_Released)
print(Bovine_Total_N_Released)
print(Pigmeat_Total_N_Released)
print(Milk_Total_N_Released)
print(Cheese_Total_N_Released)
print(Eggs_Total_N_Released)
print(Fish_Total_N_Released)
print(Cereals_Total_N_Released)
print(Fruits_Total_N_Released)
print(Pulses_Total_N_Released)
print(Starchy_Roots_Total_N_Released)
print(Vegetables_Total_N_Released)
print(Stimulants_Total_N_Released)
print(Oilcrops_Total_N_Released)
print(Sugarcrops_Total_N_Released)
print(Nuts_Total_N_Released)
print(Spices_Total_N_Released)
print(Beverages_Total_N_Released)

#################################################################################################################################################################################### 



#Institution - N from production to Food ordered ratio#### 
#'
#' @param Total_N_Released amount of nitrogen released from food production for each food category
#' @param Total_Food_Mass mass of ordered food in kg for each food category
#'
#' @return N_Production_to_FoodOrdered ration of nitrogen released from food production to the total mass of food ordered
#' @export
#'
#' @examples
#' 

calcN_Production_to_FoodOrdered <- function(Food_Category){
  total_N_released <- c(
    Poultry = "Poultry_Total_N_Released",
    Bovine = "Bovine_Total_N_Released", 
    Pigmeat = "Pigmeat_Total_N_Released",
    Milk = "Milk_Total_N_Released", 
    Cheese = "Cheese_Total_N_Released",
    Eggs = "Eggs_Total_N_Released",
    Fish = "Fish_Total_N_Released", 
    Cereals = "Cereals_Total_N_Released", 
    Fruits = "Fruits_Total_N_Released", 
    Pulses = "Pulses_Total_N_Released", 
    Starchy_Roots = "Starchy_Roots_Total_N_Released", 
    Vegetables = "Vegetables_Total_N_Released",
    Stimulants = "Stimulants_Total_N_Released", 
    Oilcrops = "Oilcrops_Total_N_Released", 
    Sugarcrops = "Sugarcrops_Total_N_Released",
    Nuts = "Nuts_Total_N_Released",
    Spices = "Spices_Total_N_Released",
    Beverages = "Beverages_Total_N_Released"
  )
  total_foodmass <- c(
    Poultry = "Poultry_Total_Food_Mass",
    Bovine = "Bovine_Total_Food_Mass", 
    Pigmeat = "Pigmeat_Total_Food_Mass",
    Milk = "Milk_Total_Food_Mass", 
    Cheese = "Cheese_Total_Food_Mass",
    Eggs = "Eggs_Total_Food_Mass",
    Fish = "Fish_Total_Food_Mass", 
    Cereals = "Cereals_Total_Food_Mass", 
    Fruits = "Fruits_Total_Food_Mass", 
    Pulses = "Pulses_Total_Food_Mass", 
    Starchy_Roots = "Starchy_Roots_Total_Food_Mass", 
    Vegetables = "Vegetables_Total_Food_Mass",
    Stimulants = "Stimulants_Total_Food_Mass", 
    Oilcrops = "Oilcrops_Total_Food_Mass", 
    Sugarcrops = "Sugarcrops_Total_Food_Mass",
    Nuts = "Nuts_Total_Food_Mass",
    Spices = "Spices_Total_Food_Mass",
    Beverages = "Beverages_Total_Food_Mass"
  )
  N_Production_to_FoodOrdered <- get(total_N_released[Food_Category]) / get(total_foodmass[Food_Category])
  return(N_Production_to_FoodOrdered)
}

Poultry_N_Production_to_FoodOrdered <- calcN_Production_to_FoodOrdered("Poultry")
Bovine_N_Production_to_FoodOrdered <- calcN_Production_to_FoodOrdered("Bovine")
Pigmeat_N_Production_to_FoodOrdered <- calcN_Production_to_FoodOrdered("Pigmeat")
Milk_N_Production_to_FoodOrdered <- calcN_Production_to_FoodOrdered("Milk")
Cheese_N_Production_to_FoodOrdered <- calcN_Production_to_FoodOrdered("Cheese")
Eggs_N_Production_to_FoodOrdered <- calcN_Production_to_FoodOrdered("Eggs")
Fish_N_Production_to_FoodOrdered <- calcN_Production_to_FoodOrdered("Fish")
Cereals_N_Production_to_FoodOrdered <- calcN_Production_to_FoodOrdered("Cereals")
Fruits_N_Production_to_FoodOrdered <- calcN_Production_to_FoodOrdered("Fruits")
Pulses_N_Production_to_FoodOrdered <- calcN_Production_to_FoodOrdered("Pulses")
Starchy_Roots_N_Production_to_FoodOrdered <- calcN_Production_to_FoodOrdered("Starchy_Roots")
Vegetables_N_Production_to_FoodOrdered <- calcN_Production_to_FoodOrdered("Vegetables")
Stimulants_N_Production_to_FoodOrdered <- calcN_Production_to_FoodOrdered("Stimulants")
Oilcrops_N_Production_to_FoodOrdered <- calcN_Production_to_FoodOrdered("Oilcrops")
Sugarcrops_N_Production_to_FoodOrdered <- calcN_Production_to_FoodOrdered("Sugarcrops")
Nuts_N_Production_to_FoodOrdered <- calcN_Production_to_FoodOrdered("Nuts")
Spices_N_Production_to_FoodOrdered <- calcN_Production_to_FoodOrdered("Spices")
Beverages_N_Production_to_FoodOrdered <- calcN_Production_to_FoodOrdered("Beverages")

# Print
print(Poultry_N_Production_to_FoodOrdered)
print(Bovine_N_Production_to_FoodOrdered)
print(Pigmeat_N_Production_to_FoodOrdered)
print(Milk_N_Production_to_FoodOrdered)
print(Cheese_N_Production_to_FoodOrdered)
print(Eggs_N_Production_to_FoodOrdered)
print(Fish_N_Production_to_FoodOrdered)
print(Cereals_N_Production_to_FoodOrdered)
print(Fruits_N_Production_to_FoodOrdered)
print(Pulses_N_Production_to_FoodOrdered)
print(Starchy_Roots_N_Production_to_FoodOrdered)
print(Vegetables_N_Production_to_FoodOrdered)
print(Stimulants_N_Production_to_FoodOrdered)
print(Oilcrops_N_Production_to_FoodOrdered)
###########################################################################################



#Institution - Total FoodWaste by each Food Category and Total Foodwaste for all food categorys####
#' 
#' @param Total_Food_Mass total food mass for the food category in kg 
#' @param FAO_Factor FAO food waste factors in percantages 
#'  
#' @return Total_Food_Waste total food in kg that is associated with human waste 
#' @export 
#' 
#' @examples 
#'  

calc_totalfoodwaste <- function(Food_Category){ 
  
  total_food_masses <- c( 
    Poultry= "Poultry_Total_Food_Mass", 
    Bovine = "Bovine_Total_Food_Mass",  
    Pigmeat = "Pigmeat_Total_Food_Mass", 
    Milk = "Milk_Total_Food_Mass", 
    Cheese = "Cheese_Total_Food_Mass", 
    Eggs = "Eggs_Total_Food_Mass", 
    Fish = "Fish_Total_Food_Mass", 
    Cereals = "Cereals_Total_Food_Mass", 
    Fruits = "Fruits_Total_Food_Mass", 
    Pulses = "Pulses_Total_Food_Mass", 
    Starchy_Roots = "Starchy_Roots_Total_Food_Mass", 
    Vegetables = "Vegetables_Total_Food_Mass", 
    Stimulants= "Stimulants_Total_Food_Mass", 
    Oilcrops = "Oilcrops_Total_Food_Mass", 
    Sugarcrops = "Sugarcrops_Total_Food_Mass", 
    Nuts = "Nuts_Total_Food_Mass", 
    Spices = "Spices_Total_Food_Mass", 
    Beverages = "Beverages_Total_Food_Mass" 
  ) 
  
  FAO_foodfactors <-c( 
    Poultry = 0.15, 
    Bovine = 0.15, 
    Pigmeat = 0.15, 
    Milk = 0.15, 
    Cheese = 0.15, 
    Eggs = 0.15, 
    Fish = 0.15, 
    Cereals = 0.15, 
    Fruits = 0.15, 
    Pulses = 0.15, 
    Starchy_Roots = 0.15, 
    Vegetables = 0.15, 
    Stimulants = 0.15, 
    Oilcrops = 0.15, 
    Sugarcrops = 0.15, 
    Nuts = 0.15, 
    Spices = 0.15, 
    Beverages = 0.15 
  ) 
  
  Total_Foodwaste <- get(total_food_masses[Food_Category]) * FAO_foodfactors[Food_Category] 
  return(Total_Foodwaste) 
} 

Poultry_Total_Foodwaste <- calc_totalfoodwaste("Poultry") 
Bovine_Total_Foodwaste <- calc_totalfoodwaste("Bovine") 
Pigmeat_Total_Foodwaste <- calc_totalfoodwaste("Pigmeat") 
Milk_Total_Foodwaste <- calc_totalfoodwaste("Milk") 
Cheese_Total_Foodwaste <- calc_totalfoodwaste("Cheese") 
Eggs_Total_Foodwaste <- calc_totalfoodwaste("Eggs") 
Fish_Total_Foodwaste <- calc_totalfoodwaste("Fish") 
Cereals_Total_Foodwaste <- calc_totalfoodwaste("Cereals") 
Fruits_Total_Foodwaste <-calc_totalfoodwaste("Fruits") 
Pulses_Total_Foodwaste <- calc_totalfoodwaste("Pulses") 
StarchyRoots_Total_Foodwaste <- calc_totalfoodwaste("Starchy_Roots") 
Pulses_Total_Foodwaste <- calc_totalfoodwaste("Pulses") 
Vegetables_Total_Foodwaste <- calc_totalfoodwaste("Vegetables") 
Stimulants_Total_Foodwaste <- calc_totalfoodwaste("Stimulants") 
Oilcrops_Total_Foodwaste <- calc_totalfoodwaste("Oilcrops") 
Sugarcrops_Total_Foodwaste <- calc_totalfoodwaste("Sugarcrops") 
Nuts_Total_Foodwaste <- calc_totalfoodwaste("Nuts") 
Spices_Total_Foodwaste <- calc_totalfoodwaste("Spices") 
Beverages_Total_Foodwaste <- calc_totalfoodwaste("Beverages") 


print(Poultry_Total_Foodwaste) 
print(Bovine_Total_Foodwaste)  
print(Pigmeat_Total_Foodwaste) 
print(Milk_Total_Foodwaste)  
print(Cheese_Total_Foodwaste) 
print(Eggs_Total_Foodwaste) 
print(Fish_Total_Foodwaste) 
print(Cereals_Total_Foodwaste) 
print(Fruits_Total_Foodwaste) 
print(Pulses_Total_Foodwaste) 
print(StarchyRoots_Total_Foodwaste)  
print(Pulses_Total_Foodwaste) 
print(Vegetables_Total_Foodwaste) 
print(Stimulants_Total_Foodwaste)  
print(Oilcrops_Total_Foodwaste) 
print(Sugarcrops_Total_Foodwaste) 
print(Nuts_Total_Foodwaste) 
print(Spices_Total_Foodwaste) 
print(Beverages_Total_Foodwaste) 

Total_Foodwaste_AllFood <- sum(Poultry_Total_Foodwaste , Bovine_Total_Foodwaste , Pigmeat_Total_Foodwaste , Milk_Total_Foodwaste , 
                               Cheese_Total_Foodwaste , Eggs_Total_Foodwaste , Fish_Total_Foodwaste , Cereals_Total_Foodwaste ,
                               Fruits_Total_Foodwaste , Pulses_Total_Foodwaste , StarchyRoots_Total_Foodwaste , Pulses_Total_Foodwaste , 
                               Vegetables_Total_Foodwaste , Stimulants_Total_Foodwaste , Oilcrops_Total_Foodwaste , Sugarcrops_Total_Foodwaste ,
                               Nuts_Total_Foodwaste , Spices_Total_Foodwaste , Beverages_Total_Foodwaste)
print(Total_Foodwaste_AllFood)
###########################################################################################



#Institution - Total Food Waste N for each Food Category ####
#' 
#' @param Total_Food_N total food mass for the food category in kg 
#' @param FAO_Factor FAO food waste factors in percentages (converted to decimal form for function)
#' 
#' @return FoodWaste_N total nitrogen in food ordered in kg 
#' @export 
#' 
#' @examples 
#'  



calc_Foodwaste_N <- function(Food_Category){ 
  
  total_food_N <- c( 
    Poultry= "Poultry_Total_Food_N", 
    Bovine = "Bovine_Total_Food_N",  
    Pigmeat = "Pigmeat_Total_Food_N", 
    Milk = "Milk_Total_Food_N", 
    Cheese = "Cheese_Total_Food_N", 
    Eggs = "Eggs_Total_Food_N", 
    Fish = "Fish_Total_Food_N", 
    Cereals = "Cereals_Total_Food_N", 
    Fruits = "Fruits_Total_Food_N", 
    Pulses = "Pulses_Total_Food_N", 
    Starchy_Roots = "Starchy_Roots_Total_Food_N", 
    Vegetables = "Vegetables_Total_Food_N", 
    Stimulants= "Stimulants_Total_Food_N", 
    Oilcrops = "Oilcrops_Total_Food_N", 
    Sugarcrops = "Sugarcrops_Total_Food_N", 
    Nuts = "Nuts_Total_Food_N", 
    Spices = "Spices_Total_Food_N", 
    Beverages = "Beverages_Total_Food_N" 
  ) 
  
  FAO_foodfactors <-c( 
    Poultry = 0.15, 
    Bovine = 0.15, 
    Pigmeat = 0.15, 
    Milk = 0.15, 
    Cheese = 0.15, 
    Eggs = 0.15, 
    Fish = 0.15, 
    Cereals = 0.15, 
    Fruits = 0.15, 
    Pulses = 0.15, 
    Starchy_Roots = 0.15, 
    Vegetables = 0.15, 
    Stimulants = 0.15, 
    Oilcrops = 0.15, 
    Sugarcrops = 0.15, 
    Nuts = 0.15, 
    Spices = 0.15, 
    Beverages = 0.15 
    
  ) 
  
  Foodwaste_N <- get(total_food_N[Food_Category]) * FAO_foodfactors[Food_Category] 
  
  return(Foodwaste_N) 
  
} 

Poultry_Foodwaste_N <- calc_Foodwaste_N("Poultry") 
Bovine_Foodwaste_N <- calc_Foodwaste_N("Bovine") 
Pigmeat_Foodwaste_N <- calc_Foodwaste_N("Pigmeat") 
Milk_Foodwaste_N <- calc_Foodwaste_N("Milk") 
Cheese_Foodwaste_N <- calc_Foodwaste_N("Cheese") 
Eggs_Foodwaste_N <- calc_Foodwaste_N("Eggs") 
Fish_Foodwaste_N <- calc_Foodwaste_N("Fish") 
Cereals_Foodwaste_N <- calc_Foodwaste_N("Cereals") 
Fruits_Foodwaste_N <-calc_Foodwaste_N("Fruits") 
Pulses_Foodwaste_N <- calc_Foodwaste_N("Pulses") 
StarchyRoots_Foodwaste_N <- calc_Foodwaste_N("Starchy_Roots") 
Pulses_Foodwaste_N <- calc_Foodwaste_N("Pulses") 
Vegetables_Foodwaste_N <- calc_Foodwaste_N("Vegetables") 
Stimulants_Foodwaste_N <- calc_Foodwaste_N("Stimulants") 
Oilcrops_Foodwaste_N <- calc_Foodwaste_N("Oilcrops") 
Sugarcrops_Foodwaste_N <- calc_Foodwaste_N("Sugarcrops") 
Nuts_Foodwaste_N <- calc_Foodwaste_N("Nuts") 
Spices_Foodwaste_N <- calc_Foodwaste_N("Spices") 
Beverages_Foodwaste_N <- calc_Foodwaste_N("Beverages") 




print(Poultry_Foodwaste_N) 
print(Bovine_Foodwaste_N)  
print(Pigmeat_Foodwaste_N) 
print(Milk_Foodwaste_N)  
print(Cheese_Foodwaste_N) 
print(Eggs_Foodwaste_N) 
print(Fish_Foodwaste_N) 
print(Cereals_Foodwaste_N) 
print(Fruits_Foodwaste_N) 
print(Pulses_Foodwaste_N) 
print(StarchyRoots_Foodwaste_N)  
print(Pulses_Foodwaste_N) 
print(Vegetables_Foodwaste_N) 
print(Stimulants_Foodwaste_N)  
print(Oilcrops_Foodwaste_N) 
print(Sugarcrops_Foodwaste_N) 
print(Nuts_Foodwaste_N) 
print(Spices_Foodwaste_N) 
print(Beverages_Foodwaste_N) 
###########################################################################################



#Institution - Percent of N in Foodwaste for each Food Category ####
#' 
#' @param Total_Foodwaste_N total Nitrogen in food waste for each food type
#' @param Total_Foodwaste total food waste in kg for each food type
#' 
#' @return percent_N_Food percentage of nitrogen in each food type
#' @export 
#' 
#' @examples 

calc_percent_N_Foodwaste <- function(Food_Category){
  total_foodwaste_n <- c(
    Poultry = "Poultry_Foodwaste_N", 
    Bovine = "Bovine_Foodwaste_N",
    Pigmeat = "Pigmeat_Foodwaste_N",
    Milk = "Milk_Foodwaste_N", 
    Cheese = "Cheese_Foodwaste_N",  
    Eggs = "Eggs_Foodwaste_N",
    Fish = "Fish_Foodwaste_N",  
    Cereals = "Cereals_Foodwaste_N", 
    Fruits = "Fruits_Foodwaste_N",
    Pulses = "Pulses_Foodwaste_N",
    Starchy_Roots = "StarchyRoots_Foodwaste_N", 
    Pulses = "Pulses_Foodwaste_N", 
    Vegetables = "Vegetables_Foodwaste_N",  
    Stimulants = "Stimulants_Foodwaste_N", 
    Oilcrops = "Oilcrops_Foodwaste_N", 
    Sugarcrops = "Sugarcrops_Foodwaste_N",  
    Nuts = "Nuts_Foodwaste_N", 
    Spices = "Spices_Foodwaste_N", 
    Beverages = "Beverages_Foodwaste_N"
  )
  
  
  total_foodwaste <-c(
    Poultry = "Poultry_Total_Foodwaste",
    Bovine = "Bovine_Total_Foodwaste",
    Pigmeat = "Pigmeat_Total_Foodwaste",
    Milk = "Milk_Total_Foodwaste",
    Cheese = "Cheese_Total_Foodwaste",
    Eggs = "Eggs_Total_Foodwaste",
    Fish = "Fish_Total_Foodwaste",
    Cereals = "Cereals_Total_Foodwaste", 
    Fruits = "Fruits_Total_Foodwaste",
    Pulses = "Pulses_Total_Foodwaste",
    Starchy_Roots = "StarchyRoots_Total_Foodwaste",  
    Pulses = "Pulses_Total_Foodwaste", 
    Vegetables = "Vegetables_Total_Foodwaste", 
    Stimulants = "Stimulants_Total_Foodwaste",
    Oilcrops = "Oilcrops_Total_Foodwaste", 
    Sugarcrops = "Sugarcrops_Total_Foodwaste",  
    Nuts = "Nuts_Total_Foodwaste",
    Spices = "Spices_Total_Foodwaste",
    Beverages = "Beverages_Total_Foodwaste"
  )
  
  percent_N_Food <- get(total_foodwaste_n[Food_Category])/ get(total_foodwaste[Food_Category])
  
  return(percent_N_Food)
}


Poultry_percent_N_Foodwaste <- calc_percent_N_Foodwaste("Poultry") 
Bovine_percent_N_Foodwaste <- calc_percent_N_Foodwaste("Bovine") 
Pigmeat_percent_N_Foodwaste <- calc_percent_N_Foodwaste("Pigmeat") 
Milk_percent_N_Foodwaste <- calc_percent_N_Foodwaste("Milk") 
Cheese_percent_N_Foodwaste <- calc_percent_N_Foodwaste("Cheese") 
Eggs_percent_N_Foodwaste <- calc_percent_N_Foodwaste("Eggs") 
Fish_percent_N_Foodwaste <- calc_percent_N_Foodwaste("Fish") 
Cereals_percent_N_Foodwaste <- calc_percent_N_Foodwaste("Cereals") 
Fruits_percent_N_Foodwaste <-calc_percent_N_Foodwaste("Fruits") 
Pulses_percent_N_Foodwaste <- calc_percent_N_Foodwaste("Pulses") 
StarchyRoots_percent_N_Foodwaste <- calc_percent_N_Foodwaste("Starchy_Roots") 
Pulses_percent_N_Foodwaste <- calc_percent_N_Foodwaste("Pulses") 
Vegetables_percent_N_Foodwaste <- calc_percent_N_Foodwaste("Vegetables") 
Stimulants_percent_N_Foodwaste <- calc_percent_N_Foodwaste("Stimulants") 
Oilcrops_percent_N_Foodwaste <- calc_percent_N_Foodwaste("Oilcrops") 
Sugarcrops_percent_N_Foodwaste <- calc_percent_N_Foodwaste("Sugarcrops") 
Nuts_percent_N_Foodwaste <- calc_percent_N_Foodwaste("Nuts") 
Spices_percent_N_Foodwaste <- calc_percent_N_Foodwaste("Spices") 
Beverages_percent_N_Foodwaste <- calc_percent_N_Foodwaste("Beverages") 




print(Poultry_percent_N_Foodwaste)
print(Bovine_percent_N_Foodwaste)  
print(Pigmeat_percent_N_Foodwaste) 
print(Milk_percent_N_Foodwaste)  
print(Cheese_percent_N_Foodwaste) 
print(Eggs_percent_N_Foodwaste) 
print(Fish_percent_N_Foodwaste) 
print(Cereals_percent_N_Foodwaste) 
print(Fruits_percent_N_Foodwaste) 
print(Pulses_percent_N_Foodwaste) 
print(StarchyRoots_percent_N_Foodwaste)  
print(Vegetables_percent_N_Foodwaste) 
print(Stimulants_percent_N_Foodwaste)  
print(Oilcrops_percent_N_Foodwaste) 
print(Sugarcrops_percent_N_Foodwaste) 
print(Nuts_percent_N_Foodwaste) 
print(Spices_percent_N_Foodwaste) 
print(Beverages_percent_N_Foodwaste) 

###########################################################################################


#Institution - N in Food consumed ####
#' 
#' @param Total_Food_N Estimated Total N in food for the food category at the institution in kg
#' @param Total_Foodwaste_N Estimated Total N in foodwaste for the food category at the institution in kg
#' 
#' @return N_Foodconsumed amount of nitrogen in food consumed
#' @export 
#' 
#' @examples 
#' 

calc_N_FoodConsumed <- function(Food_Category){
  total_food_N <- c( 
    Poultry= "Poultry_Total_Food_N", 
    Bovine = "Bovine_Total_Food_N",  
    Pigmeat = "Pigmeat_Total_Food_N", 
    Milk = "Milk_Total_Food_N", 
    Cheese = "Cheese_Total_Food_N", 
    Eggs = "Eggs_Total_Food_N", 
    Fish = "Fish_Total_Food_N", 
    Cereals = "Cereals_Total_Food_N", 
    Fruits = "Fruits_Total_Food_N", 
    Pulses = "Pulses_Total_Food_N", 
    Starchy_Roots = "Starchy_Roots_Total_Food_N", 
    Vegetables = "Vegetables_Total_Food_N", 
    Stimulants= "Stimulants_Total_Food_N", 
    Oilcrops = "Oilcrops_Total_Food_N", 
    Sugarcrops = "Sugarcrops_Total_Food_N", 
    Nuts = "Nuts_Total_Food_N", 
    Spices = "Spices_Total_Food_N", 
    Beverages = "Beverages_Total_Food_N" 
  )
  
  foodwaste_N <- c( 
    Poultry = "Poultry_Foodwaste_N", 
    Bovine = "Bovine_Foodwaste_N",  
    Pigmeat = "Pigmeat_Foodwaste_N", 
    Milk = "Milk_Foodwaste_N", 
    Cheese = "Cheese_Foodwaste_N", 
    Eggs = "Eggs_Foodwaste_N", 
    Fish = "Fish_Foodwaste_N", 
    Cereals = "Cereals_Foodwaste_N", 
    Fruits = "Fruits_Foodwaste_N", 
    Pulses = "Pulses_Foodwaste_N", 
    Starchy_Roots = "StarchyRoots_Foodwaste_N", 
    Vegetables = "Vegetables_Foodwaste_N", 
    Stimulants = "Stimulants_Foodwaste_N", 
    Oilcrops = "Oilcrops_Foodwaste_N", 
    Sugarcrops = "Sugarcrops_Foodwaste_N", 
    Nuts = "Nuts_Foodwaste_N", 
    Spices = "Spices_Foodwaste_N", 
    Beverages = "Beverages_Foodwaste_N"
  )
  N_FoodConsumed <- get(total_food_N[Food_Category]) - get(foodwaste_N[Food_Category])
  
  return(N_FoodConsumed)
}

Poultry_N_FoodConsumed <- calc_N_FoodConsumed("Poultry") 
Bovine_N_FoodConsumed <- calc_N_FoodConsumed("Bovine") 
Pigmeat_N_FoodConsumed <- calc_N_FoodConsumed("Pigmeat") 
Milk_N_FoodConsumed <- calc_N_FoodConsumed("Milk") 
Cheese_N_FoodConsumed <- calc_N_FoodConsumed("Cheese") 
Eggs_N_FoodConsumed <- calc_N_FoodConsumed("Eggs") 
Fish_N_FoodConsumed <- calc_N_FoodConsumed("Fish") 
Cereals_N_FoodConsumed <- calc_N_FoodConsumed("Cereals") 
Fruits_N_FoodConsumed <- calc_N_FoodConsumed("Fruits") 
Pulses_N_FoodConsumed <- calc_N_FoodConsumed("Pulses") 
StarchyRoots_N_FoodConsumed <- calc_N_FoodConsumed("Starchy_Roots") 
Vegetables_N_FoodConsumed <- calc_N_FoodConsumed("Vegetables") 
Stimulants_N_FoodConsumed <- calc_N_FoodConsumed("Stimulants") 
Oilcrops_N_FoodConsumed <- calc_N_FoodConsumed("Oilcrops") 
Sugarcrops_N_FoodConsumed <- calc_N_FoodConsumed("Sugarcrops") 
Nuts_N_FoodConsumed <- calc_N_FoodConsumed("Nuts") 
Spices_N_FoodConsumed <- calc_N_FoodConsumed("Spices") 
Beverages_N_FoodConsumed <- calc_N_FoodConsumed("Beverages") 


print(Poultry_N_FoodConsumed)
print(Bovine_N_FoodConsumed)  
print(Pigmeat_N_FoodConsumed) 
print(Milk_N_FoodConsumed)  
print(Cheese_N_FoodConsumed) 
print(Eggs_N_FoodConsumed) 
print(Fish_N_FoodConsumed) 
print(Cereals_N_FoodConsumed) 
print(Fruits_N_FoodConsumed) 
print(Pulses_N_FoodConsumed) 
print(StarchyRoots_N_FoodConsumed)  
print(Vegetables_N_FoodConsumed) 
print(Stimulants_N_FoodConsumed)  
print(Oilcrops_N_FoodConsumed) 
print(Sugarcrops_N_FoodConsumed) 
print(Nuts_N_FoodConsumed) 
print(Spices_N_FoodConsumed) 
print(Beverages_N_FoodConsumed)
###########################################################################################



#Institution - Mass Composted Food by each Food Category####
#' @param Total_Foodwaste_AllFood total mass of all food categorys
#' @param Total_Foodwaste total food waste for each food category in kg
#' @param Mass_Sent_Composting total mass of food sent to composting in kg
#' 
#' @return Mass_Composted total amount of food composted in kg
#' @export 
#' 
#' @examples 
#'  

calcMass_Composted <- function(Food_Category){
  total_foodwaste <- c(
    Poultry = "Poultry_Total_Foodwaste", 
    Bovine = "Bovine_Total_Foodwaste",
    Pigmeat = "Pigmeat_Total_Foodwaste",
    Milk = "Milk_Total_Foodwaste", 
    Cheese = "Cheese_Total_Foodwaste",  
    Eggs = "Eggs_Total_Foodwaste",
    Fish = "Fish_Total_Foodwaste",  
    Cereals = "Cereals_Total_Foodwaste", 
    Fruits = "Fruits_Total_Foodwaste",
    Pulses = "Pulses_Total_Foodwaste",
    Starchy_Roots = "StarchyRoots_Total_Foodwaste", 
    Pulses = "Pulses_Total_Foodwaste", 
    Vegetables = "Vegetables_Total_Foodwaste",  
    Stimulants = "Stimulants_Total_Foodwaste", 
    Oilcrops = "Oilcrops_Total_Foodwaste", 
    Sugarcrops = "Sugarcrops_Total_Foodwaste",  
    Nuts = "Nuts_Total_Foodwaste", 
    Spices = "Spices_Total_Foodwaste", 
    Beverages = "Beverages_Total_Foodwaste"
  )
  Mass_Composted <- get(total_foodwaste[Food_Category]) / (Total_Foodwaste_AllFood * Mass_AllFood_Composted)
  
  return(Mass_Composted)
}

Poultry_Mass_Composted <- calcMass_Composted("Poultry") 
Bovine_Mass_Composted <- calcMass_Composted("Bovine") 
Pigmeat_Mass_Composted <- calcMass_Composted("Pigmeat") 
Milk_Mass_Composted <- calcMass_Composted("Milk") 
Cheese_Mass_Composted <- calcMass_Composted("Cheese") 
Eggs_Mass_Composted <- calcMass_Composted("Eggs") 
Fish_Mass_Composted <- calcMass_Composted("Fish") 
Cereals_Mass_Composted <- calcMass_Composted("Cereals") 
Fruits_Mass_Composted <- calcMass_Composted("Fruits") 
Pulses_Mass_Composted <- calcMass_Composted("Pulses") 
StarchyRoots_Mass_Composted <- calcMass_Composted("Starchy_Roots") 
Vegetables_Mass_Composted <- calcMass_Composted("Vegetables") 
Stimulants_Mass_Composted <- calcMass_Composted("Stimulants") 
Oilcrops_Mass_Composted <- calcMass_Composted("Oilcrops") 
Sugarcrops_Mass_Composted <- calcMass_Composted("Sugarcrops") 
Nuts_Mass_Composted <- calcMass_Composted("Nuts") 
Spices_Mass_Composted <- calcMass_Composted("Spices") 
Beverages_Mass_Composted <- calcMass_Composted("Beverages") 

print(Poultry_Mass_Composted) 
print(Bovine_Mass_Composted)  
print(Pigmeat_Mass_Composted) 
print(Milk_Mass_Composted)  
print(Cheese_Mass_Composted) 
print(Eggs_Mass_Composted) 
print(Fish_Mass_Composted) 
print(Cereals_Mass_Composted) 
print(Fruits_Mass_Composted) 
print(Pulses_Mass_Composted) 
print(StarchyRoots_Mass_Composted)  
print(Vegetables_Mass_Composted) 
print(Stimulants_Mass_Composted)  
print(Oilcrops_Mass_Composted) 
print(Sugarcrops_Mass_Composted) 
print(Nuts_Mass_Composted) 
print(Spices_Mass_Composted) 
print(Beverages_Mass_Composted)
###########################################################################################



#Institution - N in Composted Food by each Food Category####
#' @param Total_Foodwaste_AllFood total mass of all food categorys
#' @param Total_Foodwaste total food waste for each food category in kg
#' 
#' 
#' @return N_Composted total amount of food composted in kg
#' @export 
#' 
#' @examples 
#'  

calc_N_Composted <- function(Food_Category){
  percent_N_foodwaste <- c(
    Poultry = "Poultry_percent_N_Foodwaste", 
    Bovine = "Bovine_percent_N_Foodwaste",
    Pigmeat = "Pigmeat_percent_N_Foodwaste",
    Milk = "Milk_percent_N_Foodwaste", 
    Cheese = "Cheese_percent_N_Foodwaste",  
    Eggs = "Eggs_percent_N_Foodwaste",
    Fish = "Fish_percent_N_Foodwaste",  
    Cereals = "Cereals_percent_N_Foodwaste", 
    Fruits = "Fruits_percent_N_Foodwaste",
    Pulses = "Pulses_percent_N_Foodwaste",
    Starchy_Roots = "StarchyRoots_percent_N_Foodwaste", 
    Vegetables = "Vegetables_percent_N_Foodwaste",  
    Stimulants = "Stimulants_percent_N_Foodwaste", 
    Oilcrops = "Oilcrops_percent_N_Foodwaste", 
    Sugarcrops = "Sugarcrops_percent_N_Foodwaste",  
    Nuts = "Nuts_percent_N_Foodwaste", 
    Spices = "Spices_percent_N_Foodwaste", 
    Beverages = "Beverages_percent_N_Foodwaste"
  )
  mass_composted <- c(
    Poultry = "Poultry_Mass_Composted", 
    Bovine = "Bovine_Mass_Composted",
    Pigmeat = "Pigmeat_Mass_Composted",
    Milk = "Milk_Mass_Composted", 
    Cheese = "Cheese_Mass_Composted",  
    Eggs = "Eggs_Mass_Composted",
    Fish = "Fish_Mass_Composted",  
    Cereals = "Cereals_Mass_Composted", 
    Fruits = "Fruits_Mass_Composted",
    Pulses = "Pulses_Mass_Composted",
    Starchy_Roots = "StarchyRoots_Mass_Composted", 
    Vegetables = "Vegetables_Mass_Composted",  
    Stimulants = "Stimulants_Mass_Composted", 
    Oilcrops = "Oilcrops_Mass_Composted", 
    Sugarcrops = "Sugarcrops_Mass_Composted",  
    Nuts = "Nuts_Mass_Composted", 
    Spices = "Spices_Mass_Composted", 
    Beverages = "Beverages_Mass_Composted"
  )
  N_Composted <- get(percent_N_foodwaste[Food_Category]) * get(mass_composted[Food_Category])
  
  return(N_Composted)
}
Poultry_N_Composted <- calc_N_Composted("Poultry")
Bovine_N_Composted <- calc_N_Composted("Bovine") 
Pigmeat_N_Composted <- calc_N_Composted("Pigmeat") 
Milk_N_Composted <- calc_N_Composted("Milk") 
Cheese_N_Composted <- calc_N_Composted("Cheese") 
Eggs_N_Composted <- calc_N_Composted("Eggs") 
Fish_N_Composted <- calc_N_Composted("Fish") 
Cereals_N_Composted <- calc_N_Composted("Cereals") 
Fruits_N_Composted <- calc_N_Composted("Fruits") 
Pulses_N_Composted <- calc_N_Composted("Pulses") 
StarchyRoots_N_Composted <- calc_N_Composted("Starchy_Roots") 
Vegetables_N_Composted <- calc_N_Composted("Vegetables") 
Stimulants_N_Composted <- calc_N_Composted("Stimulants") 
Oilcrops_N_Composted <- calc_N_Composted("Oilcrops")
Sugarcrops_N_Composted <- calc_N_Composted("Sugarcrops")
Nuts_N_Composted <- calc_N_Composted("Nuts")
Spices_N_Composted <- calc_N_Composted("Spices")
Beverages_N_Composted <- calc_N_Composted("Beverages")

print(Poultry_N_Composted) 
print(Bovine_N_Composted)  
print(Pigmeat_N_Composted) 
print(Milk_N_Composted)  
print(Cheese_N_Composted) 
print(Eggs_N_Composted) 
print(Fish_N_Composted) 
print(Cereals_N_Composted) 
print(Fruits_N_Composted) 
print(Pulses_N_Composted) 
print(StarchyRoots_N_Composted)  
print(Vegetables_N_Composted) 
print(Stimulants_N_Composted)  
print(Oilcrops_N_Composted) 
print(Sugarcrops_N_Composted) 
print(Nuts_N_Composted) 
print(Spices_N_Composted) 
print(Beverages_N_Composted) 
###########################################################################################



#Institution - Mass of Donated Food by each Food Category####
#' @param Total_Foodwaste_AllFood total mass of all food categorys
#' @param Total_Foodwaste total food waste for each food category in kg
#' @param Mass_AllFood_Donated mass of food sent to donatation in kg
#' 
#' @return Mass_Donated total amount of food donated for each food category in kg
#' @export 
#' 
#' @examples 
#'

calcMass_Donated <- function(Food_Category){
  
  total_foodwaste <- c(
    Poultry = "Poultry_Total_Foodwaste", 
    Bovine = "Bovine_Total_Foodwaste",
    Pigmeat = "Pigmeat_Total_Foodwaste",
    Milk = "Milk_Total_Foodwaste", 
    Cheese = "Cheese_Total_Foodwaste",  
    Eggs = "Eggs_Total_Foodwaste",
    Fish = "Fish_Total_Foodwaste",  
    Cereals = "Cereals_Total_Foodwaste", 
    Fruits = "Fruits_Total_Foodwaste",
    Pulses = "Pulses_Total_Foodwaste",
    Starchy_Roots = "StarchyRoots_Total_Foodwaste", 
    Pulses = "Pulses_Total_Foodwaste", 
    Vegetables = "Vegetables_Total_Foodwaste",  
    Stimulants = "Stimulants_Total_Foodwaste", 
    Oilcrops = "Oilcrops_Total_Foodwaste", 
    Sugarcrops = "Sugarcrops_Total_Foodwaste",  
    Nuts = "Nuts_Total_Foodwaste", 
    Spices = "Spices_Total_Foodwaste", 
    Beverages = "Beverages_Total_Foodwaste"
  )
  Mass_Donated <- Mass_AllFood_Donated * (get(total_foodwaste[Food_Category])/Total_Foodwaste_AllFood)
  
  return(Mass_Donated)
}

Poultry_Mass_Donated <- calcMass_Donated("Poultry") 
Bovine_Mass_Donated <- calcMass_Donated("Bovine") 
Pigmeat_Mass_Donated <- calcMass_Donated("Pigmeat") 
Milk_Mass_Donated <- calcMass_Donated("Milk") 
Cheese_Mass_Donated <- calcMass_Donated("Cheese") 
Eggs_Mass_Donated <- calcMass_Donated("Eggs") 
Fish_Mass_Donated <- calcMass_Donated("Fish") 
Cereals_Mass_Donated <- calcMass_Donated("Cereals") 
Fruits_Mass_Donated <- calcMass_Donated("Fruits") 
Pulses_Mass_Donated <- calcMass_Donated("Pulses") 
StarchyRoots_Mass_Donated <- calcMass_Donated("Starchy_Roots") 
Vegetables_Mass_Donated <- calcMass_Donated("Vegetables") 
Stimulants_Mass_Donated <- calcMass_Donated("Stimulants") 
Oilcrops_Mass_Donated <- calcMass_Donated("Oilcrops") 
Sugarcrops_Mass_Donated <- calcMass_Donated("Sugarcrops") 
Nuts_Mass_Donated <- calcMass_Donated("Nuts") 
Spices_Mass_Donated <- calcMass_Donated("Spices") 
Beverages_Mass_Donated <- calcMass_Donated("Beverages") 

print(Poultry_Mass_Donated) 
print(Bovine_Mass_Donated)  
print(Pigmeat_Mass_Donated) 
print(Milk_Mass_Donated)  
print(Cheese_Mass_Donated) 
print(Eggs_Mass_Donated) 
print(Fish_Mass_Donated) 
print(Cereals_Mass_Donated) 
print(Fruits_Mass_Donated) 
print(Pulses_Mass_Donated) 
print(StarchyRoots_Mass_Donated)  
print(Vegetables_Mass_Donated) 
print(Stimulants_Mass_Donated)  
print(Oilcrops_Mass_Donated) 
print(Sugarcrops_Mass_Donated) 
print(Nuts_Mass_Donated) 
print(Spices_Mass_Donated) 
print(Beverages_Mass_Donated) 
###########################################################################################



#Institution - N in Donated Food by each Food Category####
#' @param Mass_AllFood_Donated total mass of all food donated in kg
#' @param Total_Foodwaste total food waste for each food category in kg
#' @param Total_FoodWaste_AllFood total food waste of all food categorys kg
#' 
#' @return N_Donated amount of nitrogen in donated food for each food category in kg
#' @export 
#' 
#' @examples 
#'

calc_N_Donated <- function(Food_Category){
  
  total_foodwaste <- c(
    Poultry = "Poultry_Total_Foodwaste", 
    Bovine = "Bovine_Total_Foodwaste",
    Pigmeat = "Pigmeat_Total_Foodwaste",
    Milk = "Milk_Total_Foodwaste", 
    Cheese = "Cheese_Total_Foodwaste",  
    Eggs = "Eggs_Total_Foodwaste",
    Fish = "Fish_Total_Foodwaste",  
    Cereals = "Cereals_Total_Foodwaste", 
    Fruits = "Fruits_Total_Foodwaste",
    Pulses = "Pulses_Total_Foodwaste",
    Starchy_Roots = "StarchyRoots_Total_Foodwaste", 
    Pulses = "Pulses_Total_Foodwaste", 
    Vegetables = "Vegetables_Total_Foodwaste",  
    Stimulants = "Stimulants_Total_Foodwaste", 
    Oilcrops = "Oilcrops_Total_Foodwaste", 
    Sugarcrops = "Sugarcrops_Total_Foodwaste",  
    Nuts = "Nuts_Total_Foodwaste", 
    Spices = "Spices_Total_Foodwaste", 
    Beverages = "Beverages_Total_Foodwaste"
  )  
  
  N_Donated <- Mass_AllFood_Donated * (get(total_foodwaste[Food_Category]) / Total_Foodwaste_AllFood)
  
  return(N_Donated)
}

Poultry_N_Donated <- calc_N_Donated("Poultry") 
Bovine_N_Donated <- calc_N_Donated("Bovine") 
Pigmeat_N_Donated <- calc_N_Donated("Pigmeat") 
Milk_N_Donated <- calc_N_Donated("Milk") 
Cheese_N_Donated <- calc_N_Donated("Cheese") 
Eggs_N_Donated <- calc_N_Donated("Eggs") 
Fish_N_Donated <- calc_N_Donated("Fish") 
Cereals_N_Donated <- calc_N_Donated("Cereals") 
Fruits_N_Donated <- calc_N_Donated("Fruits") 
Pulses_N_Donated <- calc_N_Donated("Pulses") 
StarchyRoots_N_Donated <- calc_N_Donated("Starchy_Roots") 
Vegetables_N_Donated <- calc_N_Donated("Vegetables") 
Stimulants_N_Donated <- calc_N_Donated("Stimulants") 
Oilcrops_N_Donated <- calc_N_Donated("Oilcrops") 
Sugarcrops_N_Donated <- calc_N_Donated("Sugarcrops") 
Nuts_N_Donated <- calc_N_Donated("Nuts") 
Spices_N_Donated <- calc_N_Donated("Spices") 
Beverages_N_Donated <- calc_N_Donated("Beverages") 

print(Poultry_N_Donated) 
print(Bovine_N_Donated)  
print(Pigmeat_N_Donated) 
print(Milk_N_Donated)  
print(Cheese_N_Donated) 
print(Eggs_N_Donated) 
print(Fish_N_Donated) 
print(Cereals_N_Donated) 
print(Fruits_N_Donated) 
print(Pulses_N_Donated) 
print(StarchyRoots_N_Donated)  
print(Vegetables_N_Donated) 
print(Stimulants_N_Donated)  
print(Oilcrops_N_Donated) 
print(Sugarcrops_N_Donated) 
print(Nuts_N_Donated) 
print(Spices_N_Donated) 
print(Beverages_N_Donated)
###########################################################################################



#Institution - Total N released due to Recycling####
#' @param N_Production_to_FoodOrdered ration of nitrogen released from food production to the total mass of food ordered
#' @param Mass_Donated total amount of food donated for each food category in kg
#'
#' @return N_Released_Recycling amount of nitrogen released due to recycling
#' @export 
#' 
#' @examples 
#'

calc_N_Released_Recycling <-function(Food_Category){
  
  n_production_to_food_ordered <- c(
    Poultry = "Poultry_N_Production_to_FoodOrdered", 
    Bovine = "Bovine_N_Production_to_FoodOrdered",
    Pigmeat = "Pigmeat_N_Production_to_FoodOrdered",
    Milk = "Milk_N_Production_to_FoodOrdered", 
    Cheese = "Cheese_N_Production_to_FoodOrdered",  
    Eggs = "Eggs_N_Production_to_FoodOrdered",
    Fish = "Fish_N_Production_to_FoodOrdered",  
    Cereals = "Cereals_N_Production_to_FoodOrdered", 
    Fruits = "Fruits_N_Production_to_FoodOrdered",
    Pulses = "Pulses_N_Production_to_FoodOrdered",
    Starchy_Roots = "Starchy_Roots_N_Production_to_FoodOrdered", 
    Vegetables = "Vegetables_N_Production_to_FoodOrdered",  
    Stimulants = "Stimulants_N_Production_to_FoodOrdered", 
    Oilcrops = "Oilcrops_N_Production_to_FoodOrdered", 
    Sugarcrops = "Sugarcrops_N_Production_to_FoodOrdered",  
    Nuts = "Nuts_N_Production_to_FoodOrdered", 
    Spices = "Spices_N_Production_to_FoodOrdered", 
    Beverages = "Beverages_N_Production_to_FoodOrdered"
  )
  
  mass_donated <- c(
    Poultry = "Poultry_Mass_Donated", 
    Bovine = "Bovine_Mass_Donated",
    Pigmeat = "Pigmeat_Mass_Donated",
    Milk = "Milk_Mass_Donated", 
    Cheese = "Cheese_Mass_Donated",  
    Eggs = "Eggs_Mass_Donated",
    Fish = "Fish_Mass_Donated",  
    Cereals = "Cereals_Mass_Donated", 
    Fruits = "Fruits_Mass_Donated",
    Pulses = "Pulses_Mass_Donated",
    Starchy_Roots = "StarchyRoots_Mass_Donated", 
    Vegetables = "Vegetables_Mass_Donated",  
    Stimulants = "Stimulants_Mass_Donated", 
    Oilcrops = "Oilcrops_Mass_Donated", 
    Sugarcrops = "Sugarcrops_Mass_Donated",  
    Nuts = "Nuts_Mass_Donated", 
    Spices = "Spices_Mass_Donated", 
    Beverages = "Beverages_Mass_Donated"
  )
  
  N_Released_Recycling <- get(n_production_to_food_ordered[Food_Category]) * get(mass_donated[Food_Category])
  
  return(N_Released_Recycling)
}

Poultry_N_Released_Recycling <- calc_N_Released_Recycling("Poultry") 
Bovine_N_Released_Recycling <- calc_N_Released_Recycling("Bovine") 
Pigmeat_N_Released_Recycling <- calc_N_Released_Recycling("Pigmeat") 
Milk_N_Released_Recycling <- calc_N_Released_Recycling("Milk") 
Cheese_N_Released_Recycling <- calc_N_Released_Recycling("Cheese") 
Eggs_N_Released_Recycling <- calc_N_Released_Recycling("Eggs") 
Fish_N_Released_Recycling <- calc_N_Released_Recycling("Fish") 
Cereals_N_Released_Recycling <- calc_N_Released_Recycling("Cereals") 
Fruits_N_Released_Recycling <- calc_N_Released_Recycling("Fruits") 
Pulses_N_Released_Recycling <- calc_N_Released_Recycling("Pulses") 
StarchyRoots_N_Released_Recycling <- calc_N_Released_Recycling("Starchy_Roots") 
Vegetables_N_Released_Recycling <- calc_N_Released_Recycling("Vegetables") 
Stimulants_N_Released_Recycling <- calc_N_Released_Recycling("Stimulants") 
Oilcrops_N_Released_Recycling <- calc_N_Released_Recycling("Oilcrops") 
Sugarcrops_N_Released_Recycling <- calc_N_Released_Recycling("Sugarcrops") 
Nuts_N_Released_Recycling <- calc_N_Released_Recycling("Nuts") 
Spices_N_Released_Recycling <- calc_N_Released_Recycling("Spices") 
Beverages_N_Released_Recycling <- calc_N_Released_Recycling("Beverages") 

print(Poultry_N_Released_Recycling) 
print(Bovine_N_Released_Recycling)  
print(Pigmeat_N_Released_Recycling) 
print(Milk_N_Released_Recycling)  
print(Cheese_N_Released_Recycling) 
print(Eggs_N_Released_Recycling) 
print(Fish_N_Released_Recycling) 
print(Cereals_N_Released_Recycling) 
print(Fruits_N_Released_Recycling) 
print(Pulses_N_Released_Recycling) 
print(StarchyRoots_N_Released_Recycling)  
print(Vegetables_N_Released_Recycling) 
print(Stimulants_N_Released_Recycling)  
print(Oilcrops_N_Released_Recycling) 
print(Sugarcrops_N_Released_Recycling) 
print(Nuts_N_Released_Recycling) 
print(Spices_N_Released_Recycling) 
print(Beverages_N_Released_Recycling) 
###########################################################################################



#Institution - Total N released from foodwaste to landfill####
#' @param  Total_FoodWaste_N total nitrogen in food in kg that is associated with human waste 
#' @param N_Composted total amount of food composted in kg
#' @param N_Donated amount of nitrogen in donated food for each food category in kg
#'
#' @return N_Released_Landfill amount of nitrogen released due to food waste going to a landfill
#' @export 
#' 
#' @examples 
#'

calc_N_Released_Landfill <- function(Food_Category){
  
  foodwaste_N <- c( 
    Poultry = "Poultry_Foodwaste_N", 
    Bovine = "Bovine_Foodwaste_N",  
    Pigmeat = "Pigmeat_Foodwaste_N", 
    Milk = "Milk_Foodwaste_N", 
    Cheese = "Cheese_Foodwaste_N", 
    Eggs = "Eggs_Foodwaste_N", 
    Fish = "Fish_Foodwaste_N", 
    Cereals = "Cereals_Foodwaste_N", 
    Fruits = "Fruits_Foodwaste_N", 
    Pulses = "Pulses_Foodwaste_N", 
    Starchy_Roots = "StarchyRoots_Foodwaste_N", 
    Vegetables = "Vegetables_Foodwaste_N", 
    Stimulants = "Stimulants_Foodwaste_N", 
    Oilcrops = "Oilcrops_Foodwaste_N", 
    Sugarcrops = "Sugarcrops_Foodwaste_N", 
    Nuts = "Nuts_Foodwaste_N", 
    Spices = "Spices_Foodwaste_N", 
    Beverages = "Beverages_Foodwaste_N" 
  )
  
  total_donated <- c(
    Poultry = "Poultry_N_Donated", 
    Bovine = "Bovine_N_Donated",
    Pigmeat = "Pigmeat_N_Donated",
    Milk = "Milk_N_Donated", 
    Cheese = "Cheese_N_Donated",  
    Eggs = "Eggs_N_Donated",
    Fish = "Fish_N_Donated",  
    Cereals = "Cereals_N_Donated", 
    Fruits = "Fruits_N_Donated",
    Pulses = "Pulses_N_Donated",
    Starchy_Roots = "StarchyRoots_N_Donated", 
    Vegetables = "Vegetables_N_Donated",  
    Stimulants = "Stimulants_N_Donated", 
    Oilcrops = "Oilcrops_N_Donated", 
    Sugarcrops = "Sugarcrops_N_Donated",  
    Nuts = "Nuts_N_Donated", 
    Spices = "Spices_N_Donated", 
    Beverages = "Beverages_N_Donated"
  )  
  
  total_composted <- c(
    Poultry = "Poultry_N_Composted", 
    Bovine = "Bovine_N_Composted",
    Pigmeat = "Pigmeat_N_Composted",
    Milk = "Milk_N_Composted", 
    Cheese = "Cheese_N_Composted",  
    Eggs = "Eggs_N_Composted",
    Fish = "Fish_N_Composted",  
    Cereals = "Cereals_N_Composted", 
    Fruits = "Fruits_N_Composted",
    Pulses = "Pulses_N_Composted",
    Starchy_Roots = "StarchyRoots_N_Composted", 
    Vegetables = "Vegetables_N_Composted",  
    Stimulants = "Stimulants_N_Composted", 
    Oilcrops = "Oilcrops_N_Composted", 
    Sugarcrops = "Sugarcrops_N_Composted",  
    Nuts = "Nuts_N_Composted", 
    Spices = "Spices_N_Composted", 
    Beverages = "Beverages_N_Composted"
  )  
  
  N_Released_Landfill <- get(foodwaste_N[Food_Category]) - (get(total_donated[Food_Category]) + get(total_composted[Food_Category]))
  
  return(N_Released_Landfill)
}

Poultry_N_Released_Landfill <- calc_N_Released_Landfill("Poultry") 
Bovine_N_Released_Landfill <- calc_N_Released_Landfill("Bovine") 
Pigmeat_N_Released_Landfill <- calc_N_Released_Landfill("Pigmeat") 
Milk_N_Released_Landfill <- calc_N_Released_Landfill("Milk") 
Cheese_N_Released_Landfill <- calc_N_Released_Landfill("Cheese") 
Eggs_N_Released_Landfill <- calc_N_Released_Landfill("Eggs") 
Fish_N_Released_Landfill <- calc_N_Released_Landfill("Fish") 
Cereals_N_Released_Landfill <- calc_N_Released_Landfill("Cereals") 
Fruits_N_Released_Landfill <- calc_N_Released_Landfill("Fruits") 
Pulses_N_Released_Landfill <- calc_N_Released_Landfill("Pulses") 
StarchyRoots_N_Released_Landfill <- calc_N_Released_Landfill("Starchy_Roots") 
Vegetables_N_Released_Landfill <- calc_N_Released_Landfill("Vegetables") 
Stimulants_N_Released_Landfill <- calc_N_Released_Landfill("Stimulants") 
Oilcrops_N_Released_Landfill <- calc_N_Released_Landfill("Oilcrops") 
Sugarcrops_N_Released_Landfill <- calc_N_Released_Landfill("Sugarcrops") 
Nuts_N_Released_Landfill <- calc_N_Released_Landfill("Nuts") 
Spices_N_Released_Landfill <- calc_N_Released_Landfill("Spices") 
Beverages_N_Released_Landfill <- calc_N_Released_Landfill("Beverages") 


print(Poultry_N_Released_Landfill) 
print(Bovine_N_Released_Landfill)  
print(Pigmeat_N_Released_Landfill) 
print(Milk_N_Released_Landfill)  
print(Cheese_N_Released_Landfill) 
print(Eggs_N_Released_Landfill) 
print(Fish_N_Released_Landfill) 
print(Cereals_N_Released_Landfill) 
print(Fruits_N_Released_Landfill) 
print(Pulses_N_Released_Landfill) 
print(StarchyRoots_N_Released_Landfill)  
print(Vegetables_N_Released_Landfill) 
print(Stimulants_N_Released_Landfill)  
print(Oilcrops_N_Released_Landfill) 
print(Sugarcrops_N_Released_Landfill) 
print(Nuts_N_Released_Landfill) 
print(Spices_N_Released_Landfill) 
print(Beverages_N_Released_Landfill)
###########################################################################################



#Institution - N released from human waste####
#' @param  Total_Food_Waste_N total nitrogen in food in kg that is associated with human waste 
#' @param Sewage_N_Reduction_Factor provided by UVA , sewage treatment N reduction factor
#'
#' @return N_Released_Landfill amount of nitrogen released due to food waste going to a landfill
#' @export 
#' 
#' @examples 
#'

calc_N_Released_FoodConsumed <- function(Food_Category){
  N_foodconsumed <- c( 
    Poultry = "Poultry_N_FoodConsumed", 
    Bovine = "Bovine_N_FoodConsumed",  
    Pigmeat = "Pigmeat_N_FoodConsumed", 
    Milk = "Milk_N_FoodConsumed", 
    Cheese = "Cheese_N_FoodConsumed", 
    Eggs = "Eggs_N_FoodConsumed", 
    Fish = "Fish_N_FoodConsumed", 
    Cereals = "Cereals_N_FoodConsumed", 
    Fruits = "Fruits_N_FoodConsumed", 
    Pulses = "Pulses_N_FoodConsumed", 
    Starchy_Roots = "StarchyRoots_N_FoodConsumed", 
    Vegetables = "Vegetables_N_FoodConsumed", 
    Stimulants = "Stimulants_N_FoodConsumed", 
    Oilcrops = "Oilcrops_N_FoodConsumed", 
    Sugarcrops = "Sugarcrops_N_FoodConsumed", 
    Nuts = "Nuts_N_FoodConsumed", 
    Spices = "Spices_N_FoodConsumed", 
    Beverages = "Beverages_N_FoodConsumed" 
  )
  
  sewage_reduction_factor <- c(
    Poultry = 0.42,
    Bovine = 0.42,
    Pigmeat = 0.42,
    Milk = 0.42,
    Cheese = 0.42,
    Eggs = 0.42,
    Fish = 0.42,
    Cereals = 0.42,
    Fruits = 0.42,
    Pulses = 0.42,
    Starchy_Roots = 0.42,
    Vegetables = 0.42,
    Stimulants = 0.42,
    Oilcrops = 0.42,
    Sugarcrops = 0.42,
    Nuts = 0.42,
    Spices = 0.42,
    Beverages = 0.42
  )
  N_Released_FoodConsumed <- get(N_foodconsumed[Food_Category]) * (1 - sewage_reduction_factor[Food_Category])
  
  return(N_Released_FoodConsumed)
}


Poultry_N_Released_FoodConsumed <- calc_N_Released_FoodConsumed("Poultry") 
Bovine_N_Released_FoodConsumed <- calc_N_Released_FoodConsumed("Bovine") 
Pigmeat_N_Released_FoodConsumed <- calc_N_Released_FoodConsumed("Pigmeat") 
Milk_N_Released_FoodConsumed <- calc_N_Released_FoodConsumed("Milk") 
Cheese_N_Released_FoodConsumed <- calc_N_Released_FoodConsumed("Cheese") 
Eggs_N_Released_FoodConsumed <- calc_N_Released_FoodConsumed("Eggs") 
Fish_N_Released_FoodConsumed <- calc_N_Released_FoodConsumed("Fish") 
Cereals_N_Released_FoodConsumed <- calc_N_Released_FoodConsumed("Cereals") 
Fruits_N_Released_FoodConsumed <- calc_N_Released_FoodConsumed("Fruits") 
Pulses_N_Released_FoodConsumed <- calc_N_Released_FoodConsumed("Pulses") 
StarchyRoots_N_Released_FoodConsumed <- calc_N_Released_FoodConsumed("Starchy_Roots") 
Vegetables_N_Released_FoodConsumed <- calc_N_Released_FoodConsumed("Vegetables") 
Stimulants_N_Released_FoodConsumed <- calc_N_Released_FoodConsumed("Stimulants") 
Oilcrops_N_Released_FoodConsumed <- calc_N_Released_FoodConsumed("Oilcrops") 
Sugarcrops_N_Released_FoodConsumed <- calc_N_Released_FoodConsumed("Sugarcrops") 
Nuts_N_Released_FoodConsumed <- calc_N_Released_FoodConsumed("Nuts") 
Spices_N_Released_FoodConsumed <- calc_N_Released_FoodConsumed("Spices") 
Beverages_N_Released_FoodConsumed <- calc_N_Released_FoodConsumed("Beverages") 

# Print the results
print(Poultry_N_Released_FoodConsumed) 
print(Bovine_N_Released_FoodConsumed)  
print(Pigmeat_N_Released_FoodConsumed) 
print(Milk_N_Released_FoodConsumed)  
print(Cheese_N_Released_FoodConsumed) 
print(Eggs_N_Released_FoodConsumed) 
print(Fish_N_Released_FoodConsumed) 
print(Cereals_N_Released_FoodConsumed) 
print(Fruits_N_Released_FoodConsumed) 
print(Pulses_N_Released_FoodConsumed) 
print(StarchyRoots_N_Released_FoodConsumed)  
print(Vegetables_N_Released_FoodConsumed) 
print(Stimulants_N_Released_FoodConsumed)  
print(Oilcrops_N_Released_FoodConsumed) 
print(Sugarcrops_N_Released_FoodConsumed) 
print(Nuts_N_Released_FoodConsumed) 
print(Spices_N_Released_FoodConsumed) 
print(Beverages_N_Released_FoodConsumed)
