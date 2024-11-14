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

#Example
Institution_Population(Residential_Employees = 100, 50, 10000, 2000, 5000)




###################################################################################################################################################################################################################################


#' Institution - Total Food Mass
#'
#' @param Food_Category Kind of food 
#' @param Institution_Population Population value for Loyola
#'
#' @return Estimated total food mass for the food category at the institution in kg
#' @export
#'
#' @examples
#' Total_Food_Mass("Poultry", Institution_Population)


Total_Food_Mass <- function(Food_Category, Institution_Population) {

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
  
 
  mass <- food_masses[Food_Category] * (Institution_Population / UVA_Population)
  
  
  return(mass)
}

# Example usage:

Poultry_Total_Food_Mass <- Total_Food_Mass("Poultry", 50000)
Bovine_Total_Food_Mass <- Total_Food_Mass("Bovine", 50000)
Pigmeat_Total_Food_Mass <- Total_Food_Mass("Pigmeat", 50000)
Milk_Total_Food_Mass <- Total_Food_Mass("Milk", 50000)
Cheese_Total_Food_Mass <- Total_Food_Mass("Cheese", 50000)
Eggs_Total_Food_Mass <- Total_Food_Mass("Eggs", 50000)
Fish_Total_Food_Mass <- Total_Food_Mass("Fish", 50000)
Cereals_Total_Food_Mass <- Total_Food_Mass("Cereals", 50000)
Fruits_Total_Food_Mass <- Total_Food_Mass("Fruits", 50000)
Pulses_Total_Food_Mass <- Total_Food_Mass("Pulses", 50000)
Starchy_Roots_Total_Food_Mass <- Total_Food_Mass("Starchy_Roots", 50000)
Vegetables_Total_Food_Mass <- Total_Food_Mass("Vegetables", 50000)
Stimulants_Total_Food_Mass <- Total_Food_Mass("Stimulants", 50000)
Oilcrops_Total_Food_Mass <- Total_Food_Mass("Oilcrops", 50000)
Sugarcrops_Total_Food_Mass <- Total_Food_Mass("Sugarcrops", 50000)
Nuts_Total_Food_Mass <- Total_Food_Mass("Nuts", 50000)
Spices_Total_Food_Mass <- Total_Food_Mass("Spices", 50000)
Beverages_Total_Food_Mass <- Total_Food_Mass("Beverages", 50000)

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
#' Institution - Total Virtual N
#'
#' @param Food_Category Kind of food 
#' @param Institution_Population Population value for Loyola
#'
#' @return Estimated total virtual N for the food category at the institution in kg
#' @export
#'
#' @examples


Total_Virtual_N <- function(Food_Category, Institution_Population) {
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
  
 
  virtual <- Virtual_N[Food_Category] * (Institution_Population / UVA_Population)
  
  return(virtual)
}

# Example:
Poultry_Virtual_N <- Total_Virtual_N("Poultry", 50000)
Bovine_Virtual_N <- Total_Virtual_N("Bovine", 50000)
Pigmeat_Virtual_N <- Total_Virtual_N("Pigmeat", 50000)
Milk_Virtual_N <- Total_Virtual_N("Milk", 50000)
Cheese_Virtual_N <- Total_Virtual_N("Cheese", 50000)
Eggs_Virtual_N <- Total_Virtual_N("Eggs", 50000)
Fish_Virtual_N <- Total_Virtual_N("Fish", 50000)
Cereals_Virtual_N <- Total_Virtual_N("Cereals", 50000)
Fruits_Virtual_N <- Total_Virtual_N("Fruits", 50000)
Pulses_Virtual_N <- Total_Virtual_N("Pulses", 50000)
Starchy_Roots_Virtual_N <- Total_Virtual_N("Starchy_Roots", 50000)
Vegetables_Virtual_N <- Total_Virtual_N("Vegetables", 50000)
Stimulants_Virtual_N <- Total_Virtual_N("Stimulants", 50000)
Oilcrops_Virtual_N <- Total_Virtual_N("Oilcrops", 50000)
Sugarcrops_Virtual_N <- Total_Virtual_N("Sugarcrops", 50000)
Nuts_Virtual_N <- Total_Virtual_N("Nuts", 50000)
Spices_Virtual_N <- Total_Virtual_N("Spices", 50000)
Beverages_Virtual_N <- Total_Virtual_N("Beverages", 50000)

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
#' Institution - Total Transport N
#'
#' @param Food_Category Kind of food 
#' @param Institution_Population Population value for Loyola
#'
#' @return Estimated Total Transport N for the food category at the institution in kg
#' @export
#'
#' @examples
#' 


Total_Transport_N <- function(Food_Category, Institution_Population) {
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
  
  transport <- Transport_N[Food_Category] * (Institution_Population / UVA_Population)
  
  return(transport)
}

#Example
Institution_Population <- 50000

# Calculate and print Transport N for each category
Poultry_Transport_N <- Total_Transport_N("Poultry", Institution_Population)
Bovine_Transport_N <- Total_Transport_N("Bovine", Institution_Population)
Pigmeat_Transport_N <- Total_Transport_N("Pigmeat", Institution_Population)
Milk_Transport_N <- Total_Transport_N("Milk", Institution_Population)
Cheese_Transport_N <- Total_Transport_N("Cheese", Institution_Population)
Eggs_Transport_N <- Total_Transport_N("Eggs", Institution_Population)
Fish_Transport_N <- Total_Transport_N("Fish", Institution_Population)
Cereals_Transport_N <- Total_Transport_N("Cereals", Institution_Population)
Fruits_Transport_N <- Total_Transport_N("Fruits", Institution_Population)
Pulses_Transport_N <- Total_Transport_N("Pulses", Institution_Population)
Starchy_Roots_Transport_N <- Total_Transport_N("Starchy_Roots", Institution_Population)
Vegetables_Transport_N <- Total_Transport_N("Vegetables", Institution_Population)
Stimulants_Transport_N <- Total_Transport_N("Stimulants", Institution_Population)
Oilcrops_Transport_N <- Total_Transport_N("Oilcrops", Institution_Population)
Sugarcrops_Transport_N <- Total_Transport_N("Sugarcrops", Institution_Population)
Nuts_Transport_N <- Total_Transport_N("Nuts", Institution_Population)
Spices_Transport_N <- Total_Transport_N("Spices", Institution_Population)
Beverages_Transport_N <- Total_Transport_N("Beverages", Institution_Population)

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
#' Institution - Total N in Food 
#'
#' @param Food_Category Kind of food 
#' @param Institution_Population Population value for Loyola
#'
#' @return Estimated Total N in food for the food category at the institution in kg
#' @export
#'
#' @examples
#' 
Total_Food_N <- function(Food_Category, Institution_Population) {
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
  total_n <- Food_N[Food_Category] * (Institution_Population / UVA_Population)
  
  return(total_n)
}

# Example usage for each food category
Poultry_Total_Food_N <- Total_Food_N("Poultry", 50000)
Bovine_Total_Food_N <- Total_Food_N("Bovine", 50000)
Pigmeat_Total_Food_N <- Total_Food_N("Pigmeat", 50000)
Milk_Total_Food_N <- Total_Food_N("Milk", 50000)
Cheese_Total_Food_N <- Total_Food_N("Cheese", 50000)
Eggs_Total_Food_N <- Total_Food_N("Eggs", 50000)
Fish_Total_Food_N <- Total_Food_N("Fish", 50000)
Cereals_Total_Food_N <- Total_Food_N("Cereals", 50000)
Fruits_Total_Food_N <- Total_Food_N("Fruits", 50000)
Pulses_Total_Food_N <- Total_Food_N("Pulses", 50000)
Starchy_Roots_Total_Food_N <- Total_Food_N("Starchy_Roots", 50000)
Vegetables_Total_Food_N <- Total_Food_N("Vegetables", 50000)
Stimulants_Total_Food_N <- Total_Food_N("Stimulants", 50000)
Oilcrops_Total_Food_N <- Total_Food_N("Oilcrops", 50000)
Sugarcrops_Total_Food_N <- Total_Food_N("Sugarcrops", 50000)
Nuts_Total_Food_N <- Total_Food_N("Nuts", 50000)
Spices_Total_Food_N <- Total_Food_N("Spices", 50000)
Beverages_Total_Food_N <- Total_Food_N("Beverages", 50000)

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
#' Institution - Total N Released  
#'
#' @param Total_Transport_N total Transport N for the food category at the institution in kg
#' @param Total_Food_N total N in food for the food category at the institution in kg
#'
#' @return 
#' @export
#'
#' @examples
#' 

Total_N_Released <- function(Total_Transport_N, Total_Food_N) {
  total_n_released <- sum(Total_Transport_N, Total_Food_N)
  
  return(total_n_released)
}

#Example
Poultry_Total_N_Released <- Total_N_Released(50, 100)
Bovine_Total_N_Released <- Total_N_Released(50, 100)
Pigmeat_Total_N_Released <- Total_N_Released(50, 100)
Milk_Total_N_Released <- Total_N_Released(50, 100)
Cheese_Total_N_Released <- Total_N_Released(50, 100)
Eggs_Total_N_Released <- Total_N_Released(50, 100)
Fish_Total_N_Released <- Total_N_Released(50, 100)
Cereals_Total_N_Released <- Total_N_Released(50, 100)
Fruits_Total_N_Released <- Total_N_Released(50, 100)
Pulses_Total_N_Released <- Total_N_Released(50, 100)
Starchy_Roots_Total_N_Released <- Total_N_Released(50, 100)
Vegetables_Total_N_Released <- Total_N_Released(50, 100)
Stimulants_Total_N_Released <- Total_N_Released(50, 100)
Oilcrops_Total_N_Released <- Total_N_Released(50, 100)
Sugarcrops_Total_N_Released <- Total_N_Released(50, 100)
Nuts_Total_N_Released <- Total_N_Released(50, 100)
Spices_Total_N_Released <- Total_N_Released(50, 100)
Beverages_Total_N_Released <- Total_N_Released(50, 100)

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











