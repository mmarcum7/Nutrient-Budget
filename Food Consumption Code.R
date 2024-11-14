#Total Food Mass Calculation####
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

calc_TotalFoodMass <-function(Food_Category, Institution_Population) {
  UVA_Population <- 34056  # UVA population is a constant 
  if(Food_Category == "Poultry") {
    mass <-(254697 * Institution_Population/UVA_Population) # 254697= UVA Total food mass for poultry
  }
  else if(Food_Category == "Bovine") {
    mass <-(193361  * Institution_Population/UVA_Population) #  193361 = UVA Total food mass for Bovine
  }
  else if(Food_Category == "Pigmeat") {
    mass <-(153006 * Institution_Population/UVA_Population) #  153,006 = UVA Total food mass for pigmeat
  }
  else if(Food_Category == "Milk") {
    mass<-(438138 * Institution_Population/UVA_Population) #   438,138  = UVA Total food mass for milk
  }
  else if(Food_Category == "Cheese") {
    mass <-(204446 * Institution_Population/UVA_Population) # 204,446   = UVA Total food mass for cheese
  }
  else if(Food_Category == "Eggs") {
    mass <-(200567  * Institution_Population/UVA_Population) # 200,567    = UVA Total food mass for eggs
  }
  else if(Food_Category == "Fish") {
    mass <-(28033   * Institution_Population/UVA_Population) #  28,033= UVA Total food mass for fish
  }
  else if(Food_Category == "Cereals") {
    mass <-(422315    * Institution_Population/UVA_Population) # 422,315  = UVA Total food mass for cereals
  }
  else if(Food_Category == "Fruits") {
    mass <-(393925 * Institution_Population/UVA_Population) # 393,925  = UVA Total food mass for fruits
  }
  else if(Food_Category == "Pulses") {
    mass<-(77077  * Institution_Population/UVA_Population) #  77,077   = UVA Total food mass for pulses
  }
  else if(Food_Category == "Starchy Roots") {
    mass <-(213022  * Institution_Population/UVA_Population) # 213,022 = UVA Total food mass for startchy roots
  }
  else if(Food_Category == "Vegetables") {
    mass <-( 195840   * Institution_Population/UVA_Population) # 195,840  = UVA Total food mass for vegetables
  }
  else if(Food_Category == "Stimulants") {
    mass <-( 133601    * Institution_Population/UVA_Population) #  133,601   = UVA Total food mass for stimulants
  }
  else if(Food_Category == "Oilcrops") {
    mass <-( 115328    * Institution_Population/UVA_Population) # 115,328    = UVA Total food mass for oilcrops
  }
  else if(Food_Category == "Sugarcrops") {
    mass <-(145231     * Institution_Population/UVA_Population) # 145,231   = UVA Total food mass for suagrcrops
  }
  else if(Food_Category == "Nuts") {
    mass <-(11071   * Institution_Population/UVA_Population) # 11,071   = UVA Total food mass for nuts
  }
  else if(Food_Category == "Spices") {
    mass <-(5521 * Institution_Population/UVA_Population) # 5,521   = UVA Total food mass for spices
  }
  else if(Food_Category == "Beverages") {
    mass <-(517861 * Institution_Population/UVA_Population) # 517,861   = UVA Total food mass for beverages
  }
  
  return(mass)
}

Poultry_mass <- calc_TotalFoodMass("Poultry", 20000)
Bovine_mass <- calc_TotalFoodMass("Bovine", 20000)
Pigmeat_mass <- calc_TotalFoodMass("Pigmeat", 20000)
Milk_mass <- calc_TotalFoodMass("Milk", 20000)
Cheese_mass <- calc_TotalFoodMass("Cheese", 20000)
Eggs_mass <- calc_TotalFoodMass("Eggs", 20000)
Fish_mass <- calc_TotalFoodMass("Fish", 20000)
Cereals_mass <- calc_TotalFoodMass("Cereals", 20000)
Fruits_mass <- calc_TotalFoodMass("Fruits", 20000)
Pulses_mass <- calc_TotalFoodMass("Pulses", 20000)
Starchyroots_mass <- TotalFoodMass("Starchy Roots", 20000)
Vegetables_mass <- calc_TotalFoodMass("Vegetables", 20000)
Stimulants_mass <- calc_TotalFoodMass("Stimulants", 20000)
Oilcrops_mass <- calc_TotalFoodMass("Oilcrops", 20000)
Sugarcrops_mass <- calc_TotalFoodMass("Sugarcrops", 20000)
Nuts_mass <- calc_TotalFoodMass("Nuts", 20000)
Spices_mass <- calc_TotalFoodMass("Spices", 20000)
Beverages_mass <- calc_TotalFoodMass("Beverages", 20000)

print(Poultry_mass)
print(Bovine_mass)
print(Pigmeat_mass)
print(Milk_mass)
print(Cheese_mass)
print(Eggs_mass)
print(Fish_mass)
print(Cereals_mass)
print(Fruits_mass)
print(Pulses_mass)
print(Starchyroots_mass)
print(Vegetables_mass)
print(Stimulants_mass)
print(Oilcrops_mass)
print(Sugarcrops_mass)
print(Nuts_mass)
print(Spices_mass)
print(Beverages_mass)


#Calculate Total Food Mass
total_food_mass <- sum(Poultry_mass + Bovine_mass + Pigmeat_mass + Milk_mass + Cheese_mass + 
Eggs_mass + Fish_mass + Cereals_mass + Fruits_mass + Pulses_mass + 
Starchyroots_mass + Vegetables_mass + Stimulants_mass + 
Oilcrops_mass + Sugarcrops_mass + Nuts_mass + 
Spices_mass + Beverages_mass)
print(total_food_mass)

#Total Virtual Nitrogen Calculation####
#
#' Institution - Total Virtual N
#'
#' @param Food_Category - kind of food
#' @param Food_Mass - UVA Total Virtual N in kg
#' @param Institution_Population- population value for Loyola 
#' @param 
#'
#'
#' @return  Total Virtual N
#' @export 
#'
#' @examples
#' 

calc_TotalVirtualN <-function(Food_Category, Institution_Population) {
  UVA_Population <- 34056  # UVA population is a constant 
  if(Food_Category == "Poultry") {
    virtual <-(20546 * Institution_Population/UVA_Population) # 20546= UVA Total virtual N for poultry
  }
  else if(Food_Category == "Bovine") {
    virtual <-(40257  * Institution_Population/UVA_Population) #  40257 = UVA Total virtual N for Bovine
  }
  else if(Food_Category == "Pigmeat") {
    virtual <-(16600 * Institution_Population/UVA_Population) #  16600 = UVA Total virtual N for pigmeat
  }
  else if(Food_Category == "Milk") {
    virtual <-(9654 * Institution_Population/UVA_Population) #   9654  = UVA Total virtual N for milk
  }
  else if(Food_Category == "Cheese") {
    virtual <-(15710 * Institution_Population/UVA_Population) # 15710   = UVA Total virtual N for cheese
  }
  else if(Food_Category == "Eggs") {
    virtual <-(11010  * Institution_Population/UVA_Population) # 11010    = UVA Total virtual N for eggs
  }
  else if(Food_Category == "Fish") {
    virtual <-(1688   * Institution_Population/UVA_Population) #  1688 = UVA Total virtual N for fish
  }
  else if(Food_Category == "Cereals") {
    virtual <-(4338    * Institution_Population/UVA_Population) # 4338  = UVA Total virtual N for cereals
  }
  else if(Food_Category == "Fruits") {
    virtual <-(2247 * Institution_Population/UVA_Population) # 2247  = UVA Total virtual N for fruits
  }
  else if(Food_Category == "Pulses") {
    virtual <-(202  * Institution_Population/UVA_Population) #  202   = UVA Total virtual N for pulses
  }
  else if(Food_Category == "Starchy Roots") {
    virtual <-(418  * Institution_Population/UVA_Population) # 418 = UVA Total virtual N for startchy roots
  }
  else if(Food_Category == "Vegetables") {
    virtual <-(2405   * Institution_Population/UVA_Population) # 2405  = UVA Total virtual N for vegetables
  }
  else if(Food_Category == "Stimulants") {
    virtual <-( 35   * Institution_Population/UVA_Population) #  35   = UVA Total virtual N for stimulants
  }
  else if(Food_Category == "Oilcrops") {
    virtual <-( 306   * Institution_Population/UVA_Population) # 306    = UVA Total virtual N for oilcrops
  }
  else if(Food_Category == "Sugarcrops") {
    virtual <-(739     * Institution_Population/UVA_Population) # 739   = UVA Total virtual N for sugarcrops
  }
  else if(Food_Category == "Nuts") {
    virtual <-(69   * Institution_Population/UVA_Population) # 69  = UVA Total virtual N for nuts
  }
  else if(Food_Category == "Spices") {
    virtual <-(481 * Institution_Population/UVA_Population) # 481   = UVA Total virtual N for spices
  }
  else if(Food_Category == "Beverages") {
    virtual <-(340 * Institution_Population/UVA_Population) # 340   = UVA Total virtual N for beverages
  }
  
  return(virtual)
}

Poultry_virtualN <- calc_TotalVirtualN("Poultry", 20000)
Bovine_virtualN <- calc_TotalVirtualN("Bovine", 20000)
Pigmeat_virtualN <- calc_TotalVirtualN("Pigmeat", 20000)
Milk_virtualN <- calc_TotalVirtualN("Milk", 20000)
Cheese_virtualN <- calc_TotalVirtualN("Cheese", 20000)
Eggs_virtualN <- calc_TotalVirtualN("Eggs", 20000)
Fish_virtualN <- calc_TotalVirtualN("Fish", 20000)
Cereals_virtualN <- calc_TotalVirtualN("Cereals", 20000)
Fruits_virtualN <- calc_TotalVirtualN("Fruits", 20000)
Pulses_virtualN <- calc_TotalVirtualN("Pulses", 20000)
Starchyroots_virtualN <- calc_TotalVirtualN("Starchy Roots", 20000)
Vegetables_virtualN <- calc_TotalVirtualN("Vegetables", 20000)
Stimulants_virtualN <- calc_TotalVirtualN("Stimulants", 20000)
Oilcrops_virtualN <- calc_TotalVirtualN("Oilcrops", 20000)
Sugarcrops_virtualN <- calc_TotalVirtualN("Sugarcrops", 20000)
Nuts_virtualN <- calc_TotalVirtualN("Nuts", 20000)
Spices_virtualN <- calc_TotalVirtualN("Spices", 20000)
Beverages_virtualN <- calc_TotalVirtualN("Beverages", 20000)

print(Poultry_virtualN)
print(Bovine_virtualN)
print(Pigmeat_virtualN)
print(Milk_virtualN)
print(Cheese_virtualN)
print(Eggs_virtualN)
print(Fish_virtualN)
print(Cereals_virtualN)
print(Fruits_virtualN)
print(Pulses_virtualN)
print(Starchyroots_virtualN)
print(Vegetables_virtualN)
print(Stimulants_virtualN)
print(Oilcrops_virtualN)
print(Sugarcrops_virtualN)
print(Nuts_virtualN)
print(Spices_virtualN)
print(Beverages_virtualN)


#Calculate Total Virtual N
total_virtual_N <- sum(Poultry_virtualN + Bovine_virtualN + Pigmeat_virtualN + Milk_virtualN + Cheese_virtualN + 
Eggs_virtualN + Fish_virtualN + Cereals_virtualN + Fruits_virtualN + Pulses_virtualN + 
Starchyroots_virtualN + Vegetables_virtualN + Stimulants_virtualN + 
Oilcrops_virtualN + Sugarcrops_virtualN + Nuts_virtualN + 
Spices_virtualN + Beverages_virtualN)
print(total_virtual_N)



#Total Transport N Calculation####
#' Institution - Total Transport N
#'
#' @param Food_Category - kind of food
#' @param Food_Mass - UVA Total Transport N in kg
#' @param Institution_Population- population value for Loyola 
#' @param 
#'
#'
#' @return  Total Transport N
#' @export 
#'
#' @examples
#' 

calc_TotalTransportN <-function(Food_Category, Institution_Population) {
  UVA_Population <- 34056  # UVA population is a constant 
  if(Food_Category == "Poultry") {
    transport <-(2 * Institution_Population/UVA_Population) # 2= UVA Total transport N for poultry
  }
  else if(Food_Category == "Bovine") {
    transport <-(2  * Institution_Population/UVA_Population) #2 = UVA Total transport N for Bovine
  }
  else if(Food_Category == "Pigmeat") {
    transport <-(1 * Institution_Population/UVA_Population) #  1 = UVA Total transport N for pigmeat
  }
  else if(Food_Category == "Milk") {
    transport <-(3 * Institution_Population/UVA_Population) # 3  = UVA Total transport N for milk
  }
  else if(Food_Category == "Cheese") {
    transport <-(1 * Institution_Population/UVA_Population) # 1   = UVA Total transport N for cheese
  }
  else if(Food_Category == "Eggs") {
    transport <-(1  * Institution_Population/UVA_Population) # 1    = UVA Total transport N for eggs
  }
  else if(Food_Category == "Fish") {
    transport <-(1   * Institution_Population/UVA_Population) #  1 = UVA Total transport N for fish
  }
  else if(Food_Category == "Cereals") {
    transport <-(4    * Institution_Population/UVA_Population) # 4  = UVA Total transport N for cereals
  }
  else if(Food_Category == "Fruits") {
    transport <-(35 * Institution_Population/UVA_Population) # 35  = UVA Total transport N for fruits
  }
  else if(Food_Category == "Pulses") {
    transport <-(1  * Institution_Population/UVA_Population) # 1   = UVA Total transport N for pulses
  }
  else if(Food_Category == "Starchy Roots") {
    transport <-(2  * Institution_Population/UVA_Population) # 2 = UVA Total transport N for startchy roots
  }
  else if(Food_Category == "Vegetables") {
    transport <-(2   * Institution_Population/UVA_Population) # 2  = UVA Total transport N for vegetables
  }
  else if(Food_Category == "Stimulants") {
    transport <-( 1   * Institution_Population/UVA_Population) # 1  = UVA Total transport N for stimulants
  }
  else if(Food_Category == "Oilcrops") {
    transport <-( 1   * Institution_Population/UVA_Population) # 1    = UVA Total transport N for oilcrops
  }
  else if(Food_Category == "Sugarcrops") {
    transport <-(1     * Institution_Population/UVA_Population) # 1   = UVA Total transport N for sugarcrops
  }
  else if(Food_Category == "Nuts") {
    transport <-(0  * Institution_Population/UVA_Population) # 0  = UVA Total transport N for nuts
  }
  else if(Food_Category == "Spices") {
    transport <-(0 * Institution_Population/UVA_Population) # 0   = UVA Total transport N for spices
  }
  else if(Food_Category == "Beverages") {
    transport <-(5 * Institution_Population/UVA_Population) # 5   = UVA Total transport N for beverages
  }
  
  return(transport)
}

Poultry_transportN <- calc_TotalTransportN("Poultry", 20000)
Bovine_transportN <- calc_TotalTransportN("Bovine", 20000)
Pigmeat_transportN <- calc_TotalTransportN("Pigmeat", 20000)
Milk_transportN <- calc_TotalTransportN("Milk", 20000)
Cheese_transportN <- calc_TotalTransportN("Cheese", 20000)
Eggs_transportN <- calc_TotalTransportN("Eggs", 20000)
Fish_transportN <- calc_TotalTransportN("Fish", 20000)
Cereals_transportlN <- calc_TotalTransportN("Cereals", 20000)
Fruits_transportN <- calc_TotalTransportN("Fruits", 20000)
Pulses_transportN <- calc_TotalTransportN("Pulses", 20000)
Starchyroots_transportN <- calc_TotalTransportN("Starchy Roots", 20000)
Vegetables_transportN <- calc_TotalTransportN("Vegetables", 20000)
Stimulants_transportN <- calc_TotalTransportN("Stimulants", 20000)
Oilcrops_transportN <- calc_TotalTransportN("Oilcrops", 20000)
Sugarcrops_transportN <- calc_TotalTransportN("Sugarcrops", 20000)
Nuts_transportN <- calc_TotalTransportN("Nuts", 20000)
Spices_transportN <- calc_TotalVirtualN("Spices", 20000)
Beverages_transportN <- calc_TotalVirtualN("Beverages", 20000)

print(Poultry_transportN)
print(Bovine_transportN)
print(Pigmeat_transportN)
print(Milk_transportN)
print(Cheese_transportN)
print(Eggs_transportN)
print(Fish_transportN)
print(Cereals_transportN)
print(Fruits_transportN)
print(Pulses_transportN)
print(Starchyroots_transportN)
print(Vegetables_transportN)
print(Stimulants_transportN)
print(Oilcrops_transportN)
print(Sugarcrops_transportN)
print(Nuts_transportN)
print(Spices_transportN)
print(Beverages_transportN)


#Calculate Total transport N
total_transport_N <- sum(Poultry_transportN + Bovine_transportN + Pigmeat_transportN + Milk_transportN + Cheese_transportN + 
                         Eggs_transportN + Fish_transportN + Cereals_transportN + Fruits_transportN + Pulses_transportN + 
                         Starchyroots_transportN + Vegetables_transportN + Stimulants_transportN + 
                         Oilcrops_virtualN + Sugarcrops_virtualN + Nuts_virtualN + 
                         Spices_virtualN + Beverages_virtualN)
print(total_transport_N)



#Total N in Food Calculation####

#Total Nitrogen in Food Calculation
#' Institution - Total Transport N
#'
#' @param Food_Category - kind of food
#' @param N_in_Food- total nitrogen in food contents
#' @param Institution_Population- population value for Loyola 
#' @param 
#'
#'
#' @return  Total Nitrogen in Food
#' @export 
#'
#' @examples
#' 

calc_TotalNinFood <- function(Food_Category, Institution_Population) {
  UVA_Population <- 34056  # UVA population is a constant 
  if (Food_Category == "Poultry") {
    NinFood <- (10273 * Institution_Population / UVA_Population) # 10273 = UVA Total NinFood for poultry
  }
  else if (Food_Category == "Bovine") {
    NinFood <- (8752 * Institution_Population / UVA_Population) # 8752 = UVA Total NinFood for Bovine
  }
  else if (Food_Category == "Pigmeat") {
    NinFood <- (6148 * Institution_Population / UVA_Population) #6148  = UVA Total NinFood for pigmeat
  }
  else if (Food_Category == "Milk") {
    NinFood <- (3713 * Institution_Population / UVA_Population) # 3713 = UVA Total NinFood for milk
  }
  else if (Food_Category == "Cheese") {
    NinFood <- (6042 * Institution_Population / UVA_Population) # 6042 = UVA Total NinFood for cheese
  }
  else if (Food_Category == "Eggs") {
    NinFood <- ( 4078* Institution_Population / UVA_Population) # 4078 = UVA Total NinFood for eggs
  }
  else if (Food_Category == "Fish") {
    NinFood <- (938 * Institution_Population / UVA_Population) # 938 = UVA Total NinFood for fish
  }
  else if (Food_Category == "Cereals") {
    NinFood <- (4759 * Institution_Population / UVA_Population) # 4759 = UVA Total NinFood for cereals
  }
  else if (Food_Category == "Fruits") {
    NinFood <- (548 * Institution_Population / UVA_Population) # 548 = UVA Total NinFood for fruits
  }
  else if (Food_Category == "Pulses") {
    NinFood <- (673 * Institution_Population / UVA_Population) # 673 = UVA Total NinFood for pulses
  }
  else if (Food_Category == "Starchy Roots") {
    NinFood <- (837 * Institution_Population / UVA_Population) # 837 = UVA Total NinFood for starchy roots
  }
  else if (Food_Category == "Vegetables") {
    NinFood <- (586 * Institution_Population / UVA_Population) # 586 = UVA Total NinFood for vegetables
  }
  else if (Food_Category == "Stimulants") {
    NinFood <- (9 * Institution_Population / UVA_Population) # 9 = UVA Total NinFood for stimulants
  }
  else if (Food_Category == "Oilcrops") {
    NinFood <- (1019 * Institution_Population / UVA_Population) # 1019 = UVA Total NinFood for oilcrops
  }
  else if (Food_Category == "Sugarcrops") {
    NinFood <- (110 * Institution_Population / UVA_Population) # 110 = UVA Total NinFood for sugarcrops
  }
  else if (Food_Category == "Nuts") {
    NinFood <- (230 * Institution_Population / UVA_Population) # 230 = UVA Total NinFood for nuts
  }
  else if (Food_Category == "Spices") {
    NinFood <- (117 * Institution_Population / UVA_Population) # 117 = UVA Total NinFood for spices
  }
  else if (Food_Category == "Beverages") {
    NinFood <- (83 * Institution_Population / UVA_Population) # 83 = UVA Total NinFood for beverages
  }
  
  return(NinFood)
}

Poultry_NinFood <- calc_TotalNinFood("Poultry", 20000)
Bovine_NinFood <- calc_TotalNinFood("Bovine", 20000)
Pigmeat_NinFood <- calc_TotalNinFood("Pigmeat", 20000)
Milk_NinFood <- calc_TotalNinFood("Milk", 20000)
Cheese_NinFood <- calc_TotalNinFood("Cheese", 20000)
Eggs_NinFood <- calc_TotalNinFood("Eggs", 20000)
Fish_NinFood <- calc_TotalNinFood("Fish", 20000)
Cereals_NinFood <- calc_TotalNinFood("Cereals", 20000)
Fruits_NinFood <- calc_TotalNinFood("Fruits", 20000)
Pulses_NinFood <- calc_TotalNinFood("Pulses", 20000)
Starchyroots_NinFood <- calc_TotalNinFood("Starchy Roots", 20000)
Vegetables_NinFood <- calc_TotalNinFood("Vegetables", 20000)
Stimulants_NinFood <- calc_TotalNinFood("Stimulants", 20000)
Oilcrops_NinFood <- calc_TotalNinFood("Oilcrops", 20000)
Sugarcrops_NinFood <- calc_TotalNinFood("Sugarcrops", 20000)
Nuts_NinFood <- calc_TotalNinFood("Nuts", 20000)
Spices_NinFood <- calc_TotalNinFood("Spices", 20000)
Beverages_NinFood <- calc_TotalNinFood("Beverages", 20000)

print(Poultry_NinFood)
print(Bovine_NinFood)
print(Pigmeat_NinFood)
print(Milk_NinFood)
print(Cheese_NinFood)
print(Eggs_NinFood)
print(Fish_NinFood)
print(Cereals_NinFood)
print(Fruits_NinFood)
print(Pulses_NinFood)
print(Starchyroots_NinFood)
print(Vegetables_NinFood)
print(Stimulants_NinFood)
print(Oilcrops_NinFood)
print(Sugarcrops_NinFood)
print(Nuts_NinFood)
print(Spices_NinFood)
print(Beverages_NinFood)

# Calculate Total Nitrogen inFood
total_NinFood <- sum(Poultry_NinFood + Bovine_NinFood + Pigmeat_NinFood + Milk_NinFood + Cheese_NinFood + 
                       Eggs_NinFood + Fish_NinFood + Cereals_NinFood + Fruits_NinFood + Pulses_NinFood + 
                       Starchyroots_NinFood + Vegetables_NinFood + Stimulants_NinFood + 
                       Oilcrops_NinFood + Sugarcrops_NinFood + Nuts_NinFood + 
                       Spices_NinFood + Beverages_NinFood)
print(total_NinFood)




#Total N Released####
Poultry_total_N <- sum(Poultry_virtualN, Poultry_transportN)
Bovine_total_N <- sum(Bovine_virtualN, Bovine_transportN)
Pigmeat_total_N <- sum(Pigmeat_virtualN, Pigmeat_transportN)
Milk_total_N <- sum(Milk_virtualN, Milk_transportN)
Cheese_total_N <- sum(Cheese_virtualN, Cheese_transportN)
Eggs_total_N <- sum(Eggs_virtualN, Eggs_transportN)
Fish_total_N <- sum(Fish_virtualN, Fish_transportN)
Cereals_total_N <- sum(Cereals_virtualN, Cereals_transportN)
Fruits_total_N <- sum(Fruits_virtualN, Fruits_transportN)
Pulses_total_N <- sum(Pulses_virtualN, Pulses_transportN)
Starchyroots_total_N <- sum(Starchyroots_virtualN, Starchyroots_transportN)
Vegetables_total_N <- sum(Vegetables_virtualN, Vegetables_transportN)
Stimulants_total_N <- sum(Stimulants_virtualN, Stimulants_transportN)
Oilcrops_total_N <- sum(Oilcrops_virtualN, Oilcrops_transportN)
Sugarcrops_total_N <- sum(Sugarcrops_virtualN, Sugarcrops_transportN)
Nuts_total_N <- sum(Nuts_virtualN, Nuts_transportN)
Spices_total_N <- sum(Spices_virtualN, Spices_transportN)
Beverages_total_N <- sum(Beverages_virtualN, Beverages_transportN)


print(Poultry_total_N)
print(Bovine_total_N)
print(Pigmeat_total_N)
print(Milk_total_N)
print(Cheese_total_N)
print(Eggs_total_N)
print(Fish_total_N)
print(Cereals_total_N)
print(Fruits_total_N)
print(Pulses_total_N)
print(Starchyroots_total_N)
print(Vegetables_total_N)
print(Stimulants_total_N)
print(Oilcrops_total_N)
print(Sugarcrops_total_N)
print(Nuts_total_N)
print(Spices_total_N)
print(Beverages_total_N)

total_N_food <- sum(Poultry_total_N + Bovine_total_N + Pigmeat_total_N + Milk_total_N + Cheese_total_N + 
Eggs_total_N + Fish_total_N + Cereals_total_N + Fruits_total_N + Pulses_total_N + 
Starchyroots_total_N + Vegetables_total_N + Stimulants__total_N + 
Oilcrops_total_N + Sugarcrops_total_N + Nuts_total_N + 
Spices_total_N + Beverages_total_N)

# Print the total N
print(total_N_food)