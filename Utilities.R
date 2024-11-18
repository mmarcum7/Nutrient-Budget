#' Get Unit Type for Fuel Type- Utilities - Source: On-site stationary
#'
#'
#' @param fuel_type - Type of fuel (Mix, Distillate Oil (#1-4), Natural Gas, Propane, Coal, Renewables)
#'
#' @return  This function returns the unit associated with a given fuel type
#' 
#' @export
#'
#' @examples
#'

unit_type <- function(fuel_type) {
  ifelse(fuel_type == "Mix", "kWh",
         ifelse(fuel_type == "Distillate Oil (#1-4)", "Gallons",
                ifelse(fuel_type == "Natural Gas", "MMBtu",
                       ifelse(fuel_type == "Propane", "Gallons",
                              ifelse(fuel_type == "Coal", "Short Ton")))))
}
Mix_Unit_Type <- unit_type("Mix")
Distillate_Oil_Unit_Type <- unit_type("Distillate Oil (#1-4)")
Natural_Gas_Unit_Type <- unit_type("Natural Gas")
Propane_Unit_Type <- unit_type("Propane")
Coal_Unit_Type <- unit_type("Coal")

print(Mix_Unit_Type) 
print(Distillate_Oil_Unit_Type) 
print(Natural_Gas_Unit_Type) 
print(Propane_Unit_Type) 
print(Coal_Unit_Type)



###################################################################################################################################
#' N2O Emission Factors for Fuel Type- Utilities - Source: On-site stationary & Source: Purchased electricity (mix) 
#'
#'
#' @param fuel_type - Type of fuel (Mix, Distillate Oil (#1-4), Natural Gas, Propane, Coal, Renewables)
#'
#' @return This function returns the emission factor of N2O associated with a given fuel type.
#' 
#' @export
#'
n2o_emission_factor <- function(fuel_type) {
  if (fuel_type == "Mix") {
    return(0.000009)
  } else if (fuel_type == "Distillate Oil (#1-4)") {
    return(0.000087)
  } else if (fuel_type == "Natural Gas") {
    return(0.000106)
  } else if (fuel_type == "Propane") {
    return(0.000055)
  } else if (fuel_type == "Coal") {
    return(0.029950)
  } else if (fuel_type == "Renewables") {
    return(0)
  }
}


Mix_N2O_Emission_Factor <- n2o_emission_factor("Mix")
Distillate_Oil_N2O_Emission_Factor <- n2o_emission_factor("Distillate Oil (#1-4)")
Natural_Gas_N2O_Emission_Factor <- n2o_emission_factor("Natural Gas")
Propane_N2O_Emission_Factor <- n2o_emission_factor("Propane")
Coal_N2O_Emission_Factor <- n2o_emission_factor("Coal")
Renewables_N2O_Emission_Factor <- n2o_emission_factor("Renewables")


# Printing the results for all examples
print(Mix_N2O_Emission_Factor) 
print(Distillate_Oil_N2O_Emission_Factor) 
print(Natural_Gas_N2O_Emission_Factor) 
print(Propane_N2O_Emission_Factor) 
print(Coal_N2O_Emission_Factor) 
print(Renewables_N2O_Emission_Factor) 


###################################################################################################################################
#' Calculate N Released as N2O Utilities - Source: On-site stationary
#'
#' @param fuel_type - Type of fuel (Mix, Distillate Oil (#1-4), Natural Gas, Propane, Coal, Renewables)
#' @param amount - Amount of fuel (in appropriate units, e.g., tons, liters)
#' 
#' @return This function calculates N released from N2O
#' 
#' @export
#'
n_released_as_n2o_on_site <- function(fuel_type, amount) {
  # Get the N2O emission factor based on fuel type
  emission_factor <- n2o_emission_factor(fuel_type)
  
  n2o_released <- amount * emission_factor
  
  return(n2o_released)
}
# Example usage for calculating N2O released on-site for different fuel types
Mix_n_released_as_N2O_on_site <- n_released_as_n2o_on_site("Mix", 1000)
Distillate_Oil_n_released_as_N2O_on_site <- n_released_as_n2o_on_site("Distillate Oil (#1-4)", 1000)
Natural_Gas_n_released_as_N2O_on_site <- n_released_as_n2o_on_site("Natural Gas", 1000)
Propane_n_released_as_N2O_on_site <- n_released_as_n2o_on_site("Propane", 1000)
Coal_n_released_as_N2O_on_site <- n_released_as_n2o_on_site("Coal", 1000)
Renewables_n_released_as_N2O_on_site <- n_released_as_n2o_on_site("Renewables", 1000)

# Print the results
print(Mix_n_released_as_N2O_on_site) 
print(Distillate_Oil_n_released_as_N2O_on_site) 
print(Natural_Gas_n_released_as_N2O_on_site) 
print(Propane_n_released_as_N2O_on_site) 
print(Coal_n_released_as_N2O_on_site) 
print(Renewables_n_released_as_N2O_on_site)



###################################################################################################################################
#' NOx Emission Factors Utilities - Source: On-site stationary 
#'
#'
#' @param fuel_type - Type of fuel (Mix, Distillate Oil (#1-4), Natural Gas, Propane, Coal, Renewables)
#'
#' @return This function returns the emission factor of NOx associated with a given fuel type.
#' 
#' @export
#'
nox_emission_factor_on_site <- function(fuel_type) {

  if (fuel_type == "Natural Gas") {
    return(0.002917)
  } else if (fuel_type == "Coal") {
    return(11)
  } else if (fuel_type == "Nuclear") {
    return(0)
  } else if (fuel_type == "Distillate Oil (#1-4)") {
    return(0.004536)
  } else if (fuel_type == "Propane") {
    return(0.014000)
  } else if (fuel_type == "Renewables") {
    return(0)
  }

}
#Examples
Mix_Nox_Emission_Factor_on_site <- nox_emission_factor("Mix")
Distillate_Oil_Nox_Emission_Factor_on_site <- nox_emission_factor("Distillate Oil (#1-4)")
Natural_Gas_Nox_Emission_Factor_on_site <- nox_emission_factor("Natural Gas")
Propane_Nox_Emission_Factor_on_site <- nox_emission_factor("Propane")
Coal_Nox_Emission_Factor_on_site <- nox_emission_factor("Coal")
Renewables_Nox_Emission_Factor_on_site <- nox_emission_factor("Renewables")

# Printing the results for all fuel types 
print(Mix_Nox_Emission_Factor_on_site) 
print(Distillate_Oil_Nox_Emission_Factor_on_site) 
print(Natural_Gas_Nox_Emission_Factor_on_site) 
print(Propane_Nox_Emission_Factor_on_site) 
print(Coal_Nox_Emission_Factor_on_site) 
print(Renewables_Nox_Emission_Factor_on_site)



###################################################################################################################################

#' Calculate NOx Emission Utilities - Source: On-site stationary
#'
#' @param fuel_type - Type of fuel (Mix, Distillate Oil (#1-4), Natural Gas, Propane, Coal, Renewables)
#' @param amount - Amount of fuel (in appropriate units, e.g., tons, liters)
#' 
#' @return This function calculates the amount of NOx emission 
#' 
#' @export
#'
NOx_emissions_on_site <- function(fuel_type, amount) {
  # Get the NOx emission factor based on fuel type
  emission_factor <- nox_emission_factor(fuel_type)
  
  # Calculate the utility N released as NOx
  nox_released <- amount * emission_factor
  
  return(nox_released)
}

# Example usage with an amount of 1000 for each fuel type
Mix_Nox_emissions_on_site <- NOx_emissions_on_site("Mix", 1000)
Distillate_Oil_Nox_emissions_on_site <- NOx_emissions_on_site("Distillate Oil (#1-4)", 1000)
Natural_Gas_Nox_emissions_on_site <- NOx_emissions_on_site("Natural Gas", 1000)
Propane_Nox_emissions_on_site <- NOx_emissions_on_site("Propane", 1000)
Coal_Nox_emissions_on_site <- NOx_emissions_on_site("Coal", 1000)
Renewables_Nox_emissions_on_site <- NOx_emissions_on_site("Renewables", 1000)

# Print the results
print(Mix_Nox_emissions_on_site) 
print(Distillate_Oil_Nox_emissions_on_site) 
print(Natural_Gas_Nox_emissions_on_site) 
print(Propane_Nox_emissions_on_site) 
print(Coal_Nox_emissions_on_site) 
print(Renewables_Nox_emissions_on_site)





###################################################################################################################################
#' Calculate N Released- Utilities - Source: On-site stationary
#'
#' @param fuel_type - Type of fuel (Mix, Distillate Oil (#1-4), Natural Gas, Propane, Coal, Renewables)
#' @param n_released_as_n2o - Amount of N2O released
#' @param nox_emission - Amount of NOx released
#' 
#' @return This function calculates the total N released from a fuel.
#' 
#' @export
#'
calculate_n_released_on_site <- function(fuel_type, n_released_as_n2o, nox_emission) {
  if (fuel_type == "Distillate Oil (#1-4)") {
    result <- n_released_as_n2o * 0.64 + nox_emission * 0.30
  } else {
    result <- n_released_as_n2o * 0.64 + nox_emission * 0.30 * 0.453592
  }
  return(result)
}

distillate_oil_n_released_on_site<-calculate_n_released_on_site("Distillate Oil (#1-4)", Distillate_Oil_n_released_as_n2o, Distillate_Oil_Nox_emissions)
print(distillate_oil_n_released_on_site)



###################################################################################################################################
#' Calculate N Released as N2O Utilities - Source: Purchased electricity (mix) 
#'
#' @param total_fuel_type - Total amount of fuel used (e.g., in kWh, gallons, etc.)
#' @param contribution_fuel_type - Percentage contribution of each fuel type (e.g., 0.5 for 50%)
#' @param fuel_type - Type of fuel (Mix, Distillate Oil (#1-4), Natural Gas, Propane, Coal, Nuclear)
#' 
#' @return This function calculates the amount of nitrogen (N) released as N2O based on the fuel type and its contribution.
#' 
#' @export
#'
N_Released_as_N2O_purchased <- function(total_fuel_type, contribution_fuel_type, fuel_type) {
  # Get the N2O emission factor based on the fuel type
  emission_factor <- n2o_emission_factor(fuel_type)
  
  # Calculate N released as N2O
  n_released <- total_fuel_type * contribution_fuel_type * emission_factor
  
  return(n_released)
}

#Example
total_fuel_type <- 5000  
contribution_fuel_type <- 0.5  

Mix_N_Released_as_N2O_purchased <- N_Released_as_N2O_purchased(total_fuel_type, contribution_fuel_type, "Mix")
Distillate_Oil_N_Released_as_N2O_purchased <- N_Released_as_N2O_purchased(total_fuel_type, contribution_fuel_type, "Distillate Oil (#1-4)")
Natural_Gas_N_Released_as_N2O_purchased <- N_Released_as_N2O_purchased(total_fuel_type, contribution_fuel_type, "Natural Gas")
Propane_N_Released_as_N2O_purchased <- N_Released_as_N2O_purchased(total_fuel_type, contribution_fuel_type, "Propane")
Coal_N_Released_as_N2O_purchased <- N_Released_as_N2O_purchased(total_fuel_type, contribution_fuel_type, "Coal")
Nuclear_N_Released_as_N2O_purchased <- N_Released_as_N2O_purchased(total_fuel_type, contribution_fuel_type, "Nuclear")

# Print the results for all fuel types
print(Mix_N_Released_as_N2O_purchased)
print(Distillate_Oil_N_Released_as_N2O_purchased)
print(Natural_Gas_N_Released_as_N2O_purchased)
print(Propane_N_Released_as_N2O_purchased)
print(Coal_N_Released_as_N2O_purchased)
print(Nuclear_N_Released_as_N2O_purchased)


###################################################################################################################################
#' Calculate NOx Emission Factors Utilities -Source: Purchased electricity (mix) 
#'
#'
#' @param fuel_type - Type of fuel (Natural Gas, Coal, Nuclear, Biomass)
#'
#' @return This function returns the emission factor of NOx associated with a given fuel type.
#' 
#' @export
#'
nox_emission_factor_purchased <- function(fuel_type) {
  if (fuel_type == "Natural Gas") {
    return(0.002917)
  } else if (fuel_type == "Coal") {
    return(0.008663)
  } else if (fuel_type == "Nuclear") {
    return(0)
  } else if (fuel_type == "Biomass") {
    return(0.001102)
  }
  
}
#Examples
Natural_Gas_Nox_Emission_Factor_purchased <- nox_emission_factor_purchased("Natural Gas")
Coal_Nox_Emission_Factor_purchased <- nox_emission_factor_purchased("Coal")
Nuclear_Nox_Emission_Factor_purchased <- nox_emission_factor_purchased("Nuclear")
Biomass_Nox_Emission_Factor_purchased <- nox_emission_factor_purchased("Biomass")

print(Natural_Gas_Nox_Emission_Factor_purchased)
print(Coal_Nox_Emission_Factor_purchased)
print(Nuclear_Nox_Emission_Factor_purchased)
print(Biomass_Nox_Emission_Factor_purchased)



###################################################################################################################################
#' Calculate NOx Emission Factors Utilities -Source: Purchased electricity (mix) 
#'
#'
#' @param fuel_type - Type of fuel 
#' @param total_fuel_type - Total amount of fuel used (in kWh, gallons, etc.)
#' @param contribution_fuel_type - Percentage contribution of each fuel type (e.g., 0.5 for 50%)
#'
#' @return This function calculates the amount of NOx emissions for purchased electricity based on the fuel type and its contribution.
#' 
#' @export
# Calculate NOx emissions for purchased electricity

NOx_emissions_purchased <- function(fuel_type, total_fuel_type, contribution_fuel_type) {
  # Get the NOx emission factor for the given fuel type
  emission_factor <- nox_emission_factor_purchased(fuel_type)
  
  # Calculate NOx emissions
  nox_emissions <- emission_factor * total_fuel_type * contribution_fuel_type
  
  return(nox_emissions)
}

# Example usage with 1000 total fuel and 25% contribution for each fuel type
Natural_Gas_Nox_emissions_purchased <- NOx_emissions_purchased("Natural Gas", 1000, 0.25)  
Coal_Nox_emissions_purchased <- NOx_emissions_purchased("Coal", 1000, 0.25)  
Nuclear_Nox_emissions_purchased <- NOx_emissions_purchased("Nuclear", 1000, 0.25)  
Biomass_Nox_emissions_purchased <- NOx_emissions_purchased("Biomass", 1000, 0.25)  

# Print the results
print(Natural_Gas_Nox_emissions_purchased)
print(Coal_Nox_emissions_purchased)
print(Nuclear_Nox_emissions_purchased)
print(Biomass_Nox_emissions_purchased)


###################################################################################################################################
#' Calculate N Released - Utilities - Source: Purchased electricity (mix)
#'
#' @param fuel_type Type of fuel (e.g., "Natural Gas", "Coal", etc.)
#' @param N_Released_as_N2O_purchased Total nitrogen released as N2O from purchased electricity.
#' @param NOx_emissions_purchased Total NOx emissions from purchased electricity.
#' 
#' @return Total nitrogen released from purchased electricity, combining contributions from N2O and NOx emissions.
#' 
#' @export
#'

N_released_purchased <- function(N_Released_as_N2O_purchased, NOx_emissions_purchased) {
  N_Released_as_N2O_purchased * 0.64 + 0.453592 * NOx_emissions_purchased * 0.30
}


# Example calculations
Natural_Gas_N_Released <- N_released_purchased(Natural_Gas_N_Released_as_N2O_purchased, Natural_Gas_Nox_Emission_Factor_purchased)  
Coal_N_Released <- N_released_purchased(Coal_N_Released_as_N2O_purchased, Coal_Nox_Emission_Factor_purchased)
Nuclear_N_Released <- N_released_purchased(Nuclear_N_Released_as_N2O_purchased, Nuclear_Nox_Emission_Factor_purchased)


# Print the results
print(Natural_Gas_N_Released)
print(Coal_N_Released)
print(Nuclear_N_Released)
