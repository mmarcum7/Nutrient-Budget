#Example Nitrogen Transportation Dataframe
NitrogenTransportation.df <- read.csv("",stringsAsFactors = TRUE)

calc_n20 <- function
  Vehicle_type, Fuel_volume,Fuel_Consumption) {
}if (vehicle_type == "Institute car") {
  Fuel_volume * 0.0006
  
} else if (vehicle_type == "Institute van") {
  Fuel_volume * x
  
} else if (vehicle_type == "Institute biodiesel bus"){
  Fuel_volume * 0.000675
  
} else if (vehicle_type == "Institute bus"){
  Fuel_volume * 0.00649
  
} else if (vehicle_type == "Commuting Student Alone"
  Fuel_Consumption * 0.0006
           
} else if (vehicle_type == "Commuting Student Carpool"
  Fuel_Consumption * 0.006
                      
} else if (vehicle_type == "Commuting Student Bus"
   Fuel Consumption * 0.003
                                 
} else if (vehicle_type == "Commuting Staff Alone"
    Fuel Consumption * 0.0006
                                            
} else if (vehicle_type == "Commuting Staff Carpool"
    Fuel Consumption * 0.0006
                                                       
} else if (vehicle_type == "Commuting Staff Bus"
    Fuel_Consumption * 0.003
                                                                  
} else if (vehicle_type == "Commuting Faculty Alone"
    Fuel_Consumption * 0.0006
                                                                             
} else if (vehicle_type == "Commuting Faculty Carpool"
    Fuel_Consumption * 0.0006
                                                                                        
} else if (vehicle_type == "Commuting Faculty Bus"
    Fuel_Consumption * 0.003
                                                                                                   
                                                                                                   
 
    
    #########################################################################################################
    #' Title: Fuel Consumption
    #'
    #' @param fuel_efficiency -average fuel efficiency in miles per gallon  
    #' @param total_distance -total distance traveled in miles
    #'
    #'
    #' @return fuel_consumption in gallons
    #' @export 
    #'
    #' @examples
    #' fuel_consumption(fuel_efficiency= , total_distance=)                                                                                                      
calc_FuelConsumption <- function(vehicle_type,
  Total_Distance)
                                                                                                     
} if (vehicle_type == "Commuting Student Alone")
(1/24.2) * Total_Distance
                                                                                                           
} else if (vehicle_type == "Commuting Student Carpool")
(1/24.2) * Total_Distance
                                                                                                                      
} else if (vehicle_type == "Commuting Student Bus")
                                                                                                                                 
} else if (vehicle_type == "Commuting Staff Alone"
(1/22.1) * Total_Distance
                                                                                                                                            
} else if (vehicle_type == "Commuting Staff Carpool"
(1/22.1) * Total_Distance
                                                                                                                                                       
} else if (vehicle_type == "Commuting Staff Bus"
(1/39.7) * Total Distance
                                                                                                                                                                  
} else if (vehicle_type == "Commuting Faculty Alone"
(1/22.1) * Total_Distance
                                                                                                                                                                             
} else if (vehicle_type == "Commuting Faculty Carpool"
(1/22.1) * Total_Distance
                                                                                                                                                                                        
} else if (vehicle_type == "Commuting Faculty Bus") 
(1/39.7) * Total_Distance
                                                                                                                                                                                                   


calc_NOx <- function (vehicle_type, Fuel_Volume)
  if (vehicle type == "Institustional car")
    Fuel_Volume * 22.1 * 0.000593

else if (vehicle type == "Institutional Van")
  Fuel_Volume * x * y

else if (vehicle_type == "Institutional Bus")
  Fuel_Volume * 6.9 * 0.00028

else if (vehicle type == "Instiutional Biodiesel Bus")
  Fuel_Volume * 0.45 * 0.00649

else if (vehicle type == "Commuting Student Alone"
  Fuel_Consumption * 0.00059 * 24.2 
         
else if (vehicle type == "Commuting Student Carpool")
  Fuel_Consumption * 0.00059 * 24.2 
         
else if (vehicle type == "Commuting Student Bus")
  Fuel_Consumption * 0.00649 * 31.9 
         
else if (vehicle type == "Commuting Staff Alone")
  Fuel_Consumption * 0.00059 * 22.1
         
else if (vehicle type == "Commuting Staff Carpool")
  Fuel_Consumption * 0.00059 * 22.1
         
else if (vehicle type == "Commuting Staff Bus")
  Fuel_Consumption * 0.00649  * 39.7
         
else if (vehicle type == "Commuting Faculty Alone")
 Fuel_Consumption * 0.00059 * 22.1
         
else if (vehicle type == "Commuting Faculty Carpool")
  Fuel_Consumption * 0.00059 * 22.1
         
else if (vehicle type == "Commuting Faculty Bus")
  Fuel_Consumption * 0.00649  * 39.7
         
         
         
         
         
         
         
         
         
         
         
#' Title Total Distance
#' @param individuals -# of individuals  
#' @param commuting -% commuting by vehicle type
#' @param trips -# of trips 
#' @param days  -# of commuting days 
#' @param miles -average miles per trip
#' @param mode -alone, carpool, bus
#'
#'
#' @return total distance in miles  
#' @export 
#'
#' @examples
#' total_distance(individuals = 100, commuting_rate = 0.5, trips = 270, days = 250, miles = 10, mode = "carpool")
         
         
         
total_distance <- function(individuals, commuting_rate, trips, days, miles, mode = "alone") {
  if (mode == "alone") {
   distance <- individuals * commuting_rate * trips * days * miles
} else if (mode == "carpool") {
   distance <- (individuals * commuting_rate * trips * days * miles) / 2  
} else if (mode == "bus") {
   distance <- individuals * commuting_rate * trips * days * miles
           } 
           
           
return(distance)
}
         
 #########################################################################################################
#' Title: Fuel Consumption
#'
#' @param fuel_efficiency -average fuel efficiency in miles per gallon  
#' @param total_distance -total distance traveled in miles
#'
#'
#' @return fuel_consumption in gallons
#' @export 
#'
#' @examples
#' fuel_consumption(fuel_efficiency= , total_distance=)
         
fuel_consumption <- function(fuel_efficiency, total_distance) {
  if(fuel_efficiency =0) {
  return(0)
} else {
  consumption <- total_distance/fuel_efficiency
  return(consumption)
}
           
           
           
           
           #'Title: N Released as N2O
           #' @param fuel_consumption  -fuel consumption in gallons 
           #' @param N2O_emission_factors -emission factor for N2O
           #'
           #'
           #' @return 
           #' @export  N_released_as_N2O- N Released as N2O in kg N2O
           #'
           #' @examples
           #'  
           N_released_as_N2O <- function(fuel_consumption, N2O_emission_factor){
             N_released_N2O <-fuel_consumption * N2O_emission_factor # if drive alone/carpool  N2O_emission_factor= 0.0006; if bus N2O emissions=0.0003
             return(N2O_released)
           } 
           
           `
           #' Title:Calculate N Released as NOx
           #'
           #' @param fuel_consumption -fuel consumption in gallons
           #' @param NOx_emission_factor -emission factor for NOx in kg NOx/mi 
           #' @param average_fuel_input Average fuel input 
           #'
           #' @return Total NOx released in grams
           #' @export
           #'
           #' @examples
           #' 
           
           N_released_as_NOx <- function(fuel_consumption, NOx_emission_factor, average_fuel_input) {
             
             N_released_NOx <- fuel_consumption * NOx_emission_factor * average_fuel_input #if drive alone/carpool NOx_emission_factor=0.00059, if bus NOx_emission_factor=0.00649 
             
             return( N_released_NOx)
           }
           
           
           
           #'Title:Total N released 
           #' @param N_released_as_N2O-  N Released as N2O in kg 
           #' @param 0.63636- Conversion from N2O to N value from Inst.
           #' @param 0.30435- Conversion from NOx (NO2) to N from Inst.
           #' @param  N_released_NOx- emission factor for NOx in kg NOx/mi
           #'
           #' @return 
           #' @export  Total_N_Released
           #'
           #' @examples
           
           Total_N_Released <- function(N_released_N2O, 0.63636, N_released_NOx,0.30435)){
  total_nitrogen <- (N_released_as_N2O * 0.63636) + (N_released_as_NOx * 0.30435)
  Return(total_nitrogen)
}










#Fertilizer Calculations####
NitrogenFertilizer.df <- read.csv("",stringsAsFactors = TRUE)
TurfgrassUptake <- #Average fertilizer uptake of Turfgrass
ShrubUptake <- #Average fertilizer uptake of Shrubs
TreeUptake <- #Average fertilizer uptake of Trees
Nreleased1 <- NitrogenFertilizer.df["Amount of Nitrogen","Fertilizer 1"] * (1-TurfgrassUptake) * 0.453592 #convert from pounds ot kgs
#same caculations for all fertilizers, just need appropriate uptake factor and the amount of nitrogen in that specific fertilizer
#Ten percent of fertilizer voilitates into air