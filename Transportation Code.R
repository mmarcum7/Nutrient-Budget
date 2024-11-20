#Distance Calculation####
#' Title Total Distance
#'
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

  
calc_TotalDistance <- function(vehicle_type){
  individuals <- c(
    commuting_studentalone = 100,
    commuting_studentcarpool = 100,
    commuting_studentbus = 100,
    commuting_staffalone = 100,
    commuting_staffcarpool = 100,
    commuting_staffbus = 100,
    commuting_facultyalone = 100,
    commuting_facultycarpool = 100,
    commuting_facultybus = 100
  )
  
  commuting_rate <- c(
    commuting_studentalone = 0.25,
    commuting_studentcarpool = 0.25,
    commuting_studentbus = 0.25,
    commuting_staffalone = 0.25,
    commuting_staffcarpool = 0.25,
    commuting_staffbus = 0.25,
    commuting_facultyalone = 0.25,
    commuting_facultycarpool = 0.25,
    commuting_facultybus = 0.25
  )
  
  trips <- c(
    commuting_studentalone = 200,
    commuting_studentcarpool = 200,
    commuting_studentbus = 200,
    commuting_staffalone = 200,
    commuting_staffcarpool = 200,
    commuting_staffbus = 200,
    commuting_facultyalone = 200,
    commuting_facultycarpool = 200,
    commuting_facultybus = 200
  )
  
  days <- c(
    commuting_studentalone = 365,
    commuting_studentcarpool = 365,
    commuting_studentbus = 365,
    commuting_staffalone = 365,
    commuting_staffcarpool = 365,
    commuting_staffbus = 365,
    commuting_facultyalone = 365,
    commuting_facultycarpool = 365,
    commuting_facultybus = 365
  )
  
  miles <- c(
    commuting_studentalone = 10000,
    commuting_studentcarpool = 10000,
    commuting_studentbus = 10000,
    commuting_staffalone = 10000,
    commuting_staffcarpool = 10000,
    commuting_staffbus = 10000,
    commuting_facultyalone = 10000,
    commuting_facultycarpool = 10000,
    commuting_facultybus = 10000
  )
  
    if (vehicle_type == "commuting_studentalone" ||
       vehicle_type == "commuting_staffalone"    ||
       vehicle_type == "commuting_facultyalone"  ||
       vehicle_type == "commuting_studentbus"    ||
       vehicle_type == "commuting_staffbus"      ||
       vehicle_type == "commuting_facultybus") {
    
     distance <- individuals[vehicle_type] * commuting_rate[vehicle_type] * trips[vehicle_type] *
       days[vehicle_type] * miles[vehicle_type]
    
    
  } else if (vehicle_type == "commuting_studentcarpool" ||
             vehicle_type == "commuting_staffcarpool"   ||
             vehicle_type == "commuting_facultycarpool") {
    distance <- individuals[vehicle_type] * commuting_rate[vehicle_type] * trips[vehicle_type] *
      days[vehicle_type] * miles[vehicle_type] / 2
 
}  
return(distance)
}
 
 
Commuting_student_distance <- calc_TotalDistance("commuting_studentalone")
Commuting_studentcarpool_distance <-  calc_TotalDistance("commuting_studentcarpool")
Commuting_studentbus_distance <-  calc_TotalDistance("commuting_studentbus")
Commuting_staff_distance <-  calc_TotalDistance("commuting_staffalone")
Commuting_staffcarpool_distance <-  calc_TotalDistance("commuting_staffcarpool")
Commuting_staffbus_distance <-  calc_TotalDistance("commuting_staffbus")
Commuting_faculty_distance <- calc_TotalDistance("commuting_facultyalone")
Commuting_facultycarpool_distance <-  calc_TotalDistance("commuting_facultycarpool")
Commuting_facultybus_distance <-  calc_TotalDistance("commuting_facultybus")


print(Commuting_student_distance)
print(Commuting_studentcarpool_distance)
print(Commuting_studentbus_distance)
print(Commuting_staff_distance)
print(Commuting_staffcarpool_distance)
print(Commuting_staffbus_distance)
print(Commuting_faculty_distance)
print(Commuting_facultycarpool_distance)
print(Commuting_facultybus_distance)
###################################################################################################################################################################################################################################



#Fuel Consumption Calculation####
#' Title: Fuel Consumption
#'
#' @param fuel_efficiency -average fuel efficiency in miles per gallon  
#' @param total_distance -total distance traveled in miles
#'
#'
#' @return fuel_consumption in gallons
#'     
#' @export 
#'
#' @examples
#

calc_FuelConsumption <- function(vehicle_type){

#Fuel Efficiency Factors from UVA
  fuelefficiency_factors <- c(
    commuting_studentalone = 24.2,
    commuting_studentcarpool = 24.2,
    commuting_studentbus = 39.7,
    commuting_staffalone = 22.1,
    commuting_staffcarpool = 22.1,
    commuting_staffbus = 39.7,
    commuting_facultyalone = 22.1,
    commuting_facultycarpool = 22.1,
    commuting_facultybus = 39.7
)

  distance <- c(
    commuting_studentalone = "Commuting_student_distance",
    commuting_studentcarpool = "Commuting_studentcarpool_distance",
    commuting_studentbus = "Commuting_studentbus_distance",
    commuting_staffalone = "Commuting_staff_distance",
    commuting_staffcarpool = "Commuting_staffcarpool_distance", 
    commuting_staffbus = "Commuting_staffbus_distance", 
    commuting_facultyalone = "Commuting_faculty_distance",
    commuting_facultycarpool = "Commuting_facultycarpool_distance",
    commuting_facultybus = "Commuting_facultybus_distance"
)

  fuel_consumption <- fuelefficiency_factors[vehicle_type] * get(distance[vehicle_type])
  
  return(fuel_consumption)
}    


Commuting_student_fuelconsumption <- calc_FuelConsumption("commuting_studentalone")
Commuting_studentcarpool_fuelconsumption <- calc_FuelConsumption("commuting_studentcarpool")
Commuting_studentbus_fuelconsumption <- calc_FuelConsumption("commuting_studentbus")
Commuting_staff_fuelconsumption <- calc_FuelConsumption("commuting_staffalone")
Commuting_staffcarpool_fuelconsumption <- calc_FuelConsumption("commuting_staffcarpool")
Commuting_staffbus_fuelconsumption <- calc_FuelConsumption("commuting_staffbus")
Commuting_faculty_fuelconsumption <-calc_FuelConsumption("commuting_facultyalone")
Commuting_facultycarpool_fuelconsumption <-calc_FuelConsumption("commuting_facultycarpool")
Commuting_facultybus_fuelconsumption <-calc_FuelConsumption("commuting_facultybus")


print(Commuting_student_fuelconsumption)
print(Commuting_studentcarpool_fuelconsumption)
print(Commuting_studentbus_fuelconsumption)
print(Commuting_staff_fuelconsumption)
print(Commuting_staffcarpool_fuelconsumption)
print(Commuting_staffbus_fuelconsumption)
print(Commuting_faculty_fuelconsumption)
print(Commuting_facultycarpool_fuelconsumption)
print(Commuting_facultybus_fuelconsumption)
###################################################################################################################################################################################################################################




#Institution- N2O Released and Total N2O of all Transportation Modes#####
#'Title:Total N2O released 
#' @param Vehicle_type - mode of transportation
#' @param Fuel_volume - in gallons, the amount of gasoline held in the vehicle
#' @param Fuel_Consumption - calculated for each commuter type before, amount of gasoline used in travel to commute
#'
#' @return Amount of N2O Released in kg
#' @export  Total_N2O_Released
#'
#' @examples


calc_N2O <- function(vehicle_type) {
  
  fuelemmission_factor <- c(
    institute_car = 0.0006,
    institute_van = 0.0006,
    institute_bus = 0.00649,
    institute_biodieselbus = 0.000675,
    commuting_studentalone = 0.0006,
    commuting_studentcarpool = 0.0006,
    commuting_studentbus = 0.003,
    commuting_staffalone = 0.006, 
    commuting_staffcarpool = 0.006, 
    commuting_staffbus = 0.003,
    commuting_facultyalone = 0.0006,
    commuting_facultycarpool = 0.0006,
    commuting_facultybus = 0.003
  )
  
  fuelvolume <- c(
    institute_car = 100,
    institute_van = 100,
    institute_bus = 100,
    institute_biodieselbus = 100
  )
  
  fuelconsumption <- c(
    commuting_studentalone = "Commuting_student_fuelconsumption",
    commuting_studentcarpool = "Commuting_studentcarpool_fuelconsumption", 
    commuting_studentbus = "Commuting_studentbus_fuelconsumption",
    commuting_staffalone = "Commuting_staff_fuelconsumption", 
    commuting_staffcarpool = "Commuting_staffcarpool_fuelconsumption",
    commuting_staffbus = "Commuting_staffbus_fuelconsumption",
    commuting_facultyalone = "Commuting_faculty_fuelconsumption",
    commuting_facultycarpool = "Commuting_facultycarpool_fuelconsumption", 
    commuting_facultybus = "Commuting_facultybus_fuelconsumption"
    
  )
  if(vehicle_type == "institute_car" ||
     vehicle_type == "institute_van" ||
     vehicle_type == "institute_bus" ||
     vehicle_type == "institute_biodieselbus"){
    
    N2O_released <- fuelemmission_factor[vehicle_type] * fuelvolume[vehicle_type]
    
 } else if (vehicle_type == "commuting_studentalone" ||
      vehicle_type == "commuting_studentcarpool"     ||
      vehicle_type == "commuting_studentbus"         ||
      vehicle_type == "commuting_staffalone"         ||
      vehicle_type == "commuting_staffcarpool"       ||
      vehicle_type == "commuting_staffbus"           ||
      vehicle_type == "commuting_facultyalone"       ||
      vehicle_type == "commuting_facultycarpool"     ||
      vehicle_type == "commuting_facultybus") {
      
     N2O_released <- fuelemmission_factor[vehicle_type] * get(fuelconsumption[vehicle_type])
      
  return(N2O_released)  
}
}

Institute_car_N2O <- calc_N2O("institute_car")
Institute_van_N2O <- calc_N2O("institute_van")
Institute_bus_N2O <- calc_N2O("institute_bus")
Institute_biodieselbus_N2O <- calc_N2O("institute_biodieselbus")
Commuting_studentalone_N2O <- calc_N2O("commuting_studentalone")
Commuting_studentcarpool_N2O <- calc_N2O("commuting_studentcarpool")
Commuting_studentbus_N2O <- calc_N2O("commuting_studentbus")
Commuting_staffalone_N2O <- calc_N2O("commuting_staffalone")
Commuting_staffcarpool_N2O <- calc_N2O("commuting_staffcarpool")
Commuting_staffbus_N2O <-calc_N2O("commuting_staffbus")
Commuting_facultyalone_N2O <-calc_N2O("commuting_facultyalone")
Commuting_facultycarpool_N2O <-calc_N2O("commuting_facultycarpool")
Commuting_facultybus_N2O <-calc_N2O("commuting_facultybus")

print(Institute_car_N2O)
print(Institute_van_N2O)
print(Institute_bus_N2O)
print(Institute_biodieselbus_N2O)
print(Commuting_studentalone_N2O)
print(Commuting_studentcarpool_N2O)
print(Commuting_studentbus_N2O)
print(Commuting_staffalone_N2O)
print(Commuting_staffcarpool_N2O)
print(Commuting_staffbus_N2O)
print(Commuting_facultyalone_N2O)
print(Commuting_facultycarpool_N2O)
print(Commuting_facultybus_N2O)



#Institution - Calculate the Total N2O released from vehicles
Total_N2O_Released <- sum(
Institute_car_N2O, Institute_van_N2O, Institute_bus_N2O, Institute_biodieselbus_N2O,
Commuting_studentalone_N2O, Commuting_studentcarpool_N2O, Commuting_studentbus_N2O,
Commuting_staffalone_N2O,Commuting_staffcarpool_N2O , Commuting_staffbus_N2O,
Commuting_facultyalone_N2O, Commuting_facultycarpool_N2O, Commuting_facultybus_N2O
)
print(Total_N2O_Released) 
###################################################################################################################################################################################################################################



#Institution - NOx Released and Total NOx of all Transportation Modes####
#' Title:Calculate N Released as NOx
#'
#' @param fuel_consumption -fuel consumption in gallons
#' @param NOx_emission_factor -emission factor for NOx in kg NOx/mi 
#' @param average_fuel_input Average fuel Efficiency in miles per gallon
#'
#' @return Total NOx released in grams
#' @export
#'
#' @examples


calc_NOx <- function (vehicle_type){
   
  nox_emmissionfactor <-c(
     institute_car = 0.000593,
     institute_van = 0.000593,
     institute_bus = 0.0028,
     institute_biodieselbus = 0.00649,
     commuting_studentalone = 0.00059,
     commuting_studentcarpool = 0.00059,
     commuting_studentbus = 0.00649,
     commuting_staffalone = 0.00059,
     commuting_staffcarpool = 0.00059,
     commuting_staffbus = 0.00649,
     commuting_facultyalone = 0.00059,
     commuting_facultycarpool = 0.00059,
     commuting_facultybus = 0.00649
   )
  
  averagefuelefficiency <- c(
    institute_car = 22.1,
    institute_van = 22.1,
    institute_bus = 6.9,
    institute_biodieselbus = 0.45,
    commuting_studentalone = 24.2,
    commuting_studentcarpool = 24.2,
    commuting_studentbus = 31.9,
    commuting_staffalone = 22.1,
    commuting_staffcarpool = 22.1,
    commuting_staffbus = 39.7,
    commuting_facultyalone = 22.1,
    commuting_facultycarpool = 22.1,
    commuting_facultybus = 39.7 
  )
   
  fuelvolume <- c(
    institute_car = 100,
    institute_van = 100,
    institute_bus = 100,
    institute_biodieselbus = 100
  )
  
  fuelconsumption <- c(
    commuting_studentalone = "Commuting_student_fuelconsumption",
    commuting_studentcarpool = "Commuting_studentcarpool_fuelconsumption", 
    commuting_studentbus = "Commuting_studentbus_fuelconsumption",
    commuting_staffalone = "Commuting_staff_fuelconsumption", 
    commuting_staffcarpool = "Commuting_staffcarpool_fuelconsumption",
    commuting_staffbus = "Commuting_staffbus_fuelconsumption",
    commuting_facultyalone = "Commuting_faculty_fuelconsumption",
    commuting_facultycarpool = "Commuting_facultycarpool_fuelconsumption", 
    commuting_facultybus = "Commuting_facultybus_fuelconsumption"
  )   
  

  if(vehicle_type == "institute_car" ||
     vehicle_type == "institute_van" ||
     vehicle_type == "institute_bus" ||
     vehicle_type == "institute_biodieselbus"){
  
  NOx_released <- fuelvolume[vehicle_type] * nox_emmissionfactor[vehicle_type] * averagefuelefficiency[vehicle_type]
  
}  else if (vehicle_type == "commuting_studentalone" ||
            vehicle_type == "commuting_studentcarpool"     ||
            vehicle_type == "commuting_studentbus"         ||
            vehicle_type == "commuting_staffalone"         ||
            vehicle_type == "commuting_staffcarpool"       ||
            vehicle_type == "commuting_staffbus"           ||
            vehicle_type == "commuting_facultyalone"       ||
            vehicle_type == "commuting_facultycarpool"     ||
            vehicle_type == "commuting_facultybus") {
  
  NOx_released <- get(fuelconsumption[vehicle_type]) * nox_emmissionfactor[vehicle_type] * averagefuelefficiency[vehicle_type]
  
  return(NOx_released)  
}
}
Institute_car_NOx <- calc_NOx("institute_car")
Institute_van_NOx <- calc_NOx("institute_van")
Institute_bus_NOx <- calc_NOx("institute_bus")
Institute_biodieselbus_NOx <- calc_NOx("institute_biodieselbus")
Commuting_studentalone_NOx <- calc_NOx("commuting_studentalone")
Commuting_studentcarpool_NOx <- calc_NOx("commuting_studentcarpool")
Commuting_studentbus_NOx <- calc_NOx("commuting_studentbus")
Commuting_staffalone_NOx <- calc_NOx("commuting_staffalone")
Commuting_staffcarpool_NOx <- calc_NOx("commuting_staffcarpool")
Commuting_staffbus_NOx <-calc_NOx("commuting_staffbus")
Commuting_facultyalone_NOx <-calc_NOx("commuting_facultyalone")
Commuting_facultycarpool_NOx <-calc_NOx("commuting_facultycarpool")
Commuting_facultybus_NOx <-calc_NOx("commuting_facultybus")

print(Institute_car_NOx)
print(Institute_van_NOx)
print(Institute_bus_NOx)
print(Institute_biodieselbus_NOx)
print(Commuting_studentalone_NOx)
print(Commuting_studentcarpool_NOx)
print(Commuting_studentbus_NOx)
print(Commuting_staffalone_NOx)
print(Commuting_staffcarpool_NOx)
print(Commuting_staffbus_NOx)
print(Commuting_facultyalone_NOx)
print(Commuting_facultycarpool_NOx)
print(Commuting_facultybus_NOx)


#Calculate the Total NOx released for all vehicles         
Total_NOx_Released <- sum(
Institute_car_NOx, Institute_van_NOx, Institute_bus_NOx, Institute_biodieselbus_NOx,
Commuting_studentalone_NOx, Commuting_studentcarpool_NOx, Commuting_studentbus_NOx,
Commuting_staffalone_NOx, Commuting_staffcarpool_NOx, Commuting_staffbus_NOx,
Commuting_facultyalone_NOx, Commuting_facultycarpool_NOx, Commuting_facultybus_NOx
)

print(Total_NOx_Released)  
###################################################################################################################################################################################################################################

           

#Institution - Total Nitrogen (N2O + NOx) ####
#'Title:Total N released 
#' @param N_released_as_N2O-  N Released as N2O in kg 
#' @param 0.63636- Conversion from N2O to N value from University of Virginia
#' @param 0.30435- Conversion from NOx (NO2) to N from University of Virginia
#' @param  N_released_NOx- emission factor for NOx in kg NOx/mi
#'
#' @return 
#' @export  Total_N_Released
#
#' @examples
           
calc_Total_N <- function(){
total_nitrogen <- (Total_N2O_Released * 0.63636) + (Total_NOx_Released * 0.30435)
return(total_nitrogen)
}


Total_N_Released <-calc_Total_N()
print(Total_N_Released)

