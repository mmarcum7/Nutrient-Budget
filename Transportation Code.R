
################################################################
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



calc_totaldistance <- function(mode="alone",individuals, commuting_rate, trips, days, miles) {
  #Calculation notes
   if (mode == "alone") {
    distance<- individuals * commuting_rate * trips * days * miles
    
  } else if (mode == "carpool") {
    distance<- individuals * commuting_rate * trips * days * miles / 2 
 
  } else if (mode == "bus") {
    distance<-individuals * commuting_rate * trips * days * miles
  }
return(distance)
}
 
 
Commuting_student_distance <- calc_totaldistance("alone",100,10,100,365,1000)
Commuting_studentcarpool_distance <-  calc_totaldistance("carpool",100,10,100,365,1000)
Commuting_studentbus_distance <-  calc_totaldistance("bus",100,10,100,365,1000)
Commuting_staff_distance <-  calc_totaldistance("alone",100,10,100,365,1000)
Commuting_staffcarpool_distance <-  calc_totaldistance("carpool",100,10,100,365,1000)
Commuting_staffbus_distance <-  calc_totaldistance("bus",100,10,100,365,1000)
Commuting_faculty_distance <- calc_totaldistance("alone",100,10,100,365,1000)
Commuting_facultycarpool_distance <-  calc_totaldistance("carpool",100,10,100,365,1000)
Commuting_facultybus_distance <-  calc_totaldistance("bus",100,10,100,365,1000)


print(Commuting_student_distance)
print(Commuting_studentcarpool_distance)
print(Commuting_studentbus_distance)
print(Commuting_staff_distance)
print(Commuting_staffcarpool_distance)
print(Commuting_staffbus_distance)
print(Commuting_faculty_distance)
print(Commuting_facultycarpool_distance)
print(Commuting_facultybus_distance)
  


#########################################################################################################
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
calc_FuelConsumption <- function(vehicle_type,Distance){
#1/Fuel Efficiency of the vehicle multiplied by the distance the vehicle traveled in miles                                                                                                     
 if (vehicle_type == "Commuting Student Alone"){
  #24.2 mpg (miles per gallon) is the Fuel Efficiency for a car taken from University Of virginia values
  fuel_consumption <- (1/24.2) * Distance
  
} else if (vehicle_type == "Commuting Student Carpool"){ 
  #24.2 mpg (miles per gallon) is the Fuel Efficiency for a car taken from University Of virginia values
  fuel_consumption <- (1/24.2) * Distance
  
} else if (vehicle_type == "Commuting Student Bus"){
  #39.7 mpg (miles per gallon) is the Fuel Efficiency for a city bus taken from University Of virginia values
  fuel_consumption <- (1/39.7) * Distance  
  
} else if (vehicle_type == "Commuting Staff Alone"){
  #22.1 mpg (miles per gallon) is the Fuel Efficiency for a car taken from University Of virginia values
  fuel_consumption <- (1/22.1) * Distance
  
} else if (vehicle_type == "Commuting Staff Carpool"){
  #22.1 mpg (miles per gallon) is the Fuel Efficiency for a car taken from University Of virginia values
  fuel_consumption <- (1/22.1) * Distance
  
} else if (vehicle_type == "Commuting Staff Bus"){
  #39.7 mpg (miles per gallon) is the Fuel Efficiency for a city bus taken from University Of virginia values
  fuel_consumption <- (1/39.7) * Distance
  
} else if (vehicle_type == "Commuting Faculty Alone"){
  #22.1 mpg (miles per gallon) is the Fuel Efficiency for a car taken from University Of virginia values
  fuel_consumption <- (1/22.1) * Distance
  
} else if (vehicle_type == "Commuting Faculty Carpool"){
  #22.1 mpg (miles per gallon) is the Fuel Efficiency for a car taken from University Of virginia values
  fuel_consumption <- (1/22.1) * Distance
  
} else if (vehicle_type == "Commuting Faculty Bus"){ 
  #39.7 mpg (miles per gallon) is the Fuel Efficiency for a city bus taken from University Of virginia values
  fuel_consumption <- (1/39.7) * Distance
}  
return(fuel_consumption)
}    


Commuting_student_fuelconsumption <- calc_FuelConsumption("Commuting student alone",50)
Commuting_studentcarpool_fuelconsumption <- calc_FuelConsumption("Commuting student carpool",50)
Commuting_studentbus_fuelconsumption <- calc_FuelConsumption("Commuting student bus",50)
Commuting_staff_fuelconsumption <- calc_FuelConsumption("Commuting staff alone",50)
Commuting_staffcarpool_fuelconsumption <- calc_FuelConsumption("Commutting staff carpool",50)
Commuting_staffbus_fuelconsumption <- calc_FuelConsumption("Commuting staff bus",50)
Commuting_faculty_fuelconsumption <-calc_FuelConsumption("Commuting faculty alone",50)
Commuting_facultycarpool_fuelconsumption <-calc_FuelConsumption("Commuting faculty carpool",50)
Commuting_facultybus_fuelconsumption <-calc_FuelConsumption("Commuting faculty bus",50)


print(Commuting_student_fuelconsumption)
print(Commuting_studentcarpool_fuelconsumption)
print(Commuting_studentbus_fuelconsumption)
print(Commuting_staff_fuelconsumption)
print(Commuting_staffcarpool_fuelconsumption)
print(Commuting_staffbus_fuelconsumption)
print(Commuting_faculty_fuelconsumption)
print(Commuting_facultycarpool_fuelconsumption)
print(Commuting_facultybus_fuelconsumption)


########################################################################
#N2O Function####
#'Title:Total N2O released 
#' @param Vehicle_type - mode of transportation
#' @param Fuel_volume - in gallons, the amount of gasoline held in the vehicle
#' @param Fuel_Consumption - calculated for each commuter type before, amount of gasoline used in travel to commute
#'
#' @return Amount of N2O Released in kg
#' @export  Total_N2O_Released
#'
#' @examples

calc_n2O <- function(Vehicle_type, Fuel_volume,Fuel_consumption) {
  #Fuel Volume for Institute Vehicles and Fuel Consumption for commuter vehicles, multiplied by the Fuel Emmission Factor of that vehicle
  if (Vehicle_type == "Institute car") {
    #0.006 kg N2O/gals is N2O Emission Factor of a car taken from University Of virginia values
    TotalN2O <- Fuel_volume * 0.0006
    
  } else if (Vehicle_type == "Institute van") {
    #O is a placeholder
    #x kg N2O/gals is N2O Emission Factor of a van taken from University Of virginia values 
    TotalN2O <- Fuel_volume * 0
    
  } else if (Vehicle_type == "Institute biodiesel bus"){
    #0.000675 kgN2O/gals is N2O Emission Factor of an Institute biodiesel bus taken from University Of virginia values
    TotalN2O <- Fuel_volume * 0.000675
    
  } else if (Vehicle_type == "Institute bus"){
    #0.00649 kgN2O/gals is N2O Emission Factor of an Institute bus taken from University Of virginia values
    TotalN2O <- Fuel_volume * 0.00649
    
  } else if (Vehicle_type == "Commuting student alone") {
    #0.006 kg N2O/gals is N2O Emission Factor of an car taken from University Of virginia values
    TotalN2O <- Fuel_consumption * 0.0006
    
  } else if (Vehicle_type == "Commuting student carpool"){
    #0.006kg N2O/gals is N2O Emission Factor of a car taken from University Of virginia values
    TotalN2O <- Fuel_consumption * 0.0006
    
  } else if (Vehicle_type == "Commuting student bus"){
    #0.003 kg N2O/gals is N2O Emission Factor of a Commuter Bus taken from University Of virginia values
    TotalN2O <- Fuel_consumption * 0.003
    
  } else if (Vehicle_type == "Commuting staff alone"){
    #0.006 kg N2O/gals is N2O Emission Factor of a car taken from University Of virginia values
    TotalN2O <- Fuel_consumption * 0.0006
    
  } else if (Vehicle_type == "Commuting staff carpool"){
    #0.006 kg N2O/gals is N2O Emission Factor of a car taken from University Of virginia values  
    TotalN2O <- Fuel_consumption * 0.0006
    
  } else if (Vehicle_type == "Commuting staff bus"){
    #0.003 kg N2O/gals is N2O Emission Factor of a Commuter Bus taken from University Of virginia values
    return(Fuel_consumption * 0.003)
    
  } else if (Vehicle_type == "Commuting faculty alone"){
    #0.006 kg N2O/gals is N2O Emission Factor of a car taken from University Of virginia values
    TotalN2O <- Fuel_consumption * 0.0006
    
  } else if (Vehicle_type == "Commuting faculty carpool"){
    #0.006 kg N2O/gals is N2O Emission Factor of a car taken from University Of virginia values
    TotalN2O <- Fuel_consumption * 0.0006
    
  } else if (Vehicle_type == "Commuting faculty bus"){
    #0.003 kg N2O/gals is N2O Emission Factor of a city bus taken from University Of virginia values
    TotalN2O <- Fuel_consumption * 0.003
} 
return(TotalN2O)  
}

Institute_car_n2O <- calc_n2O("Institute car",100,0)
Institute_van_n2O <- calc_n2O("Institute van",100,0)
Institute_bus_n2O <- calc_n2O("Institute bus",100,0)
Institute_biodieselbus_n2O <- calc_n2O("Institute biodiesel bus",0,500)
Commuting_student_n2O <- calc_n2O("Commuting student alone",0,500)
Commuting_studentcarpool_n20 <- calc_n2O("Commuting student carpool",0,500)
Commuting_studentbus_n2O <- calc_n2O("Commuting student bus",0,500)
Commuting_staff_n2O <- calc_n2O("Commuting staff alone",0,500)
Commuting_staffcarpool_n2O <- calc_n2O("Commuting staff carpool",0,500)
Commuting_staffbus_n2O <-calc_n2O("Commuting staff bus",0,500)
Commuting_faculty_n2O <-calc_n2O("Commuting faculty alone",0,500)
Commuting_facultycarpool_n2O <-calc_n2O("Commuting faculty carpool",0,500)
Commuting_facultybus_n2O <-calc_n2O("Commuting faculty bus",0,500)

print(Institute_car_n2O)
print(Institute_van_n2O)
print(Institute_bus_n2O)
print(Institute_biodieselbus_n2O)
print(Commuting_student_n2O)
print(Commuting_studentcarpool_n20)
print(Commuting_studentbus_n2O)
print(Commuting_staff_n2O)
print(Commuting_staffcarpool_n2O)
print(Commuting_staffbus_n2O)
print(Commuting_faculty_n2O)
print(Commuting_facultycarpool_n2O)
print(Commuting_facultybus_n2O)

#Calculate the Total N2O released from vehicles
Total_N2O_Released <- sum(
Institute_car_n2O, Institute_van_n2O, Institute_bus_n2O, Institute_biodieselbus_n2O,
Commuting_student_n2O, Commuting_studentcarpool_n20, Commuting_studentbus_n2O,
Commuting_staff_n2O,Commuting_staffcarpool_n2O , Commuting_staffbus_n2O,
Commuting_faculty_n2O, Commuting_facultycarpool_n2O, Commuting_facultybus_n2O
)
print(Total_N2O_Released) 


####################################################################
#NOx Calculation####
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
calc_NOx <- function (vehicle_type, Fuel_Volume, Fuel_Consumption){
  
  if (vehicle_type == "Institute car"){
#22.1 miles per gallon is the Average Fuel Efficiency and 0.000593 kg NOx/miles is the NOx Emission Factor all from the the University of Virginia
    TotalNOx<- Fuel_Volume * 22.1 * 0.000593

  }else if (vehicle_type == "Institute van"){
#O are placeholders
#x miles per gallon is the Average Fuel Efficiency and y kg NOx/miles is the NOx Emission Factor all from the the University of Virginia
    TotalNOx<- Fuel_Volume * 0 * 0

  }else if (vehicle_type == "Institute bus"){
#6.9 miles per gallon is the Average Fuel Efficiency and 0.00028 kg NOx/miles is the NOx Emission Factor all from the the University of Virginia
    TotalNOx<- Fuel_Volume * 6.9 * 0.00028

  }else if (vehicle_type == "Institute biodiesel bus"){
#0.45 miles per gallon is the Average Fuel Efficiency and 0.00649 kg NOx/miles is the NOx Emission Factor all from the the University of Virginia
    TotalNOx<- Fuel_Volume * 0.45 * 0.00649

  }else if (vehicle_type == "Commuting student alone"){
#24.2 miles per gallon is the Average Fuel Efficiency and 0.00059 kg NOx/miles is the NOx Emission Factor all from the the University of Virginia
    TotalNOx<- Fuel_Consumption * 24.2 * 0.00059
         
  }else if (vehicle_type == "Commuting student carpool"){
#24.2.1 miles per gallon is the Average Fuel Efficiency and 0.00059 kg NOx/miles is the NOx Emission Factor all from the the University of Virginia
    TotalNOx<- Fuel_Consumption * 24.2 * 0.00059 
    
  }else if (vehicle_type == "Commuting student bus"){
 #31.9 miles per gallon is the Average Fuel Efficiency and 0.000649 kg NOx/miles is the NOx Emission Factor all from the the University of Virginia
    TotalNOx<- Fuel_Consumption * 31.9 * 0.00649  
         
  }else if (vehicle_type == "Commuting staff alone"){
#22.1 miles per gallon is the Average Fuel Efficiency and 0.00059 kg NOx/miles is the NOx Emission Factor all from the the University of Virginia
    TotalNOx<- Fuel_Consumption * 22.1 * 0.00059 
         
  }else if (vehicle_type == "Commuting staff carpool"){
#22.1 miles per gallon is the Average Fuel Efficiency and 0.00059 kg NOx/miles is the NOx Emission Factor all from the the University of Virginia
    TotalNOx<- Fuel_Consumption * 22.1 * 0.00059 
         
  }else if (vehicle_type == "Commuting staff bus"){
#39.7 miles per gallon is the Average Fuel Efficiency and 0.00649 kg NOx/miles is the NOx Emission Factor all from the the University of Virginia
    TotalNOx<- Fuel_Consumption * 39.7 * 0.00649
         
  }else if (vehicle_type == "Commuting faculty alone"){
#22.1 miles per gallon is the Average Fuel Efficiency and 0.00059 kg NOx/miles is the NOx Emission Factor all from the the University of Virginia
    TotalNOx<- Fuel_Consumption * 22.1 * 0.00059 
         
  }else if (vehicle_type == "Commuting faculty carpool"){
#22.1 miles per gallon is the Average Fuel Efficiency and 0.00059 kg NOx/miles is the NOx Emission Factor all from the the University of Virginia
    TotalNOx<- Fuel_Consumption * 22.1 * 0.00059 
         
 } else if (vehicle_type == "Commuting faculty bus"){
#39.7 miles per gallon is the Average Fuel Efficiency and 0.000649 kg NOx/miles is the NOx Emission Factor all from the the University of Virginia
   TotalNOx<- Fuel_Consumption * 39.7 * 0.00649  
}
return(TotalNOx)
}         

Institute_car_NOx <- calc_NOx("Institute car",100,0)
Institute_van_NOx <- calc_NOx("Institute van",100,0)
Institute_bus_NOx <- calc_NOx("Institute bus",100,0)
Institute_biodieselbus_NOx <- calc_NOx("Institute biodiesel bus",0,500)
Commuting_student_NOx <- calc_NOx("Commuting student alone",0,500)
Commuting_studentcarpool_NOx <- calc_NOx("Commuting student carpool",0,500)
Commuting_studentbus_NOx <- calc_NOx("Commuting student bus",0,500)
Commuting_staff_NOx <- calc_NOx("Commuting staff alone",0,500)
Commuting_staffcarpool_NOx <- calc_NOx("Commuting staff carpool",0,500)
Commuting_staffbus_NOx <-calc_NOx("Commuting staff bus",0,500)
Commuting_faculty_NOx <-calc_NOx("Commuting faculty alone",0,500)
Commuting_facultycarpool_NOx <-calc_NOx("Commuting faculty carpool",0,500)
Commuting_facultybus_NOx <-calc_NOx("Commuting faculty bus",0,500)

print(Institute_car_NOx)
print(Institute_van_NOx)
print(Institute_bus_n2O)
print(Institute_biodieselbus_NOx)
print(Commuting_student_NOx)
print(Commuting_studentcarpool_NOx)
print(Commuting_studentbus_NOx)
print(Commuting_staff_NOx)
print(Commuting_staffcarpool_NOx)
print(Commuting_staffbus_NOx)
print(Commuting_faculty_NOx)
print(Commuting_facultycarpool_NOx)
print(Commuting_facultybus_NOx)


#Calculate the Total NOx released for all vehicles         
Total_NOx_Released <- sum(
Institute_car_NOx, Institute_van_NOx, Institute_bus_NOx, Institute_biodieselbus_NOx,
Commuting_student_NOx, Commuting_studentcarpool_NOx, Commuting_studentbus_NOx,
Commuting_staff_NOx, Commuting_staffcarpool_NOx, Commuting_staffbus_NOx,
Commuting_faculty_NOx, Commuting_facultycarpool_NOx, Commuting_facultybus_NOx
)

print(Total_NOx_Released)  
           
           
#######################################################
#Total Nitrogen Calculation####
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

