#Example Nitrogen Transportation Dataframe
NitrogenTransportation.df <- read.csv("",stringsAsFactors = TRUE)



calc_n2o <- function(vehicle_type = c("car", "van", "bus", "biodiesel"){         #Vehicle type is the argument
  if(vehicle_type == "car"){
    return(N2O_emission <- df[df$vehicle_type == "car", "volume"] * df[df$vehicle_type == "car", "N2O Emmission Factor"])
  } else if(vehicle_type == "van"){
    return(N2O_emission <- df[df$vehicle_type == "van", "volume"] * df[df$vehicle_type == "van", "N2O Emmission Factor"])
  } else if(vehicle_type == "bus") {
    return(N2O_emission <- df[df$vehicle_type == "bus", "volume"] * df[df$vehicle_type == "bus", "N2O Emmission Factor"])
  }else if (vehicle_type == "biodiesel") }
  return(N2O_emission <- df[df$vehicle_type == "biodiesel", "volume"] * df[df$vehicle_type == "biodiesel", "N2O Emmission Factor"])
  
print(calc_n20("car")) #output is the N2O emmissions of car
  


N2OConversion <- #Institute value inserted here, converts N2O into N
NOxConversion <- #Institute value inserted here, converts NOx into N
  #Simap Used atomic weights to calcuislate: 0.6 and 0.3

#Transportation Calculations
#Fleet via University (cars,vans,trucks,bus,biodiesel bus)####
N2O_car <- NitrogenTransportation.df["volume","car"] *  NitrogenTransportation.df["N2O Emmission Factor","car"]
NOx_car <- NitrogenTransportation.df["volume","car"] * NitrogenTransportation.df["Avergae Fuel Efficiency","car"] * NitrogenTransportation.df["NOxEmissionFactor","car"]
TotalN_car <- (N20_car * N2OConversion) + (NOx_car * NOxConversion)

N2O_van <- NitrogenTransportation.df["volume","van"] *  NitrogenTransportation.df["N2O Emmission Factor","van"]
NOx_van <- NitrogenTransportation.df["volume", "van"] * NitrogenTransportation.df["Avergae Fuel Efficiency","van"] * NitrogenTransportation.df["NOxEmissionFactor","van"]
TotalN_van <- (N20_van * N2OConversion) + (NOx_van * NOxConversion)

N2O_bus <- NitrogenTransportation.df["volume","bus"] *  NitrogenTransportation.df["N2O Emmission Factor","bus"]
NOx_bus <- NitrogenTransportation.df["volume","bus"] * NitrogenTransportation.df["Avergae Fuel Efficiency","bus"] * NitrogenTransportation.df["NOx Emission Factor","bus"]
TotalN_bus <- (N20_bus * N2OConversion) + (NOx_bus * NOxConversion)

N2O_biobus <- NitrogenTransportation.df["volume","biobus"] *  NitrogenTransportation.df["N2OEmmissionFactor","biobus"]
NOx_biobus <- NitrogenTransportation.df["volume","biobus"] * NitrogenTransportation.df["AvergaeFuelEfficiency","biobus"] * NitrogenTransportation.df["NOxEmissionFactor","biobus"]
TotalN_biobus <- (N20_biobus * N2OConversion) + (NOx_biobus * NOxConversion)

#Data Needed: Volume, N2O Emmission Factor, NOx Emmission Factor, Average Fuel Efficiency



#Commuting (Students, Staff, Faculty, Visitors)####
#Students (alone,carpool,bus)####
TotalDistanceS1 <- NitrogenTransportation.df["Individuals","Students(alone)"] * NitrogenTransportation.df["Commuting", "Students(alone)"] * NitrogenTransportation.df["Trips","Students(alone)"] * NitrogenTransportation.df["Days","Students(alone)"] * NitrogenTransportation.df["Miles","Students(alone)"]
FuelConsumptionS1 <- ifelse(NitrogenTransportation.df["Average Fuel Efficiency", "Students(alone)"] == 0, 0, (1 / NitrogenTransportation.df["Average Fuel Efficiency","Students(alone"] * NitrogenTransportation.df[TotalDistanceS1])  #Sensitivity Caculations
N2O_S1 <- FuelConsumptionS1 * NitrogenTransportation.df["N2O Emmission Factor","Students(alone)"]
NOx_S1 < FuelConsumptionS1 * NitrogenTransportation.df["N2O Emmission Factor","Students(alone)"] * NitrogenTransportation.df["Average Fuel Efficiency","Students(alone)"]
TotalN_S1 <- (N2O_S1 * N20Conversion) + (NOx_S1 * NOxConversion)

TotalDistanceS2 <- NitrogenTransportation.df["Individuals","Students(carpool)"] * NitrogenTransportation.df["Commuting", "Students(carpool)"] * NitrogenTransportation.df["Trips","Students(carpool)"] * NitrogenTransportation.df["Days","Students(carpool)"] * NitrogenTransportation.df["Miles","Students(carpool)"] / 2
FuelConsumptionS2 <- ifelse(NitrogenTransportation.df["Average Fuel Efficiency", "Students(carpool)"] == 0, 0, (1 / NitrogenTransportation.df["Average Fuel Efficiency","Students(carpool"] * NitrogenTransportation.df[TotalDistanceS1])  #Sensitivity Caculations
N2O_S2 <- FuelConsumptionS2 * NitrogenTransportation.df["N2O Emmission Factor","Students(carpool)"]
NOx_S2 < FuelConsumptionS2 * NitrogenTransportation.df["N2O Emmission Factor","Students(carpool)"] * NitrogenTransportation.df["Average Fuel Efficiency","Students(carpool)"]
TotalN_S2 <- (N2O_S2 * N20Conversion) + (NOx_S2 * NOxConversion)

TotalDistanceS3 <- NitrogenTransportation.df["Individuals","Students(bus)"] * NitrogenTransportation.df["Commuting", "Students(bus)"] * NitrogenTransportation.df["Trips","Students(bus)"] * NitrogenTransportation.df["Days","Students(bus)"] * NitrogenTransportation.df["Miles","Students(bus)"]
FuelConsumptionS3 <- ifelse(NitrogenTransportation.df["Average Fuel Efficiency", "Students(bus)"] == 0, 0, (1 / NitrogenTransportation.df["Average Fuel Efficiency","Students(bus)"] * NitrogenTransportation.df[TotalDistanceS1])  #Sensitivity Caculations
N2O_S3 <- FuelConsumptionS3 * NitrogenTransportation.df["N2O Emmission Factor","Students(bus)"]
NOx_S3 < FuelConsumptionS3 * NitrogenTransportation.df["N2O Emmission Factor","Students(bus)"] * NitrogenTransportation.df["Average Fuel Efficiency","Students(bus)"]
TotalN_S3 <- (N2O_S3 * N20Conversion) + (NOx_S3 * NOxConversion)

#Staff (alone,carpool,bus)####    
TotalDistanceSf1 <- NitrogenTransportation.df["Individuals","Staff(alone)"] * NitrogenTransportation.df["Commuting", "Staff(alone)"] * NitrogenTransportation.df["Trips","Staff(alone)"] * NitrogenTransportation.df["Days","Staff(alone)"] * NitrogenTransportation.df["Miles","Staff(alone)"]
FuelConsumptionSf1 <- ifelse(NitrogenTransportation.df["Average Fuel Efficiency", "Staff(alone)"] == 0, 0, (1 / NitrogenTransportation.df["Average Fuel Efficiency","Staff(alone)"] * NitrogenTransportation.df[TotalDistanceSf1])  #Sensitivity Caculations
N2O_Sf1 <- FuelConsumptionSf1 * NitrogenTransportation.df["N2O Emmission Factor","Staff(alone)"]
NOx_Sf1 < FuelConsumptionSf1 * NitrogenTransportation.df["N2O Emmission Factor","Staff(alone)"] * NitrogenTransportation.df["Average Fuel Efficiency","Staff(alone)"]
TotalN_Sf1 <- (N2O_Sf1 * N20Conversion) + (NOx_Sf1 * NOxConversion)
                            
TotalDistanceSf2 <- NitrogenTransportation.df["Individuals","Staff(carpool)"] * NitrogenTransportation.df["Commuting", "Staff(carpool)"] * NitrogenTransportation.df["Trips","Staff(carpool)"] * NitrogenTransportation.df["Days","Staff(carpool)"] * NitrogenTransportation.df["Miles","Staff(carpool)"] / 2
FuelConsumptionSf2 <- ifelse(NitrogenTransportation.df["Average Fuel Efficiency", "Staff(carpool)"] == 0, 0, (1 / NitrogenTransportation.df["Average Fuel Efficiency","Students(carpool)"] * NitrogenTransportation.df[TotalDistanceS1])  #Sensitivity Caculations
N2O_Sf2 <- FuelConsumptionSf2 * NitrogenTransportation.df["N2O Emmission Factor","Staff(carpool)"]
NOx_Sf2 < FuelConsumptionSf2 * NitrogenTransportation.df["N2O Emmission Factor","Staff(carpool)"] * NitrogenTransportation.df["Average Fuel Efficiency","Students(carpool)"]
TotalN_Sf2 <- (N2O_Sf2 * N20Conversion) + (NOx_Sf2 * NOxConversion)
                                                        
TotalDistanceSf3 <- NitrogenTransportation.df["Individuals","Staff(bus)"] * NitrogenTransportation.df["Commuting", "Staff(bus)"] * NitrogenTransportation.df["Trips","Staff(bus)"] * NitrogenTransportation.df["Days","Staff(bus)"] * NitrogenTransportation.df["Miles","Staff(bus)"]
FuelConsumptionS3 <- ifelse(NitrogenTransportation.df["Average Fuel Efficiency", "Staff(bus)"] == 0, 0, (1 / NitrogenTransportation.df["Average Fuel Efficiency","Staff(bus)"] * NitrogenTransportation.df[TotalDistanceS1])  #Sensitivity Caculations
N2O_Sf3 <- FuelConsumptionSf3 * NitrogenTransportation.df["N2O Emmission Factor","Staff(bus)"]
NOx_Sf3 < FuelConsumptionSf3 * NitrogenTransportation.df["N2O Emmission Factor","Staff(bus)"] * NitrogenTransportation.df["Average Fuel Efficiency","Staff(bus)"]
TotalN_Sf3 <- (N2O_Sf3 * N20Conversion) + (NOx_Sf3 * NOxConversion)

#Faculty(alone,carpool,bus)####
TotalDistanceF1 <- NitrogenTransportation.df["Faculty","Students(alone)"] * NitrogenTransportation.df["Commuting", "Faculty(alone)"] * NitrogenTransportation.df["Trips","Faculty(alone)"] * NitrogenTransportation.df["Days","Faculty(alone)"] * NitrogenTransportation.df["Miles","Faculty(alone)"]
FuelConsumptionF1 <- ifelse(NitrogenTransportation.df["Average Fuel Efficiency", "Faculty(alone)"] == 0, 0, (1 / NitrogenTransportation.df["Average Fuel Efficiency","Faculty(alone)"] * NitrogenTransportation.df[TotalDistanceS1])  #Sensitivity Caculations
N2O_F1 <- FuelConsumptionF1 * NitrogenTransportation.df["N2O Emmission Factor","Faculty(alone)"]
NOx_F1 < FuelConsumptionF1 * NitrogenTransportation.df["N2O Emmission Factor","Faculty(alone)"] * NitrogenTransportation.df["Average Fuel Efficiency","Faculty(alone)"]
TotalN_F1 <- (N2O_F1 * N20Conversion) + (NOx_F1 * NOxConversion)
                            
TotalDistanceF2 <- NitrogenTransportation.df["Faculty","Students(carpool)"] * NitrogenTransportation.df["Commuting", "Faculty(carpool)"] * NitrogenTransportation.df["Trips","Faculty(carpool)"] * NitrogenTransportation.df["Days","Faculty(carpool)"] * NitrogenTransportation.df["Miles","Faculty(carpool)"] / 2
FuelConsumptionF2 <- ifelse(NitrogenTransportation.df["Average Fuel Efficiency", "Faculty(carpool)"] == 0, 0, (1 / NitrogenTransportation.df["Average Fuel Efficiency","Students(carpool)"] * NitrogenTransportation.df[TotalDistanceS1])  #Sensitivity Caculations
N2O_F2 <- FuelConsumptionF2 * NitrogenTransportation.df["N2O Emmission Factor","Faculty(carpool)"]
NOx_F2 < FuelConsumptionF2 * NitrogenTransportation.df["N2O Emmission Factor","Faculty(carpool)"] * NitrogenTransportation.df["Average Fuel Efficiency","Faculty(carpool)"]
TotalN_S2 <- (N2O_F2 * N20Conversion) + (NOx_F2 * NOxConversion)
                                                        
TotalDistanceF3 <- NitrogenTransportation.df["Faculty","Students(bus)"] * NitrogenTransportation.df["Commuting", "Faculty(bus)"] * NitrogenTransportation.df["Trips","Faculty(bus)"] * NitrogenTransportation.df["Days","Faculty(bus)"] * NitrogenTransportation.df["Miles","Faculty(bus)"]
FuelConsumptionF3 <- ifelse(NitrogenTransportation.df["Average Fuel Efficiency", "Faculty(bus)"] == 0, 0, (1 / NitrogenTransportation.df["Average Fuel Efficiency","Faculty(bus"] * NitrogenTransportation.df[TotalDistanceS1])  #Sensitivity Caculations
N2O_F3 <- FuelConsumptionF3 * NitrogenTransportation.df["N2O Emmission Factor","Faculty(bus)"]
NOx_S3 < FuelConsumptionF3 * NitrogenTransportation.df["N2O Emmission Factor","Faculty(bus)"] * NitrogenTransportation.df["Average Fuel Efficiency","Faculty(bus)"]
TotalN_S3 <- (N2O_F3 * N20Conversion) + (NOx_F3 * NOxConversion)


NitrogenFertilizer.df <- read.csv("",stringsAsFactors = TRUE)







#Fertilizer Calculations####
NitrogenFertilizer.df <- read.csv("",stringsAsFactors = TRUE)
TurfgrassUptake <- #Average fertilizer uptake of Turfgrass
ShrubUptake <- #Average fertilizer uptake of Shrubs
TreeUptake <- #Average fertilizer uptake of Trees
Nreleased1 <- NitrogenFertilizer.df["Amount of Nitrogen","Fertilizer 1"] * (1-TurfgrassUptake) * 0.453592 #convert from pounds ot kgs
#same caculations for all fertilizers, just need appropriate uptake factor and the amount of nitrogen in that specific fertilizer
#Ten percent of fertilizer voilitates into air