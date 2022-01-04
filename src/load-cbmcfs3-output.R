library(rsyncrosim)
library(tidyverse)
library(RODBC)

options(stringsAsFactors=FALSE)

# mySession <- session()
# libDir <- "C:/Users/Administrator/Desktop/A249/Demo/"
# myLibrary <- ssimLibrary(name = paste0(libDir,"Lucas Base Model Demo"), session = mySession)
# myProject <- rsyncrosim::project(myLibrary, 1)
# myScenario = scenario(myProject, scenario = "Load CBM-CFS3 Output")

# Get ST-Sim library, project and scenario ----
myLibrary <- ssimLibrary()
myProject <- project()
myScenario <- scenario()

# Identify biomass and DOM stocks ----

biomassStocks <- c("Merchantable", "Foliage", "Other", "Coarse Roots", "Fine Roots")
biomassStateAtts <- c("Merchantable", "Foliage", "Other Wood", "Coarse Roots", "Fine Roots")
biomassStateAtts <- as.list(str_c("Carbon Initial Conditions: ", biomassStateAtts))
numBiomassStocks <- length(biomassStocks)

DOMStocks <- c("Aboveground Very Fast DOM", "Aboveground Fast DOM", "Medium DOM", "Aboveground Slow DOM",
                                    "Belowground Very Fast DOM", "Belowground Fast DOM", "Belowground Slow DOM",
                                    "Branch Snag", "Stem Snag")
DOMStocks_hw <- c(DOMStocks[1:7], "Hardwood Branch Snag", "Hardwood Stem Snag")
DOMStocks_sw <- c(DOMStocks[1:7],"Softwood Branch Snag", "Softwood Stem Snag")
DOMStateAtts <- c("Aboveground Very Fast", "Aboveground Fast", "Aboveground Medium", "Aboveground Slow",
                  "Belowground Very Fast", "Belowground Fast", "Belowground Slow", "Snag Branch", "Snag Stem")
DOMStateAtts <- as.list(str_c("Carbon Initial Conditions: ", DOMStateAtts))
numDOMStocks <- length(DOMStocks)


# Pull in crosswalk table
sheetName <- "stsimcbmcfs3_CrosswalkSpecies"
crosswalkSUSTFull <- datasheet(myScenario, name = sheetName, optional = T)

# State Attribute Values ----

# Connect to CBM-CFS3 "ArchiveIndex_Beta_Install.mdb" to assess if the species is Softwood or Hardwood
CBMDatabasePath <- datasheet(myLibrary, name = "stsimcbmcfs3_Database")
CBMdatabase <- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", CBMDatabasePath))
speciesTypeTable <- sqlFetch(CBMdatabase, "tblSpeciesTypeDefault")
forestTypeTable <- sqlFetch(CBMdatabase, "tblForestTypeDefault")
close(CBMdatabase)


## State Attributes for Living Biomass

sheetName <- "stsim_StateAttributeValue"
stateAttributeInitialCarbonBiomass <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
stateAttributeInitialCarbonBiomassFull = data.frame()

for(i in seq(1:nrow(crosswalkSUSTFull))) {
  
  crosswalkSUST <- crosswalkSUSTFull %>% slice(i)
  CBMSimulationData <- read.csv(crosswalkSUST$CBMOutputFile, header=TRUE, check.names = F)
  CBMSimulationData <- CBMSimulationData[,1:(ncol(CBMSimulationData)-1)]
  
  # Get Species and Forest Type IDs
  speciesTypeID <- speciesTypeTable$SpeciesTypeID[speciesTypeTable$SpeciesTypeName == as.character(crosswalkSUST$SpeciesTypeID)]
  forestTypeID <- speciesTypeTable$ForestTypeID[speciesTypeTable$SpeciesTypeID == speciesTypeID]
  forestType <- as.character(forestTypeTable$ForestTypeName[forestTypeTable$ForestTypeID == forestTypeID])
  
  stateAttributeInitialCarbonBiomass[1:(nrow(CBMSimulationData)*numBiomassStocks), "StratumID"] <- crosswalkSUST$StratumID            
  stateAttributeInitialCarbonBiomass[1:(nrow(CBMSimulationData)*numBiomassStocks), "SecondaryStratumID"] <- crosswalkSUST$SecondaryStratumID
  stateAttributeInitialCarbonBiomass[1:(nrow(CBMSimulationData)*numBiomassStocks), "StateClassID"] <- crosswalkSUST$StateClassID
  stateAtts <- NULL
  for(r in biomassStateAtts){ stateAtts <- c(stateAtts, rep(r,nrow(CBMSimulationData)))}
  stateAttributeInitialCarbonBiomass[1:(nrow(CBMSimulationData)*numBiomassStocks), "StateAttributeTypeID"] <- stateAtts
  stateAttributeInitialCarbonBiomass[1:(nrow(CBMSimulationData)*numBiomassStocks), "AgeMin"] <- rep(CBMSimulationData[,"Time Step"], numBiomassStocks)
  stateAttributeInitialCarbonBiomass[1:(nrow(CBMSimulationData)*numBiomassStocks), "AgeMax"] <- rep(CBMSimulationData[,"Time Step"], numBiomassStocks)
  stateAttributeInitialCarbonBiomass[stateAttributeInitialCarbonBiomass$AgeMin == (nrow(CBMSimulationData)-1), "AgeMax"] <- NA
  value <- NULL
  for(r in biomassStocks){ value <- c(value, CBMSimulationData[,paste(forestType, r)])}
  stateAttributeInitialCarbonBiomass[1:(nrow(CBMSimulationData)*numBiomassStocks), "Value"] <- value
  stateAttributeInitialCarbonBiomassFull = rbind(stateAttributeInitialCarbonBiomass, stateAttributeInitialCarbonBiomassFull)
}

## State Attributes for DOM
stateAttributeInitialCarbonDOM <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
stateAttributeInitialCarbonDOMFull = data.frame()

for(i in seq(1:nrow(crosswalkSUSTFull))) {
  
  crosswalkSUST <- crosswalkSUSTFull %>% slice(i)
  CBMSimulationData <- read.csv(crosswalkSUST$CBMOutputFile, header=TRUE, check.names = F)
  CBMSimulationData <- CBMSimulationData[,1:(ncol(CBMSimulationData)-1)]
  
  # Get Species and Forest Type IDs
  speciesTypeID <- speciesTypeTable$SpeciesTypeID[speciesTypeTable$SpeciesTypeName == as.character(crosswalkSUST$SpeciesTypeID)]
  forestTypeID <- speciesTypeTable$ForestTypeID[speciesTypeTable$SpeciesTypeID == speciesTypeID]
  forestType <- as.character(forestTypeTable$ForestTypeName[forestTypeTable$ForestTypeID == forestTypeID])
  
  stateAttributeInitialCarbonDOM[1:(nrow(CBMSimulationData)*numDOMStocks), "StratumID"] <- crosswalkSUST$StratumID           
  stateAttributeInitialCarbonDOM[1:(nrow(CBMSimulationData)*numDOMStocks), "SecondaryStratumID"] <- crosswalkSUST$SecondaryStratumID
  stateAttributeInitialCarbonDOM[1:(nrow(CBMSimulationData)*numDOMStocks), "StateClassID"] <- crosswalkSUST$StateClassID
  stateAtts <- NULL
  for(r in DOMStateAtts){ stateAtts <- c(stateAtts, rep(r,nrow(CBMSimulationData)))}
  stateAttributeInitialCarbonDOM[1:(nrow(CBMSimulationData)*numDOMStocks), "StateAttributeTypeID"] <-  stateAtts
  stateAttributeInitialCarbonDOM[1:(nrow(CBMSimulationData)*numDOMStocks), "AgeMin"] <- rep(CBMSimulationData[, "Time Step"], numDOMStocks)
  stateAttributeInitialCarbonDOM[1:(nrow(CBMSimulationData)*numDOMStocks), "AgeMax"] <- rep(CBMSimulationData[, "Time Step"], numDOMStocks)
  stateAttributeInitialCarbonDOM[stateAttributeInitialCarbonDOM$AgeMin == (nrow(CBMSimulationData)-1), "AgeMax"] <- NA
  value <- NULL
  if(forestType == "Hardwood"){ for(r in DOMStocks_hw){ value <- c(value, CBMSimulationData[,r])} }
  if(forestType == "Softwood"){ for(r in DOMStocks_sw){ value <- c(value, CBMSimulationData[,r])} }
  stateAttributeInitialCarbonDOM[1:(nrow(CBMSimulationData)*numDOMStocks), "Value"] <- value
  stateAttributeInitialCarbonDOMFull = rbind(stateAttributeInitialCarbonDOM, stateAttributeInitialCarbonDOMFull)
}

## Combine the state attribute datasheets.
# Combine all State Attribute Values and select only the first 300 years since the 301-600 years are the result of initiating a harvest in the CBM run.
stateAttributesMerged <- rbind(stateAttributeInitialCarbonBiomassFull, stateAttributeInitialCarbonDOMFull)
saveDatasheet(myScenario, stateAttributesMerged, sheetName)

# outputScenario <- scenario(myProject, "State Attribute Values")
# saveDatasheet(outputScenario, stateAttributesMerged, sheetName)
