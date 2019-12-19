#__________________________________________________#
# modifyTemplateDemo.R
# For preparing a library to run stsimcbmcfs3
# Bronwyn Rayfield, ApexRMS
#__________________________________________________#

library(rsyncrosim)
library(RODBC)
library(RSQLite)
library(tidyverse)

options(stringsAsFactors=FALSE)

####################################################
# Specify input parameters for ST-Sim and CBM-CFS3 #
####################################################
# Directory paths
# Directory for SyncroSim.WinForm.exe
# DEVNOTE: SyncroSim path is no longer needed.
# SyncroSimDir <- "C:/Users/bronw/Documents/Apex/SyncroSim/2-1-16-Beta/"

# Directory for your template and working ssim libraries
myLibraryDir <- "C:/Users/leona/Documents/Apex Projects/A223 - USGS - Task Order 7/CBMCFS3/A200-20_v2.1.16/"
# Directory for your Crosswalk csv ("Crosswalk - Spatial Unit and Species Type Demo.csv")
crosswalkDir <- myLibraryDir
# Directory for your CBM-CFS3 simulation results
CBMDir <- paste0("C:/Users/leona/Documents/Apex Projects/A223 - USGS - Task Order 7/CBMCFS3/A200-20_v2.1.16/CBM-CFS3 simulation results/")
  
# Source external R scripts
# Function to create a validation scenario
source("C:/gitprojects/stsimcbmcfs3/scripts/createValidationScenario.R")

# CBM-CFS3 Inputs
# This is the path to the CBM-CFS3 database. The easiest way to get the database is to install the CBM-CFS3 and look for it in the /Admin/DBs folder
CBMDatabasePath <- "C:/Program Files (x86)/Operational-Scale CBM-CFS3/Admin/DBs/ArchiveIndex_Beta_Install.mdb"
crosswalkSUSTName <- "Crosswalk - Spatial Unit and Species Type Demo.csv"

# TODO: Will no longer need this if we set this up to go over all values of the crosswalk csv.
crosswalkRowNumber <- 1

# ST-Sim  Inputs
# The name of the ssim template library
templateLibraryName <- "CBM-CFS3 Template.ssim"
# The name you want to give to your working copy of the template
myLibraryName <- "cbmcfs3Test.ssim"
myProjectName <- "Definitions"
numTimesteps <- 300
maxAge <- 300
initialStandAge <- 0
standArea <- 1

# First do everything that needs to be done only once for the library
###########################################################
# ST-Sim open session and get parameters from data sheets #
###########################################################
# Open ST-Sim library and project
mySession <- session()
# Make a copy of the template library that will be modified 
# Note that you have to go in and manually change the name of the new library
file.copy(from=paste0(myLibraryDir, templateLibraryName), to=paste0(myLibraryDir, myLibraryName))
myLibrary <- ssimLibrary(name = paste0(myLibraryDir, myLibraryName), session = mySession)
myProject <- project(myLibrary, project=myProjectName)
name(myLibrary) <- myLibraryName


# TODO: Separate out what only needs to happen once from what needs to happen once per row
#       in the cross walk csv file.

##############################
# Library Options (CBM-CFS3) #
# TODO: THis section only needs to happen once across all rows 
##############################
sheetname <- "stsimcbmcfs3_Database"
mySheet <- datasheet(myLibrary, name=sheetname)
mySheet[1, "Path"] <- CBMDatabasePath
saveDatasheet(myLibrary, mySheet, sheetname)

########################
# Definitions (ST-Sim) #
########################
# ST-Sim Terminology
# TODO: Terminology section only needs to happen once across all rows 
sheetName <- "stsim_Terminology"
mySheet <- datasheet(myProject, name=sheetName)
mySheet$AmountLabel[1] <- "Area"
mySheet$AmountUnits[1] <- "Hectares"
mySheet$StateLabelX[1] <- "Species Type"
mySheet$StateLabelY[1] <- "Forest Type"
mySheet$PrimaryStratumLabel[1] <- "Ecological Boundary"
mySheet$SecondaryStratumLabel[1] <- "Administrative Boundary"
mySheet$TimestepUnits[1] <- "Year"
saveDatasheet(myProject, mySheet, sheetName)

#ST-Sim Age Type
# TODO: Ages section only needs to happen once across all rows 
sheetName <- "stsim_AgeType"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet[1,"Frequency"] <- 1
mySheet[1,"MaximumAge"] <- maxAge
saveDatasheet(myProject, mySheet, name=sheetName)

#ST-Sim Age Group
# TODO: Ages section only needs to happen once across all rows 
sheetName <- "stsim_AgeGroup"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet[1:(maxAge/20),"MaximumAge"] <- c(seq(from=20, to=(maxAge-1), by=20), maxAge-1)
saveDatasheet(myProject, mySheet, name=sheetName)

# ST-Sim Run Control
# TODO: Run Control section only needs to happen once across all rows 
maxTimestep <- numTimesteps
maxIteration <- 1
minTimestep <- 0
minIteration <- 1
myScenario <- scenario(myProject, scenario <- paste0("Run Control [Non-spatial; ", maxTimestep, " years; ", maxIteration, " MC]"))
sheetName <- "stsim_RunControl"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet[1,"MinimumIteration"] <- minIteration
mySheet[1,"MaximumIteration"] <- maxIteration
mySheet[1,"MinimumTimestep"] <- minTimestep
mySheet[1,"MaximumTimestep"] <- maxTimestep
mySheet[1,"IsSpatial"] <- FALSE
saveDatasheet(myScenario, mySheet, sheetName)


# ST-Sim Output options
# TODO: Output options section only needs to happen once across all rows 
myScenario <- scenario(myProject, scenario = "Output Options [Non-spatial]")
sheetName <- "stsim_OutputOptions"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet[1, "SummaryOutputSC"] <- T
mySheet[1, "SummaryOutputSCTimesteps"] <- 1
mySheet[1, "SummaryOutputSCZeroValues"] <- F
mySheet[1, "SummaryOutputTR"] <- T
mySheet[1, "SummaryOutputTRTimesteps"] <- 1
mySheet[1, "SummaryOutputTRIntervalMean"] <- F
mySheet[1, "SummaryOutputTRSC"] <- T
mySheet[1, "SummaryOutputTRSCTimesteps"] <- 1
mySheet[1, "SummaryOutputSA"] <- T
mySheet[1, "SummaryOutputSATimesteps"] <- 1
mySheet[1, "SummaryOutputTA"] <- T
mySheet[1, "SummaryOutputTATimesteps"] <- 1
mySheet[1, "SummaryOutputOmitSS"] <- F
mySheet[1, "SummaryOutputOmitTS"] <- F
saveDatasheet(myScenario, mySheet, sheetName)

# Get the SF croswalk sorted out before looping over rows as this should always be the same
crosswalkSF <- datasheet(scenario(myProject,"CBM-CFS3 Crosswalk - Carbon Stock"), name="stsimcbmcfs3_CrosswalkStock")

# Crosswalk function
crossSF <- function(CBMStock){ as.character(crosswalkSF$StockTypeID[crosswalkSF$CBMStock==CBMStock])}

# Identify biomass and DOM stocks
biomassStocks <- unlist(lapply(c("Merchantable", "Foliage", "Other", "Coarse root", "Fine root"), crossSF))
DOMStocks <- unique(unlist(lapply(c("Aboveground Very Fast DOM", "Aboveground Fast DOM", "Aboveground Medium DOM", "Aboveground Slow DOM",
                                    "Belowground Very Fast DOM", "Belowground Fast DOM", "Belowground Slow DOM",
                                    "Softwood Branch Snag", "Softwood Stem Snag", "Hardwood Branch Snag", "Hardwood Stem Snag"), 
                                  crossSF)))
numBiomassStocks <- length(biomassStocks)
numDOMStocks <- length(DOMStocks)



#########################################################
# Start Loop over croswalk rows here
#########################################################

crosswalkSUSTFull <- read.csv(paste0(crosswalkDir, crosswalkSUSTName), check.names = F)

myScenario <- scenario(myProject, scenario = paste0("Initial Conditions [Non-spatial; Single cell; ", standArea, " ha; Age ", initialStandAge, "]"))
sheetName <- "stsim_InitialConditionsNonSpatial"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet[1, "TotalAmount"] <- standArea * nrow(crosswalkSUSTFull)
mySheet[1, "NumCells"] <- nrow(crosswalkSUSTFull)
mySheet[1, "CalcFromDist"] <- T
saveDatasheet(myScenario, mySheet, sheetName)

for (row in 1:nrow(crosswalkSUSTFull)) {
  # row = 2
  # price <- stock[row, "apple"]
  
  #######################
  # Read in input files #
  #######################
  # Read in CBM-CFS3 Crosswalk for Spatial Unit and Species Type
  crosswalkSUST <- read.csv(paste0(crosswalkDir, crosswalkSUSTName), check.names = F)[row,]
  crosswalkSUST$StateLabelX <- gsub("(.*)\\:(.*)","\\1", crosswalkSUST[,"ST-Sim State Class"])
  crosswalkSUST$StateLabelY <- gsub("(.*)\\:(.*)","\\2", crosswalkSUST[,"ST-Sim State Class"])
  
  # Read in CBM-CFS3 Simulation data
  CBMSimulationData <- read.csv(paste0(CBMDir, crosswalkSUST$CBMSimulationDataFileName), header=TRUE, check.names = F)
  # Remove blank column that CBM-CFS exports by default
  CBMSimulationData <- CBMSimulationData[,1:(ncol(CBMSimulationData)-1)]
  
  # ST-Sim Primary Strata 
  sheetName <- "stsim_Stratum"
  mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
  mySheet[1:nrow(crosswalkSUST),"Name"] <- crosswalkSUST[,"ST-Sim Stratum"]
  #mySheet[1,"ID"] <- 1
  saveDatasheet(myProject, mySheet, name=sheetName, append = T)
  
  # ST-Sim Secondary Strata 
  sheetName <- "stsim_SecondaryStratum"
  mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
  mySheet[1:nrow(crosswalkSUST),"Name"] <- crosswalkSUST[,"ST-Sim Secondary Stratum"]
  #mySheet[1,"ID"] <- 1
  saveDatasheet(myProject, mySheet, name=sheetName)
  
  # ST-Sim State Label x
  sheetName <- "stsim_StateLabelX"
  mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
  mySheet[1:nrow(crosswalkSUST),"Name"] <- crosswalkSUST[,"StateLabelX"]
  saveDatasheet(myProject, mySheet, name=sheetName)
  
  # ST-Sim State Label y
  sheetName <- "stsim_StateLabelY"
  mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
  mySheet[1:nrow(crosswalkSUST),"Name"] <- crosswalkSUST[,"StateLabelY"]
  saveDatasheet(myProject, mySheet, name=sheetName)
  
  # ST-Sim StateClass
  sheetName <- "stsim_StateClass"
  mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
  mySheet[1:nrow(crosswalkSUST),"Name"] <- crosswalkSUST[,"ST-Sim State Class"]
  mySheet[1:nrow(crosswalkSUST),"StateLabelXID"] <- crosswalkSUST[,"StateLabelX"]
  mySheet[1:nrow(crosswalkSUST),"StateLabelYID"] <- crosswalkSUST[,"StateLabelY"]
  #mySheet[1, "ID"] <- 1
  saveDatasheet(myProject, mySheet, name=sheetName)
  
  # NB: No transitions for now
  
  ##########################
  # Definitions (CBM-CFS3) #
  ##########################
  sheetName <- "stsimcbmcfs3_EcoBoundary"
  mySheet <- datasheet(myProject, name=sheetName, optional=T)
  mySheet[1:nrow(crosswalkSUST),"Name"] <- crosswalkSUST[,"CBM-CFS3 Ecological Boundary"]
  saveDatasheet(myProject, mySheet, name=sheetName)
  
  sheetName <- "stsimcbmcfs3_AdminBoundary"
  mySheet <- datasheet(myProject, name=sheetName, optional=T)
  mySheet[1:nrow(crosswalkSUST),"Name"] <- crosswalkSUST[,"CBM-CFS3 Administrative Boundary"]
  saveDatasheet(myProject, mySheet, name=sheetName)
  
  sheetName <- "stsimcbmcfs3_SpeciesType"
  mySheet <- datasheet(myProject, name=sheetName, optional=T)
  mySheet[1:nrow(crosswalkSUST),"Name"] <- crosswalkSUST[,"CBM-CFS3 Species Type"]
  saveDatasheet(myProject, mySheet, name=sheetName)
  
  # NB: No disturbances for now
  
  ####################################
  # Sub-scenario datasheets (ST-Sim) #
  ####################################
  
  # ST-Sim Transition Pathways
  myScenario <- scenario(myProject, scenario <- "Pathway Diagram")
  sheetName <- "stsim_DeterministicTransition"
  mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
  mySheet[1:nrow(crosswalkSUST), "StratumIDSource"] <- crosswalkSUST[,"ST-Sim Stratum"]
  mySheet[1:nrow(crosswalkSUST), "StateClassIDSource"] <- crosswalkSUST[,"ST-Sim State Class"]
  mySheet[1:nrow(crosswalkSUST), "Location"] <- paste0("A", c(row))
  saveDatasheet(myScenario, mySheet, sheetName, append = T)
  
  # NB: No probabilistic transitions for now
  
  # ST-Sim Initial conditions
  myScenario <- scenario(myProject, scenario = paste0("Initial Conditions [Non-spatial; Single cell; ", standArea, " ha; Age ", initialStandAge, "]"))
  sheetName <- "stsim_InitialConditionsNonSpatialDistribution"
  mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
  mySheet[1:nrow(crosswalkSUST), "StratumID"] <- crosswalkSUST[,"ST-Sim Stratum"]
  mySheet[1:nrow(crosswalkSUST), "SecondaryStratumID"] <- crosswalkSUST[,"ST-Sim Secondary Stratum"]
  mySheet[1:nrow(crosswalkSUST), "StateClassID"] <- crosswalkSUST[,"ST-Sim State Class"]
  mySheet[1:nrow(crosswalkSUST), "AgeMin"] <- initialStandAge
  mySheet[1:nrow(crosswalkSUST), "RelativeAmount"] <- standArea
  saveDatasheet(myScenario, mySheet, sheetName, append = T)
  
  #ST-Sim State attribute values
  
  # Connect to CBM-CFS3 "ArchiveIndex_Beta_Install.mdb" to assess if the species is Softwood or Hardwood
  CBMdatabase <- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", CBMDatabasePath))
  
  # Get Species and Forest Type IDs
  speciesTypeTable <- sqlFetch(CBMdatabase, "tblSpeciesTypeDefault")
  speciesTypeID <- speciesTypeTable$SpeciesTypeID[speciesTypeTable$SpeciesTypeName == as.character(crosswalkSUST[1, "CBM-CFS3 Species Type"])]
  forestTypeID <- speciesTypeTable$ForestTypeID[speciesTypeTable$SpeciesTypeID == speciesTypeID]
  
  # Get Forest Type Name
  forestTypeTable <- sqlFetch(CBMdatabase, "tblForestTypeDefault")
  forestType <- as.character(forestTypeTable$ForestTypeName[forestTypeTable$ForestTypeID == forestTypeID])
  
  # Close the database (do not forget this, otherwise you lock access database from editing)
  close(CBMdatabase)
  
  # State Attribute Values for initial carbon stocks of biomass (live) components using carbon estimates
  myScenario <- scenario(myProject, scenario = "State Attribute Values")
  sheetName <- "stsim_StateAttributeValue"
  stateAttributeInitialCarbonBiomass <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
  stateAttributeInitialCarbonBiomass[1:(nrow(CBMSimulationData)*numBiomassStocks), "StratumID"] <- crosswalkSUST[1, "ST-Sim Stratum"]             
  stateAttributeInitialCarbonBiomass[1:(nrow(CBMSimulationData)*numBiomassStocks), "SecondaryStratumID"] <- crosswalkSUST[1, "ST-Sim Secondary Stratum"]
  stateAttributeInitialCarbonBiomass[1:(nrow(CBMSimulationData)*numBiomassStocks), "StateClassID"] <- crosswalkSUST[1, "ST-Sim State Class"]
  stateAttributeInitialCarbonBiomass[1:(nrow(CBMSimulationData)*numBiomassStocks), "StateAttributeTypeID"] <- c(rep("Carbon Initial Conditions: Merchantable", nrow(CBMSimulationData)),
                                                                                                                rep("Carbon Initial Conditions: Other Wood", nrow(CBMSimulationData)),
                                                                                                                rep("Carbon Initial Conditions: Foliage", nrow(CBMSimulationData)),
                                                                                                                rep("Carbon Initial Conditions: Fine Roots", nrow(CBMSimulationData)),
                                                                                                                rep("Carbon Initial Conditions: Coarse Roots", nrow(CBMSimulationData)))
  stateAttributeInitialCarbonBiomass[1:(nrow(CBMSimulationData)*numBiomassStocks), "AgeMin"] <- rep(CBMSimulationData[,"Time Step"], numBiomassStocks)
  stateAttributeInitialCarbonBiomass[1:(nrow(CBMSimulationData)*numBiomassStocks), "AgeMax"] <- rep(CBMSimulationData[,"Time Step"], numBiomassStocks)
  stateAttributeInitialCarbonBiomass[stateAttributeInitialCarbonBiomass$AgeMin == (nrow(CBMSimulationData)-1), "AgeMax"] <- NA
  stateAttributeInitialCarbonBiomass[1:(nrow(CBMSimulationData)*numBiomassStocks), "Value"] <- c(CBMSimulationData[,paste(forestType, "Merchantable")],
                                                                                                 CBMSimulationData[,paste(forestType, "Other")],
                                                                                                 CBMSimulationData[,paste(forestType, "Foliage")],
                                                                                                 CBMSimulationData[,paste(forestType, "Fine Roots")],
                                                                                                 CBMSimulationData[,paste(forestType, "Coarse Roots")])
  
  # State Attribute Values for initial carbon stocks of DOM components using CBM-CFS3
  sheetName <- "stsim_StateAttributeValue"
  stateAttributeInitialCarbonDOM <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
  stateAttributeInitialCarbonDOM[1:(nrow(CBMSimulationData)*numDOMStocks), "StratumID"] <- crosswalkSUST[1, "ST-Sim Stratum"]             
  stateAttributeInitialCarbonDOM[1:(nrow(CBMSimulationData)*numDOMStocks), "SecondaryStratumID"] <- crosswalkSUST[1, "ST-Sim Secondary Stratum"]
  stateAttributeInitialCarbonDOM[1:(nrow(CBMSimulationData)*numDOMStocks), "StateClassID"] <- crosswalkSUST[1, "ST-Sim State Class"]
  stateAttributeInitialCarbonDOM[1:(nrow(CBMSimulationData)*numDOMStocks), "StateAttributeTypeID"] <- c(rep("Carbon Initial Conditions: Aboveground Very Fast", nrow(CBMSimulationData)),
                                                                                                        rep("Carbon Initial Conditions: Aboveground Fast", nrow(CBMSimulationData)),
                                                                                                        rep("Carbon Initial Conditions: Aboveground Medium", nrow(CBMSimulationData)),
                                                                                                        rep("Carbon Initial Conditions: Aboveground Slow", nrow(CBMSimulationData)),
                                                                                                        rep("Carbon Initial Conditions: Belowground Very Fast", nrow(CBMSimulationData)),
                                                                                                        rep("Carbon Initial Conditions: Belowground Fast", nrow(CBMSimulationData)),
                                                                                                        rep("Carbon Initial Conditions: Belowground Slow", nrow(CBMSimulationData)),
                                                                                                        rep("Carbon Initial Conditions: Snag Branch", nrow(CBMSimulationData)),
                                                                                                        rep("Carbon Initial Conditions: Snag Stem", nrow(CBMSimulationData)))
  stateAttributeInitialCarbonDOM[1:(nrow(CBMSimulationData)*numDOMStocks), "AgeMin"] <- rep(CBMSimulationData[, "Time Step"], numDOMStocks)
  stateAttributeInitialCarbonDOM[1:(nrow(CBMSimulationData)*numDOMStocks), "AgeMax"] <- rep(CBMSimulationData[, "Time Step"], numDOMStocks)
  stateAttributeInitialCarbonDOM[stateAttributeInitialCarbonDOM$AgeMin == (nrow(CBMSimulationData)-1), "AgeMax"] <- NA
  stateAttributeInitialCarbonDOM[1:(nrow(CBMSimulationData)*numDOMStocks), "Value"] <- c(CBMSimulationData[,"Aboveground Very Fast DOM"],
                                                                                         CBMSimulationData[,"Aboveground Fast DOM"],
                                                                                         CBMSimulationData[,"Medium DOM"],
                                                                                         CBMSimulationData[,"Aboveground Slow DOM"],
                                                                                         CBMSimulationData[,"Belowground Very Fast DOM"],
                                                                                         CBMSimulationData[,"Belowground Fast DOM"],
                                                                                         CBMSimulationData[,"Belowground Slow DOM"],
                                                                                         CBMSimulationData[,paste(forestType, "Branch Snag")],
                                                                                         CBMSimulationData[,paste(forestType, "Stem Snag")])
  
  
  # Combine all State Attribute Values
  stateAttributes <- rbind(stateAttributeInitialCarbonBiomass, stateAttributeInitialCarbonDOM)
  saveDatasheet(myScenario, stateAttributes, sheetName, append = T)
  
  
}


######################################
# Sub-scenario datasheets (CBM-CFS3) #
######################################
myScenario <- scenario(myProject, scenario = "CBM-CFS3 Crosswalk - Spatial Unit and Species Type")
sheetName <- "stsimcbmcfs3_CrosswalkSpecies"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet[1:nrow(crosswalkSUSTFull), "EcoBoundaryID"] <- crosswalkSUSTFull[, "CBM-CFS3 Ecological Boundary"]
mySheet[1:nrow(crosswalkSUSTFull), "AdminBoundaryID"] <- crosswalkSUSTFull[, "CBM-CFS3 Administrative Boundary"]
mySheet[1:nrow(crosswalkSUSTFull), "SpeciesTypeID"] <- crosswalkSUSTFull[, "CBM-CFS3 Species Type"]
mySheet[1:nrow(crosswalkSUSTFull), "StratumID"] <- crosswalkSUSTFull[, "ST-Sim Stratum"]
mySheet[1:nrow(crosswalkSUSTFull), "SecondaryStratumID"] <- crosswalkSUSTFull[, "ST-Sim Secondary Stratum"]
mySheet[1:nrow(crosswalkSUSTFull), "TertiaryStratumID"] <- crosswalkSUSTFull[, "ST-Sim Tertiary Stratum"]
mySheet[1:nrow(crosswalkSUSTFull), "StateClassID"] <- crosswalkSUSTFull[, "ST-Sim State Class"]
saveDatasheet(myScenario, mySheet, sheetName)

#############################
# Full scenarios (STSim SF) #
# TODO: This section only needs to happen once across all rows 
#############################
myScenarioName <- "Demo"
myScenario = scenario(myProject, scenario = myScenarioName)
dependency(myScenario, 
           c("Run Control [Non-spatial; 300 years; 1 MC]",
             "Pathway Diagram",
             "Initial Conditions [Non-spatial; Single cell; 1 ha; Age 0]",
             "Output Options [Non-spatial]",
             "State Attribute Values",
             "SF Flow Pathways",
             "SF Initial Stocks",
             "SF Output Options",
             "SF Flow Order",
             "SF Stock Group Membership",
             "SF Flow Group Membership",
             "CBM-CFS3 Crosswalk - Carbon Stock",
             "CBM-CFS3 Crosswalk - Spatial Unit and Species Type"))

# TODO: Need to fix transformer.R before being able to run.
run(myProject, scenario = myScenarioName, summary = TRUE, jobs = 1)

###################################
# Validation scenarios (STSim SF) #
###################################
# Scenario(s) to validate with CBM-CFS3
myValidationScenarioName <- "Demo CBM-CFS3 Validation"
myValidationScenario = scenario(myProject, scenario = myValidationScenarioName)
dependency(myValidationScenario, 
           c("Run Control [Non-spatial; 300 years; 1 MC]",
             "Pathway Diagram",
             "Initial Conditions [Non-spatial; Single cell; 1 ha; Age 0]",
             "Output Options [Non-spatial]",
             "State Attribute Values"))

# Go to Syncrosim UI and run ST-Sim for this scenario, then run this next line of code
# TODO: May need to update "createCBMValidationScenario" to work with multiple crosswalk rows.
createCBMValidationScenario(myLibraryDir, myLibraryName, myValidationScenarioName, crosswalkSF, crosswalkSUSTFull)

