createCBMValidationScenario <- function(myLibraryDir, myLibraryName, myValidationScenarioName, crosswalkSF, crosswalkSUSTFull){
  
  
  
  # loop over the rows in the croswalkSUSTFull
  for (row in 1:nrow(crosswalkSUSTFull)) {
    #row = 1
    
    ##########################################################
    # Connect to STSim Library to add CBM validation results #
    ##########################################################
    
    # Connect to the ssim database
    drv <- dbDriver("SQLite")
    con <- dbConnect(drv, paste0(myLibraryDir, myLibraryName))
    
    # Read in CBM-CFS3 Crosswalk for Spatial Unit and Species Type
    crosswalkSUST <- read.csv(paste0(crosswalkDir, crosswalkSUSTName), check.names = F)[row,]
    crosswalkSUST$StateLabelX <- gsub("(.*)\\:(.*)","\\1", crosswalkSUST[,"ST-Sim State Class"])
    crosswalkSUST$StateLabelY <- gsub("(.*)\\:(.*)","\\2", crosswalkSUST[,"ST-Sim State Class"])
    
    # Connect to CBM-CFS3 "ArchiveIndex_Beta_Install.mdb" to assess if the species is Softwood or Hardwood
    CBMdatabase <- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", CBMDatabasePath))
    
    # Get Species and Forest Type IDs
    speciesTypeTable <- sqlFetch(CBMdatabase, "tblSpeciesTypeDefault")
    speciesTypeID <- speciesTypeTable$SpeciesTypeID[speciesTypeTable$SpeciesTypeName == as.character(crosswalkSUST[1, "CBM-CFS3 Species Type"])]
    forestTypeID <- speciesTypeTable$ForestTypeID[speciesTypeTable$SpeciesTypeID == speciesTypeID]
    
    # Get Forest Type Name
    forestTypeTable <- sqlFetch(CBMdatabase, "tblForestTypeDefault")
    forestType <- as.character(forestTypeTable$ForestTypeName[forestTypeTable$ForestTypeID == forestTypeID])
    
    close(CBMdatabase)
    
    CBMSimulationData <- read.csv(paste0(CBMDir, crosswalkSUST$CBMSimulationDataFileName), header=TRUE, check.names = F)
    # Remove blank column that CBM-CFS exports by default
    validationDataWide <- CBMSimulationData[,1:(ncol(CBMSimulationData)-1)]
    
    # Crosswalk function
    crossSF <- function(CBMStock){ as.character(crosswalkSF$StockTypeID[crosswalkSF$CBMStock==CBMStock])}
    
    # convert validation carbon data from wide to long format
    if(forestType == "Softwood"){
      validationStocks <- c("Aboveground Very Fast DOM", "Aboveground Fast DOM", "Medium DOM", "Aboveground Slow DOM",
                            "Belowground Very Fast DOM", "Belowground Fast DOM", "Belowground Slow DOM",
                            "Softwood Branch Snag", "Softwood Stem Snag",
                            "Softwood Merchantable", "Softwood Foliage", "Softwood Other", "Softwood Coarse Roots", "Softwood Fine Roots")
    }
    if(forestType == "Hardwood"){
      validationStocks <- c("Aboveground Very Fast DOM", "Aboveground Fast DOM", "Medium DOM", "Aboveground Slow DOM",
                            "Belowground Very Fast DOM", "Belowground Fast DOM", "Belowground Slow DOM",
                            "Hardwood Branch Snag", "Hardwood Stem Snag",
                            "Hardwood Merchantable", "Hardwood Foliage", "Hardwood Other", "Hardwood Coarse Roots", "Hardwood Fine Roots")
    }
    validationDataWide <- cbind("Timestep"=validationDataWide[, "Time Step"], validationDataWide[, names(validationDataWide) %in% validationStocks])
    validationCarbon <- gather(validationDataWide, Name, Amount, names(validationDataWide)[2:ncol(validationDataWide)], factor_key = TRUE)
    validationCarbon$Name <- as.character(validationCarbon$Name)
    validationCarbon$Name[validationCarbon$Name %in% c("Hardwood Fine Roots", "Softwood Fine Roots")] <- "Fine root"
    validationCarbon$Name[validationCarbon$Name %in% c("Hardwood Coarse Roots", "Softwood Coarse Roots")] <- "Coarse root"
    validationCarbon$Name[validationCarbon$Name %in% c("Hardwood Merchantable", "Softwood Merchantable")] <- "Merchantable"
    validationCarbon$Name[validationCarbon$Name %in% c("Hardwood Foliage", "Softwood Foliage")] <- "Foliage"
    validationCarbon$Name[validationCarbon$Name %in% c("Hardwood Other", "Softwood Other")] <- "Other"
    validationCarbon$Name[validationCarbon$Name == "Medium DOM"] <- "Aboveground Medium DOM"
    validationCarbon$Name <- unlist(lapply(validationCarbon$Name, crossSF))
    validationCarbon$Name <- paste(validationCarbon$Name, "[Type]")
    
    SSim_Scenario <- dbReadTable(con, "core_Scenario")
    myScenarioID <- SSim_Scenario[SSim_Scenario$Name==myValidationScenarioName, "ScenarioID"]
    SSim_ScenarioResult <-dbReadTable(con, "core_ScenarioResult")
    
    myResultScenarioID <- SSim_ScenarioResult[SSim_ScenarioResult$ScenarioID==myScenarioID, "ResultID"]
    myProjectID <- SSim_Scenario[SSim_Scenario$Name==myValidationScenarioName, "ProjectID"]
    
    STSim_Stratum <- dbReadTable(con, "stsim_Stratum") 
    myStratumID <- STSim_Stratum[STSim_Stratum$Name==crosswalkSUST[1,"ST-Sim Stratum"], "StratumID"]
    
    STSim_SecondaryStratum <- dbReadTable(con, "stsim_SecondaryStratum") 
    mySecondaryStratumID <- STSim_SecondaryStratum[STSim_SecondaryStratum$Name==crosswalkSUST[1,"ST-Sim Secondary Stratum"], "SecondaryStratumID"]
    
    STSim_TertiaryStratum <- dbReadTable(con, "stsim_TertiaryStratum") 
    myTertiaryStratumID <- STSim_TertiaryStratum[STSim_TertiaryStratum$Name==crosswalkSUST[1,"ST-Sim Tertiary Stratum"], "TertiaryStratumID"]
    
    STSim_StateClass <- dbReadTable(con, "stsim_StateClass")
    STSim_StateClass <- STSim_StateClass[STSim_StateClass$ProjectID==myProjectID,]
    myStateClassID <- STSim_StateClass[STSim_StateClass$Name==crosswalkSUST[1,"ST-Sim State Class"], "StateClassID"]
    
    SF_StockGroup <- dbReadTable(con, "stsimsf_StockGroup")
    SF_StockGroup <- SF_StockGroup[SF_StockGroup$ProjectID==myProjectID,]
    
    validationCarbon <- merge(validationCarbon, SF_StockGroup[,c("StockGroupID", "Name")], by="Name", all.x=TRUE)
    validationCarbon$ScenarioID <- myResultScenarioID
    validationCarbon$Iteration <- 1
    validationCarbon$StateClassID <- myStateClassID
    validationCarbon$StratumID <- myStratumID
    validationCarbon$SecondaryStratumID <- mySecondaryStratumID
    validationCarbon$TertiaryStratumID <- NA
    validationCarbon <- validationCarbon[,!(names(validationCarbon) %in% c("Name"))]
    
    SF_OutputStock <- dbReadTable(con, "stsimsf_OutputStock") 
    SF_OutputStock1 <- rbind(SF_OutputStock, validationCarbon)
    
    dbWriteTable(con, name="stsimsf_OutputStock", value=SF_OutputStock1, append=F, overwrite=T)
    
    dbDisconnect(con)
    
  }
}