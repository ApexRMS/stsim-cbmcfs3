library(methods)
library(rsyncrosim)
library(tidyverse)
library(RODBC)
# Before running script make sure that Microsoft Access Database Engine is installed so that you can connect to a MS Access db from R x64bit
# On Sept 27 2018, I installed AccessDatabaseEngine_X64.exe from https://www.microsoft.com/en-us/download/details.aspx?id=13255

# Get ST-Sim library and scenario
myLibrary <- ssimLibrary()
myScenario <- scenario()

###################################
# Get CBM database and crosswalks #
###################################
CBMDatabase <- datasheet(myLibrary, "stsimcbmcfs3_Database")[1,"Path"]
crosswalkStratumState <- datasheet(myScenario, "stsimcbmcfs3_CrosswalkSpecies")
crosswalkStock <- datasheet(myScenario, "stsimcbmcfs3_CrosswalkStock")
#crosswalkTransition <- datasheet(myScenario, "CBMCFS3_CrosswalkDisturbance")

# Crosswalk functions
crossSF <- function(CBMStock){ as.character(crosswalkStock$StockTypeID[crosswalkStock$CBMStock==CBMStock])}
#crossTG  <- function(CBMDisturbace){ as.character(crosswalkTransition$DisturbanceTypeID[crosswalkTransition$CBMTransitionGroupID==CBMDisturbance])}

# Identify biomass and DOM stocks
biomassStocks <- unlist(lapply(c("Merchantable", "Foliage", "Other", "Coarse root", "Fine root"), crossSF))
DOMStocks <- unique(unlist(lapply(c("Aboveground Very Fast DOM", "Aboveground Fast DOM", "Aboveground Medium DOM", "Aboveground Slow DOM",
                                    "Belowground Very Fast DOM", "Belowground Fast DOM", "Belowground Slow DOM",
                                    "Softwood Branch Snag", "Softwood Stem Snag",
                                    "Hardwood Branch Snag", "Hardwood Stem Snag"), 
                                  crossSF)))
numBiomassStocks <- length(biomassStocks)
numDOMStocks <- length(DOMStocks)

# SF Flow Pathways
flowPathways = datasheet(myScenario, name="stsimsf_FlowPathway", empty=F, optional=T)

# Identify growth, biomass transfer, emission, decay, and DOM transfer flows
growthFlows <- flowPathways[flowPathways$FromStockTypeID == crossSF("Atmosphere")]
emissionFlows <- flowPathways[flowPathways$ToStockTypeID == crossSF("Atmosphere"),]
biomassTurnoverFlows <- flowPathways[(flowPathways$FromStockTypeID %in% biomassStocks & flowPathways$ToStockTypeID %in% DOMStocks),]
DOMTransferFlows <- distinct(rbind(flowPathways[flowPathways$FromStockTypeID==crossSF("Aboveground Slow DOM") & flowPathways$ToStockTypeID == crossSF("Belowground Slow DOM"),],
                          flowPathways[flowPathways$FromStockTypeID==crossSF("Softwood Stem Snag") & flowPathways$ToStockTypeID == crossSF("Aboveground Medium DOM"),],
                          flowPathways[flowPathways$FromStockTypeID==crossSF("Softwood Branch Snag") & flowPathways$ToStockTypeID == crossSF("Aboveground Fast DOM"),],
                          flowPathways[flowPathways$FromStockTypeID==crossSF("Hardwood Stem Snag") & flowPathways$ToStockTypeID == crossSF("Aboveground Medium DOM"),],
                          flowPathways[flowPathways$FromStockTypeID==crossSF("Hardwood Branch Snag") & flowPathways$ToStockTypeID == crossSF("Aboveground Fast DOM"),]))
decayFlows <- rbind(flowPathways[(flowPathways$FromStockTypeID %in% DOMStocks & flowPathways$ToStockTypeID %in% DOMStocks) & (!(flowPathways$FlowTypeID %in% DOMTransferFlows$FlowTypeID)),])

####################################################
# CBM parameters that were not in the CBM database #
####################################################
# DOM Pool ID - "SoilPoolID" taken from CBM User manual Appendix 4 (Kull et al. 2016) - Not found in CMB database
DOMPoolID <- data.frame(CBMStock=c("Aboveground Very Fast DOM", "Belowground Very Fast DOM", "Aboveground Fast DOM", "Belowground Fast DOM", 
                                   "Aboveground Medium DOM", "Aboveground Slow DOM", "Belowground Slow DOM", "Softwood Stem Snag", "Softwood Branch Snag",
                                   "Hardwood Stem Snag", "Hardwood Branch Snag", "Black Carbon", "Peat"), 
                        SoilPoolID=c(0:12))
crosswalkStock <- merge(crosswalkStock, DOMPoolID, all=T)

# Get biomass turnover Proportions (not found in CBM database), taken from Kurtz et al. 2009
proportionMerchantableToSnag <- 1
proportionFineRootsToAGVeryFast <- 0.5
proportionFineRootsToBGVeryFast <- 0.5
proportionCoarseRootsToAGFast <- 0.5
proportionCoarseRootsToBGFast <- 0.5

stateAttributesNetGrowthMaster = datasheet(myScenario, name="stsim_StateAttributeValue", empty = T, optional = T)
flowMultiplierMaster <- datasheet(myScenario, name="stsimsf_FlowMultiplier", empty = T, optional = T)

# Loop over all entries in crosswalkStratumState
for(i in 1: nrow(crosswalkStratumState)){
  #i<-4
  ####################################
  # CBM parameters from CBM Database # 
  ####################################
  # Connect to CBM-CFS3 "ArchiveIndex_Beta_Install.mdb"
  CBMdatabase <- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", CBMDatabase))
  
  # Get Admin Boundary ID
  adminBoundaryTable <- sqlFetch(CBMdatabase, "tblAdminBoundaryDefault")
  adminBoundaryID <- adminBoundaryTable$AdminBoundaryID[adminBoundaryTable$AdminBoundaryName == as.character(crosswalkStratumState$AdminBoundaryID[i])]
  
  # Get Ecological Boundary ID
  ecoBoundaryTable <- sqlFetch(CBMdatabase, "tblEcoBoundaryDefault")
  ecoBoundaryID <- ecoBoundaryTable$EcoBoundaryID[ecoBoundaryTable$EcoBoundaryName == as.character(crosswalkStratumState$EcoBoundaryID[i])]
  
  # Get Species and Forest Type IDs
  speciesTypeTable <- sqlFetch(CBMdatabase, "tblSpeciesTypeDefault")
  speciesTypeID <- speciesTypeTable$SpeciesTypeID[speciesTypeTable$SpeciesTypeName == as.character(crosswalkStratumState$SpeciesTypeID[i])]
  forestTypeID <- speciesTypeTable$ForestTypeID[speciesTypeTable$SpeciesTypeID == speciesTypeID]
  
  # Get Forest Type Name
  forestTypeTable <- sqlFetch(CBMdatabase, "tblForestTypeDefault")
  ForestType <- as.character(forestTypeTable$ForestTypeName[forestTypeTable$ForestTypeID == forestTypeID])
  
  # Get Spatial Planning Unit ID (SPUID) from adminBoundary and ecoBoundary
  SPUTable <- sqlFetch(CBMdatabase, "tblSPUDefault")
  SPUID <- SPUTable$SPUID[SPUTable$AdminBoundaryID==adminBoundaryID & SPUTable$EcoBoundaryID==ecoBoundaryID]
  
  # Get biomass expansion factors
  biomassExpansionTable <- sqlFetch(CBMdatabase,"tblBioTotalStemwoodForestTypeDefault")
  
  # Get biomass to carbon multipliers
  biomassComponentTable <- sqlFetch(CBMdatabase,"tblBiomassComponent")
  biomassToCarbonTable <- sqlFetch(CBMdatabase,"tblBiomassToCarbonDefault")
  biomassToCarbonTable <- merge.data.frame(biomassToCarbonTable, biomassComponentTable)
  
  # Decay multipliers
  # Temperature modifier parameters
  climateTable <- sqlFetch(CBMdatabase,"tblClimateDefault")
  # There are 2 reference years but they seem to have the same values, I'm arbitrarily choosing 1980
  climateRefYear <- 1980
  meanAnnualTemp <- climateTable[climateTable$DefaultSPUID==SPUID & climateTable$Year == climateRefYear, "MeanAnnualTemp"]
  # Stand modifier parameters
  # Note that the maxDecayMult in CBM-CFS3 is 1 which makes the StandMod = 1
  # Do not calculate StandMod this round
  # From Kurz et al. 2009: "In CBM-CFS2 the default value for MaxDecayMult was two. In the CBM-CFS3 the value 
  # defaults to one because more recent studies that examined open canopy effects on decomposition indicated 
  # that decomposition rates are not always higher under open canopies and that decomposition rate responses 
  # may be ecosystem specific (Yanai et al., 2000)." 
  maxDecayMult <- ecoBoundaryTable[ecoBoundaryTable$EcoBoundaryID==ecoBoundaryID, "DecayMult"]
  
  # Get DOM parameters
  DOMParametersTable <- sqlFetch(CBMdatabase,"tblDOMParametersDefault")
  DOMParametersTable <- merge(crosswalkStock[crosswalkStock$StockTypeID %in% DOMStocks,], DOMParametersTable)
  DOMParametersTable$TempMod <- exp((meanAnnualTemp - DOMParametersTable$ReferenceTemp)*log(DOMParametersTable$Q10)*0.1)
  if(ForestType=="Softwood"){
    DOMParametersTable <- DOMParametersTable[DOMParametersTable$CBMStock != "Hardwood Stem Snag",]
    DOMParametersTable <- DOMParametersTable[DOMParametersTable$CBMStock != "Hardwood Branch Snag",]
  }
  if(ForestType=="Hardwood"){
    DOMParametersTable <- DOMParametersTable[DOMParametersTable$CBMStock != "Softwood Stem Snag",]
    DOMParametersTable <- DOMParametersTable[DOMParametersTable$CBMStock != "Softwood Branch Snag",]
  }
  DOMDecayTable <- merge(decayFlows, DOMParametersTable, by.x="FromStockTypeID", by.y="StockTypeID")
  DOMEmissionTable <- merge(emissionFlows, DOMParametersTable, by.x="FromStockTypeID", by.y="StockTypeID")
  DOMDecayTable$Multiplier <- (1 - DOMDecayTable$PropToAtmosphere) * DOMDecayTable$OrganicMatterDecayRate * DOMDecayTable$TempMod
  DOMEmissionTable$Multiplier <- DOMEmissionTable$PropToAtmosphere * DOMEmissionTable$OrganicMatterDecayRate * DOMEmissionTable$TempMod
  DOMTable <- rbind(DOMDecayTable, DOMEmissionTable)
  
  # Get DOM transfer rates
  DOMTransferTable <- sqlFetch(CBMdatabase, "tblSlowAGToBGTransferRate")
  transferRateSlowAGToBG <- signif(DOMTransferTable$SlowAGToBGTransferRate,6)
  if(ForestType=="Softwood"){
    transferRateStemSnagToDOM <- signif(ecoBoundaryTable$SoftwoodStemSnagToDOM[ecoBoundaryTable$EcoBoundaryName == as.character(crosswalkStratumState$EcoBoundaryID[i])],6)
    transferRateBranchSnagToDOM <- signif(ecoBoundaryTable$SoftwoodBranchSnagToDOM[ecoBoundaryTable$EcoBoundaryName == as.character(crosswalkStratumState$EcoBoundaryID[i])],6)
  }
  if(ForestType=="Hardwood"){
    transferRateStemSnagToDOM <- signif(ecoBoundaryTable$HardwoodStemSnagToDOM[ecoBoundaryTable$EcoBoundaryName == as.character(crosswalkStratumState$EcoBoundaryID[i])],6)
    transferRateBranchSnagToDOM <- signif(ecoBoundaryTable$HardwoodBranchSnagToDOM[ecoBoundaryTable$EcoBoundaryName == as.character(crosswalkStratumState$EcoBoundaryID[i])],6)
  }
  # Get biomass turnover rates
  speciesTurnoverRatesTable <- sqlFetch(CBMdatabase, "tblSpeciesTypeDefault")
  turnoverRates <- speciesTurnoverRatesTable[speciesTurnoverRatesTable$SpeciesTypeName==as.character(crosswalkStratumState$SpeciesTypeID[i]),]
  
  # Close the database (do not forget this, otherwise you lock access database from editing)
  close(CBMdatabase)
  
  # Get biomass turnover Proportions (not found in CBM database), taken from Kurtz et al. 2009
  if(ForestType == "Softwood") proportionFoliageToAGVeryFast <- 1
  if(ForestType == "Hardwood") proportionFoliageToAGVeryFast <- 1
  
  #####################################################
  # Biomass Turnover and DOM Decay and Transfer rates #
  #####################################################
  # DOM transfer rates
  DOMTransferFlows[DOMTransferFlows$FromStockTypeID==crossSF("Aboveground Slow DOM") & DOMTransferFlows$ToStockTypeID == crossSF("Belowground Slow DOM"), "Multiplier"] <- transferRateSlowAGToBG
  DOMTransferFlows[DOMTransferFlows$FromStockTypeID==crossSF("Softwood Stem Snag") & DOMTransferFlows$ToStockTypeID == crossSF("Aboveground Medium DOM"), "Multiplier"] <- transferRateStemSnagToDOM
  DOMTransferFlows[DOMTransferFlows$FromStockTypeID==crossSF("Softwood Branch Snag") & DOMTransferFlows$ToStockTypeID == crossSF("Aboveground Fast DOM"), "Multiplier"] <- transferRateBranchSnagToDOM
  DOMTable <- rbind(DOMTable[,names(DOMTransferFlows)], DOMTransferFlows)
  
  # Biomass turnover rates
  turnOverRateStemAnnual <- signif(ecoBoundaryTable$StemAnnualTurnOverRate[ecoBoundaryTable$EcoBoundaryName == as.character(crosswalkStratumState$EcoBoundaryID[i])],6)
  turnOverRateFineRootsAGVeryFast <- signif(turnoverRates$FineRootTurnPropSlope, 6)
  turnOverRateFineRootsBGVeryFast <- signif(turnoverRates$FineRootTurnPropSlope, 6)
  turnOverRateCoarseRootsAGFast <- signif(turnoverRates$CoarseRootTurnProp, 6)
  turnOverRateCoarseRootsBGFast <- signif(turnoverRates$CoarseRootTurnProp, 6)
  if(ForestType == "Softwood"){
    turnOverRateBranch <- signif(ecoBoundaryTable$SoftwoodBranchTurnOverRate[ecoBoundaryTable$EcoBoundaryName == as.character(crosswalkStratumState$EcoBoundaryID[i])],6)
    turnOverRateFoliage <- signif(ecoBoundaryTable$SoftwoodFoliageFallRate[ecoBoundaryTable$EcoBoundaryName == as.character(crosswalkStratumState$EcoBoundaryID[i])],6)
    }
  if(ForestType == "Hardwood"){
    turnOverRateBranch <- signif(ecoBoundaryTable$HardwoodBranchTurnOverRate[ecoBoundaryTable$EcoBoundaryName == as.character(crosswalkStratumState$EcoBoundaryID[i])],6)
    turnOverRateFoliage <- signif(ecoBoundaryTable$HardwoodFoliageFallRate[ecoBoundaryTable$EcoBoundaryName == as.character(crosswalkStratumState$EcoBoundaryID[i])],6)
    }
    
  # Turnover proportions
  proportionOtherToBranchSnag <- signif(turnoverRates$BranchesToBranchSnag,6)
  proportionOtherToAGFast <- 1 - proportionOtherToBranchSnag
  
  biomassTurnoverTable <- biomassTurnoverFlows
  biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeID == crossSF("Merchantable")] <- turnOverRateStemAnnual * proportionMerchantableToSnag 
  if(ForestType == "Softwood") biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeID == crossSF("Other") & biomassTurnoverTable$ToStockTypeID == crossSF("Softwood Branch Snag")] <- turnOverRateBranch * proportionOtherToBranchSnag
  if(ForestType == "Hardwood") biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeID == crossSF("Other") & biomassTurnoverTable$ToStockTypeID == crossSF("Hardwood Branch Snag")] <- turnOverRateBranch * proportionOtherToBranchSnag
  biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeID == crossSF("Other") & biomassTurnoverTable$ToStockTypeID == crossSF("Aboveground Fast DOM")] <- turnOverRateBranch * proportionOtherToAGFast
  biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeID == crossSF("Foliage")] <- turnOverRateFoliage * proportionFoliageToAGVeryFast 
  biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeID == crossSF("Fine root") & biomassTurnoverTable$ToStockTypeID == crossSF("Aboveground Very Fast DOM")] <- turnOverRateFineRootsAGVeryFast * proportionFineRootsToAGVeryFast
  biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeID == crossSF("Fine root") & biomassTurnoverTable$ToStockTypeID == crossSF("Belowground Very Fast DOM")] <- turnOverRateFineRootsBGVeryFast * proportionFineRootsToBGVeryFast
  biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeID == crossSF("Coarse root") & biomassTurnoverTable$ToStockTypeID == crossSF("Aboveground Fast DOM")] <- turnOverRateCoarseRootsAGFast * proportionCoarseRootsToAGFast
  biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeID == crossSF("Coarse root") & biomassTurnoverTable$ToStockTypeID == crossSF("Belowground Fast DOM")] <- turnOverRateCoarseRootsBGFast * proportionCoarseRootsToBGFast

  ########################################################
  # Calculate net growth based on mass-balance equations #
  ########################################################
  stateAttributeValues <- datasheet(myScenario, "stsim_StateAttributeValue", empty=FALSE, optional=TRUE)
  stateAttributeValuesWide <- spread(stateAttributeValues, key="StateAttributeTypeID", value = "Value")
  carbonInitialConditions <- datasheet(myScenario, "stsimsf_InitialStockNonSpatial", empty=FALSE, optional=TRUE)
  
  volumeToCarbon <- filter(stateAttributeValuesWide, StratumID == crosswalkStratumState$StratumID[i] & SecondaryStratumID == crosswalkStratumState$SecondaryStratumID[i])
  volumeToCarbon$c_m <- volumeToCarbon[, as.character(carbonInitialConditions$StateAttributeTypeID[carbonInitialConditions$StockTypeID == crossSF("Merchantable")])]
  volumeToCarbon$c_foliage <- volumeToCarbon[, as.character(carbonInitialConditions$StateAttributeTypeID[carbonInitialConditions$StockTypeID == crossSF("Foliage")])]
  volumeToCarbon$c_other <- volumeToCarbon[, as.character(carbonInitialConditions$StateAttributeTypeID[carbonInitialConditions$StockTypeID == crossSF("Other")])]
  volumeToCarbon$c_fineroots <- volumeToCarbon[, as.character(carbonInitialConditions$StateAttributeTypeID[carbonInitialConditions$StockTypeID == crossSF("Fine root")])]
  volumeToCarbon$c_coarseroots <- volumeToCarbon[, as.character(carbonInitialConditions$StateAttributeTypeID[carbonInitialConditions$StockTypeID == crossSF("Coarse root")])]
  
  volumeToCarbon$c_m1 <- c(volumeToCarbon$c_m[2:nrow(volumeToCarbon)], NA)
  volumeToCarbon$c_foliage1 <- c(volumeToCarbon$c_foliage[2:nrow(volumeToCarbon)], NA)
  volumeToCarbon$c_other1 <- c(volumeToCarbon$c_other[2:nrow(volumeToCarbon)], NA)
  volumeToCarbon$c_fineroots1 <- c(volumeToCarbon$c_fineroots[2:nrow(volumeToCarbon)], NA)
  volumeToCarbon$c_coarseroots1 <- c(volumeToCarbon$c_coarseroots[2:nrow(volumeToCarbon)], NA)
  
  # Growth happens after biomass transfer
  if(ForestType == "Softwood") volumeToCarbon$g_m <- volumeToCarbon$c_m1 - (1 - biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeID == crossSF("Merchantable") & biomassTurnoverTable$ToStockTypeID == crossSF("Softwood Stem Snag")]) * volumeToCarbon$c_m
  if(ForestType == "Hardwood") volumeToCarbon$g_m <- volumeToCarbon$c_m1 - (1 - biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeID == crossSF("Merchantable") & biomassTurnoverTable$ToStockTypeID == crossSF("Hardwood Stem Snag")]) * volumeToCarbon$c_m
  volumeToCarbon$g_foliage <- volumeToCarbon$c_foliage1 - (1 - biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeID == crossSF("Foliage") & biomassTurnoverTable$ToStockTypeID == crossSF("Aboveground Very Fast DOM")]) * volumeToCarbon$c_foliage
  if(ForestType == "Softwood") volumeToCarbon$g_other <- volumeToCarbon$c_other1 - (1 - biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeID == crossSF("Other") & biomassTurnoverTable$ToStockTypeID == crossSF("Aboveground Fast DOM")] - biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeID == crossSF("Other") & biomassTurnoverTable$ToStockTypeID == crossSF("Softwood Branch Snag")]) * volumeToCarbon$c_other
  if(ForestType == "Hardwood") volumeToCarbon$g_other <- volumeToCarbon$c_other1 - (1 - biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeID == crossSF("Other") & biomassTurnoverTable$ToStockTypeID == crossSF("Aboveground Fast DOM")] - biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeID == crossSF("Other") & biomassTurnoverTable$ToStockTypeID == crossSF("Hardwood Branch Snag")]) * volumeToCarbon$c_other
  volumeToCarbon$g_fineroots <- volumeToCarbon$c_fineroots1 - (1 - biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeID == crossSF("Fine root") & biomassTurnoverTable$ToStockTypeID == crossSF("Aboveground Very Fast DOM")]  - biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeID == crossSF("Fine root") & biomassTurnoverTable$ToStockTypeID == crossSF("Belowground Very Fast DOM")]) * volumeToCarbon$c_fineroots
  volumeToCarbon$g_coarseroots <- volumeToCarbon$c_coarseroots1 - (1 - biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeID == crossSF("Coarse root") & biomassTurnoverTable$ToStockTypeID == crossSF("Aboveground Fast DOM")]  - biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeID == crossSF("Coarse root") & biomassTurnoverTable$ToStockTypeID == crossSF("Belowground Fast DOM")]) * volumeToCarbon$c_coarseroots
  volumeToCarbon$g_all <- volumeToCarbon$g_m + volumeToCarbon$g_foliage + volumeToCarbon$g_other + volumeToCarbon$g_fineroots + volumeToCarbon$g_coarseroots
  volumeToCarbon <- volumeToCarbon[1:(nrow(volumeToCarbon)-1),]
  
  #Replace NaN values with 0's
  is.nan.data.frame <- function(x)
    do.call(cbind, lapply(x, is.nan))
  
  volumeToCarbon[is.nan.data.frame(volumeToCarbon)] <- 0
  volumeToCarbon <- cbind(volumeToCarbon[,c("StratumID", "SecondaryStratumID", "TertiaryStratumID", "StateClassID")], do.call(data.frame,lapply(volumeToCarbon[,!(names(volumeToCarbon) %in% c("StratumID", "SecondaryStratumID", "TertiaryStratumID", "StateClassID"))], function(x) replace(x, is.infinite(x),0))))
  
  #######################
  # STSim-SF datasheets #
  #######################
  # State Attribute Values for net growth based on mass-balance equations
  stateAttributesNetGrowth = datasheet(myScenario, name="stsim_StateAttributeValue", empty = T, optional = T)
  stateAttributesNetGrowth[1:nrow(volumeToCarbon), "StratumID"] <- volumeToCarbon$StratumID[1:nrow(volumeToCarbon)]
  stateAttributesNetGrowth[1:nrow(volumeToCarbon), "SecondaryStratumID"] <- volumeToCarbon$SecondaryStratumID[1:nrow(volumeToCarbon)]
  stateAttributesNetGrowth[1:nrow(volumeToCarbon), "StateClassID"] <- volumeToCarbon$StateClassID[1:nrow(volumeToCarbon)]
  stateAttributesNetGrowth[1:nrow(volumeToCarbon), "StateAttributeTypeID"] <- rep(as.character(flowPathways$StateAttributeTypeID[(flowPathways$FromStockTypeID==crossSF("Atmosphere") & flowPathways$ToStockTypeID==crossSF("Merchantable"))]), nrow(volumeToCarbon))
  stateAttributesNetGrowth[1:nrow(volumeToCarbon), "AgeMin"] <- volumeToCarbon$AgeMin[1:nrow(volumeToCarbon)]
  stateAttributesNetGrowth[1:nrow(volumeToCarbon), "AgeMax"] <- volumeToCarbon$AgeMin[1:nrow(volumeToCarbon)]
  stateAttributesNetGrowth[1:nrow(volumeToCarbon), "Value"] <- volumeToCarbon$g_all[1:nrow(volumeToCarbon)]
  stateAttributesNetGrowth[nrow(volumeToCarbon), "AgeMax"] <- NA
  
  stateAttributesNetGrowthMaster = rbind(stateAttributesNetGrowth, stateAttributesNetGrowthMaster)

  # SF Flow Pathways
  # Flow Multilpiers for biomass net growth based on volume-to-carbon proportions 
  flowMultiplierNetGrowth <- datasheet(myScenario, name="stsimsf_FlowMultiplier", empty = T, optional = T)
  flowMultiplierNetGrowth[1:(nrow(volumeToCarbon)*numBiomassStocks), "StratumID"] <- crosswalkStratumState$StratumID[i]
  flowMultiplierNetGrowth[1:(nrow(volumeToCarbon)*numBiomassStocks), "SecondaryStratumID"] <- crosswalkStratumState$SecondaryStratumID[i]
  flowMultiplierNetGrowth[1:(nrow(volumeToCarbon)*numBiomassStocks),"StateClassID"] <- crosswalkStratumState$StateClassID[i]
  flowMultiplierNetGrowth[1:(nrow(volumeToCarbon)*numBiomassStocks), "AgeMin"] <- rep(volumeToCarbon$AgeMin[1:nrow(volumeToCarbon)], numBiomassStocks)
  flowMultiplierNetGrowth[1:(nrow(volumeToCarbon)*numBiomassStocks), "AgeMax"] <- rep(volumeToCarbon$AgeMin[1:nrow(volumeToCarbon)], numBiomassStocks)
  flowMultiplierNetGrowth[1:(nrow(volumeToCarbon)*numBiomassStocks), "FlowGroupID"] <- c(rep(paste0(as.character(flowPathways$FlowTypeID[(flowPathways$FromStockTypeID==crossSF("Atmosphere") & flowPathways$ToStockTypeID==crossSF("Merchantable"))])," [Type]"), nrow(volumeToCarbon)),
                                                                                     rep(paste0(as.character(flowPathways$FlowTypeID[(flowPathways$FromStockTypeID==crossSF("Atmosphere") & flowPathways$ToStockTypeID==crossSF("Other"))]), " [Type]"),nrow(volumeToCarbon)),
                                                                                     rep(paste0(as.character(flowPathways$FlowTypeID[(flowPathways$FromStockTypeID==crossSF("Atmosphere") & flowPathways$ToStockTypeID==crossSF("Foliage"))]), " [Type]"), nrow(volumeToCarbon)),
                                                                                     rep(paste0(as.character(flowPathways$FlowTypeID[(flowPathways$FromStockTypeID==crossSF("Atmosphere") & flowPathways$ToStockTypeID==crossSF("Fine root"))]), " [Type]"), nrow(volumeToCarbon)),
                                                                                     rep(paste0(as.character(flowPathways$FlowTypeID[(flowPathways$FromStockTypeID==crossSF("Atmosphere") & flowPathways$ToStockTypeID==crossSF("Coarse root"))]), " [Type]"),  nrow(volumeToCarbon)))
  flowMultiplierNetGrowth[1:(nrow(volumeToCarbon)*numBiomassStocks), "Value"] <- c(volumeToCarbon$g_m[1:nrow(volumeToCarbon)] / volumeToCarbon$g_all[1:nrow(volumeToCarbon)],
                                                                                     volumeToCarbon$g_other[1:nrow(volumeToCarbon)] / volumeToCarbon$g_all[1:nrow(volumeToCarbon)],
                                                                                     volumeToCarbon$g_foliage[1:nrow(volumeToCarbon)] / volumeToCarbon$g_all[1:nrow(volumeToCarbon)],
                                                                                     volumeToCarbon$g_fineroots[1:nrow(volumeToCarbon)] / volumeToCarbon$g_all[1:nrow(volumeToCarbon)],
                                                                                     volumeToCarbon$g_coarseroots[1:nrow(volumeToCarbon)] / volumeToCarbon$g_all[1:nrow(volumeToCarbon)])
  #flowMultiplierNetGrowth[flowMultiplierNetGrowth$AgeMin == volumeToCarbon$AgeMin[nrow(volumeToCarbon)], "AgeMax"] <- NA
  #flowMultiplierNetGrowth[is.nan.data.frame(flowMultiplierNetGrowth)] <- 0
  
  #Flow Pathways for biomass turnover rates and DOM transfer and decay rates
  flowPathwayTable <- rbind(biomassTurnoverTable, DOMTable[,names(biomassTurnoverTable)])
  flowMultiplierTurnoverTransferDecayEmission <- datasheet(myScenario, name="stsimsf_FlowMultiplier", empty=T, optional=T)
  flowMultiplierTurnoverTransferDecayEmission[1:nrow(flowPathwayTable), "StratumID"] <- crosswalkStratumState$StratumID[i]
  flowMultiplierTurnoverTransferDecayEmission[1:nrow(flowPathwayTable), "SecondaryStratumID"] <- crosswalkStratumState$SecondaryStratumID[i]
  flowMultiplierTurnoverTransferDecayEmission[1:nrow(flowPathwayTable), "StateClassID"] <- crosswalkStratumState$StateClassID[i]
  flowMultiplierTurnoverTransferDecayEmission[1:nrow(flowPathwayTable), "FlowGroupID"] = paste0(flowPathwayTable$FlowTypeID," [Type]")
  flowMultiplierTurnoverTransferDecayEmission[1:nrow(flowPathwayTable), "Value"] = flowPathwayTable$Multiplier
  
  # Combine all flow multipliers
  flowMultiplierAll <- rbind(flowMultiplierNetGrowth, flowMultiplierTurnoverTransferDecayEmission)
  flowMultiplierMaster <- rbind(flowMultiplierAll,flowMultiplierMaster)
  
  
}

saveDatasheet(myScenario, stateAttributesNetGrowthMaster, name = "stsim_StateAttributeValue", append = TRUE)
saveDatasheet(myScenario, flowMultiplierMaster, name="stsimsf_FlowMultiplier", append=T)


