# ApexRMS - Jan 2022
# script creates "cbm-example" template library for stsimcbmcfs3 package

# setwd("C:/GitHub/stsimcbmcfs3")

library(rsyncrosim)
library(readxl)
library(tidyverse)

options(stringsAsFactors=FALSE)

# Settings ----
mySession <- session("C:/SyncroSim Versions/2-3-9")
# version(mySession)
libraryName <- "model/cbm-cfs3-example"
myProjectName <- "Definitions"
initialInputsDirectory <- "C:/GitHub/stsimcbmcfs3/data/" # "../data/"

# Build base library ----

# Ensure ST-Sim is installed
addPackage("stsim", session = mySession)
addPackage("stsimsf", session = mySession)
addPackage("stsimcbmcfs3", session = mySession)

# Create library
dir.create("model/", showWarnings = FALSE)
myLibrary <- ssimLibrary(libraryName, 
                         addon = c("stsimsf", "stsimcbmcfs3"),
                         session = mySession, overwrite = TRUE)
name(myLibrary) <- "CBM-CFS3 Example"
myProject <- project(myLibrary, project=myProjectName)

# load library
# myLibrary <- ssimLibrary(libraryName, 
#                          addon = c("stsimsf", "stsimcbmcfs3"),
#                          session = mySession)
# myProject <- project(myLibrary, project=myProjectName)


#######################
## Predefined Inputs ##
#######################
# set library/project defaults and generate scenarios that remain constant

# Library definitions ----

# Define the default path/connection to the CBM database
CBMDatabasePath <- "C:/Program Files (x86)/Operational-Scale CBM-CFS3/Admin/DBs/ArchiveIndex_Beta_Install.mdb"

sheetname <- "stsimcbmcfs3_Database"
mySheet <- datasheet(myLibrary, name=sheetname)
mySheet[1, "Path"] <- CBMDatabasePath
saveDatasheet(myLibrary, mySheet, sheetname)

# Project definitions ----

## General ----

# ### Stages
# sheetName <-"core_StageName" #"core_Transformer" 
# mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
# mySheetFull <- read_xlsx(path = paste0(initialInputsDirectory, "Stages.xlsx"), sheet = "Stages") %>%
#   data.frame()
# saveDatasheet(myProject, mySheetFull, sheetName)

# ## Strata ----
# 
# # Add [unspecified] strata as a default option
# sheetName <- "stsim_Stratum"
# mySheet <- datasheet(myProject, name = sheetName)
# mySheetFull <- add_row(mySheet, Name = "[Unspecified]")
# saveDatasheet(myProject, mySheetFull, sheetName, append = F)

## Transitions ----

### Transition type
distTypes <- read_xlsx(path = paste0(initialInputsDirectory, "Disturbance Type.xlsx"), sheet = "Disturbance Type") %>%
  data.frame() %>%
  select(DistTypeID, Name) %>% 
  filter(Name %in% c("Wildfire", "97% clear-cut")) %>%
#   mutate(transitions = case_when(str_detect(DistTypeName, "fire") ~ "Fire",
#                                  str_detect(DistTypeName, "cut") ~ "Clearcut"))
# transitionTypes <- distTypes[!is.na(distTypes$transitions),]
# transitionTypes <- transitionTypes[-which(transitionTypes$DistTypeName %in% c("Partial cutting", "Salvage logging after fire")),]
# tranistionTypes <- transitionTypes %>%
  mutate(
    color = case_when(str_detect(Name, "fire") ~ "255,255,0,0",
                                   str_detect(Name, "cut") ~ "255,255,255,0"))

transitionTypes <- distTypes %>%
  mutate(transitions = paste0(Name, " [Type]"))

sheetName <- "stsim_TransitionType"
mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
mySheetFull <- data.frame(Name = distTypes$Name, 
                          ID = distTypes$DistTypeID,
                          Color = distTypes$color ) 
saveDatasheet(myProject, mySheetFull, sheetName)

# ### Transition group
# sheetName <- "stsim_TransitionGroup"
# mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
# mySheetFull <- data.frame(Name = unique(transitionTypes$transitions))
# saveDatasheet(myProject, mySheetFull, sheetName)

## Ages ----

### Age groups
sheetName <- "stsim_AgeGroup"
mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
mySheetFull <- read_xlsx(path = paste0(initialInputsDirectory, "Age Groups.xlsx"), sheet = "Age Groups") %>%
  data.frame()
saveDatasheet(myProject, mySheetFull, sheetName)

## Advanced ----

### Attribute group
sheetName <- "stsim_AttributeGroup"
mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
mySheetFull <- read_xlsx(path = paste0(initialInputsDirectory, "Attribute Group.xlsx"), sheet = "Attribute Group") %>%
  data.frame()
saveDatasheet(myProject, mySheetFull, sheetName)

### State attribute type
sheetName <- "stsim_StateAttributeType"
mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
mySheetFull <- read_xlsx(path = paste0(initialInputsDirectory, "State Attribute Type.xlsx"), sheet = "State Attribute Type") %>%
  data.frame()
names(mySheetFull) <- names(mySheet)
saveDatasheet(myProject, mySheetFull, sheetName)

### Stock/Flow definitions ----

#### Stock type
sheetName <- "stsimsf_StockType"
mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
mySheetFull <- read_xlsx(path = paste0(initialInputsDirectory, "Stock Type.xlsx"), sheet = "Stock Type") %>%
  data.frame()
saveDatasheet(myProject, mySheetFull, sheetName)

#### Stock group
sheetName <- "stsimsf_StockGroup"
mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
mySheetFull <- read_xlsx(path = paste0(initialInputsDirectory, "Stock Group.xlsx"), sheet = "Stock Group") %>%
  data.frame()
saveDatasheet(myProject, mySheetFull, sheetName)

#### Flow type
sheetName <- "stsimsf_FlowType"
mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
mySheetFull <- read_xlsx(path = paste0(initialInputsDirectory, "Flow Type.xlsx"), sheet = "Flow Type") %>%
  data.frame()
saveDatasheet(myProject, mySheetFull, sheetName)

#### Flow group
sheetName <- "stsimsf_FlowGroup"
mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
mySheetFull <- read_xlsx(path = paste0(initialInputsDirectory, "Flow Group.xlsx"), sheet = "Flow Group") %>%
  data.frame()
saveDatasheet(myProject, mySheetFull, sheetName)

#### terminology
sheetName <- "stsimsf_Terminology"
mySheetFull <- data.frame(StockUnits = "metric tons C")
saveDatasheet(myProject, mySheetFull, sheetName)

### CBM Definitions ----

#### Ecological boundary
sheetName <- "stsimcbmcfs3_EcoBoundary"
mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
mySheetFull <- read_xlsx(path = paste0(initialInputsDirectory, "Ecological Boundary.xlsx"), sheet = "Ecological Boundary") %>%
  data.frame()
saveDatasheet(myProject, mySheetFull, sheetName)

#### Administrative boundary
sheetName <- "stsimcbmcfs3_AdminBoundary"
mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
mySheetFull <- read_xlsx(path = paste0(initialInputsDirectory, "Administrative Boundary.xlsx"), sheet = "Administrative Boundary") %>%
  data.frame()
saveDatasheet(myProject, mySheetFull, sheetName)

#### Species type
sheetName <- "stsimcbmcfs3_SpeciesType"
mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
mySheetFull <- read_xlsx(path = paste0(initialInputsDirectory, "Species Type.xlsx"), sheet = "Species Type") %>%
  data.frame()
saveDatasheet(myProject, mySheetFull, sheetName)

#### Disturbance type
sheetName <- "stsimcbmcfs3_DisturbanceType"
mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
mySheetFull <- read_xlsx(path = paste0(initialInputsDirectory, "Disturbance Type.xlsx"), sheet = "Disturbance Type") %>%
  data.frame() %>%
  select(Name)
saveDatasheet(myProject, mySheetFull, sheetName)

#### CBM-CFS3 stock
sheetName <- "stsimcbmcfs3_CBMCFS3Stock"
mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
mySheetFull <- read_xlsx(path = paste0(initialInputsDirectory, "CBM-CFS3 Crosswalk Carbon Stock.xlsx"), sheet = "CBM-CFS3 Crosswalk Carbon Stock") %>%
  data.frame() %>%
  select(CBM.CFS3.Stock)%>%
  rename(Name = CBM.CFS3.Stock) 
saveDatasheet(myProject, mySheetFull, sheetName)

## Terminology ----
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

# Predefined Input Scenario data ----

# Find the Parent Project ID to create a folder within this Project
pid <-  project(myLibrary)$projectId[1]

# Write the console command for "Run Scenario" folder
command <- paste0("\"", filepath(mySession), "/SyncroSim.Console.Exe\"",
                   " --create --folder --lib=", filepath(myLibrary),
                   " --name=1-Predefined-Inputs --tpid=", pid)

# Invoke a system command
sysOut <- system(command, intern=TRUE)

# datasheet(myProject, optional = T) # datasheet(myScenario, optional = T)

## Flow pathways
myScenarioName <- "Flow Pathways"
myScenario = scenario(myProject, scenario = myScenarioName)

sheetName <- "stsimsf_FlowPathwayDiagram"
mySheet <- datasheet(myScenario, name=sheetName, optional=T, empty = T)
mySheetFull <- read_xlsx(path = paste0(initialInputsDirectory, "Flow Pathway Diagram.xlsx"), sheet = "Flow Pathway Diagram") %>%
  data.frame()
names(mySheetFull) <- names(mySheet)
saveDatasheet(myScenario, mySheetFull, sheetName)

sheetName <- "stsimsf_FlowPathway"
mySheet <- datasheet(myScenario, name=sheetName, optional=T, empty = T)
mySheetFull <- read_xlsx(path = paste0(initialInputsDirectory, "Flow Pathways.xlsx"), sheet = "Flow Pathways") %>%
  data.frame()
names(mySheetFull) <- names(mySheet)
saveDatasheet(myScenario, mySheetFull, sheetName)

## Flow order
myScenarioName <- "Flow Order"
myScenario = scenario(myProject, scenario = myScenarioName)

sheetName <- "stsimsf_FlowOrder"
mySheet <- datasheet(myScenario, name=sheetName, optional=T, empty = T)
mySheetFull <- read_xlsx(path = paste0(initialInputsDirectory, "Flow Order.xlsx"), sheet = "Flow Order") %>%
  data.frame()
names(mySheetFull) <- names(mySheet)
saveDatasheet(myScenario, mySheetFull, sheetName)

sheetName <- "stsimsf_FlowOrderOptions"
mySheet <- datasheet(myScenario, name=sheetName, optional=T, empty = T)
mySheetFull <- addRow(mySheet, data.frame(ApplyBeforeTransitions = T, ApplyEquallyRankedSimultaneously = T))
saveDatasheet(myScenario, mySheetFull, sheetName)

## Flow group membership
myScenarioName <- "Flow Group Membership"
myScenario = scenario(myProject, scenario = myScenarioName)

sheetName <- "stsimsf_FlowTypeGroupMembership"
mySheet <- datasheet(myScenario, name=sheetName, optional=T, empty = T)
mySheetFull <- read_xlsx(path = paste0(initialInputsDirectory, "Flow Type-Group Membership.xlsx"), sheet = "Flow Type-Group Membership") %>%
  data.frame()
names(mySheetFull) <- names(mySheet)
saveDatasheet(myScenario, mySheetFull, sheetName)

## Stock group membership
myScenarioName <- "Stock Group Membership"
myScenario = scenario(myProject, scenario = myScenarioName)

sheetName <- "stsimsf_StockTypeGroupMembership"
mySheet <- datasheet(myScenario, name=sheetName, optional=T, empty = T)
mySheetFull <- read_xlsx(path = paste0(initialInputsDirectory, "Stock Type-Group Membership.xlsx"), sheet = "Stock Type-Group Membership") %>%
  data.frame()
names(mySheetFull) <- names(mySheet)
saveDatasheet(myScenario, mySheetFull, sheetName)

## Initial stocks
myScenarioName <- "Initial Stocks"
myScenario = scenario(myProject, scenario = myScenarioName)

sheetName <- "stsimsf_InitialStockNonSpatial"
mySheet <- datasheet(myScenario, name=sheetName, optional=T, empty = T)
mySheetFull <- read_xlsx(path = paste0(initialInputsDirectory, "Initial Stocks - Non Spatial.xlsx"), sheet = "Initial Stock - Non Spatial") %>%
  data.frame()
names(mySheetFull) <- names(mySheet)
saveDatasheet(myScenario, mySheetFull, sheetName)

## Stock flow output options
myScenarioName <- "Stock Flow Output Options"
myScenario = scenario(myProject, scenario = myScenarioName)

sheetName <- "stsimsf_OutputOptions"
mySheet <- datasheet(myScenario, name=sheetName, optional=T, empty = T)
mySheetFull <- read_xlsx(path = paste0(initialInputsDirectory, "SF Output Options - Spatial.xlsx"), sheet = "SF Output Options") %>%
  data.frame()
# names(mySheetFull) <- names(mySheet)
saveDatasheet(myScenario, mySheetFull, sheetName)

## Crosswalk to ST-Sim
### Disturbance
myScenarioName <- "CBM Crosswalk - Disturbance"
myScenario = scenario(myProject, scenario = myScenarioName)

sheetName <- "stsimcbmcfs3_CrosswalkDisturbance" # datasheet containing the LUCAS-CBM Stock crosswalk table
mySheet <- datasheet(myScenario, name=sheetName, optional=T, empty = T)
mySheetFull <- transitionTypes %>% 
  select(Name,transitions)
names(mySheetFull) <- names(mySheet)
saveDatasheet(myScenario, mySheetFull, sheetName)

### Carbon stock
myScenarioName <- "CBM Crosswalk - Stocks"
myScenario = scenario(myProject, scenario = myScenarioName)

sheetName <- "stsimcbmcfs3_CrosswalkStock" # datasheet containing the LUCAS-CBM Stock crosswalk table
mySheet <- datasheet(myScenario, name=sheetName, optional=T, empty = T)
mySheetFull <- read_xlsx(path = paste0(initialInputsDirectory, "CBM-CFS3 Crosswalk Carbon Stock.xlsx"), sheet = "CBM-CFS3 Crosswalk Carbon Stock") %>%
  data.frame()
names(mySheetFull) <- names(mySheet)
saveDatasheet(myScenario, mySheetFull, sheetName)

### Output options non-spatial 
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

### Output Options Spatial 
myScenario <- scenario(myProject, scenario = "Output Options [Spatial]")
sheetName <- "stsim_OutputOptionsSpatial"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheetFull <- read_xlsx(path = paste0(initialInputsDirectory, "Output Options Spatial.xlsx"), sheet = "Output Options Spatial") %>%
  data.frame()
names(mySheetFull) <- names(mySheet)
saveDatasheet(myScenario, mySheetFull, sheetName)


#########################
## USER DEFINED INPUTS ##
#########################
# generate scenarios for all required user inputs

# Write the console command for "Run Scenario" folder
command <- paste0("\"", filepath(mySession), "/SyncroSim.Console.Exe\"",
                  " --create --folder --lib=", filepath(myLibrary),
                  " --name=2-User-Defined-Inputs --tpid=", pid)

# Invoke a system command
sysOut <- system(command, intern=TRUE)

## Library Definitions ----

# Define the path/connection to the CBM database if other then default
# CBMDatabasePath <- "C:/Users/Administrator/Desktop/A249/CBM-CFS3 database/ArchiveIndex_Beta_Install.mdb"
# 
# sheetname <- "stsimcbmcfs3_Database"
# mySheet <- datasheet(myLibrary, name=sheetname)
# mySheet[1, "Path"] <- CBMDatabasePath
# saveDatasheet(myLibrary, mySheet, sheetname)

## Project Definitions ---- [Needed for Load CMBCFS3 Transformer]

# load in SUST crosswalk data
## user could also define these values in Syncrosim UI 

CBMDir <- "C:/GitHub/stsimcbmcfs3/data/user-example-inputs/cbm-cfs3-simulation-results/"
crosswalkSUSTPath <- "data/user-example-inputs/Crosswalk - Spatial Unit and Species Type.csv"
crosswalkSUSTFull <- read.csv(crosswalkSUSTPath, check.names = F)
crosswalkSUSTFull$CBMSimulationDataFilePath <- paste0(CBMDir, crosswalkSUSTFull$CBMSimulationDataFileName)
crosswalkSUSTFull$CBMSimulationDataFileName <- NULL


### Primary stratum [OPTIONAL]
## This adds all unique primary stratum names from the crosswalk table to the definitions of the new model
sheetName <- "stsim_Stratum"
mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
mySheetFull = data.frame()
for(i in seq(1:nrow(crosswalkSUSTFull))) {
  crosswalkSUST <- crosswalkSUSTFull %>% slice(i)
  myValue = crosswalkSUST$`ST-Sim Stratum`
  mySheet = addRow(mySheet, data.frame(Name = myValue,
                                       ID = 1))
  mySheetFull = bind_rows(mySheetFull, mySheet) %>% unique()
}
saveDatasheet(myProject, mySheetFull, sheetName)


### Secondary stratum [OPTIONAL]
sheetName <- "stsim_SecondaryStratum"
mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
mySheetFull = data.frame()
for(i in seq(1:nrow(crosswalkSUSTFull))) {
  crosswalkSUST <- crosswalkSUSTFull %>% slice(i)
  myValue = crosswalkSUST$`ST-Sim Secondary Stratum`
  mySheet = addRow(mySheet, data.frame(Name = myValue,
                                       ID = 1))
  mySheetFull = bind_rows(mySheetFull, mySheet) %>% unique()
}
saveDatasheet(myProject, mySheetFull, sheetName)


### State Label X 
sheetName <- "stsim_StateLabelX"
mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
mySheetFull = data.frame()
for(i in seq(1:nrow(crosswalkSUSTFull))) {
  crosswalkSUST <- crosswalkSUSTFull %>% slice(i)
  myValue = crosswalkSUST$`ST-Sim State Class`
  mySheet = addRow(mySheet, data.frame(Name = myValue))
  mySheetFull = bind_rows(mySheetFull, mySheet) %>% unique()
}
saveDatasheet(myProject, mySheetFull, sheetName)


### State Label Y 
sheetName <- "stsim_StateLabelY"
mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
mySheetFull = data.frame()
for(i in seq(1:nrow(crosswalkSUSTFull))) {
  crosswalkSUST <- crosswalkSUSTFull %>% slice(i)
  myValue = crosswalkSUST$`ST-Sim State Class`
  mySheet = addRow(mySheet, data.frame(Name = myValue))
  mySheetFull = bind_rows(mySheetFull, mySheet) %>% unique()
}
saveDatasheet(myProject, mySheetFull, sheetName)


### State Class Type [REQUIRED]
sheetName <- "stsim_StateClass"
mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
mySheetFull <- read_xlsx(path = paste0(initialInputsDirectory, "user-example-inputs/State Class.xlsx"), sheet = "State Class") %>%
  data.frame()
names(mySheetFull) <- names(mySheet)
saveDatasheet(myProject, mySheetFull, sheetName)


## Project Definitions ---- [Needed for Flow-pathways/spin-up Transformers]

maxAge <- 300 
initialStandAge <- 0
standArea <- 1

# Age type 
sheetName <- "stsim_AgeType"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet[1,"Frequency"] <- 1
mySheet[1,"MaximumAge"] <- maxAge
saveDatasheet(myProject, mySheet, name=sheetName)

## Age group
sheetName <- "stsim_AgeGroup"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet[1:(maxAge/20),"MaximumAge"] <- c(seq(from=20, to=(maxAge-1), by=20), maxAge-1)
saveDatasheet(myProject, mySheet, name=sheetName)


### Scenario Definitions ----

#### Load CBM-CFS3 Output Dependencies ---- 

### Run Control - set as default

maxTimestep <- maxAge
maxIteration <- 1
minTimestep <- 0
minIteration <- 1

myScenario <- scenario(myProject, scenario = "Run Control" )# <- paste0("Run Control [Non-spatial; ", maxTimestep, " years; ", maxIteration, " MC]"))
sheetName <- "stsim_RunControl"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet[1,"MinimumIteration"] <- minIteration
mySheet[1,"MaximumIteration"] <- maxIteration
mySheet[1,"MinimumTimestep"] <- minTimestep
mySheet[1,"MaximumTimestep"] <- maxTimestep
mySheet[1,"IsSpatial"] <- FALSE
saveDatasheet(myScenario, mySheet, sheetName)

### Species Type Crosswalk 
myScenarioName <- "CBM Crosswalk - Spatial Unit and Species Type"
myScenario = scenario(myProject, scenario = myScenarioName)
sheetName <- "stsimcbmcfs3_CrosswalkSpecies"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheetFull <- crosswalkSUSTFull
names(mySheetFull) <- names(mySheet)
saveDatasheet(myScenario, mySheetFull, sheetName)

#### Generate Flow Multiplier Dependencies ---- 

### Initial Conditions 
myScenarioName <- "Initial Conditions" # paste0("Initial Conditions [Non-spatial; Single cell; ", standArea, " ha; Age ", initialStandAge, "]")
myScenario = scenario(myProject, scenario = myScenarioName)
sheetName <- "stsim_InitialConditionsNonSpatial"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet[1, "TotalAmount"] <- standArea * nrow(crosswalkSUSTFull)
mySheet[1, "NumCells"] <- nrow(crosswalkSUSTFull)
mySheet[1, "CalcFromDist"] <- T
saveDatasheet(myScenario, mySheet, sheetName)

sheetName <- "stsim_InitialConditionsNonSpatialDistribution"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheetFull = data.frame()

for(i in seq(1:nrow(crosswalkSUSTFull))) {
  crosswalkSUST <- crosswalkSUSTFull %>% slice(i)
  mySheet = addRow(mySheet, data.frame(StratumID = crosswalkSUST$`ST-Sim Stratum`,
                                       SecondaryStratumID = crosswalkSUST$`ST-Sim Secondary Stratum`,
                                       StateClassID = crosswalkSUST$`ST-Sim State Class`,
                                       AgeMin = initialStandAge,
                                       RelativeAmount = standArea))
  mySheetFull = bind_rows(mySheetFull, mySheet) %>% unique()
}
saveDatasheet(myScenario, mySheetFull, sheetName, append = F)

#### CBM-CFS3 Spin-up Dependencies ---- 

### Transition Pathways Diagram 
# Note, only deterministic transitions are defined here.
myScenario <- scenario(myProject, scenario <- "Transition Pathways")
sheetName <- "stsim_DeterministicTransition"
mySheet <- datasheet(myScenario, name=sheetName, optional=T, empty = T)
mySheetFull = data.frame()
for(i in seq(1:nrow(crosswalkSUSTFull))) {
  crosswalkSUST <- crosswalkSUSTFull %>% slice(i)
  myValue = crosswalkSUST$`ST-Sim State Class`
  mySheet = addRow(mySheet, data.frame(StateClassIDSource = myValue,
                                       Location = paste0("A", c(i))))
} 
mySheetFull = bind_rows(mySheetFull, mySheet)
saveDatasheet(myScenario, mySheetFull, sheetName)

### CBM spin-up 
myScenarioName = "Spin-up"
myScenario = scenario(myProject, scenario = myScenarioName)
sheetName = "stsimcbmcfs3_Spinup"
sheetData = datasheet(myScenario, sheetName, empty = T)
sheetData = read.csv("data/user-example-inputs/Spin-up.csv")
# sheetData$StratumID <- NA
# sheetData$SecondaryStratumID <- NA
saveDatasheet(myScenario, sheetData, sheetName)

#### Single Cell Dependencies ----

# ### Initial Conditions - Single Cell
# myScenarioName <- "Initial Conditions - Single Cell" 
# myScenario = scenario(myProject, scenario = myScenarioName)
# sheetName <- "stsim_InitialConditionsNonSpatial"
# mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
# mySheet[1, "TotalAmount"] <- nrow(crosswalkSUSTFull)
# mySheet[1, "NumCells"] <- 1
# mySheet[1, "CalcFromDist"] <- T
# saveDatasheet(myScenario, mySheet, sheetName)
# 
# sheetName <- "stsim_InitialConditionsNonSpatialDistribution"
# mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
# mySheetFull = data.frame()
# for(i in seq(1:nrow(crosswalkSUSTFull))) {
# crosswalkSUST <- crosswalkSUSTFull %>% slice(i)
# mySheet = addRow(mySheet, data.frame(StratumID = crosswalkSUST$`ST-Sim Stratum`,
#                                        SecondaryStratumID = crosswalkSUST$`ST-Sim Secondary Stratum`,
#                                        StateClassID = crosswalkSUST$`ST-Sim State Class`,
#                                        AgeMin = initialStandAge,
#                                        RelativeAmount = standArea))
# } 
# mySheetFull = bind_rows(mySheetFull, mySheet)
# saveDatasheet(myScenario, mySheet, sheetName, append = F)


#### Landscape Dependencies ---- 

myScenario <- scenario(myProject, scenario = "Run Control - Landscape" )
sheetName <- "stsim_RunControl"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet[1,"MinimumIteration"] <- 1
mySheet[1,"MaximumIteration"] <- 3
mySheet[1,"MinimumTimestep"] <- 2021
mySheet[1,"MaximumTimestep"] <- 2030
mySheet[1,"IsSpatial"] <- T
saveDatasheet(myScenario, mySheet, sheetName)

### Initial Conditions - Landscape
myScenarioName <- "Initial Conditions - Landscape" 
myScenario = scenario(myProject, scenario = myScenarioName)
sheetName <- "stsim_InitialConditionsSpatial"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheetFull <- addRow(mySheet, data.frame(StratumFileName = paste0(initialInputsDirectory, "user-example-inputs/spatial/eco-boundary.tif"),
                           SecondaryStratumFileName = paste0(initialInputsDirectory, "user-example-inputs/spatial/admin-boundary.tif"),
                           StateClassFileName = paste0(initialInputsDirectory, "user-example-inputs/spatial/state-class.tif"), 
                           AgeFileName = paste0(initialInputsDirectory, "user-example-inputs/spatial/age.tif")))
saveDatasheet(myScenario, mySheetFull, sheetName)

sheetName <- "stsim_InitialTSTSpatial"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheetFull <- addRow(mySheet, data.frame(TransitionGroupID = "Wildfire [Type]",
                                          TSTFileName = paste0(initialInputsDirectory, "user-example-inputs/spatial/age.tif")))
saveDatasheet(myScenario, mySheetFull, sheetName)


### Transition Pathways Diagram 
# Note, only deterministic transitions are defined here.
myScenario <- scenario(myProject, scenario <- "Transition Pathways - Landscape")
sheetName <- "stsim_DeterministicTransition"
mySheet <- datasheet(myScenario, name=sheetName, optional=T, empty = T)
mySheetFull = data.frame()
for(i in seq(1:nrow(crosswalkSUSTFull))) {
  crosswalkSUST <- crosswalkSUSTFull %>% slice(i)
  myValue = crosswalkSUST$`ST-Sim State Class`
  mySheet = addRow(mySheet, data.frame(StateClassIDSource = myValue,
                                       Location = paste0("A", c(i))))
} 
mySheetFull = bind_rows(mySheetFull, mySheet)
saveDatasheet(myScenario, mySheetFull, sheetName)

sheetName <- "stsim_Transition"
mySheet <- datasheet(myScenario, name=sheetName, optional=T, empty = T)
mySheetFull = data.frame()
for(i in seq(1:nrow(crosswalkSUSTFull))) {
  crosswalkSUST <- crosswalkSUSTFull %>% slice(i)
  myValue = crosswalkSUST$`ST-Sim State Class`
  mySheet = addRow(mySheet, data.frame(StateClassIDSource = myValue,
                                       TransitionTypeID = "Wildfire",
                                       Probability = 0.01))
} 
mySheetFull = bind_rows(mySheetFull, mySheet)
saveDatasheet(myScenario, mySheetFull, sheetName)

### Transition Multiplier - Landscape
myScenario <- scenario(myProject, scenario <- "Transition Multiplier - Landscape")
sheetName <- "stsim_TransitionMultiplierValue"
mySheet <- datasheet(myScenario, name=sheetName, optional=T, empty = T)
mySheetFull = data.frame()
mySheet = addRow(mySheet, data.frame(TransitionGroupID = "Wildfire [Type]",
                                     Amount = 1, 
                                     DistributionType = "Beta", 
                                     DistributionFrequencyID = "Iteration and Timestep",
                                     DistributionSD = 2,
                                     DistributionMin = 0, 
                                     DistributionMax = 10))
mySheetFull = bind_rows(mySheetFull, mySheet)
saveDatasheet(myScenario, mySheetFull, sheetName)

### Fire Size - Landscape
myScenario <- scenario(myProject, scenario <- "Fire Size - Landscape")
sheetName <- "stsim_TransitionSizeDistribution"
mySheet <- datasheet(myScenario, name=sheetName, optional=T, empty = T)
mySheetFull = data.frame()
mySheet = addRow(mySheet, data.frame(TransitionGroupID = c("Wildfire [Type]","Wildfire [Type]"),
                                     MaximumArea = c(90, 80),
                                     RelativeAmount = c(0.9, 0.1)))
mySheetFull = bind_rows(mySheetFull, mySheet)
saveDatasheet(myScenario, mySheetFull, sheetName)


###############
## RUN SETUP ##
###############
# generate run scenarios for each transformer

# Write the console command for "Run Scenario" folder
command <- paste0("\"", filepath(mySession), "/SyncroSim.Console.Exe\"",
                  " --create --folder --lib=", filepath(myLibrary),
                  " --name=3-Run-Setup --tpid=", pid)

# Invoke a system command
sysOut <- system(command, intern=TRUE)

### Load CBM Output
myScenarioName <- "Load CBM Output"
myScenario = scenario(myProject, scenario = myScenarioName)
dependency(myScenario, 
           c("Run Control",
             "CBM Crosswalk - Stocks",
             "CBM Crosswalk - Spatial Unit and Species Type"))

# set "Load CBM-CFS3 Output" transformer to run for this scenario
sheetName <- "core_Pipeline"
# mySheet <- datasheet(myScenario, name=sheetName, optional=T, empty = T)
mySheetFull <- data.frame(StageNameID = "1 - Load CBM Output",
                          RunOrder = 1)
saveDatasheet(myScenario, mySheetFull, sheetName)


### Generate Flow Multipliers
myScenarioName <- "Generate Flow Multipliers"
myScenario = scenario(myProject, scenario = myScenarioName)
dependency(myScenario, 
           c("Flow Pathways",
             "Stock Group Membership",
             "Flow Group Membership",
             "Initial Stocks",
             "Stock Flow Output Options",
             # "Initial Conditions",
             # "Pathway Diagram",
             "CBM Crosswalk - Disturbance",
             "Load CBM Output"))

# set "Flow Pathways" transformer to run for this scenario
sheetName <- "core_Pipeline"
mySheet <- datasheet(myScenario, name=sheetName, optional=T)
mySheet <- addRow(mySheet, data.frame(
  StageNameID = "2 - Generate Flow Pathways",
  MaximumJobs = NA,
  RunOrder = 1))
saveDatasheet(myScenario, mySheet, sheetName)


### Run Spin-up
myScenarioName <- "Run Spin-up"
myScenario = scenario(myProject, scenario = myScenarioName)
dependency(myScenario, 
           c("Flow Order",
             "Spin-up",
             "Transition Pathways",
             "Generate Flow Multipliers"))

# set "spin-up" transformer to run for this scenario
sheetName <- "core_Pipeline"
mySheetFull <- data.frame(StageNameID = "3 - Run Spin-up",
                          RunOrder = 1)
saveDatasheet(myScenario, mySheetFull, sheetName)

### IN UI: set ignore dependencies for "OutputStock"

##################
## Run Forecast ##
##################

# Write the console command for "Run Scenario" folder
command <- paste0("\"", filepath(mySession), "/SyncroSim.Console.Exe\"",
                  " --create --folder --lib=", filepath(myLibrary),
                  " --name=4-Run-Forecast --tpid=", pid)

# Invoke a system command
sysOut <- system(command, intern=TRUE)

### Single cell
myScenarioName <- "Single Cell - No Disturbance"
myScenario = scenario(myProject, scenario = myScenarioName)
dependency(myScenario, 
           c("Flow Order",
             "Initial Conditions",
             "Output Options [Non-spatial]",
             "Transition Pathways",
             "Generate Flow Multipliers"))

### IN UI: set ignore dependencies for "OutputStock" and "Pipeline" 


# # set "stsim" transformer to run for this scenario
# sheetName <- "core_Pipeline"
# mySheetFull <- data.frame(StageNameID = "ST-Sim",
#                           RunOrder = 1)
# saveDatasheet(myScenario, mySheetFull, sheetName)

### Landscape
myScenarioName <- "Landscape"
myScenario = scenario(myProject, scenario = myScenarioName)
dependency(myScenario, 
           c("Run Control - Landscape",
             "Initial Conditions - Landscape",
             "Output Options [Spatial]",
             "Transition Pathways - Landscape",
             "Transition Multiplier - Landscape", 
             "Fire Size - Landscape",
             "Run Spin-up"))

# # set "stsim" transformer to run for this scenario
# sheetName <- "core_Pipeline"
# mySheetFull <- data.frame(StageNameID = "ST-Sim",
#                           RunOrder = 1)
# saveDatasheet(myScenario, mySheetFull, sheetName)    

### IN UI: set ignore dependencies for "OutputStock" and "Pipeline" 

##########################
## Rename Folders in UI ##
##########################

# 1 - Predefined Inputs ## make this folder read-only
# 2 - User Defined Inputs
## 1 - Run Setup Inputs 
## 2 - Run Forecast Inputs
# 3 - Run Setup
# 4 - Run Forecast

##############################
## create charts/maps in UI ##
##############################

# Charts ---
# 01 - Single Cell - Biomass
# 02 - Single Cell - Aboveground DOM
# 03 - Single Cell - Belowground DOM
# 04 - Spin-up - Biomass
# 05 - Spin-up - Aboveground DOM
# 06 - Spin-up - Belowground DOM
# 07 - Landscape - Area Burned 
# 08 - Landscape - Biomass
# 09 - Landscape - Aboveground DOM
# 10 - Landscape - Belowground DOM

# --> Stocks > Total > Disaggregate by: Stock Type/Group > Include data for: State Class (select one), Stock Type/Group (select biomass or A/B DOM types)

# Options > 
# Options [checked boxes]:
## Yaxis min to zero
## Fixed Y axis intervals
## Show legend
## Show scenario name
## Show scenario ID
## Show tooltips
## show tiles
# Format:
# 1 decimal place (y)

# Maps ---
# 1 - Biomass
# 2 - Flows 
# --> net growth and emissions
# 3 - Age

# in options check the "no data as zero" box for all charts except wildfire  

# # Make a console call to create/move scenarios to folders -----------
# 
# # Find the Parent Project ID to create a folder within this Project
# pid <-  project(myLibrary)$projectId[1]
# 
# # Write the console command for "Run Scenario" folder
# command <- paste0("\"", filepath(ssimSession), "/SyncroSim.Console.Exe\"",
#                   " --create --folder --lib=", filepath(myLibrary),
#                   " --name=Run-Scenarios --tpid=", pid)
# 
# # Invoke a system command
# sysOut <- system(command, intern=TRUE)
# 
# # Grab the folder ID as a variable
# folderId <- strsplit(sysOut, ": ")[[1]][2]
# 
# # Make a console call to move Scenarios to the folder
# # Pick the first Scenario in the Library to move
# sid <- scenario(myLibrary)$scenarioId[10]
# 
# # Write the console command; tfid is the folder ID flag
# command <- paste0("\"", filepath(ssimSession), "/SyncroSim.Console.Exe\"",
#                   " --move --scenario --lib=", filepath(myLibrary), " --sid=",
#                   sid, " --tfid=", folderId, " --tpid=", pid)
# sysOut <- system(command, intern=TRUE) # Invoke a system command

