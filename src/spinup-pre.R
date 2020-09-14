#######################################################
## A223 - LandCarbon - Spinnup pre-processing script ##
## Prepared bt VL sept 14 2020 - ????                ##
#######################################################

library(rsyncrosim)
library(tidyverse)

myLibrary <- ssimLibrary()
myProject <- project(myLibrary, 1)
myScenario <- scenario()

# (1) Extract spinup datasheet from library -------------------------------

# spinup <- read.csv("../../../stsimcbmcfs3/data/stsimcbmcfs3_Spinup.csv")
spinup <- datasheet(myScenario, "stsimcbmcfs3_Spinup")

# Throw warnings if empty
if (nrow(spinup) == 0){
  stop("Spinup datasheet is empty, conditions could not be initiated")
}


# (2) Impute Spinup params from CBM ---------------------------------------

if (is.null(spinup$HistoricalDisturbanceTGID)){
  # TODO script for inputation, need to do the crosswalk with the CBM database
}


# (3) Populate Run Control ------------------------------------------------

# Determine start and end
the_start <-  0
the_end <- spinup %>% 
  select(SpinupDuration, MaxAgeForLastCycle) %>% 
  rowSums() %>%
  max()

run_control <- data.frame(MinimumIteration = 1,
                          MaximumIteration = 1, 
                          MinimumTimestep = the_start, 
                          MaximumTimestep = the_end, 
                          IsSpatial = FALSE)
saveDatasheet(myScenario, data = run_control, name = "stsim_RunControl")


# (4) Populate initial Conditions -----------------------------------------

# For each spinup unique row, determine the appropriate set of IC
spinup_unique <- unique(spinup)
nrow_unique <- nrow(spinup_unique)

IC_nonspatial <- data.frame(TotalAmount = nrow_unique,
                            NumCells = nrow_unique,
                            CalcFromDist = TRUE)

IC_nonspatial_dist <- spinup %>% 
  select(StratumID, SecondaryStratumID, TertiaryStratumID, StratumID, StateClassID) %>% 
  mutate(AgeMin = 0, AgeMax = 0, RelativeAmount = 1)

saveDatasheet(myScenario, data = IC_nonspatial,
              name = "stsim_InitialConditionsNonSpatial")
saveDatasheet(myScenario, data = IC_nonspatial_dist,
              name = "stsim_InitialConditionsNonSpatialDistribution")

# (5) Populate Spatial Multipliers ----------------------------------------


