#######################################################
## A223 - LandCarbon - Spinnup pre-processing script ##
## Prepared bt VL sept 14 2020 - ????                ##
#######################################################

library(rsyncrosim)
library(tidyverse)

myLibrary <- ssimLibrary()
myProject <- project(myLibrary, 1)
myScenario <- scenario()

source("A223_helpers.R")

# (1) Extract spinup datasheet from library -------------------------------
# spinup <- read.csv("../../../stsimcbmcfs3/data/stsimcbmcfs3_Spinup.csv")  %>% 
#   mutate_if(is.factor, as.character)
spinup <- datasheet(myScenario, "stsimcbmcfs3_Spinup") %>% 
  mutate_if(is.factor, as.character)

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

final_df <- data.frame()

for (rownb in 1:nrow_unique){
  
  # Collect row-wise params
  the_row <- slice(spinup, rownb)
  stratum <- the_row$StratumID
  secondary_stratum <- the_row$SecondaryStratumID
  
  state_class <- the_row$StateClassID
  dist_hist <- the_row$HistoricalDisturbanceTGID
  dist_last <- the_row$MostRecentDisturbanceTGID
  
  cell_archetype <- paste0("Last Disturbance: ", 
                           strip_type(dist_last))
  
  spinup_duration <- the_row$SpinupDuration
  interval_dist <- the_row$ReturnInterval
  
  # Determine duration
  max_duration <- spinup %>% 
    select(ReturnInterval, SpinupDuration) %>% 
    rowSums() %>% 
    max()
  
  # Create sequences
  ts_seq <- seq(from = 0, to = spinup_duration, by = interval_dist)
  ts_seq_add <- ts_seq + 1
  
  # Bind them
  temp_df <- bind_rows(data.frame(Timestep = ts_seq, Amount = 0), 
                       data.frame(Timestep = ts_seq_add, Amount = 1)) %>% 
    mutate(TransitionGroupID = dist_hist) %>% 
    mutate_if(is.factor, as.character)
  
  # Check if the 2 disturbances are the same
  if(dist_hist != dist_last){
    end_rows <- which(temp_df$Timestep %in% 
                        c(spinup_duration, spinup_duration + 1))
    temp_df[end_rows, ]$TransitionGroupID <- dist_last
    temp_df <- add_row(temp_df, Timestep = 0, Amount = 0, 
                       TransitionGroupID = dist_last)
  }
  
  # Add row params
  temp_df <-  temp_df %>% 
    mutate(StratumID = stratum, 
           SecondaryStratumID = secondary_stratum, 
           TertiaryStratumID = cell_archetype,
           StateClassID = state_class)
  
  final_df <- bind_rows(final_df, temp_df)
}

final_df <- final_df %>% arrange(Timestep)

unique_tertiary <- data.frame(Name = unique(final_df$TertiaryStratumID))

saveDatasheet(myProject, data = unique_tertiary, 
              name = "stsim_TertiaryStratum")
saveDatasheet(myScenario, data = final_df, 
              name = "stsim_TransitionMultiplierValue")


# (6) Set Output Options --------------------------------------------------

output_options <- datasheet(myScenario, "stsim_OutputOptions")

output_options$SummaryOutputSC <- TRUE
output_options$SummaryOutputSCTimesteps <- 1
output_options$SummaryOutputTR <- TRUE
output_options$SummaryOutputTRTimesteps <- 1

saveDatasheet(myScenario, data = output_options, 
              name = "stsim_OutputOptions")
