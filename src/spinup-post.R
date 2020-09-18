########################################################
## A223 - LandCarbon - Spinnup post-processing script ##
## Prepared bt VL sept 2020                           ##
########################################################

library(rsyncrosim)
library(tidyverse)

myLibrary <- ssimLibrary()
myProject <- project(myLibrary, 1)
myScenario <- scenario()

# Source helper functions
pkg_dir <- (Sys.getenv("ssim_package_directory"))
source(file.path(pkg_dir, "stsimcbmcfs3_helpers.R"))

# (1) Extract source and destination datasheets ---------------------------

spinup <- datasheet(myScenario, "stsimcbmcfs3_Spinup") %>% 
  mutate_if(is.factor, as.character)

initial_stocks <- datasheet(myScenario, "stsimsf_InitialStockNonSpatial") %>% 
  mutate_if(is.factor, as.character)

output_stocks <- datasheet(myScenario, "stsimsf_OutputStock") %>%
  mutate_if(is.factor, as.character) %>% 
  mutate(StockTypeID = strip_type(StockGroupID)) %>% 
  left_join(initial_stocks, by = "StockTypeID")

# remove all entries with no match in the initial_stocks table
# To see what is removed:
# output_stocks %>% filter(is.na(StateAttributeTypeID)) %>% pull(StockTypeID) %>% table

output_stocks_noNA <- output_stocks %>% drop_na()

state_attributes <- datasheet(myScenario, "stsim_StateAttributeValue", optional = T) %>% 
  mutate_if(is.factor, as.character) %>% 
  filter(!str_detect(StateAttributeTypeID, "Carbon Initial Conditions"))

# (2) Wrangle outputs into state attribute table --------------------------

spinup_unique <- unique(spinup)
nrow_unique <- nrow(spinup_unique)

state_attributes_final <- state_attributes

for (rownb in 1:nrow_unique){
  
  # Determine spinup duration for this cell
  the_row <- slice(spinup, rownb)
  
  spinup_duration <- the_row$SpinupDuration
  last_cycle_duration <- the_row$MaxAgeForLastCycle
  TSTGroup <- the_row$MostRecentDisturbanceTGID
  stratum <- the_row$StratumID
  secondary_stratum <- the_row$SecondaryStratumID
  state_class <- the_row$StateClassID
  
  output_stocks_filtered <- output_stocks_noNA %>% 
    filter(Timestep >= spinup_duration, 
           Timestep <= (spinup_duration + last_cycle_duration),
           StratumID == stratum,
           SecondaryStratumID == secondary_stratum, 
           StateClassID == state_class) %>% 
    mutate(TSTMin = Timestep - spinup_duration, 
           TSTMax = TSTMin, 
           TSTGroupID = TSTGroup) %>%
    rename(Value = Amount) %>% 
    select(-c(StockGroupID, StockTypeID, TertiaryStratumID))
  
  state_attributes_final <- state_attributes_final %>% 
    bind_rows(output_stocks_filtered)
  
}

# Save 
saveDatasheet(myScenario, data = unique(state_attributes_final), 
              name = "stsim_StateAttributeValue")
