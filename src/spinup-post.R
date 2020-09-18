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

output_stocks <- datasheet(myScenario, "stsimsf_OutputStock") %>% 
  mutate_if(is.factor, as.character) %>% 
  filter()

state_attributes <- datasheet(myScenario, "stsim_StateAttributeValue", optional = T) %>% 
  mutate_if(is.factor, as.character) %>% 
  filter(!str_detect(StateAttributeTypeID, "Carbon Initial Conditions"))

# (2) Wrangle outputs into state attribute table --------------------------

spinup_unique <- unique(spinup)
nrow_unique <- nrow(spinup_unique)

for (rownb in 1:nrow_unique){
  
  # Determine spinup duration for this cell
  the_row <- slice(spinup, rownb)
  spinup_duration <- the_row$SpinupDuration
  
  output_stocks_filtered <- output_stocks %>% 
    filter(Timestep >= spinup_duration )
  
}
