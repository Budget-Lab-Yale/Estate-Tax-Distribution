#------------------------------------------------------------------------------
# main.R
# 
# Entry point into program
#------------------------------------------------------------------------------

library(tidyverse)
library(haven)
library(Hmisc)
library(quantregForest)
library(data.table)


#----------------
# Configuration
#----------------

# Global runtime options: IDs of scenario to run
scenario_ids = c('baseline', 'tcja')

# Global runtime options: data dependency filepaths
input_data_roots = list(
  scf               = '/gpfs/gibbs/project/sarin/shared/raw_data/SCF/v1/', 
  tax_data          = '/gpfs/gibbs/project/sarin/shared/model_data/Tax-Data/v1/2024111117/baseline', 
  macro_projections = '/gpfs/gibbs/project/sarin/shared/model_data/Macro-Projections/v3/2024021116/baseline'
)

# Global runtime options: output filepath
output_root = '/gpfs/gibbs/project/sarin/shared/model_data/Estate-Tax-Distribution/v1'

# Create output folders
time_stamp = file.path(format(Sys.time(), '%Y%m%d%H'))
dir.create(file.path(output_root, time_stamp), recursive = T, showWarnings = F)
for (scenario_id in scenario_ids) {
  dir.create(file.path(output_root, time_stamp, scenario_id), recursive = T, showWarnings = F)
}

#-----------------------------------------
# Simulate inheritance and associated tax
#-----------------------------------------



# TODO
# - config meaning -- maybe it's lik ebehavior modules where it's custonm code.
# - re-run model and test
