#------------------------------------------------------------------------------
# main.R
# 
# Entry point into program
#------------------------------------------------------------------------------

library(tidyverse)
library(haven)
library(Hmisc)


#----------------
# Configuration
#----------------

# Global runtime options: IDs of scenario to run
scenario_ids = c('baseline', 'tcja')

# Global runtime options: output path
output_root = '/gpfs/gibbs/project/sarin/shared/model_data/Estate-Tax-Distribution/v1'

# Parse scenario files and create output paths
source('./src/config.R')


#-----------------------------------------
# Simulate inheritance and associated tax
#-----------------------------------------


