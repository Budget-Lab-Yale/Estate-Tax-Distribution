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
library(optimx)



#----------------
# Configuration
#----------------

# Random number generator seed
set.seed(123)

# Global runtime options: data dependency filepaths
input_data_roots = list(
  scf               = 'C:/Users/jar335/Documents/Interfaces/raw_data/SCF/v1/',
  tax_data          = 'C:/Users/jar335/Documents/Interfaces/model_data/Tax-Data/v1/2025060316/baseline',
  macro_projections = 'C:/Users/jar335/Documents/Interfaces/model_data/Macro-Projections/v3/2025040115/baseline'
)

# Global runtime options: output filepath
output_root = 'C:/Users/jar335/Documents/Interfaces/model_data/Estate-Tax-Distribution/v1'

# Whether to re-estimate random forest model
estimate_qrf = F

# Create output folders
time_stamp = file.path(format(Sys.time(), '%Y%m%d%H'))
dir.create(file.path(output_root, time_stamp), recursive = T, showWarnings = F)


#-----------------------------------------
# Simulate inheritance and associated tax
#-----------------------------------------

# source('./src/tcja_ext.R')
source('./src/house_obbba.R')
