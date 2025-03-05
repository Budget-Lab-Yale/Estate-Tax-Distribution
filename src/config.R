#------------------------------------------------------------------------------
# config.R
# 
# Parses configuration files for each scenario and creates output folders
#------------------------------------------------------------------------------

# Parse scenario config files
scenario_params = list()
for (scenario in scenario_ids) {
  source(file.path('./config/', paste0(scenario, '.R')))
}

# Create output folders
time_stamp = file.path(format(Sys.time(), '%Y%m%d%H'))
dir.create(file.path(output_root, time_stamp), recursive = T, showWarnings = F)
for (scenario_id in scenario_ids) {
  dir.create(file.path(output_root, time_stamp, scenario_id), recursive = T, showWarnings = F)
}
