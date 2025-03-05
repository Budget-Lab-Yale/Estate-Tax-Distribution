#------------------------------------------------
# Scenario configuration file for current policy
#------------------------------------------------

scenario_params[['baseline']] = list(
  
  # Data dependencies
  input_data_roots = list(
    scf      = '/gpfs/gibbs/project/sarin/shared/raw_data/SCF/v1/', 
    tax_data = '/gpfs/gibbs/project/sarin/shared/model_data/Tax-Data/v1/2024111117/baseline'
  ),
  
  # Year for distributional calculations
  year = 2026,
  
  # Tax law parameters
  tax_law = list(
    exemption = 14e6,
    rate      = 0.4
  )
)

