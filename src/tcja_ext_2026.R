#------------------------------------------------------------------------------
# tcja_ext_2026.R
#
# Imputes inheritances and estate tax liability for 2026 current law and TCJA
# extension scenarios
#------------------------------------------------------------------------------


#--------------------
# Process macro data
#--------------------

# Read macro data
macro_projections_raw = c('projections.csv', 'historical.csv') %>% 
  map(
    .f = ~ input_data_roots$macro_projections %>% 
      file.path(.x) %>% 
      read_csv(show_col_types = F) 
  ) %>% 
  bind_rows() %>% 
  arrange(year)

# Create economic dataframe
macro_projections = list()
macro_projections$economic = macro_projections_raw %>%  
  select(year, gdp, cpiu, ccpiu_irs)

# Create demographic dataframe
macro_projections$demographic = macro_projections_raw %>% 
  select(year, contains('married')) %>% 
  pivot_longer(
    cols      = -year, 
    names_sep = '_', 
    names_to  = c('married', 'age'), 
    values_to = 'population'
  ) %>%
  filter(age != 'NA') %>% 
  mutate(
    married = as.integer(married == 'married'), 
    age     = as.integer(age)
  ) 

# Get historical per-capita net worth from FA 
net_worth = read_csv('./resources/financial_accounts/b101.csv', show_col_types = F) %>%
  
  # Use Q3 given SCF timeframe 
  mutate(year = as.integer(str_sub(date, 1, 4))) %>% 
  filter(str_sub(date, 6, 7) == 'Q3', year >= 2013, year <= 2023) %>% 
  select(year, net_worth = FL152090005.Q) %>% 
  mutate(net_worth = as.numeric(net_worth)) %>% 
  
  # Extend to 2026
  bind_rows(
    tibble(year = 2024:2026)
  ) %>%
  
  # Project net worth based on GDP
  left_join(
    macro_projections$economic %>% 
      select(year, gdp), 
    by = 'year'
  ) %>% 
  mutate(
    net_worth = if_else(
      year >= 2024, 
      net_worth[year == 2023] * gdp / gdp[year == 2023], 
      net_worth
    )
  ) %>%
  
  # Adjust for population
  left_join(
    macro_projections$demographic %>% 
      group_by(year) %>% 
      summarise(population = sum(population, na.rm = T)), 
    by = 'year'
  ) %>% 
  mutate(net_worth_pc = net_worth * 1e6 / population) %>%
  select(year, net_worth_pc)


#------------------
# Process SCF data 
#------------------

# Load summary extract and join inheritances from detailed file
scf = c('2013', '2016', '2019', '2022') %>% 
  map(
    .f = function(yr) {
      
      # Extract the last two digits of year for detailed file
      yy = str_sub(as.character(yr), -2)
      
      # Read the summary data
      summary_data = input_data_roots$scf %>% 
        file.path(yr, 'historical', paste0('SCFP', yr, '.csv')) %>% 
        read_csv(show_col_types = F) %>%
        mutate(
          survey_year = as.integer(yr), 
          married     = as.integer(MARRIED == 1), 
          has_kids    = as.integer(KIDS > 0)
        ) %>%
        select(survey_year, yy1 = YY1, y1 = Y1, weight = WGT, age = AGE, married, has_kids, income = INCOME)
      
      # Read the detailed data and extract inheritance information directly using col_select
      detailed_data = input_data_roots$scf %>% 
        file.path(yr, 'historical', paste0('p', yy, 'i6.dta')) %>% 
        read_dta(col_select = c(
          yy1, y1,
          x5803, x5804, x5805,
          x5808, x5809, x5810,
          x5813, x5814, x5815
        )) %>%
        
        # Rename for first, second, and third inheritances
        rename(
          type.1 = x5803, value.1 = x5804, year.1 = x5805,
          type.2 = x5808, value.2 = x5809, year.2 = x5810,
          type.3 = x5813, value.3 = x5814, year.3 = x5815
        ) %>%
        as_tibble()
      
      # Join detailed data on summary data
      summary_data %>%
        left_join(detailed_data, by = c('yy1', 'y1')) %>% 
        return()
    }
  ) %>% 
  bind_rows() %>% 
  
  # Duplicate records for lower variance
  expand_grid(dup_id = 1:5) %>% 
  mutate(weight = weight / 5)


# Adjust inheritance data to account for the fact that year-received is rounded
current_year_inheritance = scf %>% 
  select(survey_year, yy1, y1, dup_id, weight, ends_with('.1'), ends_with('.2'), ends_with('.3')) %>% 
  pivot_longer(
    cols      = c(ends_with('.1'), ends_with('.2'), ends_with('.3')), 
    names_sep = '[.]',
    names_to  = c('name', 'inheritance_number')
  ) %>% 
  pivot_wider() %>%
  
  # Timing probability weights are calibrated to estimates from Gale and Sabelhaus (2024)
  # https://www.brookings.edu/wp-content/uploads/2024/12/20241209_TPC_Galeetal_GreatWealthTransfer.pdf 
  mutate(
    p = case_when(
      survey_year == 2022 & year == 2022 ~ 1, 
      survey_year == 2022 & year == 2020 ~ 0.28, 
      survey_year == 2019 & year == 2019 ~ 0.82,
      survey_year == 2016 & year == 2015 ~ 0.36,
      survey_year == 2013 & year == 2013 ~ 1, 
      survey_year == 2013 & year == 2010 ~ 0.1,
      T ~ 0
    )
  ) %>% 
  
  # Remove non-inheritance gifts and reshape wide in inheritance
  filter(value > 0, type != 3, p > 0) %>% 
  select(-type, -year) %>% 
  pivot_wider(
    names_from  = inheritance_number, 
    names_sep   = '.', 
    values_from = c(value, p)
  ) %>% 
  mutate(
    
    # Simulate current-year inheritance
    across(
      .cols = contains('.'), 
      .fns  = ~ replace_na(., 0)
    ), 
    value.1 = value.1 * (runif(nrow(.)) < p.1),
    value.2 = value.2 * (runif(nrow(.)) < p.2),
    value.3 = value.3 * (runif(nrow(.)) < p.3),
    
    # Choose largest inheritance
    larger_1_2  = if_else(value.1 >= value.2, value.1, value.2), 
    larger_2_3  = if_else(value.2 >= value.3, value.2, value.3),
    inheritance = if_else(larger_1_2 >= larger_2_3, larger_1_2, larger_2_3), 

  ) %>% 
  select(survey_year, yy1, y1, dup_id, inheritance) 


# Construct training data
inheritance_train = scf %>% 
  select(survey_year, yy1, y1, dup_id, weight, age, married, has_kids, income) %>% 
  left_join(current_year_inheritance, by = c('survey_year', 'yy1', 'y1', 'dup_id')) %>% 
  
  # Add 0s for those with no inheritance
  mutate(
    inheritance     = replace_na(inheritance, 0), 
    has_inheritance = as.integer(inheritance > 0)
  ) %>%
  
  # Age weights forward to 2026
  left_join(
    macro_projections$demographic %>% 
      group_by(married, age) %>% 
      mutate(demographic_factor = population[year == 2026] / population) %>% 
      ungroup() %>% 
      select(survey_year = year, age, married, demographic_factor), 
    by = c('survey_year', 'age', 'married')
  ) %>% 
  mutate(weight = weight * demographic_factor) %>% 
  select(-demographic_factor) %>% 
  
  # Age inheritance values forward to 2026 
  left_join(
    net_worth %>% 
      mutate(asset_factor = net_worth_pc[year == 2026] / net_worth_pc) %>% 
      select(survey_year = year, asset_factor), 
    by = 'survey_year'
  ) %>% 
  mutate(inheritance = inheritance * asset_factor) %>% 
  select(-asset_factor) %>%
  
  # Convert income to rank-order space 
  group_by(survey_year) %>%
  mutate(
    income_pctile = cut(
      x      = income,
      breaks = wtd.quantile(income[income > 0], weight[income > 0], c(seq(0, 0.99, 0.01), seq(0.991, 1, 0.002))) + runif(105), 
      labels = c(seq(0.01, 0.99, 0.01), seq(0.992, 1, 0.002))
    ) %>% as.character() %>% as.numeric(), 
    income_pctile = case_when(
      is.na(income_pctile) & income < 1000  ~ 0, 
      is.na(income_pctile) & income >= 1000 ~ 1,
      T ~ income_pctile 
    )
  ) %>% 
  ungroup() %>% 
  select(-income) %>% 

  # Cap age at 80
  mutate(age = pmin(80, age))
  

# Unweight the data
inheritance_train = inheritance_train %>% 
  slice_sample(n = 500000, replace = T, weight_by = weight) %>% 
  mutate(weight = mean(inheritance_train$weight) * nrow(inheritance_train) / 500000)


#-----------------
# Estimate models
#-----------------

# Estimate model for presence of inheritance
if (estimate_qrf) {
  has_inheritance_qrf = quantregForest(
    x        = inheritance_train[c('age', 'married', 'income_pctile')],
    y        = as.factor(inheritance_train$has_inheritance), 
    nthreads = parallel::detectCores(),
    mtry     = 3,
    nodesize = 50
  )
  saveRDS(has_inheritance_qrf, './resources/has_inheritance_qrf.RDS')
} else {
  has_inheritance_qrf = readRDS('./resources/has_inheritance_qrf.RDS')
}


# Estimate model of inheritance value conditional on presence
nonzero_inheritance_train = inheritance_train %>% 
  filter(inheritance > 0)

# Model mean of log inheritance
mu_model <- lm(
  log(inheritance) ~ 
    married + 
    poly(age, 2) + 
    poly(income_pctile, 2) + 
    poly(age, 2):poly(income_pctile, 2),
  data = nonzero_inheritance_train
)

# Model variance using residuals
nonzero_inheritance_train$resid_sq = residuals(mu_model)^2
sigma_model <- lm(
  log(resid_sq) ~ 
    married + 
    poly(age, 2) + 
    poly(income_pctile, 2) + 
    poly(age, 2):poly(income_pctile, 2),
  data = nonzero_inheritance_train
)


#------------------------
# Fit values on tax data
#------------------------

# Read tax unit data
tax_units = input_data_roots$tax_data %>% 
  file.path('tax_units_2026.csv') %>% 
  fread() %>% 
  tibble() %>% 
  
  # Create X variables
  mutate(
    has_kids = as.integer(
      (
        (!is.na(dep_age1) & dep_age1 <= 17) +
        (!is.na(dep_age2) & dep_age2 <= 17) + 
        (!is.na(dep_age3) & dep_age3 <= 17)
      ) > 0
    ),
    married = as.integer(filing_status == 2), 
    income = wages + txbl_int + div_ord + div_pref + txbl_ira_dist + txbl_pens_dist +
             sole_prop + kg_lt + kg_st + part_active + part_passive - part_active_loss - 
             part_passive_loss - part_179 + scorp_active + scorp_passive -
             scorp_active_loss - scorp_passive_loss - scorp_179 + rent - 
             rent_loss + estate - estate_loss + farm + ui + gross_ss + other_inc,
    income_pctile = cut(
      x      = income,
      breaks = wtd.quantile(income[income > 0], weight[income > 0], c(seq(0, 0.99, 0.01), seq(0.991, 1, 0.002))) + runif(105), 
      labels = c(seq(0.01, 0.99, 0.01), seq(0.992, 1, 0.002))
    ) %>% as.character() %>% as.numeric(),
    income_pctile = case_when(
      is.na(income_pctile) & income < 100  ~ 0, 
      is.na(income_pctile) & income >= 100 ~ 1,
      T ~ income_pctile 
    )
  ) %>% 
  select(id, weight, age = age1, married, has_kids, income_pctile)


# Fit probability of inheritance and target age-marital status counts in SCF
# (i.e adjust for scope differences across X variables in data sources)
inheritance_yhat = tax_units %>%
  mutate(
    p_inheritance = predict(
      object  = has_inheritance_qrf, 
      newdata = (.),
      what    = function(x) mean(x - 1)
    )
  ) %>% 
  mutate(age_group = floor(age / 5) * 5) %>%
  left_join(
    inheritance_train %>% 
      group_by(married, age_group = floor(age / 5) * 5) %>% 
      summarise(actual = sum((inheritance > 0) * weight / 4), .groups = 'drop'), 
    by = c('married', 'age_group')
  ) %>% 
  group_by(married, age_group) %>% 
  mutate(
    p_inheritance = p_inheritance * (actual / sum(p_inheritance * weight))
  ) %>% 
  ungroup()



fit_inheritances = function(df, seed) {
  set.seed(seed)
  
  # Fit dollar-amount values
  df = df %>% 
    mutate(
      
      # Fit lognormal distribution parameters
      mu_pred    = predict(mu_model, newdata = (.)),
      sigma_pred = sqrt(exp(predict(sigma_model, newdata = (.)))),
      
      # Generate random values from predicted distribution
      inheritance = rlnorm(n(), meanlog = mu_pred, sdlog = sigma_pred)
    )
  
  # Calculate percent difference from actual across the distribution
  benchmark_factors = inheritance_train %>%
    filter(inheritance > 0) %>%
    reframe(
      p = seq(0, 1, 0.001),
      actual = wtd.quantile(inheritance, probs = seq(0, 1, 0.001))
    ) %>%
    left_join(
      df %>%
        filter(inheritance > 0) %>%
        reframe(
          p = seq(0, 1, 0.001),
          pred = wtd.quantile(inheritance, weight * p_inheritance, probs = seq(0, 1, 0.001))
        ),
      by = 'p'
    ) %>%
    mutate(
      factor = actual / pred,
      p = round(p, 3)
    )
  
  # Apply factors
  df %>%
    arrange(inheritance) %>%
    mutate(p = round(cumsum(weight) / sum(weight), 3)) %>%
    left_join(benchmark_factors %>% select(p, factor), by = 'p') %>%
    mutate(inheritance = inheritance * factor) %>% 
    return()
}
    

# Fit values
fit_seed = 8
inheritance_yhat = inheritance_yhat %>% 
  fit_inheritances(fit_seed)


#---------------------------------------
# Calibrate to match estate tax targets
#---------------------------------------


calibrate_high_wealth_inheritances = function(df, heirs_per_estate, seed) {
  
  set.seed(seed)
  
  # Estate tax targets
  wealth_brackets = tibble(
    min_wealth        = c(5e6, 10e6, 20e6, 50e6),
    max_wealth        = c(10e6, 20e6, 50e6, Inf),
    estate_count      = c(3200, 1600, 1200, 500),
    avg_estate_wealth = c(7.5, 15, 30, 175)
  )
  
  # Convert to inheritance brackets
  inheritance_brackets = wealth_brackets %>%
    mutate(
      min_inheritance        = min_wealth / heirs_per_estate,
      max_inheritance        = if_else(is.finite(max_wealth), max_wealth / heirs_per_estate, Inf),
      target_count           = estate_count * heirs_per_estate,
      target_avg_inheritance = avg_estate_wealth * 1e6 / heirs_per_estate,
      target_total_wealth    = target_count * target_avg_inheritance
    )
  
  # Copy the input dataframe
  df_cal = df %>% 
    mutate(p_inheritance = p_inheritance * if_else(inheritance * heirs_per_estate > 1e6, 0.75, 1))
  
  # Process each bracket separately
  for (i in 1:nrow(inheritance_brackets)) {
    bracket = inheritance_brackets[i,]
    
    # Find records within this bracket
    idx = which(
      (df_cal$inheritance >= bracket$min_inheritance) &
        (df_cal$inheritance < bracket$max_inheritance | is.infinite(bracket$max_inheritance))
    )
    
    # Skip if no records in bracket
    if (length(idx) == 0) next
    
    # Calculate current expected count and wealth
    current_count = sum(df_cal$p_inheritance[idx] * df_cal$weight[idx])
    current_wealth = sum(df_cal$p_inheritance[idx] * df_cal$weight[idx] * df_cal$inheritance[idx])
    
    # Step 1: Calibrate counts
    if (current_count < bracket$target_count) {
      
      # Calculate simple adjustment factor
      count_adj_factor = bracket$target_count / current_count
      
      # Apply adjustment (capped at 1.0)
      df_cal$p_inheritance[idx] = pmin(1, df_cal$p_inheritance[idx] * count_adj_factor)
      
      # Recalculate after adjustment
      current_count = sum(df_cal$p_inheritance[idx] * df_cal$weight[idx])
      current_wealth = sum(df_cal$p_inheritance[idx] * df_cal$weight[idx] * df_cal$inheritance[idx])
    }
    
    # Step 2: Calibrate wealth amounts
    current_avg_inheritance = current_wealth / current_count
    if (!is.nan(current_avg_inheritance) && current_avg_inheritance < bracket$target_avg_inheritance) {
      
      # Calculate wealth scaling factor
      wealth_factor = bracket$target_avg_inheritance / current_avg_inheritance
      
      # Apply scaling to inheritance values
      df_cal$inheritance[idx] = df_cal$inheritance[idx] * wealth_factor
      
      # Update current wealth
      current_wealth = sum(df_cal$p_inheritance[idx] * df_cal$weight[idx] * df_cal$inheritance[idx])
    }
    
    # Step 3: If still below target count, promote additional records
    remaining_deficit = bracket$target_count - current_count
    
    if (remaining_deficit > 0) {
      # Find the highest-income individuals below this bracket
      below_idx = which(
        df_cal$inheritance < bracket$min_inheritance &
          df_cal$inheritance >= quantile(df_cal$inheritance, 0.95)
      )
      
      # Sort by inheritance amount (highest first)
      below_idx = below_idx[order(df_cal$inheritance[below_idx], decreasing = TRUE)]
      
      if (length(below_idx) > 0) {
        # Select top individuals to "promote" to this bracket
        max_to_promote = min(length(below_idx), 100)  # Limit how many we promote
        to_promote = below_idx[1:max_to_promote]
        
        # Generate values at or above target average for the bracket
        alpha = 1.5  # Pareto shape parameter
        df_cal$inheritance[to_promote] = bracket$target_avg_inheritance * 
          (1 - runif(length(to_promote)))^(-1/alpha)
        
        # Increase their probability based on remaining deficit
        available_increase = sum((1 - df_cal$p_inheritance[to_promote]) * df_cal$weight[to_promote])
        
        if (available_increase > 0) {
          # Scale factor for increase
          scale_factor = min(1, remaining_deficit / available_increase)
          
          # Apply increase
          df_cal$p_inheritance[to_promote] = df_cal$p_inheritance[to_promote] +
            scale_factor * (1 - df_cal$p_inheritance[to_promote])
        }
      }
    }
  }
  return(df_cal)
}



validate_calibration = function(df, heirs_per_estate) {
  
  # Same brackets as in calibration function
  wealth_brackets = tibble(
    min_wealth        = c(5e6, 10e6, 20e6, 50e6),
    max_wealth        = c(10e6, 20e6, 50e6, Inf),
    estate_count      = c(3200, 1600, 1200, 500),
    avg_estate_wealth = c(7.5, 15, 30, 175)
  )
  
  # Calculate tax totals
  df %>% 
    mutate(
      gross_estate  = inheritance * heirs_per_estate,
      current_law   = pmax(0, gross_estate - 7.2e6) * 0.4 / heirs_per_estate, 
      tcja          = pmax(0, gross_estate - 14e6)  * 0.4 / heirs_per_estate
    ) %>% 
    group_by(
      gross_estate_group = case_when(
        gross_estate < 5e6  ~ 1,
        gross_estate < 10e6 ~ 2,
        gross_estate < 20e6 ~ 3,
        gross_estate < 50e6 ~ 4,
        gross_estate < Inf  ~ 5,
      )
    ) %>% 
    summarise(
      n_inheritances            = sum(p_inheritance * weight), 
      inheritances              = sum(inheritance * p_inheritance * weight) / 1e9, 
      amount.current_law        = sum(current_law       * p_inheritance * weight) / 1e9, 
      amount.tcja               = sum(tcja              * p_inheritance * weight) / 1e9,
      count_heirs.current_law   = sum((current_law > 0) * p_inheritance * weight), 
      count_heirs.tcja          = sum((tcja > 0)        * p_inheritance * weight), 
      count_estates.current_law = sum((current_law > 0) * p_inheritance * weight) / heirs_per_estate, 
      count_estates.tcja        = sum((tcja > 0)        * p_inheritance * weight) / heirs_per_estate,
      .groups = 'drop'
    ) %>% 
    return()
}


# 2.8 avg number of heirs gets us close to CBO/IRS targets
final_inheritance_values = inheritance_yhat %>% 
  calibrate_high_wealth_inheritances(heirs_per_estate = 2.8, seed = fit_seed)


#----------------------
# Project data forward
#----------------------

# Create directories if they don't exist
dir_baseline = file.path(output_root, time_stamp, 'baseline')
dir_tcja     = file.path(output_root, time_stamp, 'tcja_ext')
dir.create(dir_baseline, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_tcja,     recursive = TRUE, showWarnings = FALSE)


# Create projection dataframe with years 2026-2055
projection_years = 2026:2055

# Extract inflation and nominal growth indexes
indexes = macro_projections$economic %>%
  filter(year %in% projection_years) %>% 
  left_join(
    macro_projections$demographic %>% 
      group_by(year) %>% 
      summarise(population = sum(population, na.rm = T)), 
    by = 'year'
  ) %>% 
  mutate(
    gpp_pc          = gdp / population, 
    inflation_index = ccpiu_irs / ccpiu_irs[year == 2026], 
    gdp_pc_index    = gpp_pc / gpp_pc[year == 2026]
  ) %>% 
  select(year, ends_with('_index'))

# Project estate tax exemption thresholds with chained CPI
exemptions = indexes %>%
  mutate(
    
    # Base exemption amounts for 2026
    baseline_exemption = 7.2e6,
    tcja_ext_exemption = 14e6,
    
    # Index exemptions by chained CPI
    baseline_exemption = baseline_exemption * inflation_index,
    tcja_ext_exemption = tcja_ext_exemption * inflation_index
  ) %>% 
  select(year, baseline_exemption, tcja_ext_exemption)


# Function to calculate tax liability and write output files for a given year
calculate_and_write_for_year = function(year_val) {
  
  # Get growth factors for this year
  current_gdp_factor         = indexes$gdp_pc_index[indexes$year == year_val]
  current_baseline_exemption = exemptions$baseline_exemption[exemptions$year == year_val]
  current_tcja_exemption     = exemptions$tcja_ext_exemption[exemptions$year == year_val]
  
  # Scale inheritance values by GDP growth
  projected_values = final_inheritance_values %>%
    mutate(
      
      # Grow inheritances with per-capita GDP
      inheritance = inheritance * current_gdp_factor,
      
      # Calculate gross estate value
      gross_estate = inheritance * 2.8,
      
      # Calculate tax liability under baseline (current law)
      baseline_tax = pmax(0, gross_estate - current_baseline_exemption) * 0.4 / 2.8,
      
      # Calculate tax liability under TCJA extension
      tcja_tax = pmax(0, gross_estate - current_tcja_exemption) * 0.4 / 2.8,
      
      # Calculate the change in tax liability
      estate_tax_change = baseline_tax - tcja_tax
    )
  
  # Create baseline output
  baseline_output = projected_values %>%
    select(id, p_inheritance, inheritance, estate_tax_liability = baseline_tax)
  
  # Create TCJA extension output  
  tcja_output = projected_values %>%
    select(id, p_inheritance, inheritance, estate_tax_liability = tcja_tax)
  
  # Write output files
  write_csv(
    baseline_output,
    file.path(dir_baseline, paste0('estate_tax_detail_', year_val, '.csv'))
  )
  write_csv(
    tcja_output,
    file.path(dir_tcja, paste0('estate_tax_detail_', year_val, '.csv'))
  )
}



