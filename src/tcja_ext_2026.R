#------------------------------------------------------------------------------
# tcja_ext_2026.R
#
# Imputes inheritances and estate tax liability for 2026 current law and TCJA
# extension scenarios
#------------------------------------------------------------------------------


# Simulation targets 
jct_estimate        = -15               # JCT  https://www.cbo.gov/publication/60114
rev_tcja            = c(36, 42)         # Jan 2025 CBO revenue outlook
rev_current_law     = c(51, 57)         # Jan 2025 CBO revenue outlook 
returns_tcja        = c(3000, 5000)     # 2022 SOI filing year taxable stats + 1000
returns_current_law = c(9000, 12000)    # Assumption: ~2.5x the TCJA level


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
  slice_sample(n = 250000, replace = T, weight_by = weight) %>% 
  mutate(weight = mean(inheritance_train$weight) * nrow(inheritance_train) / 250000)


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
    nodesize = 25
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


# Fit probability of inheritance
inheritance_yhat = tax_units %>%
  mutate(
    p_inheritance = predict(
      object  = has_inheritance_qrf, 
      newdata = (.),
      what    = function(x) mean(x - 1)
    )
  )



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
    


do_simulation = function(df, seed, return_microdata = F) {
  
  # Fit inheritances
  df = fit_inheritances(df, seed)
  
  # Calculate taxes and get totals
  tax_totals = df %>%
    expand_grid(
      count_factor  = c(0.2, 0.25, 0.3),
      dollar_factor = c(1.5, 2, 2.5),
      top_factor    = c(3, 3.5, 4),
      avg_heirs     = seq(1.8, 2.8, 0.2)
    ) %>%
    mutate(
      p_inheritance = p_inheritance * count_factor,
      gross_estate  = inheritance * avg_heirs,
      gross_estate  = gross_estate * if_else(gross_estate > 10e6, top_factor, 1),
      current_law   = pmax(0, gross_estate * dollar_factor - 7.2e6) * 0.4 / avg_heirs, 
      tcja          = pmax(0, gross_estate * dollar_factor - 14e6)  * 0.4 / avg_heirs
    ) %>% 
    group_by(count_factor, dollar_factor, top_factor, avg_heirs) %>% 
    summarise(
      amount.current_law        = sum(current_law       * p_inheritance * weight) / 1e9, 
      amount.tcja               = sum(tcja              * p_inheritance * weight) / 1e9,
      count_heirs.current_law   = sum((current_law > 0) * p_inheritance * weight), 
      count_heirs.tcja          = sum((tcja > 0)        * p_inheritance * weight), 
      count_estates.current_law = sum((current_law > 0) * p_inheritance * weight) / mean(avg_heirs), 
      count_estates.tcja        = sum((tcja > 0)        * p_inheritance * weight) / mean(avg_heirs),
      .groups = 'drop'
    ) %>% 
    bind_rows() 
  
  # Calculate status report
  status = tax_totals %>% 
    filter(
      amount.current_law         > rev_current_law[1]     & amount.current_law         < rev_current_law[2]     & 
      amount.tcja                > rev_tcja[1]            & amount.tcja                < rev_tcja[2]            & 
      count_estates.current_law  > returns_current_law[1] & count_estates.current_law  < returns_current_law[2] & 
      count_estates.tcja         > returns_tcja[1]        & count_estates.tcja         < returns_tcja[2]
    ) %>% 
    select(count_factor, dollar_factor, top_factor, avg_heirs)
    
    return(
      list(
        success    = nrow(status) > 0,
        tax_totals = tax_totals, 
        status     = status, 
        microdata  = if (return_microdata) df else NA  
      )
    )
}


# Do simulations
sims = map(1:10, ~ do_simulation(inheritance_yhat, .x))

# See whether any were successful
sims %>% 
  map(~ .x$success) %>% 
  unlist() %>% 
  print()


# Use parameters from 8
imputed_values = do_simulation(inheritance_yhat, 8, T)$microdata %>% 
  mutate(
    p_inheritance       = p_inheritance * 0.2,
    gross_estate        = inheritance * 2.4,
    gross_estate        = gross_estate * if_else(gross_estate > 10e6, 4, 1),
    estate_tax.baseline = pmax(0, gross_estate * 2 - 7.2e6) * 0.4 / 2.4, 
    estate_tax.reform   = pmax(0, gross_estate * 2 - 14e6)  * 0.4 / 2.4, 
    estate_tax_change   = estate_tax.reform - estate_tax.baseline
  ) %>% 
  
  # Benchmark to JCT 
  mutate(
    estate_tax_change = estate_tax_change * (jct_estimate / (sum(estate_tax_change * weight * p_inheritance) / 1e9))
  )

# Write to output
output_path = file.path(output_root, time_stamp, 'tcja_ext')
dir.create(output_path, showWarnings = F)
imputed_values %>% 
  select(id, p_inheritance, inheritance, estate_tax_change) %>% 
  write_csv(file.path(output_path, 'estate_tax_detail_2026.csv'))
  

# Calculate summary stats
imputed_values %>% 
  summarise(
    n_tcja        = sum((estate_tax.reform > 0) * weight * p_inheritance),
    n_current_law = sum((estate_tax.baseline > 0) * weight * p_inheritance),
    n_cut         = sum((estate_tax.reform - estate_tax.baseline < 0) * weight * p_inheritance),
    share_cut     = weighted.mean((estate_tax.reform < estate_tax.baseline) * p_inheritance, weight),
    avg_chg       = weighted.mean((estate_tax.reform - estate_tax.baseline) * p_inheritance, weight) 
  )

