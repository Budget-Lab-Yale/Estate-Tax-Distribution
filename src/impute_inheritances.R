#------------------------------------------------------------------------------
# impute_inheritances.R
#
# TODO
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
  bind_rows()


# Adjust inheritance data to account for the fact that year-received is rounded
current_year_inheritance = scf %>% 
  select(survey_year, yy1, y1, ends_with('.1'), ends_with('.2'), ends_with('.3')) %>% 
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
  select(survey_year, yy1, y1, inheritance) 


# Construct training data
inheritance_train = scf %>% 
  select(survey_year, yy1, y1, weight, age, married, has_kids, income) %>% 
  left_join(current_year_inheritance, by = c('survey_year', 'yy1', 'y1')) %>% 
  
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
      breaks = wtd.quantile(income, weight, c(seq(0, 0.99, 0.01), seq(0.991, 1, 0.001))) + runif(110), 
      labels = c(seq(0.01, 0.99, 0.01), seq(0.991, 1, 0.001))
    ) %>% as.character() %>% as.numeric() %>% replace_na(0)
  ) %>% 
  ungroup() %>% 
  select(-income) %>% 

  # Remove billionaire's kid who gives the algorithm a hard time
  filter(inheritance < 300e6)


#-----------------------------------
# Build quantile regression forests 
#-----------------------------------

# Estimate model for presence of inheritance
has_inheritance_qrf = quantregForest(
  x        = inheritance_train[c('age', 'married', 'has_kids', 'income_pctile')],
  y        = as.factor(inheritance_train$has_inheritance), 
  nthreads = parallel::detectCores(),
  weights  = inheritance_train$weight,
  mtry     = 4,
  nodesize = 3,
  ntree    = 1000
)


# Estimate model of inheritance value conditional on presence
nonzero_inheritance_train = inheritance_train %>% 
  filter(inheritance > 0)
inheritance_qrf = quantregForest(
  x        = nonzero_inheritance_train[c('age', 'married', 'has_kids', 'income_pctile')],
  y        = nonzero_inheritance_train$inheritance, 
  nthreads = parallel::detectCores(),
  weights  = nonzero_inheritance_train$weight,
  mtry     = 4,
  nodesize = 5,
  ntree    = 1000
)


#------------------------
# Fit values on tax data
#------------------------

# Read tax unit data
tax_units = input_data_roots$tax_data %>% 
  file.path('tax_units_2026.csv') %>% 
  fread() %>% 
  tibble()


# Fit values on tax data
inheritance_yhat = tax_units %>% 
  
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
      sole_prop + part_active + part_passive - part_active_loss - 
      part_passive_loss - part_179 + scorp_active + scorp_passive -
      scorp_active_loss - scorp_passive_loss - scorp_179 + rent - 
      rent_loss + estate - estate_loss + farm + ui + gross_ss + other_inc,
    income_pctile = cut(
      x      = income,
      breaks = wtd.quantile(income, weight, c(seq(0, 0.99, 0.01), seq(0.991, 1, 0.001))) + runif(110), 
      labels = c(seq(0.01, 0.99, 0.01), seq(0.991, 1, 0.001))
    ) %>% as.character() %>% as.numeric() %>% replace_na(0)
  ) %>% 
  select(id, weight, age = age1, married, has_kids, income_pctile) %>% 
  
  # Fit values
  mutate(
    p = predict(
      object  = has_inheritance_qrf, 
      newdata = (.),
      what    = function(x) mean(x - 1)
    ), 
    yhat = predict(
      object  = inheritance_qrf, 
      newdata = (.),
      what    = function(x) sample(x, 1)
    )
  ) %>% 
  
  # Benchmark to counts from training data
  mutate(
    age_group = case_when(
      age <= 25 ~ 1, 
      age <= 35 ~ 2, 
      age <= 45 ~ 3, 
      age <= 55 ~ 4, 
      age <= 65 ~ 5, 
      age <= 75 ~ 6, 
      T         ~ 7
    )
  ) %>% 
  left_join(
    inheritance_train %>% 
      group_by(
        married, 
        age_group = case_when(
          age <= 25 ~ 1, 
          age <= 35 ~ 2, 
          age <= 45 ~ 3, 
          age <= 55 ~ 4, 
          age <= 65 ~ 5, 
          age <= 75 ~ 6, 
          T         ~ 7
        )
      ) %>% 
      summarise(target_count = sum((inheritance > 0) * weight / 4), .groups = 'drop'), 
    by = c('married', 'age_group')
  ) %>% 
  
  # Simulate
  group_by(married, age_group) %>% 
  mutate(p = p * (target_count / sum(p * weight))) %>% 
  ungroup() %>% 
  mutate(inheritance = yhat * (runif(nrow(.)) < p)) 


inheritance_yhat %>% 
  summarise(
    source    = 'pred',
    n         = sum(weight) / 1e6,
    share     = weighted.mean(inheritance > 0, weight), 
    count     = sum((inheritance > 0) * weight),
    count_mil = sum((inheritance > 1e6) * weight),
    amount    = sum(inheritance * weight) / 1e9, 
    avg       = weighted.mean(inheritance, weight * (inheritance > 0)), 
    median    = wtd.quantile(inheritance, weight * (inheritance > 0), 0.5)
  ) %>% 
  bind_rows(
    inheritance_train %>% 
      summarise(
        source    = 'actual',
        n         = sum(weight / 4) / 1e6,
        share     = weighted.mean(inheritance > 0, weight), 
        count     = sum((inheritance > 0) * weight / 4),
        count_mil = sum((inheritance > 1e6) * weight / 4),
        amount    = sum(inheritance * weight / 4) / 1e9, 
        avg       = weighted.mean(inheritance, weight * (inheritance > 0)), 
        median    = wtd.quantile(inheritance, weight * (inheritance > 0), 0.5)
      )
  )


seq(1, 4, 0.2) %>% 
  map(
    ~ inheritance_yhat %>%
      expand_grid(
        dup = 1:4
      ) %>%
      mutate(
        weight       = weight / 4,
        gross_estate = inheritance * .x * 169 / 147,
        current_law  = pmax(0, gross_estate - 13.6e6 / 2) * 0.4 / .x, 
        tcja         = pmax(0, gross_estate - 13.6e6)     * 0.4 / .x
      ) %>%
      summarise(
        n_heirs     = .x,
        amount.current_law = sum(current_law * weight) / 1e9, 
        amount.tcja        = sum(tcja        * weight) / 1e9,
        count.current_law  = sum((current_law > 0) * weight), 
        count.tcja         = sum((tcja > 0)        * weight), 
        records.current_law  = sum((current_law > 0)), 
        records.tcja         = sum((tcja > 0))
      )
  ) %>% 
  bind_rows()


