add_binary_indicators <- function(df){

  # Economic Indicators ---------------------------------------------------
  # 1. Income Poverty
  df <- df %>%
    mutate(income_poverty = case_when(
      POVERTY < 100 ~ 1, #lives below 100 percent of poverty line
      POVERTY == 0 ~ NA_real_, #NA code in census data
      POVERTY >= 100 ~ 0, #at or above poverty line
      TRUE ~ NA_real_
    ))

  # 2. Employment
  df <- df %>%
    # Set NAs for investment and retirement income
    mutate(INCINVST = if_else(INCINVST == 999999, NA_real_, INCINVST),
           INCRETIR = if_else(INCRETIR == 999999, NA_real_, INCRETIR))

  ## Set to be marked as deprived on employment indicator if no one in household works or
  ## if there's not retirement income, investment income, or an active farm
  df <- df %>%
    mutate(emp_num = if_else(EMPSTAT == 1, 1, 0)) %>% #each person codes as employed or not
    group_by(SERIAL, YEAR) %>%
    mutate(retired = if_else(any(INCRETIR > 20000), 1, 0), # people with over $20,000 retirement income are assumed to be retired
           investment = if_else(any(INCINVST > 100000), 1, 0), # people with over $100,000 investment income may be choosing not to work
           fam_emp = if_else(any(emp_num == 1)|any(retired ==1) | any(investment == 1 | FARM == 2, 0, 1))) %>%
    ungroup()

  # 3. Housing Costs
  df <- df %>%
    # Set NAs for ownership cost and family income. RENTGRS doesn't have an NA code listed
    mutate(OWNCOST = if_else(OWNCOST == 99999, NA_real_, OWNCOST),
           HHINCOME = if_else(HHINCOME == 9999999, NA_real_, HHINCOME))

  # Assign cost to household as the greater of ownership or rent costs (most houses have a 0 for one or the other)
  df <- df %>%
    group_by(row_number()) %>%
    mutate(hcost = 12 * max(OWNCOST, RENTGRS)) %>% #annualize monthly cost to compare to income
    ungroup() %>%
    select(-`row_number()`) %>%
    mutate(hcost_dep = if_else(hcost/HHINCOME >= 0.5, 1, 0))





}
