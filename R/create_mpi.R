#' Create binary deprivation indicators
#'
#' Each indicator is coded so that 1 represents a deprivation in that category
#'
#' The indicators are income poverty, employment, housing costs, educational attainment, internet access, linguistic isolation, health insurance, disability, and overcrowded housing
#' This function also drops original columsn once no longer needed in order to save memory. If you still need original columns for later analysis you can remove the select minus statements
#' @param df A dataframe.
#' @return A dataframe with dichotomous indicators
#' @examples
#' add_binary_indicators(df)
#'

add_binary_indicators <- function(df){

  # Economic Indicators ---------------------------------------------------
  # 1. Income Poverty
  df <- df %>%
    mutate(income_poverty = case_when(
      POVERTY < 100 ~ 1, #lives below 100 percent of poverty line
      POVERTY == 0 ~ NA_real_, #NA code in census data
      POVERTY >= 100 ~ 0, #at or above poverty line
      TRUE ~ NA_real_
    )) %>%
    select(-POVERTY)

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
           fam_emp = if_else(any(emp_num == 1)|any(retired ==1) | any(investment == 1 | FARM == 2), 0, 1)) %>%
    ungroup() %>%
    select(-emp_num, -retired, -investment, -INCINVST, -INCRETIR, -FARM)

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
    mutate(hcost_dep = if_else(hcost/HHINCOME >= 0.5, 1, 0)) %>%
    select(-hcost, -OWNCOST, -RENTGRS, -HHINCOME)

  # Educational ---------------------------------------------------

  # 4. Educational Attainment

  #Don't have a HS degree and over 18. EDUCD == 1 is NA.
  #EDUCD == 62 is high school graduate or GED.

  df <- df %>%
    mutate(education = if_else(EDUCD < 62 & AGE > 18 & EDUCD != 1, 1, 0)) %>%
    #For children 18 and younger, we check to see if anyone in the household has a h.s. degree
    group_by(SERIAL, YEAR) %>%
    mutate(fam_edu = if_else(any(EDUCD >= 62), 0, 1)) %>%
    ungroup() %>%
    mutate(education = case_when(
      AGE < 19 & fam_edu == 1 ~ 1,
      AGE < 19 & fam_edu == 0 ~ 0,
      TRUE ~ education
    )) %>%
    select(-EDUCD)

  # 5. Internet Access

  # If no internet access then deprived - originally I had a qualifier about owning a device.
  # But which devices are asked about switches over time
  df <- df %>%
    mutate(int_access = case_when(
      CINETHH == 3 ~ 1,
      CINETHH == 0 ~ NA_real_,
      TRUE ~ 0
    )) %>%
    select(-CINETHH)

  # 6. Linguistic Isolation
  df <- df %>%
    mutate(ling_isol = case_when(
      LINGISOL == 0 ~ NA_real_,
      LINGISOL == 1 ~ 0,
      LINGISOL == 2 ~ 1
    )) %>%
    select(-LINGISOL)

  # Health indicators ---------------------------------------------------

  # 7. Health Insurance
  # There is no NA category in the data, so just recoding from 1, 2 to 0, 1
  df <- df %>%
    mutate(health = if_else(HCOVANY == 2, 0, 1)) %>%
    select(-HCOVANY)

  # 8. Disability
  # Based on self-identified disability
  df <- df %>%
    mutate(disability = if_else(DIFFMOB == 2|DIFFCARE == 2, 1, 0)) %>%
    select(-DIFFMOB, -DIFFCARE)

  # 9. Overcrowded housing
  # More than two people per bedroom
  df <- df %>%
    group_by(SERIAL, YEAR) %>%
    mutate(hhsize = n(),
           overcrowd = if_else(hhsize > (2 * BEDROOMS), 1, 0)) %>%
    select(-BEDROOMS) # keep hhsize for later analysis
}

#' Add MPI for individuals
#'
#' @param df A dataframe.
#' @param threshold a number between 0 and 1 indicating the cutoff for MPI poverty
#' @return A dataframe with mpi values
#' @examples
#' add_mpi(df)

add_mpi <- function(df, threshold = .32){
  df <- df %>%
    mutate(mpi_score = (income_poverty + fam_emp + hcost_dep +
                          education + int_access + ling_isol +
                          health + disability + overcrowd) / 9,
           mpi_poor = if_else(mpi_score > threshold, 1, 0))
}

