# Libraries
library(tidyverse)
library(survey)

# Read in data from IPUMS
df <- read_csv("./mpi_13_18_raw.csv",
               col_types = cols(
                   YEAR = col_double(),
                   SAMPLE = col_double(),
                   SERIAL = col_double(),
                   CBSERIAL = col_double(),
                   HHWT = col_double(),
                   CLUSTER = col_double(),
                   STATEFIP = col_double(),
                   METRO = col_double(),
                   MET2013 = col_double(),
                   PUMA = col_double(),
                   STRATA = col_double(),
                   GQ = col_double(),
                   FARM = col_double(),
                   OWNCOST = col_double(),
                   RENTGRS = col_double(),
                   HHINCOME = col_double(),
                   LINGISOL = col_double(),
                   BEDROOMS = col_double(),
                   CINETHH = col_double(),
                   CILAPTOP = col_double(),
                   CISMRTPHN = col_double(),
                   CITABLET = col_double(),
                   CIHAND = col_double(),
                   CIOTHCOMP = col_double(),
                   PERNUM = col_double(),
                   PERWT = col_double(),
                   SEX = col_double(),
                   AGE = col_double(),
                   RACE = col_double(),
                   RACED = col_double(),
                   HISPAN = col_double(),
                   HISPAND = col_double(),
                   CITIZEN = col_double(),
                   HCOVANY = col_double(),
                   EDUC = col_double(),
                   EDUCD = col_double(),
                   EMPSTAT = col_double(),
                   EMPSTATD = col_double(),
                   INCINVST = col_double(),
                   INCRETIR = col_double(),
                   POVERTY = col_double(),
                   DIFFMOB = col_double(),
                   DIFFCARE = col_double()
                 )
               ) %>%
  select(-EDUC, -EMPSTATD, -CILAPTOP, -CISMRTPHN, -CITABLET, -CIHAND, -CIOTHCOMP) #unnecessary fields auto selected to go with empstat and educd

df_18_lou <- df %>%
  filter(YEAR == 2018 & MET2013 == 31140) #louisville MSA in 2018 in order to test

source('~/Documents/pers/mpitools/R/create_mpi.R')

df_test <- df_18_lou %>%
  add_binary_indicators()


