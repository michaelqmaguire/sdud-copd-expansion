#-----------------------------------------------------------------------------------------------------------------#
#                                                                                                                 #
# PROJECT: SDUD COPD MEDICAID EXPANSION				                                                                    #
# AUTHOR: MICHAEL MAGUIRE, MS, DMA II                                                                             #
# INSTITUTION: UNIVERSITY OF FLORIDA, COLLEGE OF PHARMACY                                                         #
# DEPARTMENT: PHARMACEUTICAL OUTCOMES AND POLICY                                                                  #
# SUPERVISORS: AMIE GOODIN, PHD, MPP | JUAN HINCAPIE-CASTILLO, PHARMD, PHD, MS                                    #
# SCRIPT: 02_import-and-clean.R                                                                			              #
#                                                                                                                 #
#-----------------------------------------------------------------------------------------------------------------#

library(data.table)
library(dplyr)

## Read in the dataset. 
## Coerce NDC to character. Otherwise it is read as integer.

base <-
  fread(
    file_location,
    colClasses = c("proper_ndc" = "character")
  )

## Read input files.

# IN1 = contains aggregate statistics.

in1 <-
  readxl::read_xlsx(
    path = "./data/raw/aggregate-by-state-incl-new-states.xlsx"
  )

# IN2 = contains drugs that need to be pulled.

in2 <-
  readxl::read_xlsx(
    path = "./data/raw/On-label prescriptions of inhaled bronchodillators indicated for COPD_5.13.21.xlsx"
  )

## Extract states

st <-
  toupper(unique(in1$state))

## Extract years.

yrs <- 
  unique(in1$year)

## Extract drugs. Remove quotes I intentionally placed to prevent truncated leading zeroes on import.

rx <- 
  gsub(unique(in2$NDC), pattern = "'", replacement = "")

## Subset dataset to years and states of interest.

yrSt <-
  base[
    i = (year %in% yrs) & (state %in% st),
    j = .(year, proper_ndc, quarter, state, suppression, numberrx, prodnme, gennme)
  ]

## Subset to drugs of interest.

drugs <-
  yrSt[
    i = proper_ndc %in% rx
  ]

## Create halfyear variable based on quarters.
## Remove quarter since it's not necessary anymore.
## Naming here stands for 'drugs modification 1.'.

drugsM1 <-
  drugs[
    i = ,
    j = 
      halfyear := fcase(
        quarter %in% c(1:2), 1,
        quarter %in% c(3:4), 2,
        default = NA
      )
  ]

drugsM1 %>%
  janitor::tabyl(halfyear, quarter)

## Aggregate overall by state, year, and halfyear, and suppression.

drugsAggState <-
  drugsM1[
    i  = ,
    j  = .(
      totalRX = sum(numberrx)
    ),
    by = c("year", "state", "halfyear", "suppression") 
  ]

setorder(drugsAggState, year, state, halfyear)

## Aggregate by year, generic name, state, halfyear, and whether records were suppressed.

drugsAggGeneric <-
  drugsM1[
    i  = ,
    j  = .(
      totalRX = sum(numberrx)
    ),
    by = c("year", "gennme", "state", "halfyear", "suppression")
  ]

setorder(drugsAggGeneric, year, state, halfyear, gennme)

## Aggregate by year, generic name, product name, state, half year, and whether records were suppressed.

drugsAggProdnme <-
  drugsM1[
    i  = ,
    j  = .(
      totalRX = sum(numberrx)
    ),
    by = c("year", "gennme", "prodnme", "state", "halfyear", "suppression")
  ]

setorder(drugsAggProdnme, year, state, halfyear, prodnme)  

# Creating final data set.

final <-
  in1 %>%
    select(state, group, year, half_year, chipMedicaidEnroll) %>%
    mutate(state = toupper(state)) %>%
    rename(halfyear = half_year) %>%
    left_join(
      drugsAggState,
      by = c(
        "state" = "state",
        "year" = "year",
        "halfyear" = "halfyear"
      )
    )

final

## ---------------------- ##
## Impute missing values. ##
## ---------------------- ##

drugsM1Imputed <-
  drugsM1[
    i = ,
    j = 
      imputedRx := fcase(
        suppression == "T", 10,
        suppression == "F", numberrx,
        default = NA
      )
  ]

## Checking to make sure only suppression = "T" fields were imputed.

drugsM1 %>%
  group_by(suppression) %>%
  summarize(
    numberRxCheck = sum(numberrx)
  )

drugsM1Imputed %>%
  group_by(suppression) %>%
  summarize(
    numberRxCheck = sum(imputedRx)
  )

## Aggregate imputed numbers overall by state, year, and halfyear

drugsAggStateImputed <-
  drugsM1Imputed[
    i  = ,
    j  = .(
      totalRX = sum(imputedRx)
    ),
    by = c("year", "state", "halfyear") 
  ]

setorder(drugsAggStateImputed, year, state, halfyear)

## Aggregate imputed numbers by year, generic name, state, halfyear.

drugsAggGenericImputed <-
  drugsM1Imputed[
    i  = ,
    j  = .(
      totalRX = sum(imputedRx)
    ),
    by = c("year", "gennme", "state", "halfyear")
  ]

setorder(drugsAggGenericImputed, year, state, halfyear, gennme)

## Aggregate imputed numbers by generic name, product name, state, half year.

drugsAggProdnmeImputed <-
  drugsM1Imputed[
    i  = ,
    j  = .(
      totalRX = sum(imputedRx)
    ),
    by = c("year", "gennme", "prodnme", "state", "halfyear")
  ]

setorder(drugsAggProdnmeImputed, year, state, halfyear, prodnme)  

## Create final data set with imputed numbers.

finalImputed <-
  in1 %>%
    select(state, group, year, half_year, chipMedicaidEnroll) %>%
    mutate(state = toupper(state)) %>%
    rename(halfyear = half_year) %>%
    left_join(
      drugsAggStateImputed,
      by = c(
        "state" = "state",
        "year" = "year",
        "halfyear" = "halfyear"
      )
    )

finalImputed

fwrite(drugsAggState, file = "./data/clean/01_copd-rx-aggregate-by-state.csv", sep = ",")
fwrite(drugsAggGeneric, file = "./data/clean/02_copd-rx-aggregate-by-state-and-generic.csv", sep = ",")
fwrite(drugsAggProdnme, file = "./data/clean/03_copd-rx-aggregate-by-state-generic-and-brand.csv", sep = ",")
fwrite(final, file = "./data/clean/04_copd-final.csv")

fwrite(drugsAggStateImputed, file = "./data/clean/05_imputed-copd-rx-aggregate-by-state.csv", sep = ",")
fwrite(drugsAggGenericImputed, file = "./data/clean/06_imputed-copd-rx-aggregate-by-state-and-generic.csv", sep = ",")
fwrite(drugsAggProdnmeImputed, file = "./data/clean/07_imputed-copd-rx-aggregate-by-state-generic-and-brand.csv", sep = ",")
fwrite(finalImputed, file = "./data/clean/08_imputed-copd-final.csv")
