## SDUD COPD Expansion Project - 2011 to 2016

**Objective:** Examine prescription drug changes across 50 states (including DC) following a 2014 intervention using the State Drug Utilization Data provided by CMS.
**Method:** Interrupted Time Series (ITS).
**Status:** Incomplete - Analyses being conducted.

**Lead:** 
  - Cilia Zayas, MSF, MHA | PhD Student, Research Associate III | University of Arkansas for Medical Sciences College of Medicine - Department of Biomedical Informatics
 
*Assisting*: 
 - Amie Goodin, MPP, PhD | Assistant Professor | University of Florida College of Pharmacy - Department of Pharmaceutical Outcomes and Policy
 - Juan Hincapie-Castillo, PharmD, PhD, MS | Assistant Professor | University of Florida College of Pharmacy - Department of Pharmaceutical Outcomes and Policy
 - Michael Maguire, MS | Data Management Analyst II | University of Florida College of Pharmacy - Department of Pharmaceutical Outcomes and Policy

Raw data are available [here](https://www.medicaid.gov/medicaid/prescription-drugs/state-drug-utilization-data/index.html).

Compiled data set is not publicly available, but can be made available upon request.

Methodology can be located at the following GitHub repository: **Currently under review and will be committed after final checks.**

Files included in the data/clean directory are the following:

- 01_all-plots.pdf: Contains all the plots I made just to show how the data looks.
- 01_copd-rx-aggregate-by-state.csv: This contains aggregate numbers at the state, year, and half-year level. 
- 02_copd-rx-aggregate-by-state-and-generic.csv: This contains aggregate numbers at the state, year, half-year, and generic name level.
- 03_copd-rx-aggregate-by-state-generic-and-brand.csv: This contains aggregate numbers by state, year, half-year, generic, and brand name level.
- 04_copd-final.csv: This file contains the aggregate numbers by state, year, half-year, the number of CHIP medicaid enrollees, and the treatment group.
 
Files 05-08 are the same as 01-04 except that values were imputed at the brand name level. Accordingly, each suppressed record at the brand level was given a value of ten.
 
- 09_drugs-included-in-dataset.csv: This file contains the drugs and NDC's included in the dataset.
