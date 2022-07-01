 # BGVCOL Priority Index

This repository contains scripts related with the methodolggy implemented in the BGVCOL Species Priority Index

## Requirements

- R 3.6 or above
### R Libraries 
- sets
- dplyr
- scales 
- BBmisc 
- Ckmeans.1d.dp
- stringer

## How to run?

In script folder you will find 5 R scripts, every scripts executes a R routine and return a csv file with results, In the following image you can see the process involved in each script and then the description of each one :


![fuzzyDiagScriptsv2](https://user-images.githubusercontent.com/50838265/176958279-7261ec4c-6da3-43e6-ba3e-6a6402731778.svg)

### EconBenefits.R 

#### Input
`TablaCriterios5.5.xlsx`
#### Output
`data_eco_bgv.csv`
#### ppFoodSec.R

#### Input
`TablaCriterios5.5.xlsx`
#### Output
`data_foosec_bgv.csv`
### FoodSec.R  

#### Input
`data_foosec_bgv.csv`
#### Output
``data_foosec_bgv.csv``
### Imputation.R

#### Input
`data_noimp_bgv.csv`
#### Output
`data_bgv.csv`

### IntIndex.R

#### Input
`data_bgv.csv`
#### Output
`INT_bgv_1608_1-375.csv`

