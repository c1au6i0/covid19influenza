
<!-- README.md is generated from README.Rmd. Please edit that file -->

# covid19 and influenza vaccination coverage

This repository contains the scripts used to analyzed the data reported
in the paper:

*Influenza Vaccination and COVID19 Mortality in USA* 

Claudio Zanettini\*, Mohamed Omar\*, Wikum Dinalankara, Eddie Luidy
Imada, Elizabeth Colantuoni, Giovanni Parmigiani, and Luigi Marchionni.

# Files and folders:

## code/scripts

Contains the scripts for retrieving, pre-processing and analyzing the
data.

  - `code/script/libraries_functions`: loads the libraries (including
    `covid19census`).

  - `code/script/functions_analysis`: contains functions used to
    pre-process, analyze and summarize data. Functions are documented.

  - `code/scripts/us_preprocess`: this is used to retrieve and
    pre-process U.S data. Execution of this script returns the dataframe
    used for the analyses (`dat_selected`). A static copy of the
    dataframe is in `data/data_selected_variables.RDS`.

  - `code/strata_selected_variables.R`: used to perform stratified and
    stability analyses.

  - `code/strata_selected_variables.R`: includes the analyses in which
    propesity score is divided in quintiles or tertiles and added to the
    linear model as a factor or left as a continous variable.

## code/analyses.R

The file `analyses.R` executes all the scripts in `code/scripts` and
aggregate results in 2 dataframes and in a table.

-----

# Sources and data

The file `data/data_selected_variables.RDS` contains a static copy of
the data used for the analysis.

Details regarding the data sources as well as functions to extract
updated COVID-19 data and aggregate them with other socio-economic and
health related metrics can be found in the [covid19census R
package](https://github.com/c1au6i0/covid19census). Please refer to the
package README or documentation for more information regarding the
variables. The scripts used to import static data are reported in the
package repository
[here](https://github.com/c1au6i0/covid19census/data-raw/).
