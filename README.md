# This repository contains the replication materials for the project **From Montesquieu to Scalia: Cross-national Separation of Powers and Free Expression** by **Rohnin Randles**

## *Setup and Benchmarks*

Original code run on:
* Windows 10 Pro Version 1803
* 16.0 GB RAM
* 2.60GHz CPU, 2 cores
* R version 3.4.2

`R` packages:
  * dplyr
  * tidyr
  * countrycode
  * ggplot2
  * stargazer
  * maps

Benchmarks:
* `code.R`: 145.53 sec

## *Instructions in Brief*

1. Run `data_processing.R` to compile the datasets in the ./Data/ directory into the cleaned and formatted master data frame to use in the analysis
2. Run `models.R` to generate the bivariate and multivariate linear regression results, along with the formatted tables
3. Run `figures.R` to generate all figures, including dependent and independent variable distributions, regression analysis visualizations, and predictive models

## *Data*

The ./Data/ directory contains the necessary data to replicate the analysis and anlytical figures and tables in the paper. Below, I describe each of the datasets in this directory.

* `case_names.csv`: contains a vector of country spelling and formatting based on what is used in `CCP_raw.csv`. Used throughout the code to ensure proper compatibility across all data and files.
* `CCP_raw.csv`: complete dataset on textual content of the world's constitutions. Contains variables for rights, powers, clauses, etc. Downloaded from the *Comparative Constitutions Project* (CCP).
* `ccp_rights_list.csv`: vector of select column names from `CCP_raw.csv` related to all variables listed in the Rights section of the CCP codebook.
* `cepii_data.csv`: contains data on colonial history for various countries. Used in `data_processing.R` to generate covariate for history of British colonization. Downloaded from Centre d'Etudes Prospectives et d'Informations Internationales (CEPII).
* `conAge.csv`: contains vector of constitutional age based on CCP data. Calculated by subtracting the year 2008 from the year of constitutional creation in `CCP_raw.csv`
* `countries_map.csv`: manually entered vector of country names used by the maps package in `R`. Used to ensure spelling and compatibility by `figures.R` when generating the IV map visualization.
* `editCountries.csv`: manually entered dataset containing country names and years for countries who did not have data in `CCP_raw.csv` for the year 2008, but did for other years. Used by `data_processing.R` to substitute data for those different years in `CCP_raw.csv`.
* `ex_bank.csv`: dataset of manually entered values based on the allocation of central bank appointment power in constitutional texts. Texts provided by the *Constitute Project*
* `HNL_raw.csv`: complete dataset on legislative powers based on the *Handbook of National Legislatures* by Fish & Kroenig (2009). Downloaded from the faculty website of M. Steven Fish at UC Berkeley.
* `master_table.csv`: complete dataset of all compiled data from the ./Data/ directory used in the final analysis, figures, and tables. Generated and output by `data_processing.R`.
* `MIDIP_4.01.csv`: contains data on military conflict for countries around the world. Used in `data_processing.R` to generate covariate for military interaction. MIDIP stands for **M**ilitarized **I**nterstate **Di**s**P**utes; downloaded from Correlates of War (COW).
* `multivar_model.RData`: RData file containing the multivariate regression model. Generated and output by `models.R`, used in `figures.R` to generate predictive plots.
* `nat_sec.csv`: dataset of manually entered values based on the allocation of national security council appointment power in constitutional texts. Texts provided by the *Constitute Project*
* `p4v_2008.csv`: complete dataset for the year 2008 on various governmental indicators. Downloaded from the *Polity Project*.
* `powers_list.csv`: dataset of manually entered countries whose constitutional texts include mention of competencies of the highest court. Texts provided by the *Constitute Project*
* `religion_data.csv`: complete dataset on number and percent of religious adherents by country dating back to 1945. Downloaded from Correlates of War (COW).
* `swiid_data08.csv`: dataset on income inequality providing GINI index scores for nearly all countries in the analysis (123/126). Downloaded from the Standardized World Income Inequality Database (SWIID).
* `UNHDR_gender.csv`: complete dataset on gender inequality indicators, used by `data_processing.R` to produce gender equality covariate. Downloaded from United Nations Human Development Reports (UNHDR).
* `V-Dem-CY+Others-v8.csv`: complete dataset from the *Varieties of Democracy* project. Used to provide the dependent variable for the analysis; downloaded from *Varieties of Democracy* (V-Dem).
* `WDI_military_expend.csv`: complete dataset on military expenditures as % of GDP in constant 2010 USD. Used by `data_processing.R` as a covariate; downloaded from the World Bank's World Development Indicators (WDI).
* `WDI_national_income.csv`: complete dataset on national income in constant 2010 USD. Used by `data_processing.R` as a covariate; downloaded from the World Bank's World Development Indicators (WDI).
* `WDI_technology_access.csv`: complete dataset on technology access as the number of cellular phone subscriptions per 100 people. Used by `data_processing.R` as a covariate; downloaded from the World Bank's World Development Indicators (WDI).

## *Code*

The ./Code/ directory contains the code to replicate the analysis and analytical figures and tables in the paper. Below, I describe each code script in this directory. The ./Figures/ directory contains a copy of each of the figures generated by these scripts; the ./Tables/ directory contains a copy of the LaTeX table code generated by these scripts.

* `data_processing.R`: Code used to compile the datasets for independent, dependent, and control variables located in the ./Data/ directory into one master data table to be used for the analysis. This includes all data required to create regression models and figures used in the final paper, as well as computation of the Separate Powers Index (SPI) and its three sub-indices. Generates and outputs the `master_table.csv` data frame to the ./Data/ directory.
* `models.R`: Code used to take the data from `master_table.csv` and create summary statistics of both the independent and dependent variables, bivariate and multivariate regression models. Generates and outputs all LaTeX code for the tables used in the final paper in the form of .tex files in the ./Tables/ directory.
* `figures.R`: Code used to generate all figures from `master_table.csv` used in the final paper. Generates and outputs PDF files to the ./Figures/ directory for independent and dependent variable distributions, hypothesis testing results based on multivariate regression, and predictive models for free expression regressed against the SPI and freedom of expression clauses.
