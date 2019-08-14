###############################
## Loading Required Packages ##
###############################

library(dplyr) ## contains functions for data cleaning/manipulation
library(tidyr) ## contains functions for data cleaning/manipulation
library(stargazer) ## contains functions to produce LaTeX code for tables

## Set working directory to the location of the master replication folder
setwd(" ")

#########################################################################################################
## Preliminaries -- This section MUST be run in order for other parts of the code to function properly ##
#########################################################################################################

## Load master_table.csv, containinng the data frame produced by data_processing.R in the ./Code/ directory
## Contains all compiled data from the ./Data/ directory, which is used to produce all of the tables and models in the final paper
master_table <- read.csv("./Data/master_table.csv",
                         stringsAsFactors = FALSE)

#########################################
## LaTeX Tables for Summary Statistics ##
#########################################

## Create tables for summary statistics on the dependent (freedom_vdem) and independent (SPI_index) variables
## Save them in the ./Tables/ directory as "summary_freexp.tex" and "summary_spi.tex", respectively

writeLines(capture.output(stargazer(master_table$freedom_vdem,
                                    digits = 3, summary = F, rownames = T)),
           "./tables/summary_freexp.tex")

writeLines(capture.output(stargazer(master_table$SPI_index,
                                    digits = 3, summary = F, rownames = T)),
           "./tables/summary_spi.tex")

#######################################
## Bivariate Linear Regression Model ##
#######################################

## Run a bivariate regression between log_vdem and SPI_index, as an lm() object
bivar_model <- lm(log_vdem ~ SPI_index, data = master_table[-c(86,122)])

## Generate LaTeX code for the bivariate regression results, saved in the ./Tables/ directory
writeLines(capture.output(stargazer(bivar_model,
                                    digits = 3, summary = F, rownames = T)),
           "./tables/bivar_regression.tex")
##########################################
## Multivariate Linear Regression Model ##
##########################################
##
## Secondary IV's (controls): continent, num_rights, con_age, mil_spend, nat_inc, tech_accs,
##                 gender_eq, brit_col, chrst/islm/jud/bud_pct, mil_events, 
##                gini_index (3 missing values!)
##
## Note on using both mil_spend AND mil_events:
## The logic in inlcluding both of these control variables rests on attempting to control for
## countries with a high enforcement presence (proxied by military spending), while simultaneously
## accounting for countries with increased military spending due to ongoing conflict (using total armed events from 2006-2008)
##
## Final model p-value: 0.026617
##
## To check for robustness, the following insignificant covariates were inserted into the model, and their effect on IV significance was recorded
## Covariate significance:
## num_rights - 0.015902
## gender_inequality - 0.032787
## brit_col - 0.027257
## chrst_pct - 0.030589
## gini_index - 0.00750
## con_age - 0.01858
##
## Final model covariates: 
##      European continent (dummy), 
##      N/S American continent (dummy), 
##      free expression clause (dummy), 
##      military spending, 
##      national income, 
##      military spending/events interaction term

## Run a multivariate regression between log_vdem and all IVs, as an lm() object
multivar_model <- lm(log_vdem ~ SPI_index + dummy_freexp + dummy_eu + dummy_am +
                                     mil_spend + nat_inc + mil_spend*mil_events,
                                   data = (master_table[-c(86,122),]))

## Generate LaTeX code for the multivariate regression results, saved in the ./Tables/ directory
writeLines(capture.output(stargazer(multivar_model,
                          digits = 3, summary = F, rownames = T)),
           "./tables/multivar_regression.tex")

## Save the multivariate model as an RData file in the ./Data/ directory, to be used in figures.R for predictive analysis
save(multivar_model, file = "./data/multivar_model.RData")
