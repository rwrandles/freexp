###############################
## Loading required packages ##
###############################

library(dplyr) ## contains functions for data cleaning/manipulation
library(tidyr) ## contains functions for data cleaning/manipulation
library(countrycode) ## contains functions and data for country codes, a universal country identification

## Set working directory to the location of the master replication folder
setwd(" ")

#########################################################################################################
## Preliminaries -- This section MUST be run in order for other parts of the code to function properly ##
#########################################################################################################

## Load in CCP_raw.csv from the ./Data/ directory, which contains constitutional text data from the Comparative Constitutions Project
ccp_raw <- read.csv("./Data/CCP_raw.csv",
                    header = TRUE, stringsAsFactors = FALSE) %>% arrange(country)

## Load in cawse_names.csv, which keeps track of proper spelling and formatting to ensure compatibility across all code and files
case_names <- read.csv("./Data/case_names.csv",
                       header = TRUE, stringsAsFactors = FALSE) %>% arrange(x)

## Filter the CCP data for only constitutional texts from the year 2008, as well as only those that are going to be used in the analysis (those whose names are listed in case_names)
## Finally, sort the countries from A to Z
ccp_data <- ccp_raw %>% filter(year == 2008, country %in% case_names$x) %>% arrange(country)

## Create an object named replace_list, which is a 21 x 2 dataframe to hold the information on the 21 countries whose constitutional data was not available for 2008, but was available for other years
replace_list <- data.frame(matrix("", nrow = 21, ncol = 2))
colnames(replace_list) <- c("country", "year") ## set the column names for replace_list

## Set the list of countries whose data needs to be tracked from years other than 2008 and inserted
replace_list$country <- c("Argentina",
                          "Botswana",
                          "Brazil",
                          "Gabon",
                          "Georgia",
                          "Guinea-Bissau",
                          "Guyana",
                          "Hungary",
                          "India",
                          "Italy/Sardinia",
                          "Kazakhstan",
                          "Lesotho",
                          "Malawi",
                          "New Zealand",
                          "Nicaragua",
                          "Norway",
                          "Slovakia",
                          "Sweden",
                          "Switzerland",
                          "Tunisia",
                          "United Kingdom")
## Set the year for each country that will be used for the analysis
replace_list$year <- c(1996, 
                       2004, 
                       2005, 
                       1999,
                       2004, 
                       1992, 
                       1999, 
                       2006, 
                       2004, 
                       2006, 
                       2006, 
                       2000, 
                       2000, 
                       1986, 
                       2006,
                       2005, 
                       2003, 
                       1999, 
                       2002, 
                       2001, 
                       1971)

## This for loops moves through the replace_list dataframe, and then crawls through the raw CCP data to find the country and year that match the one listed in replace_list
## Once this particular country-year is found, it then copies that row into the corresponsing row in ccp_data
for(i in 1:length(which(ccp_data$country %in% replace_list[,1]))){
  ccp_data[ccp_data$country == replace_list[i,1] ,] <- ccp_raw[(ccp_raw$country == replace_list[i,1] & ccp_raw$year == replace_list[i,2]) ,]
}

################################
## Begin variable compilation ##
################################

## Create a new data frame called master_table, which will hold the final compiled version of the data to be used for the analysis
master_table <- data.frame(matrix(nrow = 128, ncol = 63), stringsAsFactors = FALSE)

## Manually set all of the column (variable) names
colnames(master_table) <- c("country", 
                            "continent", 
                            "leg_pow", 
                            "ex_pow", 
                            "jud_pow",
                            "leg_share",
                            "ex_share",
                            "jud_share",
                            "SPI_index",
                            "freedom_vdem",
                            "num_rights",
                            "dummy_freexp",
                            "replace_ex",
                            "serve_min",
                            "interpellate",
                            "investigate",
                            "oversee_pol",
                            "appoint_pm",
                            "appoint_min",
                            "lack_pres",
                            "no_conf",
                            "no_diss",
                            "no_decree",
                            "no_veto",
                            "no_review",
                            "no_gate",
                            "no_impound",
                            "cont_resor",
                            "immunity",
                            "elected",
                            "amend",
                            "war",
                            "treaty",
                            "amnesty",
                            "pardon",
                            "judiciary",
                            "bank",
                            "media",
                            "sessions",
                            "secretary",
                            "staff",
                            "no_limit",
                            "seek_reelect",
                            "experience",
                            "jscore_1",
                            "jscore_2",
                            "jscore_3",
                            "jscore_4",
                            "jscore_5",
                            "jscore_6",
                            "jscore_7",
                            "jscore_8",
                            "jscore_9",
                            "jscore_10",
                            "escore_1",
                            "escore_2",
                            "escore_3",
                            "escore_4",
                            "escore_5",
                            "escore_6",
                            "escore_7",
                            "escore_8",
                            "escore_9")

#############
## Country ##
#############

## Copy the country column from ccp_data, which holds constitutional text data for all countries in the analysis
master_table$country <- ccp_data$country

###############
## Continent ##
###############

## Using the countrycode package, assign the continent of each country 
master_table$continent <- countrycode(sourcevar = master_table$country , origin = "country.name", destination = "continent")

## For some reason, the countrycode doesn't recognize Vietnam, so I manually set it here
master_table$continent[master_table$country == "Vietnam, Democratic Republic Of"] <- "Asia"

###################################
## Freedom of Expression (V-Dem) ##
###################################

## First, read in the .csv containing the free expression data from V-Dem
## Then filter it for the year 2008 and only include the columns for country name and freedom index
## Finally, assign this to a new object called freedom_indexData
freedom_indexData <- read.csv("./Data/V-Dem-CY+Others-v8.csv",
                              header = TRUE, stringsAsFactors = FALSE) %>% filter(year == 2008) %>%
  select(country_name, v2x_freexp_altinf)

## Set the country names to the ones in editCountries.csv. This ensures that all country names are spelled
## exactly as they appear in the CCP dataset and to ensure compatibility across datasets
freedom_indexData$country_name <- read.csv(file = "./Data/editCountries.csv",
                                           header = TRUE, stringsAsFactors = FALSE)$x

## Filter for only the countries that are used in the analysis and sort countries alphabetically
freedom_indexData <- freedom_indexData %>% filter(country_name %in% case_names$x) %>% arrange(country_name)

## Copy the free expression index column from freedom_indexData to master_table
master_table$freedom_vdem <- freedom_indexData$v2x_freexp_altinf

## Create the log-transformed DV column
master_table <- master_table %>% 
  mutate(log_vdem = log(1 - freedom_vdem))

#############################################
## legislative power indicator starts here ##
#############################################

## First, I create a vector of weights which will be applied to the 32 legislative powers catalogued in Fish & Kroenig (2009).
## The values for the weights are taken from Chernykh et. al (2017)
wts <- c(0.036001665, 0.020708647, 0.035832422, 0.038494086,	0.035647801, 0.032693817,	0.030709109, 
         0.024693446,	0.034832379, 0.036478606, 0.03389387,	0.03703248, 0.026339678, 0.036217058,	
         0.033632321, 0.03629398, 0.024154961,	0.037801742, 0.032678433,	0.034616981, 0.034001569,	
         0.020077851, 0.018600857,	0.029985998, 0.023262612,	0.023554932, 0.035893966,	0.025785805, 
         0.030832191,	0.032186099, 0.033816947, 0.03324769)

## Read in HNL_raw.csv, which contains a list of binary variables based on legislative powers; taken from the faculty website of M. Steven Fish at UC Berkeley
## Next, assign it to a new object called ppi_data
ppi_data <- read.csv("./Data/HNL_raw.csv",
                     header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM") %>% arrange(country) %>% filter(country %in% case_names$x)

## This for loop multiplies each power in ppi_data by the corresponding weight located in wts
## This value is then reassigned back into ppi_data. The end result is a weighted ppi_data
for(j in 8:39){
  ppi_data[,j] <- ppi_data[,j] * wts[j-7]
}

## Copy a subset of ppi_data (the set of columns between replace_ex and experience) into master_table
## This completes all of the components required to calculate the legislative power index
for(i in colnames(subset(ppi_data, select = c(replace_ex:experience)))){
  master_table[,i] <- ppi_data[,i]
}

## The legislative power index is calculated as the sum of all of the weighted legislative powers
## Sum up each of these columns across all rows and assign it to the leg_pow column
master_table$leg_pow <- rowSums(subset(master_table, select = replace_ex:experience))

#######################################################
## judiciary power indicator calculations start here ##
#######################################################
## sub-indices:
## jscore_1: 1/2 - is high court mentioned; 1/4 - competencies; 1/8 - access; 1/8 - term length; 
## jscore_2: T/F - majority needed to amend; 1/3 * # of branches req'd;
## jscore_3: justice nomination & approval subject to the coding chart --
##             Appointing Body
##           Leg     Ex     Jud
##      Leg   0      1/3    2/3
##      Ex   1/3     0      2/3
##      Jud  2/3     2/3     1
## jscore_4: Length of term OR Mandatory retirement, according to chart:
##          Length of term: (0,4) = 0.0; [4,6) = 0.2; [6,8) = 0.4; [8,10) = 0.6; [10,12) = 0.8; 12+ = 1.0
##          Mandatory retirement: Life = 1.0; 75+ = 1.0; [65, 75) = 0.8; <65 = 0.6
## jscore_5: Judge reelection
## jscore_6: Removing body: 1 - judicial procedure; 0 - executive; 0 - legislature; 1/2 - ex/leg committee
## jscore_7: Measure against income reduction
## jscore_8: 1 - Individuals have access in all matters; 1/2 - individuals have access in some matters; 0 - only gov't has access
## jscore_9: Power of constitutional review
## jscore_10: 1/3 - publish decision reasoning; 1/3 - publish extended version; 1/3 - publish dissents
##
## The sub-index is computed as the mean of the 10 sub-indices
#######################
## Measures from CCP ##
#######################
## jscore_1: LEVJUD, other (powers_list), CHALLEG, SUPTERMN
## jscore_2: AMNDAMAJ, AMNDAPPR, AMNDPROP
## jscore_3: SUPNOM, SUPAP
## jscore_4: SUPTERM
## jscore_5: SUPTERMN
## jscore_6: JREM, JREMPRO, JREMAP
## jscore_7: JUDSAL
## jscore_8: CHALLEG
## jscore_9: INTERP
## jscore_10: HOCOP, HOCOPW
################################################
################################################


############################
## jscore_1 -- High Court ##
############################

## Read in powers_list.csv, which contains a vector of countries whose constitutions list competencies of the high court
## Assign this vector to a new object called powers_list
powers_list <- read.csv("./Data/powers_list.csv",
                        header = TRUE, stringsAsFactors = FALSE)

## The sub-indicator is the sum of each of the four components: 
master_table$jscore_1 <- round(ifelse(ccp_data$levjud == 1 | ccp_data$levjud == 2 | ccp_data$levjud == 3, 0.5, 0) + ## +1/2 if high court is mentioned, +0 if not
                                 ifelse(ccp_data$country %in% powers_list$country, 0.25, 0) + ## +1/4 if competencies are listed, +0 if not
                                 ifelse(ccp_data$challeg_98 == 0, 0.125, 0) + ## +1/8 if court access is mentioned, +0 if not
                                 ifelse(ccp_data$supterm != 98, 0.125, 0), 2) ## +1/8 if term length is mentioned, +0 if not

## No indeterminate values to code

###################################
## jscore_2 -- Amendment Process ##
###################################

## This for loop breaks down the amendment process for each country and finds the number of branches required to amend the constitution
## This is accomplished through two indicators in the CCP data, AMNDAPPR (amendment approval) and AMNDPROP (amendment proposal)
## General logic is as follows:
##      If executive proposes and legislative approves: 2/3
##      If executive approves and legislative proposes: 2/3
##      If executive/legislative approves and proposes: 1/3
##
## In order to score, the constitution MUST require that a majority be necessary to amend. If this provision is not included, the sub-indicator is autmoatically scored a 0
##
## CCP variable guide:
##      amndappr/prop_1: Head of State
##      amndappr/prop_2: Head of Government
##      amndappr/prop_3: The Government/Cabinet
##      amndappr/prop_4: First (or only) Chamber of the Legislature
##      amndappr/prop_5: Second Chamber of the Legislature
##      amndappr/prop_6: Both Chambers of the Legislature
##
for(i in 1:nrow(master_table)){
  if(ccp_data[i,"amndamaj"] == 1){
    if((ccp_data[i,"amndappr_1"] == 1 | ccp_data[i,"amndappr_2"] == 1 | ccp_data[i,"amndappr_3"] == 1) & (ccp_data[i,"amndappr_4"] == 1 | ccp_data[i,"amndappr_5"] == 1| ccp_data[i,"amndappr_6"] == 1)){
      master_table[i,"jscore_2"] <- 0.67
    }
    else if((ccp_data[i,"amndprop_1"] == 1 | ccp_data[i,"amndprop_2"] == 1 | ccp_data[i,"amndprop_3"] == 1) & !(ccp_data[i,"amndprop_4"] == 1 | ccp_data[i,"amndprop_5"] == 1 | ccp_data[i,"amndprop_6"] == 1) & (ccp_data[i,"amndappr_4"] == 1 | ccp_data[i,"amndappr_5"] == 1 | ccp_data[i,"amndappr_6"] == 1)){
      master_table[i,"jscore_2"] <- 0.67
    }
    else if((ccp_data[i,"amndprop_4"] == 1 | ccp_data[i,"amndprop_5"] == 1 | ccp_data[i,"amndprop_6"] == 1) & !(ccp_data[i,"amndprop_1"] == 1 | ccp_data[i,"amndprop_2"] == 1 | ccp_data[i,"amndprop_3"] == 1) & (ccp_data[i,"amndappr_1"] == 1 | ccp_data[i,"amndappr_2"] == 1 | ccp_data[i,"amndappr_3"] == 1)){
      master_table[i,"jscore_2"] <- 0.67 
    }
    else if((ccp_data[i,"amndappr_1"] == 1 | ccp_data[i,"amndappr_2"] == 1 | ccp_data[i,"amndappr_3"] == 1 | ccp_data[i,"amndappr_4"] | ccp_data[i,"amndappr_5"] | ccp_data[i,"amndappr_6"] == 1 | ccp_data[i,"amndappr_7"] == 1 | ccp_data[i,"amndappr_8"] == 1)){
      master_table[i,"jscore_2"] <- 0.33
    }
    else
    {
      master_table[i,"jscore_2"] <- 4
    }
    
  }
  else if(ccp_data[i,"amndamaj"] == 2 | ccp_data[i,"amndamaj"] == 99){
    master_table[i,"jscore_2"] <- 0
  }
}

## Manual coding for indeterminate values
master_table[master_table$country == "Argentina","jscore_2"] <- 0.33
master_table[master_table$country == "Bolivia","jscore_2"] <- 0.33
master_table[master_table$country == "Bosnia-Herzegovina","jscore_2"] <- 0.33
master_table[master_table$country == "German Federal Republic","jscore_2"] <- 0.33
master_table[master_table$country == "Malaysia","jscore_2"] <- 0.33
master_table[master_table$country == "Sweden","jscore_2"] <- 0.33
master_table[master_table$country == "Tanzania/Tananyika","jscore_2"] <- 0.33

###################################################
## jscore_3 -- Nomination/Appointment Procedures ##
###################################################

## The scoring table for this column is located above, on lines 237 - 242
## This for loop moves through each row of master_table and links to that country's CCP data
## Once that happens, it then pulls the nominatng and appointing bodies for the high court
## With these two bodies, it assigns a score for the sub-indicator according to the scoring table
##
## CCP variable guide:
##      supap/nom_1: Head of State
##      supap/nom_2: Head of Government
##      supap/nom_3: The Government/Cabinet
##      supap/nom_4: First (or only) Chamber of the Legislature
##      supap/nom_5: Second Chamber of the Legislature
##      supap/nom_6: Judicial Council/Commission
##      supap/nom_7: Judiciary (other than council/commission)
##
for(i in 1:nrow(master_table)){
  if(ccp_data[i,"supnom_1"] == 1 | ccp_data[i,"supnom_2"] == 1 | ccp_data[i,"supnom_3"] == 1){
    if(ccp_data[i,"supap_1"] == 1 | ccp_data[i,"supap_2"] == 1 | ccp_data[i,"supap_3"] == 1 | ccp_data[i, "supap_98"] == 1){
      master_table[i,"jscore_3"] <- 0
    }
    else if(ccp_data[i,"supap_4"] == 1 | ccp_data[i,"supap_5"] == 1){
      master_table[i,"jscore_3"] <- 1/3
    }
    else if(ccp_data[i,"supap_6"] == 1 | ccp_data[i,"supap_7"] == 1){
      master_table[i,"jscore_3"] <- 2/3
    }
  }
  else if(ccp_data[i,"supnom_4"] == 1 | ccp_data[i,"supnom_5"] == 1){
    if(ccp_data[i,"supap_1"] == 1 | ccp_data[i,"supap_2"] == 1 | ccp_data[i,"supap_3"] == 1){
      master_table[i,"jscore_3"] <- 1/3
    }
    else if(ccp_data[i,"supap_4"] == 1 | ccp_data[i,"supap_5"] == 1 | ccp_data[i, "supap_98"] == 1){
      master_table[i,"jscore_3"] <- 0
    }
    else if(ccp_data[i,"supap_6"] == 1 | ccp_data[i,"supap_7"] == 1){
      master_table[i,"jscore_3"] <- 2/3
    }
  }
  else if(ccp_data[i,"supnom_6"] == 1 | ccp_data[i,"supnom_7"] == 1){
    if(ccp_data[i,"supap_1"] == 1 | ccp_data[i,"supap_2"] == 1 | ccp_data[i,"supap_3"] == 1){
      master_table[i,"jscore_3"] <- 2/3
    }
    else if(ccp_data[i,"supap_4"] == 1 | ccp_data[i,"supap_5"] == 1){
      master_table[i,"jscore_3"] <- 2/3
    }
    else if(ccp_data[i,"supap_6"] == 1 | ccp_data[i,"supap_7"] == 1 | ccp_data["supap_98"] == 1){
      master_table[i,"jscore_3"] <- 1
    }
  }
}

## Coding for indeterminate data ##
master_table[master_table$country == "Algeria","jscore_3"] <- 0
master_table[master_table$country == "Benin","jscore_3"] <- 0.33
master_table[master_table$country == "Bolivia","jscore_3"] <- 0.67
master_table[master_table$country == "Burkina Faso (Upper Volta)","jscore_3"] <- 0.67
master_table[master_table$country == "China","jscore_3"] <- 0.33
master_table[master_table$country == "Colombia","jscore_3"] <- 1
master_table[master_table$country == "Costa Rica","jscore_3"] <- 0
master_table[master_table$country == "Cote D'Ivoire","jscore_3"] <- 1
master_table[master_table$country == "Croatia","jscore_3"] <- 0.67
master_table[master_table$country == "Dominican Republic","jscore_3"] <- 1
master_table[master_table$country == "Ecuador","jscore_3"] <- 1
master_table[master_table$country == "Egypt","jscore_3"] <- 0
master_table[master_table$country == "Estonia","jscore_3"] <- 0.67
master_table[master_table$country == "Finland","jscore_3"] <- 0
master_table[master_table$country == "France","jscore_3"] <- 1
master_table[master_table$country == "Gabon","jscore_3"] <- 0
master_table[master_table$country == "Greece","jscore_3"] <- 0.33
master_table[master_table$country == "Guatemala","jscore_3"] <- 0
master_table[master_table$country == "Iran (Persia)","jscore_3"] <- 1
master_table[master_table$country == "Japan","jscore_3"] <- 0.33
master_table[master_table$country == "Latvia","jscore_3"] <- 0
master_table[master_table$country == "Macedonia (Former Yugoslav Republic Of)","jscore_3"] <- 1
master_table[master_table$country == "Madagascar (Malagasy)","jscore_3"] <- 0.67
master_table[master_table$country == "Morocco","jscore_3"] <- 0
master_table[master_table$country == "Nepal","jscore_3"] <- 0.67
master_table[master_table$country == "New Zealand","jscore_3"] <- 0
master_table[master_table$country == "Pakistan","jscore_3"] <- 0.67
master_table[master_table$country == "Peru","jscore_3"] <- 1
master_table[master_table$country == "Portugal","jscore_3"] <- 0
master_table[master_table$country == "Romania","jscore_3"] <- 0
master_table[master_table$country == "Yugoslavia (Serbia)","jscore_3"] <- 0.67
master_table[master_table$country == "Spain","jscore_3"] <- 0
master_table[master_table$country == "Togo","jscore_3"] <- 0.67
master_table[master_table$country == "Tunisia","jscore_3"] <- 0.67
master_table[master_table$country == "Turkey/Ottoman Empire","jscore_3"] <- 1
master_table[master_table$country == "United Arab Emirates","jscore_3"] <- 0.33
master_table[master_table$country == "Uruguay","jscore_3"] <- 0.33
master_table[master_table$country == "Venezuela","jscore_3"] <- 0
master_table[master_table$country == "Vietnam, Democratic Republic Of","jscore_3"] <- 0.33

## Coding values not specified
master_table[master_table$country == "Bosnia-Herzegovina","jscore_3"] <- 0
master_table[master_table$country == "Canada","jscore_3"] <- 0
master_table[master_table$country == "Central African Republic","jscore_3"] <- 0.33
master_table[master_table$country == "Congo, Democratic Republic Of (Zaire)","jscore_3"] <- 0
master_table[master_table$country == "Denmark","jscore_3"] <- 0
master_table[master_table$country == "Italy/Sardinia","jscore_3"] <- 1
master_table[master_table$country == "Jordan","jscore_3"] <- 0
master_table[master_table$country == "Kuwait","jscore_3"] <- 0
master_table[master_table$country == "Norway","jscore_3"] <- 0
master_table[master_table$country == "Oman","jscore_3"] <- 0
master_table[master_table$country == "Sweden","jscore_3"] <- 0
master_table[master_table$country == "Syria","jscore_3"] <- 0

## Round all values (1/3, 2/3) to two digits
master_table$jscore_3 <- sapply(master_table$jscore_3, FUN = round, digits = 2)


#####################################################
## jscore_4 -- Length of Term/Mandatory Retirement ##
#####################################################

## The scoring table for this column is located above, on lines 244 - 245
## Term lengths are specified in the CCP data, whereas mandatory retirement ages are not
## Thus, for constitutions with term lengths, this for loop pulls the length of the term from the supterm column in ccp_data
## It then assigns the value of the sub-indicator to the column in master_table based on the scoring table
for(i in 1:nrow(master_table)){
  if(ccp_data[i, "supterm"] < 4 | ccp_data[i, "supterm"] == 98 | ccp_data[i, "supterm"] == 99){
    master_table[i,"jscore_4"] <- 0
  }
  else if(ccp_data[i, "supterm"] >= 4 & ccp_data[i, "supterm"] < 6){
    master_table[i,"jscore_4"] <- 0.2
  }
  else if(ccp_data[i, "supterm"] >= 6 & ccp_data[i, "supterm"] < 8){
    master_table[i,"jscore_4"] <- 0.4
  }
  else if(ccp_data[i, "supterm"] >= 8 & ccp_data[i, "supterm"] < 10){
    master_table[i,"jscore_4"] <- 0.6
  }
  else if(ccp_data[i, "supterm"] >= 10 & ccp_data[i, "supterm"] < 12){
    master_table[i,"jscore_4"] <- 0.8
  }
  else if((ccp_data[i, "supterm"] >= 12 & ccp_data[i, "supterm"] <= 15) | ccp_data[i, "supterm"] == 89){
    master_table[i,"jscore_4"] <- 1.0
  }
  else{
    master_table[i,"jscore_4"] <- 5
  }
}

## Coding for mandatory retirement ages ##
master_table[master_table$country == "Australia", "jscore_4"] <- 0.8
master_table[master_table$country == "Bangladesh", "jscore_4"] <- 0.8
master_table[master_table$country == "Botswana", "jscore_4"] <- 0.8
master_table[master_table$country == "Bulgaria", "jscore_4"] <- 0.8
master_table[master_table$country == "Chile", "jscore_4"] <- 1.0
master_table[master_table$country == "India", "jscore_4"] <- 1.0
master_table[master_table$country == "Kyrgyz Republic", "jscore_4"] <- 0.8
master_table[master_table$country == "Nepal", "jscore_4"] <- 0.8
master_table[master_table$country == "Paraguay", "jscore_4"] <- 1.0
master_table[master_table$country == "Philippines", "jscore_4"] <- 0.8
master_table[master_table$country == "Sierra Leone", "jscore_4"] <- 0.8
master_table[master_table$country == "Singapore", "jscore_4"] <- 0.8
master_table[master_table$country == "Uganda", "jscore_4"] <- 0.6

## Coding for indeterminate values
master_table[master_table$country == "Rwanda", "jscore_4"] <- 0.6
master_table[master_table$country == "South Africa", "jscore_4"] <- 0.6

##################################
## jscore_5 -- Judge Reelection ##
##################################

master_table$jscore_5 <- ifelse(ccp_data$suptermn == 1, 0, 1) ## +1 if judges can be reelected or hold indefinite terms, +0 if not

## No indeterminate values to code ##

###############################
## jscore_6 -- Removing Body ##
###############################

## This for loop breaks down the judicial removal process for each country
## This is accomplished through two indicators in the CCP data, JREM and JREMAP
## General logic is as follows:
##      If judicial branch or judicial committee sole participants: 1
##      If executive/legislative/judicial joint participation: 1/2
##      If executive or legislative by itself: 0
##
## CCP variable guide:
##      jremap_1: Head of State
##      jremap_2: Head of Government
##      jremap_3: The Government/Cabinet
##      jremap_4: First (or only) Chamber of the Legislature
##      jremap_5: Second Chamber of the Legislature
##      jremap_6: Both Chambers of the Legislature
##      jremap_7: Public Prosecutor
##
for(i in 1:nrow(master_table)){
  if(ccp_data[i,"jrem"] == 2){
    master_table[i,"jscore_6"] <- 1
  }
  else if(ccp_data[i,"jrem"] == 96 | ccp_data[i,"jrem"] == 90 | ccp_data[i,"jremap_90"] == 1 | ccp_data[i,"jremap_98"] == 1){
    master_table[i,"jscore_6"] <- 0
  }
  else if(ccp_data[i,"jrem"] == 1){
    if((ccp_data[i,"jremap_1"] == 1 | ccp_data[i,"jremap_2"] == 1 | ccp_data[i,"jremap_3"] == 1) & (ccp_data[i,"jremap_4"] == 1 | ccp_data[i,"jremap_5"] == 1 | ccp_data[i,"jremap_6"] == 1)){
      master_table[i,"jscore_6"] <- 0.5
    }
    else if(ccp_data[i,"jrempro_8"] == 1 & (ccp_data[i,"jremap_1"] == 1 | ccp_data[i,"jremap_2"] == 1 | ccp_data[i,"jremap_3"] == 1 | ccp_data[i,"jremap_4"] == 1 | ccp_data[i,"jremap_5"] == 1 | ccp_data[i,"jremap_6"] == 1)){
      master_table[i,"jscore_6"] <- 0.5
    }
    else if((ccp_data[i,"jremap_1"] == 1 | ccp_data[i,"jremap_2"] == 1 | ccp_data[i,"jremap_3"] == 1) | (ccp_data[i,"jremap_4"] == 1 | ccp_data[i,"jremap_5"] == 1 | ccp_data[i,"jremap_6"] == 1)){
      master_table[i,"jscore_6"] <- 0
    }
    else{
      
    }
  }
}

## Coding for indeterminate values ##
master_table[master_table$country == "Albania", "jscore_6"] <- 1
master_table[master_table$country == "Argentina", "jscore_6"] <- 0
master_table[master_table$country == "Botswana", "jscore_6"] <- 0.5
master_table[master_table$country == "Brazil", "jscore_6"] <- 0
master_table[master_table$country == "Chile", "jscore_6"] <- 1
master_table[master_table$country == "China", "jscore_6"] <- 0
master_table[master_table$country == "Costa Rica", "jscore_6"] <- 1
master_table[master_table$country == "Croatia", "jscore_6"] <- 1
master_table[master_table$country == "Cyprus", "jscore_6"] <- 1
master_table[master_table$country == "Ethiopia", "jscore_6"] <- 0.5
master_table[master_table$country == "German Federal Republic", "jscore_6"] <- 1
master_table[master_table$country == "Ghana", "jscore_6"] <- 0.5
master_table[master_table$country == "Guyana", "jscore_6"] <- 0.5
master_table[master_table$country == "Honduras", "jscore_6"] <- 0
master_table[master_table$country == "Italy/Sardinia", "jscore_6"] <- 1
master_table[master_table$country == "Korea, Republic Of", "jscore_6"] <- 1
master_table[master_table$country == "Malawi", "jscore_6"] <- 1
master_table[master_table$country == "Mongolia", "jscore_6"] <- 1
master_table[master_table$country == "Nepal", "jscore_6"] <- 0
master_table[master_table$country == "Peru", "jscore_6"] <- 1
master_table[master_table$country == "Philippines", "jscore_6"] <- 0
master_table[master_table$country == "Poland", "jscore_6"] <- 1
master_table[master_table$country == "Ukraine", "jscore_6"] <- 0

##################################
## jscore_7 -- Income Reduction ##
##################################

master_table$jscore_7 <- ifelse(ccp_data$judsal == 1, 1, 0) ## +1 if salaries protected from gov. intervention, +0 if not

## Coding for indeterminate values ##
master_table[master_table$country == "Belgium", "jscore_7"] <- 0
master_table[master_table$country == "Bolivia", "jscore_7"] <- 1
master_table[master_table$country == "Ecuador", "jscore_7"] <- 1
master_table[master_table$country == "El Salvador", "jscore_7"] <- 1
master_table[master_table$country == "German Federal Republic", "jscore_7"] <- 0
master_table[master_table$country == "Greece", "jscore_7"] <- 0
master_table[master_table$country == "Nicaragua", "jscore_7"] <- 1
master_table[master_table$country == "Panama", "jscore_7"] <- 1
master_table[master_table$country == "Thailand", "jscore_7"] <- 0
master_table[master_table$country == "United Kingdom", "jscore_7"] <- 0

#################################
## jscore_8 -- Judicial Access ##
#################################

master_table$jscore_8 <- ifelse(ccp_data$challeg_8 == 1, 1, 0) ## +1 if individuals have open access, +0 if not

## Coding for indeterminate values ##
master_table[master_table$country == "Dominican Republic", "jscore_8"] <- 1
master_table[master_table$country == "Ethiopia", "jscore_8"] <- 1
master_table[master_table$country == "Guinea-Bissau", "jscore_8"] <- 1
master_table[master_table$country == "Kuwait", "jscore_8"] <- 1
master_table[master_table$country == "Mozambique", "jscore_8"] <- 1
master_table[master_table$country == "Rwanda", "jscore_8"] <- 1

#######################################
## jscore_9 -- Constitutional Review ##
#######################################

master_table$jscore_9 <- ifelse(ccp_data$interp_1 == 1 | ccp_data$interp_2 == 1 | ccp_data$interp_3 == 1 | ccp_data$interp_4 == 1, 1, 0) ## +1 if ordinary courts, supreme court, or constitutional court have power of review, +0 if not

## Coding for indeterminate values ##
master_table[master_table$country == "China", "jscore_9"] <- 0
master_table[master_table$country == "Costa Rica", "jscore_9"] <- 0.5
master_table[master_table$country == "Finland", "jscore_9"] <- 0
master_table[master_table$country == "Jamaica", "jscore_9"] <- 1
master_table[master_table$country == "Mauritius", "jscore_9"] <- 0
master_table[master_table$country == "Namibia", "jscore_9"] <- 1
master_table[master_table$country == "Nigeria", "jscore_9"] <- 1
master_table[master_table$country == "Rwanda", "jscore_9"] <- 0
master_table[master_table$country == "Vietnam, Democratic Republic Of", "jscore_9"] <- 0

######################################
## jscore_10 -- Decision Publishing ##
######################################

## Add together weighted scores of three CCP indicators:
##      hocopw_1: requires published decision
##      hocopw_2: allows separate/concurring opinions
##      hocopw_4: allows dissenting opinions
##
## In order to score, hocop (which is a binary based on whether or not decisions are mentioned in the constitution) MUST be 1, or else the score is 0
master_table$jscore_10 <- ifelse(ccp_data$hocop == 1, round((1/3) * ccp_data$hocopw_1 + (1/3) * ccp_data$hocopw_2 + (1/3) * ccp_data$hocopw_4, 2), 0)

## Coding for indeterminate values ##
master_table[master_table$country == "Sri Lanka (Ceylon)", "jscore_10"] <- 0.33

##########################################
## Compute jscore as mean of components ##
##########################################

## Set the jud_pow indicator score to the sum of jscore_1 through jscore_10 and divide by 10 to give the average of all 10 components
master_table <- master_table %>% 
  mutate(jud_pow = (select(., jscore_1:jscore_10) %>% rowSums(na.rm = TRUE)) / 10)
## Round the final indicator score to three digits
master_table$jud_pow <- round(master_table$jud_pow, digits = 3)

#######################################################
## executive power indicator calculations start here ##
#######################################################
##
## sub-indices:
## escore_1: The power to appoint cabinet ministers
## escore_2: The power to appoint supreme court judges
## escore_3: The power to appoint ordinary judges
## escore_4: The power to appoint attorney general
## escore_5: The power to appoint central bank chief
## escore_6: The power to appoint security council
## escore_7: The power to remand legislation
## escore_8: The power to issue decrees (non-emergency)
## escore_9: Assumption of emergency powers
##
## 
## The sub-index is computed as the arithmetic mean of the 10 sub-indices
########################################################################
########################################################################
##
## All indices are scored on the following scale:
## 1 - Power is held exclusively by the executive
## 0.5 - Power is shared between the executive and at least 1 other branch
## 0 - Power is not held by the executive
##
########################################################################
########################################################################

###################################
## escore_1 -- Cabinet Ministers ##
###################################

## CCP variable guide:
##      cabappt/appr_1: Head of State nominates/approves cabinet
##      cabappt/appr_2: Head of Government nominates/approves cabinet
##
for(i in 1:nrow(master_table))
{
  if((ccp_data[i,"cabappt_1"] == 1 | ccp_data[i,"cabappt_2"] == 1) & (ccp_data[i,"cabappr_1"] == 1 | ccp_data[i,"cabappr_2"] == 1)){
    master_table[i,"escore_1"] = 1
  }
  else if((ccp_data[i,"cabappt_1"] == 1 | ccp_data[i,"cabappt_2"] == 1)){
    master_table[i,"escore_1"] = 0.5
  }
  else if((ccp_data[i,"cabappr_1"] == 1 | ccp_data[i,"cabappr_2"] == 1)){
    master_table[i,"escore_1"] = 0.5
  }
  else{
    master_table[i,"escore_1"] = 0
  }
}

############################################
## escore_2 -- Supreme Court Appointments ##
############################################

## CCP variable guide:
##      supap/nom_1: Head of State
##      supap/nom_2: Head of Government
##      supap/nom_3: The Government/Cabinet
##      supap/nom_98: Unspecified
##
for(i in 1:nrow(master_table))
{
  if((ccp_data[i,"supnom_1"] == 1 | ccp_data[i,"supnom_2"] == 1 | ccp_data[i,"supnom_3"] == 1) & (ccp_data[i,"supap_1"] == 1 | ccp_data[i,"supap_2"] == 1 | ccp_data[i,"supap_3"] == 1 | ccp_data[i,"supap_98"] == 1)){
    master_table[i,"escore_2"] = 1
  }
  else if((ccp_data[i,"supnom_1"] == 1 | ccp_data[i,"supnom_2"] == 1 | ccp_data[i,"supnom_3"] == 1 | ccp_data[i,"supnom_98"] == 1) & (ccp_data[i,"supap_1"] == 1 | ccp_data[i,"supap_2"] == 1 | ccp_data[i,"supap_3"] == 1)){
    master_table[i,"escore_2"] = 1
  }
  else if((ccp_data[i,"supnom_1"] == 1 | ccp_data[i,"supnom_2"] == 1 | ccp_data[i,"supnom_3"] == 1)){
    master_table[i,"escore_2"] = 0.5
  }
  else if((ccp_data[i,"supap_1"] == 1 | ccp_data[i,"supap_2"] == 1 | ccp_data[i,"supap_3"] == 1)){
    master_table[i,"escore_2"] = 0.5
  }
  else{
    master_table[i,"escore_2"] = 0
  }
}

#############################################
## escore_3 -- Ordinary Court Appointments ##
#############################################

## CCP variable guide:
##      ordap/nom_1: Head of State
##      ordap/nom_2: Head of Government
##      ordap/nom_3: The Government/Cabinet
##      ordap/nom_98: Unspecified
##
for(i in 1:nrow(master_table))
{
  if((ccp_data[i,"ordnom_1"] == 1 | ccp_data[i,"ordnom_2"] == 1 | ccp_data[i,"ordnom_3"] == 1) & (ccp_data[i,"ordap_1"] == 1 | ccp_data[i,"ordap_2"] == 1 | ccp_data[i,"ordap_3"] == 1 | ccp_data[i,"ordap_98"] == 1)){
    master_table[i,"escore_3"] = 1
  }
  else if((ccp_data[i,"ordnom_1"] == 1 | ccp_data[i,"ordnom_2"] == 1 | ccp_data[i,"ordnom_3"] == 1 | ccp_data[i,"ordnom_98"] == 1) & (ccp_data[i,"ordap_1"] == 1 | ccp_data[i,"ordap_2"] == 1 | ccp_data[i,"ordap_3"] == 1)){
    master_table[i,"escore_3"] = 1
  }
  else if((ccp_data[i,"ordnom_1"] == 1 | ccp_data[i,"ordnom_2"] == 1 | ccp_data[i,"ordnom_3"] == 1)){
    master_table[i,"escore_3"] = 0.5
  }
  else if((ccp_data[i,"ordap_1"] == 1 | ccp_data[i,"ordap_2"] == 1 | ccp_data[i,"ordap_3"] == 1)){
    master_table[i,"escore_3"] = 0.5
  }
  else{
    master_table[i,"escore_3"] = 0
  }
}

###############################################
## escore_4 -- Attorney General Appointments ##
###############################################

## CCP variable guide:
##      agap/nom_1: Head of State
##      agap/nom_2: Head of Government
##      agap/nom_3: The Government/Cabinet
##      agap/nom_98: Unspecified
##
for(i in 1:nrow(master_table))
{
  if((ccp_data[i,"agnom_1"] == 1 | ccp_data[i,"agnom_2"] == 1 | ccp_data[i,"agnom_3"] == 1) & (ccp_data[i,"agap_1"] == 1 | ccp_data[i,"agap_2"] == 1 | ccp_data[i,"agap_3"] == 1 | ccp_data[i,"agap_98"] == 1)){
    master_table[i,"escore_4"] = 1
  }
  else if((ccp_data[i,"agnom_1"] == 1 | ccp_data[i,"agnom_2"] == 1 | ccp_data[i,"agnom_3"] == 1 | ccp_data[i,"agnom_98"] == 1) & (ccp_data[i,"agap_1"] == 1 | ccp_data[i,"agap_2"] == 1 | ccp_data[i,"agap_3"] == 1)){
    master_table[i,"escore_4"] = 1
  }
  else if((ccp_data[i,"agnom_1"] == 1 | ccp_data[i,"agnom_2"] == 1 | ccp_data[i,"agnom_3"] == 1)){
    master_table[i,"escore_4"] = 0.5
  }
  else if((ccp_data[i,"agap_1"] == 1 | ccp_data[i,"agap_2"] == 1 | ccp_data[i,"agap_3"] == 1)){
    master_table[i,"escore_4"] = 0.5
  }
  else{
    master_table[i,"escore_4"] = 0
  }
}

###################################################
## escore_5 -- Executive Bank Chief Appointments ##
###################################################

## NOTE: This variable assigned by a manually entered dataset based on text provided by the Constitute Project

## Load in ex_bank.csv and assign the values to the corresponding column in master_table
master_table$escore_5 <- (read.csv("./Data/ex_bank.csv"))$ex_bank

#########################################################
## escore_6 -- National Security Council Appointmentes ##
#########################################################

## NOTE: This variable assigned by a manually entered dataset based on text provided by the Constitute Project

## Load in nat_sec.csv and assign the values to the corresponding column in master_table
master_table$escore_6 <- read.csv("./Data/nat_sec.csv")$nat_sec

####################################
## escore_7 -- Legislation Remand ##
####################################

## CCP Variable Guide:
##      legapp: which body can approve/veto legislation
##      legapppt: can vetoing actor veto whole or part
##
for(i in 1:nrow(master_table)){
  if(ccp_data[i,"legapp"] <= 4 & ccp_data[i,"legapppt"] <= 3){
    master_table[i,"escore_7"] <- 1
  }
  else{
    master_table[i,"escore_7"] <- 0
  }
}

## Indeterminate Values
master_table[master_table$country == "Bulgaria", "escore_7"] <- 1
master_table[master_table$country == "Cyprus", "escore_7"] <- 1

###############################################
## escore_8 -- Issue Decrees (Non-emergency) ##
###############################################

## CCP Variable Guide:
##      hosdec: Head of State can issue decrees
##      hosdecim: How are HOS decrees implemented?
##      hogdec: Head of Government can issue decrees
##      hogdecim: How are HOG decrees implemented?
##
for(i in 1:nrow(master_table)){
  if((ccp_data[i,"hosdec"] == 1 | ccp_data[i,"hogdec"] == 1) & (ccp_data[i,"hosdecim"] == 1 | ccp_data[i,"hogdecim"] == 1 | ccp_data[i,"hosdecim"] == 98 | ccp_data[i,"hogdecim"] == 98)){
    master_table[i,"escore_8"] <- 1
  }
  else if((ccp_data[i,"hosdec"] == 1 | ccp_data[i,"hogdec"] == 1) & (ccp_data[i,"hosdecim"] == 2 | ccp_data[i,"hogdecim"] == 2 | ccp_data[i,"hosdecim"] == 3 | ccp_data[i,"hogdecim"] == 3 | ccp_data[i,"hosdecim"] == 90 | ccp_data[i,"hogdecim"] == 90)){
    master_table[i,"escore_8"] <- 0.5
  }
  else if((ccp_data[i,"hosdec"] >= 2 & ccp_data[i,"hogdec"] >= 2) & (ccp_data[i,"hosdec"] != 96 & ccp_data[i,"hogdec"] != 96)){
    master_table[i,"escore_8"] <- 0
  }
}

## Code for indeterminate values
master_table[master_table$country == "Burkina Faso (Upper Volta)","escore_8"] <- 1
master_table[master_table$country == "Burundi","escore_8"] <- 1
master_table[master_table$country == "Central African Republic","escore_8"] <- 0.5
master_table[master_table$country == "Ecuador","escore_8"] <- 0.5
master_table[master_table$country == "Gabon","escore_8"] <- 1
master_table[master_table$country == "Italy/Sardinia","escore_8"] <- 1
master_table[master_table$country == "Kyrgyz Republic","escore_8"] <- 0.5
master_table[master_table$country == "Moldova","escore_8"] <- 1
master_table[master_table$country == "Norway","escore_8"] <- 1
master_table[master_table$country == "Romania","escore_8"] <- 1
master_table[master_table$country == "Thailand","escore_8"] <- 0.5

##################################
## escore_9 -- Emergency Powers ##
##################################

## CCP Variable Guide:
##      em: Constitution mentions state of emergency
##      emdecl: Who can declare states of emergency
##
for(i in 1:nrow(master_table)){
  if(ccp_data[i,"em"] == 1){
    if(ccp_data[i,"emdecl"] <= 4){
      master_table[i,"escore_9"] <- 1
    }
    else if(ccp_data[i,"emdecl"] != 96){
      master_table[i,"escore_9"] <- 0
    }
  }
  else if(ccp_data[i,"em"] == 2){
    master_table[i,"escore_9"] <- 0
  }
}

## Code for indeterminate values
master_table[master_table$country == "Argentina","escore_9"] <- 0.5
master_table[master_table$country == "Chad","escore_9"] <- 0.5
master_table[master_table$country == "El Salvador","escore_9"] <- 0.5
master_table[master_table$country == "Ireland","escore_9"] <- 0
master_table[master_table$country == "Macedonia (Former Yugoslav Republic Of)","escore_9"] <- 0.5
master_table[master_table$country == "Mauritius","escore_9"] <- 0.5
master_table[master_table$country == "Mongolia","escore_9"] <- 0.5
master_table[master_table$country == "Paraguay","escore_9"] <- 0.5
master_table[master_table$country == "United Kingdom","escore_9"] <- 0

##########################################
## Compute escore as mean of components ##
##########################################

## Set the ex_pow indicator score to the sum of escore_1 through escore_9 and divide by 9 to give the average of all 9 components
master_table$ex_pow <- ((select(master_table, escore_1, escore_2, escore_3, escore_4, escore_5, escore_6, escore_7, escore_8, escore_9) %>% rowSums(na.rm = TRUE)) / 9)
## Round the final indicator score to three digits
master_table$ex_pow <- round(master_table$ex_pow, digits = 3)

###########################
## SPI_index calculation ##
###########################

## Convert all sub-indices to their "relative" counterparts,
## calculated as the branch power divided by the share of total power
master_table$leg_share <- master_table$leg_pow / (master_table$leg_pow + master_table$ex_pow + master_table$jud_pow)
master_table$ex_share <- master_table$ex_pow / (master_table$leg_pow + master_table$ex_pow + master_table$jud_pow)
master_table$jud_share <- master_table$jud_pow / (master_table$leg_pow + master_table$ex_pow + master_table$jud_pow)

## Calclate the Separate Powers Index using the formula based on Gallagher and used in the final paper
master_table$SPI_index <- round((1.5 * (master_table$ex_share^2 + master_table$leg_share^2 + master_table$jud_share^2 - (1/3))) ^ (0.5), digits = 3)

##################################
## Control Variables start here ##
##################################

########################
## Constitutional Age ##
########################

## Read in conAge.csv, which contains constitutional year data taken from the Comparative Constitutions Project
## Sort the countries alphabetically, and filter for only the countries in the analysis
## Finally, assign these values to a new object called conYear
conYear <- read.csv("./Data/conAge.csv",
                    header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM") %>% arrange(country) %>%
  filter(country %in% case_names$x)

## Take the year the constitution was implemented and subtract it from 2008 (the year of analysis).
## Next, assign this to the con_age column in master_table
master_table <- master_table %>% mutate(con_age = 2008 - conYear$year)

###########################
## Military Expenditures ##
###########################

## Read in WDI_military_expend.csv, which contains military expenditure data as % of GDP taken from the World Bank's World Development Indicators
## Assign the dataframe to a new object called military_spending
military_spending = read.csv("./Data/WDI_military_expend.csv",
                             header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM", na.string = "..")

## Sort the countries alphabetically, and filter for only the countries in the analysis
military_spending <- military_spending %>% arrange(country) %>% filter(country %in% case_names$x)

## Next, assign the column for the year 2008 to the mil_spend column in master_table
master_table <- master_table %>%
  mutate(mil_spend = military_spending$milSpending_2008)

## Missing Values
## Haiti: Military Expenditures for 2008 provided by SIPRI Yearbook 2011 (included in military expenditure CSV)
master_table[master_table$country == "Gabon", "mil_spend"] = military_spending[military_spending$country == "Gabon", "milSpending_2007"]
## Gabon: Military Expenditures for 2007 provided by SIPRI Yearbook 2011
master_table[master_table$country == "Guinea-Bissau", "mil_spend"] = military_spending[military_spending$country == "Guinea-Bissau", "milSpending_2009"]
## Guinea-Bissau: Military Expenditures for 2009 provided by SIPRI Yearbook 2011


#####################
## National Income ##
#####################

## Read in WDI_national_income.csv, which contains national income data in constant 2010 USD taken from the World Bank's World Development Indicators
## Assign the dataframe to a new object called national_income
national_income <- read.csv("./Data/WDI_national_income.csv",
                            header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM", na.strings = "..")

## Sort the countries alphabetically, and filter for only the countries in the analysis
## Next, assign the column for the year 2008 to the nat_inc column in master_table
master_table <- master_table %>%
  mutate(nat_inc = (national_income %>%
                      filter(country %in% case_names$x) %>% arrange(country))$income2008)

## Missing Values
## Syria: National Income for 2007 provided by World Bank DataBank
master_table[master_table$country == "Syria", "nat_inc"] <- national_income[national_income$country == "Syria", "income2007"]

#####################
## Gender Equality ##
#####################

## Read in UNHDR_gender.csv, which contains gender equality data taken from the United Nations' Human Development Reports
gender_equality <- read.csv("./Data/UNHDR_gender.csv",
                            header = TRUE, stringsAsFactors = FALSE)

## Sort the countries alphabetically, and filter for only the countries in the analysis
gender_equality <- gender_equality %>% arrange(country) %>% filter(country %in% case_names$x)

## Next, assign the column for the year 2010 to the gender_development and gender_inequality columns in master_table
master_table <- master_table %>% 
  mutate(gender_development = gender_equality$gdi_2010, gender_inequality = gender_equality$gii_2010)

#############################
## British Colonial Status ##
#############################

## Read in cepii_data.csv, which contains historical colonization data taken from the Centre d'Etudes Prospectives et d'Informations Internationales
cepii_data = read.csv("./Data/cepii_data.csv",
                      header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")

## Sort the countries alphabetically, and filter for only the countries in the analysis
cepii_data <- cepii_data %>% arrange(country) %>% filter(cap == 1, country %in% case_names$x)

## Next, set the brit_col variable to 1 for any country whose most recent colonizer was Great Britain, and 0 for all other countries
master_table <- master_table %>%
  mutate(brit_col = as.numeric((cepii_data$colonizer1 == "GBR")))

##############
## Religion ##
##############

## Read in religion_data.csv, which contains data on religious adherents in each country taken from the Correlates of War
religion_data = read.csv("./Data/religion_data.csv",
                         header = TRUE, stringsAsFactors = FALSE)

## Sort the countries alphabetically, and filter for the year 2005 and only the countries in the analysis
religion_data <- religion_data %>% arrange(country) %>% filter(year == 2005, country %in% case_names$x)

## Calculate the total percentage of religious adherents by subtracting the percentage of nonreligious persons from 100%
religion_data <- religion_data %>% mutate(religiousPercent = 1 - nonreligpct)

## Set the value of the columns chrst_pct, islm_pct, jud_pct, and bud_pct to the percentages of Christianity, Islam, Judaism, and Buddhism, respectively
master_table <- master_table %>%
  mutate(chrst_pct = religion_data$chrstgenpct, 
         islm_pct = religion_data$islmgenpct, 
         jud_pct = religion_data$judgenpct, 
         bud_pct = religion_data$budgenpct)

## Set the value of the rel_dom variable in master_table to 1 if any of the religions has more than 80% adherents and 0 if not
master_table <- master_table %>% mutate(rel_dom = (master_table$chrst_pct > 0.8 | master_table$islm_pct > 0.8 | master_table$jud_pct > 0.8 | master_table$bud_pct > 0.8))

##############
## Conflict ##
##############

## Read in MIDIP_4.01.csv, which contains data on military conflicts around the world taken from the Correlates of War
conflict_data = read.csv("./Data/MIDIP_4.01.csv",
                         header = TRUE, stringsAsFactors = FALSE)

## Sort the countries alphabetically, and filter for years between 2006 - 2008 and only the countries in the analysis
conflict_data <- conflict_data %>% filter(StYear %in% 2006:2008, country %in% case_names$x) %>% arrange(country)

## Join the two tables together by their countries, and fill the mil_events variable equal to the length (number of conflicts) of IncidNum3 variable
master_table <- left_join(master_table, (conflict_data %>% group_by(country) %>% summarize(mil_events = length(IncidNum3))))

## For any country without military conflicts, set mil_events to 0
master_table[which(is.na(master_table$mil_events)), "mil_events"] <- 0

###########################
## Constitutional Rights ##
###########################

## Read in ccp_rights_list.csv, which contains a vector corresponding to the name of variables in the CCP dataset that are categorized as human rights
## Next, assign this vector to a new object called rights_list
rights_list = read.csv("./Data/ccp_rights_list.csv",
                       header = TRUE, stringsAsFactors = FALSE)

## Initialize every country's number of rights to 0
master_table[,"num_rights"] <- 0

## For each right, check if it's listed as a 1 (in the constitution) or 0 (not in the constitution)
## If it is present in that country's constitution, increment the number of rights by one
## Continue until it reaches the end of the rights list
for(i in rights_list$right){
  master_table[,"num_rights"] <- master_table[,"num_rights"] + (ccp_data[,i] == 1)
}

## Initialize every country's freedom of expression binary variable to 0
master_table[,"dummy_freexp"] <- 0

## For each country, if the constitution includes a free expression clause, set dummy_freexp to 1; if not, set it to 0
for(i in 1:nrow(master_table)){
  master_table[i,"dummy_freexp"] <- (ccp_data[i,"express"] == 1)
}

################
## Gini Index ##
################

## Read in swiid_data08.csv, which contains income inequality data for the year 2008 taken from the Standardized World Income Inequality Database
## Next, assign the data to a new object called swiid_data
swiid_data <-read.csv("./Data/swiid_data08.csv")

## Sort the countries in alphabetical order, and filter for the countries in the analysis
swiid_data <- swiid_data %>% filter(country %in% case_names$x) %>% arrange(country)

## Assign the gini index column (gini_disp) from swiid_data to the gini_index column in master_table
master_table <- master_table %>% mutate(gini_index = swiid_data$gini_disp)

###################
## Polity Scores ##
###################

## Read in p4v_2008.csv, which contains numerous governance indicators for the year 2008 taken from the Polity Project
## Next, assign the data to a new object called p4v_2008
p4v_2008 <- read.csv("./Data/p4v_2008.csv",
                     header = TRUE, stringsAsFactors = FALSE)

## Sort the countries alphabetically, and filter for the countries in the analysis
p4v_2008 <- p4v_2008 %>% filter(country %in% case_names$x) %>% arrange(country)

## Assign the polity score to the polity column in master_table
master_table <- master_table %>% mutate(polity = p4v_2008$polity)

###############################
## Continent Dummy Variables ##
###############################

## Assign two geographic binary variables, dummy_eu and dummy_am, based on whether the country's continent is listed as Europe or America, respectively
master_table <- master_table %>% mutate(dummy_eu = as.numeric(continent == "Europe"), dummy_am = as.numeric(continent == "Americas"))

###################
## Write to file ##
###################

## Take the completed master_table generated by this script and save it to the ./Data/ directory
write.csv(master_table, "./Data/master_table.csv")
