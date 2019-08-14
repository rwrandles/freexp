###############################
## Loading required packages ##
###############################

library(ggplot2) ## contains functions for producing visualizations
library(dplyr) ## contains functions for data cleaning/manipulation
library(tidyr) ## contains functions for data cleaning/manipulation
library(maps) ## contains data and functions for polygons to draw maps

## Set working directory to the location of the master replication folder
setwd(" ")

#########################################################################################################
## Preliminaries -- This section MUST be run in order for other parts of the code to function properly ##
#########################################################################################################

## Load master_table.csv, containinng the data frame produced by data_processing.R in the ./Code/ directory
## Contains all compiled data from the ./Data/ directory, which is used to produce all of the figures in the final paper
master_table <- read.csv("./data/master_table.csv",
                         header = TRUE, stringsAsFactors = FALSE)

## Load multivar_model.RData, which contains the multivariate OLS model generated and output by models.R
load(file = "./data/multivar_model.RData")

## Set the theme for following figures to theme_minmal(). This theme includes no background shading, no axis lines, and an overall minimalist theme.
theme_set(theme_minimal())

## All of the figures in the paper follow the same color scheme and formatting. The color palette was provideed by ColorBrewer
## The hex codes for the colors used in the palette are:
##      #4A1486, 
##      #6A51A3, 
##      #807DBA, 
##      #9E9AC8, 
##      #BCBDDC, 
##      #DADAEB, 
##      #F2F0F7
##
## The URL to the ColorBrewer page for this palette is:
## http://colorbrewer2.org/#type=sequential&scheme=Purples&n=7
##
## General figure theme guidelines:
##      Axis titles font size = 18.0
##      Axis text font size = 12.0
##      Legend title font size = 18.0
##      Legend text font size = 12.0
##      Histogram bin alpha = 0.6
##      CI ribbon alpha = 0.4
##
## Plot fills colored as #9E9AC8
## Plot outlines (color argument) colored as #4A1486

###################################
## Dependent Variable Histograms ##
###################################

## Create ggplot objects for both the normal and log-transformed dependent variables

vdem_distribution <- ggplot(master_table[-c(86,122)], aes(x = freedom_vdem)) + ## ggplot object for normal DV. Remove rows 86 and 122, as they are not part of the final anlysis (countries with unwritten constitutions)
  geom_histogram(fill = "#9E9AC8", color = "#4A1486", alpha = 0.6) + ## create the histogram layer, with standard fill, color, and alpha
  labs(x = "V-Dem Free Expression", y = "Count (No. of Countries)") + ## add X and Y axis labels
  theme(axis.title = element_text(size = 18.0), 
        axis.text = element_text(size = 12.0), 
        legend.title = element_text(size = 18.0), 
        legend.text = element_text(size = 12.0)) ## add standard theme objects (setting font sizes for axes and legend)

vdem_log_distribution <- ggplot(master_table[-c(86,122)], aes(x = log_vdem)) + ## ggplot object for log-transformed DV. Remove rows 86 and 122, as they are not part of the final analysis (countries with unwritten constitutions)
  geom_histogram(fill = "#9E9AC8", color = "#4A1486", alpha = 0.6) + ## create the histogram layer, with standard fill, color, and alpha
  labs(x = "V-Dem Free Expression", y = "Count (No. of Countries)") + ## add X and Y axis labels
  theme(axis.title = element_text(size = 18.0), 
        axis.text = element_text(size = 12.0), 
        legend.title = element_text(size = 18.0), 
        legend.text = element_text(size = 12.0)) ## add standard theme objects (setting font sizes for axes and legend)

####################################
## Independent Variable World Map ##
####################################

## Create a new data frame, which is an identical copy of master_table, with rows 86 and 122 removed, as they are not part of the final analysis (countries with unwritten constitutions)
map_table <- master_table[-c(86,122),]
## Read in a file from the ./Data/ directory called "countries_map.csv"
## This file contains the vector of country names as they are used in the maps package
## Set this vector of countries equal to the map_table country variable to ensure that all spellings match those used in the maps package
map_table$country <- (read.csv("./Data/countries_map.csv",
                             header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")$x)

## Create a dataframe called map.world, which holds polygon information from the maps package for drawing the world map, except for Antarctice and Greenland
map.world <- map_data("world") %>% filter(!(region %in% c("Antarctica", "Greenland")))

## Create a dataframe called map.world_joined
## This object is a join between the map.world polygon dataframe and the map_table dataframe
## Essentially, this step is the one that is "applying" the data to the polygons to be drawn
map.world_joined <- left_join(map.world, map_table, by = c("region" = "country"))

## Create a second map object called map.borders
## This is so that the two maps can be layerd on top of one another, one that is filled based on the data, and a second that is white with only black borders
map.borders <- geom_polygon(data = map.world, aes(x = long, y = lat, group = group), size = 0.15, colour = "white", fill = NA)

map.spi <- ggplot() + ## create a new ggplot object to hold both map.world_joined and map.borders
  geom_polygon(data = map.world_joined, aes(x = long, y = lat, group = group, fill = SPI_index)) + ## draw the polygons in map.world_joined, and set the fill according to the index value from the map_table dataframe
  scale_fill_gradientn(colors = c("#4A1486", "#6A51A3", "#807DBA", "#9E9AC8", "#BCBDDC", "#DADAEB", "#F2F0F7"),
                       name = "Separate Powers Index", 
                       limits = c(0,1), 
                       breaks = seq(0,1,0.2))  + ## create the legend, with a manual gradient given by the standard color palette, a legend title, min and max values of 0 and 1, and ticks located every increment of 0.2
  theme_void() + ## set the theme for following figures to theme_void(). This theme includes no axes, no labels, and no legends
  map.borders + ## add the map.borders object as an overlay
  theme(legend.position = "bottom", legend.title = element_text(size = 16.0)) + ## set the theme properties, which inlucdes moving the legend to the bottom of the map, and setting its text size to 16.0
  guides(fill = guide_colorbar(title.position = "top", 
                               barwidth = 20, 
                               barheight = 0.8, 
                               nbin = 100, 
                               title.hjust = 0.5, 
                               frame.colour = "black")) + ## manually set the properties of the legend color bar; move the title to the top, manually set width and height, adjust the resoution using nbin, center the bar, and apply a black border
  coord_fixed(1.3) ## manually set the dimensions of the graph where width = 1.3 * height

#######################################################
## Hypothesis and Predictive Analysis Visualizations ##
#######################################################

## Use expand.grid to create a dataframe of hypothetical scenarios
## This dataframe contains all of the IVs used in the multivariate model
## All control variables are set at their mean values (with the exception of dummy_freexp), and the main IV is adjusted in each case, moving from 0 to 0.5 in increments of 0.02
## This creates a data frame that is 26 x 7

test_cases <- expand.grid(SPI_index = seq(0,0.5,0.02),
                          dummy_freexp = 1,
                          mil_spend = mean(master_table$mil_spend), 
                          nat_inc = mean(master_table$nat_inc), 
                          mil_events = mean(master_table$mil_events),
                          dummy_eu = mean(master_table$dummy_eu),
                          dummy_am = mean(master_table$dummy_am))

## Run a predictive model using the predict() function
## What this function does is takes all 26 observations in test_cases and inputs their values into the multivariate model
## It returns the predicted value of the DV, along with a 95% confidence interval
pred_results <- predict(multivar_model, newdata = test_cases, interval = "confidence")

## Because the DV in the regression model is log-transformed, this performs the inverse of the transform function used in models.R
## This will turn all predicted values from their log-transformed state back into terms of the original freedom index
trans_results <- -1 * exp(pred_results) + 1


pred_graph <- ggplot(bind_cols(test_cases, as.data.frame(trans_results)), aes(x = SPI_index, y = fit, ymin = lwr, ymax = upr)) + ## create a ggplot object based on the combined dataframes from expand.grid() and predict(), setting the X-axis to the IV, and Y-axis to the predicted value of the DV, along with ymin and ymax values for the 95% CI
  geom_line(color = "#4A1486", size = 1.2) + ## create a line plot, setting the color to the standard value and adjusting its thickness
  geom_ribbon(alpha = 0.4, fill = "#9E9AC8") + ## create a ribbon for the 95% CI, setting the fill and alpha to the standard values
  labs(x = "Separate Powers Index", y = "Predicted level of Free Expression", title = "") + ## set the axis titles
  theme_minimal() + ## set the theme to theme_minimal()
  scale_x_continuous(breaks = c(0,0.2,0.4)) + ## adjust the X-axis scale, with breaks at 0, 0.2, and 0.4
  lims(y = c(0.5,1)) + ## set the min and max values of the Y-axis to 0.5 and 1, respectively
  theme(axis.title = element_text(size = 18.0), axis.text = element_text(size = 12.0)) ## set the axis title and text to their standard font sizes

## Use expand.grid to create a dataframe of hypothetical scenarios
## This dataframe contains all of the IVs used in the multivariate model
## All control variables are set at their mean values, and the main IV is set at the min and max values in the dataset
## This creates a data frame that is 2 x 7

predict <- data.frame(SPI_index = c(min(master_table$SPI_index), max(master_table$SPI_index)),
                      dummy_freexp = mean(master_table$dummy_freexp),
                      dummy_eu = mean(master_table$dummy_eu),
                      dummy_am = mean(master_table$dummy_am),
                      mil_spend = mean(master_table$mil_spend),
                      nat_inc = mean(master_table$nat_inc),
                      mil_events = mean(master_table$mil_events))

## Run a predictive model using the predict() function
## What this function does is takes both observations in predict and inputs their values into the multivariate model
## It returns the predicted value of the DV, along with a 95% confidence interval
results <- predict(multivar_model, newdata = predict, interval = "confidence")

## Because the DV in the regression model is log-transformed, this performs the inverse of the transform function used in models.R
## This will turn all predicted values from their log-transformed state back into terms of the original freedom index
trans <- -1 * exp(results) + 1

## Create a new dataframe that binds the expand.grid() and predict() datasets together
case_table <- bind_cols(predict, as.data.frame(trans))

case_graph <- ggplot(case_table, aes(x = factor(SPI_index), y = fit, ymin = lwr, ymax = upr)) + ## create a ggplot object based on the combined dataframes from expand.grid() and predict(), setting the X-axis to the IV, and Y-axis to the predicted value of the DV, along with ymin and ymax values for the 95% CI
  theme_minimal() + ## set the theme to theme_minimal()
  geom_col(fill = "#9E9AC8", color = "#4A1486", width = 0.5, alpha = 0.6) + ## create a column plot, where the X-axis distinguishes the two cases and the Y-axis displays the level of the DV
  geom_point() + ## add points on top of the columns
  geom_errorbar(aes(width = 0.3)) + ## create error bars for the 95% CI, and set their width
  scale_x_discrete(labels = c("Minimum", "Maximum")) + ## create a discrete X-axis, with labels for min and max values
  scale_y_continuous(breaks = seq(0,1,0.25), limits = c(0,1)) + ## create a continuous Y-axis, with breaks at increments of 0.25, and min and max values of 0 and 1, respectively
  labs(x = "Separate Powers Index", y = "Predicted Level of Free of Expression") + ## set axis titles
  geom_text(size = 6.0, x = c(1,2), y = c(0.95,0.95), label = c(paste("[", round(case_table$upr[1],3), ", ", round(case_table$lwr[1],3), "]", sep = ""), paste("[", round(case_table$upr[2],3), ", ", round(case_table$lwr[2],3), "]", sep = ""))) + ## create text objects to display the confidence intervals, taking the data from the lwr and upr columns of the dataframe; set their size and manually place them, along with text formatting using paste() in the form of "[lwr, upr]"
  theme(axis.title = element_text(size = 18.0), axis.text = element_text(size = 12.0), legend.title = element_text(size = 18.0), legend.text = element_text(size = 12.0)) ## set axis and legend text to their standard font sizes

## Use expand.grid to create a dataframe of hypothetical scenarios
## This dataframe contains all of the IVs used in the multivariate model
## All variables are set at their mean values except dummy_freexp, which is changed from 0 to 1 between the cases
## This creates a data frame that is 2 x 7

freexp_pred <- expand.grid(SPI_index = mean(master_table$SPI_index),
                           dummy_freexp = c(0,1),
                           dummy_eu = mean(master_table$dummy_eu),
                           dummy_am = mean(master_table$dummy_am),
                           mil_spend = mean(master_table$mil_spend),
                           nat_inc = mean(master_table$nat_inc),
                           mil_events = mean(master_table$mil_events))

## Run a predictive model using the predict() function
## What this function does is takes both observations in freexp_pred and inputs their values into the multivariate model
## It returns the predicted value of the DV, along with a 95% confidence interval
freexp_results <- predict(multivar_model, newdata = freexp_pred, interval = "confidence")

## Because the DV in the regression model is log-transformed, this performs the inverse of the transform function used in models.R
## This will turn all predicted values from their log-transformed state back into terms of the original freedom index
freexp_trans <- -1 * exp(freexp_results) + 1

## Create a new dataframe that binds the expand.grid() and predict() datasets together
freexp_cases <- bind_cols(freexp_pred, as.data.frame(freexp_trans))


freexp_graph <- ggplot(freexp_cases, aes(x = factor(dummy_freexp), y = fit, ymin = lwr, ymax = upr)) + ## create a ggplot object based on the combined dataframes from expand.grid() and predict(), setting the X-axis to the IV, and Y-axis to the predicted value of the DV, along with ymin and ymax values for the 95% CI
  theme_minimal() + ## set the theme to theme_minimal()
  geom_col(fill = "#9E9AC8", color = "#4A1486", width = 0.5, alpha = 0.6) + ## create a column plot, where the X-axis distinguishes the two cases and the Y-axis displays the level of the DV
  geom_point() + ## add points on top of the columns
  geom_errorbar(aes(width = 0.35)) + ## create error bars for the 95% CI, and set their width
  scale_x_discrete(labels = c("Without Provision", "With Provision")) + ## create a discrete X-axis, with labels for min and max values
  scale_y_continuous(breaks = seq(0,1,0.25), limits = c(0,1)) + ## create a continuous Y-axis, with breaks at increments of 0.25, and min and max values of 0 and 1, respectively
  labs(x = "Constitutional Provisions for Free Expression", y = "Predicted Level of Free Expression") + ## set axis titles
  geom_text(size = 6.0, x = c(1,2), y = c(0.95,0.95), label = c(paste("[", round(freexp_cases$upr[1],3), ", ", round(freexp_cases$lwr[1],3), "]", sep = ""), paste("[", round(freexp_cases$upr[2],3), ", ", round(freexp_cases$lwr[2],3), "]", sep = ""))) + ## create text objects to display the confidence intervals, taking the data from the lwr and upr columns of the dataframe; set their size and manually place them, along with text formatting using paste() in the form of "[lwr, upr]"
  theme(axis.title = element_text(size = 18.0), axis.text = element_text(size = 12.0), legend.title = element_text(size = 18.0), legend.text = element_text(size = 12.0)) ## set axis and legend text to their standard font sizes

#################
## Plot Saving ##
#################

## Save all figures created in this code to the ./Figures/ directory

ggsave("vdem_distribution.pdf", plot = vdem_distribution, path = "./figures/")
ggsave("vdem_log_distribution.pdf", plot = vdem_log_distribution, path = "./figures/")
ggsave("map_fullscale.pdf", plot = map.spi, path = "./figures/")
ggsave("model_results.pdf", plot = pred_graph, path = "./figures/")
ggsave("mean_pred.pdf", plot = case_graph, path = "./figures/")
ggsave("freexp_pred.pdf", plot = freexp_graph, path = "./figures/")
