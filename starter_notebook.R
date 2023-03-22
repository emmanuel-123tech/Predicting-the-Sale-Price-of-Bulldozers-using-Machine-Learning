# ======================================================
# ---------- STARTER NOTEBOOK: ZINDI CPI 2023 ----------
# ======================================================

# Step 1: Load the Libraries Required 
# A library or package in R is a collection of pre-built functions and/or datasets 
# They are designed to simplify and streamline specific tasks or analyses 
# If you do not have a library installed, you need to install it (below)
# install.packages('dplyr')
# once the package is installed, you can just call it (below)
library(dplyr)
library(magrittr)
library(readxl)
library(ggplot2)

# Step 2: Reading in the Data 
# change file path to where your data is stored
data <- read_excel("~/CPI_Historic_Values_Zindi.xlsx", 
                   col_types = c("text", "text", "numeric", 
                                 "numeric"))


# lets convert the date variable into a date format
# this will help R better understand the column
# It will allow us to do things like order by date 
data$Month <- as.Date(data$Month, format = '%d-%m-%Y')

# we can plot how the series has changed over time
# a popular plotting library in R is ggplot2

ggplot(data, aes(x = Month, y = Value, color = Category)) + 
  geom_line() + geom_point()

# the graph allows us to view how the different subcategories have moved over time
# For our first submission, we'll just average the percentage change over the 12 months
# This will give us the average monthly percentage change per subcategory 
# we will then add this back to the latest value for the Subcategories
average_change_subcategories <- data %>%
  # remove headline cpi -> we'll predict it by a weighted sum of subcategories later
  dplyr::filter(Category != 'Headline_CPI') %>%
  dplyr::group_by(Category) %>%
  # get the average monthly change in each subcategory
  dplyr::summarise(Average_Change = mean(`Percentage Change (From Prior Month)`))

# now we want to add this to the latest month (in this case January) 
# first we need to isolate January's CPI numbers
january_cpi_values <- data %>%
  dplyr::filter(Month == as.Date('2023-01-31')) %>%
  dplyr::mutate(January_Values = Value) %>%
  dplyr::select(Category, January_Values)

# we are going to join the january values to the average change in subcategories
# this will allow us to consolidate information 
january_cpi_values <- dplyr::inner_join(january_cpi_values, 
                                        average_change_subcategories, 
                                        by = 'Category')


# lets calculate future month values 
# remember the objective of the exercise is to predict April Values 
# lets round off the average change to 2 decimal places
january_cpi_values$Average_Change <- round(january_cpi_values$Average_Change, 2)
# now lets predict Feb, March and April 
cpi_predictions <- january_cpi_values %>%
  dplyr::mutate(February_Predictions = January_Values + Average_Change,
                March_Predictions = February_Predictions + Average_Change,
                April_Predictions = March_Predictions + Average_Change
                )

# you can view the dataframe to see how the monthly predictions evolve
# however, we are mostly interested in Aprils prediction for submission purposes
# so lets filter
cpi_predictions <- cpi_predictions %>%
  dplyr::select(Category, April_Predictions)

# now we could make predictions for headline cpi
# or we could take our predictions for the subcategories and weight those
# this will give us a weighted sum prediction for headline cpi
# we'll get our weights from the last CPI release
categories <- c("Food and non-alcoholic beverages", "Alcoholic beverages and tobacco",
                "Clothing and footwear", "Housing and utilities",
                "Household contents and services", "Health", "Transport",
                "Communication", "Recreation and culture", "Education",
                "Restaurants and hotels", "Miscellaneous goods and services")

weights <- c(0.1714, 0.0626, 0.0365, 0.2449, 0.0437, 0.0144, 0.1435, 0.0242,
             0.052, 0.0262, 0.0325, 0.1481)

# creating a weighted dataframe 
subweights <- data.frame(cbind(categories, weights))
# change the column names so we can join them back together 
colnames(subweights) <- c('Category', 'Weight')
# remove some of the data we are no longer using 
rm(categories, weights)

# lets calculate headline cpi 
Headline_CPI <- dplyr::inner_join(cpi_predictions, subweights, by = 'Category') %>%
  dplyr::mutate(
    Weight = as.numeric(Weight),
    Weighted_Sum = April_Predictions * Weight) %>%
  dplyr::summarise(Headline_CPI = sum(Weighted_Sum))

Headline_CPI <- data.frame(cbind("headline CPI", 108.68))
# change the column names
colnames(Headline_CPI) <- c("ID", "Value")

# now lets get the data ready for a submission 
# we need to get the names in the same format as Zindi requires
# this coverts the text to lowercase
cpi_predictions$Category <- tolower(cpi_predictions$Category)
colnames(cpi_predictions) <- c("ID", "Value")

# lets add the headline cpi value to these subcategories
cpi_predictions <- rbind(Headline_CPI, cpi_predictions)

# now we need to add the month name to the ID variable
cpi_predictions$ID <- paste("April_", cpi_predictions$ID, sep = '')

# we can now write this to a csv and its ready to be scored on the zindi leaderboard
write.csv(cpi_predictions, file = 'first_submission.csv', row.names = F)

