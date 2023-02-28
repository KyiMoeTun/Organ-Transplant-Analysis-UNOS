
rm(list = ls()) # clear global environment
graphics.off() # close all graphics
if (require(pacman) == FALSE) install.packages("pacman")

#load and install packages using pacman
#p_load()

#directory for the custom functions
source("https://raw.githubusercontent.com/Ying-Ju/heart_transplant.github.io/master/custom_functions.R") # nolint: line_length_linter.


library(dplyr)
library(tidyverse)
library(survival)
library(haven)
library(readr)
library(XML)
library(rvest)
library(tibble)


# GETTING THE COLUMN NAMES FROM Liver DATA

# Set the path to the HTML file
html_path1 <- "/Users/kyimoetun/Downloads/UNOS/Copy of Delimited Text File 202112/Deceased Donor/DECEASED_DONOR_DATA.html" # nolint

# Read the HTML content
html_content1 <- read_html(html_path1)

# Extract the column labels
labels1 <- character()
for (row in html_nodes(html_nodes(html_content1, "table"), "tr")[-1]) {
  label <- html_nodes(row, "td")[1] %>% html_text()
  labels1 <- c(labels1, label)
}

# Print the number of labels
print(length(labels1))


# GETTING THE COLUMN NAMES FROM DECEASED DONOR DATA
html_path2 <- "/Users/kyimoetun/Downloads/UNOS/Copy of Delimited Text File 202112/Liver/LIVER_DATA.html" # nolint

# Read the HTML content
html_content2 <- read_html(html_path2)

# Extract the column labels
labels2 <- character()
for (row in html_nodes(html_nodes(html_content2, "table"), "tr")[-1]) {
  label <- html_nodes(row, "td")[1] %>% html_text()
  labels2 <- c(labels2, label)
}

# Print the number of labels
print(length(labels2))




#path for the data files
file_path_dcs_dnr <- "/Users/kyimoetun/Downloads/UNOS/Copy of Delimited Text File 202112/Deceased Donor/DECEASED_DONOR_DATA.DAT" # nolint: line_length_linter.
file_path_liver <- "/Users/kyimoetun/Downloads/UNOS/Copy of Delimited Text File 202112/Liver/LIVER_DATA.DAT" # nolint: line_length_linter.

# Read in the data

dcs_dnr <- read.delim(file_path_dcs_dnr, sep = "\t", header=FALSE, na.strings=c('.', "NA", "NULL", "")) # nolint #deceased_donor
colnames(dcs_dnr) <- labels1

liver <- read.delim(file_path_liver, sep = "\t", header=TRUE, na.strings=c('.', "NA", "NULL", "")) # nolint  #liver
colnames(liver) <- labels2

#show the size of the data
dim(dcs_dnr)
ncol(liver)

#show the data
head(dcs_dnr)



#assigning the row names to the column for tracking train/test purposes
liver$ID <- row.names(liver)

# Use sapply() and class() to get the data types of each column
data_types <- sapply(liver, class)

#getting the count of data types
table(data_types)

### INCLUSION EXCLUSION CRITERIA
#filtering to keep only the adult patients
temp <- liver %>% subset(AGE >= 18)  %>% subset(AGE_DON >= 18)

#removing the observations with misssing graft status and graft time
temp %<>% dplyr::filter(!is.na(GSTATUS) & !is.na(GTIME))

#check if transplant date column has missing values
sum(is.na(temp$TX_DATE))

# Convert TX_DATE column in temp dataframe to POSIXct format
temp$date_time <- as.POSIXct(temp$TX_DATE, format = "%m/%d/%Y")
class(temp$date_time)

#selecting observations with transplant date after 2000
temp_new <- subset(temp, year(date_time) > 2000)
dim(temp_new)


# Compute proportion of missing values for each column
missing_prop <- colSums(is.na(temp)) / nrow(temp)

# Create a table with the variable names and the proportion of missing values
missing_table <- tibble(
  variables = names(temp),
  percent_missing = round(colSums(is.na(temp)) / nrow(temp) * 100, 2)
) %>% 
  arrange(desc(percent_missing))

# Get column names with missing values greater than 40 percent
remove_cols <- names(which(missing_prop > 0.4))

#there were two start dates in the data, so I decided to use the first one
#liver$`VAR START DATE`[51] <- "1990-10-01"




# Remove columns from the dataset
liver_filtered <- liver[, !(names(liver) %in% remove_cols)]

table(liver$ENCEPH_TX)

table(liver_filtered$ENCEPH_TX)


sum(is.na(liver$COD))

#getting the count of unique values in the column
table(liver$COD)

table(dcs_dnr$COD_CAD_DON)
liver$PTIME


