rm(list = ls()) # clear global environment
graphics.off() # close all graphics
if (require(pacman) == FALSE) install.packages("pacman")

#load and install packages using pacman
pacman::p_load(AUC, Biocomb, car, caret, conflicted, DataExplorer, # important for exploratory data analysis 
               dataPreparation, data.table, DT, haven, magrittr, mltools, party, readxl, tidyverse, # important analytic packages
                snow, varImp
)

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
library(ggplot2)
library(gridExtra)


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
dcs_dnr <- read.delim(file_path_dcs_dnr, sep = "\t", header=FALSE, na.strings=c('.', "NA", "NULL", ""))  #deceased_donor
colnames(dcs_dnr) <- labels1

liver <- read.delim(file_path_liver, sep = "\t", header=TRUE, na.strings=c('.', "NA", "NULL", "")) # nolint  #liver
colnames(liver) <- labels2

#show the size of the data
dim(liver)
#show the data
head(liver)

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

'''  
selecting observations with transplant date after 2000
temp_new <- subset(temp, year(date_time) > 2000)
dim(temp_new)
'''
# reading the documentation excel file
xls_path <- '/Users/kyimoetun/Downloads/UNOS/IMPORTANT DOCUMENTATION/STAR File Documentation.xlsx' # nolint
varInfo <- read_excel(xls_path, sheet = "LIVER DATA", skip =1,  # nolint
                        col_types = c(rep("text", 3),
                        rep("date", 2), rep("guess", 7)))

# CMV_DON seems to have two start dates, 01-Oct-87 and 01-Oct-90
# We will only use 01-Oct-90 for the start date.
varInfo$`VAR START DATE`[51] <- "1990-10-01"
#any variable having end date will be removed
selected_rows <- is.na(varInfo$`VAR END DATE`)
#the reason we are doing this is because those variables
#will not be included in future data
#there are 43 variables having VAR END DATE
vars_to_include <- varInfo[selected_rows, 1]
# Extract the column as a vector
cols_to_include <- as.vector(vars_to_include$'VARIABLE NAME')
# Select only columns with names included in the vector list
temp_df <- dplyr::select(temp, which(names(temp) %in% cols_to_include))


# Compute proportion of missing values for each column
missing_prop <- colSums(is.na(temp_df)) / nrow(temp_df)
# Create a table with the variable names and the proportion of missing values
missing_table <- tibble(
  variables = names(temp_df),
  percent_missing = round(colSums(is.na(temp_df)) / nrow(temp_df) * 100, 2)
) %>% 
  arrange(desc(percent_missing))
# Get column names with missing values greater than 40 percent
remove_cols <- names(which(missing_prop > 0.9))
# Remove columns from dataframe
temp_df <- temp_df %>% select(-all_of(remove_cols))


# factoring the categorical variables
temp_df$CITIZENSHIP <-  as.factor(temp_df$CITIZENSHIP)
temp_df$CITIZENSHIP <-  factor(temp_df$CITIZENSHIP, levels = c(1, 2, 3, 4, 5))
temp_df$CITIZENSHIP <- factor(temp_df$CITIZENSHIP ,
                                labels = c("US Citizen", "Resident Alien",
                                "Non-Resident Alien Specify Country",
                                "Non-US Citizen/US Resident",
                                "Non-US Citizen/Non-US Resident"))

temp_df$CITIZENSHIP_DON <-  as.factor(temp_df$CITIZENSHIP_DON)
temp_df$CITIZENSHIP_DON <-  factor(temp_df$CITIZENSHIP_DON, levels = c(1, 2, 3, 4, 5, 6))
temp_df$CITIZENSHIP_DON <- factor(temp_df$CITIZENSHIP_DON, 
                                labels =  c("US Citizen", "RESIDENT ALIEN",
                                "NON-RESIDENT ALIEN, Year Entered US",
                                "Non-US Citizen/US Resident",
                                "Non-US Citizen/Non-US Resident, Traveled to US for Reason Other Than Transplant",
                                "Non-US Citizen/Non-US Resident, Traveled to US for Transplant"))
                               
# we will only use one weight variable for both donor and recipient at the time of transplant
# removing recipient weight at registration
temp_df <- subset(temp_df, select = -WGT_KG_TCR)
# removing candidate weight at listing
temp_df <- subset(temp_df, select = -INIT_WGT_KG)
# do the same for height
# removing recipient height at registration
temp_df <- subset(temp_df, select = -HGT_CM_TCR)
# removing candidate height at listing
temp_df <- subset(temp_df, select = -INIT_HGT_CM)

# remove extreme values in weight and height
temp_df <- temp_df %>%
  dplyr::filter(WGT_KG_DON_CALC >= quantile(WGT_KG_DON_CALC, 0.0001, na.rm = TRUE),
         WGT_KG_CALC >= quantile(WGT_KG_CALC, 0.0001, na.rm = TRUE),
         HGT_CM_DON_CALC >= quantile(HGT_CM_DON_CALC, 0.0001, na.rm = TRUE),
         HGT_CM_CALC >= quantile(HGT_CM_CALC, 0.0001, na.rm = TRUE))


# Create boxplots of weight and heights
# create a side-by-side boxplot of the numerical variables
# for weights
p1 <- ggplot() +
        geom_boxplot(aes(y=temp_df$WGT_KG_DON_CALC), width = 0.4, fill = "lightgrey") +
        labs(x = " Donor Weight", y = "Weight (kg)") +
        scale_y_continuous(limits=c(0,NA)) +
        theme_bw() 
p2 <- ggplot() +
        geom_boxplot(aes(y = temp_df$WGT_KG_CALC), width=0.4,fill="blue", alpha = 0.5)+
        labs(x = " Recipient Weight", y = "Weight (kg)") +
        scale_y_continuous(limits=c(0,NA)) +
        theme_bw() 
# Combine the two plots side by side using grid.arrange
grid.arrange(p1, p2, ncol = 2, top="Distribution of donor and recipient weights")

# for heights
p3 <- ggplot() +
        geom_boxplot(aes(y=temp_df$HGT_CM_DON_CALC), width = 0.4, fill = "lightgrey") +
        labs(x = " Donor Height", y = "Height (cm)") +
        scale_y_continuous(limits=c(0,NA)) +
        theme_bw() 
p4 <- ggplot() +
        geom_boxplot(aes(y = temp_df$HGT_CM_CALC), width=0.4,fill="blue", alpha = 0.5)+
        labs(x = " Recipient Height", y = "Height (cm)") +
        scale_y_continuous(limits=c(0,NA)) +
        theme_bw() 
# Combine the two plots side by side using grid.arrange
grid.arrange(p3, p4, ncol = 2, top="Distribution of donor and recipient heights")


count <- sum(temp_df$WGT_KG_DON_CALC > 200)
print(count)



# Calculate percentage of each category in every column
col_pct <- apply(temp_df, 2, function(x) max(table(x)) / length(x))
# Drop variables containing more than 90% of observations belong to one category
temp_df_filter <- temp_df[, which(col_pct < 0.9)]
# Print filtered data for reference
print(temp_df_filter)
# Using sapply and is.factor to count the number of categorical columns
cat_cols_count <- sum(sapply(temp_df_filter, is.factor))
# Displaying the result
cat_cols_count





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



# create a dummy dataframe with one numeric and one factor variable
kk <- data.frame(
  col1 = c(1, 2, 3, NA),
  col2 = factor(c("a", "b", "c", ""))
)

# loop through columns and convert factor variables
for (j in colnames(kk)) {
  if (is.factor(kk[, j])) {
    kk[, j] <- factor(kk[, j], exclude = c(NA, " ", ""))
  }
}

# check updated dataframe
str(kk)
