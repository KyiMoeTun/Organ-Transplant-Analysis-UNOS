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

#select the columns we need
liver_data <- liver %>% select("ALBUMIN_TX","ASCITES_TX","BW4","BW6","C1","C2", "CREAT_TX", # nolint
          "DQ1","DQ2","DR51","DR51_2","DR52", "DR52_2","DR53","DR53_2","ENCEPH_TX", "FINAL_ALBUMIN","FINAL_ASCITES", # nolint
          "FINAL_BILIRUBIN", "FINAL_CTP_SCORE","FINAL_DIALYSIS_PRIOR_WEEK", "FINAL_ENCEPH","FINAL_INR","FINAL_MELD_OR_PELD",  # nolint
          "FINAL_MELD_PELD_LAB_SCORE","FINAL_SERUM_CREAT", "FINAL_SERUM_SODIUM", "INIT_ALBUMIN","INIT_ASCITES", "INIT_BILIRUBIN", # nolint
          "INIT_CTP_SCORE", "INIT_MELD_PELD_LAB_SCORE",
          "INIT_SERUM_CREAT",
          "INIT_SERUM_SODIUM",
          "INR_TX",
          "NUM_PREV_TX",
          "REM_CD", "TBILI_TX", "TRR_ID_CODE")

#show the dataframe
head(liver_data)

table(liver$REM_CD)

# Compute proportion of missing values for each column
missing_prop <- colSums(is.na(liver)) / nrow(liver)

# Create a table with the variable names and the proportion of missing values
missing_table <- tibble(
  variables = names(liver),
  percent_missing = round(colSums(is.na(liver)) / nrow(liver) * 100, 2)
) %>% 
  arrange(desc(percent_missing))



# Get column names with missing values greater than 40 percent
remove_cols <- names(which(missing_prop > 0.4))

# Remove columns from the dataset
liver_filtered <- liver[, !(names(liver) %in% remove_cols)]

table(liver$ENCEPH_TX)

table(liver_filtered$ENCEPH_TX)
