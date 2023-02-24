library(dplyr)
library(tidyverse)
library(survival)
library(haven)
library(readr)
library(XML)
library(rvest)


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
dim(liver)
ncol(liver)

#show the data
head(dcs_dnr)


