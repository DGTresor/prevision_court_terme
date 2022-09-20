# Title     : loaders for realtime ipi
# Created by: lphung
# Created on: 12/09/2022

# packages -------------------------------------------------------------------------------------------------------------
library(readxl)
library(stringr)
library(dplyr)
library(tidyr)
library(lubridate)
source("./code/realtime_ipi/loader_fundamentals.R", encoding = "utf-8")

# loaders --------------------------------------------------------------------------------------------------------------
## Note: the loaders are made to be independent so if another format of excel files appear, such create a new loader

sectors_in_line_one_label_loader <- function(file_path, data_correction = "CJO-CVS") {
  # get the name of the sheet we want to use according to the data_correction we selected
  check_data_correction(data_correction)
  sheet_name <- stringr::str_subset(string = readxl::excel_sheets(file_path),
                                    pattern = paste0(".*", data_correction, ".*"))
  # load the excel file
  new_data <- readxl::read_excel(path = file_path, sheet = sheet_name)
  # clean the data to fit a proper syntax
  clean_data <- data_cleaner_for_sectors_in_line(new_data)
  return(clean_data)
}


sectors_in_line_two_labels_loader <- function(file_path, data_correction = "CJO-CVS") {
  # get the name of the sheet we want to use according to the data_correction we selected
  check_data_correction(data_correction)
  sheet_name <- stringr::str_subset(string = readxl::excel_sheets(file_path),
                                    pattern = paste0(".*", data_correction, ".*"))
  # load the excel file
  new_data <- readxl::read_excel(path = file_path, sheet = sheet_name)
  # clean the data to fit a proper syntax
  clean_data <- data_cleaner_for_sectors_in_line(new_data[, -1])   # remove the first column that contains the labels in english
  return(clean_data)
}

generic_loader <- function(file_path, data_correction = "CJO-CVS") {
  # get the name of the sheet we want to use according to the data_correction we selected
  check_data_correction(data_correction)
  sheet_name <- stringr::str_subset(string = readxl::excel_sheets(file_path),
                                    pattern = paste0(".*", data_correction, ".*"))
  # load the excel file
  new_data <- readxl::read_excel(path = file_path, sheet = sheet_name)
  # clean the data to fit a proper syntax

  clean_data <- data_cleaner_for_generic_loader(new_data)
  return(clean_data)
}


# data cleaners for specific format ------------------------------------------------------------------------------------
data_cleaner_for_sectors_in_line <- function(data) {
  ## rename the first column
  ## Note: in this format, the first column contains the industry sectors
  clean_data <- data
  colnames(clean_data)[1] <- "dimension"
  ## change the sector's id: keep only the code
  regex_pattern <- "([:alpha:]|[:digit:]){2,3}(?=[:space:]|\\)|\\])"                                  # use the regex pattern you need to clean the name of your dimensions, if it is not clean
  clean_data <- clean_data %>%
    dplyr::mutate(dimension = stringr::str_extract(clean_data$dimension, pattern = regex_pattern))
  ## pivot the data
  columns_to_pivot <- colnames(clean_data)[colnames(clean_data) != "dimension"]
  clean_data <- clean_data %>%
    tidyr::pivot_longer(cols = dplyr::all_of(columns_to_pivot),
                        names_to = "date",
                        values_to = "value")
  ## put the date in date format
  ## Note: the date values only contain the year and the month and sometimes additional characters so (1) we need to only extract the numbers and (2) add "01" for the day
  clean_data <- clean_data %>%
    dplyr::mutate(date = lubridate::ymd(paste0(stringr::str_extract(date, pattern = "[:digit:]{6}"), "01")))
  ## reorganise the dataframe
  clean_data <- clean_data %>%
    dplyr::select(date, dimension, value)
  return(clean_data)
}

data_cleaner_for_generic_loader <- function(data) {
  ## rename the first column
  ## Note: in this format, the first column contains the dates
  clean_data <- data
  colnames(clean_data)[1] <- "date"
  ## remove the industry sectors we do not need
  ## Note: in this format, each column contains the value for a specific industry sector
  clean_data <- clean_data %>%
    dplyr::select(-contains("RM0"))
  ## change the sector's id: keep only the code
  regex_pattern <- "([:alpha:]|[:digit:]){2,3}(?=[:space:]|\\)|\\]|$)"                                  # use the regex pattern you need to clean the name of your dimensions, if it is not clean
  colnames(clean_data)[-1] <- stringr::str_extract(colnames(clean_data)[-1], pattern = regex_pattern)  # we remove the first column's name that contains "date"
  ## put the date column in date format
  ## Note: the date values only contain the year and the month and sometimes additional characters so (1) we need to extract the numbers only and (2) add "01" for the day
  clean_data <- clean_data %>%
    dplyr::mutate(date = lubridate::ymd(paste0(stringr::str_extract(date, pattern = "[:digit:]{6}"), "01")))
  ## pivot the data
  columns_to_pivot <- colnames(clean_data)[colnames(clean_data) != "date"]
  clean_data <- clean_data %>%
    tidyr::pivot_longer(cols = dplyr::all_of(columns_to_pivot),
                        names_to = "dimension",
                        values_to = "value")
  ## reorganise the dataframe
  clean_data <- clean_data %>%
    dplyr::select(date, dimension, value)
  return(clean_data)
}