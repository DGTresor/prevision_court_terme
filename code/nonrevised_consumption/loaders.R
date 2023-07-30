# Title     : loaders for nonrevised household consumption
# Created by: lphung
# Created on: 26/07/2023

# packages -------------------------------------------------------------------------------------------------------------
library(readxl)
library(stringr)
library(dplyr)
library(tidyr)
library(lubridate)
source("../loader_fundamentals.R", encoding = "utf-8")
# chdir = TRUE needed because we call this Rscript from the main.R and from a RMarkdown, which define working directory differently

# loaders --------------------------------------------------------------------------------------------------------------
## Note: the loaders are made to be independent so if another format of excel files appears, just create a new loader


consumption_loader_up_to_201103 <- function(file_path, dimensions_list) {
  # load the excel file
  suppressMessages(new_data <- readxl::read_excel(path = file_path, skip = 3)) # suppress messages to prevent message of columns' type

  # clean the data to fit a proper syntax
  clean_data <- data_cleaner_for_up_to_201103_loader(new_data, dimensions_list)
  return(clean_data)
}


consumption_loader_from_201104_xls <- function(file_path, dimensions_list) {
  # get the name of the sheet we want to use; it must contains "Niveaux" (sometimes it is written "Niveaux Levels" or "Niveaux - Levels")
  sheet_name <- stringr::str_subset(string = readxl::excel_sheets(file_path),
                                    pattern = paste0(".*", "Niveaux", ".*"))
  # load the excel file
  suppressMessages(new_data <- readxl::read_excel(path = file_path, sheet = sheet_name, skip = 5)) # suppress messages to prevent message of columns' type

  # clean the data to fit a proper syntax
  clean_data <- data_cleaner_for_from_201104_xls_loader(new_data, dimensions_list)
  return(clean_data)
}


consumption_generic_loader <- function(file_path, dimensions_list) {
  # get the name of the sheet we want to use; it must contains "Niveaux" (sometimes it is written "Niveaux Levels" or "Niveaux - Levels")
  sheet_name <- stringr::str_subset(string = readxl::excel_sheets(file_path),
                                    pattern = paste0(".*", "Niveaux", ".*"))
  # load the excel file
  suppressMessages(new_data <- readxl::read_excel(path = file_path, sheet = sheet_name, skip = 5)) # suppress messages to prevent message of columns' type

  # clean the data to fit a proper syntax
  clean_data <- data_cleaner_for_consumption_generic_loader(new_data, dimensions_list)
  return(clean_data)
}

# data cleaners for specific format ------------------------------------------------------------------------------------

data_cleaner_for_up_to_201103_loader <- function(data, dimensions_list) {
  ## rename the first column
  ## Note: in this format, the first column contains the dates
  clean_data <- data
  colnames(clean_data)[1] <- "date"

  ## keep only the columns we need
  clean_data <- clean_data %>%
    dplyr::select(date, starts_with(names(dimensions_list[["up_to_201103"]])))

  ## put the date column in date format
  clean_data <- clean_data %>%
    dplyr::mutate(date = as.Date(date))

  ## clean the dataframe
  # Sometimes the first lines contain labels in English, so no data -> we can remove all lines for which there is no dates
  # once removed, put the other columns in numeric format
  clean_data <- clean_data %>%
    dplyr::filter(!is.na(date)) %>%
    dplyr::mutate_if(is.character, as.numeric)

  ## pivot the data
  columns_to_pivot <- colnames(clean_data)[colnames(clean_data) != "date"]
  clean_data <- clean_data %>%
    tidyr::pivot_longer(cols = dplyr::all_of(columns_to_pivot),
                        names_to = "dimension",
                        values_to = "value")

  ## rename the dimensions
  clean_data <- clean_data %>%
    dplyr::mutate(dimension = mapper(dimension, dimensions_list[["up_to_201103"]]))

  ## reorganise the dataframe
  clean_data <- clean_data %>%
    dplyr::select(date, dimension, value)
  return(clean_data)
}


data_cleaner_for_consumption_generic_loader <- function(data, dimensions_list) {
  clean_data <- data
  ## remove the empty columns
  clean_data <- clean_data %>%
    dplyr::select(where(~any(!is.na(.))))

  ## rename properly all columns - step 1
  # Note: because of merged cells in Excel, the column names are not in the same line
  for (column_name in names(clean_data)) {
    # for columns that does not have a proper name
    if (stringr::str_detect(column_name, "^\\.\\.\\.")) {
      column_data <- clean_data[[column_name]][!is.na(clean_data[[column_name]])]
      names(clean_data)[names(clean_data) == column_name] <- column_data[1]   # the first non-missing value is the name of the column
    }
  }

  ## remove all lines containing the column names
  clean_data <- clean_data[6:nrow(clean_data),]

  ## rename the first column
  ## Note: in this format, the first column contains the dates
  colnames(clean_data)[1] <- "date"

  ## keep only the columns we need
  clean_data <- clean_data %>%
    dplyr::select(date, starts_with(names(dimensions_list[["generic"]])))

  ## put the date column in date format
  clean_data <- clean_data %>%
    dplyr::mutate(date = as.Date(as.numeric(date), origin = "1900-01-01") - days(2)) %>%
    dplyr::mutate_if(is.character, as.numeric)

  ## pivot the data
  columns_to_pivot <- colnames(clean_data)[colnames(clean_data) != "date"]
  clean_data <- clean_data %>%
    tidyr::pivot_longer(cols = dplyr::all_of(columns_to_pivot),
                        names_to = "dimension",
                        values_to = "value")

  ## rename the dimensions
  clean_data <- clean_data %>%
    dplyr::mutate(dimension = mapper(dimension, dimensions_list[["generic"]]))

  ## reorganise the dataframe
  clean_data <- clean_data %>%
    dplyr::select(date, dimension, value)
  return(clean_data)
}