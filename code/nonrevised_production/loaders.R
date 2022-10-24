# Title     : loaders for nonrevised production from the national accounts
# Created by: lphung
# Created on: 12/10/2022

# packages -------------------------------------------------------------------------------------------------------------
library(readxl)
library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(lubridate)
source("../loader_fundamentals.R", encoding = "utf-8", chdir = TRUE)
# chdir = TRUE needed because we call this Rscript from the main.R and from a RMarkdown, which define working directory differently

# constants ------------------------------------------------------------------------------------------------------------
# /!\ Note: pre_2010 classification applies to 2010 quarters and before; for 2010, and before, the classification is different. So, we decide not to use data before 2011.
# Note: .xls, .cvs or .RData files are available only from base2000 on, i.e. starting from the 2007T4PE account.
# Note: .xls files always have the same ID code for indicators, however they have been different for .csv files and since .csv files are easier to load, we need to account for the change in ID codes.
DIMENSIONS_TO_KEEP <- list("post_19T2RD" = c("production_BE" = "TD.P1E_DI_7CH",
                                             "production_CZ" = "TD.P1E_DIM_7CH",
                                             "production_C1" = "TD.P1E_A17C1_7CH",
                                             "production_C2" = "TD.P1E_A17C2_7CH",
                                             "production_C3" = "TD.P1E_A17C3_7CH",
                                             "production_C4" = "TD.P1E_A17C4_7CH",
                                             "production_C5" = "TD.P1E_A17C5_7CH",
                                             "production_DE" = "TD.P1E_A17DE_7CH",
                                             "production_FZ" = "TD.P1E_A17FZ_7CH",
                                             "valeur_ajoutee_BE" = "TD.B1_DI_7CH",
                                             "valeur_ajoutee_CZ" = "TD.B1_DIM_7CH",
                                             "valeur_ajoutee_C1" = "TD.B1_A17C1_7CH",
                                             "valeur_ajoutee_C2" = "TD.B1_A17C2_7CH",
                                             "valeur_ajoutee_C3" = "TD.B1_A17C3_7CH",
                                             "valeur_ajoutee_C4" = "TD.B1_A17C4_7CH",
                                             "valeur_ajoutee_C5" = "TD.B1_A17C5_7CH",
                                             "valeur_ajoutee_DE" = "TD.B1_A17DE_7CH",
                                             "valeur_ajoutee_FZ" = "TD.B1_A17FZ_7CH"),
                           "pre_19T2RD_csv" = c("production_BE" = "TD_P1E_DI7_CH",
                                                "production_CZ" = "TD_P1E_DIM7_CH",
                                                "production_C1" = "TD_P1E_C17_CH",
                                                "production_C2" = "TD_P1E_C27_CH",
                                                "production_C3" = "TD_P1E_C37_CH",
                                                "production_C4" = "TD_P1E_C47_CH",
                                                "production_C5" = "TD_P1E_C57_CH",
                                                "production_DE" = "TD_P1E_DE7_CH",
                                                "production_FZ" = "TD_P1E_FZ7_CH",
                                                "valeur_ajoutee_BE" = "TD_B1_DI7_CH",
                                                "valeur_ajoutee_CZ" = "TD_B1_DIM7_CH",
                                                "valeur_ajoutee_C1" = "TD_B1_C17_CH",
                                                "valeur_ajoutee_C2" = "TD_B1_C27_CH",
                                                "valeur_ajoutee_C3" = "TD_B1_C37_CH",
                                                "valeur_ajoutee_C4" = "TD_B1_C47_CH",
                                                "valeur_ajoutee_C5" = "TD_B1_C57_CH",
                                                "valeur_ajoutee_DE" = "TD_B1_DE7_CH",
                                                "valeur_ajoutee_FZ" = "TD_B1_FZ7_CH"),
                           "pre_2010" = "")
# check if I use a list or a named vector

# loaders --------------------------------------------------------------------------------------------------------------
## Note: the loaders are made to be independent so if another format of excel or csv files appears, just create a new loader

csv_pre_19T2RD_production_loader <- function(file_path, folder_name) {
  # ensure that the file_path is complete
  ## Note: we make the assumption that either the file path is complete (e.g. "T:/SPMAE_Public/Prev_Public/CNAT/ArchivesCTrim/base2005/11T1PE/cprvolch.csv") or only the filename is missing (e.g. "T:/SPMAE_Public/Prev_Public/CNAT/ArchivesCTrim/base2005/11T1PE")
  if (!stringr::str_detect(file_path, ".csv$")) {
    file_path <- file.path(file_path, "cprvolch.csv")
  }
  # load the file
  suppressWarnings(                                                                              # suppress warnings to prevent warning of first column's name missing
    suppressMessages(new_data <- readr::read_csv(file = file_path, col_names = TRUE, skip = 1))) # suppress messages to prevent message of columns' type
  # get the dimensions' code according to the folder_name
  dimensions_list <- return_dimensions_list_for(folder_name)
  # clean the data to fit a proper syntax
  clean_data <- data_cleaner_for_csv(new_data, dimensions_list = dimensions_list)
  return(clean_data)
}

xls_production_loader <- function(file_path, folder_name) {
  # ensure that the file_path is complete
  ## Note: we make the assumption that either the file path is complete (e.g. "T:/SPMAE_Public/Prev_Public/CNAT/ArchivesCTrim/base2005/11T1PE/cprvolch.xls") or the filename is missing (e.g. "T:/SPMAE_Public/Prev_Public/CNAT/ArchivesCTrim/base2005/11T1PE")
  if (!stringr::str_detect(file_path, ".xls$")) {
    file_path <- file.path(file_path, "cprvolch.xls")
  }
  # load the file
  new_data <- readxl::read_xls(path = file_path, col_names = TRUE)
  # get the dimensions' code according to the folder_name
  dimensions_list <- return_dimensions_list_for(folder_name)
  # clean the data to fit a proper syntax
  clean_data <- data_cleaner_for_xls(new_data, dimensions_list = dimensions_list)
  return(clean_data)
}

# TODO: check code duplication with xls_production_loader()
csv_post_19T2RD_production_loader <- function(file_path, folder_name) {
  # ensure that the file_path is complete
  ## Note: we make the assumption that either the file path is complete (e.g. "T:/SPMAE_Public/Prev_Public/CNAT/ArchivesCTrim/base2005/11T1PE/cprvolch.csv") or the filename is missing (e.g. "T:/SPMAE_Public/Prev_Public/CNAT/ArchivesCTrim/base2005/11T1PE")
  if (!stringr::str_detect(file_path, ".csv$")) {
    file_path <- file.path(file_path, "cprvolch.csv")
  }
  # load the file
  suppressMessages(new_data <- readr::read_delim(file = file_path, delim = ";", col_names = TRUE)) # suppress messages to prevent message of columns' type
  # get the dimensions' code according to the folder_name
  dimensions_list <- return_dimensions_list_for(folder_name)
  # clean the data to fit a proper syntax
  clean_data <- data_cleaner_for_xls(new_data, dimensions_list = dimensions_list)
  return(clean_data)
}

# helpers --------------------------------------------------------------------------------------------------------------
return_dimensions_list_for <- function(folder_name) {
  folder_name_pattern <- "([0-9]{2})T([0-9])([A-Z]{2})"
  year <- as.numeric(sub(x = folder_name, pattern = folder_name_pattern, replacement = "\\1"))
  if (year <= 10) {
    stop("For now, we should not use data before 2011 because the quarterly national accounts' methodology was too different.")
  } else if (year > 10 & year <= 18) {
    return("pre_19T2RD_csv")
  } else if (year == 19) {
    quarter <- as.numeric(sub(x = folder_name, pattern = folder_name_pattern, replacement = "\\2"))
    estimation <- sub(x = folder_name, pattern = folder_name_pattern, replacement = "\\3")
    if (quarter == 1 | (quarter == 2 & estimation == "PE")) {
      return("pre_19T2RD_csv")
    } else {
      return("post_19T2RD")
    }
  } else if (year > 19) {
    return("post_19T2RD")
  }
}

transform_quarterly_string_dates_to_date <- function(date_column) {
  # the date values only contain the year and the quarter and sometimes additional characters so (1) we need to only extract the numbers and (2) add "01" for the day
  date_column_in_date_format <- lubridate::ymd(paste0(sub(pattern = "([0-9]{4})(Q|T)([1-9])", "\\1", date_column), # extract the year
                                                      "-",
                                                      as.numeric(sub(pattern = "([0-9]{4})(Q|T)([1-9])", "\\3", date_column)) * 3 - 2, # extract the quarter and get the corresponding month
                                                      "-01"))
  return(date_column_in_date_format)
}

# data cleaners for specific format ------------------------------------------------------------------------------------
# TODO : try to reduce code duplication between the two functions
data_cleaner_for_csv <- function(data, dimensions_list, list_of_dimensions = DIMENSIONS_TO_KEEP) {
  # get the proper list of dimensions
  list_of_dimensions <- list_of_dimensions[[dimensions_list]]
  # rename the first column
  ## Note: in this format, the first column contains national accounts' indicator codes (notably, value added and production by sectors)
  clean_data <- data
  colnames(clean_data)[1] <- "dimension"
  # keep only the dimensions we need
  clean_data <- clean_data %>%
    dplyr::filter(dimension %in% list_of_dimensions)
  # clean the dimension id
  regex_pattern_1 <- "TD.(.*)(_7CH|7_CH)"                                  # use the regex pattern you need to clean the name of your dimensions, if it is not clean
  clean_data <- clean_data %>%
    dplyr::mutate(dimension = sub(pattern = regex_pattern_1, replacement = "\\1", x = clean_data$dimension)) # that means: take the 1st group, that is what we capture in the first brackets with the regex
  regex_pattern_2 <- "(.*)_(A17)?(.*)"
  clean_data <- clean_data %>%
    dplyr::mutate(dimension = sub(pattern = regex_pattern_2, replacement = "\\1_\\3", x = clean_data$dimension)) # we want to exclude "A17" because it appears in some ID code and not in others.
  # pivot the data
  columns_to_pivot <- colnames(clean_data)[colnames(clean_data) != "dimension"]
  clean_data <- clean_data %>%
    tidyr::pivot_longer(cols = dplyr::all_of(columns_to_pivot),
                        names_to = "date",
                        values_to = "value")
  # put the date in date format
  clean_data <- clean_data %>%
    dplyr::mutate(date = transform_quarterly_string_dates_to_date(date))
  # reorganise the dataframe
  clean_data <- clean_data %>%
    dplyr::select(date, dimension, value)
  return(clean_data)
}


data_cleaner_for_xls <- function(data, dimensions_list, list_of_dimensions = DIMENSIONS_TO_KEEP) {
  # get the proper list of dimensions
  list_of_dimensions <- list_of_dimensions[[dimensions_list]]
  # rename the first column
  ## Note: in this format, the first column contains national accounts' indicator codes (notably, value added and production by sectors)
  clean_data <- data
  colnames(clean_data)[1] <- "dimension"
  # remove the second column that contains the indicator labels
  clean_data <- clean_data %>%
    dplyr::select(-INTIT)
  # keep only the dimensions we need
  clean_data <- clean_data %>%
    dplyr::filter(dimension %in% list_of_dimensions)
  # clean the dimension id
  regex_pattern_1 <- "TD.(.*)(_7CH|7_CH)"                                  # use the regex pattern you need to clean the name of your dimensions, if it is not clean
  clean_data <- clean_data %>%
    dplyr::mutate(dimension = sub(pattern = regex_pattern_1, replacement = "\\1", x = clean_data$dimension)) # that means: take the 1st group, that is what we capture in the first brackets with the regex
  regex_pattern_2 <- "(.*)_(A17)?(.*)"
  clean_data <- clean_data %>%
    dplyr::mutate(dimension = sub(pattern = regex_pattern_2, replacement = "\\1_\\3", x = clean_data$dimension)) # we want to exclude "A17" because it appears in some ID code and not in others.
  # pivot the data
  columns_to_pivot <- colnames(clean_data)[colnames(clean_data) != "dimension"]
  clean_data <- clean_data %>%
    tidyr::pivot_longer(cols = dplyr::all_of(columns_to_pivot),
                        names_to = "date",
                        values_to = "value")
  # put the date in date format
  clean_data <- clean_data %>%
    dplyr::mutate(date = sub("A(.*)", "\\1", x = date)) %>%
    dplyr::mutate(date = transform_quarterly_string_dates_to_date(date))
  # reorganise the dataframe
  clean_data <- clean_data %>%
    dplyr::select(date, dimension, value)
  return(clean_data)
}