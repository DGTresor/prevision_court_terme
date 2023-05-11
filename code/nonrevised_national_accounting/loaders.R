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

# loader for the most recent national accounting file ------------------------------------------------------------------

most_recent_compta_nat_data_loader <- function(folder_path, folder_name, file_name, dimensions_list, dimensions_list_name = NULL) {
  # checking the folder_path
  national_account_base_year <- stringr::str_extract(string = folder_path, pattern = "(?<=/)base[:digit:]{4}")
  message(paste("Le chemin du dossier pointe actuellement vers", national_account_base_year, "; Pensez à le changer si la base change."))
  # get the file_path
  file_path <- get_the_most_recent_file(folder_path)
  # get data
  clean_data <- csv_post_19T2RD_national_accounting_loader(file_path = file_path,
                                                           folder_name = stringr::str_extract(file_path, "(?<=/)[:digit:]{2}T[:digit:][:alpha:]{2}$"),
                                                           file_name = file_name,
                                                           dimensions_list = dimensions_list,
                                                           dimensions_list_name = dimensions_list_name)
  return(clean_data)
}

# loaders --------------------------------------------------------------------------------------------------------------
## Note: the loaders are made to be independent so if another format of excel or csv files appears, just create a new loader

csv_pre_19T2RD_national_accounting_loader <- function(file_path, folder_name, file_name, dimensions_list, dimensions_list_name = NULL) {
  # ensure that the file_path is complete
  ## Note: we make the assumption that either the file path is complete (e.g. "T:/SPMAE_Public/Prev_Public/CNAT/ArchivesCTrim/base2005/11T1PE/cprvolch.csv") or only the filename is missing (e.g. "T:/SPMAE_Public/Prev_Public/CNAT/ArchivesCTrim/base2005/11T1PE")
  if (!stringr::str_detect(file_path, ".csv$")) {
    file_path <- file.path(file_path, paste0(file_name, ".csv"))
  }
  # load the file
  suppressWarnings(                                                                              # suppress warnings to prevent warning of first column's name missing
    suppressMessages(new_data <- readr::read_csv(file = file_path, col_names = TRUE, skip = 1))) # suppress messages to prevent message of columns' type
  # get the dimensions' code according to the folder_name
  if (is.null(dimensions_list_name)) {
    dimensions_list_name <- return_dimensions_list_name_for(folder_name)
  }
  # clean the data to fit a proper syntax
  clean_data <- data_cleaner_for_csv(new_data, dimensions_list_name = dimensions_list_name, list_of_dimensions = dimensions_list)
  return(clean_data)
}

xls_national_accounting_loader <- function(file_path, folder_name, file_name, dimensions_list, dimensions_list_name = NULL) {
  # ensure that the file_path is complete
  ## Note: we make the assumption that either the file path is complete (e.g. "T:/SPMAE_Public/Prev_Public/CNAT/ArchivesCTrim/base2005/11T1PE/cprvolch.xls") or the filename is missing (e.g. "T:/SPMAE_Public/Prev_Public/CNAT/ArchivesCTrim/base2005/11T1PE")
  if (!stringr::str_detect(file_path, ".xls$")) {
    file_path <- file.path(file_path, paste0(file_name, ".xls"))
  }
  # load the file
  new_data <- readxl::read_xls(path = file_path, col_names = TRUE)
  # get the dimensions' code according to the folder_name
  if (is.null(dimensions_list_name)) {
    dimensions_list_name <- return_dimensions_list_name_for(folder_name)
  }
  # clean the data to fit a proper syntax
  clean_data <- data_cleaner_for_xls(new_data, dimensions_list_name = dimensions_list_name, list_of_dimensions = dimensions_list)
  return(clean_data)
}

# TODO: check code duplication with xls_production_loader()
csv_post_19T2RD_national_accounting_loader <- function(file_path, folder_name, file_name, dimensions_list, dimensions_list_name = NULL) {
  # ensure that the file_path is complete
  ## Note: we make the assumption that either the file path is complete (e.g. "T:/SPMAE_Public/Prev_Public/CNAT/ArchivesCTrim/base2005/11T1PE/cprvolch.csv") or the filename is missing (e.g. "T:/SPMAE_Public/Prev_Public/CNAT/ArchivesCTrim/base2005/11T1PE")
  if (!stringr::str_detect(file_path, ".csv$")) {
    file_path <- file.path(file_path, paste0(file_name, ".csv"))
  }
  # load the file
  suppressMessages(new_data <- readr::read_delim(file = file_path, delim = ";", col_names = TRUE)) # suppress messages to prevent message of columns' type
  # get the dimensions' code according to the folder_name
  if (is.null(dimensions_list_name)) {
    dimensions_list_name <- return_dimensions_list_name_for(folder_name)
  }
  # clean the data to fit a proper syntax
  clean_data <- data_cleaner_for_xls(new_data, dimensions_list_name = dimensions_list_name, list_of_dimensions = dimensions_list)
  return(clean_data)
}

# helpers --------------------------------------------------------------------------------------------------------------
return_dimensions_list_name_for <- function(folder_name) {
  folder_name_pattern <- "([0-9]{2})T([0-9])([A-Z]{2})"
  year <- as.numeric(sub(x = folder_name, pattern = folder_name_pattern, replacement = "\\1"))
  if (year <= 10) {
    return("pre_2011")
    message("Note that pre2011 data should only be used for now for PIB data because national accounting conventions have changed a lot between base2005 and base2000.")
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
  # date_column_in_date_format <- lubridate::ymd(paste0(sub(pattern = "([0-9]{4})(Q|T)([1-9])", "\\1", date_column), # extract the year
  #                                                     "-",
  #                                                     as.numeric(sub(pattern = "([0-9]{4})(Q|T)([1-9])", "\\3", date_column)) * 3 - 2, # extract the quarter and get the corresponding month
  #                                                     "-01"))
  # TODO : check if does not break the prevision code
  date_column_in_date_format <- lubridate::ymd(paste0(sub(pattern = "(A?)([0-9]{4})(Q|T)([1-9])", "\\2", date_column), # extract the year
                                                      "-",
                                                      as.numeric(sub(pattern = "(A?)([0-9]{4})(Q|T)([1-9])", "\\4", date_column)) * 3 - 2, # extract the quarter and get the corresponding month
                                                      "-01"))
  return(date_column_in_date_format)
}

# data cleaners for specific format ------------------------------------------------------------------------------------
# TODO : try to reduce code duplication between the two functions
data_cleaner_for_csv <- function(data, dimensions_list_name, list_of_dimensions) {
  # get the proper list of dimensions
  list_of_dimensions <- list_of_dimensions[[dimensions_list_name]]
  # rename the first column
  ## Note: in this format, the first column contains national accounts' indicator codes (notably, value added and production by sectors)
  clean_data <- data %>%
    select(-contains("INTIT")) # TODO: check if break prevision code
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


data_cleaner_for_xls <- function(data, dimensions_list_name, list_of_dimensions) {
  # get the proper list of dimensions
  list_of_dimensions <- list_of_dimensions[[dimensions_list_name]]
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