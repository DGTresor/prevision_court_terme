# Title     : meta-functions that call the loaders to construct the realtime ipi series
# Created by: lphung
# Created on: 12/09/2022

# packages -------------------------------------------------------------------------------------------------------------
library(stringr)
library(dplyr)
library(tidyr)
source("./code/realtime_ipi/loaders.R", encoding = "utf-8")

# folders --------------------------------------------------------------------------------------------------------------
IPI_DATA_FOLDER <- "S:/SPMAE/PREV/Prev3/_Fichiers_Prev3/Prod_manuf/02-IPI/mail_reaction_ipi/02-Envoi_Insee"

# structural functions -------------------------------------------------------------------------------------------------
get_column_names <- function(number_previous_values) {
  column_names <- c("date", "dimension", "t")
  if (number_previous_values > 0) {
    column_names <- c(column_names, paste0("t_", seq(1:number_previous_values)))
  }
  return(column_names)
}

get_empty_dataframe <- function(number_previous_values = 0) {
  # create the dataframe
  empty_dataframe <- data.frame(matrix(nrow = 0, ncol = number_previous_values + 3))
  # name the columns
  colnames(empty_dataframe) <- get_column_names(number_previous_values = number_previous_values)
  # impose the columns' type
  empty_dataframe <- empty_dataframe %>%
    dplyr::mutate(date = as.Date(date),
                  dimension = as.character(dimension)) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("t"), as.double))
  return(empty_dataframe)
}

# functions ------------------------------------------------------------------------------------------------------------
construct_realtime_ipi_from_scratch <- function(files_list, file_type2files_list, number_previous_values = 0, data_correction = "CJO-CVS") {
  df <- get_empty_dataframe(number_previous_values)
  # column_to_join_by <- get_column_names_to_join_by()

  for (file_name in names(files_list)) {
    print(paste("On lit le fichier :", file_name))
    loader <- get_loader(file_name, file_type2files_list)
    new_data <- loader(files_list[[file_name]], data_correction = "CJO-CVS")
    df <- construct_realtime_ipi_series(df, new_data, number_previous_values = number_previous_values)
    rm(new_data)
  }
  df <- df %>% dplyr::arrange(date)
  #select(all_of(sort(colnames(data_to_bind))))
  return(df)
}

get_loader <- function(file_name, file_type2files_list) {
  for (file_type in names(file_type2files_list)) {
    if (file_name %in% file_type2files_list[[file_type]]) {
      return(get_loader_for(file_type))
    }
  }
  return(generic_loader)
}

get_loader_for <- function(file_type) {
  if (file_type == "sectors_in_line_one_label_loader") {
    return(sectors_in_line_one_label_loader)
  } else if (file_type == "sectors_in_line_two_labels_loader") {
    return(sectors_in_line_two_labels_loader)
  } else {
    stop(paste0("No loader found for: ", file_type))
  }
}

construct_realtime_ipi_series <- function(data, new_data, number_previous_values = 0) {
  # keep only the number of previous values we want
  data_to_bind <- new_data %>%
    dplyr::filter(date >= max(date) - months(number_previous_values)) # if the number of previous values is 0, we only want the observations for the last date
  # give a rank to the dates, from t (the latest date) to t_n (the n+1 latest date)
  data_to_bind <- data_to_bind %>%
    dplyr::mutate(date_rank = dplyr::dense_rank(desc(date)) - 1L) %>%
    dplyr::mutate(date_rank = case_when(
      date_rank == "0" ~ "t",
      TRUE ~ paste0("t_", date_rank)
    ))
  # attribute as date the latest date that is also the date of the file, and pivot the data
  last_date <- as.Date(max(unique(data_to_bind$date)))
  data_to_bind <- data_to_bind %>%
    dplyr::mutate(date = last_date) %>%
    tidyr::pivot_wider(names_from = "date_rank")
  # bind all data together
  data <- dplyr::bind_rows(data, data_to_bind)
  return(data)
}


update_realtime_ipi <- function() {

}

# functions to prepare the files' list ---------------------------------------------------------------------------------
get_ipi_data_files <- function(ipi_data_folder) {
  # get the list of data files
  ipi_data_files <- list.files(path = ipi_data_folder, pattern = ".*\\.xls", recursive = TRUE, full.names = TRUE)
  # give an harmonised name to each file according to the last date of the ipi data
  names(ipi_data_files) <- stringr::str_extract(ipi_data_files, pattern = "(?<=_)[0-9]{5,6}(?=\\.xls)")
  names(ipi_data_files) <- harmonise_date_strings_in_files_name(names(ipi_data_files))  # the harmonisation is essential to ensure that the files are read in the good chronological order
  # sort the list by the names
  ipi_data_files <- ipi_data_files[sort(names(ipi_data_files))]

  return(ipi_data_files)
}

harmonise_date_strings_in_files_name <- function(vector_files_name) {
  for (file_name in 1:length(vector_files_name)) {
    if (nchar(vector_files_name[file_name]) == 5) {
      vector_files_name[file_name] <- paste0(stringr::str_sub(vector_files_name[file_name], start = 1, end = 4),  # subset the year
                                             "0",                                                                 # place a "zero" so that January 2022 is written "202201" and not "20221"
                                             stringr::str_sub(vector_files_name[file_name], start = 5, end = 5))  # subset the month
    }
  }
  return(vector_files_name)
}
