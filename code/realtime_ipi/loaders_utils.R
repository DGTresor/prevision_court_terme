# Title     : meta-functions that call the loaders to construct the realtime ipi series
# Created by: lphung
# Created on: 12/09/2022

# packages -------------------------------------------------------------------------------------------------------------
library(stringr)
library(dplyr)
source("./code/realtime_ipi/loaders.R", encoding = "utf-8")

# folders --------------------------------------------------------------------------------------------------------------
IPI_DATA_FOLDER <- "S:/SPMAE/PREV/Prev3/_Fichiers_Prev3/Prod_manuf/02-IPI/mail_reaction_ipi/02-Envoi_Insee"

# constants ------------------------------------------------------------------------------------------------------------
DATE <- as.character("date")
DIMENSION <- as.character("dimension")
VALUE <- as.character("value")
COLUMN_NAMES <- c(DATE, DIMENSION, VALUE)

# structural functions -------------------------------------------------------------------------------------------------
get_column_names_to_join_by <- function() {
  return(c(DATE, DIMENSION))
}

get_empty_dataframe <- function() {
  empty_dataframe <- data.frame(
    "date" = Date(),
    "dimension" = character(),
    "value" = double(),
    stringsAsFactors = FALSE
  )
  colnames(empty_dataframe) <- COLUMN_NAMES
  return(empty_dataframe)
}

# functions ------------------------------------------------------------------------------------------------------------
construct_realtime_ipi_from_scratch <- function(files_list, file_type2files_list, number_revision = 1, data_correction = "CJO-CVS", keep_historical_data = FALSE) {
  df <- get_empty_dataframe()
  column_to_join_by <- get_column_names_to_join_by()

  for (file_name in names(files_list)) {
    print(paste("On lit le fichier :", file_name))
    loader <- get_loader(file_name, file_type2files_list)
    new_data <- loader(files_list[[file_name]], data_correction = "CJO-CVS")
    df <- construct_realtime_ipi_series(df, new_data, column_to_join_by, number_revision = number_revision, keep_historical_data = keep_historical_data)
    rm(new_data)
  }
  df <- df %>% dplyr::arrange(date)
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

construct_realtime_ipi_series <- function(data, new_data, column_to_join_by, number_revision = 1, keep_historical_data = FALSE) {
  # specific case
  if (keep_historical_data) {
    if (nrow(data) == 0) {
      return(new_data)
    }
  }

  # generic case: only keep the observations for a certain period
  data_to_join <- new_data %>%
    dplyr::filter(date >= max(date) - months(number_revision - 1)) # if the number of revision is 1, we only want the observations for the last date
  data <- dplyr::rows_upsert(data, data_to_join, by = column_to_join_by)
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
