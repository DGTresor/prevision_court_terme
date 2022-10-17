# Title     : helpers functions to be used in loaders
# Created by: lphung
# Created on: 12/09/2022

# packages -------------------------------------------------------------------------------------------------------------
library(dplyr)

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
check_data_correction <- function(data_correction) {
  if (!(data_correction %in% c("bruts", "CJO-CVS"))) {
    stop("Please choose either \"bruts\" or \"CJO-CVS\" as data_correction argument for the loaders.")
  }
}

get_loader_provider <- function(data_source){
  if(data_source == "ipi"){
    return(get_loader_for_ipi)
  } else if (data_source == "production"){
    return(get_loader_for_production)
  } else {
    stop(paste0("No loader provider found for the data_source: ", data_source))
  }
}

get_loader <- function(file_name, file_type2files_list, loader_provider) {
  for (file_type in names(file_type2files_list)) {
    if (file_name %in% file_type2files_list[[file_type]]) {
      return(loader_provider(file_type = file_type))
    }
  }
}