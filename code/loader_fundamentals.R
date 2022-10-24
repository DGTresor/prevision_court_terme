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

get_loader_provider <- function(data_source) {
  if (data_source == "ipi") {
    return(get_loader_for_ipi)
  } else if (data_source == "production") {
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
  return(loader_provider(file_type = "generic"))
}


construct_nonrevised_series <- function(data, new_data, date_granularity = "quarter", number_previous_values = 0) {
  # keep only the number of previous values we want
  if (date_granularity == "month") {
    data_to_bind <- new_data %>%
      dplyr::filter(date >= max(date) - months(number_previous_values)) # if the number of previous values is 0, we only want the observations for the last date
  } else if (date_granularity == "quarter") {
    data_to_bind <- new_data %>%
      dplyr::filter(date >= max(date) - months(number_previous_values) * 3) # if the number of previous values is 0, we only want the observations for the last date
  } else {
    stop("Provide either \"month\" or \"quarter\" as date_granularity to the construct_nonrevised_production_series() function.")
  }
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
