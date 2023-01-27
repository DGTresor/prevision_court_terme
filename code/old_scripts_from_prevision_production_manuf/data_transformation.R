# Title     : transform data to be reading for forecasting
# Created by: lphung
# Created on: 18/07/2022

# libraries ------------------------------------------------------------------------------------------------------------

library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)

# functions ------------------------------------------------------------------------------------------------------------

## TODO: equivalent to the function "extend_series.R" of Daniel
### this function enables to extend a dataframe (and / or certain dimensions) up to a certain date and filling the missing values with the last available value for each dimension.
extend_monthly_series_for <- function(data, extend_up_to, method = "acquis", dimensions_to_extend = NULL, dimensions_to_exclude = NULL, add_option = FALSE) {
  # if add_option = FALSE => the function replaces the dimensions_to_transform by their carry-over / naive form
  # if add_option = TRUE => the function adds the carry-over / naive form of the dimensions_to_transform in the existing dataset
  # TODO: can improve the function by extending the regressors using an ARMA model rather than a carry-over / naive extension
  # TODO: choose to use either the term "carry-over" (calculer l'aquis au mois XXX pour XXX en français) or "naive"

  # select the dimensions
  if (is.null(dimensions_to_extend)) {
    dimensions_to_extend <- unique(data$dimension)
  }
  if (!is.null(dimensions_to_exclude)) {
    dimensions_to_extend <- dimensions_to_extend[!(dimensions_to_extend %in% dimensions_to_exclude)]
  }

  # define the extension period
  extended_dates <- seq.Date(from = as.Date(min(unique(data$date))),
                             to = lubridate::ymd(extend_up_to),
                             by = "month")
  empty_dataset_with_extended_dates <- data.frame("date" = rep(extended_dates, times = length(unique(data$dimension))),
                                                  "dimension" = rep(unique(data$dimension), each = length(extended_dates)))
  balanced_data <- data %>%
    dplyr::full_join(empty_dataset_with_extended_dates, by = c("date", "dimension"))  # add the dates we need

  # perform the extension
  extended_data <- balanced_data %>%
    dplyr::filter(dimension %in% dimensions_to_extend)
  if (method == "na") {
    next
  } else if (method == "acquis") {
    # NB: acquis is the French term for carry-over
    extended_data <- extended_data %>%
      fill_only_last_values(value)
  } else if (method == "lag") {
    # Note: this method should only be used when the dimensions to extend end all at the same date
    last_date_of_dimensions_to_extend <- as.Date(max(unique(data$date[data$dimension %in% dimensions_to_extend])))
    time_difference <- lubridate::interval(start = last_date_of_dimensions_to_extend, end = extend_up_to)
    nbr_lags <- time_difference %/% months(1)
    extended_data <- extended_data %>%
      group_by(dimension) %>%
      mutate(variation = lag(value, n = nbr_lags))
    extended_data <- extended_data %>%
      select(-value) %>%
      rename(value = variation) %>%
      ungroup()
  } else {
    stop("Choisissez \"na\", \"acquis\" ou \"lag\" pour l'argument method= de la fonction extend_monthly_series_for().")
  }

  # combine the transformed series with the original data
  full_data_with_extension <- combine_transformed_series_with_original_data(original_data = balanced_data,
                                                                            transformed_data = extended_data,
                                                                            dimensions_to_transform = dimensions_to_extend,
                                                                            add_option = add_option,
                                                                            prefix = method)
  return(full_data_with_extension)
}

fill_only_last_values <- function(data, column_to_fill) {
  column_to_fill <- dplyr::enquo(column_to_fill)     # NB: dplyr syntax to call columns without knowing their names
  last_available_data <- data %>%
    dplyr::filter(!is.na(!!column_to_fill)) %>%
    dplyr::group_by(dimension) %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::rename(last_date = date,
                  last_available_value = !!column_to_fill)

  filled_data <- data %>%
    dplyr::left_join(last_available_data, by = "dimension") %>%
    dplyr::group_by(dimension) %>%
    dplyr::mutate(!!column_to_fill := ifelse(date > last_date, last_available_value, !!column_to_fill)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-last_date, -last_available_value)
  return(filled_data)
}

## TODO: equivalent to the function "add_diff.R" of Daniel
get_variation_for <- function(data, variation_type, nbr_lag = 1, dimensions_to_transform = NULL, dimensions_to_exclude = NULL, add_option = FALSE) {
  # select the dimensions
  if (is.null(dimensions_to_transform)) {
    dimensions_to_transform <- unique(data$dimension)
  }
  if (!is.null(dimensions_to_exclude)) {
    dimensions_to_transform <- dimensions_to_transform[!(dimensions_to_transform %in% dimensions_to_exclude)]
  }
  data_with_variation <- data %>%
    dplyr::filter(dimension %in% dimensions_to_transform)

  # perform the transformation
  if (variation_type == "difference") {
    prefix <- paste0("diff", nbr_lag)
    data_with_variation <- data_with_variation %>%
      dplyr::group_by(dimension) %>%
      dplyr::mutate(variation = value - dplyr::lag(value, n = nbr_lag)) %>%
      dplyr::ungroup()
  } else if (variation_type == "growth_rate") {
    prefix <- paste0("var", nbr_lag)
    data_with_variation <- data_with_variation %>%
      dplyr::group_by(dimension) %>%
      dplyr::mutate(variation = (value / dplyr::lag(value, n = nbr_lag)) - 1) %>%
      dplyr::ungroup()
  } else {
    stop("Choisissez \"difference\" ou \"growth_rate\" comme argument de variation_type pour la fonction get_variation_for().")
  }
  # combine the transformed series with the original data
  data_with_variation <- data_with_variation %>%
    dplyr::select(-value) %>%
    dplyr::rename(value = variation) # harmonise the naming with original data
  full_data_with_variation <- combine_transformed_series_with_original_data(original_data = data,
                                                                            transformed_data = data_with_variation,
                                                                            dimensions_to_transform = dimensions_to_transform,
                                                                            add_option = add_option,
                                                                            prefix = prefix)
  return(full_data_with_variation)
}

month_to_quarter <- function(data, transformation_type) {
  # Mixing monthly and quarterly data is a bad idea => we transform all the dimensions to quarter
  dimensions_to_transform <- unique(data$dimension)
  quarterly_data <- data

  # perform the transformation
  if (transformation_type %in% c("sum", "mean")) {
    transformation_function <- get_transformation_function(transformation_type)

    quarterly_data <- quarterly_data %>%
      monthly_date2quarterly_date() %>%
      dplyr::group_by(date, dimension) %>%
      dplyr::summarise(value = transformation_function(value, na.rm = TRUE), .groups = "drop")

  } else if (transformation_type == "split") {
    quarterly_data <- quarterly_data %>%
      split_data_by_month_position_in_quarter()
  } else {
    stop("Choisissez \"sum\", \"mean\" ou \"split\" comme argument transformation_type pour la fonction month_to_quarter().")
  }

  # finalise output
  prefix <- ifelse(transformation_type == "split", "", "qt_")
  full_quarterly_data <- quarterly_data %>%
    dplyr::mutate(dimension = paste0(prefix, dimension))
  return(full_quarterly_data)
}

split_data_by_month_position_in_quarter <- function(data) {
  split_data <- data %>%
    dplyr::mutate(month = lubridate::month(date),
                  month_position = dplyr::case_when(month %in% c(1, 4, 7, 10) ~ 1,
                                                    month %in% c(2, 5, 8, 11) ~ 2,
                                                    month %in% c(3, 6, 9, 12) ~ 3)) %>%
    dplyr::mutate(dimension = paste0(dimension, "_m", month_position)) %>%
    dplyr::select(-c(month, month_position)) %>%
    monthly_date2quarterly_date()
  return(split_data)
}

get_transformation_function <- function(transformation_type) {
  if (transformation_type == "sum") {
    return(base::sum)
  } else if (transformation_type == "mean") {
    return(base::mean)
  } else {
    stop("Choisissez \"sum\" ou \"mean\" comme argument transformation_type pour la fonction get_transformation_function().")
  }
}

add_quarterly_dummy <- function(data, dummy_name, vector_of_dates) {
  # DEF: add quarterly dummy in column
  vector_of_dates <- lubridate::ymd(vector_of_dates)
  check_if_quarterly_dates(vector_of_dates)
  data_with_dummy <- data %>%
    mutate(dummy = ifelse(date %in% vector_of_dates, 1, 0))

  colnames(data_with_dummy)[colnames(data_with_dummy) == "dummy"] <- dummy_name

  return(data_with_dummy)
}

#TODO: need to create a test for this function
extend_prospective_balances_when_forecasting_next_quarter <- function(data, list_with_data_availability) {
  extended_data <- data
  for (data_source in names(list_with_data_availability)[names(list_with_data_availability) != "ipi"]) {
    last_month_of_data <- names(list_with_data_availability[[data_source]][list_with_data_availability[[data_source]]])
    if (last_month_of_data == "m-1") {
      #TODO: decide if I solely extend the series or lag it entirely, for now we only extend
      # we extend the prospective balances for the specific survey source (e.g. pmi)
      # prospective balances are for instance "production_prevue", "new_commandes", "carnets_globaux", etc.
      matching_regex_pattern <- paste0(data_source, ".*", "(prevu|commande|carnet).*")
      dimensions_to_extend <- stringr::str_subset(string = unique(extended_data$dimension),
                                                  pattern = matching_regex_pattern)
      last_date_of_data <- max(unique(extended_data$date[extended_data$dimension %in% dimensions_to_extend]))
      date_to_extend_to <- lubridate::ymd(last_date_of_data) + months(1)
      extended_data <- extended_data %>%
        extend_monthly_series_for(extend_up_to = date_to_extend_to, dimensions_to_extend = dimensions_to_extend, method = "lag", add_option = TRUE) %>%
        extend_monthly_series_for(extend_up_to = date_to_extend_to, dimensions_to_extend = dimensions_to_extend)
    }
  }
  return(extended_data)
}

# TODO: from automatisation_reactions => get get_reduced_centered_type() and related functions

# helpers --------------------------------------------------------------------------------------------------------------
quarter_vector2date <- function(quarter_vector, position = "end") {
  if (position == "beginning") {
    # We want the first month of the quarter as a way to give a date to the quarter
    last_month_of_quarter <- quarter_vector[["quarter"]] * 3 - 2
  } else if (position == "middle") {
    last_month_of_quarter <- quarter_vector[["quarter"]] * 3 - 1
  } else if (position == "end") {
    # We want the last month of the quarter (e.g. March for the 1st quarter) because we need data up to this month to get data for a complete quarter
    last_month_of_quarter <- quarter_vector[["quarter"]] * 3
  } else {
    stop("Choisissez \"beginning\", \"middle\" ou \"end\" pour l'argument position dans la fonction quarter_vector2date.")
  }

  quarter_date <- lubridate::ymd(paste(quarter_vector[["year"]], last_month_of_quarter, "01",
                                       sep = "-"))
  return(quarter_date)
}

monthly_date2quarterly_date <- function(data) {
  data_with_quarterly_date <- data %>%
    dplyr::mutate(quarter = lubridate::quarter(date),
                  year = lubridate::year(date)) %>%
    dplyr::mutate(month = dplyr::case_when(quarter == 1 ~ 1,
                                           quarter == 2 ~ 4,
                                           quarter == 3 ~ 7,
                                           quarter == 4 ~ 10)) %>%  # TODO: CHECK if better to do case_when or a formula : quarter * 3 - 2
    dplyr::mutate(date = lubridate::make_date(year = year, month = month)) %>% # TODO: check difference with lubridate::ymd(paste(year, month, "01", sep = "-"))
    select(-c(month, quarter, year))
  return(data_with_quarterly_date)
}


combine_transformed_series_with_original_data <- function(original_data, transformed_data, dimensions_to_transform, add_option, prefix) {
  if (add_option) {
    transformed_data <- transformed_data %>%
      dplyr::mutate(dimension = paste(prefix, dimension, sep = "_")) # NB: if we modify the dimension value, it will be considered as another dimension during the join
    dimensions_to_transform <- c() # i.e. we will take all the original dimensions
  }
  combined_data <- original_data %>%
    filter(!(dimension %in% dimensions_to_transform)) %>% # if add_option = FALSE, remove the dimensions that have been extended to prevent duplication
    dplyr::bind_rows(transformed_data)
  return(combined_data)
}

check_if_quarterly_dates <- function(vector_of_dates) {
  correct_months_for_quarters <- month(vector_of_dates) %in% c(1, 4, 7, 10)
  if (sum(correct_months_for_quarters) != length(vector_of_dates)) {
    stop("Vous devez renseigner des dates de trimestre, au format \"AAAA-MM-JJ\" avec pour mois MM 1, 4, 7 ou 10.")
  }
}

# conversion -----------------------------------------------------------------------------------------------------------

convert_to_wide_format <- function(long_format_data) {
  wide_format <- long_format_data %>%
    tidyr::pivot_wider(names_from = dimension,
                       values_from = value)
  return(wide_format)
}

# create samples for forecasting ---------------------------------------------------------------------------------------

create_sub_datasets_by_survey_type <- function(forecasting_data, list_with_data_availability) {
  data_samples <- list()
  last_month_of_ipi_data <- names(list_with_data_availability[["ipi"]][list_with_data_availability[["ipi"]]])
  for (survey in c("insee", "bdf", "pmi")) {
    # define data to use and names
    last_month_of_survey_data <- names(list_with_data_availability[[survey]][list_with_data_availability[[survey]]])
    name_sample_without_ipi <- paste(survey, last_month_of_survey_data, sep = "_")

    # create the lambda function that enables to keep only the regressors that are non missing for the last period
    is_last_value_non_missing <- function(x) ifelse(is.Date(x), all(!is.na(lubridate::ceiling_date(x))), all(!is.na(ceiling(x))))

    #TODO: change logic -> current tricks to take prospective balances of month m-1 for month m1 when forecasting the next quarter. Maybe need to reunite the values
    if (last_month_of_survey_data == "m-1") {
      last_month_of_survey_data <- "m1"
    }

    # prepare the samples
    if (last_month_of_ipi_data %in% c("m-1", "m1", "m2", "m3")) {
      name_sample_with_ipi <- paste(name_sample_without_ipi, "ipi", last_month_of_ipi_data, sep = "_")
      # # create one sub-dataset with IPI
      # if (last_month_of_ipi_data == "m-1") {
      #   last_month_of_ipi_data <- "m1"
      # }
      data_samples[[name_sample_with_ipi]] <- forecasting_data %>%
        dplyr::select(date, AR1, qt_ipi, (contains(survey) & ends_with(last_month_of_survey_data))) %>%
        dplyr::select(where(is_last_value_non_missing)) %>%  # we keep only the regressors that have non missing last values
        dplyr::left_join(forecasting_data %>% select(date, prod_manuf), by = "date") %>%  # we add the value to predict that, by definition, has missing last value
        dplyr::select(date, prod_manuf, everything()) # to tidy a bit

      # create one sub-dataset for without IPI
      data_samples[[name_sample_without_ipi]] <- data_samples[[name_sample_with_ipi]] %>%
        dplyr::select(-qt_ipi)
    } else {
      # create one sub-dataset for without IPI
      data_samples[[name_sample_without_ipi]] <- forecasting_data %>%
        dplyr::select(date, prod_manuf, AR1,
                       T1_2020, T2_2020, T3_2020, # TODO: TO DELETE
                       (contains(survey) & ends_with(last_month_of_survey_data))) %>%
        dplyr::select(where(is_last_value_non_missing)) %>%   # we keep only the regressors that have non missing last values
        dplyr::left_join(forecasting_data %>% select(date, prod_manuf), by = "date") %>%  # we add the value to predict that, by definition, has missing last value
        dplyr::select(date, prod_manuf, everything()) # to tidy a bit
    }
  }
  return(data_samples)
}
