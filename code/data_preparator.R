# Title     : Functions that enable to transform data
# Created by: lphung
# Created on: 17/10/2022

# libraries ------------------------------------------------------------------------------------------------------------
library(dplyr)
library(lubridate)
library(stringr)


# functions ------------------------------------------------------------------------------------------------------------

merge_nonrevised_and_revised_data <- function(revised_data, nonrevised_data, dimension_to_keep, column_to_use_for_revised_data, column_to_use_for_nonrevised_data, data_label = "") {
  col_revised <- enquo(column_to_use_for_revised_data)
  col_nonrevised <- enquo(column_to_use_for_nonrevised_data)

  selected_revised_data <- revised_data %>%
    dplyr::select(date, dimension, !!col_revised) %>%
    dplyr::filter(dimension == dimension_to_keep) %>%
    dplyr::mutate(dimension = ifelse(data_label == "", "revised", paste0("revised_", data_label)))

  selected_nonrevised_data <- nonrevised_data %>%
    dplyr::select(date, dimension, !!col_nonrevised) %>%
    dplyr::filter(dimension == dimension_to_keep) %>%
    dplyr::mutate(dimension = ifelse(data_label == "", "nonrevised", paste0("nonrevised_", data_label))) %>%
    dplyr::rename(!!col_revised := !!col_nonrevised)

  merged_data <- selected_nonrevised_data %>%
      dplyr::bind_rows(selected_revised_data)

  return(merged_data)
}


get_changing_base_shift <- function(df_with_revised_and_nonrevised_data, start_period, end_period) {
  dimensions <- unique(df_with_revised_and_nonrevised_data$dimension)
  revised_label <- stringr::str_subset(dimensions, "^revised.*")
  nonrevised_label <- stringr::str_subset(dimensions, "^nonrevised.*")

  period_averages <- df_with_revised_and_nonrevised_data %>%
    dplyr::filter(date >= lubridate::ymd(start_period) & date <= lubridate::ymd(end_period)) %>%
    dplyr::group_by(dimension) %>%
    dplyr::summarise(mean = mean(value, na.rm = TRUE), .groups = "drop")

  changing_base_effect <- period_averages$mean[period_averages$dimension == revised_label] - period_averages$mean[period_averages$dimension == nonrevised_label]

  return(changing_base_effect)
}






########################################################################################################################
########################################################################################################################
########################################################################################################################
################################## functions from other code ###########################################################

# conversion -----------------------------------------------------------------------------------------------------------

convert_to_wide_format <- function(long_format_data) {
  wide_format <- long_format_data %>%
    tidyr::pivot_wider(names_from = dimension,
                       values_from = value)
  return(wide_format)
}

# functions ------------------------------------------------------------------------------------------------------------

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



get_variation_for <- function(data, variation_type, nbr_lag = 1, dimensions_to_transform = NULL, dimensions_to_exclude = NULL, add_option = FALSE, keep_prefix = FALSE) {
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
                                                                            keep_prefix = keep_prefix,
                                                                            prefix = prefix)
  return(full_data_with_variation)
}

combine_transformed_series_with_original_data <- function(original_data, transformed_data, dimensions_to_transform, add_option, keep_prefix, prefix) {
  if (add_option | keep_prefix) {
    transformed_data <- transformed_data %>%
      dplyr::mutate(dimension = paste(prefix, dimension, sep = "_")) # NB: if we modify the dimension value, it will be considered as another dimension during the join
    if(add_option) {
      dimensions_to_transform <- c() # i.e. we will take all the original dimensions
    }
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
