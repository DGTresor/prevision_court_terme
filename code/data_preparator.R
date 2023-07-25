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

get_quarterly_variation_for_nonrevised_monthly_data <- function(data, month_position_of_quarters = NULL, quarterly_variation_column_name = NULL, keep_level_columns = FALSE) {
  # TODO: do the case for month_position_of_quarters = 0; not by default
  # TODO: enable when there is no dimension column in the dataframe

  # ensure that we have enough lags to calculate quarterly variation
  ## TODO: can be improved to depend on the month_position_of_quarters
  if (sum(c("t", "t_1", "t_2", "t_3", "t_4", "t_5") %in% names(data)) != 6) {
    stop("We need at least 6 lags in nonrevised data. In the construct_nonrevised_data_from_scratch functions, please set the number_previous_values argument at least to 6.")
  }

  # ensure that month_position_of_quarters %in% c(1,2,3)
  if (!is.null(month_position_of_quarters)) {
    if (!(month_position_of_quarters %in% c(1, 2, 3))) {
      stop("In the function get_quarterly_variation_for_nonrevised_monthly_data(), month_position_of_quarters argument must be 1, 2 or 3.")
    }
  }

  # get quarterly data
  quarterly_data <- month_to_quarter_for_nonrevised_data(data, month_position_of_quarters = month_position_of_quarters) %>%
    dplyr::group_by(dimension) %>%
    dplyr::arrange(date)

  # get the month_position_of_quarters
  if (is.null(month_position_of_quarters)) {
    month_position_of_quarters <- unique(quarterly_data$month_position)
  }

  # calculate quarterly variation
  if (month_position_of_quarters == 1) {
    quarterly_data <- quarterly_data %>%
      dplyr::mutate(var1 = (t * 3) / (t_1 + t_2 + t_3) - 1)
    print("Un acquis est calculé au mois 1 puisque le month_position_of_quarters = 1.")
  } else if (month_position_of_quarters == 2) {
    quarterly_data <- quarterly_data %>%
      dplyr::mutate(var1 = (t * 2 + t_1) / (t_2 + t_3 + t_4) - 1)
    print("Un acquis est calculé au mois 2 puisque le month_position_of_quarters = 1.")
  } else {
    # this is the case month_position_of_quarters == 3
    quarterly_data <- quarterly_data %>%
      dplyr::mutate(var1 = (t + t_1 + t_2) / (t_3 + t_4 + t_5) - 1)
  }

  # select the columns we need
  if (keep_level_columns) {
    if (month_position_of_quarters == 1) {
      quarterly_data <- quarterly_data %>%
        dplyr::rename(m1 = t) %>%
        dplyr::mutate(m3 = dplyr::lead(t_1, n = 1),  # if we are in the 1st month of quarter T, then the month before (t_1) is the 3rd month of quarter T-1 (so we need to lead the column by n = 1).
                      m2 = dplyr::lead(t_2, n = 1))  # if we are in the 1st month of quarter T, then two months before (t_2) is the 2nd month of quarter T-1 (so we need to lead the column by n = 1).

    } else if (month_position_of_quarters == 2) {
      quarterly_data <- quarterly_data %>%
        dplyr::rename(m2 = t,
                      m1 = t_1) %>%
        dplyr::mutate(m3 = dplyr::lead(t_2, n = 1))  # if we are in the 2nd month of quarter T, then two months before (t_2) is the 3rd month of quarter T-1 (so we need to lead the column by n = 1).
    } else {
      # this is the case month_position_of_quarters == 3
      quarterly_data <- quarterly_data %>%
        dplyr::rename(m3 = t,
                      m2 = t_1,
                      m1 = t_2)
    }
  }
  quarterly_data <- quarterly_data %>%
    dplyr::select(-matches("^t.*")) %>%
    dplyr::select(-month_position) %>%
    ungroup()

  # clean data
  if (!is.null(quarterly_variation_column_name)) {
    names(quarterly_data)[names(quarterly_data) == "var1"] <- paste("var1", quarterly_variation_column_name, sep = "_")
    names(quarterly_data)[names(quarterly_data) == "m1"] <- paste(quarterly_variation_column_name, "m1", sep = "_")
    names(quarterly_data)[names(quarterly_data) == "m2"] <- paste(quarterly_variation_column_name, "m2", sep = "_")
    names(quarterly_data)[names(quarterly_data) == "m3"] <- paste(quarterly_variation_column_name, "m3", sep = "_")
  }

  return(quarterly_data)
}

month_to_quarter_for_nonrevised_data <- function(data, month_position_of_quarters = NULL) {
  # TODO: do the case for month_position_of_quarters = 0; not by default
  quarterly_data <- data %>%
    dplyr::mutate(month = lubridate::month(date),
                  month_position = dplyr::case_when(month %in% c(1, 4, 7, 10) ~ 1,
                                                    month %in% c(2, 5, 8, 11) ~ 2,
                                                    month %in% c(3, 6, 9, 12) ~ 3))

  # define which month position we want to keep
  if (is.null(month_position_of_quarters)) {
    month_position_of_quarters <- quarterly_data$month_position[quarterly_data$date == max(unique(quarterly_data$date))]
  } else {
    # ensure that month_position_of_quarters %in% c(1,2,3)
    if (!(month_position_of_quarters %in% c(1, 2, 3))) {
      stop("In the function get_quarterly_variation_for_nonrevised_monthly_data(), month_position_of_quarters argument must be 1, 2 or 3.")
    }
  }

  quarterly_data <- quarterly_data %>%
    dplyr::filter(month_position == month_position_of_quarters) %>%
    select(-month) %>%
    monthly_date2quarterly_date()

  message("\nUne colonne a été ajoutée dans ces données trimestrielles pour savoir, pour chaque trimestre, quelle est la position du mois.
  C'est-à-dire si month_position = 2, alors la colonne t correspond aux données du 2e mois de chaque trimestre, la colonne t_1 correspond aux données du 1er mois de chaque trimestre,
  la colonne t_2 correspond aux données du 3e mois du trimestre précédent de chaque trimestre, etc.
  Si cette colonne n'est pas utile, la supprimer.")

  return(quarterly_data)
}

keep_available_months_in_survey_data <- function(data_split_by_month, insee_month_position, pmi_month_position) {
  message("Actuellement, cette fonction ne marche que pour les données Insee et PMI.")
  dimensions <- unique(data_split_by_month$dimension)

  # define the regex
  insee_regex <- convert_month_position_to_subset_regex(insee_month_position)
  pmi_regex <- convert_month_position_to_subset_regex(pmi_month_position)
  complete_regex <- paste0("(insee.*_m(", insee_regex, ")$)|(pmi.*_m(", pmi_regex, ")$)")

  # select the available dimensions
  available_dimensions <- stringr::str_subset(dimensions, complete_regex)

  return(data_split_by_month %>% dplyr::filter(dimension %in% available_dimensions))
}

convert_month_position_to_subset_regex <- function(month_position) {
  if (month_position == 1) {
    return("1")
  } else if (month_position == 2) {
    return("1|2")
  } else if (month_position == 3) {
    return(".")
  } else {
    stop("The available month_position are: 1, 2 and 3 even if that is the month 2 or 3 of the quarter T-1; you will then use lags.")
  }
}


day_to_month <- function(data, transformation_type) {
  monthly_data <- data

  if (!(transformation_type %in% c("mean", "carry_over_mean"))) {
    stop("Choisissez \"mean\" ou \"carry_over_mean\" comme argument transformation_type pour la fonction day_to_month().")
  }

  # perform the transformation
  if (transformation_type == "carry_over_mean") {  # TODO: does not work perfectly, needs improvements (does not consider that the data frequency is working day and not everyday)
    # when a month is incomplete, the last value observed is imputed to the missing values and then the monthly mean is calculated
    # otherwise, for "mean": when a month is incomplete, the monthly mean is imputed to the missing values

    # define the extension period
    month_end_date <- lubridate::ceiling_date(max(unique(monthly_data$date)), unit = "month", change_on_boundary = FALSE) - days(1)  # because of the rounding, we need to remove one day, otherwise we are at the beginning of the next month
    extended_dates <- seq.Date(from = as.Date(min(unique(monthly_data$date))),
                               to = lubridate::ymd(month_end_date),
                               by = "day")
    empty_dataset_with_extended_dates <- data.frame("date" = rep(extended_dates, times = length(unique(monthly_data$dimension))),
                                                    "dimension" = rep(unique(monthly_data$dimension), each = length(extended_dates)))

    # extend the data
    monthly_data <- monthly_data %>%
      dplyr::full_join(empty_dataset_with_extended_dates, by = c("date", "dimension")) %>%  # add the dates we need at the end of the last month for which we have data
      fill_only_last_values(value)
  }

  # perform the monthly mean
  monthly_data <- monthly_data %>%
    dplyr::mutate(monthly_date = lubridate::floor_date(date, unit = "month")) %>%
    dplyr::group_by(monthly_date, dimension) %>%
    dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::rename(date = monthly_date)

  return(monthly_data)
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

convert_to_long_format <- function(wide_format_data) {
  columns_to_pivot <- names(wide_format_data)[names(wide_format_data) != "date"]
  if ("dimenion" %in% columns_to_pivot) {
    wide_format_data <- wide_format_data %>%
      dplyr::rename(primary_dimension = dimension)
    columns_to_pivot <- c("primary_dimension", columns_to_pivot[columns_to_pivot != "dimension"])
  }

  long_format <- wide_format_data %>%
    tidyr::pivot_longer(cols = columns_to_pivot,
                        names_to = "dimension",
                        values_to = "value")
  return(long_format)
}

# functions ------------------------------------------------------------------------------------------------------------

month_to_quarter <- function(data, transformation_type) {
  # Mixing monthly and quarterly data is a bad idea => we transform all the dimensions to quarter
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

# TODO: to delete => these functions do not properly take into account the nonrevised data. See function month_to_quarter_for_nonrevised_data()
# split_data_in_quarter_for_vintage <- function(data, lag_quarter = FALSE) {
#   # check only one dimension # TODO: make it compatible for several dimensions
#   sole_dimension <- unique(data$dimension)
#   if (length(sole_dimension) != 1) {
#     stop("Data must have only one dimension for \"split_data_in_quarter_for_vintage\" in the month_to_quarter() function.")
#   }
#   split_data <- data %>%
#     dplyr::mutate(month = lubridate::month(date),
#                   month_position = dplyr::case_when(month %in% c(1, 4, 7, 10) ~ 1,
#                                                     month %in% c(2, 5, 8, 11) ~ 2,
#                                                     month %in% c(3, 6, 9, 12) ~ 3))
#
#   # keep the quarterly dates
#   month_of_last_quarter <- split_data$month_position[split_data$date == max(unique(split_data$date))]
#   split_data <- split_data %>%
#     dplyr::filter(month_position == month_of_last_quarter) %>%
#     dplyr::select(-c(month, month_position, dimension)) %>%
#     monthly_date2quarterly_date()
#
#   # define the new column names
#   column_names <- define_column_names_for_vintage_data(dimension = sole_dimension, month_position = month_of_last_quarter, number_data_columns = length(split_data) - 1,
#                                                        lag_quarter = lag_quarter)
#   names(split_data) <- column_names
#
#   return(split_data)
# }
#
# define_column_names_for_vintage_data <- function(dimension, month_position, number_data_columns, lag_quarter) {
#   if (lag_quarter) {
#     data_columns <- paste0(dimension, "_m", month_position -
#       3 -
#       seq(from = 0, to = (number_data_columns - 1)))
#     message("Puisque l'option lag_quater a été mise à TRUE, le mois 3 du trimestre T est nommé ipi_m0. \nIl faut donc avancer les colonnes d'un trimestre (avec la fonction lead()) pour que l'ipi_m0 corresponde bien au m0 du trimestre T+1.")
#   } else {
#     data_columns <- paste0(dimension, "_m", month_position - seq(from = 0, to = (number_data_columns - 1)))
#   }
#   column_names <- c("date", data_columns)
#   column_names <- stringr::str_replace(column_names, pattern = "-", replacement = "_")
#   return(column_names)
# }

get_transformation_function <- function(transformation_type) {
  if (transformation_type == "sum") {
    return(base::sum)
  } else if (transformation_type == "mean") {
    return(base::mean)
  } else {
    stop("Choisissez \"sum\" ou \"mean\" comme argument transformation_type pour la fonction get_transformation_function().")
  }
}


get_variation_for <- function(data, variation_type, nbr_lag = 1, nbr_lead = 1, dimensions_to_transform = NULL, dimensions_to_exclude = NULL, add_option = FALSE, keep_prefix = FALSE, prefix = NULL) {
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
    if (is.null(prefix)) { prefix <- paste0("diff", nbr_lag) }
    data_with_variation <- data_with_variation %>%
      dplyr::group_by(dimension) %>%
      dplyr::mutate(variation = value - dplyr::lag(value, n = nbr_lag)) %>%
      dplyr::ungroup()
  } else if (variation_type == "growth_rate") {
    if (is.null(prefix)) { prefix <- paste0("var", nbr_lag) }
    data_with_variation <- data_with_variation %>%
      dplyr::group_by(dimension) %>%
      dplyr::mutate(variation = (value / dplyr::lag(value, n = nbr_lag)) - 1) %>%
      dplyr::ungroup()
  } else if (variation_type == "lag") {
    if (is.null(prefix)) { prefix <- paste0("lag", nbr_lag) }
    data_with_variation <- data_with_variation %>%
      dplyr::group_by(dimension) %>%
      dplyr::mutate(variation = dplyr::lag(value, n = nbr_lag)) %>%
      dplyr::ungroup()
  } else if (variation_type == "lead") {
    if (is.null(prefix)) { prefix <- paste0("lead", nbr_lead) }
    data_with_variation <- data_with_variation %>%
      dplyr::group_by(dimension) %>%
      dplyr::mutate(variation = dplyr::lead(value, n = nbr_lead)) %>%
      dplyr::ungroup()
  } else {
    stop("Choisissez \"difference\", \"growth_rate\", \"lag\" ou \"lead\" comme argument de variation_type pour la fonction get_variation_for().")
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
    if (add_option) {
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