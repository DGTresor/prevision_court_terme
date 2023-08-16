# Created by: lphung
# Created on: 10/01/2023
# last update: 27/01/2023

# libraries --------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(lubridate)
library(devEMF)


# functions --------------------------------------------------------------------

put_data_in_base_100 <- function(data, base_date) {
  # extract the base values for each dimension
  data_base <- data %>%
    filter(date %in% ymd(base_date)) %>%
    group_by(dimension) %>%
    summarise(base_value = mean(value, na.rm = TRUE), .groups = "drop")

  # create a new column with the base_value
  data_with_base <- data %>%
    left_join(data_base, by = "dimension")

  # get the index base 100 for the base_date
  data_with_base <- data_with_base %>%
    mutate(indice = (value / base_value) * 100) %>%
    select(-base_value)

  return(data_with_base)
}

create_label_vector_based_on_dimension_and_label_columns <- function(data_to_plot, dimensions_to_plot) {
  label_vector <- c()

  for (dimension in dimensions_to_plot) {
    label_vector <- c(label_vector, unique(data_to_plot$label[data_to_plot$dimension == dimension]))
  }

  return(label_vector)
}

get_reduced_centered_type <- function(reduced_centered, data_source) {
  if (reduced_centered == "manual_for_pmi") {
    if (data_source %in% c("insee", "bdf")) {
      reduced_centered_type <- "default"
    } else if (data_source == "pmi") {
      reduced_centered_type <- "manual"
    } else {
      stop("Choose \"insee\", \"bdf\" or \"pmi\" as the data_source argument for the center_and_reduce_data() function.")
    }
  } else {
    reduced_centered_type <- reduced_centered
  }
  return(reduced_centered_type)
}

center_and_reduce_data <- function(data, data_source, reduced_centered_type) {
  if (reduced_centered_type == "no") {
    return(data)
  } else if (reduced_centered_type == "default") {
    scaled_data <- data %>%
      group_by(dimension) %>%
      mutate(value = scale(value)) %>%
      ungroup()
  } else if (reduced_centered_type == "manual") {
    # the "manual" option enables to mean-centered Insee and BdF business sentiments exactly around 100, and PMI indices exactly around 50
    scaled_data <- center_and_reduce_data_manually(data = data, data_source = data_source)
  } else {
    stop("Choose \"no\", \"default\", \"manual\" or \"manual_for_pmi\" as the reduced_centered argument for the center_and_reduce_data() function.")
  }
  return(scaled_data)
}

center_and_reduce_data_manually <- function(data, data_source) {
  # define the centering value
  if (data_source %in% c("insee", "bdf")) {
    mean_value <- 100
  } else if (data_source == "pmi") {
    mean_value <- 50
  } else {
    stop("Choose \"insee\", \"bdf\" or \"pmi\" as the data_source argument for the center_and_reduce_data() function.")
  }

  # rescale the data
  ## deal with the first dimension
  dimensions_list <- unique(data$dimension)
  scaled_data <- data %>%
    filter(dimension == dimensions_list[1]) %>%
    mutate(squared_diff = (value - mean_value)^2)
  nbr_value <- sum(!is.na(scaled_data$value))
  data_standard_deviation <- sqrt((1 / nbr_value) * sum(scaled_data$squared_diff))
  scaled_data <- scaled_data %>%
    mutate(value = (value - mean_value) / data_standard_deviation) %>%
    select(-squared_diff)
  rm(nbr_value, data_standard_deviation)

  ## deal with the other dimensions, if exist
  if (length(dimensions_list) > 1) {
    for (dimension in 2:length(dimensions_list)) {
      scaled_dimension <- data %>%
        filter(dimension == dimensions_list[dimension]) %>%
        mutate(squared_diff = (value - mean_value)^2)
      nbr_value <- sum(!is.na(scaled_dimension$value))
      data_standard_deviation <- sqrt((1 / nbr_value) * sum(scaled_dimension$squared_diff))
      scaled_dimension <- scaled_dimension %>%
        mutate(value = (value - mean_value) / data_standard_deviation) %>%
        select(-squared_diff)

      scaled_data <- scaled_data %>%
        bind_rows(scaled_dimension)
    }
  }
  return(scaled_data)
}

multiply_by_2_pmi_dimension <- function(merged_data, dimensions_list = NULL) {
  if (is.null(dimensions_list)) {
    dimensions_list <- unique(merged_data$dimension)
  }

  if ("pmi" %in% dimensions_list) {
    expended_data <- merged_data %>%
      select(date, dimension, value) %>%
      tidyr::pivot_wider(names_from = dimension,
                         values_from = value)
    expended_data <- expended_data %>%
      mutate(pmi = pmi * 2)

    # put data back in good format
    data_with_pmi_rescaled <- expended_data %>%
      tidyr::pivot_longer(cols = all_of(dimensions_list),
                          names_to = "dimension",
                          values_to = "value") %>%
      return(data_with_pmi_rescaled)
  } else {
    return(merged_data)
  }
}