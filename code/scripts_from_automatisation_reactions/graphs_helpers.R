# Created by: lphung
# Created on: 13/05/2022
# last update: 27/01/2023

# libraries --------------------------------------------------------------------

library(dplyr)
library(lubridate)
library(stringr)

# design graph axes -------------------------------------------------------------

get_y_scale_breaks <- function(horizontal_line, y_min, y_max, y_breaks, data_to_plot) {
  y_min <- ifelse(is.null(y_min), min(data_to_plot$value, na.rm = TRUE), y_min)
  y_max <- ifelse(is.null(y_max), max(data_to_plot$value, na.rm = TRUE), y_max)

  # needed constant
  gap_min_max <- y_max - y_min

  if (is.null(y_breaks)) {
    # define an approximated y_breaks, if not given
    y_breaks <- gap_min_max / 6  # we arbitrarily decide to have 6 breaks on the graph if nothing is specify

    # get the ideal y_breaks rounding value
    y_breaks <- get_the_ideal_y_breaks(y_breaks)
  }

  # get the break sequence
  if (is.null(horizontal_line)) {
    ratio_min_breaks <- round(y_min / y_breaks, 0)
    number_breaks <- round(gap_min_max / y_breaks, 0)
    lower_bound <- ifelse(ratio_min_breaks == 0, y_breaks, y_breaks * ratio_min_breaks)
    upper_bound <- lower_bound + y_breaks * number_breaks
    break_sequence <- seq(from = lower_bound, to = upper_bound, by = y_breaks)
  } else {
    gap_max_line <- y_max - horizontal_line
    number_upper_breaks <- round(gap_max_line / y_breaks, 0)
    upper_bound <- horizontal_line + y_breaks * number_upper_breaks
    gap_min_line <- horizontal_line - y_min
    number_lower_breaks <- round(gap_min_line / y_breaks, 0)
    lower_bound <- horizontal_line - y_breaks * number_lower_breaks
    break_sequence <- seq(from = lower_bound, to = upper_bound, by = y_breaks)
  }
  return(break_sequence)
}

get_the_ideal_y_breaks <- function(y_breaks) {
  rounding_value <- convert_digits_to_rounding_value(y_breaks)
  power <- convert_rounding_value_to_power(rounding_value)
  ideal_scale_breaks <- c(1, 2, 5, 10) * power
  return(get_the_closest_value(y_breaks, ideal_scale_breaks))
}

get_the_closest_value <- function(number, vector_of_numbers){
  df_numbers <- data.frame(numbers = vector_of_numbers)
  df_numbers <- df_numbers %>%
    dplyr::mutate(gap = abs(numbers - number))

  min_gap <- min(df_numbers$gap)

  return(df_numbers$numbers[df_numbers$gap == min_gap])
}

convert_digits_to_rounding_value <- function(number) {
  if (stringr::str_detect(number, "^0.")) {
    decimals <- stringr::str_replace(number, "(^0\\.)(.*)", "\\2")
    return(nchar(decimals))
  } else {
    integer <- stringr::str_replace(number, "([:digit:]*)\\.([:digit:]*)", "\\1")
    return(-nchar(integer) + 1)
  }
}

convert_rounding_value_to_power <- function(rounding_value) {
  power <- as.numeric(paste0("1e", -rounding_value))
  return(power)
}

get_x_date_scale_elements <- function(date_breaks, date_labels, date_column) {
  date_scale_elements <- list("date_breaks" = NA, "date_labels" = NA, "date_limits" = c(NA, NA), "expand" = TRUE) # the default "expand" argument should be TRUE according to data vizualisation theory

  min_date <- min(unique(date_column))
  max_date <- max(unique(date_column))
  gap_min_max <- lubridate::interval(min_date, max_date) %/% months(1)
  gap_min_max_in_years <- round(gap_min_max / 12, 0)

  if (!is.null(date_breaks)) {
    date_scale_elements[["date_breaks"]] <- date_breaks
    date_scale_elements[["date_labels"]] <- ifelse(is.null(date_labels), "%m/%y", date_labels)
    date_scale_elements[["date_limits"]] <- get_date_limits(date_breaks, min_date)[["date_limits"]]
    date_scale_elements[["expand"]] <- get_date_limits(date_breaks, min_date)[["expand"]]
    return(date_scale_elements)
  }

  if (gap_min_max_in_years > 8) {
    date_scale_elements[["date_breaks"]] <- "1 year"
    date_scale_elements[["date_labels"]] <- "%Y"
  } else if (gap_min_max_in_years %in% 6:8) {
    date_scale_elements[["date_breaks"]] <- "6 months"
    date_scale_elements[["date_labels"]] <- "%m/%y"
  } else if (gap_min_max_in_years %in% 4:5) {
    date_scale_elements[["date_breaks"]] <- "6 months"
    date_scale_elements[["date_labels"]] <- "%b %y"
  } else if (gap_min_max_in_years %in% 2:3) {
    date_scale_elements[["date_breaks"]] <- "3 months"
    date_scale_elements[["date_labels"]] <- "%b %y"
  } else if (gap_min_max_in_years == 1) {
    date_scale_elements[["date_breaks"]] <- "2 months"
    date_scale_elements[["date_labels"]] <- "%b %y"
  } else if (gap_min_max_in_years < 1) {
    date_scale_elements[["date_breaks"]] <- "1 month"
    date_scale_elements[["date_labels"]] <- "%b %Y"
  }

  # check if a date_labels was not already define
  date_scale_elements[["date_labels"]] <- ifelse(is.null(date_labels), date_scale_elements[["date_labels"]], date_labels)

  # # define axis' limits
  # ## Note: due to difficulties (cf. Note for the get_date_limits() function, we decide to simplify the rule and not expand the axis in any case, though we should have)
  date_scale_elements[["date_limits"]] <- get_date_limits(date_scale_elements[["date_breaks"]], min_date)[["date_limits"]]
  date_scale_elements[["expand"]] <- get_date_limits(date_scale_elements[["date_breaks"]], min_date)[["expand"]]


  return(date_scale_elements)
}

get_date_limits <- function(date_breaks, min_date) {
  # Note: if we do expand the axis, for months appearing in the non_expansion_cases, substracting one month is necessary
  # to see the floor date appearing as the first month break on the plot if the time range is sufficiently large, otherwise
  # we need to substract different amount of months according to the time range; we a rule complicated to reproduce.
  # Though, we decide to not expand the axis at all
  date_limits_elements <- list("date_limits" = c(NA, NA), "expand" = FALSE)    # Note, default case should be with expand = TRUE, but, see Note, we put FALSE
  starting_date <- lubridate::floor_date(min_date, date_breaks)

  # min_date_month <- lubridate::month(min_date)
  # non_expansion_cases <- list("1 year" = c(1),
  #                             "6 months" = c(1, 7),
  #                             "3 months" = c(1, 4, 7, 10),
  #                             "2 months" = c(1, 3, 5, 7, 9, 11))
  #
  # if (date_breaks %in% names(non_expansion_cases) && !(min_date_month %in% non_expansion_cases[[date_breaks]])) {
  #   date_limits_elements[["expand"]] <- FALSE
  #   date_limits_elements[["date_limits"]] <- c(starting_date, NA)
  # } else {
  #   date_limits_elements[["date_limits"]] <- c(starting_date - months(1), NA)  # see function's note
  # }

  date_limits_elements[["date_limits"]] <- c(starting_date, NA)

  return(date_limits_elements)
}

# format axes ----------------------------------------------------------------------------
format_dates_on_two_lines <- function(x) {
  months <- stringr::str_sub(x, start = 6, end = 7)             # get number of the month
  years <- lubridate::year(x)                                   # get the year
  if_else((is.na(lag(years)) & as.numeric(months) > 6) |
            months == "06" |
            is.na(lead(years)),  # Conditions for pasting.
          true = paste(months, years, sep = "\n"),
          false = months)
}

format_variations_with_point <- function(x) {
  paste(x, "pt")
}