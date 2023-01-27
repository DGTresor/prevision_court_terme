# Created by: lphung
# Created on: 13/05/2022

# libraries --------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtext)
library(grDevices)
library(lubridate)
library(devEMF)

# Dependancy to:
source("./graph_design_parameters.R", encoding = "utf-8")

# graph functions --------------------------------------------------------------------

# TODO : to use
# annotate("text", x = XTBG, y = YTBG, label = texte.bas.gauche.texte , col="black", size=texte.bas.gauche.taille, hjust = 0)

business_sentiment_indices_graph <- function(graph_name, data, graph_folder, graph_start_date = NULL, graph_end_date = NULL,
                                             dimensions_to_plot = NULL, main_dimension = "", label_list = NULL, title = "", graph_source = "",
                                             color_list_name = "DGTresor_colors", horizontal_line = NULL,
                                             y_min = NA, y_max = NA, date_breaks = "1 year", date_labels = "%Y",
                                             graph_saving = TRUE, graph_saving_format = "emf") {
  # preparing the constants
  ## get the dimensions to plot
  if (is.null(dimensions_to_plot)) {
    dimensions_to_plot <- unique(data$dimension)
  }
  # get the plot period
  if (is.null(graph_start_date)) {
    graph_start_date <- ymd(min(unique(data$date)))
  } else {
    graph_start_date <- ymd(graph_start_date)
  }
  if (is.null(graph_end_date)) {
    graph_end_date <- ymd(max(unique(data$date)))
  } else {
    graph_end_date <- ymd(graph_end_date)
  }

  # get the colors
  color_palette <- return_color_palette(color_list_name = color_list_name, nb_dimensions = length(dimensions_to_plot[dimensions_to_plot != main_dimension]))

  # keep the data we need for the plot
  data_to_plot <- data %>%
    filter(dimension %in% dimensions_to_plot,
           date >= graph_start_date & date <= graph_end_date)

  # plot the basic graph
  indices_plot <- ggplot(data_to_plot) +
    geom_line(data = data_to_plot %>% filter(dimension != main_dimension),
              aes(x = date, y = value, color = dimension)) +
    scale_x_date(date_breaks = date_breaks, date_labels = date_labels) +
    my_theme() +
    theme(legend.position = "bottom")

  # add an horizontal line
  if (!is.null(horizontal_line)) {
    indices_plot <- indices_plot +
      geom_hline(yintercept = horizontal_line, col = "black", size = 0.5)
  }

  # add the curve for the main_dimension, if there is one
  indices_plot <- indices_plot +
    geom_line(data = data_to_plot %>% filter(dimension == main_dimension),
              aes(x = date, y = value, linetype = dimension),
              col = "black")                                                  # put the main dimension in black


  # modify the lines' colors and order the legend items
  if (!is.null(label_list)) {  # dans le cas où une liste de label a été fournie // can overwrite the use of the label column
    ordered_labels <- mapper(dimensions_to_plot[dimensions_to_plot != main_dimension], label_list)
    indices_plot <- indices_plot +
      scale_color_manual(breaks = dimensions_to_plot[dimensions_to_plot != main_dimension],
                         labels = ordered_labels,
                         palette = color_palette) +
      scale_linetype_manual(values = "longdash",                    # put a dashed line for the main dimension
                            labels = label_list[[main_dimension]])
  } else if ("label" %in% names(data_to_plot)) {  # dans le cas où une colonne de label existe dans le jeu de données
    data_breaks <- dimensions_to_plot[dimensions_to_plot != main_dimension]
    data_labels <- create_label_vector_based_on_dimension_and_label_columns(data_to_plot = data_to_plot, dimensions_to_plot = data_breaks)
    indices_plot <- indices_plot +
      scale_color_manual(breaks = data_breaks,
                         labels = data_labels,
                         palette = color_palette) +
      scale_linetype_manual(values = "longdash",                    # put a dashed line for the main dimension
                            labels = unique(data_to_plot$label[data_to_plot$dimension == main_dimension]))
  } else {
    indices_plot <- indices_plot +
      scale_color_manual(palette = color_palette) +
      scale_linetype_manual(values = "longdash")                   # put a dashed line for the main dimension
  }

  # modify the scale of the y axis
  if (!is.na(y_max) & !is.na(y_min)) {
    indices_plot <- indices_plot +
      scale_y_continuous(limits = c(as.numeric(y_min), as.numeric(y_max)))
  }

  # add the labels
  # Note : can either use caption or tag for the "Derniers points" et "Source"
  # text_for_scale_used <- paste()
  indices_plot <- indices_plot +
    labs(title = title,
         # subtitle = text_for_scale_used,
         caption = paste0(paste("Derniers points :", month(graph_end_date, label = TRUE, abbr = FALSE), year(graph_end_date)),
                          paste("\nSource :", graph_source)))

  # saving and returning the graph
  if (graph_saving) {
    if (graph_saving_format == "emf") {
      save_in_emf_format(indices_plot, file.path(graph_folder, paste0(graph_name, ".emf")))
    } else {
      ggplot2::ggsave(filename = file.path(graph_folder, paste0(graph_name, ".", graph_saving_format)), plot = indices_plot)
    }
  }
  return(indices_plot)
  #TODO: check if returning a list with several objects (e.g. graph_start_date, graph_end_date, data_to_plot) could be efficient to create other functions on top adding more layers to the graph
}

compare_insee_pmi_bdf_for_one_index_graph <- function(graph_name, graph_folder, insee_data = NULL, bdf_data = NULL, pmi_data = NULL,
                                                      graph_start_date = NULL, label_list = NULL,
                                                      title = "", graph_source = "", color_list_name = "DGTresor_colors",
                                                      y_min = NA, y_max = NA, date_breaks = "1 year", date_labels = "%Y",
                                                      graph_saving = TRUE, graph_saving_format = "emf", reduced_centered = "no") {

  # merge the data from the different providers
  merged_data <- concatanate_insee_pmi_bdf_data_with_one_dimension(insee_data = insee_data, bdf_data = bdf_data, pmi_data = pmi_data, reduced_centered = reduced_centered)
  dimensions_to_plot <- unique(merged_data$dimension)

  # preparing the constants
  ## get the plot period
  if (is.null(graph_start_date)) {
    graph_start_date <- ymd(min(unique(merged_data$date)))
  } else {
    graph_start_date <- ymd(graph_start_date)
  }

  # get the colors
  color_palette <- return_color_palette(color_list_name = color_list_name, nb_dimensions = length(dimensions_to_plot))

  # keep the data we need for the plot
  data_to_plot <- merged_data %>%
    filter(date >= graph_start_date)
  if (reduced_centered == "no") {
    data_to_plot <- data_to_plot %>%
      multiply_by_2_pmi_dimension(dimensions_list = dimensions_to_plot)
    horizontal_line <- 100
  } else {
    horizontal_line <- 0
  }

  # plot the basic graph
  indices_plot <- ggplot(data_to_plot) +
    geom_line(data = data_to_plot,
              aes(x = date, y = value, color = dimension)) +
    scale_x_date(date_breaks = date_breaks, date_labels = date_labels
                 # expand = expansion()                                  # ensure that the plot starts at the axis
    ) +
    my_theme() +
    theme(legend.position = "bottom") +
    geom_hline(yintercept = horizontal_line, col = "black", size = 0.5)               # add an horizontal line

  # create a second axis for PMI data if data has not been mean-centered
  if (!is.null(pmi_data) & reduced_centered == "no") {
    indices_plot <- indices_plot +
      scale_y_continuous(sec.axis = (sec.axis = sec_axis(~. / 2)))
  }

  # modify the lines' colors
  if (length(label_list) == 0) {
    label_list <- list("insee" = "Insee", "bdf" = "Banque de France", "pmi" = "PMI")
  }
  ordered_labels <- mapper(dimensions_to_plot, label_list)
  indices_plot <- indices_plot +
    scale_color_manual(breaks = dimensions_to_plot,
                       labels = ordered_labels,
                       palette = color_palette)

  # # modify the scale of the y axis
  # if (!is.na(y_max) & !is.na(y_min)) {
  #   indices_plot <- indices_plot +
  #     scale_y_continuous(limits = c(as.numeric(y_min), as.numeric(y_max)))
  # }

  # add the labels
  # Note : can either use caption or tag for the "Derniers points" et "Source"
  text_for_scale_used <- get_scale_text_for_compare_insee_pmi_bdf(dimensions_list = dimensions_to_plot, reduced_centered = reduced_centered)
  text_for_date <- get_date_text_for_compare_insee_pmi_bdf(merged_data = merged_data, dimensions_list = dimensions_to_plot)
  indices_plot <- indices_plot +
    labs(title = title,
         subtitle = text_for_scale_used,
         caption = paste0(paste("Derniers points :", text_for_date),
                          paste("\nSource :", graph_source)))

  # saving and returning the graph
  if (graph_saving) {
    if (graph_saving_format == "emf") {
      save_in_emf_format(indices_plot, file.path(graph_folder, paste0(graph_name, ".emf")))
    } else {
      ggplot2::ggsave(filename = file.path(graph_folder, paste0(graph_name, ".", graph_saving_format)), plot = indices_plot)
    }
  }
  return(indices_plot)
}


# support functions ----------------------------------------------------------------------------------------------------

save_in_emf_format <- function(graph, graph_name, graph_width = 10, graph_height = 5) {
  emf(file = graph_name, # name of the graph to save
      width = graph_width, # in inches
      height = graph_height # in inches
  )
  print(graph)
  dev.off()
}

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

textbox_in_bold_or_italic <- function(my_text, bold = FALSE, italic = FALSE) {
  if (!bold & !italic) {
    return(my_text)
  } else if (!bold & italic) {
    return(paste0("<i>", my_text)) # HTML tag for italic
  } else if (bold & !italic) {
    return(paste0("<b>", my_text)) # HTML tag for bold
  } else {
    return(paste0("<b><i>", my_text))
  }
}

concatanate_one_dimension_dataframes <- function(named_business_sentiments_list) {
  # the named_business_sentiments_list must contain a named list of dataframes containing only one dimension, i.e. one business sentiment or balance

  if (length(named_business_sentiments_list) %in% c(0, 1)) {
    stop("Vous avez fourni un seul (ou zéro) indicateur à la fonction join_business_sentiments_data(), l'usage de cette fonction n'est donc pas utile.")
  } else {

    # first check
    ensure_there_is_only_one_dimension(named_business_sentiments_list)

    # deal with the first business sentiment
    merged_data <- named_business_sentiments_list[[1]] %>%
      select(date, value) %>%
      mutate(dimension = names(named_business_sentiments_list)[1])

    # deal with the other business sentiments
    for (business_sentiment in 2:length(named_business_sentiments_list)) {

      business_sentiment_data <- named_business_sentiments_list[[business_sentiment]] %>%
        select(date, value) %>%
        mutate(dimension = names(named_business_sentiments_list)[business_sentiment])

      merged_data <- merged_data %>%
        bind_rows(business_sentiment_data)
    }
    return(merged_data)
  }
}

ensure_there_is_only_one_dimension <- function(data_list) {
  for (data in 1:length(data_list)) {
    if (length(unique(data_list[[data]]$dimension)) != 1) {
      stop("Chaque dataframe dans la liste ne doit contenir qu'une seule dimension, i.e. qu'un seul indicateur.")
    }
  }
}

concatanate_insee_pmi_bdf_data_with_one_dimension <- function(insee_data, bdf_data, pmi_data, reduced_centered = "no") {
  # create the data list and transform, if needed, the series to make them comparable
  data_list <- list()
  if (!is.null(insee_data)) {
    data_list[["insee"]] <- center_and_reduce_data(insee_data, data_source = "insee", reduced_centered_type = get_reduced_centered_type(reduced_centered = reduced_centered, data_source = "insee")) }
  if (!is.null(bdf_data)) {
    data_list[["bdf"]] <- center_and_reduce_data(bdf_data, data_source = "bdf", reduced_centered_type = get_reduced_centered_type(reduced_centered = reduced_centered, data_source = "bdf")) }
  if (!is.null(pmi_data)) {
    data_list[["pmi"]] <- center_and_reduce_data(pmi_data, data_source = "pmi", reduced_centered_type = get_reduced_centered_type(reduced_centered = reduced_centered, data_source = "pmi")) }
  if (length(data_list) %in% c(0, 1)) {
    stop("Donnez au moins deux séries à la fonction compare_insee_pmi_bdf_for_one_index_graph.")
  }

  # merge the data
  merged_data <- concatanate_one_dimension_dataframes(data_list)
  return(merged_data)
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

get_scale_text_for_compare_insee_pmi_bdf <- function(dimensions_list, reduced_centered) {
  if (reduced_centered == "no") {
    if ("pmi" %in% dimensions_list) {
      text_end <- "indice PMI à droite"
    } else {
      return("")
    }
    if ("insee" %in% dimensions_list) {
      if ("bdf" %in% dimensions_list) {
        return(paste0("Indices Insee et Banque de France à gauche ; ", text_end))
      } else {
        return(paste0("Indice Insee à gauche et ", text_end))
      }
    } else {
      return(paste0("Indice Banque de France à gauche et ", text_end))
    }
  } else {
    return("Ecart à la moyenne en point d'écart-type")
  }
}

get_date_text_for_compare_insee_pmi_bdf <- function(merged_data, dimensions_list) {
  last_dates <- as.Date(c())
  if ("insee" %in% dimensions_list) { last_dates <- c(last_dates, "Insee" = get_last_date_with_available_data(merged_data, dimension = "insee")) }
  if ("bdf" %in% dimensions_list) { last_dates <- c(last_dates, "BdF" = get_last_date_with_available_data(merged_data, dimension = "bdf")) }
  if ("pmi" %in% dimensions_list) { last_dates <- c(last_dates, "PMI" = get_last_date_with_available_data(merged_data, dimension = "pmi")) }

  max_date <- max(last_dates, na.rm = TRUE)
  min_date <- min(last_dates, na.rm = TRUE)

  max_date_string <- paste(month(max_date, label = TRUE, abbr = FALSE), year(max_date))

  if (min_date == max_date) {
    return(max_date_string)
  } else {
    min_date_string <- paste(month(min_date, label = TRUE, abbr = FALSE), year(min_date))

    if (length(dimensions_list) == 2) {
      return(paste0(max_date_string, " (", names(last_dates[last_dates == max_date]), ") et ",
                    min_date_string, " (", names(last_dates[last_dates == min_date]), ")"))
    } else if (length(last_dates[last_dates == max_date]) == 2) {
      return(paste0(max_date_string, " (", names(last_dates[last_dates == max_date])[1], " et ", names(last_dates[last_dates == max_date])[2], "), ",
                    min_date_string, " (", names(last_dates[last_dates == min_date]), ")"))
    } else if (length(last_dates[last_dates == min_date]) == 2) {
      return(paste0(max_date_string, " (", names(last_dates[last_dates == max_date]), ") et ",
                    min_date_string, " (", names(last_dates[last_dates == min_date])[1], " et ", names(last_dates[last_dates == min_date])[2], ")"))
    } else {
      medium_date <- last_dates[last_dates != max_date & last_dates != min_date]
      medium_date_string <- paste(month(medium_date, label = TRUE, abbr = FALSE), year(medium_date))
      return(paste0(max_date_string, " (", names(last_dates[last_dates == max_date]), "), ",
                    medium_date_string, " (", names(last_dates[last_dates == medium_date]), "), ",
                    min_date_string, " (", names(last_dates[last_dates == min_date]), ")"))

    }
  }
}

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

create_label_vector_based_on_dimension_and_label_columns <- function(data_to_plot, dimensions_to_plot) {
  label_vector <- c()

  for (dimension in dimensions_to_plot) {
    label_vector <- c(label_vector, unique(data_to_plot$label[data_to_plot$dimension == dimension]))
  }

  return(label_vector)
}