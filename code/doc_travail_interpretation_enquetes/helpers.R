# Title     : TODO
# Objective : TODO
# Created by: lphung
# Created on: 18/01/2023

# libraries ------------------------------------------------------------------------------------------------------------
library(dplyr)
library(readr)
library(Metrics)

# dependencies
source("../nonrevised_national_accounting/loaders.R", encoding = "utf-8", chdir = TRUE)
source("../old_scripts_from_prevision_production_manuf/loading_data.R", encoding = "utf-8", chdir = TRUE)
# chdir = TRUE needed because we call this Rscript from the main.R and from a RMarkdown, which define working directory differently

# constants ------------------------------------------------------------------------------------------------------------
PATH_TO_PMI_DATA <- "S:/SPMAE/PREV/Prev3/_Fichiers_Prev3/Synthèse/6. Enquêtes PMI/4. Préparation mail réaction PMI/Données/Data PMI.xlsx"
PATH_TO_FR_DEROULEUR <- list(path = "S:/SPMAE/PREV/Prev3/_Fichiers_Prev3/Synthèse/FR_Dérouleur.xlsx",
                             sheet = "Données enquêtes")
# PATH_TO_DATA_FOR_NOWCASTING <- "./input/donnees_pour_nowcasting_en_temps_reel.xlsx"
PATH_TO_DATA_FOR_NOWCASTING <- "S:/SPMAE/PREV/Prev3/_Fichiers_Prev3/Stages/Antoine_Claisse/donnees/donnees_pour_nowcasting_en_temps_reel.xlsx"


# PIB_DIMENSIONS <- list("default" = c("pib" = "TD.PIB_7CH"))
PMI_DIMENSIONS_LIST <- list(
  ".omposite" = "composite",
  "industrie" = "industrie", # Pour "Synthétique industrie"
  "services" = "services"
)
INSEE_DIMENSIONS_LIST <- list(
  "global" = "global",
  "industrie" = "industrie",
  "services" = "services",
  "construction" = "construction"
)
BDF_DIMENSIONS_LIST <- list(
  "Climat composite" = "global",
  "industrie" = "industrie",
  "services" = "services",
  "construction" = "construction"
)

# TODO: put all the function below in data_importator.R // functions useful for the independent project of Document de travail
# functions to load data -----------------------------------------------------------------------------------------------

# most_recent_compta_nat_data_loader <- function(folder_path, file_name, dimensions_list_name, dimensions_list) {
#   file_path <- get_compta_nat_most_recent_file(folder_path, file_name)
#   suppressMessages(data <- readr::read_delim(file = file_path, delim = ";", col_names = TRUE)) # suppress messages to prevent message of columns' type
#
#   clean_data <- data_cleaner_for_csv(data, dimensions_list_name = dimensions_list_name, list_of_dimensions = dimensions_list)
#   return(clean_data)
# }
#
# get_compta_nat_most_recent_file <- function(folder_path, file_name) {
#   # Note: le file_name doit contenir l'extension, e.g. cprvolch.csv
#   national_account_base_year <- stringr::str_extract(string = folder_path, pattern = "(?<=/)base[:digit:]{4}")
#   message(paste("Le chemin du dossier pointe actuellement vers", national_account_base_year, "; Pensez à le changer si la base change."))
#   most_recent_folder <- get_the_most_recent_file(folder_path)
#   file_path <- file.path(most_recent_folder, file_name)
#   return(file_path)
# }

# get_the_most_recent_file <- function(folder_path, exclusion_list = NULL) {
#   # get all the folders' names in the folder
#   files_names <- list.files(path = folder_path, full.names = FALSE)
#
#   # remove certain specific files if needed
#   if (!is.null(exclusion_list)) {
#     files_names <- files_names[!(files_names %in% exclusion_list)]
#   }
#
#   # the current alphanumeric classification enables that the last folder is the most recent
#   most_recent_file <- file.path(folder_path, files_names[length(files_names)])
#
#   return(most_recent_file)
# }

load_data_for_nowcasting <- function(path_to_data, sheets_to_load) {
  survey_data <- NULL
  for (sheet in sheets_to_load) {
    new_data <- load_data_for_nowcasting_for_sheet(path_to_data = path_to_data, sheet = sheet)

    # deal with the specific case of pmi_industrie_production_passee, which exists in the sheets indices_synthetiques & pmi_sous_soldes
    if (sheet == "pmi_sous_soldes" && "indices_synthetiques" %in% sheets_to_load) {
      new_data <- new_data %>%
        dplyr::filter(dimension != "pmi_industrie_production_passee")
    }

    if (is.null(survey_data)) {
      survey_data <- new_data
    } else {
      survey_data <- survey_data %>%
        dplyr::bind_rows(new_data)
    }
    rm(new_data)
  }
}

load_data_for_nowcasting_for_sheet <- function(path_to_data, sheet = "indices_synthetiques") {
  data <- readxl::read_xlsx(path_to_data, sheet = sheet)
  data <- data[3:nrow(data),] %>%
    dplyr::mutate(date = as.Date(as.numeric(date), origin = "1900-01-01") - days(2)) %>%
    dplyr::mutate_if(is.character, as.numeric) %>%
    tidyr::pivot_longer(cols = names(data)[names(data) != "date"],
                        values_to = "value",
                        names_to = "dimension")
  return(data)
}

load_pmi_data_from_excel_all_dimensions <- function(path_to_data, column_list, dimensions_label = NULL) {
  suppressMessages(imported_data <- readxl::read_xlsx(path = path_to_data, sheet = "France", skip = 1)[-c(1:4),]) # suppress messages to prevent message of columns' type and column renaming
  clean_data <- excel_dataframe_cleaner(imported_data, column_list)
  # pmi_industry <- clean_data[, c(1, 3:11)]  # keep only the columns corresponding to the indstry survey's balances

  # all balances are in columns => need to do a pivot
  clean_data <- prepare_balances_data(clean_data, data_source = "pmi")

  # add labels if exist and reorganise the dataframe
  if (!is.null(dimensions_label)) {
    clean_data$label <- mapper(clean_data$dimension, dimensions_label)
    clean_data <- clean_data %>%
      dplyr::select(date, dimension, label, value)
  }
  return(clean_data)
}

fr_derouleur_importator <- function(path_to_climat_data, column_list, data_source, columns_to_import) {

  suppressMessages(imported_climat <- readxl::read_xlsx(path = path_to_climat_data[["path"]], sheet = path_to_climat_data[["sheet"]], skip = 1)[-c(1), columns_to_import]) # suppress messages to prevent message of columns' type
  clean_data <- excel_dataframe_cleaner(imported_climat, column_list)

  # all balances are in columns => need to do a pivot
  clean_data <- prepare_balances_data(clean_data, data_source = data_source)

  return(clean_data)
}

# functions for identifying outliers -----------------------------------------------------------------------------------

identify_outliers <- function(data, variable_name, standard_deviation_multiplier = 1, exclusion_dates = c(), robust_estimates = FALSE) {
  # Note : the variable must be in column
  subset_data <- data %>%
    dplyr::filter(!(date %in% exclusion_dates))

  if (robust_estimates) {
    # For normal distribution, a robust estimate of the mean is the median AND a robust estimate of the standard deviation is 1.4826 * MAD (Median Absolute Deviation)
    # Note : the stats::mad() computes directly the estimate of the standard deviation (so no need to multiply it by 1.4826)
    # Source : https://en.wikipedia.org/wiki/Robust_measures_of_scale
    variable_standard_deviation <- stats::mad(subset_data[[variable_name]])
    variable_mean <- median(subset_data[[variable_name]])

  } else {
    variable_standard_deviation <- sd(subset_data[[variable_name]])
    variable_mean <- mean(subset_data[[variable_name]])
  }

  # On utilise suppressWarnings() car quand aucune date ne sera compatible avec le filtre, R retournera un Warning.
  suppressWarnings(outliers_dates <- c(subset_data$date[subset_data[[variable_name]] < (variable_mean - standard_deviation_multiplier * variable_standard_deviation)],
                                       subset_data$date[subset_data[[variable_name]] > (variable_mean + standard_deviation_multiplier * variable_standard_deviation)]))
  return(list("outliers_dates" = outliers_dates, "mean" = variable_mean, "standard_deviation" = variable_standard_deviation))
}

identify_outliers_iteratively <- function(data, variable_name, standard_deviation_multiplier = 1, max_nb_tour = 10) {
  # 1er iteration
  outliers_dates <- identify_outliers(data, variable_name, standard_deviation_multiplier)[["outliers_dates"]]
  all_outliers_dates <- outliers_dates
  new_outliers_dates <- outliers_dates
  nb_tour <- 1

  while (!(length(new_outliers_dates) == 0 | nb_tour >= max_nb_tour)) {
    outliers_info <- identify_outliers(data, variable_name, standard_deviation_multiplier, all_outliers_dates)
    new_outliers_dates <- outliers_info[["outliers_dates"]]
    all_outliers_dates <- c(all_outliers_dates, new_outliers_dates)
    nb_tour <- nb_tour + 1
  }
  if (length(new_outliers_dates) == 0) {
    print(paste("La convergence a été obtenue en :", nb_tour, "tour(s)."))
  } else {
    print(paste("L'itération a été arrêtée au bout de :", nb_tour, "tour(s) comme spécifié avec l'argument max_nb_tour."))
  }
  return(list("outliers_dates" = all_outliers_dates, "mean" = outliers_info[["mean"]], "standard_deviation" = outliers_info[["standard_deviation"]]))
}

# functions for correlations with rolling window ----------------------------------------------------------------------

compute_correlations_with_rolling_window <- function(data, variables_of_interest, window_size, cor_na_treatment = "everything", cor_method = "pearson") {
  # First occurence
  first_correlations <- cor(data[1:window_size,] %>% dplyr::select(-date), use = cor_na_treatment, method = cor_method)[, variables_of_interest]
  rolling_correlations <- data.frame(first_correlations,
                                     dimension = row.names(first_correlations),
                                     date = rep(data$date[[window_size]], length(row.names(first_correlations))))

  # Following occurences
  for (i in (window_size + 1):nrow(data)) {
    correlations <- cor(data[(i - window_size + 1):i,] %>% dplyr::select(-date), use = cor_na_treatment, method = cor_method)[, variables_of_interest]
    new_df <- data.frame(correlations,
                         dimension = row.names(correlations),
                         date = rep(data$date[[i]], length(row.names(first_correlations))))
    rolling_correlations <- rolling_correlations %>%
      dplyr::bind_rows(new_df)
  }
  return(rolling_correlations)
}

create_table_correlation_pib <- function(correlation_data) {
  table <- correlation_data %>%
    dplyr::group_by(dimension) %>%
    dplyr::summarise(var1_PIB = mean(var1_PIB), var4_PIB = mean(var4_PIB)) %>%
    dplyr::filter(!(dimension %in% c("var1_PIB", "var4_PIB")))
  return(table)
}

plot_graph_evol_correlation_pib <- function(correlation_data, pib_var_name, window_size, date_period_in_string_for_title, nb_dimensions = NULL, dimensions_label_list = NULL) {
  # préparer les données à plotter et le nombre de dimensions pour la palette de couleur
  plot_data <- correlation_data %>% dplyr::filter(!(dimension %in% c("var1_PIB", "var4_PIB")))
  if (is.null(nb_dimensions)) {
    nb_dimensions <- length(unique(plot_data$dimension))
  }
  color_palette_dimensions <- color_palette_for(color_list_name = "FR_derouleur", nb_dimensions = nb_dimensions)

  # préparer le titre du graphique et vérifier que l'argument pib_var_name est correct
  if (pib_var_name == "var1_PIB") {
    plot_title <- "la variation trimestrielle du PIB"
  } else if (pib_var_name == "var4_PIB") {
    plot_title <- "le glissement annuel du PIB"
  } else {
    stop("L'argument pib_var_name de la fonction plot_graph_evol_correlation_pib() peut être : \"var1_PIB\" ou \"var4_PIB\".")
  }

  # réaliser le graphique
  plot <- ggplot(plot_data) +
    aes(x = date, y = var1_PIB, color = dimension) +
    geom_line() +
    labs(title = paste("Evolution dans le temps de la corrélation entre les climats et", plot_title),
         subtitle = paste("Estimation roulante réalisée sur fenêtres de", window_size / 4, "ans sur la période", date_period_in_string_for_title),
         caption = "Source : Insee, Banque de France (BdF) et S&P") +
    dgtresor_theme() +
    scale_y_continuous(breaks = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8),
                       labels = scales::percent_format(accuracy = 1L, decimal.mark = ","))

  if (is.null(dimensions_label_list)) {
    plot <- plot +
      scale_color_manual(palette = color_palette_dimensions)

  } else {
    plot <- plot +
      scale_color_manual(labels = dimensions_label_list,
                         palette = color_palette_dimensions)
  }
  return(plot)
}


# functions for regressions --------------------------------------------------------------------------------------------

create_summary_for_several_simple_regressions <- function(y_var, list_x_var, reg_data, window_size, for_ga = FALSE) {
  # NOTE: this function only works in the specific case of a simple model with a constant and one explanatory variable
  regressions_summary <- summarise_simple_regression_with_rolling_windows(y_var, list_x_var[[1]], reg_data, window_size, for_ga)

  for (x_var in list_x_var[2:length(list_x_var)]) {
    regressions_summary <- regressions_summary %>%
      dplyr::bind_rows(summarise_simple_regression_with_rolling_windows(y_var, x_var, reg_data, window_size, for_ga))
  }
  return(regressions_summary)
}

summarise_simple_regression_with_rolling_windows <- function(y_var, x_var, reg_data, window_size, for_ga = FALSE, dimension_name = NULL) {
  # NOTE: this function only works in the specific case of a simple model with a constant and one explanatory variable
  # TODO: make this function works when we want to compare the performance with glissement annuel and not directly with the quarterly variation
  # unspecific name
  if (is.null(dimension_name)) {
    dimension_name <- x_var
  }
  # First occurence
  reg_data_1 <- reg_data[1:window_size,]
  reg_model <- lm(reg_data_1[[y_var]] ~ reg_data_1[[x_var]], data = reg_data_1)
  if (for_ga) {
    # TODO

  } else {
    reg_model_rmse <- Metrics::rmse(actual = reg_model$model[[1]], predicted = reg_model$fitted.values)
    reg_model_mae <- Metrics::mae(actual = reg_model$model[[1]], predicted = reg_model$fitted.values)
  }
  regression_summary <- data.frame(dimension = dimension_name,
                                   date = reg_data$date[[window_size]],
                                   constant = reg_model$coefficients[["(Intercept)"]],
                                   coefficient = reg_model$coefficients[[2]],
                                   adjusted_r_squared = summary(reg_model)$adj.r.squared,
                                   rmse = reg_model_rmse,
                                   mae = reg_model_mae
  )
  # Check if there is no rolling window
  if (window_size == nrow(reg_data)) {
    return(regression_summary)
  }
  # Following occurences
  for (i in (window_size + 1):nrow(reg_data)) {
    reg_data_i <- reg_data[(i - window_size + 1):i,]
    reg_model_i <- lm(as.formula(paste0(y_var, " ~ ", x_var)), data = reg_data_i)
    new_df <- data.frame(dimension = dimension_name,
                         date = reg_data$date[[i]],
                         constant = reg_model_i$coefficients[["(Intercept)"]],
                         coefficient = reg_model_i$coefficients[[2]],
                         adjusted_r_squared = summary(reg_model_i)$adj.r.squared,
                         rmse = Metrics::rmse(actual = reg_model_i$model[[1]], predicted = reg_model_i$fitted.values),
                         mae = Metrics::mae(actual = reg_model_i$model[[1]], predicted = reg_model_i$fitted.values)
    )
    regression_summary <- regression_summary %>%
      dplyr::bind_rows(new_df)
    rm(reg_model_i)
  }
  return(regression_summary)
}


# Temporary functions -----------------

create_summary_for_several_simple_regressions_to_estimate_quarterly_variation_of_GDP_with_ga <- function(y_var, list_x_var, reg_data) {
  # NOTE: this function only works in the specific case of a simple model with a constant and one explanatory variable
  regressions_summary <- summarise_simple_regression_to_estimate_quarterly_variation_of_GDP_with_ga(y_var, list_x_var[[1]], reg_data)

  for (x_var in list_x_var[2:length(list_x_var)]) {
    regressions_summary <- regressions_summary %>%
      dplyr::bind_rows(summarise_simple_regression_to_estimate_quarterly_variation_of_GDP_with_ga(y_var, x_var, reg_data))
  }
  return(regressions_summary)
}

summarise_simple_regression_to_estimate_quarterly_variation_of_GDP_with_ga <- function(y_var, x_var, reg_data, dimension_name = NULL) {
  # NOTE: in this function we consider that we want to estimate the quarterly variation of GDP with its annual variation (ga, i.e. glissement annuel in French)
  # NOTE: so we assume that the reg_data contains the following variables: PIB, var1_PIB and that var4_PIB is the y_var
  # NOTE: this function only works in the specific case of a simple model with a constant and one explanatory variable
  # TODO: for now, only works with reg_data = regression_data_ga (in the selection of the observertions to use)

  # unspecific name
  if (is.null(dimension_name)) {
    dimension_name <- x_var
  }

  # regression
  reg_model <- lm(reg_data[[y_var]] ~ reg_data[[x_var]], data = reg_data)

  # compile RMSE and MAE
  reg_input_output <- data.frame(PIB = reg_data$PIB,
                                 var4_PIB = reg_data$var4_PIB,
                                 var1_PIB = reg_data$var1_PIB,
                                 fitted_var4_PIB = reg_model$fitted.values)
  reg_input_output <- reg_input_output %>%
    dplyr::mutate(fitted_var1_PIB = (((1 + fitted_var4_PIB) * lag(PIB, 4) - lag(PIB, 1))) / lag(PIB, 1))
  reg_model_rmse <- Metrics::rmse(reg_input_output$var1_PIB[c(5:31, 36:71)], reg_input_output$fitted_var1_PIB[c(5:31, 36:71)]) # TODO: observation' numbers only work for the dataframe regression_data_ga
  reg_model_mae <- Metrics::mae(reg_input_output$var1_PIB[c(5:31, 36:71)], reg_input_output$fitted_var1_PIB[c(5:31, 36:71)])

  regression_summary <- data.frame(dimension = dimension_name,
                                   date = reg_data$date[[nrow(reg_data)]],
                                   constant = reg_model$coefficients[[1]],
                                   coefficient = reg_model$coefficients[[2]],
                                   adjusted_r_squared = summary(reg_model)$adj.r.squared,
                                   rmse = reg_model_rmse,
                                   mae = reg_model_mae
  )
  return(regression_summary)
}

# functions for previsions --------------------------------------------------------------------------------------------

create_summary_for_several_simple_out_of_sample_nowcasting <- function(y_var, list_x_var, reg_data, window_size, dimension_name = NULL, rolling_window = FALSE, list_of_outliers_dates = list()) {
  # NOTE: this function only works in the specific case of a simple model with a constant and one explanatory variable
  regressions_summary <- summarise_simple_out_of_sample_nowcasting(y_var, list_x_var[[1]], reg_data, window_size, dimension_name, rolling_window, list_of_outliers_dates)

  for (x_var in list_x_var[2:length(list_x_var)]) {
    regressions_summary <- regressions_summary %>%
      dplyr::bind_rows(summarise_simple_out_of_sample_nowcasting(y_var, x_var, reg_data, window_size, dimension_name, rolling_window, list_of_outliers_dates))
  }
  return(regressions_summary)
}


summarise_simple_out_of_sample_nowcasting <- function(y_var, x_var, reg_data, window_size, dimension_name = NULL, rolling_window = FALSE, list_of_outliers_dates = list()) {
  # NOTE: this function only works in the specific case of a simple model with a constant and one explanatory variable
  # unspecific name
  if (is.null(dimension_name)) {
    dimension_name <- x_var
  }
  # First occurence
  train_set <- reg_data[1:window_size,]
  reg_model <- lm(as.formula(paste0(y_var, " ~ ", x_var)), data = train_set)
  regression_summary <- data.frame(dimension = dimension_name,
                                   date = reg_data$date[[window_size + 1]],
                                   constant = reg_model$coefficients[[1]],
                                   coefficient = reg_model$coefficients[[2]],
                                   adjusted_r_squared = summary(reg_model)$adj.r.squared,
                                   reg_rmse = Metrics::rmse(actual = reg_model$model[[1]], predicted = reg_model$fitted.values),
                                   reg_mae = Metrics::mae(actual = reg_model$model[[1]], predicted = reg_model$fitted.values),
                                   predicted_values = predict(reg_model, reg_data[window_size + 1,])[[1]],
                                   expected_values = reg_data[[y_var]][window_size + 1]
  )
  # Check if there is no rolling/expanding window
  if (window_size == (nrow(reg_data) - 1)) {
    return(regression_summary)
  }
  # Following occurences
  for (i in (window_size + 1):(nrow(reg_data) - 1)) {
    # prevent to forecast an outlier
    if (reg_data$date[[i + 1]] %in% list_of_outliers_dates) {
      new_df <- data.frame(dimension = dimension_name,
                           date = reg_data$date[[i + 1]],
                           constant = NA_real_,
                           coefficient = NA_real_,
                           adjusted_r_squared = NA_real_,
                           reg_rmse = NA_real_,
                           reg_mae = NA_real_,
                           predicted_values = NA_real_,
                           expected_values = reg_data[[y_var]][i + 1]
      )
    } else {
      # check if there is a rolling or an expanding window
      if (rolling_window) {
        start_period <- (i - window_size + 1)
      } else {
        start_period <- 1
      }
      # perform the forecast
      train_set_i <- reg_data[start_period:i,]
      reg_model_i <- lm(as.formula(paste0(y_var, " ~ ", x_var)), data = train_set_i)
      new_df <- data.frame(dimension = dimension_name,
                           date = reg_data$date[[i + 1]],
                           constant = reg_model_i$coefficients[[1]],
                           coefficient = reg_model_i$coefficients[[2]],
                           adjusted_r_squared = summary(reg_model_i)$adj.r.squared,
                           reg_rmse = Metrics::rmse(actual = reg_model_i$model[[1]], predicted = reg_model_i$fitted.values),
                           reg_mae = Metrics::mae(actual = reg_model_i$model[[1]], predicted = reg_model_i$fitted.values),
                           predicted_values = predict(reg_model_i, reg_data[i + 1,])[[1]],
                           expected_values = reg_data[[y_var]][i + 1]
      )
      rm(reg_model_i)
    }
    regression_summary <- regression_summary %>%
      dplyr::bind_rows(new_df)
  }
  return(regression_summary)
}


# special function # TODO: add more explanation

create_reality_summary <- function(data_source, files_list, file_type2files_list, file_name, dimensions_list, prevision_data, quarters_to_predict) {
  for (i in 1:17) {
    # folder_root <- ifelse(i <= 11, file.path(NATIONAL_ACCOUNTING_DATA_FOLDER, "base2010"), NATIONAL_ACCOUNTING_DATA_FOLDER_BASE2014) #ATTENTION: magic number linked to quarters_to_load (base 2010 up to 18T1PE)
    # if (i <= 15) {
    #   revised_pib <- csv_pre_19T2RD_national_accounting_loader(file_path = file.path(folder_root, quarters_to_load[i]),
    #                                                            folder_name = quarters_to_load[i],
    #                                                            file_name = "erevolch",
    #                                                            dimensions_list = PIB_DIMENSIONS,
    #                                                            dimensions_list_name = "pre_19T2RD_csv")
    # } else {
    #   revised_pib <- xls_national_accounting_loader(file_path = file.path(folder_root, quarters_to_load[i]),
    #                                                 folder_name = quarters_to_load[i],
    #                                                 file_name = "erevolch",
    #                                                 dimensions_list = PIB_DIMENSIONS,
    #                                                 dimensions_list_name = "$post_19T2RD_xls")
    # }

    loader_provider <- get_loader_provider(data_source = data_source)
    folder_name <- names(files_list)[i]
    print(paste("On s'occupe du dossier :", folder_name))
    loader <- get_loader(folder_name, file_type2files_list, loader_provider)
    revised_pib <- loader(files_list[[folder_name]], folder_name, file_name, dimensions_list)

    revised_pib_for_prev <- revised_pib %>%
      dplyr::mutate(var1_PIB_revised = value / lag(value) - 1) %>%
      dplyr::select(date, var1_PIB_revised)

    max_date <- quarters_to_predict[i]

    prevision_data_rev <- prevision_data %>%
      dplyr::full_join(revised_pib_for_prev, by = "date") %>%
      dplyr::arrange(date) %>%
      dplyr::filter(date >= lubridate::ymd("2007-10-01") & date <= max_date)

    if (i == 1) {

      # first occurence
      if (nrow(prevision_data_rev) != 32 + 1) { stop("Problem in the sample") }
      reality_exercise_summary <- create_summary_for_several_simple_out_of_sample_nowcasting(y_var = "var1_PIB_revised",
                                                                                             list_x_var = colnames(prevision_data_rev)[!(colnames(prevision_data_rev) %in% c("date", "var1_PIB", "var4_PIB", "var1_PIB_revised"))],
                                                                                             reg_data = prevision_data_rev,
                                                                                             window_size = 32) # Note: 32 quarters = 8 years
    } else {
      # other occurences
      if (nrow(prevision_data_rev) != 32 + i) { stop(paste("Prévision", max_date, ": Problem in the sample")) }
      new_summary <- create_summary_for_several_simple_out_of_sample_nowcasting(y_var = "var1_PIB_revised",
                                                                                list_x_var = colnames(prevision_data_rev)[!(colnames(prevision_data_rev) %in% c("date", "var1_PIB", "var4_PIB", "var1_PIB_revised"))],
                                                                                reg_data = prevision_data_rev,
                                                                                window_size = 32 + i - 1) # Note: 32 quarters = 8 years
      reality_exercise_summary <- reality_exercise_summary %>%
        dplyr::bind_rows(new_summary)
      rm(new_summary)
    }
  }
  return(reality_exercise_summary)
}




