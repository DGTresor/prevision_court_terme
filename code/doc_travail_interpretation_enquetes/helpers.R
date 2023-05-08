# Title     : TODO
# Objective : TODO
# Created by: lphung
# Created on: 18/01/2023

# libraries ------------------------------------------------------------------------------------------------------------
library(dplyr)
library(readr)
library(Metrics)

# dependencies
source("../nonrevised_production/loaders.R", encoding = "utf-8", chdir = TRUE)
source("../old_scripts_from_prevision_production_manuf/loading_data.R", encoding = "utf-8", chdir = TRUE)
# chdir = TRUE needed because we call this Rscript from the main.R and from a RMarkdown, which define working directory differently

# constants ------------------------------------------------------------------------------------------------------------
PIB_DATA_FOLDER <- "T:/SPMAE_Public/Prev_Public/CNAT/ArchivesCTrim/base2014"
PIB_FILE_NAME <- "erevolch.csv"
PATH_TO_PMI_DATA <- "S:/SPMAE/PREV/Prev3/_Fichiers_Prev3/Synthèse/6. Enquêtes PMI/4. Préparation mail réaction PMI/Données/Data PMI.xlsx"
PATH_TO_FR_DEROULEUR <- list(path = "S:/SPMAE/PREV/Prev3/_Fichiers_Prev3/Synthèse/FR_Dérouleur.xlsx",
                             sheet = "Données enquêtes")

PIB_DIMENSIONS <- list("default" = c("pib" = "TD.PIB_7CH"))
PMI_DIMENSIONS_LIST <- list(
  "Composite" = "composite",
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

# functions to load data -----------------------------------------------------------------------------------------------

compta_nat_loader <- function(folder_path, file_name, dimensions_list_name, dimensions_list) {
  file_path <- get_compta_nat_most_recent_file(folder_path, file_name)
  suppressMessages(data <- readr::read_delim(file = file_path, delim = ";", col_names = TRUE)) # suppress messages to prevent message of columns' type

  clean_data <- data_cleaner_for_csv(data, dimensions_list_name = dimensions_list_name, list_of_dimensions = dimensions_list)
  return(clean_data)
}

get_compta_nat_most_recent_file <- function(folder_path, file_name) {
  message("Le chemin du dossier pointe actuellement vers base2014 ; Pensez à le changer si la base change.")
  most_recent_folder <- get_the_most_recent_file(folder_path)
  file_path <- file.path(most_recent_folder, file_name)
  return(file_path)
}

get_the_most_recent_file <- function(folder_path, exclusion_list = NULL) {
  # get all the folders' names in the folder
  files_names <- list.files(path = folder_path, full.names = FALSE)

  # remove certain specific files if needed
  if (!is.null(exclusion_list)) {
    files_names <- files_names[!(files_names %in% exclusion_list)]
  }

  # the current alphanumeric classification enables that the last folder is the most recent
  most_recent_file <- file.path(folder_path, files_names[length(files_names)])

  return(most_recent_file)
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

  } else {
    reg_model_rmse <- Metrics::rmse(actual = reg_model$model[[1]], predicted = reg_model$fitted.values)
    reg_model_mae <- Metrics::mae(actual = reg_model$model[[1]], predicted = reg_model$fitted.values)
  }
  regression_summary <- data.frame(dimension = dimension_name,
                                   date = reg_data$date[[window_size]],
                                   constant = reg_model$coefficients[[1]],
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
    reg_model_i <- lm(reg_data_i[[y_var]] ~ reg_data_i[[x_var]], data = reg_data_i)
    new_df <- data.frame(dimension = dimension_name,
                         date = reg_data$date[[i]],
                         constant = reg_model_i$coefficients[[1]],
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

# functions for regressions --------------------------------------------------------------------------------------------

# summarise_simple_regression_with_rolling_windows <- function(y_var, x_var, reg_data, window_size, dimension_name = NULL) {
#   # NOTE: this function only works in the specific case of a simple model with a constant and one explanatory variable
#   # unspecific name
#   if (is.null(dimension_name)) {
#     dimension_name <- x_var
#   }
#   # First occurence
#   reg_data_1 <- reg_data[1:window_size,]
#   reg_model <- lm(reg_data_1[[y_var]] ~ reg_data_1[[x_var]], data = reg_data_1)
#   regression_summary <- data.frame(dimension = dimension_name,
#                                    date = reg_data$date[[window_size]],
#                                    constant = reg_model$coefficients[[1]],
#                                    coefficient = reg_model$coefficients[[2]],
#                                    adjusted_r_squared = summary(reg_model)$adj.r.squared,
#                                    rmse = Metrics::rmse(actual = reg_model$model[[1]], predicted = reg_model$fitted.values),
#                                    mae = Metrics::mae(actual = reg_model$model[[1]], predicted = reg_model$fitted.values)
#   )
#   # Check if there is no rolling window
#   if (window_size == nrow(reg_data)){
#     return(regression_summary)
#   }
#   # Following occurences
#   for (i in (window_size + 1):nrow(reg_data)) {
#     reg_data_i <- reg_data[(i - window_size + 1):i,]
#     reg_model_i <- lm(reg_data_i[[y_var]] ~ reg_data_i[[x_var]], data = reg_data_i)
#     new_df <- data.frame(dimension = dimension_name,
#                          date = reg_data$date[[i]],
#                          constant = reg_model_i$coefficients[[1]],
#                          coefficient = reg_model_i$coefficients[[2]],
#                          adjusted_r_squared = summary(reg_model_i)$adj.r.squared,
#                          rmse = Metrics::rmse(actual = reg_model_i$model[[1]], predicted = reg_model_i$fitted.values),
#                          mae = Metrics::mae(actual = reg_model_i$model[[1]], predicted = reg_model_i$fitted.values)
#     )
#     regression_summary <- regression_summary %>%
#       dplyr::bind_rows(new_df)
#     rm(reg_model_i)
#   }
#   return(regression_summary)
# }