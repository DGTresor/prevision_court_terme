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
  } else if (data_source == "pib") {
    return(get_loader_for_pib)
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

## TODO: maybe put functions for national accounting data elsewhere => a common folder for nonrevised_production and nonrevised_pib ?
# functions for national accounting data -------------------------------------------------------------------------------
# todo: check duplication with construct_nonrevised_ipi_from_scratch()
construct_nonrevised_national_account_data_from_scratch <- function(data_source, files_list, file_type2files_list, file_name, number_previous_values = 0) {
  df <- get_empty_dataframe(number_previous_values)
  # column_to_join_by <- get_column_names_to_join_by()

  loader_provider <- get_loader_provider(data_source = data_source)
  for (folder_name in names(files_list)) {
    print(paste("On s'occupe du dossier :", folder_name))
    loader <- get_loader(folder_name, file_type2files_list, loader_provider)
    # file_path <- file.path(files_list[[folder_name]], "cprvolch")
    new_data <- loader(files_list[[folder_name]], folder_name, file_name)
    df <- construct_nonrevised_series(df, new_data, date_granularity = "quarter", number_previous_values = number_previous_values)
    rm(new_data)
  }
  df <- df %>% dplyr::arrange(date)
  #select(all_of(sort(colnames(data_to_bind))))
  return(df)
}

# functions to prepare the files' list for national accounting data ----------------------------------------------------
get_national_accounting_data_files <- function(national_accounting_data_folder, estimation_type, subset_regex = NULL) {
  # define the regex pattern corresponding to the estimation_type
  estimation_pattern <- get_estimation_pattern_for_estimation_type(estimation_type)
  # get the list of folders containing each quarterly account
  national_accounting_data_folders <- list.dirs(national_accounting_data_folder, recursive = TRUE, full.names = TRUE)
  national_accounting_data_folders <- stringr::str_subset(string = national_accounting_data_folders,
                                                          pattern = paste0(".*/base(?!(1980)|(1995)|(2000))[:digit:]{4}/(?!(10))[:digit:]{2}T[:digit:]", estimation_pattern, "$"))
  ## Note: the regex means: ".*/base(?!(1980)|(1995)|(2000))[:digit:]{4}/" -> we take the folders that contain data from base 2005 and onward
  ## "(?!(10))[:digit:]{2}T[:digit:](PE|RD)$" -> we take the folders that contain data for the PE (Premiere Estimation) or RD (Resultats Detailles) in the format e.g. 11T1PE (Première Estimation du T1 2011),
  ## end we exclude data for the year 2010 for which data follows another classification

  #todo: to delete when other loaders created
  # production_data_folders <- stringr::str_subset(string = production_data_folders,
  #                                                pattern = ".*/((1.*)|(20.*)|(21.*)|(22T1PE)|(22T1RD))")

  # reduce the list of folders for additional purposes
  if (!is.null(subset_regex)) {
    national_accounting_data_folders <- stringr::str_subset(string = national_accounting_data_folders,
                                                            pattern = subset_regex)
  }

  # give an harmonised name to each file according to the last date of the national accounting data
  names(national_accounting_data_folders) <- stringr::str_extract(national_accounting_data_folders, pattern = "(?<=/)[:digit:]{2}T[:digit:](PE|RD)$")
  ## Note: to ensure that the folders are read in the good chronological order, it is essential that they are named properly

  # sort the list by the names
  national_accounting_data_folders <- national_accounting_data_folders[sort(names(national_accounting_data_folders))]

  return(national_accounting_data_folders)
}

get_estimation_pattern_for_estimation_type <- function(estimation_type) {
  if (estimation_type %in% c("PE", "RD")) {
    return(estimation_type)
  } else if (estimation_type == "all") {
    return("(PE|RD)")
  } else {
    stop("estimation_type can only be: \"PE\", \"RD\" or \"all\".")
  }
}