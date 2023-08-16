# Title     : meta-functions that call the loaders to construct the production series from the national accounts
# Created by: lphung
# Created on: 12/10/2022

# packages -------------------------------------------------------------------------------------------------------------
library(stringr)
library(dplyr)
library(tidyr)
source("./loaders.R", encoding = "utf-8", chdir = TRUE)
# chdir = TRUE needed because we call this Rscript from the main.R and from a RMarkdown, which define working directory differently

# folders --------------------------------------------------------------------------------------------------------------
DGTRESOR_T_SERVER_PATH <- "T:/SPMAE_Public/Prev_Public" # TODO: to remove for public release -> create a branch for public release
NATIONAL_ACCOUNTING_DATA_FOLDER <- file.path(DGTRESOR_T_SERVER_PATH, "CNAT/ArchivesCTrim")
NATIONAL_ACCOUNTING_DATA_FOLDER_BASE2014 <- file.path(DGTRESOR_T_SERVER_PATH, "CNAT/ArchivesCTrim/base2014") # this one is to be used for revised data because it contains the most recent files

# constants ------------------------------------------------------------------------------------------------------------
## constants for production
PRODUCTION_FILE_NAME <- "cprvolch"
PRODUCTION_DIMENSIONS <- list("revised" = c("production" = "TD.P1E_DIM_7CH"),
                              "post_19T2RD" = c("production_BE" = "TD.P1E_DI_7CH",
                                                "production_CZ" = "TD.P1E_DIM_7CH",
                                                "production_C1" = "TD.P1E_A17C1_7CH",
                                                "production_C2" = "TD.P1E_A17C2_7CH",
                                                "production_C3" = "TD.P1E_A17C3_7CH",
                                                "production_C4" = "TD.P1E_A17C4_7CH",
                                                "production_C5" = "TD.P1E_A17C5_7CH",
                                                "production_DE" = "TD.P1E_A17DE_7CH",
                                                "production_FZ" = "TD.P1E_A17FZ_7CH",
                                                "valeur_ajoutee_BE" = "TD.B1_DI_7CH",
                                                "valeur_ajoutee_CZ" = "TD.B1_DIM_7CH",
                                                "valeur_ajoutee_C1" = "TD.B1_A17C1_7CH",
                                                "valeur_ajoutee_C2" = "TD.B1_A17C2_7CH",
                                                "valeur_ajoutee_C3" = "TD.B1_A17C3_7CH",
                                                "valeur_ajoutee_C4" = "TD.B1_A17C4_7CH",
                                                "valeur_ajoutee_C5" = "TD.B1_A17C5_7CH",
                                                "valeur_ajoutee_DE" = "TD.B1_A17DE_7CH",
                                                "valeur_ajoutee_FZ" = "TD.B1_A17FZ_7CH"),
                              "pre_19T2RD_csv" = c("production_BE" = "TD_P1E_DI7_CH",
                                                   "production_CZ" = "TD_P1E_DIM7_CH",
                                                   "production_C1" = "TD_P1E_C17_CH",
                                                   "production_C2" = "TD_P1E_C27_CH",
                                                   "production_C3" = "TD_P1E_C37_CH",
                                                   "production_C4" = "TD_P1E_C47_CH",
                                                   "production_C5" = "TD_P1E_C57_CH",
                                                   "production_DE" = "TD_P1E_DE7_CH",
                                                   "production_FZ" = "TD_P1E_FZ7_CH",
                                                   "valeur_ajoutee_BE" = "TD_B1_DI7_CH",
                                                   "valeur_ajoutee_CZ" = "TD_B1_DIM7_CH",
                                                   "valeur_ajoutee_C1" = "TD_B1_C17_CH",
                                                   "valeur_ajoutee_C2" = "TD_B1_C27_CH",
                                                   "valeur_ajoutee_C3" = "TD_B1_C37_CH",
                                                   "valeur_ajoutee_C4" = "TD_B1_C47_CH",
                                                   "valeur_ajoutee_C5" = "TD_B1_C57_CH",
                                                   "valeur_ajoutee_DE" = "TD_B1_DE7_CH",
                                                   "valeur_ajoutee_FZ" = "TD_B1_FZ7_CH"),
                              "pre_2010" = "")
# The file we need is "cprvolch", according to the years, it can be in .RData, .xls or .cvs.
# it contains the production (National accounting classification: P1) and the value added (B1).
# We use data from base2005 onward; the exclusion of data previous to base2005 is dealt in the function: get_national_accounting_data_files()

## constants for nonrevised gdp (i.e., pib in French)
PIB_FILE_NAME <- "erevolch"
PIB_DIMENSIONS <- list("post_19T2RD" = c("pib" = "TD.PIB_7CH"),
                       "pre_19T2RD_csv" = c("pib" = "TD_PIB7_CH"),
                       "pre_2011" = c("pib" = "TD.PIB7_CH"))
# The file we need is "erevolch", according to the years, it can be in .RData, .xls or .cvs.
# it contains the PIB.
# We use data from base2005 onward; the exclusion of data previous to base2005 is dealt in the function: get_national_accounting_data_files()

## note for national accounting dimensions
# /!\ Note: pre_2010 classification applies to 2010 quarters and before; for 2010, and before, the classification is different. So, we decide not to use data before 2011.
# Note: .xls, .cvs or .RData files are available only from base2000 on, i.e. starting from the 2007T4PE account.
# Note: .xls files always have the same ID code for indicators, however they have been different for .csv files and since .csv files are easier to load, we need to account for the change in ID codes.


# functions ------------------------------------------------------------------------------------------------------------
# todo: check duplication with construct_nonrevised_ipi_from_scratch()
construct_nonrevised_national_account_data_from_scratch <- function(data_source, files_list, file_type2files_list, file_name, dimensions_list, number_previous_values = 0) {
  df <- get_empty_dataframe(number_previous_values)
  # column_to_join_by <- get_column_names_to_join_by()

  loader_provider <- get_loader_provider(data_source = data_source)
  for (folder_name in names(files_list)) {
    print(paste("On s'occupe du dossier :", folder_name))
    loader <- get_loader(folder_name, file_type2files_list, loader_provider)
    # file_path <- file.path(files_list[[folder_name]], "cprvolch")
    new_data <- loader(files_list[[folder_name]], folder_name, file_name, dimensions_list)
    df <- construct_nonrevised_series(df, new_data, date_granularity = "quarter", number_previous_values = number_previous_values)
    rm(new_data)
  }
  df <- df %>% dplyr::arrange(date)
  #select(all_of(sort(colnames(data_to_bind))))
  return(df)
}

get_loader_for_national_accounting <- function(file_type) {
  if (file_type == "pre_19T2RD_csv") {
    return(csv_pre_19T2RD_national_accounting_loader)
  } else if (file_type == "post_19T2RD_xls") {
    return(xls_national_accounting_loader)
  } else if (file_type == "post_19T2RD_csv") {
    return(csv_post_19T2RD_national_accounting_loader)
  } else  if (file_type == "pre_2011_xls") {
    return(xls_national_accounting_loader)  # only for PIB data for now because the convention (for activity sectors notably) changed between base2000 and base2005
  } else {
    stop(paste0("No loader found for the file_type: ", file_type))
  }
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

# functions to prepare the files' list ---------------------------------------------------------------------------------
get_national_accounting_data_files <- function(national_accounting_data_folder, estimation_type, subset_regex = NULL, starting_period = "standard") {
  # define the regex pattern corresponding to the estimation_type
  estimation_pattern <- get_estimation_pattern_for_estimation_type(estimation_type)

  # get the regex pattern according to the starting_period
  regex_for_folder_names <- get_regex_pattern_according_to_starting_period(starting_period, estimation_pattern)

  # get the list of folders containing each quarterly account
  national_accounting_data_folders <- list.dirs(national_accounting_data_folder, recursive = TRUE, full.names = TRUE)
  national_accounting_data_folders <- stringr::str_subset(string = national_accounting_data_folders,
                                                          pattern = regex_for_folder_names)

  # if needed, reduce the list of folders selected
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

get_regex_pattern_according_to_starting_period <- function(starting_period, estimation_pattern){
    if (starting_period == "standard") {
    return(paste0(".*/base(?!(1980)|(1995)|(2000))[:digit:]{4}/(?!(10))[:digit:]{2}T[:digit:]", estimation_pattern, "$"))
    ## Note: the regex part: ".*/base(?!(1980)|(1995)|(2000))[:digit:]{4}/" means that we take the folders that contain data from base2005 and onward because the national accoutning conventions have changed, notably the classification for activity sectors
    ## "(?!(10))[:digit:]{2}T[:digit:](PE|RD)$" -> we take the folders that contain data for the PE (Premiere Estimation) or RD (Resultats Detailles) in the format e.g. 11T1PE (Première Estimation du T1 2011),
    ## end we exclude data for the year 2010 (of base2005) for which data follows another classification
  } else if (starting_period == "base2000") {
    return(paste0(".*/base(?!(1980)|(1995))[:digit:]{4}/[:digit:]{2}T[:digit:]", estimation_pattern, "$"))
    ## Note: compared to the regex for the standard starting period, we keep the base2000 and the year 2010 from the base2005
    ## For now, that should only be used for PIB data
  } else {
    stop("Pour l'argument starting_period de la fonction get_regex_pattern_according_to_starting_period() appelée par la fonction get_national_accounting_data_files(): choisissez \"standard\" ou \"base2000\".
    On notera qu'à ce stade, seules les données de PIB peuvent commencer à la base 2000, toutes les autres séries doivent commencer en 2011 (base2005) car les conventions ont sensiblement changées et notamment la classification des secteurs d'activité.")
  }
}