# Title     : TODO
# Objective : TODO
# Created by: lphung
# Created on: 12/09/2022

# initialise the environment -------------------------------------------------------------------------------------------
rm(list = ls())

# packages -------------------------------------------------------------------------------------------------------------
library(stringr)
library(dplyr)
library(lubridate)
library(stats)
library(forecast)
library(ggplot2)
source("./code/data_importator.R", encoding = "utf-8")
source("./code/data_preparator.R", encoding = "utf-8")
source("./code/nonrevised_ipi/loaders_utils.R", encoding = "utf-8", chdir = TRUE)
source("./code/nonrevised_national_accounting/loaders_utils.R", encoding = "utf-8", chdir = TRUE)
source("./code/doc_travail_interpretation_enquetes/helpers.R", encoding = "utf-8", chdir = TRUE)
# chdir = TRUE needed because we call this Rscript from the main.R and from a RMarkdown, which define working directory differently

source("./code/old_scripts_from_prevision_production_manuf/loading_data.R", encoding = "utf-8")
source("./code/old_scripts_from_prevision_production_manuf/data_transformation.R", encoding = "utf-8")


# constants to define --------------------------------------------------------------------------------------------------
UPDATE_REVISED_IPI_DATA <- FALSE
UPDATE_REVISED_PRODUCTION_DATA <- FALSE
ONLY_UPDATE_NONREVISED_IPI_DATA <- TRUE
ONLY_UPDATE_NONREVISED_PRODUCTION_DATA <- TRUE
# Note: production data in this project always refers to the production or value added in the manufacturing sector in the quarterly national accounts

# 1. load nonrevised ipi -------------------------------------------------------------------------------------------------
if (ONLY_UPDATE_NONREVISED_IPI_DATA) {
  # TODO: nonrevised_ipi <- update_nonrevised_ipi()
  load("./data/nonrevised_ipi_2023-03-01.RData")
} else {
  # preparation of the list
  IPI_DATA_FILES <- get_ipi_data_files(IPI_DATA_FOLDER)
  # TODO: check where to put that -> in a README.md // Note: convention de nommage des fichiers d'IPI: la date du nom de fichier doit contenir la dernière date pour laquelle nous avons l'IPI (et non la date de publication)
  IPI_FILES_TYPES <- list("sectors_in_line_one_label_loader" = c("200901"),
                          "sectors_in_line_two_labels_loader" = stringr::str_subset(names(IPI_DATA_FILES), "(^2009(?!(01)).*)|(^2010(?!(11)|(12)).*)"))

  # preparation of the matrix
  nonrevised_ipi <- construct_nonrevised_ipi_from_scratch(files_list = IPI_DATA_FILES,
                                                          file_type2files_list = IPI_FILES_TYPES,
                                                          number_previous_values = 166, #24
                                                          data_correction = "CJO-CVS") # TODO: data_correction as argument within the function

  save(nonrevised_ipi, file = paste0("./data/", "nonrevised_ipi_", max(unique(nonrevised_ipi[["date"]])), ".RData"))
}

# 2. load nonrevised production ----------------------------------------------------------------------------------------
if (ONLY_UPDATE_NONREVISED_PRODUCTION_DATA) {
  # nonrevised_national_accounting <- update_nonrevised_production()
  load("./data/nonrevised_production_2023-01-01PE.RData")
} else {
  # preparation of the list
  PRODUCTION_DATA_FOLDERS <- get_national_accounting_data_files(NATIONAL_ACCOUNTING_DATA_FOLDER, estimation_type = "PE")
  # we keep only the folders containing the 1st estimation of the quarterly accounts (PE)
  ## Note : we do not want to use the .RData file (available from 19T4) because they are ts() series not in dataframe format and we would need to reconstruct everything
  PRODUCTION_FILES_TYPES <- list("pre_19T2RD_csv" = stringr::str_subset(names(PRODUCTION_DATA_FOLDERS), "(^1(?!(9T2RD)|(9T3PE)|(9T3RD)|(9T4PE)|(9T4RD)).*)"), # we want
                                 "post_19T2RD_xls" = stringr::str_subset(names(PRODUCTION_DATA_FOLDERS), "^((19T2RD)|(19T3PE)|(19T3RD)|(19T4PE)|(19T4RD))|(^20.*)|(^21.*)|^((22T1PE)|(22T1RD))"),
                                 "post_19T2RD_csv" = stringr::str_subset(names(PRODUCTION_DATA_FOLDERS), "(^2(?!(0.*)|(1.*)|(2T1PE)|(2T1RD)).*)"))

  # preparation of the matrix
  nonrevised_production <- construct_nonrevised_national_account_data_from_scratch(data_source = "national_accounting",
                                                                                   files_list = PRODUCTION_DATA_FOLDERS,
                                                                                   file_type2files_list = PRODUCTION_FILES_TYPES,
                                                                                   file_name = PRODUCTION_FILE_NAME,
                                                                                   dimensions_list = PRODUCTION_DIMENSIONS,
                                                                                   number_previous_values = 47) #24

  save(nonrevised_production, file = paste0("./data/", "nonrevised_production_", max(unique(nonrevised_production[["date"]])), "PE.RData"))
}

# 3. create the dataframes for the prevision ---------------------------------------------------------------------------
# Note: we forcast only the production in the manufacturing industry (i.e. CZ sector)

# 3.1. correct the non-revised data for the effects of changing bases
# load revised data to calculate the effects of changing bases
if (UPDATE_REVISED_IPI_DATA) {
  revised_ipi <- load_data_from_insee(dimensions_to_load = IPI_SECTORS_NAF2,
                                      dimensions_label_list = SECTORS_LABEL_LIST) %>%
    dplyr::select(-label)
  save(revised_ipi, file = paste0("./data/", "revised_ipi_", max(unique(revised_ipi[["date"]])), ".RData"))
} else {
  load("./data/revised_ipi_2023-03-01.RData")
}

if (UPDATE_REVISED_PRODUCTION_DATA) {
  revised_production <- most_recent_compta_nat_data_loader(folder_path = NATIONAL_ACCOUNTING_DATA_FOLDER_BASE2014,
                                                           file_name = PRODUCTION_FILE_NAME,
                                                           dimensions_list = PRODUCTION_DIMENSIONS,
                                                           dimensions_list_name = "revised")

  save(revised_production, file = paste0("./data/", "revised_production_", max(unique(nonrevised_production[["date"]])), "PE.RData")) # ATTENTION: choose PE or RD
} else {
  load("./data/revised_production_2023-01-01PE.RData")
}

# get the needed sector and, compare revised and nonrevised data
compare_ipi <- merge_nonrevised_and_revised_data(revised_data = revised_ipi, nonrevised_data = nonrevised_ipi,
                                                 dimension_to_keep = "CZ", column_to_use_for_revised_data = value, column_to_use_for_nonrevised_data = t, data_label = "ipi")

compare_production <- merge_nonrevised_and_revised_data(revised_data = revised_production, nonrevised_data = nonrevised_production,
                                                        dimension_to_keep = "P1E_DIM", column_to_use_for_revised_data = value, column_to_use_for_nonrevised_data = t, data_label = "production")

# get nonrevised data corrected for the effects of changing bases
## TO CORRECT the non-revised data for the effects of changing bases: # TODO: create a function for that (across columns) in data_preparator.R // see which method is the best
## for ipi
ipi_changing_base_shift_2009_2012 <- get_changing_base_shift(compare_ipi, "2009-01-01", "2012-12-01")
nonrevised_ipi_cz <- nonrevised_ipi %>%
  dplyr::select(date, dimension, t, t_1, t_2, t_3, t_4, t_5) %>%
  dplyr::filter(dimension == "CZ") %>%
  dplyr::mutate(dimension = "ipi") %>%
  dplyr::mutate(
    t = case_when(
      lubridate::year(date) >= 2009 & lubridate::year(date) < 2013 ~ t + ipi_changing_base_shift_2009_2012,
      TRUE ~ t),
    t_1 = case_when(
      lubridate::year(date) >= 2009 & lubridate::year(date) < 2013 ~ t_1 + ipi_changing_base_shift_2009_2012,
      TRUE ~ t_1),
    t_2 = case_when(
      lubridate::year(date) >= 2009 & lubridate::year(date) < 2013 ~ t_2 + ipi_changing_base_shift_2009_2012,
      TRUE ~ t_2),
    t_3 = case_when(
      lubridate::year(date) >= 2009 & lubridate::year(date) < 2013 ~ t_3 + ipi_changing_base_shift_2009_2012,
      TRUE ~ t_3),
    t_4 = case_when(
      lubridate::year(date) >= 2009 & lubridate::year(date) < 2013 ~ t_4 + ipi_changing_base_shift_2009_2012,
      TRUE ~ t_4),
    t_5 = case_when(
      lubridate::year(date) >= 2009 & lubridate::year(date) < 2013 ~ t_5 + ipi_changing_base_shift_2009_2012,
      TRUE ~ t_5)
  )

# for production
production_changing_base_shift_2011_2013 <- get_changing_base_shift(compare_production, "2011-01-01", "2013-10-01")
production_changing_base_shift_2014_2018 <- get_changing_base_shift(compare_production, "2014-01-01", "2018-01-01")
nonrevised_production_cz <- nonrevised_production %>%
  dplyr::select(date, dimension, t, t_1) %>%
  dplyr::filter(dimension == "P1E_DIM") %>%
  dplyr::mutate(dimension = "production") %>%
  dplyr::mutate(
    t = case_when(
      lubridate::year(date) >= 2011 & lubridate::year(date) <= 2013 ~ t + production_changing_base_shift_2011_2013,
      lubridate::year(date) >= 2014 & date <= lubridate::ymd("2018-01-01") ~ t + production_changing_base_shift_2014_2018,
      TRUE ~ t),
    t_1 = case_when(
      lubridate::year(date) >= 2011 & lubridate::year(date) <= 2013 ~ t_1 + production_changing_base_shift_2011_2013,
      lubridate::year(date) >= 2014 & date <= lubridate::ymd("2018-01-01") ~ t_1 + production_changing_base_shift_2014_2018,
      TRUE ~ t_1)
  )

