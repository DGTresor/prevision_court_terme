# Title     : TODO
# Objective : TODO
# Created by: lphung
# Created on: 08/05/2023


# initialise the environment -------------------------------------------------------------------------------------------
rm(list = ls())

# packages -------------------------------------------------------------------------------------------------------------
library(stringr)
library(dplyr)
library(lubridate)
library(stats)
library(forecast)
library(ggplot2)
# source("./code/data_importator.R", encoding = "utf-8")
# source("./code/data_preparator.R", encoding = "utf-8")
# source("./code/nonrevised_ipi/loaders_utils.R", encoding = "utf-8", chdir = TRUE)
# source("./code/nonrevised_national_accounting/loaders_utils.R", encoding = "utf-8", chdir = TRUE)
# # chdir = TRUE needed because we call this Rscript from the main.R and from a RMarkdown, which define working directory differently
#
# source("./code/scripts_from_prevision_production_manuf/loading_data.R", encoding = "utf-8")
# source("./code/scripts_from_prevision_production_manuf/data_transformation.R", encoding = "utf-8")
# source("./code/doc_travail_interpretation_enquetes/helpers.R", encoding = "utf-8")

source("./code/doc_travail_interpretation_enquetes/nonrevised_pib/loaders_utils.R", encoding = "utf-8", chdir = TRUE)

# constants to define --------------------------------------------------------------------------------------------------
UPDATE_REVISED_PIB_DATA <- FALSE
UPDATE_NONREVISED_PIB_DATA <- FALSE

# 1. load revised pib -------------------------------------------------------------------------------------------


# 2. load nonrevised pib ----------------------------------------------------------------------------------------
if (UPDATE_NONREVISED_PIB_DATA) {
  # preparation of the list
  PIB_DATA_FOLDERS <- get_national_accounting_data_files(NATIONAL_ACCOUNTING_DATA_FOLDER,
                                                         estimation_type = "PE",                                   # we keep only the folders containing the 1st estimation of the quarterly accounts (PE)
                                                         subset_regex = ".*/(?!(2.))[:digit:]{2}T[:digit:]PE")     # we keep data only up to 2019T4

  PIB_FILES_TYPES <- list("pre_19T2RD_csv" = stringr::str_subset(names(PIB_DATA_FOLDERS), "(^1(?!(9T2RD)|(9T3PE)|(9T3RD)|(9T4PE)|(9T4RD)).*)"),
                          "post_19T2RD_xls" = stringr::str_subset(names(PIB_DATA_FOLDERS), "^((19T2RD)|(19T3PE)|(19T3RD)|(19T4PE)|(19T4RD))|(^20.*)|(^21.*)|^((22T1PE)|(22T1RD))")
                          #"post_19T2RD_csv" = stringr::str_subset(names(PIB_DATA_FOLDERS), "(^2(?!(0.*)|(1.*)|(2T1PE)|(2T1RD)).*)") # aucun de ce type avant 2020
  )

  # preparation of the matrix
  nonrevised_pib <- construct_nonrevised_national_account_data_from_scratch(data_source = "national_accounting",
                                                                            files_list = PIB_DATA_FOLDERS,
                                                                            file_type2files_list = PIB_FILES_TYPES,
                                                                            file_name = PIB_FILE_NAME,
                                                                            dimensions_list = PIB_DIMENSIONS,
                                                                            number_previous_values = 47) #24

  save(nonrevised_pib, file = paste0("./data/", "nonrevised_pib_", max(unique(nonrevised_pib[["date"]])), "PE.RData"))
} else {
  load("./data/nonrevised_pib_2019-10-01PE.RData")
}

# 3. create the dataframes for the prevision ---------------------------------------------------------------------------
# Note: we forcast only the production in the manufacturing industry (i.e. CZ sector)

# 3.1. correct the non-revised data for the effects of changing bases
# load revised data to calculate the effects of changing bases

# TODO: use for revised_production 2019T4PE
if (UPDATE_REVISED_PRODUCTION_DATA) {
  revised_pib <- most_recent_compta_nat_data_loader(folder_path = NATIONAL_ACCOUNTING_DATA_FOLDER_BASE2014,
                                                           file_name = PIB_FILE_NAME,
                                                           dimensions_list = PIB_DIMENSIONS)

  save(revised_pib, file = paste0("./data/", "revised_pib_", max(unique(nonrevised_pib[["date"]])), "PE.RData")) # ATTENTION: choose PE or RD
} else {
  load("./data/revised_pib_2023-01-01PE.RData")
}

# get the needed sector and, compare revised and nonrevised data
compare_production <- merge_nonrevised_and_revised_data(revised_data = revised_production, nonrevised_data = nonrevised_pib,
                                                        dimension_to_keep = "P1E_DIM", column_to_use_for_revised_data = value, column_to_use_for_nonrevised_data = t, data_label = "production")

# get nonrevised data corrected for the effects of changing bases
## TO CORRECT the non-revised data for the effects of changing bases: # TODO: create a function for that (across columns) in data_preparator.R // see which method is the best

# for production
production_changing_base_shift_2011_2013 <- get_changing_base_shift(compare_production, "2011-01-01", "2013-10-01")
production_changing_base_shift_2014_2018 <- get_changing_base_shift(compare_production, "2014-01-01", "2018-01-01")
nonrevised_production_cz <- nonrevised_pib %>%
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
