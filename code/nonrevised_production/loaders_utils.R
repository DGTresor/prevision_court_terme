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
PRODUCTION_DATA_FOLDER <- "T:/SPMAE_Public/Prev_Public/CNAT/ArchivesCTrim"

# The file we need is "cprvolch", according to the years, it can be in .RData, .xls or .cvs.
# it contains the production (National accounting classification: P1) and the value added (B1).
# We use data from base2005 onward; the exclusion of data previous to base2005 is dealt in the function: get_national_accounting_data_files()

# functions ------------------------------------------------------------------------------------------------------------
# # todo: check duplication with construct_nonrevised_ipi_from_scratch()
# construct_nonrevised_national_account_data_from_scratch <- function(data_source, files_list, file_type2files_list, number_previous_values = 0) {
#   df <- get_empty_dataframe(number_previous_values)
#   # column_to_join_by <- get_column_names_to_join_by()
#
#   loader_provider <- get_loader_provider(data_source = data_source)
#   for (folder_name in names(files_list)) {
#     print(paste("On s'occupe du dossier :", folder_name))
#     loader <- get_loader(folder_name, file_type2files_list, loader_provider)
#     # file_path <- file.path(files_list[[folder_name]], "cprvolch")
#     new_data <- loader(files_list[[folder_name]], folder_name)
#     df <- construct_nonrevised_series(df, new_data, date_granularity = "quarter", number_previous_values = number_previous_values)
#     rm(new_data)
#   }
#   df <- df %>% dplyr::arrange(date)
#   #select(all_of(sort(colnames(data_to_bind))))
#   return(df)
# }

get_loader_for_production <- function(file_type) {
  if (file_type == "pre_19T2RD_csv") {
    return(csv_pre_19T2RD_production_loader)
  } else if (file_type == "post_19T2RD_xls") {
    return(xls_production_loader)
  } else if (file_type == "post_19T2RD_csv") {
    return(csv_post_19T2RD_production_loader)
  } else {
    stop(paste0("No loader found for the file_type: ", file_type))
  }
}

# get_file_name_for <- function(loader_name, folder_path) {
#   file_path <- paste0(folder_path, "/cprvolch") # That is the national accounts' file we want to use for production and value added data in volume with chained prices
#   file_type <- stringr::str_extract(loader_name, "^[:alpha:]*(?=_)")
#   if (file_type == "excel") {
#     return(paste0(file_path, ".xls"))
#   } else if (file_type == "csv") {
#     return(paste0(file_path, ".csv"))
#   } else if (file_type == "rdata") {
#     return(paste0(file_path, ".RData"))
#   } else {
#     stop(paste("No file for file_type:", file_type))
#   }
# }

update_nonrevised_production <- function() {

}

# functions to prepare the files' list ---------------------------------------------------------------------------------
# get_national_accounting_data_files <- function(national_accounting_data_folder, estimation_type) {
#   # define the regex pattern corresponding to the estimation_type
#   estimation_pattern <- get_estimation_pattern_for_estimation_type(estimation_type)
#   # get the list of folders containing each quarterly account
#   national_accounting_data_folders <- list.dirs(national_accounting_data_folder, recursive = TRUE, full.names = TRUE)
#   national_accounting_data_folders <- stringr::str_subset(string = national_accounting_data_folders,
#                                                           pattern = paste0(".*/base(?!(1980)|(1995)|(2000))[:digit:]{4}/(?!(10))[:digit:]{2}T[:digit:]", estimation_pattern, "$"))
#   ## Note: the regex means: ".*/base(?!(1980)|(1995)|(2000))[:digit:]{4}/" -> we take the folders that contain data from base 2005 and onward
#   ## "(?!(10))[:digit:]{2}T[:digit:](PE|RD)$" -> we take the folders that contain data for the PE (Premiere Estimation) or RD (Resultats Detailles) in the format e.g. 11T1PE (Première Estimation du T1 2011),
#   ## end we exclude data for the year 2010 for which data follows another classification
#
#   #todo: to delete when other loaders created
#   # production_data_folders <- stringr::str_subset(string = production_data_folders,
#   #                                                pattern = ".*/((1.*)|(20.*)|(21.*)|(22T1PE)|(22T1RD))")
#
#   # give an harmonised name to each file according to the last date of the ipi data
#   names(national_accounting_data_folders) <- stringr::str_extract(national_accounting_data_folders, pattern = "(?<=/)[:digit:]{2}T[:digit:](PE|RD)$")
#   ## Note: to ensure that the folders are read in the good chronological order, it is essential that they are named properly
#
#   # sort the list by the names
#   national_accounting_data_folders <- national_accounting_data_folders[sort(names(production_data_folders))]
#
#   return(national_accounting_data_folders)
# }

# get_estimation_pattern_for_estimation_type <- function(estimation_type) {
#   if (estimation_type %in% c("PE", "RD")) {
#     return(estimation_type)
#   } else if (estimation_type == "all") {
#     return("(PE|RD)")
#   } else {
#     stop("estimation_type can only be: \"PE\", \"RD\" or \"all\".")
#   }
# }