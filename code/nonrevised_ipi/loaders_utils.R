# Title     : meta-functions that call the loaders to construct the nonrevised ipi series
# Created by: lphung
# Created on: 12/09/2022

# packages -------------------------------------------------------------------------------------------------------------
library(stringr)
library(dplyr)
library(tidyr)
source("./loaders.R", encoding = "utf-8", chdir = TRUE)
# chdir = TRUE needed because we call this Rscript from the main.R and from a RMarkdown, which define working directory differently

# folders --------------------------------------------------------------------------------------------------------------
DGTRESOR_S_SERVER_PATH <- "S:/SPMAE/PREV/Prev3/_Fichiers_Prev3" # TODO: to remove for public release -> create a branch for public release
IPI_DATA_FOLDER <- file.path(DGTRESOR_S_SERVER_PATH, "Prod_manuf/02-IPI/mail_reaction_ipi/02-Envoi_Insee")

# functions ------------------------------------------------------------------------------------------------------------
construct_nonrevised_ipi_from_scratch <- function(files_list, file_type2files_list, number_previous_values = 0, data_correction = "CJO-CVS") {
  df <- get_empty_dataframe(number_previous_values)
  # column_to_join_by <- get_column_names_to_join_by()

  loader_provider <- get_loader_provider(data_source = "ipi")
  for (file_name in names(files_list)) {
    print(paste("On lit le fichier :", file_name))
    loader <- get_loader(file_name, file_type2files_list, loader_provider)
    new_data <- loader(files_list[[file_name]], data_correction = data_correction)
    df <- construct_nonrevised_series(df, new_data, date_granularity = "month", number_previous_values = number_previous_values)
    rm(new_data)
  }
  df <- df %>% dplyr::arrange(date)
  #select(all_of(sort(colnames(data_to_bind))))
  return(df)
}

get_loader_for_ipi <- function(file_type) {
  if (file_type == "sectors_in_line_one_label_loader") {
    return(sectors_in_line_one_label_loader)
  } else if (file_type == "sectors_in_line_two_labels_loader") {
    return(sectors_in_line_two_labels_loader)
  } else if (file_type == "generic") {
    return(generic_loader)
  } else {
    stop(paste0("No loader found for the file_type: ", file_type))
  }
}