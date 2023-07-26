# Title     : meta-functions that call the loaders to construct the nonrevised household consumption series
# Created by: lphung
# Created on: 26/07/2023

# packages -------------------------------------------------------------------------------------------------------------
library(stringr)
library(dplyr)
library(tidyr)
source("./loaders.R", encoding = "utf-8", chdir = TRUE)
# chdir = TRUE needed because we call this Rscript from the main.R and from a RMarkdown, which define working directory differently

# folders --------------------------------------------------------------------------------------------------------------
CONSUMPTION_DATA_FOLDER <- "S:/SPMAE/PREV/Prev3/_Fichiers_Prev3/Stages/Antoine_Claisse/donnees/donnees_conso_menages"

# constantes -----------------------------------------------------------------------------------------------------------

# TODO: for now, to ease the work of the function complete_missing_data_for_consumption(), we only keep the sectors that are present in all datafiles
CONSUMPTION_DIMENSIONS <- list("up_to_201103" = c("Produits manufacturés" = "consommation_biens_manufactures",  # à gauche: nom des colonnes dans le Excel, à droite: le nom qu'on souhaite leur donner
                                                  "Biens durables" = "consommation_biens_durables",
                                                  "Textile-cuir" = "consommation_textile_cuir",
                                                  # "Autres produits manufacturés" = "consommation_autres_manufactures",
                                                  "Equipement du logement" = "consommation_equipement_logement"
                                                  # "Automobiles" = "consommation_automobile",
                                                  # "Commerce de détail" = "consommation_commerce_detail"
),
                               "generic" = c("Biens manufacturés" = "consommation_biens_manufactures",  # à gauche: nom des colonnes dans le Excel, à droite: le nom qu'on souhaite leur donner
                                             # "Biens fabriqués" = "consommation_biens_fabriques",
                                             "Biens durables" = "consommation_biens_durables",
                                             # "Automobiles" = "consommation_automobile",                # cette catégorie évolue au cours du temps (soit "Automobiles" soit "Matériels de transport") - voir si ça correspond au même champ ou pas
                                             # "Matériels de transport" = "consommation_materiels_transport",
                                             "Equipement du logement" = "consommation_equipement_logement",
                                             "Textile-cuir" = "consommation_textile_cuir"
                                             # "Autres biens fabriqués" = "consommation_autres_fabriques"
                                             # "Energie" = "consommation_energie",
                                             # "Cokéfaction et raffinage" = "consommation_cokefaction_raffinage"
                               ))

# functions ------------------------------------------------------------------------------------------------------------
construct_nonrevised_consumption_from_scratch <- function(files_list, file_type2files_list, dimensions_list, number_previous_values = 0) {
  df <- get_empty_dataframe(number_previous_values)
  # column_to_join_by <- get_column_names_to_join_by()

  loader_provider <- get_loader_provider(data_source = "consumption")
  for (file_name in names(files_list)) {
    print(paste("On lit le fichier :", file_name))
    loader <- get_loader(file_name, file_type2files_list, loader_provider)
    new_data <- loader(files_list[[file_name]], dimensions_list)
    df <- construct_nonrevised_series(df, new_data, date_granularity = "month", number_previous_values = number_previous_values)
    rm(new_data)
  }
  df <- df %>%
    dplyr::arrange(date) %>%
    complete_missing_data_for_consumption()

  return(df)
}

get_loader_for_consumption <- function(file_type) {
  if (file_type == "generic") {
    return(consumption_generic_loader)
  } else if (file_type == "up_to_201103") {
    return(consumption_loader_up_to_201103)
  } else {
    stop(paste0("No loader found for the file_type: ", file_type))
  }
}


complete_missing_data_for_consumption <- function(nonrevised_data) {
  # Note, for some years (2009 to 2015 included), no data is published in July; no data is published on January 2019. So we will use the data from the month after.
  # Beware, the values must be shift: the value of July 2009 from the observation line of August 2008 is in the t_1 column, etc.
  vector_of_complete_dates <- seq.Date(from = as.Date(min(unique(nonrevised_data$date))),
                                       to = as.Date(max(unique(nonrevised_data$date))),
                                       by = "month")
  date_dataframe <- data.frame("date" = rep(vector_of_complete_dates, times = length(unique(nonrevised_data$dimension))),
                               "dimension" = rep(unique(nonrevised_data$dimension), each = length(vector_of_complete_dates)))

  # extend dataset
  complete_data <- nonrevised_data %>%
    dplyr::full_join(date_dataframe, by = c("date", "dimension")) %>%
    arrange(date)

  # complete dataset
  vector_of_missing_dates <- c("2009-07-01", "2010-07-01", "2011-07-01", "2012-07-01", "2013-07-01", "2014-07-01", "2015-07-01", "2019-01-01")
  for (missing_date in vector_of_missing_dates) {
    # put the missing_date in date format; note that if vector_of_missing_dates would be a date vector, the date would be lost through the for operator
    missing_date <- lubridate::ymd(missing_date)
    for (dimension in unique(complete_data$dimension)) {
      data_to_fill <- complete_data[complete_data$date == missing_date + months(1) & complete_data$dimension == dimension,] %>%
        dplyr::mutate(date = as.Date(missing_date)) %>%
        dplyr::select(-t) %>%        # we remove the t column to shift all the columns by 1
        dplyr::mutate(new_col = NA)  # we add one empty column at the end so we have the same number of columns; because we shift all the value columns, the last one will be empty
      names(data_to_fill) <- names(complete_data)
      complete_data[complete_data$date == as.Date(missing_date) & complete_data$dimension == dimension,] <- data_to_fill
    }
  }
  return(complete_data)
}