# Title     : import survey data
# Created by: lphung
# Created on: 06/09/2022

# packages -----------------------------------------------------------------------------------------------------------
library(dplyr)
library(stringr)
library(readxl)

# constants ------------------------------------------------------------------------------------------------------------
DGTRESOR_S_SERVER_PATH <- "S:/SPMAE/PREV/Prev3/_Fichiers_Prev3" # TODO: to remove for public release -> create a branch for public release
PATH_TO_DATA_FOR_NOWCASTING <- file.path(DGTRESOR_S_SERVER_PATH, "Synthèse/1_donnees/donnees_pour_nowcasting_en_temps_reel.xlsx")

# functions to load data -----------------------------------------------------------------------------------------------

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
  return(survey_data)
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
