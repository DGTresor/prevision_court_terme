# Title     : import IPI and industrial production from the Insee database
# Created by: lphung
# Created on: 06/09/2022

# packages -----------------------------------------------------------------------------------------------------------
library(dplyr)
library(stringr)
library(insee)

# constants ------------------------------------------------------------------------------------------------------------
IPI_SECTORS_NAF2 <- c("A10-CZ", "A17-C1", "A17-C2", "A17-C3", "A17-C4", "A17-C5", "A38-CF", "A10-BE", "A17-DE")

# Liste pour avoir le label des secteurs
SECTORS_LABEL_LIST <- list(
  "BE" = "Industrie",
  "CZ" = "Industrie manufacturière",
  "C1" = "Agro-alimentaire",
  "C2" = "Cokéfaction-raffinage",
  "C3" = "Biens d'équipement",
  "C4" = "Matériels de transport",
  "C5" = "Autes produits industriels",
  "CI" = "Produits informatiques",
  "CJ" = "Equipements électriques",
  "CK" = "Machines et équipements",
  "CL1" = "Automobile",
  "CL2" = "Autres matériels de transport",
  "CB" = "Textile",
  "CC" = "Bois-papier",
  "CF" = "Pharmacie",
  "CG" = "Caoutchouc-plastique",
  "CH" = "Métallurgie",
  "CM" = "Autres industries",
  "FZ" = "Construction",
  "DE" = "Energie"
)

SECTORS_PLURAL_LIST <- c("C4", "C5", "CI", "CJ", "CK", "CL2", "CM")

# functions ------------------------------------------------------------------------------------------------------------
load_data_from_insee <- function(dimensions_to_load, dimensions_label_list){
  # parameters to connect to the Insee database
  Sys.setenv(INSEE_download_option_method = "curl")
  Sys.setenv(INSEE_download_option_port = "8080")
  Sys.setenv(INSEE_download_option_extra = "-U : --proxy-ntlm --proxy http.proxyvip.alize:8080")
  Sys.setenv(INSEE_download_option_proxy = "http.proxyvip.alize")
  Sys.setenv(INSEE_download_option_auth = "ntlm")

  # list of the Insee's datasets
  # dataset_list = get_dataset_list() # useful to get look for the data we need

  # get data
  data <-  get_insee_idbank(get_ipi_series_id_for_sector(dimensions_to_load[1])) %>%
    select(DATE, OBS_VALUE) %>%
    mutate(DIMENSION = dimensions_to_load[1])

  for(dimension in dimensions_to_load[2:length(dimensions_to_load)]){
    new_data <-  get_insee_idbank(get_ipi_series_id_for_sector(dimension)) %>%
      select(DATE, OBS_VALUE) %>%
      mutate(DIMENSION = dimension)

    data <- data %>%
      bind_rows(new_data)
  }

  clean_data <- data_cleaner(data, dimensions_label_list)
  return(clean_data)
}


get_ipi_series_id_for_sector <- function(sector){
  series_id <- get_idbank_list("IPI-2015") %>%
    filter(NAF2 == sector) %>% #sector
    filter(NATURE == "INDICE") %>%
    filter(CORRECTION == "CVS-CJO") %>%  #SA-WDA, seasonally adjusted, working day adjusted
    pull(idbank)

  return(series_id)
}


data_cleaner <- function(data, columns_label) {
  # rename the columns
  clean_data <- data %>%
    rename(date = DATE,
           dimension = DIMENSION,
           value = OBS_VALUE)

  # change the sector's id: keep only the last 2 characters
  regex_pattern <- "([:alpha:]|[:digit:]){2}$"                                  # use the regex pattern you need to clean the name of your dimensions, if it is not clean
  clean_data <- clean_data %>%
    mutate(dimension = str_extract(clean_data$dimension, pattern = regex_pattern))

  # add columns' label
  clean_data$label <- mapper(clean_data$dimension, columns_label)

  # reorganise the dataframe
  clean_data <- clean_data %>%
    select(date, dimension, label, value) %>%
    arrange(date)

  return(clean_data)
}

mapper <- function(column_to_map, map) {
  return(ifelse(column_to_map %in% names(map), as.character(map[column_to_map]), as.character(column_to_map)))
}