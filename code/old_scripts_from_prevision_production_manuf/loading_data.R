# Title     : load data
# Created by: lphung
# Created on: 13/07/2022

# libraries ------------------------------------------------------------------------------------------------------------

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(insee)
library(stringr)

# paths ----------------------------------------------------------------------------------------------------------------
## series of national accounts' manufacturing production
CNAT_PROD_MANUF_PATH <- "S:/SPMAE/PREV/Prev3/France/CNT14/Prix_CH/CNTmanuf_CH.xls"
CNAT_PROD_MANUF_DESA_PATH <- "S:/SPMAE/PREV/Prev3/France/CNT14/Prix_CH/CNTmanufDesa_CH.xls"
# TODO: check difference between the two and decide what to choose; also decide why this file and not another -> original code uses "CNATmanufDesa_CH", which data comes from cprvolch.xlsx
# TODO: "CNTmanufDesa_CH" data comes from DataInsight "Production Produit - Biens Manufactures - Volume aux Prix de l'Annee Precedente Chaines - Serie CVS-CJO - France", ID: "INSE010565219.Q"
# TODO: check if we can load data directly from Insee

## path to PMI survey's balances data
PATH_TO_PMI_DATA <- "S:/SPMAE/PREV/Prev3/_Fichiers_Prev3/Synthèse/6. Enquêtes PMI/4. Préparation mail réaction PMI/Données/Data PMI.xlsx"
# TODO: import that from DataInsight directly ; or DBnomics ?

## path to Insee survey's balances data
PATH_TO_INSEE_MANUF <- "S:/SPMAE/PREV/Prev3/_Fichiers_Prev3/Prod_manuf/04-Enquetes/01-Enq_Insee/03-Series_Insee_DI/DI_EnqIndMens_Insee.xlsx"
PATH_TO_QUARTERLY_INSEE_MANUF <- "S:/SPMAE/PREV/Prev3/_Fichiers_Prev3/Prod_manuf/04-Enquetes/01-Enq_Insee/03-Series_Insee_DI/DI_EnqIndTrim_Insee.xlsx"
# TODO: import that from the Insee online database directly; note that some quarterly balances became monthly

## path to Banque de France (BdF or bdf) survey's balances data
PATH_TO_BDF_MANUF <- "S:/SPMAE/PREV/Prev3/_Fichiers_Prev3/Prod_manuf/04-Enquetes/02-Enq_BdF/02-Series_longues/DI_EnqMensInd_BdF.xlsx"
# TODO: import that from the Insee online database directly

# constants ------------------------------------------------------------------------------------------------------------
INSEE_IPI_CODES <- c("A10-CZ")
IPI_SECTOR_LABEL_LIST <- list("CZ" = "Industrie manufacturière")

PMI_EXCEL_DIMENSIONS_TO_DATAFRAME_DIMENSIONS <- list(
  "industrie" = "climat", # Pour "Synthétique industrie"
  "Production pass.e" = "production_passee", # Pour "Production passée"
  "Emploi" = "emploi", # équivalent à "effectifs_passes" pour les enquêtes Insee et Banque de France
  "Nouvelles commandes" = "new_commandes",
  "Stocks d'intrants" = "stocks_achats",
  "livraisons" = "delais_livraison",
  "Commandes Export" = "commandes_export",
  "Prix output" = "prix_factures",
  "Prix input" = "prix_achats"
)

INSEE_EXCEL_DIMENSIONS_TO_DATAFRAME_DIMENSIONS <- list(
  "Indicateur synth.tique" = "climat", # Pour "Indacteur synthétique d'activité"
  "Indicateur de retournement" = "retournement",
  "Production pass.e" = "production_passee", # Pour "Production passée"
  "Production pr.vue" = "production_prevue", # Pour "Production prévue", encore appelée "Perspectives personnelles"
  "Perspectives g.n.rales" = "perspectives_generales", # Pour "Prespectives générales"
  "Carnets globaux" = "carnets_globaux",
  "Carnets .trangers" = "carnets_etrangers", # Pour "Carnets étrangers"
  "Stocks de produits finis" = "stocks_produits_finis",
  "Prix de vente" = "prix_factures",
  "Prix industriels" = "prix_industriels", # NB: la question des prix industriels diffère par rapport au prix des achats dans l'enquête PMI; dans l'enquête Insee elle concerne une vision globale du secteur industriel
  "Effectifs pass.s" = "effectifs_passes", # Pour "Effectifs passés"
  "Effectifs pr.vus" = "effectifs_prevus", # Pour "Effectifs prévus"
  "Indicateur de surprise lisse" = "surprise_lisse", # Attention, l'ordre est important pour le remplacement!
  "Indicateur de surprise" = "surprise"
)

INSEE_QUARTERLY_EXCEL_DIMENSIONS_TO_DATAFRAME_DIMENSIONS <- list(
  "difficult.s d.offre et de demande" = "difficultes_offre_demande", # Attention, l'ordre est important pour le remplacement!
  "difficult.s d.offre" = "difficultes_offre",
  "difficult.s de demande" = "difficultes_demande",
  "difficult.s de recrutement" = "difficultes_recrutement",
  "aucune difficult." = "aucune_difficulte",
  "demande insuffisante" = "demande_insuffisante",
  "difficult.s d..quipement" = "difficultes_equipement",
  "personnel insuffisant" = "personnel_insuffisant",
  "contraintes financi.res" = "contraintes_financieres",
  "difficult.s d.approvisionnement" = "difficultes_approvisionnement",
  "autres difficult.s" = "autres_difficultes",
  "Peut accro.te production avec embauche" = "peut_accroitre_prod",
  "Goulots de production" = "goulots_production",
  "Goulots d..quipement uniquement" = "goulots_equipement_uniquement",
  "Goulots d..quipement" = "goulots_equipement",
  "Autres types de goulots" = "autres_goulots",
  "Jugement capacit. de production" = "jugement_capacite",
  "TUC" = "TUC"
)

# loading functions ----------------------------------------------------------------------------------------------------
## loading national accounts' data from excel
load_cnat_prod_manuf <- function(path_to_data) {
  suppressMessages(raw_prod_data <- readxl::read_excel(path = path_to_data, sheet = "niv", col_names = FALSE, skip = 3)[, c(1:2)]) # suppressMessages() to not display the naming of columns
  colnames(raw_prod_data) <- c("trim_date", "value")

  # deal with the date
  ## Note: the date column "trim_date" is in the format "aaTq" with aa the two last digits of the year and q the quarter
  ## Note: month = quarter * 3 - 2
  ## Note: to get the year we need to differenciate between 19aa and 20aa; data goes from 1978 to recent year
  formated_prod_data <- raw_prod_data %>%
    dplyr::mutate(quarter = substr(trim_date, start = 4, stop = 4),
           year_2digits = substr(trim_date, start = 1, stop = 2)) %>%
    dplyr::mutate(month = as.numeric(quarter) * 3 - 2,   #TODO: CHECK: better to do that or to do a case when
           year = ifelse(year_2digits >= 78,
                         paste0("19", year_2digits),
                         paste0("20", year_2digits))) %>%
    dplyr::mutate(date = lubridate::make_date(year = year, month = month)) # TODO: CHECK comparison with: lubridate::ymd(paste(year, month, "01"))

  # name the dimension
  formated_prod_data <- formated_prod_data %>%
    dplyr::mutate(dimension = "prod_manuf") %>%
    dplyr::select(date, dimension, value)

  return(formated_prod_data)
}

## loading insee IPI data from the insee online database
load_ipi_from_insee <- function(insee_ipi_codes) {
  ipi_data <- load_data_from_insee(insee_series_code = insee_ipi_codes, # we want the IPI only for manufacturing production
                                   data_type = "ipi")
  formated_ipi_data <- ipi_data %>%
    insee_data_formater(regex_pattern_to_clean_insee_codes = ".*") %>%   # no need to clean insee code; useful if we work we several sectors
    dplyr::mutate(dimension = "ipi") # rename the insee code to "ipi"

  return(formated_ipi_data)
}

## loading PMI data from excel
load_pmi_data_from_excel <- function(path_to_data, column_list, dimensions_label = NULL) {
  suppressMessages(imported_data <- readxl::read_xlsx(path = path_to_data, sheet = "France", skip = 1)[-c(1:4),]) # suppress messages to prevent message of columns' type and column renaming
  clean_data <- excel_dataframe_cleaner(imported_data, column_list)
  pmi_industry <- clean_data[, c(1, 3:11)]  # keep only the columns corresponding to the indstry survey's balances

  # all balances are in columns => need to do a pivot
  pmi_industry <- prepare_balances_data(pmi_industry, data_source = "pmi")

  # add labels if exist and reorganise the dataframe
  if (!is.null(dimensions_label)) {
    pmi_industry$label <- mapper(pmi_industry$dimension, dimensions_label)
    pmi_industry <- pmi_industry %>%
      dplyr::select(date, dimension, label, value)
  }
  return(pmi_industry)
}

## loading monthly Insee's survey data from excel
load_monthly_insee_survey_data_from_excel <- function(path_to_data, column_list) {
  suppressMessages(raw_insee <- readxl::read_excel(path = path_to_data, skip = 2)[-c(1, 2), c(1, 12:25)]) # suppress messages to prevent message of columns' type and column renaming
  formated_insee <- raw_insee %>%
    excel_dataframe_cleaner(column_list)

  # all balances are in columns => need to do a pivot
  formated_insee <- prepare_balances_data(formated_insee, data_source = "insee")

  return(formated_insee)
}

## loading monthly Insee's survey data from excel
load_quarterly_insee_survey_data_from_excel <- function(path_to_data, column_list){
  quarterly_insee_data <- load_quarterly_insee_survey_data_from_one_excel_sheet(path_to_data = path_to_data, excel_sheet = "difficulte_offre_demande", columns_to_select = c(1:4), column_list = column_list) %>%
    dplyr::bind_rows(load_quarterly_insee_survey_data_from_one_excel_sheet(path_to_data = path_to_data, excel_sheet = "difficulte_detail", columns_to_select = c(1,3:10), column_list = column_list)) %>%
    dplyr::bind_rows(load_quarterly_insee_survey_data_from_one_excel_sheet(path_to_data = path_to_data, excel_sheet = "goulot_production", columns_to_select = c(1:3), column_list = column_list)) %>%
    dplyr::bind_rows(load_quarterly_insee_survey_data_from_one_excel_sheet(path_to_data = path_to_data, excel_sheet = "goulot_detail", columns_to_select = c(1:4), column_list = column_list)) %>%
    dplyr::bind_rows(load_quarterly_insee_survey_data_from_one_excel_sheet(path_to_data = path_to_data, excel_sheet = "TUC", columns_to_select = c(1:3), column_list = column_list))

    return(quarterly_insee_data)
}

load_quarterly_insee_survey_data_from_one_excel_sheet <- function(path_to_data, excel_sheet, columns_to_select, column_list) {
  suppressMessages(raw_insee <- readxl::read_excel(path = path_to_data, sheet = excel_sheet, skip = 1)[-c(1, 2), columns_to_select]) # suppress messages to prevent message of columns' type and column renaming
  formated_insee <- raw_insee %>%
    excel_dataframe_cleaner(column_list)

  # all balances are in columns => need to do a pivot
  formated_insee <- prepare_balances_data(formated_insee, data_source = "insee")

  return(formated_insee)
}

## loading Banque de France's survey data from excel
load_bdf_survey_data_from_excel <- function(path_to_data) {
  suppressMessages(raw_bdf <- readxl::read_excel(path = path_to_data, skip = 2)[-c(1:4), c(1:10, 12:17)]) # suppress messages to prevent message of columns' type and column renaming
  formated_bdf <- raw_bdf %>%
    excel_dataframe_cleaner()

  # all balances are in columns => need to do a pivot
  formated_bdf <- prepare_balances_data(formated_bdf, data_source = "bdf")

  return(formated_bdf)
}

# helpers to load insee data -------------------------------------------------------------------------------------------
load_data_from_insee <- function(insee_series_code, data_type) {
  # parameters to connect to the Insee database
  Sys.setenv(INSEE_download_option_method = "curl")
  Sys.setenv(INSEE_download_option_port = "8080")
  Sys.setenv(INSEE_download_option_extra = "-U : --proxy-ntlm --proxy http.proxyvip.alize:8080")
  Sys.setenv(INSEE_download_option_proxy = "http.proxyvip.alize")
  Sys.setenv(INSEE_download_option_auth = "ntlm")

  # list of the Insee's datasets
  # dataset_list = get_dataset_list() # useful to get look for the data we need

  # use the proper loading function according to the data type
  loading_function <- get_insee_loading_function(data_type)

  # get data
  data <- insee::get_insee_idbank(loading_function(insee_series_code[1])) %>%
    select(DATE, OBS_VALUE) %>%
    mutate(DIMENSION = insee_series_code[1])

  if (length(insee_series_code) > 1) {
    for (dimension in insee_series_code[2:length(insee_series_code)]) {
      new_data <- insee::get_insee_idbank(loading_function(dimension)) %>%
        select(DATE, OBS_VALUE) %>%
        mutate(DIMENSION = dimension)

      data <- data %>%
        bind_rows(new_data)
    }
  }
  return(data)
}

get_insee_loading_function <- function(data_type) {
  if (data_type == "ipi") {
    return(get_ipi_series_id_for_sector)
  } else {
    stop("Choose \"ipi\" as data_type argument for the get_loading_function().")
  }
}

get_ipi_series_id_for_sector <- function(sector) {
  series_id <- insee::get_idbank_list("IPI-2015") %>%
    filter(NAF2 == sector) %>% #sector
    filter(NATURE == "INDICE") %>%
    filter(CORRECTION == "CVS-CJO") %>%  #SA-WDA, seasonally adjusted, working day adjusted
    pull(idbank)

  return(series_id)
}

insee_data_formater <- function(data, regex_pattern_to_clean_insee_codes, columns_label = list()) {
  # rename the columns
  clean_data <- data %>%
    rename(date = DATE,
           dimension = DIMENSION,
           value = OBS_VALUE)

  # use the regex pattern you need to clean the name of your dimensions, if it is not clean
  clean_data <- clean_data %>%
    mutate(dimension = stringr::str_extract(clean_data$dimension, pattern = regex_pattern_to_clean_insee_codes))

  # add columns' label
  if (length(columns_label) > 0) {
    clean_data$label <- mapper(clean_data$dimension, columns_label)
  }

  # reorganise the dataframe
  clean_data <- clean_data %>%
    select(date, dimension, contains("label"), value) %>%
    arrange(date)

  return(clean_data)
}

# general helpers ------------------------------------------------------------------------------------------------------

mapper <- function(column_to_map, map) {
  return(ifelse(column_to_map %in% names(map), as.character(map[column_to_map]), as.character(column_to_map)))
}

excel_dataframe_cleaner <- function(imported_data, conversion_list_for_column_names = NULL) {
  # cleaning the date column
  clean_data <- imported_data
  colnames(clean_data)[1] <- "date"
  clean_data <- clean_data %>%
    mutate(date = as.Date(as.numeric(date), origin = "1900-01-01") - days(2)) %>%  # The default Excel's origin date is 00/01/1900 but there is a problem when indicating that
    # the trick to remove 2 days (- days(2)) works well
    filter(!is.na(date))                                                           # We remove all the lines for which there is no date

  # convert all columns except the date one into numeric
  # NB: must be done before rename the columns because mutate() cannot work with duplicate columns (i.e. columns with the same name)
  clean_data <- clean_data %>%
    mutate_if(is.character, as.numeric)

  # changing the column' names to appropriate column names
  if (!is.null(conversion_list_for_column_names)) {
    for (name in names(conversion_list_for_column_names)) {
      colnames(clean_data)[stringr::str_detect(colnames(clean_data), name)] <- conversion_list_for_column_names[[name]]
    }
  }

  return(clean_data)
}

prepare_balances_data <- function(survey_balances, data_source) {
  # put data in long format
  columns_to_pivot <- colnames(survey_balances)[colnames(survey_balances) != "date"]
  balances_data <- survey_balances %>%
    pivot_longer(cols = all_of(columns_to_pivot),
                 names_to = "dimension",
                 values_to = "value")

  # mention the sector's name in the dimensions; notably to discriminate between industry and services balances that are named the same way
  ## TODO: check which method is best
  # balances_data <- balances_data %>%
  #   mutate(dimension = paste(sector_name, dimension, sep = "_")) %>%
  #   remove_repetition_in_dimension_names()
  balances_data$dimension <- paste(data_source, balances_data$dimension, sep = "_")

  return(balances_data)
}