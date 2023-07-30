# Title     : TODO
# Objective : TODO
# Created by: lphung
# Created on: 21/06/2023


# initialise the environment -------------------------------------------------------------------------------------------
rm(list = ls())

# packages -------------------------------------------------------------------------------------------------------------
library(stringr)
library(dplyr)
library(lubridate)
library(stats)
library(forecast)
library(ggplot2)

source("./code/nonrevised_national_accounting/loaders_utils.R", encoding = "utf-8", chdir = TRUE)
source("./code/nonrevised_ipi/loaders_utils.R", encoding = "utf-8", chdir = TRUE)
source("./code/nonrevised_consumption/loaders_utils.R", encoding = "utf-8", chdir = TRUE)
source("./code/doc_travail_interpretation_enquetes/helpers.R", encoding = "utf-8", chdir = TRUE)
source("./code/data_preparator.R", encoding = "utf-8")


# 1. load nonrevised pib ----------------------------------------------------------------------------------------
# preparation of the list
PIB_DATA_FOLDERS <- get_national_accounting_data_files(NATIONAL_ACCOUNTING_DATA_FOLDER,
                                                       starting_period = "base2000",                             # only works for PIB data, look at the function's comments for more details
                                                       estimation_type = "PE"                                    # we keep only the folders containing the 1st estimation of the quarterly accounts (PE)
)

PIB_FILES_TYPES <- list("pre_2011_xls" = stringr::str_subset(names(PIB_DATA_FOLDERS), "(^0.*)|(^10.*)"),
                        "pre_19T2RD_csv" = stringr::str_subset(names(PIB_DATA_FOLDERS), "(^1(?!(0.*)|(9T2RD)|(9T3PE)|(9T3RD)|(9T4PE)|(9T4RD)).*)"),
                        "post_19T2RD_xls" = stringr::str_subset(names(PIB_DATA_FOLDERS), "^((19T2RD)|(19T3PE)|(19T3RD)|(19T4PE)|(19T4RD))|(^20.*)|(^21.*)|^((22T1PE))"),
                        "post_19T2RD_csv" = stringr::str_subset(names(PIB_DATA_FOLDERS), "(^2(?!(0.*)|(1.*)|(2T1PE)).*)") # aucun de ce type avant 2020
)

# preparation of the matrix
nonrevised_pib <- construct_nonrevised_national_account_data_from_scratch(data_source = "national_accounting",
                                                                          files_list = PIB_DATA_FOLDERS,
                                                                          file_type2files_list = PIB_FILES_TYPES,
                                                                          file_name = PIB_FILE_NAME,
                                                                          dimensions_list = PIB_DIMENSIONS,
                                                                          number_previous_values = 47) #24

save(nonrevised_pib, file = paste0("./data/", "nonrevised_pib_", max(unique(nonrevised_pib[["date"]])), "PE.RData"))


# 2. load survey data -----------------------------------------------------------------------------------------------

sheets_to_load <- c("pmi_flash", "indices_synthetiques", "pmi_sous_soldes", "insee_sous_soldes", "insee_contraintes_prod", "bdf_sous_soldes", "donnees_financieres_mensuelles")
survey_data <- load_data_for_nowcasting(PATH_TO_DATA_FOR_NOWCASTING, sheets_to_load = sheets_to_load)


# traitement des données financières journalières (jours ouvrés) : prix de l'or et du pétrole
price_data <- load_data_for_nowcasting_for_sheet(PATH_TO_DATA_FOR_NOWCASTING, sheet = "donnees_financieres_jours_ouvra") %>%
  day_to_month(transformation_type = "carry_over_mean")

# pull all data together
survey_data <- survey_data %>%
  dplyr::bind_rows(price_data)

# transformation des données
survey_data_split <- survey_data %>%
  month_to_quarter(transformation_type = "split")
survey_data_split <- survey_data_split %>%
  get_variation_for(variation_type = "lead", nbr_lead = 1, add_option = TRUE, dimensions_to_transform = stringr::str_subset(unique(survey_data_split$dimension), ".*_m1"))
# Note : we create lead only for the indices at month 1

survey_data_growth_rate <- survey_data_split %>%
  get_variation_for(variation_type = "growth_rate", add_option = TRUE, prefix = "gt") # création du glissement trimestriel (en taux de croissance)

survey_data_difference <- survey_data_split %>%
  get_variation_for(variation_type = "difference", keep_prefix = TRUE, prefix = "difftrim") # création du glissement trimestriel (en différence) # TODO: erase if not performing

# TODO: test: création des variations mensuelles seulement pour les indicateurs financiers
survey_data_vm <- survey_data %>%
  dplyr::filter(dimension %in% stringr::str_subset(unique(dimension), pattern = "^finance_.*")) %>%
  get_variation_for(variation_type = "growth_rate", keep_prefix = TRUE, prefix = "vm") %>%
  month_to_quarter(transformation_type = "split")

full_survey_data <- survey_data_growth_rate %>%
  dplyr::bind_rows(survey_data_difference) %>%
  dplyr::bind_rows(survey_data_vm) %>%
  convert_to_wide_format()
# %>% dplyr::select(-contains("construction"))

save(full_survey_data, file = paste0("./code/doc_travail_interpretation_enquetes/data/survey_data_tresor_eco_", today(), ".RData"))


# 3. load IPI  ---------------------------------------------------------------------------------------------------------

IPI_DATA_FILES <- get_indices_data_files(IPI_DATA_FOLDER)
# TODO: check where to put that -> in a README.md // Note: convention de nommage des fichiers d'IPI: la date du nom de fichier doit contenir la dernière date pour laquelle nous avons l'IPI (et non la date de publication)
IPI_FILES_TYPES <- list("sectors_in_line_one_label_loader" = c("200901"),
                        "sectors_in_line_two_labels_loader" = stringr::str_subset(names(IPI_DATA_FILES), "(^2009(?!(01)).*)|(^2010(?!(11)|(12)).*)"))

# preparation of the matrix
nonrevised_ipi <- construct_nonrevised_ipi_from_scratch(files_list = IPI_DATA_FILES,
                                                        file_type2files_list = IPI_FILES_TYPES,
                                                        number_previous_values = 166, #24
                                                        data_correction = "CJO-CVS") # TODO: data_correction as argument within the function

save(nonrevised_ipi, file = paste0("./data/", "nonrevised_ipi_", max(unique(nonrevised_ipi[["date"]])), ".RData"))


# 4. load household consumption  ---------------------------------------------------------------------------------------

CONSUMPTION_DATA_FILES <- get_indices_data_files(CONSUMPTION_DATA_FOLDER)

CONSUMPTION_FILES_TYPES <- list("up_to_201103" = stringr::str_subset(names(CONSUMPTION_DATA_FILES), "(^2009.*)|(^2010.*)|(^2011(01|02|03))"),
                                "generic" = stringr::str_subset(names(CONSUMPTION_DATA_FILES), "(^2011((0[4-9])|(1.)))|(^201[2-9].*)|(^202[0-3])")
)

# preparation of the matrix
nonrevised_consumption <- construct_nonrevised_consumption_from_scratch(files_list = CONSUMPTION_DATA_FILES,
                                                                        file_type2files_list = CONSUMPTION_FILES_TYPES,
                                                                        dimensions_list = CONSUMPTION_DIMENSIONS,
                                                                        number_previous_values = 166)

save(nonrevised_consumption, file = paste0("./data/", "nonrevised_consumption_", max(unique(nonrevised_consumption[["date"]])), ".RData"))


# 5. load unemployment data --------------------------------------------------------------------------------------------

employment_data <- load_employment_data(PATH_TO_EMPLOYMENT_DATA, dimensions_list = EMPLOYMENT_DIMENSIONS_LIST) %>%
  dplyr::group_by(dimension) %>%
  dplyr::mutate(value = as.numeric(scale(value))) %>% # centered-reduced data
  dplyr::ungroup()

# transformation des données
employment_data_split <- employment_data %>%
  month_to_quarter(transformation_type = "split")
employment_data_split <- employment_data_split %>%
  get_variation_for(variation_type = "lead", nbr_lead = 1, add_option = TRUE, dimensions_to_transform = stringr::str_subset(unique(employment_data_split$dimension), ".*_m1"))
# Note : we create lead only for the indices at month 1

employment_data_growth_rate <- employment_data_split %>%
  get_variation_for(variation_type = "growth_rate", add_option = TRUE, prefix = "gt") # création du glissement trimestriel (en taux de croissance)

employment_data_difference <- employment_data_split %>%
  get_variation_for(variation_type = "difference", keep_prefix = TRUE, prefix = "difftrim") # création du glissement trimestriel (en différence) # TODO: erase if not performing

full_employment_data <- employment_data_growth_rate %>%
  dplyr::bind_rows(employment_data_difference) %>%
  convert_to_wide_format()

save(full_employment_data, file = paste0("./code/doc_travail_interpretation_enquetes/data/full_employment_", today(), ".RData"))

# create the dataframes for the prevision ------------------------------------------------------------------------------

## Note that what matters is the PIB quarterly variable => get the quarterly variable published at each period and then apply
## the variations to the level to get rebased data

# get nonrevised PIB quarterly growth rate
corrected_nonrevised_pib <- nonrevised_pib %>%
  dplyr::select(date, dimension, t, t_1, t_4) %>% # dplyr::select(matches("(date)|(dimension)|(^t$)|(^t_[1-9]$)")) # To keep several t_XX
  dplyr::filter(dimension == "PIB") %>%
  dplyr::mutate(dimension = "nonrevised_pib") %>%
  dplyr::mutate(var1_PIB = t / t_1 - 1,
                var4_PIB = t / t_4 - 1) %>%
  dplyr::select(date, var1_PIB)  # TODO: pou l'instant, le glissement annuel du PIB ne nous sert pas (var4_PIB)

# get nonrevised IPI -------------------------------------------------------------
corrected_nonrevised_ipi <- nonrevised_ipi %>%
  dplyr::filter(dimension %in% c("BE", "CZ", "DE"))

# création de la variation mensuelle de l'IPI  # TODO: delete if not useful
ipi_vm <- corrected_nonrevised_ipi %>%
  dplyr::select(date, dimension, t, t_1) %>%
  dplyr::mutate(value = t / t_1 - 1) %>%
  dplyr::mutate(dimension = paste0("vm_ipi_", dimension)) %>%
  dplyr::select(date, dimension, value) %>%
  month_to_quarter(transformation_type = "split")

# création du glissement trimestriel de l'IPI
ipi_gt <- corrected_nonrevised_ipi %>%
  dplyr::select(date, dimension, t, t_3) %>%
  dplyr::mutate(value = t / t_3 - 1) %>%
  dplyr::mutate(dimension = paste0("gt_ipi_", dimension)) %>%
  dplyr::select(date, dimension, value) %>%
  month_to_quarter(transformation_type = "split")

# création du glissement annuel de l'IPI
ipi_ga <- corrected_nonrevised_ipi %>%
  dplyr::select(date, dimension, t, t_12) %>%
  dplyr::mutate(value = t / t_12 - 1) %>%
  dplyr::mutate(dimension = paste0("ga_ipi_", dimension)) %>%
  dplyr::select(date, dimension, value) %>%
  month_to_quarter(transformation_type = "split")

# création des IPI aux différents mois du trimestre + création de la variation trimestrielle de l'IPI (avec acquis au dernier mois disponible)
ipi_blocking_method <- corrected_nonrevised_ipi %>%
  get_quarterly_variation_for_nonrevised_monthly_data(quarterly_variation_column_name = "ipi", keep_level_columns = TRUE)
ipi_blocking_method <- ipi_blocking_method %>%
  tidyr::pivot_wider(id_cols = colnames(ipi_blocking_method),
                     names_from = dimension,
                     values_from = c(ipi_m1, ipi_m2, ipi_m3, var1_ipi))
names(ipi_blocking_method) <- str_replace(names(ipi_blocking_method), pattern = "(ipi)_(m[:digit:])_([:alpha:]{2})", replacement = "\\1_\\3_\\2")

# joindre toutes les données d'IPI entre elles
ipi_data <- ipi_vm %>%
  dplyr::bind_rows(ipi_gt) %>%
  dplyr::bind_rows(ipi_ga) %>%
  convert_to_wide_format() %>%
  dplyr::full_join(ipi_blocking_method, by = "date")

# get nonrevised household consumption -------------------------------------------------------------
corrected_nonrevised_consumption <- nonrevised_consumption %>%
  dplyr::filter(dimension %in% c("consommation_biens_manufactures"))

# # création de la variation mensuelle de la consommation des ménages  # TODO: à ajouter si utile, mais je ne pense pas que ça le soit
# consumption_vm <- corrected_nonrevised_consumption %>%
#   dplyr::select(date, dimension, t, t_1) %>%
#   dplyr::mutate(value = t / t_1 - 1) %>%
#   dplyr::mutate(dimension = paste0("vm_", dimension)) %>%
#   dplyr::select(date, dimension, value) %>%
#   month_to_quarter(transformation_type = "split")

# création du glissement trimestriel de la consommation des ménages
consumption_gt <- corrected_nonrevised_consumption %>%
  dplyr::select(date, dimension, t, t_3) %>%
  dplyr::mutate(value = t / t_3 - 1) %>%
  dplyr::mutate(dimension = paste0("gt_", dimension)) %>%
  dplyr::select(date, dimension, value) %>%
  month_to_quarter(transformation_type = "split")

# création du glissement annuel de la consommation des ménages
consumption_ga <- corrected_nonrevised_consumption %>%
  dplyr::select(date, dimension, t, t_12) %>%
  dplyr::mutate(value = t / t_12 - 1) %>%
  dplyr::mutate(dimension = paste0("ga_", dimension)) %>%
  dplyr::select(date, dimension, value) %>%
  month_to_quarter(transformation_type = "split")

# création de la consommation des ménages aux différents mois du trimestre + création de la variation trimestrielle (avec acquis au dernier mois disponible)
consumption_blocking_method <- corrected_nonrevised_consumption %>%
  get_quarterly_variation_for_nonrevised_monthly_data(quarterly_variation_column_name = "consommation_biens_manufactures", keep_level_columns = TRUE) %>%
  dplyr::select(-dimension)

# joindre toutes les données de consommation des ménages entre elles
consumption_data <- consumption_gt %>%
  # dplyr::bind_rows(consumption_vm) %>%
  dplyr::bind_rows(consumption_ga) %>%
  convert_to_wide_format() %>%
  dplyr::full_join(consumption_blocking_method, by = "date")


# joindre tous les données entre elles --------------------------------------------
full_data <- corrected_nonrevised_pib %>%
  dplyr::full_join(full_survey_data, by = "date") %>%
  dplyr::full_join(ipi_data, by = "date") %>%
  dplyr::full_join(consumption_data, by = "date") %>%
  dplyr::full_join(full_employment_data, by = "date")

# prepare data for regression
prevision_data <- full_data %>%
  dplyr::arrange(date)

save(prevision_data, file = paste0("./code/doc_travail_interpretation_enquetes/data/prevision_data_", today(), ".RData"))

