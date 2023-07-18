# Title     : Real-time nowcasting of French GDP based on survey data
# Objective : Reproduce the results of the Document de Travail n°XXX of the French Treasury (Direction générale du Trésor)
# Created by: lphung
# Created on: 08/05/2023

# disclaimer -----------------------------------------------------------------------------------------------------------
# The results of the Document de Travail n°XXX of the French Treasury (Direction générale du Trésor) have been obtained with the following data:
# For revised GDP data: the GDP series from the First Estimation of French National Accounting of the 1st quarter of 2023
# For survey data: raw survey data have been extracted using the DataInsight software on July 18th, 2023

# Note: GDP in French if PIB. Hence, PIB variables relate to GDP.

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
source("./code/doc_travail_interpretation_enquetes/helpers.R", encoding = "utf-8", chdir = TRUE)
source("./code/data_preparator.R", encoding = "utf-8")

source("./code/scripts_from_automatisation_reactions/general_graph_functions.R", encoding = "utf-8", chdir = TRUE)

# constants to define --------------------------------------------------------------------------------------------------
UPDATE_REVISED_PIB_DATA <- FALSE
UPDATE_NONREVISED_PIB_DATA <- FALSE
UPDATE_SURVEY_DATA <- FALSE

# 1. load revised GDP --------------------------------------------------------------------------------------------------
if (UPDATE_REVISED_PIB_DATA) {
  # Note: we need data up to the First Estimation (i.e. première estimation in French, PE) of 2019T4 for the doc_travail
  revised_pib <- xls_national_accounting_loader(file_path = file.path(NATIONAL_ACCOUNTING_DATA_FOLDER_BASE2014, "23T1PE"),
                                                folder_name = "23T1PE",
                                                file_name = "erevolch",
                                                dimensions_list = PIB_DIMENSIONS,
                                                dimensions_list_name = "post_19T2RD")

  save(revised_pib, file = paste0("./data/", "revised_pib_", max(unique(revised_pib[["date"]])), "PE.RData")) # ATTENTION: choose PE or RD
} else {
  load("./data/revised_pib_2023-01-01PE.RData")
}

# 2. load nonrevised GDP -----------------------------------------------------------------------------------------------
if (UPDATE_NONREVISED_PIB_DATA) {
  # preparation of the list of GDP data files containing all the vintages
  PIB_DATA_FOLDERS <- get_national_accounting_data_files(NATIONAL_ACCOUNTING_DATA_FOLDER,
                                                         starting_period = "base2000",                             # only works for PIB data, look at the function's comments for more details
                                                         estimation_type = "PE",                                   # we keep only the folders containing the 1st estimation of the quarterly accounts (PE)
                                                         subset_regex = ".*/(?!(2.))[:digit:]{2}T[:digit:]PE"      # we keep data only up to 2019T4
  )

  PIB_FILES_TYPES <- list("pre_2011_xls" = stringr::str_subset(names(PIB_DATA_FOLDERS), "(^0.*)|(^10.*)"),
                          "pre_19T2RD_csv" = stringr::str_subset(names(PIB_DATA_FOLDERS), "(^1(?!(0.*)|(9T2RD)|(9T3PE)|(9T3RD)|(9T4PE)|(9T4RD)).*)"),
                          "post_19T2RD_xls" = stringr::str_subset(names(PIB_DATA_FOLDERS), "^((19T2RD)|(19T3PE)|(19T3RD)|(19T4PE)|(19T4RD))|(^20.*)|(^21.*)|^((22T1PE))"),
                          "post_19T2RD_csv" = stringr::str_subset(names(PIB_DATA_FOLDERS), "(^2(?!(0.*)|(1.*)|(2T1PE)).*)") # aucun de ce type avant 2020
  )

  # preparation of the matrix of vintage data
  nonrevised_pib <- construct_nonrevised_national_account_data_from_scratch(data_source = "national_accounting",
                                                                            files_list = PIB_DATA_FOLDERS,
                                                                            file_type2files_list = PIB_FILES_TYPES,
                                                                            file_name = PIB_FILE_NAME,
                                                                            dimensions_list = PIB_DIMENSIONS,
                                                                            number_previous_values = 47) #24

  save(nonrevised_pib, file = paste0("./data/", "nonrevised_pib_", max(unique(nonrevised_pib[["date"]])), "PE.RData"))
} else {
  load("./data/nonrevised_pib_2023-01-01PE.RData")
}

# 3. load survey data --------------------------------------------------------------------------------------------------
if (UPDATE_SURVEY_DATA) {
  survey_data <- load_data_for_nowcasting(PATH_TO_DATA_FOR_NOWCASTING, sheet = "indices_synthetiques")

  save(survey_data, file = paste0("./code/doc_travail_interpretation_enquetes/data/survey_data_doc_travail_", lubridate::today(), ".RData"))

} else {
  load("./code/doc_travail_interpretation_enquetes/data/survey_data_doc_travail_20230718.RData")
}


# 4. create the dataframes for the prevision ---------------------------------------------------------------------------

## add additional variables (features): the monthly variation and monthly difference of survey data
## and transform survey data to deal with the mix frequency problem by using the "blocking method"
survey_data_growth_rate <- survey_data %>%
  get_variation_for(variation_type = "growth_rate", add_option = TRUE, prefix = "vm1") %>%  # create the monthly variation of survey data (variation mensuelle in French)
  month_to_quarter(transformation_type = "split")                                           # apply the blocking method


survey_data_difference <- survey_data %>%
  get_variation_for(variation_type = "difference", keep_prefix = TRUE, prefix = "dm1") %>%  # create the monthly difference of survey data (différence mensuelle in French)
  month_to_quarter(transformation_type = "split")                                           # apply the blocking method

full_survey_data <- survey_data_growth_rate %>%
  dplyr::bind_rows(survey_data_difference)

# create the lead variables, i.e. the variables at month 1 from the T+1 quarter
full_survey_data <- full_survey_data %>%
  get_variation_for(variation_type = "lead", nbr_lead = 1, add_option = TRUE, dimensions_to_transform = stringr::str_subset(unique(full_survey_data$dimension), ".*_m1")) %>%
  convert_to_wide_format() %>%
  dplyr::select(date, contains("industrie"), contains("services"), contains("global"), contains("composite"), -contains("production_passee"))

save(full_survey_data, file = paste0("./code/doc_travail_interpretation_enquetes/data/data_prev_doc_travail_", lubridate::today(), ".RData"))


## transform GDP data to get its quarterly variation and its annual variation
corrected_nonrevised_pib <- nonrevised_pib %>%
  dplyr::select(date, dimension, t, t_1, t_4) %>%
  dplyr::filter(dimension == "PIB") %>%
  dplyr::mutate(dimension = "nonrevised_pib") %>%
  dplyr::mutate(var1_PIB = t / t_1 - 1,
                var4_PIB = t / t_4 - 1) %>%
  dplyr::select(date, var1_PIB, var4_PIB)

## merge GDP and survey data
full_data <- corrected_nonrevised_pib %>%
  dplyr::full_join(full_survey_data, by = "date")

# prepare data for regression
prevision_data <- full_data %>%
  dplyr::arrange(date) %>%
  dplyr::filter(date >= lubridate::ymd("2007-10-01") & date <= lubridate::ymd("2019-10-01"))

# 4. realise forcasts "in real-time" with vintage data -----------------------------------------------------------------
real_time_out_of_sample_current_quarter_nowcasting <- create_summary_for_several_simple_out_of_sample_nowcasting(y_var = "var1_PIB",
                                                                                                                 list_x_var = colnames(prevision_data)[!(colnames(prevision_data) %in% c("date", "var1_PIB", "var4_PIB"))],
                                                                                                                 reg_data = prevision_data,
                                                                                                                 window_size = 32) # Note: 32 quarters = 8 years

nowcasting_summaries <- real_time_out_of_sample_current_quarter_nowcasting %>%
  dplyr::group_by(dimension) %>%
  dplyr::summarise(adjusted_r_squared = mean(adjusted_r_squared),
                   rmse = Metrics::rmse(actual = expected_values, predicted = predicted_values),
                   mae = Metrics::mae(actual = expected_values, predicted = predicted_values), .groups = "drop") %>%
  dplyr::mutate(horizon = stringr::str_extract(dimension, "(lead)|(.{2}$)")) %>%
  dplyr::filter(rmse <= 0.0018) %>%
  dplyr::arrange(horizon, rmse) %>%
  dplyr::select(horizon, everything())

best_models <- unique(nowcasting_summaries$dimension)

expected_data <- real_time_out_of_sample_current_quarter_nowcasting %>%
  dplyr::filter(dimension == "insee_global_m2") %>%  # we just need to take any one of the dimensions
  dplyr::mutate(dimension = "expected") %>%
  dplyr::select(date, dimension, expected_values) %>%
  dplyr::rename(value = expected_values)

graph_data <- real_time_out_of_sample_current_quarter_nowcasting %>%
  dplyr::filter(dimension %in% best_models) %>%
  dplyr::select(date, dimension, predicted_values) %>%
  dplyr::rename(value = predicted_values) %>%
  dplyr::bind_rows(expected_data) %>%
  dplyr::mutate(horizon = stringr::str_extract(dimension, "(lead)|(.{2}$)")) %>%
  dplyr::mutate(horizon = ifelse(horizon == "ed", "expected", horizon))


# graphes
graph_donnees_conj(title = "Nowcasting hors échantillon du trimestre T",
                   subtitle = "Données d'enquêtes au mois 1 du trimestre T",
                   data = graph_data %>% filter(horizon %in% c("m1", "expected")),
                   main_dimension = "expected",
                   graph_source = "Insee, Banque de France, S&P, calculs DG Trésor",
                   graph_saving = FALSE) +
  scale_y_continuous(breaks = c(-0.001, 0, 0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007),
                     labels = scales::percent_format(accuracy = 0.1))

graph_donnees_conj(title = "Nowcasting hors échantillon du trimestre T",
                   subtitle = "Données d'enquêtes au mois 2 du trimestre T",
                   data = graph_data %>% filter(horizon %in% c("m2", "expected")),
                   main_dimension = "expected",
                   graph_source = "Insee, Banque de France, S&P, calculs DG Trésor",
                   graph_saving = FALSE) +
  scale_y_continuous(breaks = c(-0.001, 0, 0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007),
                     labels = scales::percent_format(accuracy = 0.1))

graph_donnees_conj(title = "Nowcasting hors échantillon du trimestre T",
                   subtitle = "Données d'enquêtes au mois 3 du trimestre T",
                   data = graph_data %>% filter(horizon %in% c("m3", "expected")),
                   main_dimension = "expected",
                   graph_source = "Insee, Banque de France, S&P, calculs DG Trésor",
                   graph_saving = FALSE) +
  scale_y_continuous(breaks = c(-0.001, 0, 0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007),
                     labels = scales::percent_format(accuracy = 0.1))

graph_donnees_conj(title = "Nowcasting hors échantillon du trimestre T",
                   subtitle = "Données d'enquêtes au mois 1 du trimestre T+1",
                   data = graph_data %>% filter(horizon %in% c("lead", "expected")),
                   main_dimension = "expected",
                   graph_source = "Insee, Banque de France, S&P, calculs DG Trésor",
                   graph_saving = FALSE) +
  scale_y_continuous(breaks = c(-0.001, 0, 0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007),
                     labels = scales::percent_format(accuracy = 0.1))


# 5. realise forecasts made "in pseudo real-time" with revised data ----------------------------------------------------

revised_pib_for_prev <- revised_pib %>%
  dplyr::mutate(var1_PIB_revised = value / lag(value) - 1) %>%
  dplyr::select(date, var1_PIB_revised)

prevision_data_rev <- prevision_data %>%
  dplyr::full_join(revised_pib_for_prev, by = "date") %>%
  dplyr::filter(date >= lubridate::ymd("2007-10-01") & date <= lubridate::ymd("2019-10-01"))

pseudo_real_time_out_of_sample_current_quarter_nowcasting <- create_summary_for_several_simple_out_of_sample_nowcasting(y_var = "var1_PIB_revised",
                                                                                                                        list_x_var = colnames(prevision_data_rev)[!(colnames(prevision_data_rev) %in% c("date", "var1_PIB", "var4_PIB", "var1_PIB_revised"))],
                                                                                                                        reg_data = prevision_data_rev,
                                                                                                                        window_size = 32) # Note: 32 quarters = 8 years

# 5.1. estimation of the model in pseudo real time - comparison with revised data
nowcasting_summaries_pseudo_real_time <- pseudo_real_time_out_of_sample_current_quarter_nowcasting %>%
  dplyr::group_by(dimension) %>%
  dplyr::summarise(adjusted_r_squared = mean(adjusted_r_squared),
                   rmse = Metrics::rmse(actual = expected_values, predicted = predicted_values),
                   mae = Metrics::mae(actual = expected_values, predicted = predicted_values), .groups = "drop") %>%
  dplyr::mutate(horizon = stringr::str_extract(dimension, "(lead)|(.{2}$)")) %>%
  dplyr::filter(rmse <= 0.0033) %>%
  dplyr::arrange(horizon, rmse) %>%
  dplyr::select(horizon, everything())

# 5.2. estimation of the model in pseudo real time - comparison with non-revised data
PE_expected_values <- real_time_out_of_sample_current_quarter_nowcasting %>%
  dplyr::filter(dimension == "insee_global_m1") %>%  # just take any dimension
  dplyr::select(date, expected_values) %>%
  dplyr::rename(expected_values_PE = expected_values)

nowcasting_summaries_pseudo_real_time_PE <- pseudo_real_time_out_of_sample_current_quarter_nowcasting %>%
  dplyr::full_join(PE_expected_values, by = "date")
nowcasting_summaries_pseudo_real_time_PE <- nowcasting_summaries_pseudo_real_time_PE %>%
  dplyr::group_by(dimension) %>%
  dplyr::summarise(adjusted_r_squared = mean(adjusted_r_squared),
                   rmse = Metrics::rmse(actual = expected_values_PE, predicted = predicted_values),     # ATTENTION! we use expected_values_PE
                   mae = Metrics::mae(actual = expected_values_PE, predicted = predicted_values), .groups = "drop") %>%   # ATTENTION! we use expected_values_PE
  dplyr::mutate(horizon = stringr::str_extract(dimension, "(lead)|(.{2}$)")) %>%
  dplyr::filter(rmse <= 0.0018) %>%
  dplyr::arrange(horizon, rmse) %>%
  dplyr::select(horizon, everything())


# 6. Check stability of model selection --------------------------------------------------------------------------------
## Note: due to the small sample size, model selection at each horizon is here based on in-sample rmse

table_model_stability <- real_time_out_of_sample_current_quarter_nowcasting %>%
  dplyr::mutate(horizon = stringr::str_extract(dimension, "(lead)|(.{2}$)"))

table_model_stability_h1 <- table_model_stability %>%
  dplyr::filter(horizon == "m1") %>%
  dplyr::group_by(date) %>%
  dplyr::filter(reg_rmse == min(reg_rmse)) %>%
  dplyr::select(date, dimension, reg_rmse)

table_model_stability_h2 <- table_model_stability %>%
  dplyr::filter(horizon == "m2") %>%
  dplyr::group_by(date) %>%
  dplyr::filter(reg_rmse == min(reg_rmse)) %>%
  dplyr::select(date, dimension, reg_rmse)

table_model_stability_h3 <- table_model_stability %>%
  dplyr::filter(horizon == "m3") %>%
  dplyr::group_by(date) %>%
  dplyr::filter(reg_rmse == min(reg_rmse)) %>%
  dplyr::select(date, dimension, reg_rmse)

table_model_stability_hlead <- table_model_stability %>%
  dplyr::filter(horizon == "lead") %>%
  dplyr::group_by(date) %>%
  dplyr::filter(reg_rmse == min(reg_rmse)) %>%
  dplyr::select(date, dimension, reg_rmse)

# 6. Check stability of model selection for Insee and PMI data ---------------------------------------------------------
## Note: BdF surveys are realised later than Insee and PMI ones, so they include more information, which can explain their good performance
## Note: due to the small sample size, model selection at each horizon is here based on in-sample rmse

table_model_stability_v2 <- real_time_out_of_sample_current_quarter_nowcasting %>%
  dplyr::mutate(horizon = stringr::str_extract(dimension, "(lead)|(.{2}$)")) %>%
  dplyr::filter(!(dimension %in% stringr::str_subset(unique(dimension), "bdf")))  # remove BdF survey variables

table_model_stability_v2_h1 <- table_model_stability_v2 %>%
  dplyr::filter(horizon == "m1") %>%
  dplyr::group_by(date) %>%
  dplyr::filter(reg_rmse == min(reg_rmse)) %>%
  dplyr::select(date, dimension, reg_rmse)

table_model_stability_v2_h2 <- table_model_stability_v2 %>%
  dplyr::filter(horizon == "m2") %>%
  dplyr::group_by(date) %>%
  dplyr::filter(reg_rmse == min(reg_rmse)) %>%
  dplyr::select(date, dimension, reg_rmse)

table_model_stability_v2_h3 <- table_model_stability_v2 %>%
  dplyr::filter(horizon == "m3") %>%
  dplyr::group_by(date) %>%
  dplyr::filter(reg_rmse == min(reg_rmse)) %>%
  dplyr::select(date, dimension, reg_rmse)

table_model_stability_v2_hlead <- table_model_stability_v2 %>%
  dplyr::filter(horizon == "lead") %>%
  dplyr::group_by(date) %>%
  dplyr::filter(reg_rmse == min(reg_rmse)) %>%
  dplyr::select(date, dimension, reg_rmse)

# 7. Reproduce reality of iterative pseudo-real time exercises  --------------------------------------------------------
QUARTERS_TO_LOAD <- c("15T3PE", "15T4PE", "16T1PE", "16T2PE", "16T3PE", "16T4PE", "17T1PE", "17T2PE", "17T3PE", "17T4PE", "18T1PE", "18T2PE", "18T3PE", "18T4PE", "19T1PE", "19T2PE", "19T3PE")
QUARTERS_TO_PREDICT <- c("2015-10-01", "2016-01-01", "2016-04-01", "2016-07-01", "2016-10-01", "2017-01-01", "2017-04-01", "2017-07-01", "2017-10-01", "2018-01-01", "2018-04-01", "2018-07-01", "2018-10-01", "2019-01-01", "2019-04-01", "2019-07-01", "2019-10-01")

PIB_DATA_FOLDERS_FOR_REALITY_EXERCISE <- get_national_accounting_data_files(NATIONAL_ACCOUNTING_DATA_FOLDER,
                                                                            estimation_type = "PE",                                         # we keep only the folders containing the 1st estimation of the quarterly accounts (PE)
                                                                            subset_regex = ".*/(?!((0.)|(11)|(12)|(13)|(14)|(2.)))[:digit:]{2}T[:digit:]PE"     # we take data only from 2015T1; we keep data only up to 2019T4
)
PIB_DATA_FOLDERS_FOR_REALITY_EXERCISE <- PIB_DATA_FOLDERS_FOR_REALITY_EXERCISE[3:(length(PIB_DATA_FOLDERS_FOR_REALITY_EXERCISE) - 1)] # we start from 2015T3 and up to 2019T3


PIB_FILES_TYPES_FOR_REALITY_EXERCISE <- list("pre_19T2RD_csv" = stringr::str_subset(names(PIB_DATA_FOLDERS_FOR_REALITY_EXERCISE), "(^1(?!((0|1|2|3|4).*)|(5T1PE)|(5T2PE)|(9T3PE)|(9T3RD)|(9T4PE)|(9T4RD)).*)"),
                                             "post_19T2RD_xls" = stringr::str_subset(names(PIB_DATA_FOLDERS_FOR_REALITY_EXERCISE), "^((19T2RD)|(19T3PE))"))


reality_nowcasting_exercise <- create_reality_summary(data_source = "national_accounting",
                                                      files_list = PIB_DATA_FOLDERS_FOR_REALITY_EXERCISE,
                                                      file_type2files_list = PIB_FILES_TYPES_FOR_REALITY_EXERCISE,
                                                      file_name = PIB_FILE_NAME,
                                                      dimensions_list = PIB_DIMENSIONS,
                                                      prevision_data = prevision_data,
                                                      quarters_to_predict = QUARTERS_TO_PREDICT)


nowcasting_summaries_reality_exercise <- reality_nowcasting_exercise %>%
  dplyr::full_join(PE_expected_values, by = "date")
nowcasting_summaries_reality_exercise <- nowcasting_summaries_reality_exercise %>%
  dplyr::group_by(dimension) %>%
  dplyr::summarise(adjusted_r_squared = mean(adjusted_r_squared),
                   # reg_rmse = mean(reg_rmse),
                   # reg_mae = mean(reg_mae),
                   rmse = Metrics::rmse(actual = expected_values_PE, predicted = predicted_values),     # ATTENTION! we use expected_values_PE
                   mae = Metrics::mae(actual = expected_values_PE, predicted = predicted_values), .groups = "drop") %>%   # ATTENTION! we use expected_values_PE
  dplyr::mutate(horizon = stringr::str_extract(dimension, "(lead)|(.{2}$)")) %>%
  dplyr::filter(rmse <= 0.0018) %>%
  dplyr::arrange(horizon, rmse) %>%
  dplyr::select(horizon, everything())


# 8. data for future leakage box  --------------------------------------------------------------------------------------

# load revised data
REDOWNLOAD_REVISED_IPI_DATA <- FALSE
REDOWNLOAD_REVISED_PRODUCTION_DATA <- FALSE

if (REDOWNLOAD_REVISED_IPI_DATA) {
  revised_ipi <- generic_loader(file_path = "S:/SPMAE/PREV/Prev3/_Fichiers_Prev3/Prod_manuf/02-IPI/mail_reaction_ipi/02-Envoi_Insee/2019/series_longues_ipi_201912.xls")
  save(revised_ipi, file = paste0("./data/", "revised_ipi_", max(unique(revised_ipi[["date"]])), ".RData"))
} else {
  load("./data/revised_ipi_2019-12-01.RData")
  revised_ipi_2019 <- revised_ipi
}

if (REDOWNLOAD_REVISED_PRODUCTION_DATA) {
  revised_production <- xls_national_accounting_loader(file_path = file.path(NATIONAL_ACCOUNTING_DATA_FOLDER_BASE2014, "19T4PE"),
                                                       folder_name = "14T4PE",
                                                       file_name = "cprvolch",
                                                       dimensions_list = PRODUCTION_DIMENSIONS,
                                                       dimensions_list_name = "revised")
  save(revised_production, file = paste0("./data/", "revised_production_", max(unique(revised_production[["date"]])), "PE.RData")) # ATTENTION: choose PE or RD
} else {
  load("./data/revised_production_2019-10-01PE.RData")
  revised_production_2019 <- revised_production
}

# load mostrecent revised data
load("./data/revised_production_2023-01-01PE.RData")
revised_production_2023 <- revised_production
load("./data/revised_ipi_2023-03-01.RData")
revised_ipi_2023 <- revised_ipi

# load nonrevised data
load("./data/nonrevised_production_2023-01-01PE.RData")
load("./data/nonrevised_ipi_2023-03-01.RData")

nonrevised_production_for_prev <- nonrevised_production %>%
  dplyr::select(date, dimension, t, t_1, t_4) %>%
  dplyr::filter(dimension == "P1E_DIM") %>% # choose the manufacturing sector (CZ or DIM)
  dplyr::arrange(date) %>%
  dplyr::mutate(var1_production = t / t_1 - 1,
                var4_production = t / t_4 - 1) %>%
  dplyr::select(date, var1_production, var4_production)

revised_production_for_prev_2019 <- revised_production_2019 %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(var1_production_revised_2019 = value / lag(value) - 1) %>%
  dplyr::select(date, var1_production_revised_2019)

nonrevised_ipi_for_prev <- nonrevised_ipi %>%
  dplyr::select(date, dimension, t, t_1, t_2, t_3, t_4, t_5) %>%
  dplyr::filter(dimension == "CZ") %>% # choose the manufacturing sector (CZ)
  dplyr::arrange(date) %>%
  get_quarterly_variation_for_nonrevised_monthly_data(month_position_of_quarters = 3,
                                                      quarterly_variation_column_name = "nonrevised_ipi") %>%
  dplyr::select(-dimension)

revised_ipi_for_prev_2019 <- revised_ipi_2019 %>%
  dplyr::filter(dimension == "CZ") %>%
  month_to_quarter(transformation_type = "sum") %>%
  dplyr::mutate(var1_revised_ipi_2019 = value / lag(value) - 1) %>%
  dplyr::select(date, var1_revised_ipi_2019)

revised_production_for_prev_2023 <- revised_production_2023 %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(var1_production_revised_2023 = value / lag(value) - 1) %>%
  dplyr::select(date, var1_production_revised_2023)

revised_ipi_for_prev_2023 <- revised_ipi_2023 %>%
  dplyr::filter(dimension == "CZ") %>%
  month_to_quarter(transformation_type = "sum") %>%
  dplyr::mutate(var1_revised_ipi_2023 = value / lag(value) - 1) %>%
  dplyr::select(date, var1_revised_ipi_2023)

production_ipi_data <- nonrevised_production_for_prev %>%
  dplyr::full_join(revised_production_for_prev_2019, by = "date") %>%
  dplyr::full_join(nonrevised_ipi_for_prev, by = "date") %>%
  dplyr::full_join(revised_ipi_for_prev_2019, by = "date") %>%
  dplyr::full_join(revised_production_for_prev_2023, by = "date") %>%
  dplyr::full_join(revised_ipi_for_prev_2023, by = "date") %>%
  dplyr::filter(date >= lubridate::ymd("2011-01-01") & date <= lubridate::ymd("2019-10-01"))

# export data ----------------------------------------------------------------------------------------------------------

data_for_excel_figure_m1 <- graph_data %>%
  dplyr::filter(horizon %in% c("expected", "m1")) %>%
  dplyr::select(-horizon) %>%
  tidyr::pivot_wider(names_from = dimension,
                     values_from = value)

data_for_excel_figure_m2 <- graph_data %>%
  dplyr::filter(horizon %in% c("expected", "m2")) %>%
  dplyr::select(-horizon) %>%
  tidyr::pivot_wider(names_from = dimension,
                     values_from = value)

data_for_excel_figure_m3 <- graph_data %>%
  dplyr::filter(horizon %in% c("expected", "m3")) %>%
  dplyr::select(-horizon) %>%
  tidyr::pivot_wider(names_from = dimension,
                     values_from = value)

data_for_excel_figure_lead <- graph_data %>%
  dplyr::filter(horizon %in% c("expected", "lead")) %>%
  dplyr::select(-horizon) %>%
  tidyr::pivot_wider(names_from = dimension,
                     values_from = value)


writexl::write_xlsx(list("data_figure_prev_m1" = data_for_excel_figure_m1,
                         "data_figure_prev_m2" = data_for_excel_figure_m2,
                         "data_figure_prev_m3" = data_for_excel_figure_m3,
                         "data_figure_prev_lead" = data_for_excel_figure_lead,
                         "data_tableau_1" = nowcasting_summaries,
                         "data_tableau_2" = nowcasting_summaries_pseudo_real_time,
                         "data_tableau_3" = nowcasting_summaries_pseudo_real_time_PE,
                         "data_tableau_4" = nowcasting_summaries_reality_exercise,
                         "data_tableau_stabilite_1" = table_model_stability_h1,
                         "data_tableau_stabilite_2" = table_model_stability_h2,
                         "data_tableau_stabilite_3" = table_model_stability_h3,
                         "data_tableau_stabilite_4" = table_model_stability_hlead,
                         "data_tableau_stabilite_ssbdf_1" = table_model_stability_v2_h1,
                         "data_tableau_stabilite_ssbdf_2" = table_model_stability_v2_h2,
                         "data_tableau_stabilite_ssbdf_3" = table_model_stability_v2_h3,
                         "data_tableau_stabilite_ssbdf_4" = table_model_stability_v2_hlead,
                         "data_future_leakage_box" = production_ipi_data),
                    path = "./code/doc_travail_interpretation_enquetes/output/prevision_output_for_graphs.xlsx",
                    col_names = TRUE, format_headers = FALSE)