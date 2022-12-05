# Title     : TODO
# Objective : TODO
# Created by: lphung
# Created on: 12/09/2022

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
source("./code/nonrevised_production/loaders_utils.R", encoding = "utf-8", chdir = TRUE)
# chdir = TRUE needed because we call this Rscript from the main.R and from a RMarkdown, which define working directory differently

source("./code/scripts_from_prevision_production_manuf/loading_data.R", encoding = "utf-8")
source("./code/scripts_from_prevision_production_manuf/data_transformation.R", encoding = "utf-8")


# constants to define --------------------------------------------------------------------------------------------------
ONLY_UPDATE_NONREVISED_IPI_DATA <- TRUE
ONLY_UPDATE_NONREVISED_PRODUCTION_DATA <- TRUE
# Note: production data in this project always refers to the production or value added in the manufacturing sector in the quarterly national accounts

# 1. load nonrevised ipi -------------------------------------------------------------------------------------------------
if (ONLY_UPDATE_NONREVISED_IPI_DATA) {
  # nonrevised_ipi <- update_nonrevised_ipi()
  load("./data/nonrevised_ipi_2022-09-01.RData")
} else {
  # preparation of the list
  IPI_DATA_FILES <- get_ipi_data_files(IPI_DATA_FOLDER)
  # TODO: check where to put that -> in a README.md // Note: convention de nommage des fichiers d'IPI: la date du nom de fichier doit contenir la dernière date pour laquelle nous avons l'IPI (et non la date de publication)
  IPI_FILES_TYPES <- list("sectors_in_line_one_label_loader" = c("200901"),
                          "sectors_in_line_two_labels_loader" = stringr::str_subset(names(IPI_DATA_FILES), "(^2009(?!(01)).*)|(^2010(?!(11)|(12)).*)"))

  # preparation of the matrix
  nonrevised_ipi <- construct_nonrevised_ipi_from_scratch(files_list = IPI_DATA_FILES,
                                                          file_type2files_list = IPI_FILES_TYPES,
                                                          number_previous_values = 24,
                                                          data_correction = "CJO-CVS") # TODO: data_correction as argument within the function
}

# save(nonrevised_ipi, file = paste0("./data/", "nonrevised_ipi_", max(unique(nonrevised_ipi[["date"]])), ".RData"))

# 2. load nonrevised production ----------------------------------------------------------------------------------------
if (ONLY_UPDATE_NONREVISED_PRODUCTION_DATA) {
  # nonrevised_production <- update_nonrevised_production()
  load("./data/nonrevised_production_2022-07-01PE.RData")
} else {
  # preparation of the list
  PRODUCTION_DATA_FOLDERS <- get_production_data_files(PRODUCTION_DATA_FOLDER, estimation_type = "PE")
  # we keep only the folders containing the 1st estimation of the quarterly accounts (PE)
  ## Note : we do not want to use the .RData file (available from 19T4) because they are ts() series not in dataframe format and we would need to reconstruct everything
  PRODUCTION_FILES_TYPES <- list("pre_19T2RD_csv" = stringr::str_subset(names(PRODUCTION_DATA_FOLDERS), "(^1(?!(9T2RD)|(9T3PE)|(9T3RD)|(9T4PE)|(9T4RD)).*)"), # we want
                                 "post_19T2RD_xls" = stringr::str_subset(names(PRODUCTION_DATA_FOLDERS), "^((19T2RD)|(19T3PE)|(19T3RD)|(19T4PE)|(19T4RD))|(^20.*)|(^21.*)|^((22T1PE)|(22T1RD))"),
                                 "post_19T2RD_csv" = stringr::str_subset(names(PRODUCTION_DATA_FOLDERS), "(^2(?!(0.*)|(1.*)|(2T1PE)|(2T1RD)).*)"))

  # preparation of the matrix
  nonrevised_production <- construct_nonrevised_production_from_scratch(files_list = PRODUCTION_DATA_FOLDERS,
                                                                        file_type2files_list = PRODUCTION_FILES_TYPES,
                                                                        number_previous_values = 24)
}

# save(nonrevised_production, file = paste0("./data/", "nonrevised_production_", max(unique(nonrevised_production[["date"]])), "PE.RData"))

# 3. create the dataframes for the prevision ---------------------------------------------------------------------------
# Note: we forcast only the production in the manufacturing industry (i.e. CZ sector)

# 3.1. correct the non-revised data for the effects of changing bases
# TODO: create a function for that (across columns) in data_preparator.R
# load revised data to calculate the effects of changing bases
load("./data/revised_ipi_2022-08-01.RData")
load("./data/revised_production_2022-04-01PE.RData")

# get the needed sector and, compare revised and nonrevised data
compare_ipi <- merge_nonrevised_and_revised_data(revised_data = revised_ipi, nonrevised_data = nonrevised_ipi,
                                                 dimension_to_keep = "CZ", column_to_use_for_revised_data = value, column_to_use_for_nonrevised_data = t, data_label = "ipi")

compare_production <- merge_nonrevised_and_revised_data(revised_data = revised_production, nonrevised_data = nonrevised_production,
                                                        dimension_to_keep = "P1E_DIM", column_to_use_for_revised_data = value, column_to_use_for_nonrevised_data = t, data_label = "production")

# get nonrevised data corrected for the effects of changing bases
nonrevised_ipi_cz <- compare_ipi %>%
  dplyr::filter(dimension == "nonrevised_ipi") %>%
  dplyr::mutate(value = case_when(
    lubridate::year(date) >= 2009 & lubridate::year(date) < 2013 ~ value + get_changing_base_shift(compare_ipi, "2009-01-01", "2012-12-01"),
    TRUE ~ value
  ))

nonrevised_production_cz <- compare_production %>%
  dplyr::filter(dimension == "nonrevised_production") %>%
  dplyr::mutate(value = case_when(
    lubridate::year(date) >= 2011 & lubridate::year(date) <= 2013 ~ value + get_changing_base_shift(compare_production, "2011-01-01", "2013-10-01"),
    lubridate::year(date) >= 2014 & date <= lubridate::ymd("2018-01-01") ~ value + get_changing_base_shift(compare_production, "2014-01-01", "2018-01-01"),
    TRUE ~ value
  ))

# 3.2. create the meta-dataframe for the previsions
# TODO: check functions in the "regular prevision" code for the transformations
ipi_for_prev <- nonrevised_ipi_cz %>%
  dplyr::mutate(dimension = "ipi") %>%
  get_variation_for(variation_type = "growth_rate", add_option = TRUE) %>%
  month_to_quarter(transformation_type = "split") %>% # TODO : keep the proper date and add column with the corresponding quarter (column "trim") + column "month_in_quarter"
  convert_to_wide_format() %>%
  dplyr::mutate(acquis_ipi_m1 = ipi_m1 * 3,  # TODO: create a function for that in long format
                acquis_ipi_m2 = ipi_m1 + ipi_m2 * 2,
                avg_acquis_ipi_m2 = ((ipi_m1 + ipi_m2) / 2) * 3,
                acquis_ipi_m3 = ipi_m1 + ipi_m2 + ipi_m3) %>%
  dplyr::mutate(var1_acquis_m1 = acquis_ipi_m1 / dplyr::lag(acquis_ipi_m1) - 1,
                var1_acquis_m2 = acquis_ipi_m2 / dplyr::lag(acquis_ipi_m2) - 1,
                var1_avg_acquis_m2 = avg_acquis_ipi_m2 / dplyr::lag(avg_acquis_ipi_m2) - 1,
                var1_acquis_m3 = acquis_ipi_m3 / dplyr::lag(acquis_ipi_m3) - 1)

production_for_prev <- nonrevised_production_cz %>%
  dplyr::mutate(dimension = "production") %>%
  get_variation_for(variation_type = "growth_rate", add_option = TRUE, dimensions_to_transform = "production") %>%
  convert_to_wide_format() %>%
  dplyr::mutate(lag1_production = dplyr::lag(production))

data_for_prev <- ipi_for_prev %>%
  dplyr::full_join(production_for_prev, by = "date") %>%
  dplyr::filter(date >= lubridate::ymd("2011-01-01")) %>%
  dplyr::arrange(date)

# cleaning data_for_prev
data_for_prev <- data_for_prev %>%
  dplyr::arrange(date)

# 3.3. create the dataframes to test different models
# NOTE: we test the models only for IPI with the second month of the quarter
# TODO: to continue here => need to create 3 dataframes for the 3 models (when we have m_1, m_2 and m_3)

# data_for_model_level_sum <- data_for_prev %>%
#   dplyr::mutate(lag_acquis_ipi_m2 = lag(acquis_ipi_m2)) %>%
#   dplyr::select(production, acquis_ipi_m2,lag_acquis_ipi_m2, lag1_production)
#
# data_for_model_level_split <- data_for_prev %>%
#   dplyr::mutate(lag_ipi_m1 = lag(ipi_m1),
#                 lag_ipi_m2 = lag(ipi_m2)) %>%
#   dplyr::select(production, ipi_m1, ipi_m2, lag_ipi_m1, lag_ipi_m2, lag1_production)
#

data_for_model_var_sum <- data_for_prev %>%
  dplyr::filter(date <= ymd("2022-07-01")) %>%
  dplyr::arrange(date) %>%
  dplyr::select(date, var1_production, var1_acquis_m2) %>%
  dplyr::mutate(lag1_var1_production = lag(var1_production))
# %>% # TODO: maybe add the lag of the var1_acquis_m3 to get the variation of the full quarter before
#   stats::ts(start = c(2011, 2), frequency = 4)

# data_for_model_var_split <- data_for_prev %>%
#   dplyr::select(var1_production, var1_ipi_m1, var1_ipi_m2) %>%
#   stats::ts(start = c(2011, 2), frequency = 4)


# 3.4 add surveys data
pmi_manuf <- load_pmi_data_from_excel(path_to_data = PATH_TO_PMI_DATA,
                                      column_list = PMI_EXCEL_DIMENSIONS_TO_DATAFRAME_DIMENSIONS)
survey_data <- pmi_manuf %>%
  filter(dimension %in% c("pmi_climat", "pmi_production_passee"
                          # , "pmi_new_commandes", "pmi_emploi"
                          # , "pmi_delais_livraison", "pmi_stocks_achats"
  ))

transfo_survey_data <- survey_data %>%
  # get_variation_for(variation_type = "difference", add_option = TRUE, nbr_lag = 1) %>%
  # get_variation_for(variation_type = "difference", add_option = TRUE, nbr_lag = 3,
  #                   dimensions_to_transform = c("pmi_climat", "pmi_production_passee")) %>%
  month_to_quarter(transformation_type = "split") %>%
  pivot_wider(names_from = dimension,
              values_from = value) %>%
  mutate(sum_climat = pmi_climat_m1 + pmi_climat_m2 + pmi_climat_m3,
         sum_prod_passee = pmi_production_passee_m1 +
           pmi_production_passee_m2 +
           pmi_production_passee_m3) %>%
  mutate(sum_climat_vt = sum_climat - lag(sum_climat),
         sum_prod_passee_vt = sum_prod_passee - lag(sum_prod_passee),
         sum_climat_ga = sum_climat - lag(sum_climat, n = 4),
         sum_prod_passee_ga = sum_prod_passee - lag(sum_prod_passee, n = 4),
         sum_climat_growthrate = sum_climat / lag(sum_climat) - 1,
         sum_climat_growthrate4 = sum_climat / lag(sum_climat, n = 4) - 1) %>%
  filter(date >= ymd("2011-01-01") & date <= ymd("2022-07-01"))

data_merged <- data_for_model_var_sum %>%
  full_join(transfo_survey_data, by = "date")

#TODO : keep data for october 2022 (BCSE prod-passee of october => prod of septembre) and sum there

# regressors <- convert_to_wide_format(transfo_ipi) %>%
#   dplyr::left_join(convert_to_wide_format(transfo_survey_data), by = "date")
#
# forecasting_data <- convert_to_wide_format(transfo_cnat_prod_manuf) %>%
#   dplyr::full_join(regressors, by = "date") %>%
#   mutate(AR1 = lag(prod_manuf)) %>%
#   filter(date > lubridate::ymd("2000-01-01")) %>%
#   add_quarterly_dummy(dummy_name = "T1_2020", vector_of_dates = ymd(c("2020-01-01"))) %>%
#   add_quarterly_dummy(dummy_name = "T2_2020", vector_of_dates = ymd(c("2020-04-01"))) %>%
#   add_quarterly_dummy(dummy_name = "T3_2020", vector_of_dates = ymd(c("2020-07-01")))


# 4. generate the models for second month of the quarter ---------------------------------------------------------------


data_final <- data_merged %>%
  filter(date <= ymd("2022-04-01"))

start_from <- 18
predicted_values <- c()
expected_values <- c()
for (i in start_from:(nrow(data_final) - 1)) {
  train_set <- data_final[3:i,]
  # model <- lm(var1_production ~ var1_acquis_m2 , data = train_set) -> rmse = 0.009136706 period [2011-07-01;2019-10-01] // rmse = 0.02071128 period [2011-07-01;2022-04-01]
  # model <- lm(var1_production ~ var1_acquis_m2 + sum_prod_passee_vt, data = train_set) -> rmse = 0.009019557 period [2011-07-01;2019-10-01]
  # model <- lm(var1_production ~ var1_acquis_m2 + sum_prod_passee_ga, data = train_set) -> rmse = 0.009049178 period [2011-07-01;2019-10-01]
  # model <- lm(var1_production ~ var1_acquis_m2 + sum_climat_vt, data = train_set) -> rmse = 0.008805136 period [2011-07-01;2019-10-01] // rmse = 0.01980073 period [2011-07-01;2022-04-01]
  # model <- lm(var1_production ~ var1_acquis_m2 + sum_climat_ga, data = train_set) -> rmse = 0.008830841 period [2011-07-01;2019-10-01]
  # model <- lm(var1_production ~ var1_acquis_m2 + sum_climat_growthrate, data = train_set) -> rmse = 0.008777745 period [2011-07-01;2019-10-01] // rmse = 0.01927782 period [2011-07-01;2022-04-01]
  # model <- lm(var1_production ~ var1_acquis_m2 + sum_climat_growthrate4, data = train_set) -> rmse = 0.008764019 period [2011-07-01;2019-10-01] // rmse = 0.02173239 period [2011-07-01;2022-04-01]
  model <- lm(var1_production ~ var1_acquis_m2 + sum_climat_growthrate, data = train_set)
  predicted_values <- c(predicted_values, predict(model, data_final[i + 1,])[[1]])
  expected_values <- c(expected_values, data_for_prev$var1_production[i + 1])
}

rmse_m1 <- Metrics::rmse(expected_values, predicted_values)
mae_m1 <- Metrics::mae(expected_values, predicted_values)


test <- data.frame(index = seq(1:length(predicted_values)), predicted_values = predicted_values, expected_values = expected_values)

test2 <- test %>%
  pivot_longer(cols = c("predicted_values", "expected_values"),
               names_to = "dimension",
               values_to = "value")

ggplot(test2, aes(x = index, y = value, color = dimension)) +
  geom_line()


# 5. calculate the prevision for the 2022Q3 ---------------------------------------------------------------

data_final <- data_merged %>%
  filter(date <= ymd("2022-07-01"))

start_from <- 18
predicted_values <- c()
expected_values <- c()
for (i in start_from:(nrow(data_final) - 1)) {
  train_set <- data_final[3:i,]
  model <- lm(var1_production ~ var1_acquis_m2, data = train_set)
  predicted_values <- c(predicted_values, predict(model, data_final[i + 1,])[[1]])
  expected_values <- c(expected_values, data_for_prev$var1_production[i + 1])

}

rmse_m1 <- Metrics::rmse(expected_values, predicted_values)
mae_m1 <- Metrics::mae(expected_values, predicted_values)


test <- data.frame(index = seq(1:length(predicted_values)), predicted_values = predicted_values, expected_values = expected_values)

test2 <- test %>%
  pivot_longer(cols = c("predicted_values", "expected_values"),
               names_to = "dimension",
               values_to = "value")

  # get the colors
  color_palette <- return_color_palette(color_list_name = "DGTresor_colors", nb_dimensions = 2)


ggplot(test2, aes(x = index, y = value, color = dimension)) +
  geom_line() +
  labs(title = "France : prévision de la production dans l'industrie manufacturière",
       subtitle = "Variation trimestrielle",
       caption = "Derniers points : PE 2022T3, IPI d'août et PMI de septembre, \nCalculs DG Trésor") +
  guides(color = guide_legend(title = "")) +
  scale_color_manual(breaks = c("expected_values", "predicted_values"),
                     labels = c("Observation", "Prévision"),
                     palette = color_palette) +
  my_theme()


