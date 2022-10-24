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
source("./code/data_importator.R", encoding = "utf-8")
source("./code/data_preparator.R", encoding = "utf-8")
source("./code/nonrevised_ipi/loaders_utils.R", encoding = "utf-8", chdir = TRUE)
source("./code/nonrevised_production/loaders_utils.R", encoding = "utf-8", chdir = TRUE)
# chdir = TRUE needed because we call this Rscript from the main.R and from a RMarkdown, which define working directory differently


# constants to define --------------------------------------------------------------------------------------------------
ONLY_UPDATE_NONREVISED_IPI_DATA <- TRUE
ONLY_UPDATE_NONREVISED_PRODUCTION_DATA <- TRUE
# Note: production data in this project always refers to the production or value added in the manufacturing sector in the quarterly national accounts

# 1. load nonrevised ipi -------------------------------------------------------------------------------------------------
if (ONLY_UPDATE_NONREVISED_IPI_DATA) {
  # nonrevised_ipi <- update_nonrevised_ipi()
  load("./data/nonrevised_ipi_2022-08-01.RData")
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
if (ONLY_UPDATE_NONREVISED_IPI_DATA) {
  # nonrevised_production <- update_nonrevised_production()
  load("./data/nonrevised_production_2019-04-01.RData")
} else {
  # preparation of the list
  PRODUCTION_DATA_FOLDERS <- get_production_data_files(PRODUCTION_DATA_FOLDER, estimation_type = "PE")
  # we keep only the folders containing the 1st estimation of the quarterly accounts (PE)
  PRODUCTION_FILES_TYPES <- list("pre_19T2RD_csv" = stringr::str_subset(names(PRODUCTION_DATA_FOLDERS), "^1(?!(9T2RD)|(9T3PE)|(9T3RD)|(9T4PE)|(9T4RD)).*"), # we want
                                 "post_19T2RD_xls" = "",
                                 "post_19T2RD_rdata" = "")

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
load("./data/revised_production_19T2PE.RData")

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
  dplyr::mutate(AR1_production = dplyr::lag(production))

data_for_prev <- ipi_for_prev %>%
  dplyr::full_join(production_for_prev, by = "date") %>%
  dplyr::filter(date >= lubridate::ymd("2011-04-01")) %>%
  dplyr::arrange(date)

# cleaning data_for_prev
data_for_prev <- data_for_prev %>%
  dplyr::filter(date <= ymd("2019-01-01")) %>%
  dplyr::arrange(date)

# 3.3. create the dataframes to test different models
# NOTE: we test the models only for IPI with the second month of the quarter
# TODO: to continue here => need to create 3 dataframes for the 3 models (when we have m_1, m_2 and m_3)

data_for_model_level_sum <- data_for_prev %>%
  dplyr::mutate(lag_acquis_ipi_m2 = lag(acquis_ipi_m2)) %>%
  dplyr::select(production, acquis_ipi_m2,lag_acquis_ipi_m2, AR1_production)
#
data_for_model_level_split <- data_for_prev %>%
  dplyr::mutate(lag_ipi_m1 = lag(ipi_m1),
                lag_ipi_m2 = lag(ipi_m2)) %>%
  dplyr::select(production, ipi_m1, ipi_m2, lag_ipi_m1, lag_ipi_m2, AR1_production)
# %>%
#   stats::ts(start = c(2011, 2), frequency = 4)

data_for_model_var_sum <- data_for_prev %>%
  dplyr::filter(date <= ymd("2019-01-01")) %>%
  dplyr::arrange(date) %>%
  dplyr::select(var1_production, var1_acquis_m2) %>%
  dplyr::mutate(AR1_var1_production = lag(var1_production))
# %>% # TODO: maybe add the lag of the var1_acquis_m3 to get the variation of the full quarter before
#   stats::ts(start = c(2011, 2), frequency = 4)

# data_for_model_var_split <- data_for_prev %>%
#   dplyr::select(var1_production, var1_ipi_m1, var1_ipi_m2) %>%
#   stats::ts(start = c(2011, 2), frequency = 4)

# 4. generate the models for second month of the quarter ---------------------------------------------------------------

start_from <- 18
predicted_values <- c()
expected_values <- c()
for (i in start_from:(nrow(data_for_model_var_sum)-1)){
  train_set <- data_for_model_var_sum[2:i,]
  model <- lm(var1_production ~ var1_acquis_m2 + AR1_var1_production, data = train_set)
  # print(summary(model))
  predicted_values <- c(predicted_values, predict(model, data_for_model_var_sum[i+1,])[[1]])
  expected_values <- c(expected_values, data_for_prev$var1_production[i+1])
}

rmse_m1 <- Metrics::rmse(expected_values, predicted_values)
mae_m1 <- Metrics::mae(expected_values, predicted_values)


# predicted_values <- c()
# expected_values <- c()
# for (i in start_from:(nrow(data_for_model_level_split)-1)){
#   train_set <- data_for_model_level_split[2:i,]
#   model <- lm(production ~ ipi_m1 + ipi_m2 + lag_ipi_m1 + lag_ipi_m2 + AR1_production, data = train_set)
#   # print(summary(model))
#   in_level_current_predicted_production <- predict(model, data_for_model_level_split[i+1,])[[1]]
#   in_level_previous_production <- data_for_prev$production[i]
#   variation_prediction <- in_level_current_predicted_production / in_level_previous_production - 1
#   predicted_values <- c(predicted_values, variation_prediction)
#   expected_values <- c(expected_values, data_for_prev$var1_production[i+1])
# }
#
# rmse_m2 <- Metrics::rmse(expected_values, predicted_values)
# mae_m2 <- Metrics::mae(expected_values, predicted_values)



# predicted_values <- c()
# expected_values <- c()
# for (i in start_from:(nrow(data_for_model_level_sum)-1)){
#   train_set <- data_for_model_level_sum[2:i,]
#   model <- lm(production ~ acquis_ipi_m2 + lag_acquis_ipi_m2 + AR1_production, data = train_set)
#   # print(summary(model))
#   in_level_current_predicted_production <- predict(model, data_for_model_level_sum[i+1,])[[1]]
#   in_level_previous_production <- data_for_prev$production[i]
#   variation_prediction <- in_level_current_predicted_production / in_level_previous_production - 1
#   predicted_values <- c(predicted_values, variation_prediction)
#   expected_values <- c(expected_values, data_for_prev$var1_production[i+1])
# }
#
# rmse_m3 <- Metrics::rmse(expected_values, predicted_values)
# mae_m3 <- Metrics::mae(expected_values, predicted_values)


test <- data.frame(index = seq(1:length(predicted_values)), predicted_values = predicted_values, expected_values = expected_values)

test2 <- test %>%
  pivot_longer(cols = c("predicted_values", "expected_values"),
               names_to = "dimension",
  values_to = "value")

ggplot(test2, aes(x = index, y = value, color = dimension)) +
  geom_line()

