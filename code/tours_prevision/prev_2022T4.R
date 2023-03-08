# Title     : TODO
# Objective : TODO
# Created by: lphung
# Created on: 05/12/2022

# initialise the environment -------------------------------------------------------------------------------------------
rm(list = ls())

# packages -------------------------------------------------------------------------------------------------------------
library(stringr)
library(dplyr)
library(lubridate)
library(stats)
library(forecast)
library(ggplot2)
library(DGTresorGraphes)

source("../data_importator.R", encoding = "utf-8")
source("../data_preparator.R", encoding = "utf-8")
source("../nonrevised_ipi/loaders_utils.R", encoding = "utf-8", chdir = TRUE)
source("../nonrevised_production/loaders_utils.R", encoding = "utf-8", chdir = TRUE)
# chdir = TRUE needed because we call this Rscript from the main.R and from a RMarkdown, which define working directory differently

source("../old_scripts_from_prevision_production_manuf/loading_data.R", encoding = "utf-8")
source("../old_scripts_from_prevision_production_manuf/data_transformation.R", encoding = "utf-8")


# load data ------------------------------------------------------------------------------------------------------------
load("../../data/nonrevised_ipi_2022-10-01.RData")
load("../../data/nonrevised_production_2022-07-01PE.RData")
load("../../data/revised_ipi_2022-10-01.RData")
load("../../data/revised_production_2022-07-01RD.RData")

# prepare data for prev ------------------------------------------------------------------------------------------------
# get the needed sector and, compare revised and nonrevised data
compare_ipi <- merge_nonrevised_and_revised_data(revised_data = revised_ipi, nonrevised_data = nonrevised_ipi,
                                                 dimension_to_keep = "CZ", column_to_use_for_revised_data = value, column_to_use_for_nonrevised_data = t, data_label = "ipi")

compare_production <- merge_nonrevised_and_revised_data(revised_data = revised_production, nonrevised_data = nonrevised_production,
                                                        dimension_to_keep = "P1E_DIM", column_to_use_for_revised_data = value, column_to_use_for_nonrevised_data = t, data_label = "production")

# get nonrevised data corrected for the effects of changing bases
nonrevised_ipi_cz <- compare_ipi %>%
  dplyr::filter(dimension == "../nonrevised_ipi") %>%
  dplyr::mutate(value = case_when(
    lubridate::year(date) >= 2009 & lubridate::year(date) < 2013 ~ value + get_changing_base_shift(compare_ipi, "2009-01-01", "2012-12-01"),
    TRUE ~ value
  ))

nonrevised_production_cz <- compare_production %>%
  dplyr::filter(dimension == "../nonrevised_production") %>%
  dplyr::mutate(value = case_when(
    lubridate::year(date) >= 2011 & lubridate::year(date) <= 2013 ~ value + get_changing_base_shift(compare_production, "2011-01-01", "2013-10-01"),
    lubridate::year(date) >= 2014 & date <= lubridate::ymd("2018-01-01") ~ value + get_changing_base_shift(compare_production, "2014-01-01", "2018-01-01"),
    TRUE ~ value
  ))

# create the meta-dataframe for the previsions
ipi_for_prev <- nonrevised_ipi_cz %>%
  dplyr::mutate(dimension = "ipi") %>%
  get_variation_for(variation_type = "growth_rate", add_option = TRUE) %>%
  month_to_quarter(transformation_type = "split") %>%
  convert_to_wide_format() %>%
  dplyr::mutate(acquis_ipi_m1 = ipi_m1 * 3,
                # acquis_ipi_m2 = ipi_m1 + ipi_m2 * 2,
                # avg_acquis_ipi_m2 = ((ipi_m1 + ipi_m2) / 2) * 3,
                acquis_ipi_m3 = ipi_m1 + ipi_m2 + ipi_m3
  ) %>%
  dplyr::mutate(var1_acquis_m1 = acquis_ipi_m1 / dplyr::lag(acquis_ipi_m3) - 1
                #  ,
                # var1_acquis_m2 = acquis_ipi_m2 / dplyr::lag(acquis_ipi_m2) - 1,
                # var1_avg_acquis_m2 = avg_acquis_ipi_m2 / dplyr::lag(avg_acquis_ipi_m2) - 1,
                # var1_acquis_m3 = acquis_ipi_m3 / dplyr::lag(acquis_ipi_m3) - 1
  )

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

data_for_model_var_sum <- data_for_prev %>%
  dplyr::filter(date <= ymd("2022-10-01")) %>%
  dplyr::arrange(date) %>%
  dplyr::select(date, var1_production, var1_acquis_m1) %>%
  dplyr::mutate(lag1_var1_production = lag(var1_production))
# %>% # TODO: maybe add the lag of the var1_acquis_m3 to get the variation of the full quarter before

# prepare survey data for prev -----------------------------------------------------------------------------------------
pmi_manuf <- load_pmi_data_from_excel(path_to_data = PATH_TO_PMI_DATA,
                                      column_list = PMI_EXCEL_DIMENSIONS_TO_DATAFRAME_DIMENSIONS)
survey_data <- pmi_manuf %>%
  filter(dimension %in% c("pmi_climat", "pmi_production_passee", "pmi_new_commandes"
                          # , "pmi_emploi",
                          # , "pmi_delais_livraison", "pmi_stocks_achats"
  ))

transfo_survey_data <- survey_data %>%
  # get_variation_for(variation_type = "difference", add_option = TRUE, nbr_lag = 1) %>%
  # get_variation_for(variation_type = "difference", add_option = TRUE, nbr_lag = 3,
  #                   dimensions_to_transform = c("pmi_climat", "pmi_production_passee")) %>%
  month_to_quarter(transformation_type = "split") %>%
  pivot_wider(names_from = dimension,
              values_from = value) %>%
  mutate(sum_climat = pmi_climat_m1 + pmi_climat_m2, # + pmi_climat_m3,
         sum_prod_passee = pmi_production_passee_m1 + pmi_production_passee_m2,
         sum_new_commandes = pmi_new_commandes_m1 + pmi_new_commandes_m2) %>%  #  + pmi_production_passee_m3) %>%
  mutate(sum_climat_vt = sum_climat - lag(sum_climat),
         sum_prod_passee_vt = sum_prod_passee - lag(sum_prod_passee),
         sum_climat_ga = sum_climat - lag(sum_climat, n = 4),
         sum_prod_passee_ga = sum_prod_passee - lag(sum_prod_passee, n = 4),
         sum_climat_growthrate = sum_climat / lag(sum_climat) - 1,
         sum_climat_growthrate4 = sum_climat / lag(sum_climat, n = 4) - 1,
         sum_prod_passee_growthrate = sum_prod_passee / lag(sum_prod_passee) - 1,
         sum_new_commandes_growthrate = sum_new_commandes / lag(sum_new_commandes) - 1) %>%
  filter(date >= ymd("2011-01-01") & date <= ymd("2022-10-01"))

data_merged <- data_for_model_var_sum %>%
  full_join(transfo_survey_data, by = "date")


# do the prevision -----------------------------------------------------------------------------------------------------
# TODO : replace 2020T2-T4 to NA
data_final <- data_merged %>%
  filter(date <= ymd("2022-10-01")) %>%  # NOTE: filter one quarter before the quarter to predict to calculate the RMSE
  mutate(var1_production_complete = var1_production) %>%
  mutate(var1_production = case_when(
    date == ymd("2020-04-01") ~ NA_real_,
    date == ymd("2020-07-01") ~ NA_real_,
    date == ymd("2020-10-01") ~ NA_real_,
    TRUE ~ var1_production
  ))

start_from <- 20
predicted_values <- c()
expected_values <- c()
for (i in start_from:(nrow(data_final) - 1)) {
  if (!(i %in% c(37, 38, 39))) { # do not take dates for 2020T1, 2020T2, 2020T3 CF. i +1 dans la formule => concerne les trimestres 2020-T2-T4
    train_set <- data_final[3:i,]
    model <- lm(var1_production ~ var1_acquis_m1 + sum_new_commandes_growthrate, data = train_set)
    predicted_values <- c(predicted_values, predict(model, data_final[i + 1,])[[1]])
    expected_values <- c(expected_values, data_for_prev$var1_production[i + 1])
  }
}


model_rmse <- Metrics::rmse(expected_values, predicted_values)
model_mae <- Metrics::mae(expected_values, predicted_values)

model_rmse
model_mae

predicted_values <- c(predicted_values[1:17], NA, NA, NA, predicted_values[18:length(predicted_values)])
expected_values <- c(expected_values[1:17], data_final$var1_production_complete[38], data_final$var1_production_complete[39], data_final$var1_production_complete[40], expected_values[18:length(expected_values)])


test <- data.frame(index = seq(1:length(predicted_values)), predicted_values = predicted_values, expected_values = expected_values)

test2 <- test %>%
  pivot_longer(cols = c("predicted_values", "expected_values"),
               names_to = "dimension",
               values_to = "value")

# get the colors
color_palette <- color_palette_for(color_list_name = "FR_derouleur", nb_dimensions = 2)


ggplot(test2, aes(x = index, y = value, color = dimension)) +
  geom_line() +
  labs(title = "France : prévision de la production dans l'industrie manufacturière",
       subtitle = "Variation trimestrielle",
       caption = "Derniers points : RD 2022T3, IPI d'octobre et PMI de novembre, \nCalculs DG Trésor") +
  guides(color = guide_legend(title = "")) +
  scale_color_manual(breaks = c("expected_values", "predicted_values"),
                     labels = c("Observation", "Prévision"),
                     palette = color_palette) +
  dgtresor_theme()

