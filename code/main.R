# Title     : TODO
# Objective : TODO
# Created by: lphung
# Created on: 12/09/2022

# packages -------------------------------------------------------------------------------------------------------------
library(stringr)
library(dplyr)
library(ggplot2)
library(plotly)
source("./code/nonrevised_ipi/loaders_utils.R", encoding = "utf-8")
source("./code/nonrevised_production/loaders_utils.R", encoding = "utf-8")

# constants to define --------------------------------------------------------------------------------------------------
ONLY_UPDATE_NONREVISED_IPI_DATA <- TRUE
ONLY_UPDATE_NONREVISED_PRODUCTION_DATA <- TRUE
# Note: production data in this project always refers to the production or value added in the manufacturing sector in the quarterly national accounts

# preparation of lists -------------------------------------------------------------------------------------------------
IPI_DATA_FILES <- get_ipi_data_files(IPI_DATA_FOLDER)
PRODUCTION_DATA_FOLDERS <- get_production_data_files(PRODUCTION_DATA_FOLDER, estimation_type = "PE")
# we keep only the folders containing the 1st estimation of the quarterly accounts (PE)

IPI_FILES_TYPES <- list("sectors_in_line_one_label_loader" = c("200901", "200903"),
                        "sectors_in_line_two_labels_loader" = stringr::str_subset(names(IPI_DATA_FILES), "(^2009(?!(01)|(03)).*)|(^2010(?!(11)|(12)).*)"))

PRODUCTION_FILES_TYPES <- list("pre_19T2RD_csv" = stringr::str_subset(names(PRODUCTION_DATA_FOLDERS), "^1(?!(9T2RD)|(9T3PE)|(9T3RD)|(9T4PE)|(9T4RD)).*"), # we want
                               "post_19T2RD_xls" = "",
                               "post_19T2RD_rdata" = "")

# 1. load nonrevised ipi -------------------------------------------------------------------------------------------------
if (ONLY_UPDATE_NONREVISED_IPI_DATA) {
  # nonrevised_ipi <- update_nonrevised_ipi()
  load("./data/nonrevised_ipi_2022-08-01.RData")
} else {
  nonrevised_ipi <- construct_nonrevised_ipi_from_scratch(files_list = IPI_DATA_FILES,
                                                          file_type2files_list = IPI_FILES_TYPES,
                                                          number_previous_values = 24,
                                                          data_correction = "CJO-CVS") # TODO: data_correction as argument within the function
}

# save(nonrevised_ipi, file = paste0("./data/", "nonrevised_ipi_", max(unique(nonrevised_ipi[["date"]])), ".RData"))

# 2. load nonrevised production ----------------------------------------------------------------------------------------
if (ONLY_UPDATE_NONREVISED_IPI_DATA) {
  load("./data/nonrevised_production_2019-04-01.RData")
} else {
  nonrevised_production <- construct_nonrevised_production_from_scratch(files_list = PRODUCTION_DATA_FOLDERS,
                                                                      file_type2files_list = PRODUCTION_FILES_TYPES,
                                                                      number_previous_values = 24)
}

# save(nonrevised_production, file = paste0("./data/", "nonrevised_production_", max(unique(nonrevised_production[["date"]])), ".RData"))

# comparing data -------------------------------------------------------------------------------------------------------
source("./code/data_importator.R", encoding = "utf-8")

ipi_data <- load_data_from_insee(dimensions_to_load = IPI_SECTORS_NAF2,
                                 dimensions_label_list = SECTORS_LABEL_LIST)

ipi_data_cz <- ipi_data %>%
  dplyr::select(-label) %>%
  dplyr::filter(dimension == "CZ") %>%
  dplyr::mutate(dimension = "ipi_from_insee_plateform")

nonrevised_ipi_cz <- nonrevised_ipi %>%
  dplyr::filter(dimension == "CZ") %>%
  dplyr::mutate(dimension = "nonrevised_ipi") %>%
  dplyr::select(date, dimension, t) %>%
  dplyr::rename(value = t)

compare_ipi <- nonrevised_ipi_cz %>%
  dplyr::bind_rows(ipi_data_cz) %>%
  dplyr::arrange(date) %>%
  dplyr::group_by(dimension) %>%
  dplyr::mutate(evol = (value / dplyr::lag(value)) - 1) %>%
  ungroup()

my_plot <- ggplot(compare_ipi) +
  aes(x = date, y = value, color = dimension) +
  geom_line()

ggplotly(my_plot)

# calcul des révisions

test <- compare_ipi %>%
  dplyr::filter(date >= "2013-01-01") %>%
  dplyr::select(-evol) %>%
  tidyr::pivot_wider(names_from = dimension,
                     values_from = value) %>%
  dplyr::mutate(diff = nonrevised_ipi - ipi_from_insee_plateform) %>%
  dplyr::mutate(diff_sup = case_when(
    diff > 0 ~ 1,
    TRUE ~ 0
  ))

test %>% dplyr::summarise(somme_ecart = mean(diff, na.rm = TRUE),
                          part_superieur = mean(diff_sup, na.rm = TRUE))