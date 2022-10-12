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

# constants to define --------------------------------------------------------------------------------------------------
ONLY_UPDATE_NONREVISED_IPI_DATA <- FALSE

# preparation of lists -------------------------------------------------------------------------------------------------
IPI_DATA_FILES <- get_ipi_data_files(IPI_DATA_FOLDER)

IPI_FILES_TYPES <- list("sectors_in_line_one_label_loader" = c("200901", "200903"),
                        "sectors_in_line_two_labels_loader" = stringr::str_subset(names(IPI_DATA_FILES), "(^2009(?!(01)|(03)).*)|(^2010(?!(11)|(12)).*)"))


# 1. load nonrevised ipi -------------------------------------------------------------------------------------------------

if (ONLY_UPDATE_NONREVISED_IPI_DATA) {
  # nonrevised_ipi <- update_nonrevised_ipi()
  nonrevised_ipi <- load("./data/nonrevised_ipi_2022-08-01.RData")
} else {
  nonrevised_ipi <- construct_nonrevised_ipi_from_scratch(files_list = IPI_DATA_FILES,
                                                      file_type2files_list = IPI_FILES_TYPES,
                                                      number_previous_values = 24,
                                                      data_correction = "CJO-CVS")
}

#save(nonrevised_ipi, file = paste0("./data/", "nonrevised_ipi_", max(unique(nonrevised_ipi[["date"]])), ".RData"))

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
  dplyr::mutate(evol = (value / dplyr::lag(value)) -1) %>%
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