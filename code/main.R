# Title     : TODO
# Objective : TODO
# Created by: lphung
# Created on: 12/09/2022

# packages -------------------------------------------------------------------------------------------------------------
library(stringr)
library(dplyr)
library(ggplot2)
library(plotly)
source("./code/realtime_ipi/loaders_utils.R", encoding = "utf-8")

# constants to define --------------------------------------------------------------------------------------------------
ONLY_UPDATE_REALTIME_IPI_DATA <- FALSE

# preparation of lists -------------------------------------------------------------------------------------------------
IPI_DATA_FILES <- get_ipi_data_files(IPI_DATA_FOLDER)

IPI_FILES_TYPES <- list("sectors_in_line_one_label_loader" = c("200901", "200903"),
                        "sectors_in_line_two_labels_loader" = stringr::str_subset(names(IPI_DATA_FILES), "(^2009(?!(01)|(03)).*)|(^2010(?!(11)|(12)).*)"))


# 1. load realtime ipi -------------------------------------------------------------------------------------------------

if (ONLY_UPDATE_REALTIME_IPI_DATA) {
  realtime_ipi <- update_realtime_ipi()
} else {
  realtime_ipi <- construct_realtime_ipi_from_scratch(files_list = IPI_DATA_FILES,
                                                      file_type2files_list = IPI_FILES_TYPES,
                                                      number_revision = 1,
                                                      data_correction = "CJO-CVS",
                                                      keep_historical_data = FALSE)
}

#
# test_path <- "S:/SPMAE/PREV/Prev3/_Fichiers_Prev3/Prod_manuf/02-IPI/mail_reaction_ipi/02-Envoi_Insee/2020/series_longues_ipi_20202.xls"
#
# test <- generic_loader(test_path)

# comparing data -------------------------------------------------------------------------------------------------------
source("./code/data_importator.R", encoding = "utf-8")

ipi_data <- load_data_from_insee(dimensions_to_load = IPI_SECTORS_NAF2,
                                 dimensions_label_list = SECTORS_LABEL_LIST)

ipi_data_cz <- ipi_data %>%
  dplyr::select(-label) %>%
  dplyr::filter(dimension == "CZ") %>%
  dplyr::mutate(dimension = "ipi_from_insee_plateform")

realtime_ipi_cz <- realtime_ipi %>%
  dplyr::filter(dimension == "CZ") %>%
  dplyr::mutate(dimension = "realtime_ipi")

compare_ipi <- realtime_ipi_cz %>%
  dplyr::bind_rows(ipi_data_cz) %>%
  dplyr::arrange(date) %>%
  dplyr::group_by(dimension) %>%
  dplyr::mutate(evol = (value / dplyr::lag(value)) -1) %>%
  ungroup()

my_plot <- ggplot(compare_ipi) +
  aes(x = date, y = evol, color = dimension) +
  geom_line()

ggplotly(my_plot)
