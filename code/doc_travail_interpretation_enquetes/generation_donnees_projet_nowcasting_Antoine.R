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

# survey_data <- load_data_for_nowcasting(PATH_TO_DATA_FOR_NOWCASTING)

sheets_to_load <- c("pmi_flash", "indices_synthetiques", "pmi_sous_soldes", "insee_contraintes_prod", "bdf_sous_soldes")
survey_data <- NULL
for (sheet in sheets_to_load) {
  new_data <- load_data_for_nowcasting(PATH_TO_DATA_FOR_NOWCASTING, sheet = sheet)

  # deal with the specific case of pmi_industrie_production_passee, which exists in the sheets indices_synthetiques & pmi_sous_soldes
  if(sheet == "pmi_sous_soldes" && "indices_synthetiques" %in% sheets_to_load){
    new_data <- new_data %>%
      dplyr::filter(dimension != "pmi_industrie_production_passee")
  }

  if(is.null(survey_data)){
    survey_data <- new_data
  } else {
    survey_data <- survey_data %>%
      dplyr::bind_rows(new_data)
  }
  rm(new_data)
}

survey_data_split <- survey_data %>%
  month_to_quarter(transformation_type = "split")
survey_data_split <- survey_data_split %>%
  get_variation_for(variation_type = "lead", nbr_lead = 1, add_option = TRUE, dimensions_to_transform = stringr::str_subset(unique(survey_data_split$dimension), ".*_m1"))
# Note : we create lead only for the indices at month 1

survey_data_growth_rate <- survey_data_split %>%
  get_variation_for(variation_type = "growth_rate", add_option = TRUE)

survey_data_difference <- survey_data_split %>%
  get_variation_for(variation_type = "difference", keep_prefix = TRUE)

full_survey_data <- survey_data_growth_rate %>%
  dplyr::bind_rows(survey_data_difference) %>%
  convert_to_wide_format()
# %>% dplyr::select(-contains("construction"))

# save(full_survey_data, file = paste0("./code/doc_travail_interpretation_enquetes/data/data_prev_doc_travail_", today(), ".RData"))

# 3. create the dataframes for the prevision ---------------------------------------------------------------------------

## Note that what matters is the PIB quarterly variable => get the quarterly variable published at each period and then apply
## the variations to the level to get rebased data

# get nonrevised PIB quarterly growth rate
corrected_nonrevised_pib <- nonrevised_pib %>%
  dplyr::select(date, dimension, t, t_1, t_4) %>% # dplyr::select(matches("(date)|(dimension)|(^t$)|(^t_[1-9]$)")) # To keep several t_XX
  dplyr::filter(dimension == "PIB") %>%
  dplyr::mutate(dimension = "nonrevised_pib") %>%
  dplyr::mutate(var1_PIB = t / t_1 - 1,
                var4_PIB = t / t_4 - 1) %>%
  dplyr::select(date, var1_PIB, var4_PIB)
# TODO: to get GDP in level, apply the quarterly variations backward to the last level of GDP available (for the last base)


full_data <- corrected_nonrevised_pib %>%
  dplyr::full_join(full_survey_data, by = "date")

# prepare data for regression
prevision_data <- full_data %>%
  dplyr::arrange(date)

save(prevision_data, file = paste0("./code/doc_travail_interpretation_enquetes/data/prevision_data_", today(), ".RData"))

