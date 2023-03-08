# Title     : TODO
# Objective : investigate the relationships between GDP and business sentiments
# Created by: lphung
# Created on: 18/01/2023

# TODO : mettre le tout dans un .Rmd

# clean environment ----------------------------------------------------------------------------------------------------
rm(list = ls())

# libraries ------------------------------------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(corrplot)
library(stats)
library(lubridate)
library(ggplot2)
library(zoo)

source("./code/doc_travail_interpretation_enquetes/helpers.R", encoding = "utf-8")
source("./code/data_preparator.R", encoding = "utf-8")
source("./code/scripts_from_automatisation_reactions/general_graph_functions.R", encoding = "utf-8", chdir = TRUE)
# chdir = TRUE needed because we call this Rscript from the main.R and from a RMarkdown, which define working directory differently


# constants ------------------------------------------------------------------------------------------------------------

LOAD_EXISTING_DATA <- TRUE      # if FALSE, update data

# load data ------------------------------------------------------------------------------------------------------------

if (LOAD_EXISTING_DATA) {
  load("./code/doc_travail_interpretation_enquetes/data/data_doc_travail_20230131.RData")
} else {
  # read data --------------------
  pib_data <- compta_nat_loader(folder_path = PIB_DATA_FOLDER,
                                file_name = PIB_FILE_NAME,
                                dimensions_list = PIB_DIMENSIONS,
                                dimensions_list_name = "default")

  pmi_data <- load_pmi_data_from_excel_all_dimensions(path_to_data = PATH_TO_PMI_DATA,
                                                      column_list = PMI_DIMENSIONS_LIST) %>%
    filter(dimension %in% c("pmi_composite", "pmi_industrie", "pmi_services"))
  print("Dans nos fichiers, nous n'avons pas de données PIB avant le 1er janvier 1999. Pourtant, on devrait pouvoir trouver des données depuis le T3 1998.")

  insee_data <- fr_derouleur_importator(path_to_climat_data = PATH_TO_FR_DEROULEUR,
                                        column_list = INSEE_DIMENSIONS_LIST,
                                        data_source = "insee",
                                        columns_to_import = c(1, 15, 16, 17, 19))

  # télécharger les données BdF
  bdf_data <- fr_derouleur_importator(path_to_climat_data = PATH_TO_FR_DEROULEUR,
                                      column_list = BDF_DIMENSIONS_LIST,
                                      data_source = "bdf",
                                      columns_to_import = c(1, 9, 10, 11, 12))

  # prepare data preparation  -----------
  pib_data <- pib_data %>%
    get_variation_for(variation_type = "growth_rate", add_option = TRUE, dimensions_to_transform = "PIB") %>%
    get_variation_for(variation_type = "growth_rate", add_option = TRUE, dimensions_to_transform = "PIB", nbr_lag = 4)

  survey_data <- insee_data %>%
    dplyr::bind_rows(pmi_data) %>%
    dplyr::bind_rows(bdf_data)

  survey_data_split <- survey_data %>%
    month_to_quarter(transformation_type = "split") %>%
    get_variation_for(variation_type = "growth_rate", add_option = TRUE, dimensions_to_transform = c("insee_global_m1", "insee_global_m3")) %>%
    get_variation_for(variation_type = "difference", add_option = TRUE, dimensions_to_transform = c("insee_global_m1", "insee_global_m3"))

  survey_data_mean <- survey_data %>%
    month_to_quarter(transformation_type = "mean")

  survey_data_mean_vt <- survey_data_mean %>%
    get_variation_for(variation_type = "growth_rate", add_option = TRUE, keep_prefix = TRUE)

  survey_data_mean_ga <- survey_data_mean %>%
    get_variation_for(variation_type = "growth_rate", nbr_lag = 4, keep_prefix = TRUE)

  survey_data_mean_diff <- survey_data_mean %>%
    get_variation_for(variation_type = "difference", keep_prefix = TRUE)

  full_data <- pib_data %>%
    dplyr::bind_rows(survey_data_split) %>%
    dplyr::bind_rows(survey_data_mean_vt) %>%
    dplyr::bind_rows(survey_data_mean_ga) %>%
    dplyr::bind_rows(survey_data_mean_diff) %>%
    convert_to_wide_format() %>%
    dplyr::mutate(lead_var1_insee_global_m3 = lead(var1_insee_global_m1),
                  lead_diff1_insee_global_m3 = lead(diff1_insee_global_m1),
                  lead_insee_global_m1 = lead(insee_global_m1))

  full_data <- full_data %>%
    dplyr::select(date, contains(c("PIB", "global", "composite"))) %>%
    dplyr::mutate(pmi_composite_m_1 = lag(pmi_composite_m3),
                  pmi_composite_m_2 = lag(pmi_composite_m2),
                  bdf_global_m_1 = lag(bdf_global_m3),
                  bdf_global_m_2 = lag(bdf_global_m2)
    ) %>%
    dplyr::mutate(pmi_composite_trim_formel = 1 / 9 * pmi_composite_m3 +
      2 / 9 * pmi_composite_m2 +
      1 / 3 * pmi_composite_m1 +
      2 / 9 * pmi_composite_m_1 +
      1 / 9 * pmi_composite_m_2,
                  bdf_global_trim_formel = 1 / 9 * bdf_global_m3 +
                    2 / 9 * bdf_global_m2 +
                    1 / 3 * bdf_global_m1 +
                    2 / 9 * bdf_global_m_1 +
                    1 / 9 * bdf_global_m_2) %>%
    dplyr::select(-pmi_composite_m_1, -pmi_composite_m_2, -bdf_global_m_1, -bdf_global_m_2)

  save(full_data, file = "./code/doc_travail_interpretation_enquetes/data/data_doc_travail_20230131.RData")
}


# analysis -------------------------------------------------------------------------------------------------------------

## Partie 0 : cut outliers --------------------------------------------

# Data for graph
data_graph_evol_PIB <- full_data %>%
  dplyr::filter(date >= lubridate::ymd("1999-01-01") & date <= lubridate::ymd("2019-10-10")) %>%
  dplyr::select(date, var1_PIB, var4_PIB) %>%
  tidyr::pivot_longer(cols = c("var1_PIB", "var4_PIB"),
                      names_to = "dimension",
                      values_to = "value") %>%
  dplyr::mutate(quarter = zoo::as.yearqtr(date))

# Graphique variation trimestrielle
ggplot(data_graph_evol_PIB %>% filter(dimension == "var1_PIB")) +
  aes(x = quarter, y = value) +
  geom_line(color = "darkblue") +
  labs(title = "Variation trimestrielle du PIB trimestriel français",
       subtitle = "Période : T1 1999 - T4 2019",
       caption = "Source : Insee, calculs DG Trésor,
                  Données : RD 2022T3 des comptes nationaux trimestriels") +
  my_theme() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = c(-0.02, -0.01, 0, 0.01, 0.02),
                     labels = scales::percent_format(accuracy = 1L, decimal.mark = ",")) +
  zoo::scale_x_yearqtr(format = "T%q %Y") +
  geom_hline(yintercept = 0.011, color = "darkblue", linetype = "longdash") +
  geom_hline(yintercept = -0.006, color = "darkblue", linetype = "longdash")

# Graphique glissement annuel
ggplot(data_graph_evol_PIB %>% filter(dimension == "var4_PIB")) +
  aes(x = quarter, y = value) +
  geom_line(color = "darkorange") +
  labs(title = "Glissement annuel du PIB trimestriel français",
       subtitle = "Période : T1 1999 - T4 2019",
       caption = "Source : Insee, calculs DG Trésor,
                  Données : RD 2022T3 des comptes nationaux trimestriels") +
  my_theme() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = c(-0.04, -0.03, -0.02, -0.01, 0, 0.01, 0.02, 0.03, 0.04, 0.05),
                     labels = scales::percent_format(accuracy = 1L, decimal.mark = ",")) +
  zoo::scale_x_yearqtr(format = "T%q %Y") +
  geom_hline(yintercept = 0.031, color = "darkorange", linetype = "longdash") +
  geom_hline(yintercept = -0.001, color = "darkorange", linetype = "longdash")


############ Enseignement 0.1 :
# Il y a deux outliers évidents : T4 2008 et T1 2009 ;
# le point T2 2008 est bas mais ça devrait aller ;
# le point T4 1999 est particulièrement haut

full_data %>%
  dplyr::select(date, var1_PIB) %>%
  dplyr::filter(date >= lubridate::ymd("1999-01-01") & date <= lubridate::ymd("2019-10-10")) %>%
  dplyr::mutate(period = case_when(
    date <= lubridate::ymd("2008-01-01") ~ "avant 2008-9",
    date >= lubridate::ymd("2009-04-01") ~ "après 2008-9",
    TRUE ~ NA_character_
  )) %>%
  dplyr::filter(!(is.na(period))) %>%
  dplyr::group_by(period) %>%
  dplyr::summarise(mean = mean(var1_PIB),
                   median = median(var1_PIB))

full_data %>%
  dplyr::select(date, var1_PIB) %>%
  dplyr::filter(date >= lubridate::ymd("1999-01-01") & date <= lubridate::ymd("2019-10-10")) %>%
  dplyr::filter(date != lubridate::ymd("1999-10-01")) %>% # remove outlier
  dplyr::mutate(period = case_when(
    date <= lubridate::ymd("2008-01-01") ~ "avant 2008-9",
    date >= lubridate::ymd("2009-04-01") ~ "après 2008-9",
    TRUE ~ NA_character_
  )) %>%
  dplyr::filter(!(is.na(period))) %>%
  dplyr::group_by(period) %>%
  dplyr::summarise(mean = mean(var1_PIB),
                   median = median(var1_PIB))

full_data %>%
  dplyr::select(date, var1_PIB) %>%
  dplyr::filter(date >= lubridate::ymd("1990-01-01") & date <= lubridate::ymd("2019-10-10")) %>%
  dplyr::filter(date != lubridate::ymd("1999-10-01")) %>% # remove outlier
  dplyr::mutate(period = case_when(
    date <= lubridate::ymd("2008-01-01") ~ "avant 2008-9",
    date >= lubridate::ymd("2009-04-01") ~ "après 2008-9",
    TRUE ~ NA_character_
  )) %>%
  dplyr::filter(!(is.na(period))) %>%
  dplyr::group_by(period) %>%
  dplyr::summarise(mean = mean(var1_PIB),
                   median = median(var1_PIB))

############ Enseignement 0.2 :
# On a l'impression de voir deux régimes de croissance, avant et après 2008-9.
# entre 1999T1-2008T1 : 0.55% (et 0.53% sans outlier 1999T4) et MEDIANE : 0.57% (et 0.56% sans outlier 1999T4)
# entre 2009T2-2019T4 : 0.33% et MEDIANE : 0.36%
# et entre 1990T1-2008T1 : 0.50% (et 0.48% sans outlier 1999T4) et MEDIANE : 0.51% (0.51% sans outlier 1999T4)

# TODO : cacluler moyenne roulante sur 10 ans de la variation trimestrielle du PIB
# First occurence
# WINDOW_SIZE <- 10 * 4 # ATTENTION, en nombre de trimestres !!!
# first_correlations <- cor(main_indices_data_reduced[1:WINDOW_SIZE,] %>% dplyr::select(-date), method = "pearson")[, c("var1_PIB", "var4_PIB")]
# rolling_correlations <- data.frame(first_correlations,
#                                    dimension = row.names(first_correlations),
#                                    date = rep(main_indices_data_reduced$date[[WINDOW_SIZE]], 7))
#
# # Following occurences
# for (i in (WINDOW_SIZE + 1):nrow(main_indices_data_reduced)) {
#   correlations <- cor(main_indices_data_reduced[(i - WINDOW_SIZE + 1):i,] %>% dplyr::select(-date), method = "pearson")[, c("var1_PIB", "var4_PIB")]
#   new_df <- data.frame(correlations,
#                        dimension = row.names(correlations),
#                        date = rep(main_indices_data_reduced$date[[i]], 7))
#   rolling_correlations <- rolling_correlations %>%
#     dplyr::bind_rows(new_df)
# }


# Plot PIB et climats
## en variation trimestrielle
data_graph_evol_PIB_climat_vt <- full_data %>%
  dplyr::filter(date >= lubridate::ymd("1999-01-01") & date <= lubridate::ymd("2019-10-10")) %>%
  dplyr::select(date, var1_PIB, lead_insee_global_m1, qt_pmi_composite) %>%
  tidyr::pivot_longer(cols = c("var1_PIB", "lead_insee_global_m1", "qt_pmi_composite"),
                      names_to = "dimension",
                      values_to = "value")

data_graph_evol_PIB_climat_ga <- full_data %>%
  dplyr::filter(date >= lubridate::ymd("1999-01-01") & date <= lubridate::ymd("2019-10-10")) %>%
  dplyr::select(date, var4_PIB, qt_insee_global, qt_bdf_global, qt_pmi_composite) %>%
  tidyr::pivot_longer(cols = c("var4_PIB", "qt_insee_global", "qt_bdf_global", "qt_pmi_composite"),
                      names_to = "dimension",
                      values_to = "value")

compare_insee_pmi_bdf_for_one_index_graph(graph_name = "pib_var_trim_insee_pmi",
                                          graph_folder = "./output",
                                          insee_data = data_graph_evol_PIB_climat_vt %>% dplyr::filter(dimension == "lead_insee_global_m1"),
                                          pmi_data = data_graph_evol_PIB_climat_vt %>% dplyr::filter(dimension == "qt_pmi_composite"),
                                          title = "Variation trimestrielle du PIB et indicateurs synthétiques centrés-réduits",
                                          label_list = list("insee" = "Climat global de l'Insee au mois 1 du trimestre T+1",
                                                            "pmi" = "PMI trimestriel moyen"),
                                          reduced_centered = "manual",
                                          graph_source = "Insee et S&P",
                                          graph_saving = FALSE) +
  geom_line(data = data_graph_evol_PIB_climat_vt %>% dplyr::filter(dimension == "var1_PIB"),
            aes(x = date, y = value * 100, linetype = dimension), color = "black") +
  scale_linetype_manual(values = "longdash",
                        labels = "Variation trimestrielle du PIB") +
  scale_y_continuous(sec.axis = sec_axis(~. / 100,
                                         labels = scales::percent_format(accuracy = 1L, decimal.mark = ","))) + # add a second axis
  labs(subtitle = "Ecart à la moyenne en point d'écart-type (à gauche pour les indices) et pourcentage de variation (à droite pour le PIB)")


compare_insee_pmi_bdf_for_one_index_graph(graph_name = "pib_ga_insee_pmi_bdf",
                                          graph_folder = "./output",
                                          insee_data = data_graph_evol_PIB_climat_ga %>% dplyr::filter(dimension == "qt_insee_global"),
                                          bdf_data = data_graph_evol_PIB_climat_ga %>% dplyr::filter(dimension == "qt_bdf_global"),
                                          pmi_data = data_graph_evol_PIB_climat_ga %>% dplyr::filter(dimension == "qt_pmi_composite"),
                                          title = "Glissement annuel du PIB et indicateurs synthétiques trimestriels moyens centrés-réduits",
                                          label_list = list("insee" = "Climat global Insee",
                                                            "bdf" = "Climat global Banque de France",
                                                            "pmi" = "PMI composite"),
                                          reduced_centered = "manual",
                                          graph_source = "Insee, Banque de France et S&P",
                                          graph_saving = FALSE) +
  geom_line(data = data_graph_evol_PIB_climat_ga %>% dplyr::filter(dimension == "var4_PIB"),
            aes(x = date, y = value * 100 - 1, linetype = dimension), color = "black") +
  scale_linetype_manual(values = "longdash",
                        labels = "Glissement annuel du PIB") +
  scale_y_continuous(breaks = c(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4),
                     sec.axis = sec_axis(~(. + 1) / 100,
                                         breaks = (c(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4) + 1) / 100,
                                         labels = scales::percent_format(accuracy = 1L, decimal.mark = ","))) + # add a second axis
  labs(subtitle = "Ecart à la moyenne en point d'écart-type (à gauche pour les indices) et pourcentage de variation (à droite pour le PIB)")


## Partie 1 : correlations --------------------------------------------

subset_data <- full_data
# %>%
#   dplyr::select(date, var1_PIB, var4_PIB, insee_global_m1, insee_global_m3, lead_insee_global_m1, qt_insee_global,
#                 var1_insee_global_m3, lead_var1_insee_global_m3, var1_qt_insee_global,
#                 diff1_insee_global_m3, lead_diff1_insee_global_m3, diff1_qt_insee_global,
#                 qt_pmi_composite, var1_qt_pmi_composite, diff1_qt_pmi_composite,
#                 qt_bdf_global, var1_qt_bdf_global, diff1_qt_bdf_global,
#                 var4_qt_pmi_composite, var4_qt_bdf_global, var4_qt_insee_global
#   )

corrplot_data <- subset_data %>%
  dplyr::filter(date >= lubridate::ymd("1999-04-01") & date <= lubridate::ymd("2019-10-10")
                # & !(date %in% c(lubridate::ymd("2008-10-01"), lubridate::ymd("2009-01-01"))) # pour variation trimestrielle
                # & !(date %in% c(lubridate::ymd("2009-04-01"), lubridate::ymd("2009-07-01"), lubridate::ymd("2009-10-01"))) # pour glissement annuel
                # & date != lubridate::ymd("1999-10-01")  # pour variation trimestrielle
                # & date >= lubridate::ymd("2001-01-01") # pour glissement annuel
  )

corr_values <- cor(corrplot_data %>% dplyr::select(-date), method = "pearson")
corrplot::corrplot(corr_values[, c("var1_PIB", "var4_PIB")],
                   method = "number", type = "lower", cl.pos = "n")

############ 1er enseignement :
# Sur la période du T2 1999 au T4 2019, on voit clairement que comme l'indique la théorie, les climats
# des affaires trimestrialisées (i.e. moyenne sur le trimestre) en niveau sont les plus corrélés à la variation trimestrielle du PIB,
# a contrario des variations trimestrielles des climats trimestrialisées. On notera que compte tenu de la spécificité de la formulation
# des questions Insee, le climat des affaires du mois m+1 (i.e. 1er mois) du trimestre T+1 est plus corrélé à la variation trimestrielle
# du trimestre T que le climat trimestrialisé.
# La théorie rejoint ainsi la pratique et ça confirme que les chefs d'entreprise répondent précisément à la question.
# On notera aussi qu'à défaut d'avoir les données du mois m+1 du trimestre T+1, on peut se contenter des données du mois m+3 du trimestre T
# pour approximer la variation trimestrielle du trimestre T.
# TODO: ploter graphique


############ 2ème enseignement :
# Les climats en niveau sont beaucoup plus corrélés au glissement annuel du PIB qu'à sa variation trimestrielle,
# avec un gain beaucoup plus net pour les climats Insee et BdF que pour le PMI composite. On notera aussi que le climat
# Insee trimestrialisé fonctionne très bien.


## différence à un seuil : normalement, si c'est seulement un shift à une constance, les corrélations ne devraient pas être modifiées
# test avec 50 et la moyenne de long-terme pour les PMI.
long_term_mean_pmi <- mean(corrplot_data$qt_pmi_composite, na.rm = TRUE)
# TODO : do a long-term mean without outliers (crise 2008  ; autour années 2000 super haut ; on a l'impression qu'entre 2000-2010 la moyenne est autour 55 et entre 2010-1019 autour de 52-53)

main_indices_data <- subset_data %>%
  dplyr::select(date, var1_PIB, var4_PIB, insee_global_m3, lead_insee_global_m1, qt_insee_global, qt_pmi_composite, qt_bdf_global)

main_indices_data_thresold <- main_indices_data %>%
  dplyr::mutate(seuil_insee_global_m3 = insee_global_m3 - 100,
                seuil_lead_insee_global_m1 = lead_insee_global_m1 - 100,
                seuil_qt_insee_global = qt_insee_global - 100,
                seuil_qt_pmi_composite = qt_pmi_composite - 50,
                seuil_mean_qt_pmi_composite = qt_pmi_composite - long_term_mean_pmi,
                seuil_qt_bdf_global = qt_bdf_global - 100)

corrplot_threshold <- main_indices_data_thresold %>%
  dplyr::filter(date >= lubridate::ymd("1999-04-01") & date <= lubridate::ymd("2019-10-10"))

corrplot::corrplot(cor(corrplot_threshold %>% dplyr::select(-date), method = "pearson")[, c("var1_PIB", "var4_PIB")],
                   method = "number", type = "lower", cl.pos = "n")

############ 3ème enseignement :
# Effectivement, ça ne change rien.


## Check stabilité corrélation dans le temps (fenêtre de 10 ans roulantes)
# -----------------------> GO TO PARTIE 3 to check relations between indices and define the estimation period
# NOTE : use subset_data to replicate the results for all indices; the starting date must be 2000-01-01 for the var4_qt_pmi_composite indice
# NOTE : use main_indices_data for the graphs

tableau_1_doc_travail <- subset_data %>%
  dplyr::filter(date >= lubridate::ymd("1999-04-01") & date <= lubridate::ymd("2019-10-10")) %>%
  dplyr::arrange(date)

tableau_2_doc_travail_vt <- subset_data %>%
  dplyr::filter(date >= lubridate::ymd("2001-01-01") & date <= lubridate::ymd("2019-10-10")) %>%
  dplyr::filter(!(date %in% c(lubridate::ymd("2008-10-01"), lubridate::ymd("2009-01-01")))) %>% # pour variation trimestrielle
  dplyr::arrange(date)

tableau_2_doc_travail_ga <- subset_data %>%
  dplyr::filter(date >= lubridate::ymd("2001-01-01") & date <= lubridate::ymd("2019-10-10")) %>%
  dplyr::filter(!(date %in% c(lubridate::ymd("2008-10-01"), lubridate::ymd("2009-01-01"), lubridate::ymd("2009-04-01"), lubridate::ymd("2009-07-01"), lubridate::ymd("2009-10-01")))) %>% # pour glissement annuel
  dplyr::arrange(date)

graph_corr_doc_travail_vt <- main_indices_data %>%
  dplyr::filter(date >= lubridate::ymd("2001-01-01") & date <= lubridate::ymd("2019-10-10")) %>%
  dplyr::filter(!(date %in% c(lubridate::ymd("2008-10-01"), lubridate::ymd("2009-01-01")))) %>% # pour variation trimestrielle
  dplyr::arrange(date)

graph_corr_doc_travail_ga <- main_indices_data %>%
  dplyr::filter(date >= lubridate::ymd("2001-01-01") & date <= lubridate::ymd("2019-10-10")) %>%
  dplyr::filter(!(date %in% c(lubridate::ymd("2008-10-01"), lubridate::ymd("2009-01-01"), lubridate::ymd("2009-04-01"), lubridate::ymd("2009-07-01"), lubridate::ymd("2009-10-01")))) %>% # pour glissement annuel
  dplyr::arrange(date)


# -------------------> sample to use for correlations' tables and graphs :
SAMPLE_TO_USE_CORR <- tableau_2_doc_travail_ga

# First occurence
WINDOW_SIZE <- 10 * 4 # ATTENTION, en nombre de trimestres !!!
first_correlations <- cor(SAMPLE_TO_USE_CORR[1:WINDOW_SIZE,] %>% dplyr::select(-date), method = "pearson")[, c("var1_PIB", "var4_PIB")]
rolling_correlations <- data.frame(first_correlations,
                                   dimension = row.names(first_correlations),
                                   date = rep(SAMPLE_TO_USE_CORR$date[[WINDOW_SIZE]], length(row.names(first_correlations))))

# Following occurences
for (i in (WINDOW_SIZE + 1):nrow(SAMPLE_TO_USE_CORR)) {
  correlations <- cor(SAMPLE_TO_USE_CORR[(i - WINDOW_SIZE + 1):i,] %>% dplyr::select(-date), method = "pearson")[, c("var1_PIB", "var4_PIB")]
  new_df <- data.frame(correlations,
                       dimension = row.names(correlations),
                       date = rep(SAMPLE_TO_USE_CORR$date[[i]], length(row.names(first_correlations))))
  rolling_correlations <- rolling_correlations %>%
    dplyr::bind_rows(new_df)
}

# tables
rolling_correlations %>%
  dplyr::group_by(dimension) %>%
  dplyr::summarise(var1_PIB = mean(var1_PIB), var4_PIB = mean(var4_PIB)) %>%
  dplyr::filter(!(dimension %in% c("var1_PIB", "var4_PIB")))

# Plotting evolution
color_palette_5_dimensions <- return_color_palette(color_list_name = "DGTresor_colors", nb_dimensions = 5)

ggplot(rolling_correlations %>% dplyr::filter(!(dimension %in% c("var1_PIB", "var4_PIB")))) +
  aes(x = date, y = var1_PIB, color = dimension) +
  geom_line() +
  labs(title = "Evolution dans le temps de la corrélation entre les climats et la variation trimestrielle du PIB",
       subtitle = paste("Estimation roulante réalisée sur fenêtres de", WINDOW_SIZE / 4, "ans sur la période 2001T1 - 2019T4 (exclusion crise 2008-9)"),
       caption = "Source : Insee, Banque de France (BdF) et S&P") +
  my_theme() +
  scale_y_continuous(breaks = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6),
                     labels = scales::percent_format(accuracy = 1L, decimal.mark = ",")) +
  scale_color_manual(labels = list("qt_pmi_composite" = "PMI composite trimestrialisé",
                                   "qt_bdf_global" = "Climat global BdF trimestrialisé",
                                   "qt_insee_global" = "Climat global Insee trimestrialisé",
                                   "insee_global_m3" = "Climat global au mois 3 de l'Insee",
                                   "lead_insee_global_m1" = "Climat global au mois 1 (T+1) de l'Insee"),
                     palette = color_palette_5_dimensions)


ggplot(rolling_correlations %>% dplyr::filter(!(dimension %in% c("var1_PIB", "var4_PIB")))) +
  aes(x = date, y = var4_PIB, color = dimension) +
  geom_line() +
  labs(title = "Evolution dans le temps de la corrélation entre les climats et le glissement annuel du PIB",
       subtitle = paste("Estimation roulante réalisée sur fenêtres de", WINDOW_SIZE / 4, "ans sur la période 2001T1 - 2019T4 (exclusion crise 2008-9)"),
       caption = "Source : Insee, Banque de France (BdF) et S&P") +
  my_theme() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L, decimal.mark = ",")) +
  scale_color_manual(labels = list("qt_pmi_composite" = "PMI composite trimestrialisé",
                                   "qt_bdf_global" = "Climat global BdF trimestrialisé",
                                   "qt_insee_global" = "Climat global Insee trimestrialisé",
                                   "insee_global_m3" = "Climat global au mois 3 de l'Insee",
                                   "lead_insee_global_m1" = "Climat global au mois 1 (T+1) de l'Insee"),
                     palette = color_palette_5_dimensions)


############ 4ème enseignement :
# La relation n'est pas du tout stable dans le temps quand on prend des fenêtres roulantes de 10 trimestres (WINDOW_SIZE = 10, i.e. 2 ans et demi)
# entre les climats et la variation trimestrielle du PIB : la corrélation est quasiment divisée par 2 (65% -> 35%)
# alors que la corrélation reste plus élevée pour le glissement annuel du PIB (85% -> 70%) même si elle n'est pas beaucoup plus stable.
# Avec une fenêtre de 10 ans (WINDOW_SIZE = 40), c'est beaucoup plus stable. On observe toutefois une lente décrue de la corrélation
# avec une cassure en 2019.
# intéressant d'étudier la cassure en 2019 plus tard => provient de la sortie de 2008-9 de l'échatillon

############ 5ème enseignement :
# Sur la période 1999T2-2019T4 : les corrélations sont tirées à la hausse par la période 2008-9 ; si on retire les trimestres 2008T4 et 2009T1,
# alors la corrélation à la variation trimestrielle du PIB se détériore nettement (65% -> 50%) alors qu'elle reste correcte pour le glissement
# annuel (90% -> 85%). Si on retire toute la période 2008T4-2009T4 : la corrélation à la v.t. s'améliore légèrement (+1/2 pt; sauf pour le PMI) et
# la corrélation au g.a. se détériore légèrement (-1/2 pt) mais s'améliore nettement pour le PMI (60%->80%)
# CCL : PMI a eu beaucoup de mal à prédire les extrêmes, dont crise 2008-9 => quand on la retire, tout va mieux
# => deux phénomènes : crise 2008-9 tire à la hausse les corrélations pour Insee et BdF mais détériore corrélation PMI
# TODO: + méthodes pour comparer différents échantillons avant 2020 et après 2020 (pour tester s'il y a une rupture) voire même avant / après 2019 (cf. enseignement 4)


## Partie 2 : régression du mémo --------------------------------------------
regression_data <- subset_data %>%
  dplyr::select(date, var1_PIB, var4_PIB, insee_global_m3, lead_insee_global_m1, bdf_global_m3, pmi_composite_m3) %>%
  dplyr::filter(date >= lubridate::ymd("2001-01-01") & date <= lubridate::ymd("2019-10-10"))
regression_data_vt <- regression_data %>%
  dplyr::filter(!(date %in% c(lubridate::ymd("2008-10-01"), lubridate::ymd("2009-01-01"))))
regression_data_ga <- regression_data_vt %>%
  dplyr::filter(!(date %in% c(lubridate::ymd("2009-04-01"), lubridate::ymd("2009-07-01"), lubridate::ymd("2009-10-01"))))
# MEMO 2019 002366 : période 1998T3-2018T4 MAIS dans nos données actuelles : PMI dispo pas avant 01/01/1999
# Compte-tenu des études sur la période : on va prendre 2001T1-2019T4 en retirant 2008T4 et 2009T1
#〖PIB〗_T= α+ β*I_T

### Régressions simples
# provide statistics
general_statistics <- regression_data_vt %>%
  dplyr::mutate(var4_PIB = case_when(                                                 # exclure les points aberrants pour le glissement annuel du PIB
    date %in% c(lubridate::ymd("2009-04-01"), lubridate::ymd("2009-07-01"), lubridate::ymd("2009-10-01")) ~ NA_real_,
    TRUE ~ var4_PIB
  )) %>%
  tidyr::pivot_longer(cols = colnames(regression_data_vt)[2:length(regression_data_vt)],
                      names_to = "dimension",
                      values_to = "value")
general_statistics <- general_statistics %>%
  dplyr::group_by(dimension) %>%
  dplyr::summarise(mean = mean(value, na.rm = TRUE),
                   standard_deviation = sd(value, na.rm = TRUE))
general_statistics

# summary of simple regressions for all indices without rolling windows
# NOTE: if we do not want a rolling window => set the window_size to the dataframe size
create_summary_for_several_simple_regressions(y_var = "var1_PIB",
                                              list_x_var = colnames(regression_data_vt)[!(colnames(regression_data_vt) %in% c("date", "var1_PIB", "var4_PIB"))],
                                              reg_data = regression_data_vt,
                                              window_size = nrow(regression_data_vt))

############ 6ème enseignement :
# Si on utilise le dataframe subset_data en filtrant bien la période (cf. ligne ci-dessous)
# clean_subset_data <- subset_data %>% dplyr::filter(date >= lubridate::ymd("2001-01-01") & date <= lubridate::ymd("2019-10-10")) %>% dplyr::filter(!(date %in% c(lubridate::ymd("2008-10-01"), lubridate::ymd("2009-01-01"))))
# On trouve bien que les variables lead_insee_global_m1 (puis insee_global_m3), pmi_composite_m3, et bdf_global_m3 sont bien les plus performantes

############ 7ème enseignement :
# Si on utilise le dataframe regression_data (i.e. sans enlever la crise 2008-9) alors les R-sq ajusté sont quasiment 2 fois meilleurs
# MAIS les RMSE et MAE sont nettement moins bons (de l'ordre de 20% plus mauvais)


# Régressions au seuil pour calcul heuristique - sans période roulante
#〖PIB〗_T= α+ β(I_T-seuil)
regression_data_vt_seuil <- regression_data_vt %>%
  dplyr::mutate(seuil_insee_global_m3 = insee_global_m3 - 100,
                seuil_lead_insee_global_m1 = lead_insee_global_m1 - 100,
                seuil_bdf_global_m3 = bdf_global_m3 - 100,
                seuil_pmi_composite_m3 = pmi_composite_m3 - 50) %>%
  dplyr::select(-c("insee_global_m3", "lead_insee_global_m1", "bdf_global_m3", "pmi_composite_m3"))

create_summary_for_several_simple_regressions(y_var = "var1_PIB",
                                              list_x_var = colnames(regression_data_vt_seuil)[!(colnames(regression_data_vt_seuil) %in% c("date", "var1_PIB", "var4_PIB"))],
                                              reg_data = regression_data_vt_seuil,
                                              window_size = nrow(regression_data_vt_seuil)) %>%
  dplyr::mutate(var_trim_pib_pour_indice_au_seuil = constant * 100,
                valeur_indice_pour_var_trim_pib_nulle = case_when(
                  stringr::str_detect(dimension, "pmi") ~ 50,
                  TRUE ~ 100
                )) %>%
  dplyr::mutate(valeur_indice_pour_var_trim_pib_nulle = round(valeur_indice_pour_var_trim_pib_nulle - (constant / coefficient), 0)) %>%
  select(dimension, var_trim_pib_pour_indice_au_seuil, valeur_indice_pour_var_trim_pib_nulle)
# On notera que les calculs de statistiques sont exactement équivalents avec les soldes classiques et les soldes en différence à leur solde; ce qui est normal


### Régressions avec estimation roulante réalisée sur fenêtres de 10 ans
REG_WINDOW_SIZE <- 10 * 4 # ATTENTION, en nombre de trimestres !!!
summary_of_rolling_regressions <- create_summary_for_several_simple_regressions(y_var = "var1_PIB",
                                                                                list_x_var = colnames(regression_data_vt)[!(colnames(regression_data_vt) %in% c("date", "var1_PIB", "var4_PIB"))],
                                                                                reg_data = regression_data_vt,
                                                                                window_size = REG_WINDOW_SIZE)
summary_of_rolling_regressions %>%
  dplyr::group_by(dimension) %>%
  dplyr::summarise(constant = mean(constant),
                   coefficient = mean(coefficient),
                   adjusted_r_squared = mean(adjusted_r_squared),
                   rmse = mean(rmse),
                   mae = mean(mae))

############ 8ème enseignement :
# Les résultats sont différents sur période roulante et sans période roulante.
# A l'exception du solde PMI, les RMSE et MAE sont moins bons sur période roulante que sur période complète (sauf mae pour BdF)
# Explication : reflète perte qualité régression au cours du temps ? ou juste 10 ans pas assez pour avoir une bonne puissance statistique ?
# TODO: investigate


# Plotting evolution
ggplot(summary_of_rolling_regressions) +
  aes(x = date, y = adjusted_r_squared, color = dimension) +
  geom_line() +
  labs(title = paste("Estimation roulante sur ", WINDOW_SIZE / 4, "ans des R2 ajustés"))

ggplot(summary_of_rolling_regressions) +
  aes(x = date, y = rmse * 100, color = dimension) +
  geom_line() +
  labs(title = paste("Estimation roulante sur ", WINDOW_SIZE / 4, "ans des RMSE"))

ggplot(summary_of_rolling_regressions) +
  aes(x = date, y = mae * 100, color = dimension) +
  geom_line() +
  labs(title = paste("Estimation roulante sur ", WINDOW_SIZE / 4, "ans des MAE"))


# Régressions au seuil pour calcul heuristique - avec période roulante
summary_of_rolling_regressions_seuil <- create_summary_for_several_simple_regressions(y_var = "var1_PIB",
                                                                                      list_x_var = colnames(regression_data_vt_seuil)[!(colnames(regression_data_vt_seuil) %in% c("date", "var1_PIB", "var4_PIB"))],
                                                                                      reg_data = regression_data_vt_seuil,
                                                                                      window_size = WINDOW_SIZE) %>%
  dplyr::mutate(var_trim_pib_pour_indice_au_seuil = constant * 100,
                valeur_indice_pour_var_trim_pib_nulle = case_when(
                  stringr::str_detect(dimension, "pmi") ~ 50,
                  TRUE ~ 100
                )) %>%
  dplyr::mutate(valeur_indice_pour_var_trim_pib_nulle = round(valeur_indice_pour_var_trim_pib_nulle - (constant / coefficient), 0)) %>%
  select(date, dimension, coefficient, var_trim_pib_pour_indice_au_seuil, valeur_indice_pour_var_trim_pib_nulle)

summary_of_rolling_regressions_seuil %>%
  dplyr::group_by(dimension) %>%
  dplyr::summarise(coefficient = mean(coefficient),
                   var_trim_pib_pour_indice_au_seuil = mean(var_trim_pib_pour_indice_au_seuil),
                   valeur_indice_pour_var_trim_pib_nulle = round(mean(valeur_indice_pour_var_trim_pib_nulle), 0))

# Plotting evolution
ggplot(summary_of_rolling_regressions_seuil) +
  aes(x = date, y = var_trim_pib_pour_indice_au_seuil, color = dimension) +
  geom_line() +
  labs(title = paste("Estimation roulante sur ", WINDOW_SIZE / 4, "ans de la croissance trimestrielle du PIB compatible \navec un climat constant à son seuil"))


rolling_regressions_coef <- summary_of_rolling_regressions_seuil %>%
  dplyr::mutate(coefficient = case_when(
    dimension %in% c("bdf_global_m3", "insee_global_m3", "lead_insee_global_m1") ~ coefficient * 4,
    dimension == "pmi_composite_m3" ~ coefficient * 2,
    TRUE ~ coefficient
  ))

ggplot(rolling_regressions_coef) +
  aes(x = date, y = coefficient * 100, color = dimension) +
  geom_line() +
  labs(title = paste("Croissance associée à une haussede 4 pts des climats Insee et BdF et de 2 pts du PMI
  (estimation roulante sur ", WINDOW_SIZE / 4, "ans)"))


############ 8ème enseignement :
# Pour la période 1999T1 - 2018T4, avec une estimation roulante sur 10 ans, j'arrive à reproduire les mêmes résultats mais avec
# un décalage de +/-0,1pt. A voir si ce n'est pas dû aux outliers (pour le calcul de la constante et du coefficient).
# Voir aussi si ça ne peut pas être à cause des données => aller chercher les données disponibles en juin 2019.
# REPONSE : non, ce n'est pas les outliers MAIS les soldes ; puisque je ne prends pas les mêmes !!!
# TODO: compare with and without 2008-9 crisis


## Partie 3 : Approche alternative à la régression -----------------------------
# quelle valeur de climat pour une croissance nulle du PIB et quelle croissance du PIB pour un climat à son seuil

## 1. Approche graphique:
full_sample_graphs <- main_indices_data %>%
  dplyr::filter(date >= lubridate::ymd("1999-01-01") & date <= lubridate::ymd("2019-10-01"))

sample_without_outliers_graphs_vt <- main_indices_data %>%
  dplyr::filter(date >= lubridate::ymd("2001-01-01") &
                  date <= lubridate::ymd("2019-10-01") &
                  !(date %in% c(lubridate::ymd("2008-10-01"), lubridate::ymd("2009-01-01"))))

sample_without_outliers_graphs_ga <- main_indices_data %>%
  dplyr::filter(date >= lubridate::ymd("2001-01-01") &
                  date <= lubridate::ymd("2019-10-01") &
                  !(date %in% c(lubridate::ymd("2008-10-01"), lubridate::ymd("2009-01-01"), lubridate::ymd("2009-04-01"), lubridate::ymd("2009-07-01"), lubridate::ymd("2009-10-01"))))

### a. variation trimestrielle
# PMI - full sample
ggplot(data = full_sample_graphs) +
  aes(x = qt_pmi_composite, y = var1_PIB, color = date, label = date) +
  geom_point() +
  geom_label() +
  geom_smooth(method = lm, color = "black") +
  labs(title = "Variation trimestrielle du PIB trimestriel français et PMI composite",
       subtitle = "Période : T2 1999 - T4 2019",
       caption = "Source : Insee, Markit",
       x = "PMI composite trimestrialisé",
       y = "Variation trimestrielle du PIB"
  ) +
  my_theme() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = c(-0.02, -0.015, -0.01, -0.005, 0, 0.005, 0.01, 0.015, 0.02),
                     labels = scales::percent_format(accuracy = 0.1, decimal.mark = ","))

# PMI - sample without outliers
ggplot(data = sample_without_outliers_graphs_vt) +
  aes(x = qt_pmi_composite, y = var1_PIB, color = date, label = date) +
  geom_point() +
  geom_label() +
  geom_smooth(method = lm, color = "black") +
  labs(title = "Variation trimestrielle du PIB trimestriel français et PMI composite",
       subtitle = "Période : T1 2001 - T4 2019 (avec exclusion de la crise 2008-9)",
       caption = "Source : Insee, Markit",
       x = "PMI composite trimestrialisé",
       y = "Variation trimestrielle du PIB"
  ) +
  my_theme() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = c(-0.006, -0.004, -0.002, 0, 0.002, 0.004, 0.006, 0.008, 0.01),
                     labels = scales::percent_format(accuracy = 0.1, decimal.mark = ","))


# Insee lead M1 - full sample
ggplot(data = full_sample_graphs) +
  aes(x = lead_insee_global_m1, y = var1_PIB, color = date, label = date) +
  geom_point() +
  geom_label() +
  geom_smooth(method = lm, color = "black") +
  labs(title = "Variation trimestrielle du PIB trimestriel français et climat Insee",
       subtitle = "Période : T2 1999 - T4 2019",
       caption = "Source : Insee",
       x = "Climat Insee au mois 1 du trimestre T+1",
       y = "Variation trimestrielle du PIB au trimestre T"
  ) +
  my_theme() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = c(-0.02, -0.015, -0.01, -0.005, 0, 0.005, 0.01, 0.015, 0.02),
                     labels = scales::percent_format(accuracy = 0.1, decimal.mark = ","))

# Insee lead M1 - sample without outliers
ggplot(data = sample_without_outliers_graphs_vt) +
  aes(x = lead_insee_global_m1, y = var1_PIB, color = date, label = date) +
  geom_point() +
  geom_label() +
  geom_smooth(method = lm, color = "black") +
  labs(title = "Variation trimestrielle du PIB trimestriel français et climat Insee",
       subtitle = "Période : T1 2001 - T4 2019 (avec exclusion de la crise 2008-9)",
       caption = "Source : Insee",
       x = "Climat Insee au mois 1 du trimestre T+1",
       y = "Variation trimestrielle du PIB au trimestre T"
  ) +
  my_theme() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = c(-0.006, -0.004, -0.002, 0, 0.002, 0.004, 0.006, 0.008, 0.01),
                     labels = scales::percent_format(accuracy = 0.1, decimal.mark = ","))

# Climat trim BdF - full sample
ggplot(data = full_sample_graphs) +
  aes(x = qt_bdf_global, y = var1_PIB, color = date, label = date) +
  geom_point() +
  geom_label() +
  geom_smooth(method = lm, color = "black") +
  labs(title = "Variation trimestrielle du PIB trimestriel français et climat Banque de France",
       subtitle = "Période : T2 1999 - T4 2019",
       caption = "Source : Insee, Banque de France",
       x = "Climat Banque de France trimestrialisé",
       y = "Variation trimestrielle du PIB"
  ) +
  my_theme() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = c(-0.02, -0.015, -0.01, -0.005, 0, 0.005, 0.01, 0.015, 0.02),
                     labels = scales::percent_format(accuracy = 0.1, decimal.mark = ","))

# Climat trim BdF - sample without outliers
ggplot(data = sample_without_outliers_graphs_vt) +
  aes(x = qt_bdf_global, y = var1_PIB, color = date, label = date) +
  geom_point() +
  geom_label() +
  geom_smooth(method = lm, color = "black") +
  labs(title = "Variation trimestrielle du PIB trimestriel français et climat Banque de France",
       subtitle = "Période : T1 2001 - T4 2019 (avec exclusion de la crise 2008-9)",
       caption = "Source : Insee, Banque de France",
       x = "Climat Banque de France trimestrialisé",
       y = "Variation trimestrielle du PIB"
  ) +
  my_theme() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = c(-0.006, -0.004, -0.002, 0, 0.002, 0.004, 0.006, 0.008, 0.01),
                     labels = scales::percent_format(accuracy = 0.1, decimal.mark = ","))


### a. glissement annuel
# PMI - full sample
ggplot(data = full_sample_graphs) +
  aes(x = qt_pmi_composite, y = var4_PIB, color = date, label = date) +
  geom_point() +
  geom_label() +
  geom_smooth(method = lm, color = "black") +
  labs(title = "Glissement annuel du PIB trimestriel français et PMI composite",
       subtitle = "Période : T2 1999 - T4 2019",
       caption = "Source : Insee, Markit",
       x = "PMI composite trimestrialisé",
       y = "Glissement annuel du PIB"
  ) +
  my_theme() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = c(-0.04, -0.03, -0.02, -0.01, 0, 0.01, 0.02, 0.03, 0.04, 0.05),
                     labels = scales::percent_format(accuracy = 0.1, decimal.mark = ","))

# PMI - sample without outliers
ggplot(data = sample_without_outliers_graphs_ga) +
  aes(x = qt_pmi_composite, y = var4_PIB, color = date, label = date) +
  geom_point() +
  geom_label() +
  geom_smooth(method = lm, color = "black") +
  labs(title = "Glissement annuel du PIB trimestriel français et PMI composite",
       subtitle = "Période : T1 2001 - T4 2019 (avec exclusion de la crise 2008-9)",
       caption = "Source : Insee, Markit",
       x = "PMI composite trimestrialisé",
       y = "Glissement annuel du PIB"
  ) +
  my_theme() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = c(-0.02, -0.01, 0, 0.01, 0.02, 0.03, 0.04, 0.05),
                     labels = scales::percent_format(accuracy = 0.1, decimal.mark = ","))

# Insee lead M1 - full sample
ggplot(data = full_sample_graphs) +
  aes(x = lead_insee_global_m1, y = var4_PIB, color = date, label = date) +
  geom_point() +
  geom_label() +
  geom_smooth(method = lm, color = "black") +
  labs(title = "Glissement annuel du PIB trimestriel français et climat Insee",
       subtitle = "Période : T2 1999 - T4 2019",
       caption = "Source : Insee",
       x = "Climat Insee au mois 1 du trimestre T+1",
       y = "Glissement annuel du PIB au trimestre T"
  ) +
  my_theme() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = c(-0.04, -0.03, -0.02, -0.01, 0, 0.01, 0.02, 0.03, 0.04, 0.05),
                     labels = scales::percent_format(accuracy = 0.1, decimal.mark = ","))

# Insee lead M1 - sample without outliers
ggplot(data = sample_without_outliers_graphs_ga) +
  aes(x = lead_insee_global_m1, y = var4_PIB, color = date, label = date) +
  geom_point() +
  geom_label() +
  geom_smooth(method = lm, color = "black") +
  labs(title = "Glissement annuel du PIB trimestriel français et climat Insee",
       subtitle = "Période : T2 1999 - T4 2019 (avec exclusion de la crise 2008-9)",
       caption = "Source : Insee",
       x = "Climat Insee au mois 1 du trimestre T+1",
       y = "Glissement annuel du PIB au trimestre T"
  ) +
  my_theme() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = c(-0.02, -0.01, 0, 0.01, 0.02, 0.03, 0.04, 0.05),
                     labels = scales::percent_format(accuracy = 0.1, decimal.mark = ","))

# Insee trim - full sample
ggplot(data = full_sample_graphs) +
  aes(x = qt_insee_global, y = var4_PIB, color = date, label = date) +
  geom_point() +
  geom_label() +
  geom_smooth(method = lm, color = "black") +
  labs(title = "Glissement annuel du PIB trimestriel français et climat Insee",
       subtitle = "Période : T2 1999 - T4 2019",
       caption = "Source : Insee",
       x = "Climat Insee trimestrialisé",
       y = "Glissement annuel du PIB"
  ) +
  my_theme() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = c(-0.04, -0.03, -0.02, -0.01, 0, 0.01, 0.02, 0.03, 0.04, 0.05),
                     labels = scales::percent_format(accuracy = 0.1, decimal.mark = ","))

# Insee trim - sample without outliers
ggplot(data = sample_without_outliers_graphs_ga) +
  aes(x = qt_insee_global, y = var4_PIB, color = date, label = date) +
  geom_point() +
  geom_label() +
  geom_smooth(method = lm, color = "black") +
  labs(title = "Glissement annuel du PIB trimestriel français et climat Insee",
       subtitle = "Période : T2 1999 - T4 2019 (avec exclusion de la crise 2008-9)",
       caption = "Source : Insee",
       x = "Climat Insee trimestrialisé",
       y = "Glissement annuel du PIB"
  ) +
  my_theme() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = c(-0.02, -0.01, 0, 0.01, 0.02, 0.03, 0.04, 0.05),
                     labels = scales::percent_format(accuracy = 0.1, decimal.mark = ","))

# Climat trim BdF - full sample
ggplot(data = full_sample_graphs) +
  aes(x = qt_bdf_global, y = var4_PIB, color = date, label = date) +
  geom_point() +
  geom_label() +
  geom_smooth(method = lm, color = "black") +
  labs(title = "Glissement annuel du PIB trimestriel français et climat Banque de France",
       subtitle = "Période : T2 1999 - T4 2019",
       caption = "Source : Insee, Banque de France",
       x = "Climat Banque de France trimestrialisé",
       y = "Glissement annuel du PIB"
  ) +
  my_theme() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = c(-0.04, -0.03, -0.02, -0.01, 0, 0.01, 0.02, 0.03, 0.04, 0.05),
                     labels = scales::percent_format(accuracy = 0.1, decimal.mark = ","))

# Climat trim BdF - sample without outliers
ggplot(data = sample_without_outliers_graphs_ga) +
  aes(x = qt_bdf_global, y = var4_PIB, color = date, label = date) +
  geom_point() +
  geom_label() +
  geom_smooth(method = lm, color = "black") +
  labs(title = "Glissement annuel du PIB trimestriel français et climat Banque de France",
       subtitle = "Période : T1 2001 - T4 2019 (avec exclusion de la crise 2008-9)",
       caption = "Source : Insee, Banque de France",
       x = "Climat Banque de France trimestrialisé",
       y = "Glissement annuel du PIB"
  ) +
  my_theme() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = c(-0.02, -0.01, 0, 0.01, 0.02, 0.03, 0.04, 0.05),
                     labels = scales::percent_format(accuracy = 0.1, decimal.mark = ","))


## 2. régression logit ? // Quelle variation du PIB la plus probable pour un PMI à 50 et un climat à 100 ? // climat le plus probable pour une croissance à 0%
## Estimation changement de régime ? avant-après 2008-9 voire 2020-2021 ?


# TODO : check sensibilité de la croissance au climat : regarder non-linéarité