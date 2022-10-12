# Title     : helpers functions to be used in loaders
# Created by: lphung
# Created on: 12/09/2022

# packages -------------------------------------------------------------------------------------------------------------



# functions ------------------------------------------------------------------------------------------------------------

check_data_correction <- function(data_correction) {
  if (!(data_correction %in% c("bruts", "CJO-CVS"))) {
    stop("Please choose either \"bruts\" or \"CJO-CVS\" as data_correction argument for the loaders.")
  }
}