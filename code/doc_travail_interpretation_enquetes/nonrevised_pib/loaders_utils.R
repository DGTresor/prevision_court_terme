# Title     : meta-functions that call the loaders to construct the production series from the national accounts
# Created by: lphung
# Created on: 08/05/2023

# packages -------------------------------------------------------------------------------------------------------------
library(stringr)
library(dplyr)
library(tidyr)
source("./loaders.R", encoding = "utf-8", chdir = TRUE)
source("../../loader_fundamentals.R", encoding = "utf-8", chdir = TRUE)

# folders --------------------------------------------------------------------------------------------------------------
NATIONAL_ACCOUNTING_DATA_FOLDER <- "T:/SPMAE_Public/Prev_Public/CNAT/ArchivesCTrim"
PIB_FILE_NAME <- "erevolch.csv"
PIB_DIMENSIONS <- list("default" = c("revised_pib" = "TD.PIB_7CH")) # -> file 2019T4PE

# The file we need is "erevolch", according to the years, it can be in .RData, .xls or .cvs.
# it contains the PIB.
# We use data from base2005 onward; the exclusion of data previous to base2005 is dealt in the function: get_national_accounting_data_files()
