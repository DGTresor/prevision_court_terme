# Title     : Project initializator
# Objective : setting up the architecture and the dependencies to make the project operational
# Created by: lphung
# Created on: 17/08/2023

# please specify if you are a user within or without the French Treasury
IN_FRENCH_TREASURY <- TRUE

# this project has been developed with R.4.0.2.
message("Ce projet a été développé avec la version R 4.0.2.")

# create the project architecture
dir.create("./output")
if (IN_FRENCH_TREASURY) {
  # Note: this folder will not be usefull for individuals external to the French Treasury
  dir.create("./data")
}

# package management
## package manager: https://cran.r-project.org/web/packages/renv/vignettes/renv.html; https://rstudio.github.io/renv/articles/renv.html
## load package manager, if needed
if(!("renv" %in% rownames(installed.packages()))){
  install.packages("renv")
}

## download the appropriate versions of packages to ensure the proper functioning of the code
renv::restore()

