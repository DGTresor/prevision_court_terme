# Title     : Project initializator
# Objective : setting up the architecture and the dependencies to make the project operational
# Created by: lphung
# Created on: 17/08/2023

# this project has been developed with R.4.2.2
message("Ce projet fonctionne avec la version R 4.2.2.; il est également compatible avec la version R.4.2.3.")

# create the project architecture
dir.create("./output")
dir.create("./data") # Note: this folder will not be useful for individuals external to the French Treasury


# package management
## package manager: https://cran.r-project.org/web/packages/renv/vignettes/renv.html; https://rstudio.github.io/renv/articles/renv.html
## ensure that the .Rprofile is read
source(".Rprofile")

## download the appropriate versions of packages to ensure the proper functioning of the code
renv::restore()

