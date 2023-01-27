# libraries ------------------------------------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(ggtext)
library(grDevices)

# define my theme ------------------------------------------------------------------------------------------------------
# define the default theme
my_theme <- function() {
  theme(plot.title = element_text(hjust = 0.5,                         # center the plot title
                                  size = 25,  # 18                         # define the title size
                                  # face = "bold",
                                  family = "sans",                     # define the police of the title: "sans" = "Arial"
                                  color = rgb(red = 0, green = 0, blue = 128 / 255)),
        plot.subtitle = element_text(family = "sans",
                                     size = 15, # 12
                                     color = "black"),
        legend.title = element_blank(),
        legend.text = element_text(family = "sans",                    # define the police of the legend: "sans" = "Arial"
                                   size = 15),
        legend.key = element_rect(fill = NA),                          # remove the grey background behind legend symbols
        plot.caption = element_text(family = "sans",
                                    size = 15),
        # axis.title = element_blank(),                                  # remove the titles of the axis
        axis.title = element_text(family = "sans",                       # OR - give it the same parameters as the other components
                                 size = 15,
                                 color = "black"),
        axis.text = element_text(family = "sans",
                                 size = 15,
                                 color = "black"),
        panel.background = element_rect(fill = "white"),               # remove the background color
        panel.border = element_rect(size = 1,
                                    color = "black",
                                    fill = NA),                        # add a frame to the graph
        panel.grid.major = element_line(size = 0.5,
                                        color = "gray"),
        panel.grid.minor = element_blank()
  )
}

# define the default design for the geom elements
update_geom_defaults("line", list(size = 1.1))
update_geom_defaults("point", list(size = 2, shape = 15))

# define colors --------------------------------------------------------------------------------------------------------

return_color_palette <- function(color_list_name, nb_dimensions) {
  color_list <- return_color_list(color_list_name)
  if (as.numeric(nb_dimensions) >= length(color_list)) {
    return(grDevices::colorRampPalette(color_list))
  } else {
    return(grDevices::colorRampPalette(color_list[1:as.numeric(nb_dimensions)]))
  }
}

return_color_list <- function(color_list_name) {
  if (color_list_name == "DGTresor_colors") {

    return(c(rgb(red = 0, green = 0, blue = 128 / 255),                  # dark blue
             rgb(red = 255 / 255, green = 102 / 255, blue = 0),          # orange
             rgb(red = 255 / 255, green = 204 / 255, blue = 0),          # yellow
             rgb(red = 0, green = 176 / 255, blue = 240 / 255),          # light blue
             rgb(red = 0, green = 176 / 255, blue = 80 / 255)))          # green
  }
  else if (color_list_name == "DGTresor_blue_black_gray") {

    return(c(rgb(red = 200 / 255, green = 200 / 255, blue = 200 / 255),           # gray
             rgb(red = 0, green = 0, blue = 0),                                   # black
             rgb(red = 0, green = 0, blue = 128 / 255)))                          # dark blue
  }
  else if (color_list_name == "DGTresor_five_indus_sectors") {

    return(c(rgb(red = 0, green = 176 / 255, blue = 80 / 255),     # green for agriculture
             rgb(red = 0, green = 0, blue = 128 / 255),            # dark blue
             rgb(red = 255 / 255, green = 102 / 255, blue = 0),    # orange
             rgb(red = 255 / 255, green = 204 / 255, blue = 0),    # yellow
             rgb(red = 0, green = 176 / 255, blue = 240 / 255)))   # light blue
  }
  else if (color_list_name == "DGTresor_eight_indus_sectors") {

    return(c(rgb(red = 0, green = 176 / 255, blue = 80 / 255),             # green for agriculture
             rgb(red = 255 / 255, green = 102 / 255, blue = 0),            # orange
             rgb(red = 255 / 255, green = 204 / 255, blue = 0),            # yellow
             rgb(red = 255 / 255, green = 0, blue = 0),                    # red
             rgb(red = 0, green = 0, blue = 128 / 255),                    # dark blue
             rgb(red = 0, green = 176 / 255, blue = 240 / 255),            # light blue
             rgb(red = 181 / 255, green = 202 / 255, blue = 146 / 255),    # light green
             rgb(red = 128 / 255, green = 128 / 255, blue = 128 / 255)))   # grey
  }
  else if (color_list_name == "TEST") {

    return(c(rgb(red = 0, green = 0, blue = 128 / 255),                  # dark blue
             rgb(red = 255 / 255, green = 102 / 255, blue = 0),          # orange
             rgb(red = 255 / 255, green = 204 / 255, blue = 0),          # yellow
             rgb(red = 0, green = 176 / 255, blue = 240 / 255)))         # light blue
  }
  else {
    stop("Please provide a valid color_list_name argument. Check the various color lists in the ./code/graph_helpers.R file.")
  }
}
