# Objective : reproduce the French Treasury graphic charter
# Created by: lphung
# Created on: 16/08/2023

# libraries ------------------------------------------------------------------------------------------------------------

library(ggplot2)
library(grDevices)


# constants ------------------------------------------------------------------------------------------------------------

dgtresor_color_list <- list("FR_derouleur" = c(grDevices::rgb(red = 0, green = 0, blue = 128 / 255),                  # dark blue
                                               grDevices::rgb(red = 255 / 255, green = 102 / 255, blue = 0),          # orange
                                               grDevices::rgb(red = 255 / 255, green = 204 / 255, blue = 0),          # yellow
                                               grDevices::rgb(red = 0, green = 176 / 255, blue = 240 / 255),          # light blue
                                               grDevices::rgb(red = 0, green = 176 / 255, blue = 80 / 255)),          # green
                            "blue_black_gray" = c(grDevices::rgb(red = 200 / 255, green = 200 / 255, blue = 200 / 255),           # gray
                                                  grDevices::rgb(red = 0, green = 0, blue = 0),                                   # black
                                                  grDevices::rgb(red = 0, green = 0, blue = 128 / 255)),                          # dark blue
                            "five_sectors" = c(grDevices::rgb(red = 0, green = 176 / 255, blue = 80 / 255),     # green for agriculture (always first in alphabetical ordrer)
                                               grDevices::rgb(red = 0, green = 0, blue = 128 / 255),            # dark blue for raffi
                                               grDevices::rgb(red = 255 / 255, green = 102 / 255, blue = 0),    # orange for
                                               grDevices::rgb(red = 255 / 255, green = 204 / 255, blue = 0),    # yellow
                                               grDevices::rgb(red = 0, green = 176 / 255, blue = 240 / 255)),   # light blue
                            "eight_sectors" = c(grDevices::rgb(red = 0, green = 176 / 255, blue = 80 / 255),             # green for agriculture
                                                grDevices::rgb(red = 255 / 255, green = 102 / 255, blue = 0),            # orange
                                                grDevices::rgb(red = 255 / 255, green = 204 / 255, blue = 0),            # yellow
                                                grDevices::rgb(red = 255 / 255, green = 0, blue = 0),                    # red
                                                grDevices::rgb(red = 0, green = 0, blue = 128 / 255),                    # dark blue
                                                grDevices::rgb(red = 0, green = 176 / 255, blue = 240 / 255),            # light blue
                                                grDevices::rgb(red = 181 / 255, green = 202 / 255, blue = 146 / 255),    # light green
                                                grDevices::rgb(red = 128 / 255, green = 128 / 255, blue = 128 / 255))    # grey
)

# functions ------------------------------------------------------------------------------------------------------------

dgtresor_theme <- function() {
  # define the default design for the geom elements
  ggplot2::update_geom_defaults("line", list(size = 1.1))
  ggplot2::update_geom_defaults("point", list(size = 2, shape = 15))

  # define the theme
  ggplot2::theme(plot.title = element_text(hjust = 0.5,                         # center the plot title
                                           size = 18,                           # define the title size
                                           # face = "bold",
                                           family = "sans",                     # define the police of the title: "sans" = "Arial"
                                           color = rgb(red = 0, green = 0, blue = 128 / 255)),
                 plot.subtitle = element_text(family = "sans",
                                              size = 12,
                                              color = "black"),
                 legend.title = element_blank(),                                # remove the title of the legend
                 legend.text = element_text(family = "sans",                    # define the police of the legend: "sans" = "Arial"
                                            size = 12),
                 legend.key = element_rect(fill = NA),                          # remove the grey background behind legend symbols
                 plot.caption = element_text(family = "sans",
                                             size = 12),
                 axis.title = element_blank(),                                  # remove the titles of the axis
                 axis.text = element_text(family = "sans",
                                          size = 12,
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


color_palette_for <- function(nb_dimensions, color_list_name = names(dgtresor_color_list)) {
  color_list_name <- match.arg(color_list_name)
  vector_of_colors <- dgtresor_color_list[[color_list_name]]

  if (as.numeric(nb_dimensions) >= length(vector_of_colors)) {
    return(grDevices::colorRampPalette(vector_of_colors))
  } else {
    return(grDevices::colorRampPalette(vector_of_colors[1:as.numeric(nb_dimensions)]))
  }
}