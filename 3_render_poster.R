##########==========##########==========##########==========##########==========

## SET UP ======================================================================

## meta-information
## Author: Josh M.
## Creation: 2021-02-07
## Description: render project poster visualization for the crowded scatterplots
  ## project.

## environment setup
remove(list = objects())
options(width = 80, scipen = 2, digits = 6)
library(readxl)
library(tidyverse)

## READ IN DATA ================================================================

poster_dims <- read_xlsx("A_Inputs/poster_dims.xlsx")

## RENDER PLOT LAYOUT ELEMENTS =================================================

## initialize plot
crowded_scatterplot <- ggplot() +
  coord_fixed(xlim = c(0, 36), ylim = c(0, 24), ratio = 1, expand = FALSE) +
  theme_void()

## render dividers
crowded_scatterplot <- crowded_scatterplot +
  geom_segment(
    data = filter(poster_dims, str_detect(element, "divider_v")),
    mapping = aes(
      x = (x_min + x_max) / 2,
      xend = (x_min + x_max) / 2,
      y = y_min,
      yend = y_max
      ),
    size = 2, color = "gray80"
    ) +
  geom_segment(
    data = filter(poster_dims, str_detect(element, "divider_h")),
    mapping = aes(
      x = x_min,
      xend = x_max,
      y = (y_min + y_max) / 2,
      yend = (y_min + y_max) / 2
      ),
    size = 2, color = "gray80"
    ) +
  geom_point(
    data = filter(poster_dims, str_detect(element, "divider_v")),
    mapping = aes(
      x = (x_min + x_max) / 2,
      y = (y_min + y_max) / 2
      ),
    color = "white", size = 2^4, shape = 15
    )

## poster explanation
explanation <- c(
  "A scatterplot visualizes the relationship between two numeric variables. ",
  "One variable provides the x-axis coordinate for a shape (typically a dot) ",
  "and the other variable provides the y-axis coordinate.  However, ",
  "scatterplots are ineffective if many shapes have the same coordinates ",
  "because most the shapes cannot be seen beneath the shape on top.  ",
  "This poster discusses four solutions to the crowded scatterplot problem, ",
  "using a population map of the United States as an example.",
  "",
  "The US population is a crowded scatterplot in that a large segment of the ",
  "population lives in high population density urban areas that are too small ",
  "to be visible on a typical map.  For example, New York City has over a",
  "million more residents than Idaho, Montana, Wyoming, North Dakota, South ",
  "Dakota, and Nebraska combined. However, these states occupy 1,200 times ",
  "more surface area on a US map.  If you place a dot on a US map for every ",
  "resident, New Yorkers would be much less visible than the ",
  "residents of the aforementioned states.  Even within those states, the ",
  "same challenges occurs.  For example, the city of Cheyenne holds nearly ",
  "10% of the population of Wyoming.  However, the city occupies ",
  "only 0.02% of the surface area of Wyoming, making these residents less ",
  "visible than other Wyomingites.",
  "",
  "This R code underlying this project is on GitHub at: ",
  "https://github.com/sjoshuam/crowded_scatterplot"
  )
explanation <- tapply(explanation, cumsum(!nzchar(explanation)), paste,
  collapse = "") %>%
  str_wrap(width = 72) %>%
  paste(collapse = "\n\n")

## render poster explanation panel
crowded_scatterplot <- crowded_scatterplot +
  geom_text(
    data = filter(poster_dims, element == "plot_12"),
    mapping = aes(x = x_min, y = y_max - 0.5),
    hjust = 0, vjust = 1, size = 7, label = explanation,
    color = "black", fontface ="bold"
  ) + geom_text(
    data = filter(poster_dims, element == "title_12"),
    mapping = aes(x = x_min, y = y_max),
    hjust = 0, vjust = 1, size = 10,
    label = "Solutions To The Crowded Scatterplot Problem",
    color = "black", fontface ="bold"
      )

## WRITE POSTER TO DISK =================================================

pdf(file = "C_Outputs/crowded_scatterplot_poster.pdf", width = 36, height = 24)
crowded_scatterplot
graphics.off()

png(file = "C_Outputs/crowded_scatterplot.png", width = 36, height = 24,
  units = "in", res = 72)
crowded_scatterplot
graphics.off()

##########==========##########==========##########==========##########==========