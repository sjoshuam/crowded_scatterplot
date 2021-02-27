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
library(mapproj)
library(tidyverse)

## READ IN DATA ================================================================

poster_dim <- read_xlsx("A_Inputs/poster_dims.xlsx") %>%
  as.data.frame()
rownames(poster_dim) <- poster_dim$element

proximity_solution <- as_tibble(
  readRDS("B_Intermediates/proximity_solution.RData"))
spacing_solution <- as_tibble(readRDS("B_Intermediates/spacing_solution.RData"))
cluster_solution <- readRDS("B_Intermediates/cluster_solution.RData")
population <- readRDS("B_Intermediates/population.RData")
pop_points <- readRDS("B_Intermediates/pop_points.RData")


conus_map   <- map_data("state") %>%
  as_tibble() %>%
  rename(lon = long) %>%
  mutate(region = str_to_title(region)) %>%
  mutate(state = state.abb[match(region, state.name)])

## RENDER TOP LEFT PANEL AND BASIC SKELETON ====================================

## establish basic palette
CircleMath <- function(num, full_circle = 12) {
  too_much <- num %/% full_circle
  num <- num - (full_circle * too_much)
  return(num)
  }
hue_point <- 4 #sample(0:11, 1)
standard_color <- list(
  # "background_dark"  = hsv(h = CircleMath(hue_point + 6) / 12, s = 0.7, v= 0.3),
  # "background_mid"   = hsv(h = CircleMath(hue_point + 6) / 12, s = 0.5, v= 0.5),
  # "background_light" = hsv(h = CircleMath(hue_point + 6) / 12, s = 0.1, v= 0.9),
  "background_dark"  = hsv(h = 0, s = 0, v= 0.0),
  "background_mid"   = hsv(h = 0, s = 0, v= 0.5),
  "background_light" = hsv(h = 0, s = 0, v= 1.0),
  "midground_dark"   = hsv(h = 06 / 12, s = 0.7, v= 0.3),
  "midground_mid"    = hsv(h = 06 / 12, s = 0.5, v= 0.5),
  "midground_light"  = hsv(h = 06 / 12, s = 0.1, v= 0.9),
  "foreground_dark"  = hsv(h = 08 / 12, s = 0.7, v= 0.3),
  "foreground_mid"   = hsv(h = 08 / 12, s = 0.5, v= 0.5),
  "foreground_light" = hsv(h = 08 / 12, s = 0.1, v= 0.9)
)

## initialize plot
crowded_scatterplot <- ggplot() +
  coord_fixed(xlim = c(0, 36), ylim = c(0, 24), ratio = 1, expand = FALSE) +
  theme_void() + 
  theme(
    panel.background = element_rect(fill = standard_color$background_light)
    )

## render dividers
crowded_scatterplot <- crowded_scatterplot +
  geom_segment(
    data = filter(poster_dim, str_detect(element, "divider_v")),
    mapping = aes(
      x = (x_min + x_max) / 2,
      xend = (x_min + x_max) / 2,
      y = y_min,
      yend = y_max
      ),
    size = 2, color = standard_color$background_dark
    ) +
  geom_segment(
    data = filter(poster_dim, str_detect(element, "divider_h")),
    mapping = aes(
      x = x_min,
      xend = x_max,
      y = (y_min + y_max) / 2,
      yend = (y_min + y_max) / 2
      ),
    size = 2, color = standard_color$background_dark
    ) +
  geom_point(
    data = filter(poster_dim, str_detect(element, "divider_v")),
    mapping = aes(
      x = (x_min + x_max) / 2,
      y = (y_min + y_max) / 2
      ),
    color = standard_color$background_light, size = 2^4, shape = 15
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
  "to be visible on a typical map.  For example, New York City has over a ",
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
    data = filter(poster_dim, element == "plot_12"),
    mapping = aes(x = (x_min + x_max) / 2, y = y_max - 0.5),
    vjust = 1, size = 7, label = explanation,
    color = standard_color$background_dark, fontface ="bold"
  ) + geom_text(
    data = filter(poster_dim, element == "title_12"),
    mapping = aes(x = (x_min + x_max) / 2, y = y_max),
    vjust = 1, size = 12,
    label = "Solutions To The Crowded Scatterplot Problem",
    color = standard_color$background_dark, fontface ="bold"
      )

## project geographic coordinates
ProjectAlbersConus <- function(x,
  scale_factors = c(x_min = -0.332, y_min = -1.528, scale = 0.71)
  ) {
  
  ## project map
  albers <- mapproject(x = x$lon, y = x$lat, projection = "albers",
    orientation = c(90, 0, -98.583333), parameters = c(24.520833, 49.384472))
  
  ## calculate scale factors
  new_scale_factors <-c(
    x_min = min(albers$x),
    y_min = min(albers$y),
    scale = max(c(
      diff(range(albers$x)),
      diff(range(albers$y))
      ))
    )
   
    print("Calculated scale factors:")
    print(as.character(new_scale_factors))   
    
  if (is.null(new_scale_factors)) {
    scale_factors <- new_scale_factors
    }
  
  ## center projected coordinates and scale
  x$lon_albers  <- albers$x  - scale_factors["x_min"]
  x$lat_albers  <- albers$y  - scale_factors["y_min"]
  
  x$lon_albers <- x$lon_albers / scale_factors["scale"]
  x$lat_albers <- x$lat_albers / scale_factors["scale"]
  
  ## express results
  x
}

population <- ProjectAlbersConus(population)
pop_points <- ProjectAlbersConus(pop_points)
conus_map <- ProjectAlbersConus(conus_map)

## RENDER US MAP OUTLINES FOR EACH PANEL =======================================

PanelMutation <- function(the_data = conus_map, the_dims = poster_dim, panel) {
  the_data <- the_data %>%
    mutate(
      panel_x = lon_albers * the_dims[panel, "x_size"] +
        the_dims[panel, "x_min"],
      panel_y = lat_albers * the_dims[panel, "x_size"] +
        the_dims[panel, "y_min"]
      )
  return(the_data)
  }

## middle top
conus_map <- PanelMutation(panel = "plot_22")
crowded_scatterplot <- crowded_scatterplot +
  geom_polygon(
    data = conus_map,
    mapping = aes(x = panel_x, y = panel_y, group = group),
    fill = standard_color$background_light,
    color = standard_color$background_mid
    )

## right top

## left bottom
conus_map <- PanelMutation(panel = "plot_11")
crowded_scatterplot <- crowded_scatterplot +
  geom_polygon(
    data = conus_map,
    mapping = aes(x = panel_x, y = panel_y, group = group),
    fill = standard_color$background_light,
    color = standard_color$background_mid
    )

## middle bottom
conus_map <- PanelMutation(panel = "plot_21")
crowded_scatterplot <- crowded_scatterplot +
  geom_polygon(
    data = conus_map,
    mapping = aes(x = panel_x, y = panel_y, group = group),
    fill = standard_color$background_light,
    color = standard_color$background_mid
    )

## left bottom
conus_map <- PanelMutation(panel = "plot_31")
crowded_scatterplot <- crowded_scatterplot +
  geom_polygon(
    data = conus_map,
    mapping = aes(x = panel_x, y = panel_y, group = group),
    fill = standard_color$background_light,
    color = standard_color$background_mid
    )

## RENDER MIDDLE TOP PANEL (GEOGRAPHIC POPULATION DISTRIBUTION) ================

## add plot title
crowded_scatterplot <- crowded_scatterplot +
  geom_text(
    data = tibble(NA),
    x = (poster_dim["title_22", "x_min"] + poster_dim["title_22", "x_max"]) / 2,
    y = (poster_dim["title_22", "y_min"] + poster_dim["title_22", "y_max"]) / 2,
    label = "Geographic Distribution of the US Population",
    fontface = "bold", size = 12, color = standard_color$background_dark,
    vjust = 0
    )

## render population
crowded_scatterplot <- crowded_scatterplot +
  geom_point(
    data = PanelMutation(the_data = population, panel = "plot_22"),
    mapping = aes(x = panel_x, y = panel_y),
    size = 0.1,
    color = standard_color$midground_mid
    )

## RENDER RIGHT TOP PANEL (HIDDEN POINTS) ==============================

## add plot title
crowded_scatterplot <- crowded_scatterplot +
  geom_text(
    data = filter(poster_dim, element == "title_32"),
    mapping = aes(x = (x_min + x_max) / 2, y = (y_min + y_max) / 2),
    label = "Hidden Points On Geographic Map",
    fontface = "bold", size = 12, color = standard_color$background_dark,
    vjust = 0)

## generate hidden points count by state
hidden_points <- pop_points %>%
  left_join(select(population, full_fips, state), by = "full_fips") %>%
  mutate("hidden" = as.numeric(duplicated(full_fips))) %>%
  group_by(state) %>%
  summarize(
    visible = sum(1 - hidden),
    hidden  = sum(hidden),
    total = visible + hidden
    )

warning("TODO: Calculate dot arrangement based on visible/invisible by state")

## RENDER LEFT BOTTOM PANEL (CLUSTER SOLUTION) =================================

## add plot title
crowded_scatterplot <- crowded_scatterplot + geom_text(
  data = filter(poster_dim, element == "title_11"),
  mapping = aes(x = (x_min + x_max) / 2, y = (y_min + y_max) / 2),
  label = "Points Lumped Together And Sized Accordingly",
  fontface = "bold", size = 12, color = standard_color$background_dark,
  vjust = 0
  )

## determine cluster under-layer coloring
Recycle <- function(x, n) {
  x <- rep(x, times = ceiling(n / length(x)))
  x <- x[seq(n)]
  return(x)
}

cluster_solution$hue <- Recycle(
  x = seq(from = 0, to = 11 / 12, by = 1 / 12),
  n = nrow(cluster_solution)
  )

cluster_color <- cluster_solution %>%
  mutate(cluster = seq(nrow(cluster_solution))) %>%
  unnest(cols = full_fips ) %>%
  select(cluster, full_fips, hue)

population <- population %>% left_join(cluster_color, by = "full_fips")

## render population
cluster_solution <- cluster_solution %>%
  mutate(size = 2^4.2 * population / max(population))
crowded_scatterplot <- crowded_scatterplot +
  geom_point(
    data = PanelMutation(the_data = population, panel = "plot_11"),
    mapping = aes(x = panel_x, y = panel_y),
    size = 0.1,
    color = hsv(h = population$hue, s = 0.1, v = 0.9)
    ) +
  geom_point(
    data = PanelMutation(the_data = cluster_solution, panel = "plot_11"),
    mapping = aes(x = panel_x, y = panel_y
      ),
    size = cluster_solution$size,
    color = "transparent",
    fill = hsv(h = cluster_solution$hue, s = 0.1, v = 0.9),
    pch = 21
    ) +
  geom_point(
    data = PanelMutation(the_data = cluster_solution, panel = "plot_11"),
    mapping = aes(x = panel_x, y = panel_y
      ),
    size = cluster_solution$size,
    color = hsv(h = cluster_solution$hue, s = 0.80, v = 0.40),
    fill = "transparent",
    pch = 21, stroke = 1.5
    ) +
  theme(legend.position = "none")

## RENDER MIDDLE BOTTOM PANEL (PROXIMITY SOLUTION) =============================

## add plot title
crowded_scatterplot <- crowded_scatterplot + geom_text(
  data = filter(poster_dim, element == "title_21"),
  mapping = aes(x = (x_min + x_max) / 2, y = (y_min + y_max) / 2),
  label = "Points Shaded If 1,000,000+ People Nearby",
  fontface = "bold", size = 12, color = standard_color$background_dark,
  vjust = 0
  )

## render population
proximity_solution <- proximity_solution %>% filter(points > 10^6)
crowded_scatterplot <- crowded_scatterplot +
  geom_point(
    data = PanelMutation(the_data = population, panel = "plot_21"),
    mapping = aes(x = panel_x, y = panel_y),
    size = 0.1,
    color = standard_color$midground_mid
    )  +
  geom_point(
    data = PanelMutation(the_data = proximity_solution, panel = "plot_21"),
    mapping = aes(x = panel_x, y = panel_y),
    size = 0.1,
    color = standard_color$foreground_dark
    )

## RENDER RIGHT BOTTOM PANEL (SPACING SOLUTION) ===================================

## add plot title
crowded_scatterplot <- crowded_scatterplot + geom_text(
  data = filter(poster_dim, element == "title_31"),
  mapping = aes(x = (x_min + x_max) / 2, y = (y_min + y_max) / 2),
  label = "Points Spaced So That All Are Visible",
  fontface = "bold", size = 12, color = standard_color$background_dark,
  vjust = 0
  )

## render population
crowded_scatterplot <- crowded_scatterplot +
  geom_point(
    data = PanelMutation(the_data = spacing_solution, panel = "plot_31"),
    mapping = aes(x = panel_x, y = panel_y),
    size = 0.1,
    color = standard_color$midground_mid
    ) +
  geom_point(
    data = PanelMutation(the_data = population, panel = "plot_31"),
    mapping = aes(x = panel_x, y = panel_y),
    size = 0.1,
    color = standard_color$midground_mid
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