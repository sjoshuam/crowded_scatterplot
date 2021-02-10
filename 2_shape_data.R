##########==========##########==========##########==========##########==========

## SETUP =======================================================================

## meta-information
## Author: Joshua M.
## Creation: 2021-02-07
## Version: 4.0.3
## Description: calculates several solutions to the crowded scatterplot problem

## setup
remove(list= objects())
options(scipen = 2, digits = 2, width = 80, start_time = Sys.time(),
  test_mode = FALSE)
library(sp)
library(maps)
library(mapproj)
library(tidyverse)

## READ IN DATA ================================================================

population <- readRDS("B_Intermediates/population.RData")
pop_points <- readRDS("B_Intermediates/pop_points.RData")

## MAP =========================================================================

## extract map data
conus_map   <- map_data("state") %>%
  as_tibble() %>%
  rename(lon = long)

## project map
project_conus_map <- function(x,
  scale_factors = c(x_min = -0.33, y_min = -1.52, scale = 0.71)
  ) {
  
  ## project map
  albers <- mapproject(x = x$lon, y = x$lat, projection = "albers",
    orientation = c(90, 0, -98.583333), parameters = c(24.520833, 49.384472))
  
  ## calculate scale factors
  if (is.null(scale_factors)) {
    
    scale_factors <-c(
      x_min = min(albers$x),
      y_min = min(albers$y),
      scale = max(c(
        diff(range(albers$x)),
        diff(range(albers$y))
        ))
      )
   
    print("Scale factors:")
    print(scale_factors)   
    
    }
  
  ## center projected coordinates and scale
  x$lon_albers  <- albers$x  - scale_factors["x_min"]
  x$lat_albers  <- albers$y  - scale_factors["y_min"]
  
  x$lon_albers <- x$lon_albers / scale_factors["scale"]
  x$lat_albers <- x$lat_albers / scale_factors["scale"]
  
  x
}

conus_map   <- project_conus_map(conus_map)
population  <- project_conus_map(population)
pop_points  <- project_conus_map(pop_points)
lon_lat <- c("lon_albers", "lat_albers")

## NO SOLUTION =================================================================

## determine how many points are obscured - no other calculations needed
pop_points$visible <- !duplicated(pop_points$full_fips)

## SPACE OUT POINTS ============================================================

## declare function
decrowd_space <- function(points_xy, border_xyg = NULL, point_density = 0.02,
  sample_size = if_else(options()$test_mode, 10^2, 10^4)
  ) {
  
  ## conform class of inputs
  points_xy <- as.data.frame(points_xy)
  border_xyg <- as.data.frame(border_xyg)
  
  ## sample if the number of points is large
  if (!is.null(sample_size)) {
    sample_size <- min(sample_size, nrow(points_xy))
    sample_selection <- seq(nrow(points_xy))
    sample_selection <- sample(sample_selection, size = sample_size)
    points_xy <- points_xy[sample_selection, ]
    remove(sample_selection)
  }
  remove(sample_size)
  
  ## calculate total number of points needed and extreme coordinates
  grid_size <- ceiling(nrow(points_xy) / point_density) %>% sqrt()
  
  if (is.null(border_xyg)) {
    grid_extreme <- data.frame(points_xy, "group" = 1)
  } else {
    grid_extreme <- border_xyg[ , 1:2]
  }
  grid_extreme <- apply(grid_extreme, 2, range)
  
  grid_size <- max(abs(apply(grid_extreme, 2, diff))) / grid_size

  ## generate grid of points
  xy_grid <- list(
    "x" = seq(from = grid_extreme[1, 1], to = grid_extreme[2, 1],
      by = grid_size),
    "y" = seq(from = grid_extreme[1, 2], to = grid_extreme[2, 2],
      by = grid_size)
    )
  xy_grid <- data.frame(expand.grid(xy_grid$x, xy_grid$y))
  colnames(xy_grid) <- c("x", "y")

  # limit spacing grid to polygons if applicable
  point_in_poly <- function(poly_xy, point_xy) {
    z <- sp::point.in.polygon(
      point_xy[, 1], point_xy[, 2],
      poly_xy[, 1], poly_xy[, 2],
      )
    as.logical(z)
    }
  if (!is.null(border_xyg)) {
    border_xyg <- split(border_xyg[, 1:2], border_xyg[, 3])
    border_xyg <- mapply(FUN = point_in_poly, border_xyg,
      point_xy = list(xy_grid))
    border_xyg <- apply(border_xyg, 1, any)
    xy_grid <- xy_grid[border_xyg, ]
  }
  
  ## calculate rough point groups
  points_xy$token <- paste(
    round(points_xy[, 1] / grid_size) * grid_size,
    round(points_xy[, 2] / grid_size) * grid_size
    )
  simple_xy <- points_xy[!duplicated(points_xy$token), ]
  
  ## calculate distance between points and point grid
  closest_grid <- sp::spDists(as.matrix(simple_xy[, 1:2]), as.matrix(xy_grid))
  rownames(closest_grid) <- simple_xy$token
  closest_grid <- closest_grid[points_xy$token, ]
  
  ## assign points to grid
  points_xy$grid <- NA
  for(iter in seq(nrow(points_xy))) {
    points_xy$grid[iter] <- which.min(closest_grid[iter, ])
    closest_grid[, points_xy$grid[iter]] <- Inf
    }
  
  ## express results
  xy_grid[points_xy$grid, ]
}

## execute function
space_solution <- decrowd_space(
  pop_points[, lon_lat],
  border_xyg = conus_map[, c(lon_lat, "group")]
  ) %>%
  as_tibble()

## CLUSTERING SOLUTION =========================================================

decrowd_cluster <- function(the_data, coordinates, weight,
  clusters = 50, simplify_size = if_else(options()$test_mode, 10^3, 5 * 10^4)
  ) {
  
  ## double check class
  the_data <- as_tibble(the_data)
  
  ## apply initial implication
  if (nrow(the_data) > simplify_size) {
    kmeans_centers <- kmeans( select(the_data, all_of(coordinates)),
      centers = simplify_size,
      iter.max = 10^3
      )
    the_data$kmeans_cluster <- kmeans_centers$cluster
    kmeans_centers <- as_tibble(kmeans_centers$centers)
  } else {
    kmeans_centers <- select(the_data, all_of(coordinates))
    the_data$kmeans_cluster <- seq(nrow(the_data))
  }
  
  ## cluster points further via Ward's minimum distance
  ward_clusters <- sp::spDists(as.matrix(kmeans_centers)) %>%
    as.dist() %>%
    hclust(method = "ward.D2") %>%
    cutree(k = clusters) %>%
    tibble() %>%
    magrittr::set_colnames("ward_cluster") %>%
    bind_cols("kmeans_cluster" = seq(nrow(kmeans_centers)))
  the_data <- left_join(the_data, ward_clusters, by = "kmeans_cluster") %>%
    mutate(type = "point")
  remove(ward_clusters, kmeans_centers)
  
  ## generate cluster-wise object
  ward_clusters <- the_data %>%
    select(all_of(c(coordinates, weight, "ward_cluster"))) %>%
    magrittr::set_colnames(LETTERS[1:4]) %>%
    group_by(D) %>%
    summarize(
      mean(A),
      mean(B),
      sum(C)
      ) %>%
    mutate(type = "cluster", kmeans_cluster = NA)
  colnames(ward_clusters)[1:4] <- c("ward_cluster", coordinates, weight)
  the_data <- bind_rows(the_data, ward_clusters)
  
  ## express results
  the_data
  }

## execute function
set.seed(6432)
cluster_solution <- decrowd_cluster(
  the_data = population,
  coordinates = lon_lat,
  weight = "population"
  )

## PROXIMITY SOLUTION ==========================================================

## declare function
decrowd_proximity <- function(the_data, coordinates, weight, grid_size,
  border_xyg = NULL,
  max_dist = 50 * (1609/1000), longlat = TRUE) {
  
  ## double check class
  the_data <- as_tibble(the_data)
  
  ## calculate grid spacing
  grid_spacing <- the_data %>%
    select(all_of(coordinates)) %>%
    magrittr::set_colnames(c("x", "y")) %>%
    summarize(x = diff(range(x)), y = diff(range(y))) %>%
    unlist() %>%
    abs() %>%
    max()
  grid_spacing <- grid_spacing / sqrt(grid_size)
  
  ## generate point grid
  grid_xy <- expand_grid(
    "x"= seq(
      from = min(pull(the_data, all_of(coordinates[1]))),
      to = max(pull(the_data, all_of(coordinates[1]))),
      by = grid_spacing),
    "y"= seq(
      from = min(pull(the_data, all_of(coordinates[2]))),
      to = max(pull(the_data, all_of(coordinates[2]))),
      by = grid_spacing)
    )
  
  # limit spacing grid to polygons if applicable
  point_in_poly <- function(poly_xy, point_xy) {
    z <- sp::point.in.polygon(
      unlist(point_xy[, 1]), unlist(point_xy[, 2]),
      unlist(poly_xy[, 1]), unlist(poly_xy[, 2]),
      )
    as.logical(z)
    }
  if (!is.null(border_xyg)) {
    border_xyg <- split(border_xyg[, 1:2], border_xyg[, 3])
    border_xyg <- mapply(FUN = point_in_poly, border_xyg,
      point_xy = list(grid_xy))
    border_xyg <- apply(border_xyg, 1, any)
    grid_xy <- grid_xy[border_xyg, ]
  }
  
  ## calculate distance between grid points and input points
  distance_matrix <- sp::spDists(
    as.matrix(select(the_data, all_of(coordinates))),
    as.matrix(grid_xy),
    longlat = longlat
    )
  distance_matrix <- distance_matrix < max_dist
  distance_matrix <- apply(distance_matrix, 2, as.numeric)
  diag(distance_matrix) <- 0
  
  ## calculate total population in range of each grid point
  grid_xy$weight <- colSums(distance_matrix * pull(the_data, all_of(weight)))
  colnames(grid_xy) <- c(coordinates, weight)
  
  ## return output
  grid_xy
}

## execute function
proximity_solution <- decrowd_proximity(
  the_data = population,
  border_xyg = conus_map[, c("lon", "lat", "group")],
  coordinates = c("lon", "lat"),
  weight = "population",
  grid_size = if_else(options()$test_mode, 50^2, 100^2)
  )

proximity_solution <- project_conus_map(proximity_solution)

## -------------
pdf("~/Desktop/crowded_plots.pdf")

plot(
  x = population$lon_albers,
  y = population$lat_albers,
  xlim = c(0, 1), ylim = c(0, 0.6),
  cex = 0.15, asp = 1,
  pch = 16
  )

plot(space_solution$x, space_solution$y,
  xlim = c(0, 1), ylim = c(0, 0.6),
  pch =16, asp = 1, cex = 0.15)
title(main = "spacing")


temp <- filter(cluster_solution, type == "cluster")

plot(temp$lon_albers, temp$lat_albers,
  xlim = c(0, 1), ylim = c(0, 0.6),
  pch =16, asp = 1, cex = (temp$population/max(temp$population)) * 4
  )
title(main = "cluster")

plot(proximity_solution$lon_albers, proximity_solution$lat_albers,
  xlim = c(0, 1), ylim = c(0, 0.6),
  pch =16, asp = 1, cex = proximity_solution$population/max(proximity_solution$population))
title(main = "proximity")

graphics.off()

## [EMPTY SLOT] ================================================================

warning("Open in case geom_polygon(stat = \"density2d\") doesn't pan out")

## EXPORT FINAL PRODUCTS =======================================================

saveRDS(space_solution,     file = "B_Intermediates/space_solution.RData")
saveRDS(cluster_solution,   file = "B_Intermediates/cluster_solution.RData")
saveRDS(proximity_solution, file = "B_Intermediates/proximity_solution.RData")

##########==========##########==========##########==========##########==========

Sys.time() - options()$start_time