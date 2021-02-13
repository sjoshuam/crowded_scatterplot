##########==========##########==========##########==========##########==========

## SETUP =======================================================================

## meta-information
## Author: Joshua M.
## Creation: 2021-02-07
## Version: 4.0.3
## Description: calculates three solutions to the crowded scatterplot problem -
   ## 1. Cluster - Sum population into a small number of regional dots
   ## 2. Proximity count - Generate a regular lattice of points and sum the
      ## population within a radius of each point
   ## 3. Space out points - Move dots representing a chunk of population so that
      ## they do not occlude each other.

## setup
remove(list= objects())
options(scipen = 2, digits = 2, width = 80, start_time = Sys.time(),
  test_mode = TRUE, start_time = Sys.time(), refresh = TRUE)
library(sp)
library(maps)
library(mapproj)
library(tidyverse)
library(parallel)

## READ IN DATA ================================================================

population <- readRDS("B_Intermediates/population.RData")
pop_points <- readRDS("B_Intermediates/pop_points.RData")

conus_map   <- map_data("state") %>% as_tibble() %>% rename(lon = long)

## TEST MODE ===================================================================

if (options()$test_mode) {

population <- slice_sample(population, n = min(nrow(population), 2 * 10^4))
pop_points <- slice_sample(pop_points, n = min(nrow(pop_points), 2 * 10^4))
conus_map <- filter(conus_map,
  !(region %in% c(
    "florida", "texas", "louisiana", "rhode island",
    "washington", "idaho", "montana", "north dakota", "minneapolis", 
    "maine", "new hampshire", "vermont", "massachusetts", "connecticut"
    ))
  )
}

## PROJECT COORDIANTES =========================================================

## declare map projection function
ProjectAlbersConus <- function(x,
  scale_factors = c(x_min = -0.33, y_min = -1.52, scale = 0.71)
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
   
    print("Scale factors:")
    print(scale_factors)   
    
  if (is.null(scale_factors)) {
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

## execute map projection function
conus_map   <- ProjectAlbersConus(conus_map)
population  <- ProjectAlbersConus(population)
pop_points  <- ProjectAlbersConus(pop_points)
lon_lat <- c("lon_albers", "lat_albers")

## GENERATE POINT GRIDS ========================================================

   ## supports the proximity and spacing solutions

## declare point generation function 
MakePointGrid <- function(all_polygon, xy_name, group_name,
  n_points = nrow(pop_points) * 20
  ) {

  ## calcualte key polygon measures
  point_range  <- all_polygon %>%
    select(all_of(xy_name)) %>%
    rename(x = 1, y = 2) %>%
    summarize(
      xmax = max(x),
      xmin = min(x),
      ymax = max(y),
      ymin = min(y),
      xspace = (max(x) - min(x)) / sqrt(n_points),
      yspace = (max(y) - min(y)) / sqrt(n_points)
      ) %>%
    mutate("space" = max(xspace, yspace)) %>%
    as.list()

  ## generate point grid
  point_grid <- expand.grid(
    x = seq(
      to = point_range$xmax,
      from = point_range$xmin,
      by = point_range$space
      ),
    y = seq(
      to = point_range$ymax,
      from = point_range$ymin,
      by = point_range$space
      )
    )

  
  ## detect points outside each polygons
  PointInPolygon <- function(polygon_xy, point_xy) {
    y <- sp::point.in.polygon(
      point.x = point_xy[, 1],
      point.y = point_xy[, 2],
      pol.x = polygon_xy[, 1],
      pol.y = polygon_xy[, 2]
      )
    as.logical(y)
  }
  
  in_polygon <- split(
    x = all_polygon[, xy_name],
    f= all_polygon[[group_name]]
    )
  par_cluster <- makeCluster(floor(detectCores() * 0.8))
  in_polygon <- parLapply(
    cl = par_cluster,
    X = in_polygon,
    fun = PointInPolygon,
    point_xy = point_grid
    )
  stopCluster(par_cluster)
  
  ## exclude points outside all polygons
  in_polygon <- simplify2array(in_polygon)
  in_polygon <- apply(in_polygon, 1, any)
  point_gird <- point_grid[in_polygon, ]

  ## express results
  tibble(point_gird)
}

## execute function
projected_grid <- MakePointGrid(all_polygon = conus_map,
  xy_name = lon_lat, group_name = "group")

remove(MakePointGrid)

## CALCULATE DISTANCE BETWEEN POINTS AND GRID ==================================

## declare distance calculation function
DetectProximity <- function(xy1_indexed, xy2, max_dist = 0.00035 * 200) {

  xy1 <- xy1_indexed[, 1:2]
  xy1_index <- xy1_indexed[, 3]
  remove(xy1_indexed)

  ## calculate distance matrix
  distance_matrix <- outer(xy1[, 1], xy2[, 1], FUN = "-")
  y_distance <- outer(xy1[, 2], xy2[, 2], FUN = "-")
  distance_matrix <- distance_matrix^2
  y_distance <- y_distance^2
  distance_matrix <- distance_matrix + y_distance
  distance_matrix <- sqrt(distance_matrix)
  remove(y_distance, xy2, xy1)

  ## reshape matrix to long format
  proximity_list <- data.frame(
    expand.grid(xy1_index, seq(ncol(distance_matrix))),
    round(as.vector(distance_matrix), 3)
    )
  colnames(proximity_list) <- c("row", "col", "dist")
  remove(distance_matrix)

  ## exclude long distances and express
  proximity_list <- proximity_list[proximity_list$dist <= max_dist, ]
  proximity_list <- proximity_list[order(proximity_list$dist), ]
  return(proximity_list)
}

## calculate distance
CalculateDistanceInParallel <- function(xy1, xy2, DistFunc) {
  
  ## generate index for the matrix that will be split into parallel chunks
  xy1 <- data.frame(xy1, "index" = seq(nrow(xy1)))
  
  ## split into chunks
  cut_vector <- seq(nrow(xy1))
  cut_vector <- cut(cut_vector,
    breaks = max(
      floor(detectCores() * 0.5),
      floor(length(cut_vector) / 400)
      )
    )
  cut_vector <- as.numeric(cut_vector)
  xy1 <-  split(xy1, f = cut_vector)

  ## execution function on each chunk
  parallel_cluster <- makeCluster(floor(detectCores() * 0.8))
  the_results <- parLapply(
    cl = parallel_cluster,
    X = xy1,
    fun = DistFunc,
    xy2 = xy2
    )
  stopCluster(parallel_cluster)
  
  ## compile results and express result
  remove(xy2, xy1)
  gc()
  the_results <- do.call(what = rbind, args = the_results)
  rownames(the_results) <- NULL
  return(the_results)
}

population_distance <- CalculateDistanceInParallel(
  xy1 = projected_grid[, c("x", "y")],
  xy2 = population[, lon_lat],
  DistFunc = DetectProximity
  )
saveRDS(population_distance, file = "B_Intermediates/population_distance.RData")
remove(population_distance)

pop_point_distance <- CalculateDistanceInParallel(
  xy1 = projected_grid[, c("x", "y")],
  xy2 = pop_points[, lon_lat],
  DistFunc = DetectProximity
  )

## save objects to free up RAM
saveRDS(pop_point_distance, file = "B_Intermediates/pop_point_distance.RData")
remove(pop_point_distance)

saveRDS(projected_grid, file = "B_Intermediates/projected_grid.RData")
remove(projected_grid)

## CALCULATE KMEANS SOLUTION ===================================================

## calculate kmeans clusters
population$kmeans_solution <- kmeans(
  x = as.matrix(population[, lon_lat]),
  centers = 10^4
  )$cluster
cluster_solution <- population %>%
  group_by(kmeans_solution) %>%
  summarize(
    lon_albers = median(lon_albers),
    lat_albers = median(lat_albers),
    population = sum(population)
    )

## simplify further by proximity
cluster_solution$cluster_solution <- sp::spDists(
  x = as.matrix(cluster_solution[, lon_lat]),
  y = as.matrix(cluster_solution[, lon_lat]),
  longlat = FALSE
  ) %>%
  as.dist() %>%
  hclust() %>%
  cutree(h = 0.00035 * 200)
population <- left_join(
  population,
  cluster_solution[, c("kmeans_solution", "cluster_solution")]
  )

## generate cluster solution object
cluster_solution <- population %>%
  group_by(cluster_solution) %>%
  summarize(
    "lon_albers" = median(lon_albers),
    "lat_albers" = median(lat_albers),
    "population" = sum(population)
    ) %>%
  select(-cluster_solution)

## CALCULATE SPACING SOLUTION ==================================================

## load pop_point distance object
pop_point_distance <- readRDS("B_Intermediates/pop_point_distance.RData")
pop_point_distance <- pop_point_distance[sample(nrow(pop_point_distance)), ]
pop_point_distance <- pop_point_distance[order(pop_point_distance$dist), ]

## assign pop_points points to unique grid points
spacing_solution <- vector(mode = "list")
iter <- 0

while (nrow(pop_point_distance) > 0) {
  iter <- iter + 1
  
  ## find unique grid-pop_point pairs
  i <- !duplicated(pop_point_distance$row) & !duplicated(pop_point_distance$col)
  spacing_solution[[iter]] <- pop_point_distance[i, ]
  
  ## exclude rows/columns that have been paired
  i <- pop_point_distance$row %in% spacing_solution[[iter]]$row
  i <- i | (pop_point_distance$col %in% spacing_solution[[iter]]$col)
  pop_point_distance <- pop_point_distance[!i, ]
  
  if(iter > 10^3) break
  if (((iter %% 10) == 0) | (iter < 10)) {
    print(paste("Records remaining: ", nrow(pop_point_distance)))
  }
}

spacing_solution <- do.call(what = rbind, args = spacing_solution) %>%
  as_tibble()
rownames(spacing_solution) <- NULL

remove(iter, i)

## prepare point grid object
projected_grid <- readRDS("B_Intermediates/projected_grid.RData")
colnames(projected_grid) <- c("x_grid", "y_grid")
projected_grid$row <- seq(nrow(projected_grid))

## incorporate coordinates into pop_points object
pop_points$col <- seq(nrow(pop_points))
pop_points <- pop_points %>%
  left_join(
    select(spacing_solution, row, col),
    by = "col"
    ) %>%
  left_join(
    projected_grid,
    by = "row"
    )

remove(projected_grid)

## generate a solution object
spacing_solution <- pop_points %>%
  select(x_grid, y_grid, full_fips) %>%
  rename(lon_albers = x_grid, lat_albers = y_grid)

## CALCULATE PROXIMITY SOLUTION ================================================

## load relevant data
projected_grid <- readRDS("B_Intermediates/projected_grid.RData")
projected_grid$row <- seq(nrow(projected_grid))
population_distance <- readRDS("B_Intermediates/population_distance.RData")
population$col <- seq(nrow(population))

## find closest grid point for each population point
population_distance <- population_distance %>%
  arrange(dist) %>%
  as_tibble() %>%
  filter(dist < 0.00035 * 50) %>%
  left_join(select(population, col, population), by = "col") %>%
  group_by(row) %>%
  summarize("population" = sum(population))

proximity_solution <- projected_grid %>%
  left_join(select(population_distance, row, population), by = "row") %>%
  filter(!is.na(population)) %>%
  rename("lon_albers" = 1, "lat_albers" = 2) %>%
  select(lon_albers, lat_albers, population)

## EXPORT SOLUTIONS ============================================================

## export solutions
save(cluster_solution, file = "B_Intermediates/cluster_solution.RData")
save(proximity_solution, file = "B_Intermediates/proximity_solution.RData")
save(spacing_solution, file = "B_Intermediates/spacing_solution.RData")

## delete intermediate objects
file.remove("B_Intermediates/projected_grid.RData")
file.remove("B_Intermediates/population_distance.RData")
file.remove("B_Intermediates/pop_point_distance.RData")

## FOOTER ======================================================================
Sys.time() - options()$start_time

##########==========##########==========##########==========##########==========
