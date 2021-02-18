##########==========##########==========##########==========##########==========

## SETUP =======================================================================

## meta-information
## Author: Joshua M.
## Creation: 2021-02-15 (version 5)
## Version: 4.0.3
## Description: calculates three solutions to the crowded scatterplot problem -
   ## 1. Cluster - Sum population into a small number of regional dots
   ## 2. Proximity count - Generate a regular lattice of points and sum the
      ## population within a radius of each point
   ## 3. Space out points - Move dots representing a chunk of population so that
      ## they do not occlude each other.
## Note: For a laptop, this is a big computation. If not running in test_mode,
   ## it will take 2-3 hours.

## setup
remove(list= objects())
options(scipen = 2, digits = 3, width = 80, start_time = Sys.time(),
  test_mode = FALSE, start_time = Sys.time(), refresh = TRUE)
options()$start_time

options(cores = ifelse(options()$test_mode, 0.75, 0.50))

library(sp)
library(maps)
library(mapproj)
library(tidyverse)
library(parallel)

## READ IN DATA ================================================================

population <- readRDS("B_Intermediates/population.RData")
pop_points <- readRDS("B_Intermediates/pop_points.RData")

conus_map   <- map_data("state") %>%
  as_tibble() %>%
  rename(lon = long) %>%
  mutate(region = str_to_title(region)) %>%
  mutate(state = state.abb[match(region, state.name)])

## TEST MODE ===================================================================

if (options()$test_mode) {
  
  ## define test states
  big_states <- c(
    "TX", "CA",
    "MT", "NM", "AZ", "NV", "CO",
    "FL", "NY", "PA", "IL", "OH"
    )
  if (TRUE) {
    big_states <- c(big_states,
      "WY", "OR", "ID", "UT", "KS", "MN", "NE",
      "GA", "NC", "MI", "NJ", "VA", "WA", "AZ", "MA"
      )
  }
  names(big_states) <- state.name[state.abb %in% big_states]
  
  ## filter data to test states
  conus_map <- conus_map %>% filter(!(region %in% names(big_states)))
  population <- population %>% filter(!(state  %in% big_states))
  pop_points <- pop_points %>% filter(full_fips %in% population$full_fips)
  remove(big_states)
  
}

## DECLARE MAP REDUCE FUNCTIONS ================================================
Sys.time() - options()$start_time

## create directory to hold mapped data objects
unlink("B_Intermediates/map_lapply", recursive = TRUE)
dir.create("B_Intermediates/map_lapply")

## declare function to generate mapped data objects
MapCreate <- function(dest_dir, data_object, cut_vector) {
  
  ## refine vector used to divide the dataset into chunks
  if (is.numeric(cut_vector)) {
    data_name <- ceiling(log10(max(cut_vector))) + 1
    cut_vector <- cut_vector + 10^data_name
    cut_vector <- paste0("Obj", cut_vector)
  }

  ## split dataset according to cut vector
  data_object <- as.data.frame(data_object)
  data_object <- split(x = data_object, f = cut_vector)
  data_object <- lapply(data_object, as.matrix)

  ## generate data object names
  dest_dir <- paste0("B_Intermediates/map_lapply/", dest_dir)
  data_name <- paste0("/", names(data_object), ".RData")
  data_name <- paste0(dest_dir, data_name)

  ## save to disk
  dir.create(dest_dir)
  invisible(mapply(
    FUN = saveRDS,
    object = data_object,
    file = data_name
    ))
  
  }

## declare function to apply functions to data objects
MapLapply <- function(dest_dir, FUN) {
  
  ## create a list of data objects
  dest_dir <- paste0("B_Intermediates/map_lapply/", dest_dir)
  dest_dir <- list.files(dest_dir, full.names = TRUE)
  names(dest_dir) <- str_remove(dest_dir, ".+/")
  
  ## create a wrapper for the function
  FunctionWrapper <- function(dd, f = FUN) {
    x <- readRDS(dd)
    x <- f(x)
    saveRDS(x, file = dd)
  }
  function_environment = environment()
  
  ## apply function to each data object
  par_cluster <- makeCluster(floor(detectCores() * options()$cores))
  clusterExport(cl = par_cluster,
    varlist = c("FUN", "FunctionWrapper"), envir = function_environment)
  parLapplyLB(cl = par_cluster, X = dest_dir, fun = FunctionWrapper)
  stopCluster(par_cluster)
}

## declare function to retrieve data objects
MapRetrieve <- function(dest_dir) {
  
  ## create a list of data objects
  dest_dir <- paste0("B_Intermediates/map_lapply/", dest_dir)
  dest_dir <- list.files(dest_dir, full.names = TRUE)
  names(dest_dir) <- str_remove(dest_dir, ".+/")
  
  ## apply function to each data object
  par_cluster <- makeCluster(detectCores() - 2)
  the_result <- parLapplyLB(cl = par_cluster, X = dest_dir, fun = readRDS)
  stopCluster(par_cluster)
  
  ## express results
  return(the_result)
}

## declare function to duplicate map object
MapDuplicate <- function(dest_dir, new_dir) {
  dest_dir <- paste0("B_Intermediates/map_lapply/", dest_dir)
  dir.create(paste0("B_Intermediates/map_lapply/", new_dir))
  new_dir <- paste0("B_Intermediates/map_lapply/", new_dir)
  dest_dir <- list.files(dest_dir, full.names = TRUE)
  invisible(file.copy(from = dest_dir, to = new_dir, recursive = TRUE))
}

## declare function to bundle objects in a list
MapBundle <- function(read_dir1, read_dir2, write_dir) {
  
  unlink(paste0("B_Intermediates/map_lapply/", write_dir), recursive = TRUE)
  dir.create(paste0("B_Intermediates/map_lapply/", write_dir))
  
  ## generate file roster
  output_files <- data.frame(
    "Input_1" = list.files(paste0("B_Intermediates/map_lapply/", read_dir1),
      full.names = TRUE),
    "Input_2" = list.files(paste0("B_Intermediates/map_lapply/", read_dir2),
      full.names = TRUE),
    "Output" = list.files(paste0("B_Intermediates/map_lapply/", read_dir2),
      full.names = TRUE)
    )
  names(output_files)[1:2] <- c(read_dir1, read_dir2)
  output_files$Output <- gsub(
    pattern = read_dir2,
    replacement = write_dir,
    x = output_files$Output
    )
  
  output_files <- split(output_files, seq(nrow(output_files)))
  output_files <- lapply(output_files, unlist)
  
  ## function to read in a package of objects, packaged as a list
  ReadPair <- function(x) {
    y <- x[3]
    x <- x[-3]
    z <- tapply(x, names(x), readRDS)
    z <- z[names(x)[1:2]]
    saveRDS(z, file = y)
    }
  
  ## read in objects
  parallel_cluster <- makeCluster(detectCores() - 2)
  function_environment <- environment()
  output_files <- parLapplyLB(
    cl = parallel_cluster,
    X = output_files,
    fun = ReadPair
    )
  stopCluster(parallel_cluster)
  
}

## PROJECT GEOGRAPHIC COORDIANTES ==============================================
Sys.time() - options()$start_time

## declare map projection function
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

## execute map projection function
conus_map   <- ProjectAlbersConus(conus_map)
population  <- ProjectAlbersConus(population)
pop_points  <- ProjectAlbersConus(pop_points)
lon_lat <- c("lon_albers", "lat_albers")

## merge tiny states
conus_map$state <- recode(conus_map$state,
    "NH" = "AA", "VT" = "AA", "MA" = "AA", "CT" = "AA", "RI" = "AA",
    "MD" = "BB", "DE" = "BB", "DC" = "BB", "NJ" = "BB"
    )
population$state <- recode(population$state,
    "NH" = "AA", "VT" = "AA", "MA" = "AA", "CT" = "AA", "RI" = "AA",
    "MD" = "BB", "DE" = "BB", "DC" = "BB", "NJ" = "BB"
    )

## GENERATE POINT LATTICES =====================================================
Sys.time() - options()$start_time

MakeLattice <- function(polygon_xyg) {
  
  ## declare point in polygon wrapper function
  PointInPolygon <- function(pol, pnt) {
    sp::point.in.polygon(
      point.x = pnt[, 1],
      point.y = pnt[, 2],
      pol.x = pol[, 1],
      pol.y = pol[, 2]
      )
    }
  
  ## make rectangular lattice
  point_lattice <- expand.grid(
    seq(from = min(polygon_xyg[, 1]), to = max(polygon_xyg[, 1]),
      by = 0.00035 * 2),
    seq(from = min(polygon_xyg[, 2]), to = max(polygon_xyg[, 2]),
      by = 0.00035 * 2),
    stringsAsFactors = FALSE
    )
  colnames(point_lattice) <- colnames(polygon_xyg)[1:2]

  ## offset each other row
  i <- sort(unique(point_lattice[, 2]))
  i <- i[c(T, F)]
  i <- point_lattice[, 2] %in% i
  point_lattice[i, 1] <- point_lattice[i, 1] - 0.00035 * 1

  ## determine which lattice points are within a polygon
  polygon_xyg <- split(
    as.data.frame(polygon_xyg[, 1:2]),
    polygon_xyg[, 3]
    )
  polygon_xyg <- lapply(polygon_xyg, as.matrix)
  polygon_xyg <- mapply(
    FUN = PointInPolygon,
    pol = polygon_xyg,
    pnt = list(point_lattice)
    )
  polygon_xyg <- as.logical(rowSums(polygon_xyg))

  ## filter grid points to those within polygon and express
  point_lattice <- point_lattice[polygon_xyg, ]
  return(point_lattice)
}

## execute function
MapCreate("grid",
  as.matrix(conus_map[, c("lon_albers", "lat_albers", "group")]),
  conus_map$state)
MapLapply("grid", MakeLattice)

## CALCULATE DISTANCE BETWEEN POINTS AND GRID ==================================
Sys.time() - options()$start_time

## declare function to calculate distance between points and grid
CalculateProximity <- function(xy) {
  
  ## calculate distance
  d <- outer(xy[[1]][, 1], xy[[2]][, 1], FUN = "-")^2
  d <- d + outer(xy[[1]][, 2], xy[[2]][, 2], FUN = "-")^2
  d <- sqrt(d)
  dimnames(d) <- list(rownames(xy[[1]]), rownames(xy[[2]]))
  print(dim(d))
  
  ## limit distances to proximity
  d <- data.frame(
    expand.grid(rownames(d), colnames(d), stringsAsFactors = FALSE),
    as.vector(d)
  )
  colnames(d) <- c("row", "col", "dist")
  d <- d[d[, 3] < 0.00035 * 200, ]
  d <- d[order(d[, 3]), ]
  d[, 3] <- round(d[, 3], 4)
  return(d)
  }

## split population object into chunks; package with grid chunks
population_matrix <- population %>%
  as.data.frame() %>%
  magrittr::set_rownames(population$full_fips) %>%
  select(lon_albers, lat_albers, state)

MapCreate(
  "population",
  as.matrix(population_matrix[, c("lon_albers", "lat_albers")]),
  population_matrix[, "state"]
  )
MapBundle("population", "grid", "population_distance")

remove(population_matrix)

## split pop_points into chunks; package with grid chunks
population_matrix <- pop_points %>%
  as.data.frame() %>%
  left_join(population[, c("full_fips", "state")]) %>%
  magrittr::set_rownames(paste0("p", seq(nrow(pop_points)))) %>%
  select(lon_albers, lat_albers, state)

MapCreate(
  "pop_points",
  as.matrix(population_matrix[, c("lon_albers", "lat_albers")]),
  population_matrix[, "state"]
  )
MapBundle("pop_points", "grid", "pop_points_distance")

remove(population_matrix)

## calculate distance between each population/pop_points and grid chunk pair
MapLapply("population_distance", CalculateProximity)
MapLapply("pop_points_distance", CalculateProximity)

## CALCULATE PROXIMITY SOLUTION ================================================
Sys.time() - options()$start_time

## declare function to find points within a radius of a grid point
FindPointsInRadius <- function(x){
  x[[2]] <- x[[2]][x[[2]][, 3] <= 0.00035 * 25, ]
  x[[2]] <- tapply(x[[2]][, 1], x[[2]][, 2], unique)
  x[[2]] <- x[[2]][rownames(x[[1]])]
  x[[1]]$points <- x[[2]]
  x <- x[[1]][!sapply(x[[1]]$points, is.null), ]
  return(x)
}

## find closest grid point to each population point
MapBundle("grid", "population_distance", "proximity_solution")
MapLapply("proximity_solution", FindPointsInRadius)

## retrieve and unpack distributed calculations
proximity_solution <- MapRetrieve("proximity_solution")
proximity_solution <- do.call(what = "rbind", args = proximity_solution)

## sum population at each point
population <- readRDS("B_Intermediates/population.RData")
population <- setNames(population$population, population$full_fips)

SumPopulation <- function(pop_names, pop = population) {
  sum(pop[names(pop) %in% pop_names])
}

parallel_cluster <- makeCluster(detectCores() * options()$cores)
proximity_solution$points <- parSapply(
    cl = parallel_cluster,
    X = proximity_solution$points,
    FUN = SumPopulation,
    pop = population
    )
stopCluster(parallel_cluster)

## save results and clean up objects that are no longer needed
saveRDS(proximity_solution, file = "B_Intermediates/proximity_solution.RData")
remove(population, proximity_solution)
unlink("B_Intermediates/map_lapply/proximity_solution", recursive = TRUE)
unlink("B_Intermediates/map_lapply/population_distance", recursive = TRUE)

## CALCULATE SPACING SOLUTION ==================================================
Sys.time() - options()$start_time

## declare function to find unique grid-point pairs
FindUniqueGridPointPairs <- function(x){
  
  ## add some randomness to the distance sorting order
  x[[2]] <- x[[2]][sample(nrow(x[[2]])), ]
  x[[2]] <- x[[2]][order(x[[2]][, 3]), ]
  the_distance <- x[[2]]
  x[[2]] <- NULL
  x <- x[[1]]

  ## filter distance grid
  result_basket <- vector(mode = "list")
  iter <- 0

  while (nrow(the_distance) > 0) {
    iter <- iter + 1
    i <- !duplicated(the_distance[, 1])
    i <- i & !duplicated(the_distance[, 2])
    result_basket[[iter]] <- the_distance[i, ]
    i <- the_distance[, 1] %in% result_basket[[iter]][, 1]
    i <- i | (the_distance[, 2] %in% result_basket[[iter]][, 2])
    the_distance <- the_distance[!i, ]
    if (iter > 200) break
  }

  result_basket <- do.call(what = rbind, args = result_basket)
  x <- x[match(result_basket[, 2], rownames(x)), ]
  result_basket <- cbind(result_basket, x)
  return(result_basket)
}

# ## find with grid points were already pop_points
# FindGridedPopPoints <- function(x) {
#   x[x[, 3] < 0.00035 * 2, 2]
# }
# 
# MapDuplicate("pop_points_distance", "already_on_grid")
# MapLapply("already_on_grid", FindGridedPopPoints)

## find unique grid-point pairs
MapBundle("grid", "pop_points_distance", "spacing_solution")
MapLapply("spacing_solution", FindUniqueGridPointPairs) ##!!!

## retrieve and unpack distributed calculations
spacing_solution <- MapRetrieve("spacing_solution")
spacing_solution <- do.call(what = "rbind", args = spacing_solution)

# ## retrieve "already a pop_point" variables and incorporate
# already_on_grid <- MapRetrieve("already_on_grid")
# already_on_grid <- unlist(already_on_grid, use.names = FALSE)
# spacing_solution <- spacing_solution %>%
#   mutate(already_on_grid = col %in% already_on_grid)

## save results and clean up objects that are no longer needed
saveRDS(spacing_solution, file = "B_Intermediates/spacing_solution.RData")
remove(pop_points, spacing_solution)
unlink("B_Intermediates/map_lapply/spacing_solution", recursive = TRUE)
unlink("B_Intermediates/map_lapply/pop_points_distance", recursive = TRUE)

## CALCULATE KMEANS SOLUTION ===================================================
Sys.time() - options()$start_time

## read in data
population <- readRDS("B_Intermediates/population.RData")
population  <- ProjectAlbersConus(population)

## calculate kmeans clusters
population$kmeans_solution <- kmeans(
  x = as.matrix(population[, lon_lat]),
  centers = 10^4,
  iter.max = 10^3
  )$cluster
cluster_solution <- population %>%
  group_by(kmeans_solution) %>%
  summarize(
    lon_albers = sum(lon_albers * population),
    lat_albers = sum(lat_albers * population),
    population = sum(population),
    full_fips = list(full_fips)
    ) %>%
  mutate(
    lon_albers = lon_albers / pmax(population, 1),
    lat_albers = lat_albers / pmax(population, 1)
    ) 

## simplify further by proximity
cluster_solution$cluster_solution <- dist(
  as.matrix(cluster_solution[, lon_lat])) %>%
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
    lon_albers = sum(lon_albers * population),
    lat_albers = sum(lat_albers * population),
    population = sum(population),
    full_fips = list(full_fips)
    ) %>%
  mutate(
    lon_albers = lon_albers / pmax(population, 1),
    lat_albers = lat_albers / pmax(population, 1)
    ) %>%
  select(-cluster_solution)

## clean up environment
saveRDS(cluster_solution, file = "B_Intermediates/cluster_solution.RData")
remove(population, cluster_solution)

## VISUALLLY INSPECT RESULTS ===================================================

if (TRUE) {
  
pdf("~/Desktop/VisualInspection.pdf")
  
cluster_solution <- readRDS("B_Intermediates/cluster_solution.RData")
plot(cluster_solution[, 1:2], pch= 16, asp = 1,
  cex = 4 * cluster_solution$population / max(cluster_solution$population),
  main = "Clusters"
  )
points(cluster_solution[, 1:2], pch= 1, asp = 1, col= "red", lwd = 0.5,
  cex = 4 * cluster_solution$population / max(cluster_solution$population),
  main = "Clusters"
  )
  
proximity_solution <- readRDS("B_Intermediates/proximity_solution.RData")
plot(proximity_solution[, 1:2], pch= 16, asp = 1,
  cex = proximity_solution[, 3] / max(proximity_solution[,3]),
  main = "Proximity"
  )

spacing_solution <- readRDS("B_Intermediates/spacing_solution.RData")
plot(spacing_solution[, 4:5], pch= 16, cex = 0.1, asp = 1,
  main = "Spacing"
  )

graphics.off()

}
  
##########==========##########==========##########==========##########==========
Sys.time() - options()$start_time
