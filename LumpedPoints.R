##########==========##########==========##########==========##########==========

##########========== HEADER

## Meta-information
  ## Author: Joshua Mendelsohn
  ## Creation: 2020-10-07
  ## Description: This script declares a function for rendering crowded
    ## crowded scatterplots and demonstrates it. The demonstration before/after
    ## scatterplot saves to "C_Outputs"

## Environment set-up
remove(list= objects())
options(digits= 6, scipen= 4, width= 80, startTime= Sys.time())

##== Inputs

## DW-nominate scores for the 116th US Congress
nominate_scores <- read.csv( file.path("A_Inputs", "HS116_members.csv"))

##########========== REFINE TEST DATA

## Strip out meta-data
nominate_scores$Meta <- NULL

## Simplify party codes
match_index <- c( "R"= 200, "D"= 100, "I"= 328 )
match_index <- match_index[match(nominate_scores$party_code, match_index)]
nominate_scores$party_code <- names(match_index)
remove(match_index)

## Rename nominate coordinates (for convenience)
nominate_scores$X <- nominate_scores$nominate_dim1
nominate_scores$Y <- nominate_scores$nominate_dim2

##########========== CREATE FUNCTIONS

#####===== Function to suggest number of centers for kmeans()

CenterDiagnostic <- function(xy_coords, good_enough= 0.9, max_centers= 16) {

  ## Define values to try
  xy_centers <- 4:min(max_centers, nrow(xy_coords))

  ## Try kmeans() for each test value and record the results
  KMeans <- function(cent, xy) {
    y <- kmeans(x= xy, centers= cent)
    return(y$betweenss/y$totss)
    }

  ## 
  use_cores <- floor(parallel::detectCores() * 0.8)
  use_cores <- parallel::makeCluster(use_cores)
  center_tests <- parallel::parLapply(
    cl= use_cores,
    fun= KMeans,
    X= xy_centers,
    xy= xy_coords
  )
  parallel::stopCluster(use_cores)
  remove(use_cores)

  ## Package results for analysis
  center_tests <- data.frame(
    "Centers"= xy_centers,
    "Score"= unlist(center_tests)
    )

  ## Determine a good choice for centers argument of kmeans()
  if(max(center_tests$Score) > good_enough) {
    threshold <- min( which(center_tests$Score > good_enough))
    } else {
      threshold <- which.max(center_tests$Scores)
    }

    ## Express output
    return(center_tests$Centers[threshold])
  }

####==== Function to simplify data by kmeans() clusters

Simplify2Centers <- function(xy_coords, num_centers= CenterDiagnostic) {

  ## Determine number of centers
  if(class(num_centers) == "function") {num_centers <- num_centers(xy_coords)}

  ## Create a dataset of clusters
  xy_kmeans <- kmeans(xy_coords, centers= num_centers, iter.max= 1000L)
  xy_centers <- as.data.frame(xy_kmeans$centers)

  ## Add information to xy_centers objects
  xy_centers$num_nodes <- xy_kmeans$size
  xy_centers$nodes <- tapply(
    X= 1:length(xy_kmeans$cluster),
    INDEX= xy_kmeans$cluster,
    FUN= identity
    )

  ## Express output
  return(xy_centers)
  }

####==== Render 

##########========== DEMONSTRATE FUNCTION
xy_centers <- Simplify2Centers(nominate_scores[, c("X", "Y")])
objects()

##########========== FOOTER

## Run time
Sys.time() - options()$startTime

##########==========##########==========##########==========##########==========