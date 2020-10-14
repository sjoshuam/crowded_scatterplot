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

CenterDiagnostic <- function (xy_coords, good_enough= 0.95, max_centers= 16) {

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
  center_tests$Score[ is.na(center_tests$Score)] <- 0

  ## Determine a good choice for centers argument of kmeans()
  if(max(center_tests$Score) > good_enough) {
    threshold <- min( which(center_tests$Score > good_enough))
    } else {
      threshold <- min( which(center_tests$Scores == max(center_tests$Scores)))
    }

    ## Express output
    return(center_tests$Centers[threshold])
  }

####==== Function to simplify data by kmeans() clusters

Simplify2Centers <- function (xy_coords, num_centers= CenterDiagnostic) {

  ## Determine number of centers
  if(class(num_centers) == "function") {num_centers <- num_centers(xy_coords)}

  ## Create a dataset of clusters
  xy_kmeans <- kmeans(xy_coords, centers= num_centers, iter.max= 1000L)
  xy_centers <- as.data.frame(xy_kmeans$centers)
  colnames(xy_centers) <- c("X", "Y")

  ## Add information to xy_centers objects
  xy_centers$num_nodes <- xy_kmeans$size
  xy_centers$nodes <- tapply(
    X= 1:length(xy_kmeans$cluster),
    INDEX= xy_kmeans$cluster,
    FUN= identity
    )
  attr(xy_centers, "CenterLabels") <- xy_kmeans$cluster

  ## Express output
  return(xy_centers)
  }

####==== Render original and clustered plots side-by-side

ComparePlots <- function (
	before, after,
	tune_size= 1.4,
	before.col= NULL, after.col= NULL,
	plot_file= file.path("C_Outputs", "LumpedPoints.pdf")
	) {
	## Define colors
	if (is.null(before.col)) {before.col <- "black"}
	if (is.null(after.col))  { after.col <- "black"}
	
  #####===== Prepare for rendition
  ## Initialize graphical device 
  pdf( plot_file, width= 10, height= 5)
  par(mfrow= c(1, 2), font= 2)
  
  ## Determine scaling factor for cluster plot
  cluster_scale <- after$num_nodes / min(after$num_nodes)
  cluster_scale <- log(cluster_scale + 2, base= 2) * tune_size
  
  #####===== Render the original scatter plot
  plot(x= before[, 1:2],
  	xaxt= "n", yaxt= "n", xlab= "", ylab= "", bty= "n", pch= 16,
  	xlim= range(before[, 1]), ylim= range(before[, 2])
  	)
  axis(side= 1, col.ticks= "black", col= "transparent" )
  axis(side= 2, col.ticks= "black", col= "transparent" )
  title(main= "Original Scatter Plot")
  
  #####===== Render the clustered plot
  ## Render original scatter as faded underlay
  plot(x= before[, 1:2], col= before.col,
  	xaxt= "n", yaxt= "n", xlab= "", ylab= "", bty= "n", pch= 16,
  	xlim= range(before[, 1]), ylim= range(before[, 2])
  	)
  axis(side= 1, col.ticks= "black", col= "transparent" )
  axis(side= 2, col.ticks= "black", col= "transparent" )
  title(main= "Clustered Scatter Plot")
  
  ## Render clusters
  points(x= after[,  c("X", "Y")], cex= cluster_scale, pch= 16)
  
  ## Label clusters with number of points
  rect(
  	ybottom= after$Y - strheight(after$num_nodes) * 0.5,
  	ytop= after$Y + strheight(after$num_nodes) * 0.5,
  	xleft= after$X - strwidth(after$num_nodes) * 0.5,
  	xright= after$X + strwidth(after$num_nodes) * 0.5,
  	col= "black"
    )
  text(x= after[,  c("X", "Y")],labels= after$num_nodes, col= after.col)
  
  #####===== Terminate graphical device
  graphics.off()

}

#####===== Generate perceptually even color scheme
ColorScheme <- function (num_colors, s= 0.1, v= 0.9) {
	
	## Use HCL color system to generate perceptually spaced hues
	color_scheme <- 1 - {1 / num_colors}
	color_scheme <- seq(from= 0, to= color_scheme * 360, length.out= num_colors)
	color_scheme <- rgb2hsv( col2rgb( hcl(h= color_scheme)))["h", ]
	
	## Use HSV color system to generate colors
	color_scheme <- hsv(h= color_scheme, s= s, v= v)

	## express results	
	return(color_scheme)
	}

##########========== DEMONSTRATE FUNCTION

## Generate data on k-means clusters
set.seed(817)
xy_centers <- Simplify2Centers(nominate_scores[, c("X", "Y")])
nominate_scores$CenterLabels <- attr(xy_centers, "CenterLabels")

## Generate color scheme
color_scheme <- ColorScheme( nrow(xy_centers))

## Plot original and clustered plot side by side
ComparePlots(
	before= nominate_scores[, c("X", "Y")],
	after= xy_centers,
	before.col= color_scheme[nominate_scores$CenterLabels],
	after.col= color_scheme
	)

##########========== FOOTER

## Run time
Sys.time() - options()$startTime

##########==========##########==========##########==========##########==========