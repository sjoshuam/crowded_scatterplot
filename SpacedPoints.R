##########==========##########==========##########==========##########==========

##########========== HEADER

## Meta-information
  ## Author: Joshua Mendelsohn
  ## Creation: 2020-10-09 on R v4.0.2
  ## Description: This script holds functions for rendering crowded scatterplots
    ## using a point-spacing strategy. The demonstration before/after
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

#####===== Generate regular lattices of points, so that scatterplot can be
  ## conformed to regularly spaced lattice points
GenerateLattice <- function (xy_coords, point_density= 1/3) {
	
	## Determine x-spacing for lattice
	x_space <- abs( diff( range(xy_coords[,1])))
	x_space <- x_space / sqrt( nrow(xy_coords))
	x_space <- x_space * sqrt(point_density)

	## Determine y-spacing for lattice
	y_space <- sqrt( x_space^2 - {x_space/2}^2 )
	
	## Find all valid points in the grid
	x_points <- range(xy_coords[, 1])
	x_points <- seq(from= min(x_points), to= max(x_points), by= x_space)
	y_points <- range(xy_coords[, 1])
	y_points <- seq(from= min(y_points), to= max(y_points), by= y_space)

	## Generate lattice
	xy_lattice <- cbind(
		"X"= rep(x_points, each= length(y_points)),
		"Y"= rep(y_points, times= length(x_points))
				)
	if(nrow(xy_lattice) < nrow(xy_coords)) {stop("Point_density is too high!")}

	## Offset lattice
	offset <- unique(xy_lattice[,"Y"])
	offset <- offset[{{1:length(offset)} %% 2} == 0]
	offset <- xy_lattice[,"Y"] %in% offset
	xy_lattice[offset, "X"] <- xy_lattice[offset, "X"] + {x_space / 4}
	xy_lattice[!offset, "X"] <- xy_lattice[!offset, "X"] - {x_space / 4}
	
	return(xy_lattice)
}

Distance <- function (xy1, xy2) {
	x_dist <- outer(xy1[, 1], xy2[, 1], FUN= "-")^2
	y_dist <- outer(xy1[, 2], xy2[, 2], FUN= "-")^2
	xy_dist <- sqrt(x_dist + y_dist)
	xy_dist
	}

#####===== Assign points to lattice points so that they have regular spacing

SpacedPoints <- function (xy_coords, max_iter= 100) {
	
	## generate lattice and calculate distances between points and lattice
	xy_lattice <- GenerateLattice(xy_coords)
	dist_matrix <- Distance(xy_coords, xy_lattice)
	
	## Iteratively assign points to lattice grid (first iteration)
	closest_lattice <- apply(dist_matrix, 1, which.min)
	closest_lattice[duplicated(closest_lattice)] <- NA
	dist_matrix[ , closest_lattice[!is.na(closest_lattice)]] <- Inf
	
	## Iteratively assign points to lattice grid (subsequent iterations)
	iter <- 0
	while ( any(is.na(closest_lattice)) & {iter < max_iter} ) {
		iter <- iter + 1
		temp_iter <- apply(dist_matrix, 1, which.min)
		closest_lattice[is.na(closest_lattice)] <- temp_iter[is.na(closest_lattice)]
	  closest_lattice[duplicated(closest_lattice)] <- NA
	  dist_matrix[ , closest_lattice[!is.na(closest_lattice)]] <- Inf
		}
	
	## Package outputs
	xy_lattice <- as.data.frame(xy_lattice[closest_lattice, ])
	colnames(xy_lattice) <- c("X", "Y")

	return(xy_lattice)
}

####==== Render original and clustered plots side-by-side

ComparePlots <- function (
	before, after,
	tune_size= 1.4,
	plot_file= file.path("C_Outputs", "SpacedPoints.pdf")
	) {
  #####===== Prepare for rendition
  ## Initialize graphical device 
  pdf( plot_file, width= 10, height= 5)
  par(mfrow= c(1, 2), font= 2)
  
  #####===== Render the original scatter plot
  plot(x= before[, 1:2],
  	xaxt= "n", yaxt= "n", xlab= "", ylab= "", bty= "n", pch= 16,
  	xlim= range(before[, 1]), ylim= range(before[, 2])
  	)
  axis(side= 1, col.ticks= "black", col= "transparent" )
  axis(side= 2, col.ticks= "black", col= "transparent" )
  title(main= "Original Scatter Plot")
  
  #####===== Render the re-spaced scatter plot
  plot(x= after[, 1:2],
  	xaxt= "n", yaxt= "n", xlab= "", ylab= "", bty= "n", pch= 16,
  	xlim= range(before[, 1]), ylim= range(before[, 2])
  	)
  axis(side= 1, col.ticks= "black", col= "transparent" )
  axis(side= 2, col.ticks= "black", col= "transparent" )
  title(main= "Original Scatter Plot")
  
  #####===== Terminate graphical device
  graphics.off()

}

##########========== DEMONSTRATION FUNCTION

## Generate data
xy_spaced <- SpacedPoints(nominate_scores[, c("X", "Y")])

## Plot original and spaced plot side by side
ComparePlots(
	before= nominate_scores[, c("X", "Y")],
	after= xy_spaced
	)

##########========== FOOTER

## Run time
Sys.time() - options()$startTime

##########==========##########==========##########==========##########==========