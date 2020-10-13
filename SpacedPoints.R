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


plot( GenerateLattice(nominate_scores[,c("X", "Y")]) )



##########========== 

##########==========##########==========##########==========##########==========