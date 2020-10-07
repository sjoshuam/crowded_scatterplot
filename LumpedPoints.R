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

##########========== FOOTER

## Run time
Sys.time() - options()$startTime

##########==========##########==========##########==========##########==========