##########==========##########==========##########==========##########==========

## SET UP ======================================================================

## meta-information
## Author: Joshua M.
## Creation: 2021-02-07
## Version: 4.0.3
## Description: pull population data at the tract level for the contiguous
   ## united states

## set up coding environment
remove(list = objects())
options(width = 80, scipen = 2, digits = 6, test_mode = FALSE)

library(foreign)
library(jsonlite)
library(tidyverse)
library(parallel)

## load api key
source("../api_keys/censusAPI.R")

## PULL POPULATION DATA FROM CENSUS ============================================

##  retrieve state fips codes
state_url <- url("https://www2.census.gov/geo/docs/reference/state.txt")
state_codes <- state_url %>%
  readLines() %>%
  str_split("\\|") %>%
  simplify2array() %>%
  t() 
colnames(state_codes) <- str_to_lower(state_codes[1, ])
state_codes <- as_tibble(state_codes[-1, ])

close(state_url)
remove(state_url)

## filter codes to contiguous United States (CONUS)
state_codes <- filter(state_codes,
  !(stusab %in% c("AS", "GU", "MP", "PR", "UM", "VI", "AK", "HI"))
  )

## construct API queries
state_codes$census_query <- paste0(
  "https://api.census.gov/data/2019/acs/acs5?",
  "get=NAME,B01001_001E&for=tract:*&in=state:",
  state_codes$state,
  "&key=",
  census_api_key
  )

## reduce pull size for test mode
if (options()$test_mode) {
  state_codes <- filter(state_codes, stusab %in% c("DC", "MD", "DE"))
}

## pull data
api_pull <- function(x) {
  x <- url(x)
  y <- readLines(x)
  y <- jsonlite::fromJSON(y)
  colnames(y) <- y[1, ]
  y <- y[-1, ]
  y <- dplyr::as_tibble(y)
  close(x)
  y
}

population <- as.list(state_codes$census_query)

p_cluster <- makeCluster(floor(detectCores() * 0.8))
population <- parLapply(population, api_pull, cl = p_cluster)
stopCluster(p_cluster)

remove(api_pull, p_cluster)

sum(sapply(population, function(x){sum(as.numeric(x$B01001_001E))})) / 10^6

## PULL GEOGRAPHIC DATA FROM TIGER =============================================

## assemble download links
state_codes$tiger_query <- paste0(
  "ftp://ftp2.census.gov/geo/tiger/TIGER2019/TRACT/tl_2019_",
  state_codes$state,
  "_tract.zip"
  )
state_codes$tiger_dest <- paste0(
  "B_Intermediates/tiger/state",
  state_codes$state,
  ".zip"
  )

## download zip packages and extract relavant files
dir.create("B_Intermediates/tiger", showWarnings = FALSE)
file.remove(list.files("B_Intermediates/tiger", full.names = TRUE))

download_file <- function(x) {
  y <- stringr::str_remove(x, ".*/")
  y <- paste0("B_Intermediates/tiger/", y)
  download.file(url = x, destfile = y)
  unzip(y, exdir = "B_Intermediates/tiger/")
  }

p_cluster <- makeCluster(floor(detectCores() * 0.8))
invisible(parLapply(
  cl = p_cluster,
  fun = download_file,
  X = state_codes$tiger_query
  ))

stopCluster(p_cluster)
remove(p_cluster, download_file)

file.remove(
  list.files("B_Intermediates/tiger", full.names = TRUE, pattern = "[^(dbf)]$")
  )

## COMPILE DATA AND REFINE =====================================================

## compile tiger data
read_dbf <- function(x) {
  x <- foreign::read.dbf(x)
  x <- x[, c("GEOID", "INTPTLON", "INTPTLAT")]
  x <- tibble::as_tibble(apply(x, 2, as.character))
  x$INTPTLON <- as.numeric(x$INTPTLON)
  x$INTPTLAT <- as.numeric(x$INTPTLAT)
  x
}

p_cluster <- makeCluster(floor(detectCores() * 0.8))
coordinates <- parLapply(
  cl = p_cluster,
  fun = read_dbf,
  X = list.files("B_Intermediates/tiger", full.names = TRUE)
  )

stopCluster(p_cluster)
remove(p_cluster)
remove(read_dbf)
file.remove(list.files("B_Intermediates/tiger", full.names = TRUE))
unlink("B_Intermediates/tiger", recursive = TRUE)

coordinates <- do.call(
  what = bind_rows,
  args = coordinates
  )

## compile census data
population <- do.call(what = bind_rows, args = population)

sum(as.numeric(population$B01001_001E)) / 10^6

## add textual nomenclature for counties and states
population <- population %>%
  left_join(state_codes[, c("state", "stusab")], by = "state") %>%
  rename("state_fips" = state, "state" = stusab, "county_fips" = county) %>%
  rename("tract_fips" = tract, "population" = B01001_001E) %>%
  mutate("county" = str_remove(NAME, "^[^,]+, ")) %>%
  mutate("county" = str_remove(county, ",.*$")) %>%
  select(-NAME) %>%
  mutate("population" = as.numeric(population)) %>%
  mutate("full_fips"= paste0(state_fips, county_fips, tract_fips))

sum(population$population) / 10^6

population <- left_join(population, coordinates, by = c("full_fips" = "GEOID"))
remove(coordinates)

sum(population$population / 10^6) #7.725

## EXPAND TO SCATTERPLOT FORMAT ================================================

## rename latitude/longitude
population <- rename(population, c(lon = INTPTLON, lat = INTPTLAT))

## generate scatterplot format dataset
pop_points <- round(population$population / 5000)
pop_points <- rep(population$full_fips, pop_points) %>%
  tibble() %>%
  rename("full_fips" = 1) %>%
  left_join(select(population, full_fips, lon, lat), by = "full_fips")
  

## SAVE DATA OBJECTS ===========================================================

saveRDS(population, file = "B_Intermediates/population.RData")
saveRDS(pop_points, file = "B_Intermediates/pop_points.RData")

##########==========##########==========##########==========##########==========