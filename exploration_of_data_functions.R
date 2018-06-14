############### SESYNC Research Support: Processing data ########## 
## Exploration of data for FishingandUrbanInequality pursuit gropu
## 
## DATE CREATED: 05/31/2018
## DATE MODIFIED: 06/14/2018
## AUTHORS: Benoit Parmentier 
## PROJECT: 
## ISSUE: 
## TO DO:
##
## COMMIT: testing tigris
##
## Links to investigate:
#https://journal.r-project.org/archive/2016/RJ-2016-043/index.html
#https://walkerke.github.io/2017/05/tigris-metros/
#https://cran.r-project.org/web/packages/censusapi/vignettes/getting-started.html

#https://journal.r-project.org/archive/2016/RJ-2016-043/RJ-2016-043.pdf
###################################################
#

###### Library used

library(gtools)                              # loading some useful tools 
library(sp)                                  # Spatial pacakge with class definition by Bivand et al.
library(spdep)                               # Spatial pacakge with methods and spatial stat. by Bivand et al.
library(rgdal)                               # GDAL wrapper for R, spatial utilities
library(raster)
library(gdata)                               # various tools with xls reading, cbindX
library(rasterVis)                           # Raster plotting functions
library(parallel)                            # Parallelization of processes with multiple cores
library(maptools)                            # Tools and functions for sp and other spatial objects e.g. spCbind
library(maps)                                # Tools and data for spatial/geographic objects
library(plyr)                                # Various tools including rbind.fill
library(spgwr)                               # GWR method
library(rgeos)                               # Geometric, topologic library of functions
library(gridExtra)                           # Combining lattice plots
library(colorRamps)                          # Palette/color ramps for symbology
library(ggplot2)
library(lubridate)
library(dplyr)
library(rowr)                                # Contains cbind.fill
library(car)
library(sf)
library(tigris)

library(acs) # census api
library(leaflet)

library(tidyverse)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

## Add key to .Renviron
#Sys.setenv(CENSUS_KEY=YOURKEYHERE)
## Reload .Renviron
#readRenviron("~/.Renviron")
## Check to see that the expected key is output in your R console
#Sys.getenv("CENSUS_KEY")

###### Functions used in this script and sourced from other files

create_dir_fun <- function(outDir,out_suffix=NULL){
  #if out_suffix is not null then append out_suffix string
  if(!is.null(out_suffix)){
    out_name <- paste("output_",out_suffix,sep="")
    outDir <- file.path(outDir,out_name)
  }
  #create if does not exists
  if(!file.exists(outDir)){
    dir.create(outDir)
  }
  return(outDir)
}

#Used to load RData object saved within the functions produced.
load_obj <- function(f){
  env <- new.env()
  nm <- load(f, env)[1]
  env[[nm]]
}

## This is a slightly modified function from the package and paper by
#Kyle Walker , The R Journal (2016) 8:2, pages 231-242.

metro_tracts <- function(metro_name) {
  
  # First, identify which states intersect the metro area using the
  # `states` function in tigris
  st <- states(cb = TRUE)
  cb <- core_based_statistical_areas(cb = TRUE)
  metro <- filter(cb, grepl(metro_name, NAME))
  
  stcodes <- st[metro,]$STATEFP
  
  # Then, fetch the tracts, using rbind_tigris if there is more
  # than one state
  if (length(stcodes) > 1) {
    tr <- rbind_tigris(
      map(stcodes, function(x) {
        tracts(x, cb = TRUE)
      })
    )
  } else {
    tr <- tracts(stcodes, cb = TRUE)
  }
  
  # Now, find out which tracts are within the metro area
  within <- st_within(tr, metro)
  
  within_lgl <- map_lgl(within, function(x) {
    if (length(x) == 1) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })
  
  # Finally, subset and return the output
  output <- tr[within_lgl,]
  
  return(output)
  
}

## This is a slightly modified function from the package and paper by
#Kyle Walker , The R Journal (2016) 8:2, pages 231-242.

# Write function to get ZCTAs for a given metro
get_zcta_zones <- function(metro_name) {
  ## This gets all zcta in sf format:
  zips <- zctas(cb = TRUE) #all zcta in US (4,269 polygons)
  
  metros <- core_based_statistical_areas(cb = TRUE) # get all metro areas
  # Subset for specific metro area
  # (be careful with duplicate cities like "Washington")
  my_metro <- metros[grepl(sprintf("^%s", metro_name),
                           metros$NAME, ignore.case = TRUE), ]
  
  # Find all ZCTAs that intersect the metro boundary
  
  class(my_metro)
  class(zips)
  #metro_zips <- over(my_metro, zips, returnList = TRUE)[[1]]
  #Error in (function (classes, fdef, mtable)  : 
  #            unable to find an inherited method for function ‘over’ for signature ‘"sf", "sf"’
  
  my_metro_sp <- as(my_metro,"Spatial")
  zips_sp <- as(zips,"Spatial")
  
  metro_zips <- over(my_metro_sp, zips_sp, returnList = TRUE)[[1]]
  
  my_zips <- zips[zips$ZCTA5CE10 %in% metro_zips$ZCTA5CE10, ]
  # Return those ZCTAs
  return(my_zips)
}

############################ End of script #####################################################

#income_data <- acs.fetch(endyear = 2012, 
#                         geography = geo.make(state = "TX", 
#                                              county = c(113, 439), 
#                                              tract = "*"), 
#                         variable = "B19013_001",
#                         key = key_val)
#dfw_merged <- geo_join(dfw, income_df, "GEOID", "GEOID")

